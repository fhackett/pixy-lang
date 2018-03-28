module Pixy.Delay where

import Prelude hiding (max)
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Except

import Pixy.Syntax

data ConstraintError
    = ConstraintMismatch Constraint
    deriving (Show)

data CVar
    = Gen String !Int
    | Bound Var
    deriving (Eq, Show)

data CVal = CVar CVar | CVal Int
    deriving (Show)

data Constraint
    = K CVar !Int -- ^ The variable is constrained to a constant delay
    | E CVar CVar !Int -- ^ The variable is constrained to be equal to another variable with an offset
    | LE CVar CVal CVal !Int -- ^ The variable is constrained to be less than or equal to another variable with an offset
    | Max CVar [CVal] !Int
    | Sub CVar CVal CVal !Int
    deriving (Show)

max :: CVar -> [CVar] -> Int -> Constraint
max x ys = Max x (fmap CVar ys)

le :: CVar -> CVar -> Int -> Constraint
le x y = LE x (CVar x) (CVar y)

sub :: CVar -> CVar -> CVar -> Int -> Constraint
sub x y z = Sub x (CVar y) (CVar z)

-- | Increments the delay of a constraint
increment :: Constraint -> Constraint
increment (K v i) = K v (i + 1)
increment (E x y i) = E x y (i + 1)
increment (LE x xv y i) = LE x xv y (i + 1)
increment (Max x ys i) = Max x ys (i + 1)
increment (Sub x y z i) = Sub x y z (i + 1)

constrained :: Constraint -> CVar
constrained (K v _) = v
constrained (E x _ _) = x
constrained (LE x _ _ _) = x
constrained (Max x _ _) = x
constrained (Sub x _ _ _) = x

genCVar :: (MonadState Int m) => String -> m CVar
genCVar n = do
    k <- get
    put (k + 1)
    return $ Gen n k

type CM = ReaderT ([CVar], Int) (State Int)

runCM :: CM a -> a
runCM = flip evalState 0 . flip runReaderT ([], 1)

isRec :: (MonadReader ([CVar], Int) m) => CVar -> m Bool
isRec v = asks (elem v . fst)

fbyLevel :: (MonadReader ([CVar], Int) m) => m Int
fbyLevel = asks snd

constraints :: (MonadState Int m, MonadReader ([CVar], Int) m) => Expr -> CVar -> m [Constraint]
constraints e v = case e of
    (Var x) -> isRec (Bound x) >>= \case
            True -> return [K v 1]
            False -> return [E v (Bound x) 1]
    (Const _) -> return [K v 0]
    (If c t f) -> do
        c' <- genCVar "c"
        t' <- genCVar "t"
        f' <- genCVar "f"
        cc <- constraints c c'
        tt <- constraints t t'
        ff <- constraints f f'
        return $ [max v [c', t', f'] 0] ++ cc ++ tt ++ ff
    (Check e) -> constraints e v
    (Next e) -> fmap (\c -> if constrained c == v then increment c else c) <$> constraints e v
    (Fby l r) -> do
        lhs <- genCVar "lhs"
        rhs <- genCVar "rhs"
        ll <- constraints l lhs
        rr <- local (\(vs, l) -> (v:vs, l+1)) (constraints r rhs)
        l <- fbyLevel
        return $ [sub v lhs rhs l, le rhs lhs l] ++ ll ++ rr
    (Where body bs) -> do
        cbody <- constraints body v
        cbs <- sequence $ (\(v,e) -> constraints e (Bound v)) <$> bs
        return $ cbody ++ join cbs
    (App _ args) -> return []
    (Binop _ l r) -> do
        lhs <- genCVar "bl"
        rhs <- genCVar "br"
        cl <- constraints l lhs
        cr <- constraints r rhs
        return $ [max v [lhs, rhs] 0] ++ cl ++ cr

-- We have to work backwards, substituting and solving as we go
reduce :: [Constraint] -> Either Constraint [Constraint]
reduce = reduce' . reverse
    where

        reduce' :: [Constraint] -> Either Constraint [Constraint]
        reduce' [] = return []
        reduce' (c:cs) = case c of
            (K v i) -> do
                cs' <- reduce' $ subst v i <$> cs
                case v of
                    (Bound _) -> return $ c:cs'
                    (Gen _ _) -> return cs'
            -- (E x y i) -> do
            (LE x (CVal i) (CVal j) k) ->
                if (i <= j + k) then reduce' cs 
                else throwError $ c
            _ -> (c:) <$> reduce' cs

        subst :: CVar -> Int -> Constraint -> Constraint
        subst v i (E x y j) | v == y = (K x (i + j))
        subst v i (Sub x (CVar y) z j) | v == y = (Sub x (CVal i) z j)--(E x z (i + j))
        subst v i (Sub x y (CVar z) j) | v == z = 
            case y of
                (CVar y) -> (E x y (j - i))
                (CVal k) -> (K x (k - i + j))
        subst v i (Max x ys j) = 
            let 
                ys' = (replace (cvarEq v) (CVal i) ys)
            in 
                if all cval ys' then K x (j + (maximum $ (\(CVal k) -> k) <$> ys'))
                else (Max x ys' j)
        -- Note that we have structured things in such a way that
        -- -- we always hit the RHS of a LE before the right
        subst v i (LE x xval (CVar y) j) | v == y = (LE x xval (CVal i) j)
        subst v i (LE x (CVar xval) y j) | v == xval = (LE x (CVal i) y j)
        -- TODO: Fill in the other cases
        subst _ _ c = c

        replace :: (a -> Bool) -> a -> [a] -> [a]
        replace p f [] = []
        replace p y (x:xs) = if p x then y:replace p y xs else x:replace p y xs

        cvarEq :: CVar -> CVal -> Bool
        cvarEq x (CVar y) = x == y
        cvarEq _ _ = False

        cval :: CVal -> Bool
        cval (CVar _) = False
        cval (CVal _) = True

genConstraints :: [Function] -> [Constraint]
genConstraints fs = join $ runCM $ traverse (\(Function n _ e) -> constraints e (Bound n)) fs