{-# LANGUAGE RankNTypes #-}
module Pixy.Delay where

import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad.Except
import Control.Applicative (Alternative, (<|>))
import Data.Foldable (traverse_)

import qualified Data.Map as Map
import Data.Map (Map, (!))
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Data.List (foldl1)

import Pixy.Syntax

{-
When dealing with a where =
Replace all instances of the 
-}

-- Given a solver state, I can give you a list of all possible solver states and a's
-- SolverState s -> [(SolverState s,a)]
newtype Solver s a = Solver { unSolver :: StateT (SolverState s) [] a }
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadState (SolverState s))

newtype SolverVar s = SolverVar { unVar :: Var }
    deriving (Eq, Ord)

data VarInfo s = VarInfo
     { delayedConstraints :: Solver s Bool, values :: IntSet }

data SolverState s = SolverState { varSupply :: SolverVar s , varMap :: Map (SolverVar s) (VarInfo s) }

add :: Constraint -> Solver s Bool
add (CEQ t1 t2) = undefined


newvar :: Solver s CTerm
newvar = undefined
-- add (CEQ x (CConst i) j) = do
--     return undefined
-- -- add c

-- domain :: SolverVar -> Solver IntSet
-- domain x = do
--     vm <- gets varMap
--     return . values $ vm ! x

-- update :: SolverVar -> IntSet -> Solver ()
-- update x i = do
--     s <- get
--     let vm = varMap s
--     let vi = vm ! x
--     put $ s { varMap = Map.insert x (vi { values = i}) vm }
--     delayedConstraints vi


data Constraint
    = CEQ CTerm CTerm -- x = y + i
    | CLEQ CTerm CTerm -- x <= y + i
    deriving (Show)

data CTerm
    = CVar Var
    | CPlus CTerm Int
    | CMax CTerm CTerm
    | CConst !Int
    deriving (Show)

(@=) :: CTerm -> CTerm -> Constraint
t1 @= t2 = CEQ t1 t2


(@<=) :: CTerm -> CTerm -> Constraint
t1 @<= t2 = CLEQ t1 t2

(@+) :: CTerm -> Int -> CTerm
(CConst i) @+ j = CConst (i + j)
t1 @+ 0 = t1
t1 @+ i = CPlus t1 i


-- max_ :: [CTerm] -> CTerm
-- max_ = foldl1 max'

max' :: CTerm -> CTerm -> CTerm
max' (CConst i) (CConst j) = CConst (max i j)
max' (CMax t1 (CConst i)) (CConst j) = max' t1 (CConst (max i j))
max' (CMax (CConst i) t1) (CConst j) = max' t1 (CConst (max i j))
max' (CConst i) (CMax (CConst j) t1) = max' t1 (CConst (max i j))
max' (CConst i) (CMax t1 (CConst j)) = max' t1 (CConst (max i j))
max' (CVar x) (CMax (CVar y) t1) | x == y = max' t1 (CVar x)
max' (CVar x) (CMax t1 (CVar y)) | x == y = max' t1 (CVar x)
max' (CMax t1 (CVar y)) (CVar x) | x == y = max' t1 (CVar x)
max' (CMax (CVar y) t1) (CVar x) | x == y = max' t1 (CVar x)
max' t1 t2 = CMax t1 t2



-- (@+) :: Constraint -> Int -> Constraint
-- (CEQ t1 t2 i) @+ j = CEQ t1 t2 (i + j)
-- (CLEQ t1 t2 i) @+ j = CLEQ t1 t2 (i + j)

data GenState = GenState
    { currentDelay :: Int
    , fbyDepth :: Int
    , recVars :: [Var]
    }

type GenConstraints a = (ReaderT GenState (Writer [Constraint])) a--(CTree a -> CTree a)

runGenConstraints :: GenConstraints a -> (a, [Constraint])
runGenConstraints m = runWriter $ runReaderT m (GenState 1 1 [])


{-
TODO: Annotate each node of the tree with the appropriate delay
-}
constraints :: Expr -> GenConstraints CTerm
constraints = \case
    (Var x) -> do
        d <- asks currentDelay
        return $ CVar x @+ d
    (Const _) -> return $ CConst 0
    (Check e) -> constraints e
    (If c t f) -> do
        cc <-  constraints c
        tt <- constraints t
        ff <- constraints f
        return $ max' cc (max' tt ff)
    (Next e) -> nextDepth (constraints e)
    (Fby l r) -> do
        ll <- constraints l
        rr<- fbyRhs (constraints r)
        d <- asks fbyDepth
        tell [rr @<= (ll @+ d)]
        return ll
    (Where body bs) -> do
        traverse_ whereConstraints bs
        constraints body
    (App _ _) -> return $ CConst 0
    (Binop _ l r) -> do
        ll <- constraints l
        rr <- constraints r
        return $ max' ll rr
    where
        nextDepth :: GenConstraints a -> GenConstraints a
        nextDepth = local (\s -> s { currentDelay = 1 + (currentDelay s)})

        fbyRhs :: GenConstraints a -> GenConstraints a
        fbyRhs = local (\s -> s { fbyDepth = 1 + (fbyDepth s) })

        whereConstraints :: (Var, Expr) -> GenConstraints ()
        whereConstraints (v,e) = do
            ee <- local (\s -> s { recVars = v:(recVars s) }) (constraints e)
            tell [CVar v @= ee]

genConstraints :: Function -> [Constraint]
genConstraints (Function n args body) = snd $ runGenConstraints $ constraints body

-- constraints ::  CTerm -> Expr -> CTree a -> GenConstraints (CTree a)
-- constraints v e k = case e of
--     (Var x) -> do
--         d <- asks currentDelay
--         return (v @=+ (bound x :+ (d + 1)) $ k) 
--     (Const _) -> return (v @= (CConst 0) $ k) 
--     (If c t f) -> genVar $ \cv -> genVar $ \tv -> genVar $ \fv ->
--             ((return (v @= (max_ [cv, tv, fv]) $ k))
--                 /\ constraints cv c 
--                 /\ constraints tv t 
--                 /\ constraints fv f)
--     (Next e) -> nextDepth (constraints v e k)
--     -- (Fby l r) -> do
    --     lhs <- genCVar "lhs"
    --     rhs <- genCVar "rhs"
    --     d <- asks fbyDepth
    --     (return $ v @=+ ((lhs :- rhs) :+ d))
    --         /\ (return $ rhs @<=+ (lhs :+ d))
    --         /\ constraints lhs l
    --         /\ fbyRhs (constraints rhs r)
    -- (Where body bs) -> do
    --     cbs <- traverse (\(v,e) -> constraints (bound v) e) bs
    --     cb <- constraints v body
    --     return $ foldl (.) id (cb:cbs)
    -- (App _ _) -> return id
    -- (Binop _ l r) -> do
    --     lhs <- genCVar "bl"
    --     rhs <- genCVar "br"
    --     (return $ v @= max_ [lhs, rhs])
    --         /\ constraints lhs l
    --         /\ constraints rhs r

   --flip runStateT 0 $ constraints
-- constraints (Var x) = isRec x >>= \case
--     True ->V


-- normalizeRules :: CTerm -> Maybe CTerm
-- normalizeRules = \case
--     -- Constant Reduction rules
--     (CConst i :+ CConst j) -> Just $ CConst (i + j)
--     (CConst i :- CConst j) -> Just $ CConst (i - j)
--     (CMax (CConst i) (CConst j)) -> Just $ CConst (max i j)
--     -- Identity Rules
--     (CConst 0 :+ x) -> Just x
--     (x :+ CConst 0) -> Just x
--     (x :- CConst 0) -> Just x
--     -- Associative Rules
--     (CConst i :+ CConst j :+ z) -> Just $ CConst (i + j) :+ z
--     (CConst i :+ z :+ CConst j) -> Just $ CConst (i + j) :+ z
--     (CConst i :+ CConst j :- z) -> Just $ CConst (i + j) :- z
--     (CConst i :+ z :- CConst j) -> Just $ CConst (i - j) :+ z
--     _ -> Nothing


-- normalize :: CTerm -> CTerm
-- normalize = \case
--     (CVar x) -> (CVar x)
--     (CConst i) -> (CConst i)
--     (x :+ y) -> case (normalize x, normalize y) of
--         (CConst i, CConst j) -> CConst (i + j)
--         (CConst 0, y) -> y
--         (x, CConst 0) -> x
--         (CConst i, CConst j :+ z) -> CConst (i + j) :+ z
--         (CConst i, z :+ CConst j) -> CConst (i + j) :+ z
--         (CConst i :+ z, CConst j) -> CConst (i + j) :+ z
--         (z :+ CConst i, CConst j) -> CConst (i + j) :+ z
--         -- (CConst i, CConst j :- z) -> CConst (i + j) :- z
--         -- (CConst i :- z, CConst j) -> CConst (i + j) :- z
--         (x,y) -> x :+ y
    -- (CNeg x) -> case normalize x of
    --     (CConst i) -> CConst (-i)
    --     (CConst i :+ z) -> 
    --     x -> CNeg x


        -- (CConst i, (CP))
        -- normalizeBin :: CTerm -> CTerm -> (CTerm -> CTerm -> CTerm) -> CTerm

        -- (CConst i, CConst j) -> CConst (i + j)
        -- (x,y) -> CPlus x y
    -- (CMinus x y) -> case (normalize x, normalize y) of
        -- (CConst i, CConst j) -> CConst (i - j)
        -- (x,y) -> CMinus x y
-- normalize (CMax x y) = case (normalize x, normalize y) of
--     (CConst i, CConst j) -> CConst (max i j)
--     (x,y) -> CMax x y

-- data ConstraintError
--     = ConstraintMismatch Constraint
--     deriving (Show)


-- data CVal = CVar CVar | CVal Int
--     deriving (Show)

-- data Constraint
--     = K CVar !Int -- ^ The variable is constrained to a constant delay
--     | E CVar CVar !Int -- ^ The variable is constrained to be equal to another variable plus some delay
--     | LE CVar CVal !Int -- ^ The variable is constrained to be less than or equal to another variable plus some delay
--     | Max CVar [CVal] !Int -- ^ The variable is constrained to 
--     | Sub CVar CVar CVar !Int -- ^ x = y - z + i
--     deriving (Show)

-- max :: CVar -> [CVar] -> Int -> Constraint
-- max x ys = Max x (fmap CVar ys)

-- le :: CVar -> CVar -> Int -> Constraint
-- le x y = LE x (CVar x) (CVar y)

-- sub :: CVar -> CVar -> CVar -> Int -> Constraint
-- sub x y z = Sub x (CVar y) (CVar z)

-- -- | Increments the delay of a constraint
-- increment :: Constraint -> Constraint
-- increment (K v i) = K v (i + 1)
-- increment (E x y i) = E x y (i + 1)
-- increment (LE x xv y i) = LE x xv y (i + 1)
-- increment (Max x ys i) = Max x ys (i + 1)
-- increment (Sub x y z i) = Sub x y z (i + 1)

-- constrained :: Constraint -> CVar
-- constrained (K v _) = v
-- constrained (E x _ _) = x
-- constrained (LE x _ _ _) = x
-- constrained (Max x _ _) = x
-- constrained (Sub x _ _ _) = x

-- genCVar :: (MonadState Int m) => String -> m CVar
-- genCVar n = do
--     k <- get
--     put (k + 1)
--     return $ Gen n k

-- type CM = ReaderT ([CVar], Int) (State Int)

-- runCM :: CM a -> a
-- runCM = flip evalState 0 . flip runReaderT ([], 1)

-- isRec :: (MonadReader ([CVar], Int) m) => CVar -> m Bool
-- isRec v = asks (elem v . fst)

-- fbyLevel :: (MonadReader ([CVar], Int) m) => m Int
-- fbyLevel = asks snd

-- constraints :: (MonadState Int m, MonadReader ([CVar], Int) m) => Expr -> CVar -> m [Constraint]
-- constraints e v = case e of
--     (Var x) -> isRec (Bound x) >>= \case
--             True -> return [K v 1]
--             False -> return [E v (Bound x) 1]
--     (Const _) -> return [K v 0]
--     (If c t f) -> do
--         c' <- genCVar "c"
--         t' <- genCVar "t"
--         f' <- genCVar "f"
--         cc <- constraints c c'
--         tt <- constraints t t'
--         ff <- constraints f f'
--         return $ [max v [c', t', f'] 0] ++ cc ++ tt ++ ff
--     (Check e) -> constraints e v
--     (Next e) -> fmap (\c -> if constrained c == v then increment c else c) <$> constraints e v
--     (Fby l r) -> do
--         lhs <- genCVar "lhs"
--         rhs <- genCVar "rhs"
--         ll <- constraints l lhs
--         rr <- local (\(vs, l) -> (v:vs, l+1)) (constraints r rhs)
--         l <- fbyLevel
--         return $ [sub v lhs rhs l, le rhs lhs l] ++ ll ++ rr
--     (Where body bs) -> do
--         cbody <- constraints body v
--         cbs <- sequence $ (\(v,e) -> constraints e (Bound v)) <$> bs
--         return $ cbody ++ join cbs
--     (App _ args) -> return []
--     (Binop _ l r) -> do
--         lhs <- genCVar "bl"
--         rhs <- genCVar "br"
--         cl <- constraints l lhs
--         cr <- constraints r rhs
--         return $ [max v [lhs, rhs] 0] ++ cl ++ cr

-- -- We have to work backwards, substituting and solving as we go
-- reduce :: [Constraint] -> Either Constraint [Constraint]
-- reduce = reduce' . reverse
--     where

--         reduce' :: [Constraint] -> Either Constraint [Constraint]
--         reduce' [] = return []
--         reduce' (c:cs) = case c of
--             (K v i) -> do
--                 cs' <- reduce' $ subst v i <$> cs
--                 case v of
--                     (Bound _) -> return $ c:cs'
--                     (Gen _ _) -> return cs'
--             -- (E x y i) -> do
--             (LE x (CVal i) (CVal j) k) ->
--                 if (i <= j + k) then reduce' cs 
--                 else throwError $ c
--             _ -> (c:) <$> reduce' cs

--         subst :: CVar -> Int -> Constraint -> Constraint
--         subst v i (E x y j) | v == y = (K x (i + j))
--         subst v i (Sub x (CVar y) z j) | v == y = (Sub x (CVal i) z j)--(E x z (i + j))
--         subst v i (Sub x y (CVar z) j) | v == z = 
--             case y of
--                 (CVar y) -> (E x y (j - i))
--                 (CVal k) -> (K x (k - i + j))
--         subst v i (Max x ys j) = 
--             let 
--                 ys' = (replace (cvarEq v) (CVal i) ys)
--             in 
--                 if all cval ys' then K x (j + (maximum $ (\(CVal k) -> k) <$> ys'))
--                 else (Max x ys' j)
--         -- Note that we have structured things in such a way that
--         -- -- we always hit the RHS of a LE before the right
--         subst v i (LE x xval (CVar y) j) | v == y = (LE x xval (CVal i) j)
--         subst v i (LE x (CVar xval) y j) | v == xval = (LE x (CVal i) y j)
--         -- TODO: Fill in the other cases
--         subst _ _ c = c

--         replace :: (a -> Bool) -> a -> [a] -> [a]
--         replace p f [] = []
--         replace p y (x:xs) = if p x then y:replace p y xs else x:replace p y xs

--         cvarEq :: CVar -> CVal -> Bool
--         cvarEq x (CVar y) = x == y
--         cvarEq _ _ = False

--         cval :: CVal -> Bool
--         cval (CVar _) = False
--         cval (CVal _) = True

-- genConstraints :: [Function] -> [Constraint]
-- genConstraints fs = join $ runCM $ traverse (\(Function n _ e) -> constraints e (Bound n)) fs