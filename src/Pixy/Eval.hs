module Pixy.Eval where

import Prelude hiding (init, max)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.Void
import Data.Bifunctor
import Data.Foldable
import Data.Ord (comparing)
import Pixy.Syntax
import qualified Data.Map.Strict as Map
import Data.List (find)
import Data.Map (Map)
import System.Exit
--DEBUGGING
import Debug.Trace

{-
The pixy evaluation model is as follows:
Set up the initial state
Continue looping, passing in the initial state to create a new state
We use some tricks to represent the state directly on the AST itself
-}


data EvalError
    = UndefinedVariable Var
    | UndefinedFunction FName
    | BooleanExpected Value
    | NilExpected Value
    | OperandMismatch Binop Value Value
    | DivideByZero
    deriving (Show)

data ConstraintError
    = ConstraintMismatch CVar Int Int
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

max :: CVar -> [CVar] -> Int -> Constraint
max x ys i = Max x (fmap CVar ys) i

le :: CVar -> CVar -> Int -> Constraint
le x y i = LE x (CVar x) (CVar y) i

sub :: CVar -> CVar -> CVar -> Int -> Constraint
sub x y z i = Sub x (CVar y) (CVar z) i

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

type CM = ReaderT [CVar] (State Int)

runCM :: CM a -> a
runCM = flip evalState 0 . flip runReaderT []

isRec :: (MonadReader [CVar] m) => CVar -> m Bool
isRec v = asks (elem v)

constraints :: (MonadState Int m, MonadReader [CVar] m) => Expr -> CVar -> m [Constraint]
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
        rr <- local (v:) (constraints r rhs)
        return $ [sub v lhs rhs 1, le rhs lhs 1] ++ ll ++ rr
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
reduce :: [Constraint] -> Either ConstraintError [Constraint]
reduce = reduce' . reverse
    where

        reduce' :: [Constraint] -> Either ConstraintError [Constraint]
        reduce' [] = return []
        reduce' (c:cs) = case c of
            (K v i) -> do
                cs' <- reduce' $ subst v i <$> cs
                case v of
                    (Bound _) -> return $ c:cs'
                    (Gen _ _) -> return cs'
            (LE x (CVal i) (CVal j) k) ->
                if (i <= j + k) then reduce cs 
                else throwError $ ConstraintMismatch x i j
            _ -> (c:) <$> reduce' cs

        subst :: CVar -> Int -> Constraint -> Constraint
        subst v i (E x y j) | v == y = (K x (i + j))
        subst v i (Sub x (CVar y) z j) | v == y = (Sub x (CVal i) z j)--(E x z (i + j))
        subst v i (Sub x y (CVar z) j) | v == z = 
            case y of
                (CVar y) -> (E x y (j - i))
                (CVal k) -> (K x (k - i + j))
        -- Note that we have structured things in such a way that
        -- -- we always hit the RHS of a LE before the right
        subst v i (LE x xval (CVar y) j) | v == y = (LE x xval (CVal i) j)
        subst v i (LE x (CVar xval) y j) | v == xval = (LE x (CVal i) y j)
        -- TODO: Fill in the other cases
        subst _ _ c = c

init :: (MonadReader [Function] m, MonadError EvalError m) => Expr -> m ExprS
init (Var x) = return $ VarS x
init (Const k) = return $ ConstS k
init (If c t f) = IfS <$> init c <*> init t <*> init f
init (Check e) = CheckS <$> init e
init (Fby l r) = FbyS False <$> init l <*> init r
init (Next e) = NextS False VNil <$> init e
init (Where body bs) = do
    bs' <- mapM init $ Map.fromList bs
    body' <- init body
    return $ WhereS body' ((,VNil) <$> bs')
init (App fname args) = do
    f <- find (\(Function n _ _) -> n == fname) <$> ask
    args' <- init `mapM` args
    case f of
        Just (Function _ argNames e) -> AppS <$> init e <*> pure (Map.fromList $ zip argNames args')
        Nothing -> throwError $ UndefinedFunction fname
init (Binop b l r) = BinopS b <$> init l <*> init r

lookupValue :: (MonadReader (Map Var Value) m, MonadError EvalError m) => Var -> m Value
lookupValue v = do
    res <- asks (Map.lookup v)
    case res of
        Just val -> return val
        Nothing -> throwError $ UndefinedVariable v

withBindings :: (MonadReader (Map Var Value) m) => Map Var (ExprS, Value) -> m a -> m a
withBindings bs = local (Map.union (fmap snd bs))

eval :: (MonadReader (Map Var Value) m, MonadError EvalError m) => ExprS -> m (ExprS, Value)
eval (VarS x) = (VarS x,) <$> lookupValue x
eval (ConstS k) = return (ConstS k, k)
eval (IfS c t f) = do
    (c', cVal) <- eval c
    case cVal of
        VBool True -> do
            (t', tVal) <- eval t
            f' <- chokeEval f
            return (IfS c' t' f', tVal)
        VBool False -> do
            t' <- chokeEval t
            (f', fVal) <- eval f
            return (IfS c' t' f', fVal)
        v -> throwError $ BooleanExpected v
eval (CheckS e) = do
    (e', eVal) <- eval e
    case eVal of
        VNil -> return (CheckS e', VBool False)
        _ -> return (CheckS e', VBool True)
eval (FbyS latch l r) =
    if latch then do
        l' <- chokeEval l
        (r', rVal) <- eval r
        return (FbyS True l' r', rVal)
    else do
        (l', lVal) <- eval l
        (r') <- chokeEval r
        case lVal of
            VNil -> return (FbyS False l' r', VNil)
            v -> return (FbyS True l' r', v)
eval (NextS latch vBuffer e) =
    if latch then do
        (e', eVal) <- eval e
        -- TODO: Ask Finn if a nil should disable buffering forever
        case vBuffer of
            VNil -> return (NextS True VNil e', VNil)
            v -> return (NextS True eVal e', v)
    else do
        (e', eVal) <- eval e
        case eVal of
            VNil -> return (NextS False VNil e', VNil)
            v -> return (NextS True v e', VNil)
eval (WhereS body bs) = do
    bs' <- withBindings bs (mapM (eval . fst) bs)
    (body', bodyVal) <- withBindings bs' (eval body)
    return (WhereS body' bs', bodyVal)
eval (AppS f args) = do
    args' <- mapM eval args
    (f', fVal) <- withBindings args' (eval f)
    return (AppS f' (fmap fst args'), fVal)
eval (BinopS op l r) = do
    (l', lVal) <- eval l
    (r', rVal) <- eval r
    resVal <- case (op, lVal, rVal) of
        (Plus, VInt i, VInt j) -> return $ VInt (i + j)
        (Minus, VInt i, VInt j) -> return $ VInt (i - j)
        (Times, VInt i, VInt j) -> return $ VInt (i * j)
        (Divide, VInt i, VInt j)  ->
            if j /= 0 then return $ VInt (i `div` j)
            else throwError $ DivideByZero
        (Equals, VInt i, VInt j) -> return $ VBool (i == j)
        (_, VNil, VNil) -> return VNil
        (op, lVal, rVal) -> throwError $ OperandMismatch op lVal rVal
    return (BinopS op l' r', resVal)




chokeEval :: (MonadReader (Map Var Value) m, MonadError EvalError m) => ExprS -> m (ExprS)
chokeEval e = do
    (e', eVal) <- chokeEval' e
    case eVal of
        VNil -> return e'
        v -> throwError $ NilExpected v
        where
            chokeEval' :: (MonadReader (Map Var Value) m, MonadError EvalError m) => ExprS -> m (ExprS, Value)
            chokeEval' (VarS x) = return (VarS x, VNil)
            chokeEval' (ConstS k) = return (ConstS k, VNil)
            chokeEval' (IfS c t f) = choked (IfS <$> chokeEval c <*> chokeEval t <*> chokeEval f)
            chokeEval' (CheckS e) = choked (CheckS <$> chokeEval e)
            chokeEval' (FbyS latch l r) = choked (FbyS latch <$> chokeEval l <*> chokeEval r)
            chokeEval' (NextS latch vBuffer e) = choked (NextS latch vBuffer <$> chokeEval e)
            chokeEval' (WhereS body bs) = do
                bs' <- withBindings bs (mapM (eval . fst) bs)
                body' <- chokeEval body
                return (WhereS body' bs', VNil)
            chokeEval' (AppS f args) = do
                args' <- mapM chokeEval args
                f' <- withBindings (choked args') (chokeEval f)
                return (AppS f' args', VNil)
            chokeEval' (BinopS op l r) = choked (BinopS op <$> chokeEval l <*> chokeEval r)


            choked :: (Functor f) => f ExprS -> f (ExprS, Value)
            choked = fmap (,VNil)

type Eval r = ReaderT r (Except EvalError)

runEval :: Eval r a -> r -> Either EvalError a
runEval m r = runExcept $ runReaderT m r

evalLoop :: [Function] -> Expr -> [Either EvalError Value]
evalLoop fs e =
    case runEval (init e) fs of
        Left err -> [Left err]
        Right s -> loop s
    where
        loop :: ExprS -> [Either EvalError Value]
        loop s =
            case runEval (eval s) Map.empty of
                Left err -> [Left err]
                Right (s',v) -> (Right v):loop s'
        -- loop s = (\(s',v) -> loop s') =<< runEval (eval s) (Map.empty)

genConstraints :: [Function] -> [Constraint]
genConstraints fs = join $ runCM $ traverse (\(Function n _ e) -> constraints e (Bound n)) fs

