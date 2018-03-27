module Pixy.Eval where

import Prelude hiding (init)
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

data CVar
    = Gen String !Int
    | Bound Var
    deriving (Eq, Show)

data Constraint
    = K CVar !Int -- ^ The variable is constrained to a constant delay
    | E CVar CVar !Int -- ^ The variable is constrained to be equal to another variable with an offset
    | LE CVar CVar !Int -- ^ The variable is constrained to be less than or equal to another variable with an offset
    | Max CVar CVar CVar !Int
    deriving (Show)


-- | Increments the delay of a constraint
increment :: Constraint -> Constraint
increment (K v i) = K v (i + 1)
increment (E x y i) = E x y (i + 1)
increment (LE x y i) = LE x y (i + 1)
increment (Max x y z i) = Max x y z (i + 1)

constrained :: Constraint -> CVar
constrained (K v _) = v
constrained (E x _ _) = x
constrained (LE x _ _) = x
constrained (Max x _ _ _) = x

genCVar :: String -> State Int CVar
genCVar s = do
    k <- get
    put (k + 1)
    return $ Gen s k

constraints :: Expr -> CVar -> State Int [Constraint]
constraints e v = case e of
    (Var x) -> return [E v (Bound x) 1]
    (Const _) -> return [K v 0]
    (If c t f) -> do
        cc <- constraints c v
        tt <- constraints t v
        ff <- constraints f v
        return $ cc ++ tt ++ ff
    (Check e) -> constraints e v
    (Next e) -> fmap (\c -> if constrained c == v then increment c else c) <$> constraints e v
    (Fby l r) -> do
        lhs <- genCVar "lhs"
        rhs <- genCVar "rhs"
        ll <- constraints l lhs
        rr <- constraints r rhs
        return $ [E v lhs 0] ++ ll ++ rr ++ [LE rhs lhs 1]
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
        return $ [Max v lhs rhs 0] ++ cl ++ cr



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
            case runEval (eval s) (Map.empty) of
                Left err -> [Left err]
                Right (s',v) -> (Right v):loop s'
        -- loop s = (\(s',v) -> loop s') =<< runEval (eval s) (Map.empty)

genConstraints :: [Function] -> [Constraint]
genConstraints fs = join $ fst $ (flip runState 0) $ traverse (\(Function n _ e) -> constraints e (Bound n)) fs

