{-# LANGUAGE BangPatterns #-}
module Pixy.Eval where

import Prelude hiding (init)
import qualified Prelude as P (init)
import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Void
import Data.Bifunctor
import Data.Foldable
import Data.Ord (comparing)
import Data.Semigroup 
import Pixy.Syntax
import qualified Data.Map.Strict as Map
import Data.List (find)
import Data.Map (Map)
import System.Exit
--DEBUGGING
import Debug.Trace

import Pixy.Delay
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

newtype MaxNat = MaxNat { getMaxNat :: Int }
    deriving (Eq, Ord, Num)

instance Monoid MaxNat where
    mappend = max 
    mempty = 1

init :: (MonadRWS [Function] MaxNat [Int] m) => Expr -> m ExprS
init = \case
    (Var x) -> return $ VarS x
    (Const k) -> return $ ConstS k
    (If c t f) -> IfS <$> init c <*> init t <*> init f
    (Check e) -> CheckS <$> init e
    (Fby l r) -> FbyS False <$> init l <*> init r
    (Next e) -> NextS <$> censor (+1) (init e)
    (Where body bs) -> do
        bs' <- Map.fromList <$> traverse initVar bs
        (body', b) <- listen (init body)
        writer (WhereS body' bs', b)
    (App fname args) -> do
        f <- find (\(Function n _ _) -> n == fname) <$> ask
        args' <- init `mapM` args
        case f of
            Just (Function _ argNames e) -> AppS <$> init e <*> pure (Map.fromList $ zip argNames args')
            Nothing -> error $ "init: Undefined Function " ++ fname
    (Binop op l r) -> BinopS op <$> init l <*> init r
    (Unary op e) -> UnaryS op <$> init e
    where
        initVar :: (MonadRWS [Function] MaxNat [Int] m) => (Var, Expr) -> m (Var, VarInfo)
        initVar (v,e) = do
            d <- popDelay
            (e', b) <- listens getMaxNat (init e)
            return (v, VarInfo { varExpr = e', varDelay = d, varBuffer = replicate b VNil })

        popDelay :: (MonadState [Int] m) => m Int
        popDelay = do
            d:ds <- get
            put ds
            return d

runInit :: [Function] -> ExprS
runInit fs = case genConstraints fs of
    Just cs -> 
        let (Just (Function _ _ body)) = find (\(Function n _ _) -> n == "main") fs
        in fst $ evalRWS (init body) fs cs

withBindings :: (MonadReader (Map Var [Value]) m) => Map Var VarInfo -> m a -> m a
withBindings bs = local (Map.union (fmap varBuffer bs))
-- withBindings bs = local (Map.union (Map.mapWithKey (\v vi -> let vb = varBuffer vi in trace (show v ++ ":" ++ show vb) vb) bs))

eval :: (MonadReader (Map Var [Value]) m) => ExprS -> m (ExprS, Value)
eval = \case
        (VarS x) -> (VarS x,) <$> lookupValue x 
        (ConstS k) -> return (ConstS k, k)
        (IfS c t f) -> do
            (c',cVal) <- eval c
            case cVal of
                VBool True -> do
                    (t', tVal) <- eval t
                    f' <- chokeEval f
                    return (IfS c' t' f', tVal)
                VBool False -> do
                    t' <- chokeEval t
                    (f', fVal) <- eval f
                    return (IfS c' t' f', fVal)
                v -> error "Boolean Expected!"
        (CheckS e) -> do
            (e', eVal) <- eval e
            case eVal of
                VNil -> return (CheckS e', VBool False)
                _ -> return (CheckS e', VBool True)
        (FbyS latch l r) -> 
            if latch then do
                l' <- chokeEval l
                (r', rVal) <- eval r
                return (FbyS True l' r', rVal)
            else do
                (l', lVal) <- eval l
                r' <- chokeEval r
                case lVal of
                    VNil -> return (FbyS False l' r', VNil)
                    v -> return (FbyS True l' r', v)
        (NextS e) -> do
            (e', eVal) <- nextValue (eval e)
            return (NextS e', eVal)
        (WhereS body bs) -> do
            bs' <- withBindings bs (Map.traverseWithKey evalBinding bs)
            (body', bodyVal) <- withBindings bs' (eval body)
            return (WhereS body' bs', bodyVal)
        (AppS f args) -> do
            args' <- mapM eval args
            (f', fVal) <- withBindings (argBindings args') (eval f)
            return (AppS f' (fmap fst args'), fVal)
        (BinopS op l r) -> do
            (l', lVal) <- eval l
            (r', rVal) <- eval r
            let resVal = evalBinop op lVal rVal
            return (BinopS op l' r', resVal)
        (UnaryS op e) -> do
            (e', eVal) <- eval e
            let resVal = evalUnary op eVal
            return (UnaryS op e', resVal)
    where
        lookupValue :: (MonadReader (Map Var [Value]) m) => Var -> m Value
        lookupValue x = do
            vm <- ask
            case Map.lookup x vm of
                Just (v:_) -> return v
                Just ([]) -> error $ "lookupValue: Variable " ++ x ++ " has an empty buffer"
                Nothing -> error $ "lookupValue: Undefined variable " ++ x

        nextValue :: (MonadReader (Map Var [Value]) m) => m a -> m a
        nextValue = local (fmap tail)

        evalBinop :: Binop -> Value -> Value -> Value
        evalBinop Plus (VInt i) (VInt j) = VInt (i + j)
        evalBinop Minus (VInt i) (VInt j) = VInt (i - j)
        evalBinop Times (VInt i) (VInt j) = VInt (i * j)
        evalBinop Divide (VInt i) (VInt j) = VInt (i `div` j)
        evalBinop Modulo (VInt i) (VInt j) = VInt (i `mod` j)
        evalBinop Or (VBool True) _ = VBool True
        evalBinop Or (VBool False) (VBool j) = VBool j
        evalBinop And (VBool False) _ = VBool False
        evalBinop And (VBool True) (VBool j) = VBool j
        evalBinop Equals (VInt i) (VInt j) = VBool (i == j)
        evalBinop NotEquals (VInt i) (VInt j) = VBool (i /= j)
        evalBinop LessThanEquals (VInt i) (VInt j) = VBool (i <= j)
        evalBinop LessThan (VInt i) (VInt j) = VBool (i < j)
        evalBinop GreaterThanEquals (VInt i) (VInt j) = VBool (i >= j)
        evalBinop GreaterThan (VInt i) (VInt j) = VBool (i > j)
        evalBinop op lVal rVal = error $ "Operand Mismatch: " ++ show op ++ " " ++ show lVal ++ " " ++ show rVal --throwError $ OperandMismatch op lVal rVal
                -- (op, lVal, rVal) -> 
                -- (Modulo, VInt i, VInt j) -> VInt (i `mod` j)
                -- (Equals, VInt i, VInt j) -> VBool (i == j)
                -- (Equals, VInt i, VInt j) -> VBool (i == j)
                -- (_, VNil, VNil) -> il
        evalUnary Not (VBool b) = VBool (not b)
        evalUnary Trace v = trace ("Trace:" ++ show v) v
        evalUnary op e = error $ "Operand Mismatch: " ++ show op ++ " " ++ show e

        argBindings :: Map Var (ExprS, Value) -> Map Var VarInfo
        argBindings = fmap (\(e,v) -> VarInfo e 0 [v]) 

evalBinding :: (MonadReader (Map Var [Value]) m) => Var -> VarInfo -> m VarInfo
evalBinding x (VarInfo e d vs) 
    | d > 0 = return $ VarInfo e (d - 1) vs
    | otherwise = do
        (e', v) <- local (Map.update (Just . reverse) x) (eval e)
        let vs' = (v:P.init vs)
        return $ VarInfo e' d vs'


chokeEval :: (MonadReader (Map Var [Value]) m) => ExprS -> m ExprS
chokeEval = \case 
    (VarS x) -> return $ VarS x
    (ConstS k) -> return $ ConstS k
    (IfS c t f) -> IfS <$> chokeEval c <*> chokeEval t <*> chokeEval f
    (CheckS e) -> CheckS <$> chokeEval e
    (FbyS latch l r) -> FbyS latch <$> chokeEval l <*> chokeEval r
    (NextS e) -> NextS <$> chokeEval e
    (WhereS body bs) -> WhereS <$> chokeEval body <*> withBindings bs (Map.traverseWithKey evalBinding bs)
    (AppS f args) -> do
        args' <- mapM chokeEval args
        f' <- withBindings (argBindings args') (chokeEval f)
        return $ AppS f' args'
    (BinopS op l r) -> BinopS op <$> chokeEval l <*> chokeEval r
    (UnaryS op e) -> UnaryS op <$> chokeEval e
    where
        argBindings :: Map Var ExprS -> Map Var VarInfo
        argBindings = fmap (\e -> VarInfo e 0 [VNil])

evalLoop :: [Function] -> Expr -> [Value]
evalLoop fs e =
    let (Just cs) = genConstraints fs
        (Just (Function _ _ body)) = find (\(Function n _ _) -> n == "main") fs
    in loop $ fst $ evalRWS (init e) fs cs
    where
        loop :: ExprS -> [Value]
        loop s =
            let (s', v) = runReader (eval s) Map.empty
            in v:loop s'