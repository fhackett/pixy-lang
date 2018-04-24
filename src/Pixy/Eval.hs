{-# LANGUAGE BangPatterns #-}
module Pixy.Eval where

import Prelude hiding (init)
import qualified Prelude as P (init)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Data.Void
import Data.Bifunctor
import Data.Foldable
import Data.Ord (comparing)
import Data.Semigroup 
import Pixy.Syntax
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Sequence as Seq
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Sequence (Seq(..))
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

data InitState = InitState
    { delayStack :: [Int]
    , bufferSizes :: Map Var Int
    }

data InitReaderState = InitReaderState
    { functions :: [Function]
    , depth :: Int
    }

init :: (MonadReader InitReaderState m, MonadState InitState m) => Expr -> m ExprS
init = \case
    (Var x) -> do
        recordDepth x 
        return $ VarS x
    (Const k) -> return $ ConstS k
    (If c t f) -> IfS <$> init c <*> init t <*> init f
    (Fby l r) -> FbyS False <$> init l <*> init r
    (Next e) -> NextS <$> nextDepth (init e)
    (Where body bs) -> do
        bs' <- initWhereVars bs
        body' <- (init body)
        return $ WhereS body' bs'
    (App fname args) -> do
        f <- find (\(Function n _ _) -> n == fname) <$> asks functions
        args' <- init `mapM` args
        case f of
            Just (Function _ argNames e) -> AppS <$> init e <*> pure (Map.fromList $ zip argNames args')
            Nothing -> error $ "init: Undefined Function " ++ fname
    (Binop op l r) -> BinopS op <$> init l <*> init r
    (Unary op e) -> UnaryS op <$> init e
    where
        initVar :: (MonadReader InitReaderState m, MonadState InitState m) => (Var, Expr) -> m (Var, VarInfo)
        initVar (v,e) = do
            d <- popDelay
            e' <- (init e)
            return (v, VarInfo { varExpr = e', varDelay = d, varBuffer = Seq.singleton VNil })

        initWhereVars :: (MonadReader InitReaderState m, MonadState InitState m) => [(Var, Expr)] -> m (Map Var VarInfo)
        initWhereVars bs = do
            outerSizes <- gets bufferSizes
            bs' <- Map.fromList <$> traverse initVar bs
            sizes <- gets bufferSizes
            let definedSet = Set.fromList $ fst <$> bs
            modify (\s -> s { bufferSizes = Map.union (Map.withoutKeys sizes definedSet ) outerSizes })
            return $ Map.merge Map.preserveMissing Map.dropMissing (Map.zipWithMatched (\_ vi n -> vi { varBuffer = Seq.replicate (max n (length $ varBuffer vi)) VNil })) bs' sizes

        nextDepth :: (MonadReader InitReaderState m) => m a -> m a
        nextDepth = local (\s -> s { depth = (depth s) + 1})

        recordDepth :: (MonadReader InitReaderState m, MonadState InitState m) => Var -> m ()
        recordDepth x = do
            d <- asks depth
            modify (\s -> s { bufferSizes = Map.insertWith max x d (bufferSizes s) })

        popDelay :: (MonadState InitState m) => m Int
        popDelay = do
            d:ds <- gets delayStack
            modify (\s -> s { delayStack = ds })
            return d

runInit :: [Function] -> Expr -> ExprS
runInit fs e = case genConstraints fs e of
    Just cs -> flip evalState (InitState cs Map.empty) $ runReaderT (init e) (InitReaderState fs 1)

withBindings :: (MonadReader (Map Var (Seq Value)) m) => Map Var VarInfo -> m a -> m a
withBindings !bs = local (Map.union (fmap varBuffer bs))
-- withBindings bs = local (Map.union (Map.mapWithKey (\v vi -> let vb = varBuffer vi in trace (show v ++ ":" ++ show vb) vb) bs))

eval :: (MonadReader (Map Var (Seq Value)) m) => ExprS -> m (ExprS, Value)
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
        lookupValue :: (MonadReader (Map Var (Seq Value)) m) => Var -> m Value
        lookupValue x = do
            vm <- ask
            case Map.lookup x vm of
                Just (v :<| _) -> return v
                Just (_) -> error $ "lookupValue: Variable " ++ x ++ " has an empty buffer"
                Nothing -> error $ "lookupValue: Undefined variable " ++ x

        nextValue :: (MonadReader (Map Var (Seq Value)) m) => m a -> m a
        nextValue = local (fmap $ Seq.drop 1)

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

        evalUnary Not (VBool b) = VBool (not b)
        evalUnary Check v = VBool (v /= VNil)
        evalUnary Trace v = trace ("Trace:" ++ show v) v
        evalUnary op e = error $ "Operand Mismatch: " ++ show op ++ " " ++ show e

        argBindings :: Map Var (ExprS, Value) -> Map Var VarInfo
        argBindings = fmap (\(e,v) -> VarInfo e 0 $ Seq.singleton v) 

evalBinding :: (MonadReader (Map Var (Seq Value)) m) => Var -> VarInfo -> m VarInfo
evalBinding x (VarInfo e d vs) 
    | d > 0 = do
        e' <- chokeEval e
        return $ VarInfo e' (d - 1) vs
    | otherwise = do
        (e', v) <- eval e
        let vs' = v :<| (Seq.take (length vs - 1) vs)
        return $ VarInfo e' d vs'


chokeEval :: (MonadReader (Map Var (Seq Value)) m) => ExprS -> m ExprS
chokeEval = \case 
    (VarS x) -> return $ VarS x
    (ConstS k) -> return $ ConstS k
    (IfS c t f) -> IfS <$> chokeEval c <*> chokeEval t <*> chokeEval f
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
        argBindings = fmap (\e -> VarInfo e 0 $ Seq.singleton VNil)

evalLoop :: [Function] -> Expr -> [Value]
evalLoop fs e =
    let (Just cs) = genConstraints fs e
    in loop $ runInit fs e
    where
        loop :: ExprS -> [Value]
        loop s =
            let (s', v) = runReader (eval s) Map.empty
            in v:loop s'