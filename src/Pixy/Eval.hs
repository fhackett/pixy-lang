{-# LANGUAGE BangPatterns #-}
module Pixy.Eval where

import Prelude hiding (init)
import qualified Prelude as P (init)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.RWS.Strict
import Data.Void
import Data.Bifunctor
import Data.Foldable
import Data.Ord (comparing)
import Data.Maybe (fromMaybe) 
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Sequence as Seq
import Data.List (find)
import Data.Map.Strict (Map, (!))
import Data.Sequence (Seq(..), (!?))
import Data.Set (Set)
import System.Exit
--DEBUGGING
import Debug.Trace

import Pixy.Syntax
import Pixy.Delay
import Pixy.PrettyPrint
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
    }

data InitReaderState = InitReaderState
    { functions :: [Function]
    , fbyDepth :: Int
    , varDelays :: Map Var Int
    }

data InitWriter = InitWriter 
    { bufferSizes :: Map Var Int 
    }

instance Monoid InitWriter where
    mempty = InitWriter Map.empty
    mappend (InitWriter bx) (InitWriter by) = InitWriter (Map.unionWith max bx by) 

init :: (MonadRWS InitReaderState InitWriter InitState m) => Expr -> m ExprS
init = \case
    (Var x) -> do
        d <- getVarDepth x
        writer (VarS x (d - 1), InitWriter (Map.singleton x d))
    (Const k) -> return $ ConstS k
    (If c t f) -> IfS <$> init c <*> init t <*> init f
    (Fby l r) -> FbyS False <$> init l <*> (addDepth 1 $ init r)
    (Next e) -> addDepth (-1) (init e)
    (Where body bs) -> initWhere body bs
    (App fname args) -> do
        f <- find (\(Function n _ _) -> n == fname) <$> asks functions
        case f of
            Just f -> initApp f args
            Nothing -> error $ "init: Undefined Function " ++ fname
    (Binop op l r) -> BinopS op <$> init l <*> init r
    (Unary op e) -> UnaryS op <$> init e
    where
        initVar ::  (MonadRWS InitReaderState InitWriter InitState m) => (Var, Expr) -> m (Var, VarInfo)
        initVar (x,e) = do
            Just d <- asks (Map.lookup x . varDelays)
            e' <- addDepth d (init e)
            return (x, VarInfo { varExpr = e', varDelay = d, varBuffer = Seq.singleton VNil })

        getVarDelays :: (MonadRWS InitReaderState InitWriter InitState m) => [Var] -> m (Map Var Int)
        getVarDelays vs = Map.fromList <$> traverse (\v -> (v,) <$> popDelay) vs

        withVarDelays :: (MonadReader InitReaderState m) => Map Var Int -> m a -> m a
        withVarDelays vm = local (\s -> s { varDelays = Map.union vm (varDelays s) })


        initWhere :: (MonadRWS InitReaderState InitWriter InitState m) => Expr -> [(Var, Expr)] -> m ExprS
        initWhere body bs = do
            depth <- asks fbyDepth
            vd <- getVarDelays $ fmap fst bs
            (vm, varSizes) <- 
                listens bufferSizes 
                $ withVarDelays vd
                $ Map.fromList 
                <$> (addDepth (-depth) 
                $ traverse initVar bs)
            (body', bodySize) <- 
                listens bufferSizes 
                $ withVarDelays (fmap (\d -> d - 1) vd)
                $ init body
            let buffers = Map.unionWith max varSizes bodySize
            let definedSet = Set.fromList $ fst <$> bs
            let bs' = Map.merge Map.preserveMissing Map.dropMissing (Map.zipWithMatched (\_ vi n -> vi { varBuffer = mkBuffer n })) vm buffers
            censor (\w -> w { bufferSizes = Map.withoutKeys (bufferSizes w) definedSet }) (return $ WhereS body' bs')

        initApp :: (MonadRWS InitReaderState InitWriter InitState m) => Function -> [Expr] -> m ExprS
        initApp (Function fname argNames e) args = do
            fDepth <- popDelay
            depth <- asks fbyDepth
            as <- traverse (listens bufferSizes . addDepth (-fDepth) . init) args
            let argDelays = Map.fromList $ zip argNames $ repeat (depth - fDepth - 1)
            -- Compute the buffer size required by each argument
            (e' , argBuffers) <- 
                censor (const mempty) 
                $ listens (Map.filterWithKey (\k _ -> k `elem` argNames) . bufferSizes) 
                $ withVarDelays argDelays
                $ init e
            let argLevels = fmap (fromMaybe 0 . flip Map.lookup argBuffers) argNames
            let args' = fmap (\(e,b) -> VarInfo e 0 (mkBuffer b)) $ zip (fmap fst as) argLevels
            let argMap = Map.fromList $ zip argNames args'
            return (AppS e' argMap)


        mkBuffer :: Int -> Seq Value
        mkBuffer n = Seq.replicate n VNil

        addDepth :: (MonadReader InitReaderState m) => Int -> m a -> m a
        addDepth k = local (\s -> s { fbyDepth = (fbyDepth s) + k})

        getVarDepth :: (MonadReader InitReaderState m) => Var -> m Int
        getVarDepth x = do
            vd <- asks (fromMaybe 0 . (Map.lookup x) . varDelays)
            depth <- asks fbyDepth
            return (depth - vd)


        popDelay :: (MonadState InitState m) => m Int
        popDelay = do
            d:ds <- gets delayStack
            modify (\s -> s { delayStack = ds })
            return d

runInit :: [Function] -> Expr -> ExprS
runInit fs e = case genConstraints fs e of
    Just (c:cs) -> fst $ evalRWS (init e) (initReaderState c fs) (initState cs)
    Nothing -> error "runInit: Couldn't solve constraints" 
    where
        initReaderState d fs = InitReaderState { functions = fs, fbyDepth = d, varDelays = Map.empty }
        initState cs = InitState { delayStack = cs }

withBindings :: (MonadReader EvalState m) => Map Var VarInfo -> m a -> m a
withBindings bs = local (\s -> s { buffers = Map.union (fmap varBuffer bs) (buffers s) })

data EvalState = EvalState
    { buffers :: Map Var (Seq Value)
    , bufferIndex :: Int
    }

initEvalState = EvalState
    { buffers = Map.empty
    , bufferIndex = 0
    }

eval :: (MonadReader EvalState m) => ExprS -> m (ExprS, Value)
eval = \case
        (VarS x offset) -> (VarS x offset,) <$> lookupValue x offset
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
        (WhereS body bs) -> do
            bs' <- withBindings bs (Map.traverseWithKey evalBinding bs)
            (body', bodyVal) <- withBindings bs' (eval body)
            return (WhereS body' bs', bodyVal)
        (AppS f args) -> do
            args' <- Map.traverseWithKey evalBinding args
            (f', fVal) <- withBindings args' (eval f)
            return (AppS f' args', fVal)
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
        lookupValue :: (MonadReader EvalState m) => Var -> Int -> m Value
        lookupValue x offset = do
            vm <- asks buffers
            let buff = fromMaybe (error $ "lookupValue: No buffer for " ++ x) $ Map.lookup x vm 
            case buff !? offset of
                Just v -> return v
                Nothing -> error $ "lookupValue: Tried to lookup buffer for " ++ x ++ " at index " ++ show offset ++ " with buffer size " ++ (show $ Seq.length buff)

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

evalBinding :: (MonadReader EvalState m) => Var -> VarInfo -> m VarInfo
evalBinding x (VarInfo e d vs) 
    | d > 0 = do
        e' <- chokeEval e
        return $ VarInfo e' (d - 1) vs
    | otherwise = do
        (e', v) <- eval e
        let (vs' :|> _) = vs
        return $ VarInfo e' d (v :<| vs')


chokeEval :: (MonadReader EvalState m) => ExprS -> m ExprS
chokeEval = \case 
    (VarS x offset) -> return $ VarS x offset
    (ConstS k) -> return $ ConstS k
    (IfS c t f) -> IfS <$> chokeEval c <*> chokeEval t <*> chokeEval f
    (FbyS latch l r) -> FbyS latch <$> chokeEval l <*> chokeEval r
    (WhereS body bs) -> do
        bs' <- withBindings bs (Map.traverseWithKey evalBinding bs)
        body' <- withBindings bs' $ chokeEval body
        return $ WhereS body' bs'
    (AppS f args) -> do
        args' <-  Map.traverseWithKey evalBinding args
        f' <- withBindings args' (chokeEval f)
        return $ AppS f' args'
    (BinopS op l r) -> BinopS op <$> chokeEval l <*> chokeEval r
    (UnaryS op e) -> UnaryS op <$> chokeEval e

    where 
        chokeEvalArg :: (MonadReader EvalState m) => VarInfo -> m VarInfo
        chokeEvalArg vi = do
            e' <- chokeEval (varExpr vi)
            return $ vi { varExpr = e' }

evalLoop :: [Function] -> Expr -> [Value]
evalLoop fs e =
    let (Just cs) = genConstraints fs e
    in loop $ runInit fs e
    where
        loop :: ExprS -> [Value]
        loop s =
            let (!s', !v) = trace (pp s) runReader (eval s) initEvalState
            in v:loop s'