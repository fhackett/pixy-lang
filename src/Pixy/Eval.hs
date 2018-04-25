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
import Pixy.Syntax
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Sequence as Seq
import Data.List (find)
import Data.Map.Strict (Map, (!))
import Data.Sequence (Seq(..), (!?))
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
    }

data InitReaderState = InitReaderState
    { functions :: [Function]
    , fbyDepth :: Int
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
        d <- asks fbyDepth
        trace (x ++" Needs buffer of size " ++ show d) writer (VarS x (d - 1), InitWriter (Map.singleton x d))
    (Const k) -> return $ ConstS k
    (If c t f) -> IfS <$> init c <*> init t <*> init f
    (Fby l r) -> FbyS False <$> init l <*> (addDepth 1 $ init r)
    (Next e) -> addDepth (-1) (init e)
    (Where body bs) -> do
        bs' <- initWhereVars bs
        -- Prevent lookups from hitting the -1 index
        body' <- addDepth 1 $ init body
        return $ WhereS body' bs'
    (App fname args) -> do
        f <- find (\(Function n _ _) -> n == fname) <$> asks functions
        case f of
            Just f -> initApp f args
            Nothing -> error $ "init: Undefined Function " ++ fname
    (Binop op l r) -> BinopS op <$> init l <*> init r
    (Unary op e) -> UnaryS op <$> init e
    where
        initVar ::  (MonadRWS InitReaderState InitWriter InitState m) => (Var, Expr) -> m (Var, VarInfo)
        initVar (v,e) = do
            d <- popDelay
            e' <- addDepth d (init e)
            return (v, VarInfo { varExpr = e', varDelay = d, varBuffer = Seq.singleton VNil })

        initWhereVars :: (MonadRWS InitReaderState InitWriter InitState m) => [(Var, Expr)] -> m (Map Var VarInfo)
        initWhereVars bs = do
            (bs', sizes) <- listens bufferSizes $ Map.fromList <$> traverse initVar bs
            let definedSet = Set.fromList $ fst <$> bs
            trace ("Buffers:" ++ (show $ sizes)) return () 
            let vm = Map.merge Map.preserveMissing Map.dropMissing (Map.zipWithMatched (\_ vi n -> vi { varBuffer = mkBuffer n })) bs' sizes
            censor (\w -> w { bufferSizes = Map.withoutKeys (bufferSizes w) definedSet }) (return vm)

        initApp :: (MonadRWS InitReaderState InitWriter InitState m) => Function -> [Expr] -> m ExprS
        initApp (Function fname argNames e) args = do
            trace ("Initializing Function " ++ fname) return ()
            fDepth <- popDelay
            depth <- asks fbyDepth
            trace ("ExtraFunctionDepth: " ++ show fDepth) return ()
            trace ("Depth: " ++ show depth) return ()
            -- Get the Max Delay map from the body
            depth <- asks fbyDepth
            -- Compute the extra depths added by each argument
            (e' , argBuffers) <- censor (const mempty) $ listens (Map.filterWithKey (\k _ -> k `elem` argNames) . bufferSizes) $ addDepth (fDepth - depth + 1) $ init e
            let argLevels = fmap (fromMaybe 0 . flip Map.lookup argBuffers) argNames
            args' <- traverse (\(n, a, l) -> initArg n a fDepth l) $ zip3 argNames args argLevels
            let argMap = Map.fromList args'
            trace ("ArgBuffers:" ++ (show $ fmap varBuffer argMap)) return () 
            return (AppS e' argMap)

        initArg :: (MonadRWS InitReaderState InitWriter InitState m) => Var -> Expr -> Int -> Int -> m (Var, VarInfo)
        initArg x a fDepth argBuffer = do
            -- we need to subtract the delay of the argument from arg buffer size
            depth <- asks fbyDepth
            trace ("Evaluating Argument " ++ show a) return ()
            (a', sizes) <- listens bufferSizes $ addDepth (-fDepth) $ init a
            let d = maybe 0 snd $ Map.lookupMax sizes
            -- trace ("ArgBuffer " ++ x ++ " : " ++ show argBuffer) return ()
            return (x, VarInfo a' 0 (mkBuffer argBuffer))


        mkBuffer :: Int -> Seq Value
        mkBuffer n = Seq.replicate n VNil

        addDepth :: (MonadReader InitReaderState m) => Int -> m a -> m a
        addDepth k = local (\s -> s { fbyDepth = (fbyDepth s) + k})

        popDelay :: (MonadState InitState m) => m Int
        popDelay = do
            d:ds <- gets delayStack
            modify (\s -> s { delayStack = ds })
            return d

runInit :: [Function] -> Expr -> ExprS
runInit fs e = case genConstraints fs e of
    Just cs -> fst $ evalRWS (init e) (initReaderState fs) (initState cs)
    where
        initReaderState fs = InitReaderState { functions = fs, fbyDepth = 0 }
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
            let buff = vm ! x
            case buff !? offset of
                Just v -> return v
                Nothing -> error $ "lookupValue: Tried to lookup buffer for " ++ x ++ " at index " ++ show offset ++ " with buffer size " ++ (show $ Seq.length buff)

            -- let vs = (buffers s) ! x
            -- if (currentVar s) == x 
            --     then undefined
            --     else undefined

            -- case Map.lookup x vm of
            --     Just (vs) -> return v
            --     Just (_) -> error $ "lookupValue: Variable " ++ x ++ " has an empty buffer"
            -- --     Nothing -> error $ "lookupValue: Undefined variable " ++ x

        -- addIndex :: (MonadReader EvalState m) => Int -> m a -> m a
        -- addIndex k = local (\s -> s { bufferIndex =  + (bufferIndex s) })

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
        -- let vi = VarInfo e' d (v :<| vs')
        -- trace (x ++ " has Buffer " ++ show vs) return vi


chokeEval :: (MonadReader EvalState m) => ExprS -> m ExprS
chokeEval = \case 
    (VarS x offset) -> return $ VarS x offset
    (ConstS k) -> return $ ConstS k
    (IfS c t f) -> IfS <$> chokeEval c <*> chokeEval t <*> chokeEval f
    (FbyS latch l r) -> FbyS latch <$> chokeEval l <*> chokeEval r
    (WhereS body bs) -> WhereS <$> chokeEval body <*> withBindings bs (Map.traverseWithKey evalBinding bs)
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
            let (!s', !v) = runReader (eval s) initEvalState
            in v:loop s'