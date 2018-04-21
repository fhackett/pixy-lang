{-# LANGUAGE RankNTypes #-}
module Pixy.Delay where

import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad.Except
import Control.Applicative (Alternative)
import Data.Maybe (mapMaybe)
import Data.Foldable (traverse_)

import qualified Data.Map as Map
import Data.Map (Map, (!?))
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Data.List (find)

import Pixy.Syntax
import Pixy.Solver.FD
import qualified Pixy.Solver.Domain as Domain

data DelayState = DelayState
    { currentDelay :: Int
    , fbyDepth :: Int
    , whereVars :: [Var]
    , functions :: Map FName Function
    }

initState :: [Function] -> DelayState
initState fs = DelayState
    { currentDelay = 1
    , fbyDepth = 1
    , whereVars = []
    , functions = Map.fromList $ (\f@(Function n _ _) -> (n,f)) <$> fs
    }
type Delay s a = (ReaderT DelayState (StateT (Map Var (FDExpr s)) (FD s)) a)

runDelay :: Delay s a -> [Function] -> FD s (a, Map Var (FDExpr s))
runDelay m fs = flip runStateT Map.empty $ runReaderT m (initState fs)

getVar :: Var -> Delay s (FDExpr s)
getVar x = do
    vm <- get
    case vm !? x of
        Just vx -> return vx
        Nothing -> error $ "getVar: unbound variable " ++ x

putVar :: Var -> FDExpr s -> Delay s ()
putVar x e = do
    vm <- get
    put $ Map.insert x e vm

getFunction :: FName -> Delay s Function
getFunction n = do
    fs <- asks functions
    case fs !? n of
        Just f -> return f
        Nothing -> error $ "getFunction: unbound variable " ++ n

varDelay :: Var -> Delay s (FDExpr s)
varDelay x = do
    s <- ask
    if x `elem` whereVars s
        then return (int $ currentDelay s - 1)
        else return (int $ currentDelay s)

-- init :: (MonadReader [Function] m, MonadError EvalError m) => Expr -> m ExprS
-- init (Var x) = return $ VarS x
-- init (Const k) = return $ ConstS k
-- init (If c t f) = IfS <$> init c <*> init t <*> init f
-- init (Check e) = CheckS <$> init e
-- init (Fby l r) = FbyS False <$> init l <*> init r
-- init (Next e) = NextS False VNil <$> init e
-- init (Where body bs) = do
--     bs' <- mapM init $ Map.fromList bs
--     body' <- init body
--     return $ WhereS body' ((,VNil) <$> bs')
-- init (App fname args) = do
--     f <- find (\(Function n _ _) -> n == fname) <$> ask
--     args' <- init `mapM` args
--     case f of
--         Just (Function _ argNames e) -> AppS <$> init e <*> pure (Map.fromList $ zip argNames args')
--         Nothing -> throwError $ UndefinedFunction fname
-- init (Binop b l r) = BinopS b <$> init l <*> init r

constraints :: Expr -> Delay s (FDExpr s, ExprS)
constraints = \case
    (Var x) -> do
        vx <- getVar x
        d <- varDelay x
        return $ (vx + d, VarS x)
    (Const k) -> return (0, ConstS k)
    (Check e) -> constraints e
    (If c t f) -> do
        (cc, c') <- constraints c
        (tt, t') <- constraints t
        (ff, f') <- constraints f
        return (cmax [cc, tt, ff], IfS c' t' f')
    (Next e) -> nextDepth (constraints e)
    (Fby l r) -> do
        (ll, l') <- constraints l
        (rr, r') <- fbyRhs (constraints r)
        d <- asks (int . fbyDepth)
        lift $ lift (rr #<= ll + d)
        return (ll, FbyS undefined l' r')
    (Where body bs) -> do
        bs' <- Map.fromList <$> (traverse whereConstraints =<< traverse bindVar bs)
        (bodyDelay, body') <- whereBody bs (constraints body)
        return (bodyDelay, WhereS body' bs')
    (App n args) -> do
        f <- getFunction n
        vargs <- traverse constraints args
        fConstraints f vargs
    (Binop op l r) -> do
        (ll, l') <- constraints l
        (rr, r') <- constraints r
        return (cmax [ll, rr], BinopS op l' r')
    where
        nextDepth :: Delay s a -> Delay s a
        nextDepth = local (\s -> s { currentDelay = 1 + (currentDelay s)})

        fbyRhs :: Delay s a -> Delay s a
        fbyRhs = local (\s -> s { fbyDepth = 1 + (fbyDepth s) })

        bindVar :: (Var, Expr) -> Delay s (Var, FDExpr s, Expr)
        bindVar (v, e) = do
            v' <- lift $ lift $ new $ Domain.range 0 Domain.sup
            putVar v v'
            return (v, v', e)

        whereConstraints :: (Var, FDExpr s, Expr) -> Delay s (Var, (ExprS, Int, Value))
        whereConstraints (v, v', e) = do
            (ee, e') <- constraints e
            lift $ lift (v' #== ee)
            return (v, (e', undefined, VNil))

        whereBody :: [(Var, Expr)] -> Delay s a -> Delay s a
        whereBody bs = local (\s -> s { whereVars = fst <$> bs })

        fConstraints :: Function -> [(FDExpr s, ExprS)] -> Delay s (FDExpr s, ExprS)
        fConstraints (Function n argnames body) args = do
            let vargs = Map.fromList $ zip argnames args
            s <- get
            put $ Map.union (fmap fst vargs) s
            (fdelay, body') <- constraints body
            return (fdelay, AppS body' (fmap snd vargs))

genConstraints :: [Function] -> Maybe [(Var, Int)]
genConstraints fs = tryHead $ runFD $ do
    let (Just (Function _ _ body)) = find (\(Function n _ _) -> n == "main") fs
    (mdelay, vars) <- runDelay (constraints body) fs
    -- mdelay #== 0
    let (vs, es) = unzip $ Map.assocs vars
    zip vs <$> label es
    where
        tryHead :: [a] -> Maybe a
        tryHead (x:_) = Just x
        tryHead [] = Nothing


{- 
The next step is to actually place the delays on the tree nodes
The important ones are 
1) the RHS of fbys
2) the delay of variables within a where

when evaluating, you choke evaluate, and count down the delay counter, until
the delay hits zero
-}
