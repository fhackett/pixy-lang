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
import Data.List (find)

import Pixy.Syntax
import CLPHS.FD
import qualified CLPHS.FD.Domain as Domain

import Debug.Trace

data DelayState s = DelayState
    { currentDelay :: Int
    , fbyDepth :: Int
    , varDelays :: Map Var (FDExpr s)
    , whereVars :: [Var]
    , functions :: Map FName Function
    }

initState :: [Function] -> DelayState s
initState fs = DelayState
    { currentDelay = 1
    , fbyDepth = 1
    , varDelays = Map.empty
    , whereVars = []
    , functions = Map.fromList $ (\f@(Function n _ _) -> (n,f)) <$> fs
    }
type Delay s a = (WriterT [FDExpr s] (ReaderT (DelayState s) (FD s)) a)

runDelay :: Delay s a -> [Function] -> FD s (a, [FDExpr s])
runDelay m fs = flip runReaderT (initState fs) $ runWriterT m

getVar :: Var -> Delay s (FDExpr s)
getVar x = do
    vm <- asks varDelays
    case vm !? x of
        Just vx -> return vx
        Nothing -> error $ "getVar: unbound variable " ++ x

bindVar :: Var -> FDExpr s -> Delay s a -> Delay s a
bindVar x e = local (\s -> s { varDelays = Map.insert x e (varDelays s) })
    -- vm <- get
    -- put $ Map.insert x e vm

getFunction :: FName -> Delay s Function
getFunction n = do
    fs <- asks functions
    case fs !? n of
        Just f -> return f
        Nothing -> error $ "getFunction: unbound variable " ++ n

getVarDelay :: Var -> Delay s (FDExpr s)
getVarDelay x = do
    s <- ask 
    return (int $ currentDelay s)
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

constraints :: Expr -> Delay s (FDExpr s)
constraints = \case
    (Var x) -> do
        vx <- getVar x
        d <- getVarDelay x
        return $ (vx + d)
    (Const k) -> return 0
    (Check e) -> constraints e
    (If c t f) -> do
        cc <- constraints c
        tt <- constraints t
        ff <- constraints f
        return (cmax [cc, tt, ff])
    (Next e) -> nextDepth (constraints e)
    (Fby l r) -> do
        ll <- constraints l
        rr <- fbyRhs (constraints r)
        d <- asks (int . fbyDepth)
        lift $ lift (rr #<= ll + d)
        return ll
    (Where body bs) -> withVars bs $ \vs -> do
        traverse_ whereConstraints vs
        whereBody bs (constraints body)
    (App n args) -> do
        f <- getFunction n
        vargs <- traverse constraints args
        fConstraints f vargs
    (Binop op l r) -> do
        ll <- constraints l
        rr <- constraints r
        return (cmax [ll, rr])
    where
        nextDepth :: Delay s a -> Delay s a
        nextDepth = local (\s -> s { currentDelay = 1 + (currentDelay s)})

        fbyRhs :: Delay s a -> Delay s a
        fbyRhs = local (\s -> s { fbyDepth = 1 + (fbyDepth s) })

        withVars :: [(Var, Expr)] -> ([(FDExpr s, Expr)] -> Delay s a) -> Delay s a
        withVars ve f = do
            vars <- lift $ lift $ news (length ve) (Domain.range 0 Domain.sup)
            tell vars
            let (vs, es) = unzip ve
            let delays = Map.fromList $ zip vs vars
            local (\s -> s { varDelays = Map.union delays (varDelays s) }) (f $ zip vars es)

        whereConstraints :: (FDExpr s, Expr) -> Delay s ()
        whereConstraints (v, e) = do
            ee <- constraints e
            lift $ lift (v #== ee)

        whereBody :: [(Var, Expr)] -> Delay s a -> Delay s a
        whereBody bs = local (\s -> s { whereVars = fst <$> bs })

        fConstraints :: Function -> [(FDExpr s)] -> Delay s (FDExpr s)
        fConstraints (Function n argnames body) args = 
            let vargs = Map.fromList $ zip argnames args
            in local (\s -> s { varDelays = Map.union vargs (varDelays s)}) (constraints body)

genConstraints :: [Function] -> Maybe [Int]
genConstraints fs = tryHead $ runFD $ do
    let (Just (Function _ _ body)) = find (\(Function n _ _) -> n == "main") fs
    (mdelay, vars) <- runDelay (constraints body) fs
    label vars
    where
        tryHead :: [a] -> Maybe a
        tryHead (x:_) = Just x
        tryHead [] = Nothing

{- 
When putting the delays on the tree, they almost need to be functions

some sort of onSolve callback would be very, very useful

The next step is to actually place the delays on the tree nodes
The important ones are 
1) the RHS of fbys
2) the delay of variables within a where

when evaluating, you choke evaluate, and count down the delay counter, until
the delay hits zero
-}
