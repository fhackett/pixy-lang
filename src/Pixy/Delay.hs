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

constraints :: Expr -> Delay s (FDExpr s)
constraints = \case
    (Var x) -> do
        vx <- getVar x
        d <- varDelay x
        return $ vx + d
    (Const _) -> return 0
    (Check e) -> constraints e
    (If c t f) -> do
        cc <- constraints c
        tt <- constraints t
        ff <- constraints f
        return $ cmax [cc, tt, ff]
    (Next e) -> nextDepth (constraints e)
    (Fby l r) -> do
        ll <- constraints l
        rr <- fbyRhs (constraints r)
        d <- asks (int . fbyDepth)
        lift $ lift (rr #<= ll + d)
        return ll
    (Where body bs) -> do
        traverse_ whereConstraints =<< traverse bindVar bs
        whereBody bs (constraints body)
    (App n args) -> do
        f <- getFunction n
        vargs <- traverse constraints args
        fConstraints f vargs
    (Binop _ l r) -> do
        ll <- constraints l
        rr <- constraints r
        return $ cmax [ll, rr]
    where
        nextDepth :: Delay s a -> Delay s a
        nextDepth = local (\s -> s { currentDelay = 1 + (currentDelay s)})

        fbyRhs :: Delay s a -> Delay s a
        fbyRhs = local (\s -> s { fbyDepth = 1 + (fbyDepth s) })

        bindVar :: (Var, Expr) -> Delay s (FDExpr s, Expr)
        bindVar (v, e) = do
            v' <- lift $ lift $ new $ Domain.range 0 Domain.sup
            putVar v v'
            return (v', e)

        whereConstraints :: (FDExpr s, Expr) -> Delay s ()
        whereConstraints (v,e) = do
            ee <- constraints e
            lift $ lift (v #== ee)

        whereBody :: [(Var, Expr)] -> Delay s a -> Delay s a
        whereBody bs = local (\s -> s { whereVars = fst <$> bs })

        fConstraints :: Function -> [FDExpr s] -> Delay s (FDExpr s)
        fConstraints (Function n argnames body) args = do
            let vargs = Map.fromList $ zip argnames args
            s <- get
            put $ Map.union vargs s
            constraints body

genConstraints :: [Function] -> Maybe [(Var, Int)]
genConstraints fs = tryHead $ runFD $ do
    let (Just (Function _ _ body)) = find (\(Function n _ _) -> n == "main") fs
    (mdelay, vars) <- runDelay (constraints body) fs
    mdelay #== 0
    let (vs, es) = unzip $ Map.assocs vars
    zip vs <$> label es
    where
        tryHead :: [a] -> Maybe a
        tryHead (x:_) = Just x
        tryHead [] = Nothing


-- To compute delays for the entire program, you need to start at the main
-- function and walk through
-- when you hit a function application, you compute the delay of the function,
-- binding the arguments to their actual values
-- The next step is to acutally place the delays on the tree nodes