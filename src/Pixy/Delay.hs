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
import Data.Map (Map, (!))
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Data.List (foldl1)

import Pixy.Syntax
import Pixy.Solver.FD
import qualified Pixy.Solver.Domain as Domain

data DelayState = DelayState
    { currentDelay :: Int
    , fbyDepth :: Int
    }

initState :: DelayState
initState = DelayState
    { currentDelay = 1
    , fbyDepth = 1
    }
type Delay s a = (ReaderT DelayState (StateT (Map Var (FDExpr s)) (FD s)) a)

runDelay :: Delay s a -> FD s (a, Map Var (FDExpr s))
runDelay m = flip runStateT Map.empty $ runReaderT m initState

getVar :: Var -> Delay s (FDExpr s)
getVar x = do
    vm <- get
    return $ vm ! x

putVar :: Var -> FDExpr s -> Delay s ()
putVar x e = do
    vm <- get
    put $ Map.insert x e vm

constraints :: Expr -> Delay s (FDExpr s)
constraints = \case
    (Var x) -> do
        d <- asks (int . currentDelay)
        vx <- getVar x
        return $ vx + d
    (Const _) -> return 0
    (Check e) -> constraints e
    (If c t f) -> do
        cc <- constraints c
        tt <- constraints t
        ff <- constraints f
        return $ cmax cc (cmax tt ff)
    (Next e) -> nextDepth (constraints e)
    (Fby l r) -> do
        ll <- constraints l
        rr <- fbyRhs (constraints r)
        d <- asks (int . fbyDepth)
        lift $ lift (rr #<= ll + d)
        return ll
    (Where body bs) -> do
        traverse_ whereConstraints =<< traverse bindVar bs
        constraints body
    (App _ _ ) -> return 0
    (Binop _ l r) -> do
        ll <- constraints l
        rr <- constraints r
        return $ cmax ll rr
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

genConstraints :: Function -> Maybe [Int]
genConstraints (Function n args body) = tryHead $ runFD $ do
    vargs <- news (length args) (Domain.range 0 10000)
    (_, vars) <- runDelay $ constraints body
    return undefined
    label (vargs ++ Map.elems vars)
    where
        tryHead :: [a] -> Maybe a
        tryHead (x:_) = Just x
        tryHead [] = Nothing