{-# LANGUAGE RankNTypes #-}
module Pixy.Delay
    (genConstraints)
where

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
    { fbyDepth :: Int
    , varDelays :: Map Var (FDExpr s)
    , whereVars :: [Var]
    , functions :: Map FName Function
    }

initState :: [Function] -> DelayState s
initState fs = DelayState
    { fbyDepth = 1
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
    wv <- asks whereVars
    if x `elem` wv
        then return (int $ 0)
        else return (int $ 1)

constraints :: Expr -> Delay s (FDExpr s)
constraints = \case
    (Var x) -> do
        vx <- getVar x
        d <- getVarDelay x
        return $ (vx + d)
    (Const k) -> return 0
    (If c t f) -> do
        cc <- constraints c
        tt <- constraints t
        ff <- constraints f
        return (cmax [cc, tt, ff])
    (Next e) -> (+1) <$> (constraints e)
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
    (Unary op e) -> constraints e
    where

        fbyRhs :: Delay s a -> Delay s a
        fbyRhs = local (\s -> s { fbyDepth = 1 + (fbyDepth s) })

        withVars :: [(Var, Expr)] -> ([(FDExpr s, Expr)] -> Delay s a) -> Delay s a
        withVars ve f = do
            vars <- lift $ lift $ news (length ve) (Domain.range 0 Domain.sup)
            tell vars
            let (vs, es) = unzip ve
            let delays = Map.fromList $ zip vs vars
            local (\s -> s { varDelays = Map.union delays (varDelays s) }) (f (zip vars es))

        whereConstraints :: (FDExpr s, Expr) -> Delay s ()
        whereConstraints (v, e) = do
            ee <- constraints e
            lift $ lift (v #== ee)

        whereBody :: [(Var, Expr)] -> Delay s a -> Delay s a
        whereBody bs = local (\s -> s { whereVars = fst <$> bs })

        fConstraints :: Function -> [(FDExpr s)] -> Delay s (FDExpr s)
        fConstraints (Function n argnames body) args = do
            retDelay <- lift $ lift $ new (Domain.range 0 Domain.sup)
            tell [retDelay]
            let vargs = Map.fromList $ zip argnames args
            bodyDelay <- local (\s -> s { varDelays = vargs, whereVars = argnames}) (constraints body)
            let argMax = case args of
                    [] -> 0
                    _ -> cmax args
            lift $ lift $ retDelay #== (bodyDelay - argMax)
            return bodyDelay

genConstraints :: [Function] -> Expr -> Maybe [Int]
genConstraints fs e = tryHead $ runFD $ do
    (mdelay, vars) <- runDelay (constraints e) fs
    mVar <- new (Domain.range 0 Domain.sup)
    mVar #== mdelay
    label (mVar:vars)
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
