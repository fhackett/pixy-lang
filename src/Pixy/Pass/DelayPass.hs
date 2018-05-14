{-# LANGUAGE GADTs #-}
module Pixy.Pass.DelayPass where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer hiding ((<>))
import Control.Monad.Except
import Control.Applicative (Alternative)
import Data.Bifunctor
import Data.Maybe (mapMaybe)
import Data.Foldable (traverse_, foldMap)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Graph as Graph
import Data.Map (Map, (!))
import Data.Set (Set)
import Data.Graph (Graph, Vertex)
import Data.List (find, foldl', sortBy, partition)
import Data.Array (assocs)

import Data.Text.Prettyprint.Doc

import Pixy.Data.Name
import Pixy.Data.Delay
import Pixy.Data.Subst

import Pixy.Syntax
import Pixy.Error
import Pixy.Parser.Parser
import Pixy.PrettyPrint

import Debug.Trace

data DelayEnv = DelayEnv 
    { varDelays :: Map Name Delay
    , fnDelays :: Map Name (Function DelayPass)
    , depth :: Int
    }

data Inferred = Inferred
    { inferredConstraints :: Constraints
    , inferredDelay :: Delay
    }

instance Monoid Inferred where
    mempty = Inferred { inferredConstraints = mempty, inferredDelay = 0 }
    mappend (Inferred cs1 d1) (Inferred cs2 d2) = Inferred { inferredConstraints = mappend cs1 cs2, inferredDelay = dmax [d1, d2]  }


data DelayError
    = CyclicConstraints [Constraint EQ]
    | UnsolvedVariables [Name]
    | ConstraintViolation (Constraint GEQ)

instance Error DelayError where
    toError (CyclicConstraints cs) = ErrorMessage { errorDescription = pretty "Cylic constraints:" <+> vcat (fmap pretty cs)} 
    toError (UnsolvedVariables x) = ErrorMessage { errorDescription = pretty "Unsolved variables:" <+> pretty x }
    toError (ConstraintViolation c) = ErrorMessage { errorDescription = pretty "Constraint violation:" <+> pretty c }


instance Substitutable Name Delay DelayEnv where
    apply s env = env { varDelays = fmap (apply s) (varDelays env)  }
    fv env = fv $ Map.elems (varDelays env)

instance Substitutable Name Delay DelayAnnName where
    apply s (DelayAnnName n t d) = (DelayAnnName n t (apply s d))
    fv (DelayAnnName _ _ d) = fv d

instance Substitutable Name Delay (Expr DelayPass) where
    apply s = \case
        (Var x) -> Var x
        (Const k) -> (Const k)
        (If c t f) -> If (apply s c) (apply s t) (apply s f)
        (Next e) -> Next (apply s e)
        (Fby l r) -> Fby (apply s l) (apply s r)
        (Where body bs) -> 
            let bs' = Map.map (apply s) $ Map.mapKeys (apply s) bs
            in Where (apply s body) bs'
        (App fname args) -> App fname (fmap (apply s) args)
        (Binop op l r) -> Binop op (apply s l) (apply s r)
        (Unary op e) -> Unary op (apply s e)
    fv = \case
        (Var x) -> Set.empty
        (Const k) -> Set.empty
        (If c t f) -> Set.unions [fv c, fv t, fv f]
        (Next e) -> fv e
        (Fby l r) -> Set.union (fv l) (fv r)
        (Where body bs) -> Set.unions $ (fv body):(fmap fv $ Map.elems bs)
        (App fname args) -> Set.unions (fmap fv args)
        (Binop _ l r) -> Set.union (fv l) (fv r)
        (Unary _ e) -> fv e

lookupVar :: (MonadReader DelayEnv m) => Name -> m Delay
lookupVar x = asks ((! x) . varDelays)

lookupFn :: (MonadReader DelayEnv m) => Name -> m (Function DelayPass)
lookupFn x = asks ((! x) . fnDelays)

addDepth :: (MonadReader DelayEnv m) => Int -> m a -> m a
addDepth k = local (\s -> s { depth = k + (depth s) })

withVars :: (MonadReader DelayEnv m) => Map Name Delay -> m a -> m a
withVars vs = local (\s -> s { varDelays = Map.union vs (varDelays s) })

addConstraint :: (MonadWriter Inferred m) => Constraint a -> m ()
addConstraint c@(_ :==: _) = tell $ Inferred { inferredConstraints =  Constraints { eqConstraints = [c], constraints = [] }, inferredDelay = 0 }
addConstraint c@(_ :>=: _) = tell $ Inferred { inferredConstraints =  Constraints { eqConstraints = [], constraints = [c] }, inferredDelay = 0 }

addConstraints :: (MonadWriter Inferred m) => Constraints -> m ()
addConstraints cs = tell $ Inferred { inferredConstraints = cs, inferredDelay = 0 }

withDelay :: (MonadWriter Inferred m) => Delay -> a -> m a
withDelay d a = writer (a, Inferred { inferredConstraints = mempty, inferredDelay = d })

getDelay :: (MonadWriter Inferred m) => m a -> m (a, Delay)
getDelay = listens inferredDelay

addDelay :: (MonadWriter Inferred m) => Delay -> m a -> m a
addDelay d = censor (\w -> w { inferredDelay = (d + inferredDelay w) })

maskDelay :: (MonadWriter Inferred m) => m a -> m a
maskDelay = censor (\w -> w { inferredDelay = 0 })

infer :: (MonadReader DelayEnv m, MonadWriter Inferred m, MonadError DelayError m) => Expr TypeCheckPass -> m (Expr DelayPass)
infer = \case
    (Var x) -> do
        vd <- lookupVar x
        d <- asks (toDelay . depth)
        withDelay vd $ Var x
    (Const k) -> return $ Const k
    (If c t f) -> If <$> infer c <*> infer t <*> infer f
    (Next e) -> Next <$> (addDelay 1 $ infer e)
    (Fby l r) -> do
        (l', ld) <- getDelay $ infer l
        (r', rd) <- maskDelay $ getDelay $ addDepth 1 $ infer r
        d <- asks (toDelay . depth)
        addConstraint ((ld + d) :>=: rd)
        return $ Fby l' r'
    (Where body bs) -> do
        let (vs, es) = (Map.keys bs, Map.elems bs)
        let varNames = fmap tyAnnName vs
        let annDelays = Map.fromList $ zip vs (fmap (fromLinearEq . var) varNames)
        let annVar = uncurry (zipWith fromTyAnnName) $ unzip $ Map.toList annDelays
        let delays = Map.mapKeys tyAnnName annDelays
        -- We only want to compute the delay of the body, so we mask the rest
        d <- asks depth
        es' <- maskDelay 
            $ withVars (fmap (+ 1) delays) 
            $ traverse whereVar 
            $ zip varNames es
        let bs' = Map.fromList $ zip annVar es'
        body' <- withVars delays $ infer body
        return $ Where body' bs'
    (App fname args) -> do
        fd <- lookupFn fname
        (annArgs, argDelays) <- unzip <$> (maskDelay $ traverse (getDelay . infer) args)
        let argSubst = Subst $ Map.fromList $ zip (fnArgs fd) argDelays
        -- Apply the argument substitution to the functions constraints, and then emit them
        let (_, fDelay, fCs) = fnInfo fd
        addConstraints (apply argSubst fCs)
        -- Compute the delay by applying the argument substitutions to the function delay
        let fd = apply argSubst fDelay
        withDelay fd $ App fname annArgs
    (Binop op l r) -> Binop op <$> infer l <*> infer r
    (Unary op e) -> Unary op <$> infer e

    where
        whereVar :: (MonadReader DelayEnv m, MonadWriter Inferred m, MonadError DelayError m) => (Name, Expr TypeCheckPass) -> m (Expr DelayPass)
        whereVar (d,e) = do
            (e', ed) <- getDelay $ infer e
            addConstraint (d :==: ed)
            return e'

solver :: (MonadError DelayError m) => Map Name Delay -> Constraints -> m (Subst Name Delay, Constraints)
solver args cs = do
    -- First we need to check that the constraints that aren't dependent on the arguments are valid
    -- We also need to get the required substitutions
    let (localCs, argCs) = splitConstraints cs
    s <- solveLocalConstraints localCs
    return (Subst $ Map.union (unSubst s) args, apply s argCs)
    where 
        splitConstraints :: Constraints -> (Constraints, Constraints)
        splitConstraints (Constraints ecs cs) = 
            let argSet = Map.keysSet args
                (localEcs, argEcs) =  partition (\e -> disjoint (fv e) (Set.fromList $ Map.keys args)) ecs
                (localCs, argCs) =  partition (\e -> disjoint (fv e) (Set.fromList $ Map.keys args)) cs
            in (Constraints localEcs localCs, Constraints argEcs argCs)

{-
    In order to determine if a given constraint set is solvable, we create a graph of variables
    where if the delay of a variable 'x' depends on the delay of another variable 'y', there is an edge from 'x' to 'y'
-}
buildConstraintGraph :: [Constraint EQ] -> (Graph, Vertex -> (Constraint EQ, Name, [Name]))
buildConstraintGraph ecs = 
    let fvs = fv ecs
        lowerBound = uniqueName $ minimum fvs 
        upperBound = uniqueName $ maximum fvs
    in Graph.graphFromEdges' $ fmap mkEdges ecs
    where
        mkEdges :: Constraint EQ -> (Constraint EQ, Name, [Name])
        mkEdges c@(x :==: e) = 
            let edges = Set.toList $ fv e
            in (c, x, edges)

cycles :: Graph -> [Vertex]
cycles graph =
  map fst . filter isCyclicAssoc . assocs $ graph
  where
    isCyclicAssoc = uncurry $ reachableFromAny graph

reachableFromAny :: Graph -> Vertex -> [Vertex] -> Bool
reachableFromAny graph node =
  elem node . concatMap (Graph.reachable graph)

solveLocalConstraints :: (MonadError DelayError m) => Constraints -> m (Subst Name Delay)
solveLocalConstraints (Constraints ecs cs) = do
    -- To solve the constraints, 1st build a graph for every variable in the equality constraints
    let (graph, index) = buildConstraintGraph ecs
    let cIndex v = let (c,_,_) = index v in c
    -- If the constraint graph is cyclic, that means we cannot solve it
    let cycs = cIndex <$> cycles graph
    unless (null cycs) (throwError $ CyclicConstraints cycs)
    -- To make sure that the constraints are solved in the proper order, topologically sort the constraint graph
    let tgraph = cIndex <$> (reverse $ Graph.topSort graph)
    -- We then have to simply reduce each constraint and substitute the value of the variable forward
    (dm, cs') <- simplify (Constraints tgraph cs)
    -- Once that is completed, check that all of the non-equality constraints hold
    traverse_ checkConstraint cs'
    return $ Subst $ fmap toDelay dm
    where
        simplify :: (MonadError DelayError m) => Constraints -> m (Map Name Int, [Constraint GEQ])
        simplify (Constraints [] cs) = return (Map.empty, cs)
        simplify (Constraints ((x :==: e):ecs) cs) = do
            (dm, cs') <- simplify $ apply (singletonSubst x e) (Constraints ecs cs)
            d <- reduce e
            return (Map.insert x d dm, cs')

        reduce :: (MonadError DelayError m) => Delay -> m Int
        reduce (Maximum eqs) = maximum <$> traverse solveEq eqs

        solveEq :: (MonadError DelayError m) => LinearEq Name Int -> m Int
        solveEq (LinearEq tms c) | Map.null tms = return c
                                 | otherwise = throwError $ UnsolvedVariables (Map.keys tms)

        checkConstraint :: (MonadError DelayError m) => Constraint GEQ -> m ()
        checkConstraint (x :>=: y) = do
            x' <- reduce x
            y' <- reduce y
            unless (x' >= y') (throwError $ ConstraintViolation (x :>=: y))

disjoint :: (Ord a) => Set a -> Set a -> Bool
disjoint a b = Set.null $ Set.intersection a b

functionDelay :: (MonadState (Map Name (Function DelayPass)) m, MonadError DelayError m) => Function TypeCheckPass -> m (Function DelayPass)
functionDelay f@(Function fname args body ty) = do
    let argDelays = Map.fromList $ fmap (\x -> (x, fromLinearEq $ var x)) args
    fDelays <- get
    let inferEnv = DelayEnv { varDelays = argDelays, fnDelays = fDelays, depth = 1 }
    (body', inferred) <- runWriterT $ runReaderT (infer body) inferEnv

    (subst, cs) <- solver argDelays (inferredConstraints inferred)
    let delay = apply subst (inferredDelay inferred)
    let bodyAnn = apply subst body' 

    let delayF = (Function fname args bodyAnn (ty, delay, cs))
    put (Map.insert fname delayF fDelays)
    return delayF

delayPass :: [Function TypeCheckPass] -> Either ErrorMessage [Function DelayPass]
delayPass fs = first toError $ evalStateT (traverse functionDelay fs) Map.empty
