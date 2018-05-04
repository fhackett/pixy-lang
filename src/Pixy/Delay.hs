{-# LANGUAGE RankNTypes #-}
module Pixy.Delay
where

import Control.Monad.RWS hiding ((<>))
import Control.Monad.Except
import Control.Applicative (Alternative)
import Data.Maybe (mapMaybe)
import Data.Foldable (traverse_)

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map, (!?))
import Data.Set (Set)
import Data.List (find, foldl')

import Pixy.Syntax
import Pixy.Parser.Parser
import Pixy.PrettyPrint
import Text.PrettyPrint
import CLPHS.FD
import qualified CLPHS.FD.Domain as Domain

import Debug.Trace



-- data DelayState s = DelayState
--     { fbyDepth :: Int
--     , varDelays :: Map Var (FDExpr s)
--     , whereVars :: [Var]
--     , functions :: Map FName Function
--     }

-- initState :: [Function] -> DelayState s
-- initState fs = DelayState
--     { fbyDepth = 1
--     , varDelays = Map.empty
--     , whereVars = []
--     , functions = Map.fromList $ (\f@(Function n _ _) -> (n,f)) <$> fs
--     }
-- type Delay s a = (WriterT [FDExpr s] (ReaderT (DelayState s) (FD s)) a)

-- runDelay :: Delay s a -> [Function] -> FD s (a, [FDExpr s])
-- runDelay m fs = flip runReaderT (initState fs) $ runWriterT m

-- getVar :: Var -> Delay s (FDExpr s)
-- getVar x = do
--     vm <- asks varDelays
--     case vm !? x of
--         Just vx -> return vx
--         Nothing -> error $ "getVar: unbound variable " ++ x

-- bindVar :: Var -> FDExpr s -> Delay s a -> Delay s a
-- bindVar x e = local (\s -> s { varDelays = Map.insert x e (varDelays s) })
--     -- vm <- get
--     -- put $ Map.insert x e vm

-- getFunction :: FName -> Delay s Function
-- getFunction n = do
--     fs <- asks functions
--     case fs !? n of
--         Just f -> return f
--         Nothing -> error $ "getFunction: unbound variable " ++ n

-- getVarDelay :: Var -> Delay s (FDExpr s)
-- getVarDelay x = do
--     wv <- asks whereVars
--     if x `elem` wv
--         then return (int $ 0)
--         else return (int $ 1)

{-
We don't need to use a full blown SMT solver here,
in fact, we are probably better off without it, as the error reporting will be way better
-}

newtype DVar = DV { unDVar :: String }
    deriving (Eq, Ord)

data Delay
    = DVar DVar
    | DLit Int
    | Delay :+: Delay
    | Max Delay Delay
    deriving (Eq)

instance Pretty DVar where
    ppr (DV x) = text x

instance Pretty Delay where
    ppr (DVar x) = ppr x
    ppr (DLit x) = ppr x
    ppr (x :+: y) = ppr x <+> text "+" <+> ppr y
    ppr (Max x y) = text "max" <> parens (ppr x <> comma <+> ppr y)

dmax :: [Delay] -> Delay
dmax [] = DLit 0
dmax (d:ds) = foldl' Max d ds

data Constraint
    = Delay :>=: Delay

instance Pretty Constraint where
    ppr (x :>=: y) = ppr x <+> text ">=" <+> ppr y

-- Allows for delay polymorphsim
-- Ex: "n(x) = next next x" has delay "forall x. x + 2"
data Scheme = ForAll [DVar] Delay

data DelayEnv = DelayEnv 
    { varDelays :: Map Var Delay
    , depth :: Int
    }

newtype VarSupply = VarSupply { getVarSupply :: Int }

data DelayError = UndefinedVariable Var

type Infer = RWST DelayEnv Unifier VarSupply (Except DelayError)

type Solve = Except DelayError

newtype Subst = Subst (Map DVar Delay)
    deriving (Monoid)

type Unifier = (Subst, [Constraint])

class Substitutable a where
    apply :: Subst -> a -> a
    fv :: a -> Set DVar

instance Substitutable Delay where
    apply (Subst s) d@(DVar x) = Map.findWithDefault d x s
    apply _ (DLit k) = DLit k
    apply s (x :+: y) = case (apply s x, apply s y) of
        (DLit i, DLit j) -> DLit (i + j)
        (x' :+: DLit i, DLit j) -> (x' :+: DLit (i + j))
        (DLit i :+: x', DLit j) -> (x' :+: DLit (i + j))
        (DLit i, x' :+: DLit j) -> (x' :+: DLit (i + j))
        (DLit i, DLit j :+: x') -> (x' :+: DLit (i + j))
        (x', y') -> (x' :+: y')
    apply s (Max x y) = case (apply s x, apply s y) of
        (DLit i, DLit j) -> DLit (max i j)
        (Max x' (DLit i), DLit j) -> Max x' (DLit $ max i j)
        (Max (DLit i) x', DLit j) -> Max x' (DLit $ max i j)
        (DLit i, Max x' (DLit j)) -> Max x' (DLit $ max i j)
        (DLit i, Max (DLit j) x') -> Max x' (DLit $ max i j)
        (x', y') -> Max x' y'
    
    fv (DVar x) = Set.singleton x
    fv (DLit _) = Set.empty
    fv (x :+: y) = Set.union (fv x) (fv y)
    fv (Max x y) = Set.union (fv x) (fv y)

instance Substitutable Scheme where
    apply (Subst s) (ForAll vs d) = 
        let s' = Subst $ foldr Map.delete s vs
        in ForAll vs $ apply s' d

    fv (ForAll vs d) = fv d `Set.difference` Set.fromList vs

instance Substitutable Constraint where
    apply s (x :>=: y) = (apply s x) :>=: (apply s y) 

    fv (x :>=: y) = Set.union (fv x) (fv y)

instance Substitutable a => Substitutable [a] where
    apply = map . apply
    fv = foldr (Set.union . fv) Set.empty

instance Substitutable DelayEnv where
    apply s env = env { varDelays = fmap (apply s) (varDelays env)  }
    fv env = fv $ Map.elems (varDelays env)


freshen :: DVar -> Infer DVar
freshen (DV x) = do
    s <- gets getVarSupply
    put $ VarSupply (s + 1)
    return (DV $ x ++ show s)
    where

instantiate :: Scheme -> Infer Delay
instantiate (ForAll vs d) = do
    vs' <- fmap DVar <$> traverse freshen vs
    let s = Subst $ Map.fromList $ zip vs vs'
    return $ apply s d

generalize :: DelayEnv -> Delay -> Scheme
generalize env d =
    let vs = Set.toList $ fv d `Set.difference` fv env
    in ForAll vs d
-- where as = Set.toList $ ftv t `Set.difference` ftv env

lookupVar :: Var -> Infer Delay
lookupVar x = do
    s <- asks (Map.lookup x . varDelays)
    case s of
        Just d -> return d
        Nothing -> throwError $ UndefinedVariable x

addConstraint :: Constraint -> Infer ()
addConstraint c = tell (emptySubst ,[c])

infer :: Expr -> Infer Delay
infer = \case
    (Var x) -> lookupVar x
    (Const k) -> return $ DLit 0
    (If c t f) -> do
        cc <- infer c
        tt <- infer t
        ff <- infer f
        return $ dmax [cc,tt,ff]
    (Next e) -> (:+: DLit 1) <$> infer e
    (Fby l r) -> do
        ll <- infer l
        rr <- fbyRhs $ infer r
        d <- asks (DLit . depth)
        addConstraint (ll :+: d :>=: rr)
        return ll
    (Where body bs) -> do
        let (vs,es) = unzip bs
        vars <- traverse (freshen . DV) vs
        let delays = Map.fromList $ zip vs (fmap DVar vars)
        withVars (fmap (:+: DLit 1) delays) $ traverse_ whereVar $ zip vars es
        withVars delays $ infer body
    (App fname args) -> return $ DLit 0
    (Binop _ l r) -> do
        ll <- infer r
        rr <- infer r
        return $ dmax [ll, rr]
    (Unary _ e) -> infer e

    where
        fbyRhs :: Infer a -> Infer a
        fbyRhs = local (\s -> s { depth = 1 + (depth s) })

        whereVar :: (DVar, Expr) -> Infer Delay 
        whereVar (d,e) = mfix (\ee -> censor (subst ee) (infer e))
            where
                subst ee (s, cs) =
                    let sub = Subst $ Map.singleton d ee
                    in (sub `compose` s, apply sub cs)

        withVars :: Map Var Delay -> Infer a -> Infer a
        withVars vs = local (\s -> s { varDelays = Map.union vs (varDelays s) })

constraints :: Expr -> Either DelayError Unifier
constraints e = fmap snd $ runExcept $ evalRWST (infer e) initEnv initVarSupply 
    where
        initEnv = DelayEnv { varDelays = Map.empty, depth = 1 }
        initVarSupply = VarSupply 0

emptySubst :: Subst
emptySubst = mempty

compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ fmap (apply (Subst s1)) s2 `Map.union` s1

unifyMany :: [Delay] -> [Delay] -> Solve Subst
unifyMany [] [] = return emptySubst
unifyMany (d1:ds1) (d2:ds2) = do
    s1 <- unifies d1 d2 
    s2 <- unifyMany (apply s1 ds1) (apply s1 ds2)
    return (s2 `compose` s1)

unifies :: Delay -> Delay -> Solve Subst
unifies d1 d2 | d1 == d2 = return emptySubst
unifies (DVar x) d = x `bind` d
unifies d (DVar x) = x `bind` d
unifies (a :+: b) (c :+: d) = unifyMany [a,c] [b,d]

-- x + 2 == max(x, y + 3) 

bind :: DVar -> Delay -> Solve Subst
bind x d | d == DVar x = return emptySubst
         | otherwise = return $ Subst $ Map.singleton x d
solver :: Unifier -> Solve Subst 
solver (s, cs) = case cs of
    [] -> return s

-- constraints :: Expr -> Delay s (FDExpr s)
-- constraints = \case
--     (Var x) -> do
--         vx <- getVar x
--         d <- getVarDelay x
--         return $ (vx + d)
--     (Const k) -> return 0
--     (If c t f) -> do
--         cc <- constraints c
--         tt <- constraints t
--         ff <- constraints f
--         return (cmax [cc, tt, ff])
--     (Next e) -> (+1) <$> (constraints e)
--     (Fby l r) -> do
--         ll <- constraints l
--         rr <- fbyRhs (constraints r)
--         d <- asks (int . fbyDepth)
--         lift $ lift (rr #<= ll + d)
--         return ll
--     (Where body bs) -> withVars bs $ \vs -> do
--         traverse_ whereConstraints vs
--         whereBody bs (constraints body)
--     (App n args) -> do
--         f <- getFunction n
--         vargs <- traverse constraints args
--         fConstraints f vargs
--     (Binop op l r) -> do
--         ll <- constraints l
--         rr <- constraints r
--         return (cmax [ll, rr])
--     (Unary op e) -> constraints e
--     where

--         fbyRhs :: Delay s a -> Delay s a
--         fbyRhs = local (\s -> s { fbyDepth = 1 + (fbyDepth s) })

--         withVars :: [(Var, Expr)] -> ([(FDExpr s, Expr)] -> Delay s a) -> Delay s a
--         withVars ve f = do
--             vars <- lift $ lift $ news (length ve) (Domain.range 0 Domain.sup)
--             tell vars
--             let (vs, es) = unzip ve
--             let delays = Map.fromList $ zip vs vars
--             local (\s -> s { varDelays = Map.union delays (varDelays s) }) (f (zip vars es))

--         whereConstraints :: (FDExpr s, Expr) -> Delay s ()
--         whereConstraints (v, e) = do
--             ee <- constraints e
--             lift $ lift (v #== ee)

--         whereBody :: [(Var, Expr)] -> Delay s a -> Delay s a
--         whereBody bs = local (\s -> s { whereVars = fst <$> bs })

--         fConstraints :: Function -> [(FDExpr s)] -> Delay s (FDExpr s)
--         fConstraints (Function n argnames body) args = do
--             retDelay <- lift $ lift $ new (Domain.range 0 Domain.sup)
--             tell [retDelay]
--             let vargs = Map.fromList $ zip argnames args
--             bodyDelay <- local (\s -> s { varDelays = vargs, whereVars = argnames}) (constraints body)
--             let argMax = case args of
--                     [] -> 0
--                     _ -> cmax args
--             lift $ lift $ retDelay #== (bodyDelay - argMax)
--             return bodyDelay

-- genConstraints :: [Function] -> Expr -> Maybe [Int]
-- genConstraints fs e = tryHead $ runFD $ do
--     (mdelay, vars) <- runDelay (constraints e) fs
--     mVar <- new (Domain.range 0 Domain.sup)
--     mVar #== mdelay
--     label (mVar:vars)
--     where
--         tryHead :: [a] -> Maybe a
--         tryHead (x:_) = Just x
--         tryHead [] = Nothing

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


test :: String -> IO ()
test f = do
    contents <- readFile f
    let (Right [Function _ _ e]) = runParser program "" contents
    let (Right (s, cs)) = constraints e
    traverse_ (putStrLn . pp) (apply s cs)