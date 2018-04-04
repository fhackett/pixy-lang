{-# LANGUAGE RankNTypes #-}
module Pixy.Delay where

import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad.Except
import Control.Applicative (Alternative, (<|>))
import Data.Maybe (mapMaybe)
import Data.Foldable (traverse_)

import qualified Data.Map as Map
import Data.Map (Map, (!))
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Data.List (foldl1)

import Pixy.Syntax

{-
When dealing with a where =
Replace all instances of the 
-}

-- Given a solver state, I can give you a list of all possible solver states and a's
-- SolverState s -> [(SolverState s,a)]
newtype Solver s a = Solver { unSolver :: StateT (SolverState s) [] a }
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadState (SolverState s))

data SolverVar s
    = Gen Int
    | Bound Var
    deriving (Eq, Ord)

data VarInfo s = VarInfo
     { delayedConstraints :: Solver s (), values :: IntSet }

data SolverState s = SolverState { varSupply :: Int , varMap :: Map (SolverVar s) (VarInfo s) }
type SolverConstraint s = Solver s ()

initSolverState :: SolverState s
initSolverState = SolverState { varSupply = 0, varMap = Map.empty }

runSolver :: (forall s . Solver s a) -> [a]
runSolver s = evalStateT (unSolver s) initSolverState

maxDepth :: Int
maxDepth = 100

isOneOf :: SolverVar s -> [Int] -> Solver s ()
x `isOneOf` d = modify $ \s ->
    let vm = varMap s
        vi = VarInfo {
            delayedConstraints = return (),
            values = IntSet.fromList d
        }
    in s { varMap = Map.insert x vi vm }

boundVar :: String -> [Int] -> Solver s (SolverVar s)
boundVar n d = do
    let v = Bound n
    v `isOneOf` d
    return v

genVar :: [Int] -> Solver s (SolverVar s)
genVar d = do
    v <- nextVar
    v `isOneOf` d
    return v
    where
        nextVar :: Solver s (SolverVar s)
        nextVar = do
            s <- get
            let k = varSupply s
            put $ s { varSupply = k + 1 }
            return $ Gen k
        
domain :: SolverVar s -> Solver s IntSet
domain x = do
    s <- get
    return . values $ varMap s ! x

update :: SolverVar s -> IntSet -> SolverConstraint s
update x i = do
    s <- get
    let vm = varMap s
    let vi = vm ! x
    put $ s { varMap = Map.insert x (vi { values = i }) vm }
    delayedConstraints vi

addConstraint :: SolverVar s -> SolverConstraint s -> SolverConstraint s
addConstraint x constraint = do
    s <- get
    let vm = varMap s
    let vi = vm ! x
    let cs = delayedConstraints vi
    put $ s { varMap = Map.insert x (vi { delayedConstraints = cs >> constraint }) vm }

type BinaryConstraint s = SolverVar s -> SolverVar s -> SolverConstraint s

addBinaryConstraint :: BinaryConstraint s -> BinaryConstraint s
addBinaryConstraint f x y = do
    let constraint = f x y
    constraint
    addConstraint x constraint
    addConstraint y constraint

hasValue :: SolverVar s -> Int -> SolverConstraint s
x `hasValue` val = do
    dx <- domain x
    guard $ val `IntSet.member` dx
    let i = IntSet.singleton val
    when (i /= dx) $ update x i

same :: SolverVar s -> SolverVar s -> SolverConstraint s
same = addBinaryConstraint $ \x y -> do
    dx <- domain x
    dy <- domain y
    let i = IntSet.intersection dx dy
    guard $ not $ IntSet.null i
    when (i /= dx) $ update x i
    when (i /= dy) $ update y i

leq :: SolverVar s -> SolverVar s -> SolverConstraint s
leq = addBinaryConstraint $ \x y -> do
    dx <- domain x
    dy <- domain y
    let dx' = IntSet.filter (<= IntSet.findMax dy) dx
    let dy' = IntSet.filter (>= IntSet.findMin dx) dy
    guard $ not $ IntSet.null dx'
    guard $ not $ IntSet.null dy'
    when (dx /= dx') $ update x dx'
    when (dy /= dy') $ update y dy'


varsLabelling :: [SolverVar s] -> Solver s [(Var, Int)]
varsLabelling vs = (mapMaybe getVar) <$> mapM label vs
    where
        label var = do
            vals <- domain var
            val <- Solver . lift $ IntSet.toList vals
            var `hasValue` val
            return (var, val)
        
        getVar (Bound x, k) = Just (x, k)
        getVar (Gen _, _) = Nothing

data SolverExpr s
    = SInt Int
    | SVar (SolverVar s)
    | SPlus (SolverExpr s) (SolverExpr s)
    | SMax (SolverExpr s) (SolverExpr s)

instance Num (SolverExpr s) where
    (+) = SPlus
    -- TODO: The rest of the num methods
    fromInteger = SInt . fromInteger

interpret :: SolverExpr s -> Solver s (SolverVar s)
interpret (SInt k) = genVar [k]
interpret (SVar x) = return x
interpret (SPlus l r) = interpretBinary (+) l r
-- TODO: Do we need to add extra constraints?
interpret (SMax l r) = interpretBinary max l r

interpretBinary :: (Int -> Int -> Int) -> SolverExpr s -> SolverExpr s -> Solver s (SolverVar s)
interpretBinary op l r = do
    vl <- interpret l
    vr <- interpret r
    dl <- domain vl
    dr <- domain vr
    v <- genVar [ nl `op` nr | nl <- IntSet.elems dl, nr <- IntSet.elems dr ]
    let pc = constraintBinary (\nv nl nr -> nv == nl `op` nr) v vl vr
        ncl = constraintBinary (\nl nv nr -> nv == nl `op` nr) vl v vr
        ncr = constraintBinary (\nr nv nl -> nv == nl `op` nr) vr v vl
    addConstraint vl $ pc >> ncr
    addConstraint vr $ pc >> ncl
    addConstraint v $ ncl >> ncr
    return v

constraintBinary :: (Int -> Int -> Int -> Bool) -> SolverVar s -> SolverVar s -> SolverVar s -> SolverConstraint s
constraintBinary pred x y z = do
    dx <- domain x
    dy <- domain y
    dz <- domain z
    let dx' = IntSet.fromList [nx | nx <- IntSet.elems dx, ny <- IntSet.elems dy, nz <- IntSet.elems dz, pred nx ny nz ]
    guard $ not $ IntSet.null dx'
    when (dx /= dx') $ update x dx'

(#==) :: SolverExpr s -> SolverExpr s -> SolverConstraint s
l #== r = do
    vl <- interpret l
    vr <- interpret r
    vl `same` vr

(#<=) :: SolverExpr s -> SolverExpr s -> SolverConstraint s
l #<= r = do
    vl <- interpret l
    vr <- interpret r
    vl `leq` vr

(#+) :: SolverExpr s -> SolverExpr s -> SolverExpr s
l #+ r = SPlus l r

max' :: SolverExpr s -> SolverExpr s -> SolverExpr s
max' = SMax

gen :: [Int] -> Solver s (SolverExpr s)
gen d = SVar <$> genVar d

bound :: String -> [Int] -> Solver s (SolverExpr s)
bound s d = SVar <$> boundVar s d

bounds :: [String] -> [Int] -> Solver s [SolverExpr s]
bounds ss d = traverse (flip bound d) ss

labelling :: [SolverExpr s] -> Solver s [(Var, Int)]
labelling = varsLabelling <=< mapM interpret

data GenState = GenState
    { currentDelay :: Int
    , fbyDepth :: Int
    }

initState :: GenState
initState = GenState
    { currentDelay = 1
    , fbyDepth = 1
    }

type CM s a = (ReaderT GenState (WriterT [SolverExpr s] (Solver s))) a--(CTree a -> CTree a)

runCM :: CM s a -> Solver s (a, [SolverExpr s])
runCM m = runWriterT $ runReaderT m initState
-- runGenConstraints :: GenConstraints s a -> (a, [Constraint])
-- runGenConstraints m = runWriter $ runReaderT m initState


{-
TODO: Annotate each node of the tree with the appropriate delay

This should be migrated to the strategy shown here:
http://overtond.blogspot.ca/2008/07/pre.html
-}
constraints :: Expr -> CM s (SolverExpr s)
constraints = \case
    (Var x) -> do
        d <- asks (SInt . currentDelay)
        return $ (SVar $ Bound x) + d
    (Const _) -> return 0
    (Check e) -> constraints e
    (If c t f) -> do
        cc <-  constraints c
        tt <- constraints t
        ff <- constraints f
        return $ max' cc (max' tt ff)
    (Next e) -> nextDepth (constraints e)
    (Fby l r) -> do
        ll <- constraints l
        rr<- fbyRhs (constraints r)
        d <- asks (SInt . fbyDepth)
        lift $ lift (rr #<= (ll + d))
        return ll
    (Where body bs) -> do
        bs' <- traverse bindVar bs
        traverse_ whereConstraints bs'
        constraints body
    (App _ _) -> return 0
    (Binop _ l r) -> do
        ll <- constraints l
        rr <- constraints r
        return $ max' ll rr
    where
        nextDepth :: CM s a -> CM s a
        nextDepth = local (\s -> s { currentDelay = 1 + (currentDelay s)})

        fbyRhs :: CM s a -> CM s a
        fbyRhs = local (\s -> s { fbyDepth = 1 + (fbyDepth s) })

        bindVar :: (Var, Expr) -> CM s (SolverExpr s, Expr)
        bindVar (v, e) = do
            v' <- lift $ lift $ bound v [0..maxDepth]
            tell [v']
            return (v', e)

        whereConstraints :: (SolverExpr s, Expr) -> CM s ()
        whereConstraints (v,e) = do
            ee <- constraints e
            lift $ lift (v #== ee)

genConstraints :: Function -> [[(Var, Int)]]
genConstraints (Function n args body) = runSolver $ do
    vargs <- bounds args [0..maxDepth]
    (_, vars) <- runCM $ constraints body
    labelling (vargs ++ vars)