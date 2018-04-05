{-
This is a port of the prolog clpfd library
-}
{-# LANGUAGE RankNTypes #-}
module Pixy.Solver.FD
    ( FD
    , FDExpr
    , runFD
    , int
    , (#==)
    , (#/=)
    , (#>=)
    , (#<=)
    , (#>)
    , (#<)
    , cmax
    , new
    , news
    , label
    )
where

import Control.Monad.State.Lazy
import Control.Monad (unless)
import Control.Applicative (Alternative)

import qualified Data.Map as Map
import Data.Map (Map, (!))

import qualified Pixy.Solver.Domain as Domain
import Pixy.Solver.Domain (Domain, Bound)


newtype FD s a = FD { unFD :: StateT (FDState s) [] a }
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadState (FDState s))

newtype FDVar s = FDVar { unFDVar :: Int }
    deriving (Eq, Ord)

type FDConstraint s = FD s ()

data VarInfo s = VarInfo { delayedConstraints :: FDConstraint s, values :: Domain }

data FDState s = FDState { varSupply :: FDVar s, varMap :: Map (FDVar s) (VarInfo s) }

initState :: FDState s
initState = FDState { varSupply = FDVar 0, varMap = Map.empty }

runFD :: (forall s . FD s a) -> [a]
runFD s = evalStateT (unFD s) initState

newVar :: Domain -> FD s (FDVar s)
newVar d = do
    v <- nextVar
    v `isOneOf` d
    return v
    where
        nextVar :: FD s (FDVar s)
        nextVar = do
            s <- get
            let v = varSupply s
            put s { varSupply = FDVar (unFDVar v + 1) }
            return v

        isOneOf :: FDVar s -> Domain -> FDConstraint s
        x `isOneOf` d = modify $ \s ->
            let vm = varMap s
                vi = VarInfo {
                    delayedConstraints = return (),
                    values = d
                }
            in s { varMap = Map.insert x vi vm }

unboundedVar :: FD s (FDVar s)
unboundedVar = newVar $ Domain.range Domain.inf Domain.sup

domain :: FDVar s -> FD s Domain
domain x = do
    s <- get
    return $ values $ varMap s ! x

domain' :: FDVar s -> FD s (Domain, Bound, Bound)
domain' x = do
    dx <- domain x
    return (dx, Domain.findMin dx, Domain.findMax dx)

update :: FDVar s -> Domain -> FDConstraint s
update x d = do
    s <- get
    let vm = varMap s
    let vi = vm ! x
    put $ s { varMap = Map.insert x (vi { values = d }) vm }
    delayedConstraints vi

addConstraint :: FDVar s -> FDConstraint s -> FDConstraint s
addConstraint x constraint = do
    s <- get
    let vm = varMap s
    let vi = vm ! x
    let cs = delayedConstraints vi
    put $ s { varMap = Map.insert x (vi { delayedConstraints = cs >> constraint }) vm }

type BinaryConstraint s = FDVar s -> FDVar s -> FDConstraint s

addBinaryConstraint :: BinaryConstraint s -> BinaryConstraint s
addBinaryConstraint f x y = do
    let constraint = f x y
    constraint
    addConstraint x constraint
    addConstraint y constraint

hasValue :: FDVar s -> Int -> FDConstraint s
x `hasValue` n = do
    dx <- domain x
    guard $ n `Domain.member` dx
    let d = Domain.singleton n
    when (d /= dx) $ update x d

eq :: FDVar s -> FDVar s -> FDConstraint s
eq = addBinaryConstraint $ \x y -> do
    dx <- domain x
    dy <- domain y
    let i = Domain.intersection dx dy
    guard $ not $ Domain.null i
    when (i /= dx) $ update x i
    when (i /= dy) $ update y i

neq :: FDVar s -> FDVar s -> FDConstraint s
neq = addBinaryConstraint $ \x y -> do
    dx <- domain x
    dy <- domain y
    guard $ Domain.size dx > 1 || Domain.size dy > 1 || dx /= dy

geq :: FDVar s -> FDVar s -> FDConstraint s
geq = addBinaryConstraint $ \x y -> do
    dx <- domain x
    dy <- domain y
    unless (Domain.findMin dx >= Domain.findMax dy) $ pgeq x y
    where
        pgeq :: FDVar s -> FDVar s -> FDConstraint s
        pgeq x y = do
            (dx, dxl, dxu) <- domain' x
            (dy, dyl, dyu) <- domain' y
            unless (dxl >= dyu) $ do
                updateBounds x (max dxl dyl) dxu 
                updateBounds y dyl (min dxu dyu)

varsLabel :: [FDVar s] -> FD s [Int]
varsLabel = mapM label
    where
        label var = do
            vals <- domain var
            val <- FD . lift $ Domain.toList vals
            var `hasValue` val
            return val

data FDExpr s
    = Int !Int
    | Var !(FDVar s)
    | Plus !(FDExpr s) !(FDExpr s)
    | Minus !(FDExpr s) !(FDExpr s)
    -- | Times !(FDExpr s) !(FDExpr s)
    | Max !(FDExpr s) !(FDExpr s)

instance Num (FDExpr s) where
    (+) = Plus
    (-) = Minus
    -- (*) = Times
    fromInteger = Int . fromInteger

int :: Int -> FDExpr s
int = Int

-- | Takes an FDExpr and generates an auxilary variable representing the result of the expression
interpret :: FDExpr s -> FD s (FDVar s)
interpret (Int n) = newVar $ Domain.singleton n
interpret (Var v) = return v
interpret (Plus x y) = do
    vx <- interpret x
    vy <- interpret y
    vz <- unboundedVar
    pplus vx vy vz
    return vz
interpret (Minus x y) = do
    vx <- interpret x
    vy <- interpret y
    vz <- unboundedVar
    pplus vy vz vx
    return vz
interpret (Max x y) = do
    vx <- interpret x
    vy <- interpret y
    vz <- unboundedVar
    vz `geq` vx
    vz `geq` vy
    pmax vx vy vz
    return vz

pplus :: FDVar s -> FDVar s -> FDVar s -> FDConstraint s
pplus x y z = do
    (dx, dxl, dxu) <- domain' x
    (dy, dyl, dyu) <- domain' y
    (dz, dzl, dzu) <- domain' z
    let dxl' = max dxl (dzl - dyu)
    let dxu' = min dxu (dzu - dyl)
    updateBounds x dxl' dxu'
    (dy, dyl, dyu) <- domain' y
    let dyl' = max dyl (dzl - dxu')
    let dyu' = min dyu (dzu - dxl')
    updateBounds y dyl' dyu'
    (dz, dzl, dzu) <- domain' z
    let dzl' = max dzl (dxl' + dyl')
    let dzu' = min dzu (dxu' + dyu')
    updateBounds z dzl' dzu'

pmax :: FDVar s -> FDVar s -> FDVar s -> FDConstraint s
pmax x y z = do
    dz <- domain z
    (_, dxl, dxu) <- domain' x
    (_, dyl, dyu) <- domain' y
    if | dyl > dxu -> eq z y
       | dyu < dxl -> eq z x
       | otherwise -> do
        let dz' = Domain.removeGreater (max dxu dyu) dz
        putDomain z dz dz'

updateBounds :: FDVar s -> Bound -> Bound -> FDConstraint s
updateBounds x dxl' dxu' = do
    (dx, dxl, dxu) <- domain' x
    unless (dxl == dxl' && dxu == dxu') $ do
        let dx' = Domain.intersection dx (Domain.range dxl' dxu')
        putDomain x dx dx'

putDomain :: FDVar s -> Domain -> Domain -> FDConstraint s
putDomain x dx dx' = do
    guard $ not $ Domain.null dx'
    when (dx' /= dx) $ update x dx'


liftJ :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
liftJ f x y = join $ liftM2 f x y

infixl 1 #==, #/=, #<=, #>=, #>, #<

(#==) :: FDExpr s -> FDExpr s -> FDConstraint s
x #== y = liftJ eq (interpret x) (interpret y)

(#/=) :: FDExpr s -> FDExpr s -> FDConstraint s
x #/= y = liftJ neq (interpret x) (interpret y)

(#>=) :: FDExpr s -> FDExpr s -> FDConstraint s
x #>= y = liftJ geq (interpret x) (interpret y)

(#<=) :: FDExpr s -> FDExpr s -> FDConstraint s
x #<= y = y #>= x

(#>) :: FDExpr s -> FDExpr s -> FDConstraint s
x #> y = (x + 1) #>= y

(#<) :: FDExpr s -> FDExpr s -> FDConstraint s
x #< y = y #> x

cmax :: [FDExpr s] -> FDExpr s
cmax = foldl1 Max

new :: Domain -> FD s (FDExpr s)
new d = Var <$> newVar d

news :: Int -> Domain -> FD s [FDExpr s]
news n d = replicateM n (new d)

label :: [FDExpr s] -> FD s [Int]
label = varsLabel <=< mapM interpret