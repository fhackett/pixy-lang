{-# LANGUAGE GADTs #-}
module Pixy.Data.Delay where

import Data.Semigroup
import Data.List (groupBy, maximumBy, sort)
import Data.Maybe

import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Set as Set

import Data.Map (Map)
import Data.Set (Set)

import Pixy.Data.Name
import Pixy.Data.Subst

-- Linear Equations are just a map of variables to coefficients, along with a constant
data LinearEq v a = LinearEq { terms :: Map v a, constant :: a }
    deriving (Eq, Ord, Show)

normalize :: (Num a, Eq a) => LinearEq v a -> LinearEq v a
normalize (LinearEq m c) = LinearEq (Map.filter (/= 0) m) c

var :: (Num a) => v -> LinearEq v a
var x = LinearEq { terms = Map.singleton x 1, constant = 0 }

vars :: LinearEq v a -> Set v
vars = Map.keysSet . terms

coeffs :: LinearEq v a -> [a]
coeffs = Map.elems . terms

lplus :: (Ord v, Num a, Eq a) => LinearEq v a -> LinearEq v a -> LinearEq v a
(LinearEq m1 c1) `lplus` (LinearEq m2 c2) = normalize $ LinearEq (Map.unionWith (+) m1 m2) (c1 + c2)

lminus :: (Ord v, Num a, Eq a) => LinearEq v a -> LinearEq v a -> LinearEq v a
(LinearEq m1 c1) `lminus` (LinearEq m2 c2) = normalize $ LinearEq (Map.unionWith (-) m1 m2) (c1 - c2)

lnegate :: (Ord v, Num a, Eq a) => LinearEq v a -> LinearEq v a
lnegate (LinearEq m c) = LinearEq (fmap negate m) (negate c)

instance (Ord v, Num a, Eq a) => Num (LinearEq v a)  where
    (+) = lplus
    (-) = lminus
    negate = lnegate
    fromInteger k = LinearEq { terms = Map.empty, constant = fromInteger k }
    -- Linear Equations are an additive group, so there isn't really a concept of multiplication
    (*) = undefined
    abs = undefined
    signum = undefined

class (Num a) => AdditiveModule r a where
    (+.) :: a -> r a -> r a
    (.+) :: r a -> a -> r a

class (Num a) => MultiplicativeModule r a where
    (*.) :: a -> r a -> r a
    (.*) :: r a -> a -> r a

instance (Ord v, Num a) => AdditiveModule (LinearEq v) a where
    k +. (LinearEq m c) = LinearEq m (k + c)
    (LinearEq m c) .+ k = LinearEq m (c + k)

instance (Ord v, Num a) => MultiplicativeModule (LinearEq v) a where
    k *. (LinearEq m c) = LinearEq (fmap (k *) m) (k * c)
    (LinearEq m c) .* k = LinearEq (fmap (* k) m) (c * k)

-- Delays are, in essence, linear functions (augmented with 'max') a with names as variables
-- Because of the fact that '+' is distributive over 'max' (a + max(x,y) = max(a + x, a + y))
-- and the fact that `max` is associative, we can write our delay equations out as the maximum of a list of equations
data DelayEq v a = Maximum { equations :: [LinearEq v a] }
    deriving (Show)

fromLinearEq :: LinearEq v a -> DelayEq v a
fromLinearEq eq = Maximum [eq]

dplus :: (Ord v, Num a, Eq a) => DelayEq v a -> DelayEq v a -> DelayEq v a
(Maximum as) `dplus` (Maximum bs) = Maximum [a + b | a <- as, b <- bs]

dnegate :: (Ord v, Num a, Eq a) => DelayEq v a -> DelayEq v a
dnegate (Maximum as) = Maximum (fmap negate as)

dminus :: (Ord v, Num a, Eq a) => DelayEq v a -> DelayEq v a -> DelayEq v a
(Maximum as) `dminus` (Maximum bs) = Maximum [a - b | a <- as, b <- bs]

dmax :: (Ord v, Num a, Ord a) => [DelayEq v a] -> DelayEq v a
dmax ds = 
    -- Eliminates all equations with the same terms but different coefficients
    let simplify eqs = maximumBy (\a b -> compare (constant a) (constant b)) <$> (groupBy (\a b -> terms a == terms b) $ sort eqs)
    in Maximum $ simplify $ concatMap equations ds

instance (Ord v, Num a, Eq a) => Num (DelayEq v a) where
    (+) = dplus
    (-) = dminus
    negate = dnegate
    fromInteger k = Maximum [fromInteger k]
    -- DelayEq v Equations are an additive group, so there isn't really a concept of multiplication
    (*) = undefined
    abs = undefined
    signum = undefined

instance (Ord v, Num a) => AdditiveModule (DelayEq v) a where
    k +. (Maximum as) = Maximum (fmap (k +.) as)
    (Maximum as) .+ k = Maximum (fmap (.+ k) as)

instance (Ord v, Num a) => MultiplicativeModule (DelayEq v) a where
    k *. (Maximum as) = Maximum (fmap (k *.) as)
    (Maximum as) .* k = Maximum (fmap (.* k) as)

substEq :: (Ord v, Num a, Eq a) => Map v (DelayEq v a) -> LinearEq v a -> DelayEq v a
substEq subst (LinearEq tms c) = 
    let delays = Map.elems $ Map.merge (Map.mapMissing singletonDelay) Map.dropMissing (Map.zipWithMatched substitute) tms subst
    in c +. (sum delays)
    where
        singletonDelay :: (Ord v, Num a) => v -> a -> DelayEq v a
        singletonDelay n k = Maximum [(LinearEq (Map.singleton n k) 0)]

        substitute :: (Ord v, Num a) => v -> a -> DelayEq v a -> DelayEq v a
        substitute n coeff delay = coeff *. delay

instance (Ord v, Num a, Ord a) => Substitutable v (DelayEq v a) (DelayEq v a) where
    apply (Subst s) (Maximum eqs) = dmax (fmap (substEq s) eqs)
    fv (Maximum eqs) = Set.unions $ fmap vars eqs

type Delay = DelayEq Name Int

toDelay :: (Integral a) => a -> Delay
toDelay = fromInteger . toInteger

getValue :: Delay -> Int
getValue (Maximum eqs)
    | and (fmap (Map.null . terms) eqs) = maximum $ fmap constant eqs
    | otherwise = error "getValue: tried to get value of delay with unsolved variables."

data EQ
data GEQ

data Constraint a where
    (:==:) :: Name -> Delay -> Constraint EQ
    (:>=:) :: Delay -> Delay -> Constraint GEQ

instance Substitutable Name Delay (Constraint a) where
    apply s (x :==: y) = x :==: (apply s y) 
    apply s (x :>=: y) = (apply s x) :>=: (apply s y) 

    fv (x :==: y) = Set.insert x (fv y)
    fv (x :>=: y) = Set.union (fv x) (fv y)

data Constraints = Constraints 
    { eqConstraints :: [Constraint EQ]
    , constraints :: [Constraint GEQ]
    }

instance Substitutable Name Delay Constraints where
    apply s (Constraints ecs cs) = Constraints (apply s ecs) (apply s cs)
    fv (Constraints ecs cs) = Set.union (fv ecs) (fv cs)

instance Semigroup Constraints where
    cs1 <> cs2 = Constraints { eqConstraints = (eqConstraints cs1) ++ (eqConstraints cs2), constraints = (constraints cs1) ++ (constraints cs2)} 

instance Monoid Constraints where
    mempty = Constraints { eqConstraints = [], constraints = [] }
    mappend = (<>)