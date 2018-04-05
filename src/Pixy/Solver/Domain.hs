module Pixy.Solver.Domain 
    ( Domain
    , Bound
    , inf
    , sup
    , member
    , size
    , null
    , findMax
    , findMin
    , range
    , toList
    , union
    , intersection
    , removeGreater
    , removeLesser
    , singleton
    )
where

import Prelude hiding (null)
import Data.List (sortBy)

data Bound
    = N !Int    -- ^ Some integer bound
    | Inf       -- ^ Infinium of Z (negative infinity)
    | Sup       -- ^ Supremum of Z (positve infinity)
    deriving (Show)

instance Eq Bound where
    (==) = bequal 
    (/=) = bnequal

instance Ord Bound where
    compare = bcompare 

instance Num Bound where
    (+) = bbinop (+)
    (*) = bbinop (*)
    abs = babs
    signum = bsignum
    negate = bnegate
    fromInteger = N . fromInteger

bequal :: Bound -> Bound -> Bool
bequal (N i) (N j) = i == j
bequal Inf Inf = True
bequal Sup Sup = True
bequal _ _ = False

bnequal :: Bound -> Bound -> Bool
bnequal (N i) (N j) = i /= j
bnequal Inf Inf = False
bnequal Sup Sup = False
bnequal _ _ = True

bcompare :: Bound -> Bound -> Ordering
bcompare (N i) (N j) = compare i j
bcompare Inf Inf = EQ
bcompare Inf _ = LT
bcompare _ Inf = GT
bcompare Sup Sup = EQ
bcompare Sup _ = GT
bcompare _ Sup = LT

bbinop :: (Int -> Int -> Int) -> Bound -> Bound -> Bound
bbinop op (N i) (N j) = N (i `op` j)
bbinop _ Sup Inf = error "bplus: indeterminate form"
bbinop _ Inf Sup = error "bplus: indeterminate form"
bbinop _ Inf _ = Inf
bbinop _ _ Inf = Inf
bbinop _ Sup _ = Sup
bbinop _ _ Sup = Sup

babs :: Bound -> Bound
babs (N i) = N  (abs i)
babs _ = Sup

bsignum :: Bound -> Bound
bsignum (N i) = N (signum i)
bsignum Inf = N (-1)
bsignum Sup = N 1

bnegate :: Bound -> Bound
bnegate (N i) = N (-i)
bnegate Inf = Sup
bnegate Sup = Inf

inf :: Bound
inf = Inf

sup :: Bound
sup = Sup

data Domain
    = Empty                         -- ^ The empty domain
    | Split !Int !Domain !Domain    -- ^ Split on some integer N, with Left and Right domains whose elements are all greater than or less than N, respectively
    | Range !Bound !Bound               -- ^ An interval with bounds From and To
    deriving (Show)

instance Eq Domain where
    d1 == d2 = dequal d1 d2
    d1 /= d2 = dnequal d1 d2

dequal :: Domain -> Domain -> Bool
dequal Empty Empty = True
dequal (Split n1 l1 r1) (Split n2 l2 r2) = (n1 == n2) && dequal l1 l2 && dequal r1 r2
dequal (Range from1 to1) (Range from2 to2) = from1 == from2 && to1 == to2
dequal _ _ = False

dnequal :: Domain -> Domain -> Bool
dnequal Empty Empty = False
dnequal (Split n1 l1 r1) (Split n2 l2 r2) = (n1 /= n2) || (dnequal l1 l2) || (dnequal r1 r2)
dnequal (Range from1 to1) (Range from2 to2) = (from1 /= from2) || (to1 /= to2)
dnequal _ _ = True

-- | Tests to see if a domain contains an integer
member :: Int -> Domain -> Bool
member _ Empty = False
member i (Split n l r)
    | i < n = member i l
    | i > n = member i r
    | otherwise = False
member i (Range from to) = N i >= from && N i <= to

size Empty = 0
size (Split _ l r) = size l + size r
size (Range from to) = to - from

null :: Domain -> Bool
null Empty = True
null _ = False

findMax :: Domain -> Bound
findMax Empty = error "findMax: Empty domain has no maximal element"
findMax (Split _ _ r) = findMax r
findMax (Range _ to) = to

findMin :: Domain -> Bound
findMin Empty = error "findMin: Empty domain has no minimal element"
findMin (Split _ _ r) = findMin r
findMin (Range from _) = from

range :: Bound -> Bound -> Domain
range = Range

-- | Converts a domain into the list of integers it contains
toList :: Domain -> [Int]
toList Empty = []
toList (Split _ l r) = toList l ++ toList r
toList (Range (N from) (N to)) = [from .. to]
toList _ = error "toList: Infinite domain"

-- | Converts a domain into a series of (From, To) intervals
intervals :: Domain -> [(Bound, Bound)]
intervals Empty = []
intervals (Split _ l r) = intervals l ++ intervals r
intervals (Range from to) = [(from, to)]

-- | Merges together a series of potentially overlapping intervals
mergeIntervals :: [(Bound, Bound)] -> [(Bound, Bound)]
mergeIntervals is = mergeOverlapping $ sortBy (\a b -> compare (fst a) (fst b)) is
    where
        mergeOverlapping :: [(Bound, Bound)] -> [(Bound, Bound)]
        mergeOverlapping [] = []
        mergeOverlapping ((from, to):rest) =
            let (to', rest') = mergeRemaining to rest
            in mergeOverlapping ((from, to'):rest')

        mergeRemaining :: Bound -> [(Bound, Bound)] -> (Bound, [(Bound, Bound)])
        mergeRemaining end [] = (end, [])
        mergeRemaining end ((from,to):rest)
            | to <= (end + 1) = mergeRemaining (max end to) rest
            | otherwise = (end, (from,to):rest)

-- | Converts a series of intervals into a domain
intervalsToDomain :: [(Bound, Bound)] -> Domain
intervalsToDomain [] = Empty
intervalsToDomain ((from, to):[]) = Range from to
intervalsToDomain is =
    let (front, back@((N start,_):_)) = splitAt ((length is + 1) `div` 2) is
    in Split (start - 1) (intervalsToDomain front) (intervalsToDomain back)

-- | Computes the union of 2 domains
union :: Domain -> Domain -> Domain
union d1 d2 = intervalsToDomain $ mergeIntervals (intervals d1 ++ intervals d2)

-- | Computes the intersection of 2 domains
intersection :: Domain -> Domain -> Domain
intersection Empty _ = Empty
intersection (Split n l r) d =
    case (intersection l d, intersection r d) of
        (Empty, r') -> r'
        (l', Empty) -> l'
        (l', r') -> Split n l' r'
intersection (Range from to) d = narrow d from to

-- | Narrows down a domain using an interval
narrow :: Domain -> Bound -> Bound -> Domain
narrow Empty _ _ = Empty
narrow (Split n l r) from to
    | to < N n = narrow l from to
    | from > N n = narrow r from to
    | otherwise =
        case (narrow l from to, narrow r from to) of
            (Empty, r') -> r'
            (l', Empty) -> l'
            (l', r') -> Split n l' r'
narrow (Range l u) from to =
    let from' = max l from
        to' = min u to
    in if from' > to' then Empty else Range from' to'

-- | Removes all elements greater than a given bound
removeGreater :: Bound -> Domain -> Domain
removeGreater _ Empty = Empty
removeGreater n (Split s l r)
    | n >= (N s) = 
        case removeGreater n r of
            Empty -> l
            r' -> Split s l r'
    | otherwise = removeGreater n l
removeGreater n (Range from to) 
    | from > n = Empty
    | otherwise = Range from (min n to)

removeLesser :: Bound -> Domain -> Domain
removeLesser _ Empty = Empty
removeLesser n (Split s l r)
    | n <= (N s) =
        case removeLesser n l of
            Empty -> r
            l' -> Split s l' r
    | otherwise = removeLesser n r
removeLesser n (Range from to)
    | to < n = Empty
    | otherwise = Range (max n from) to



-- | Creates a domain with one element
singleton :: Int -> Domain
singleton n = Range (N n) (N n)