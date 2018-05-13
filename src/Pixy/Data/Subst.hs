{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module Pixy.Data.Subst where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)

import Data.Semigroup

import Pixy.Data.Name

newtype Subst v a = Subst { unSubst :: Map v a }
    deriving (Show, Functor)

emptySubst :: Subst v a
emptySubst = Subst $ Map.empty

singletonSubst :: (Ord v) => v -> a -> Subst v a
singletonSubst x d = Subst $ Map.singleton x d

class Substitutable v s a | v a -> s  where
    apply :: Subst v s -> a -> a
    fv :: a -> Set v

instance (Ord v, Substitutable v s a) => Substitutable v s [a] where
    apply = fmap . apply
    fv = foldr (Set.union . fv) Set.empty

instance (Ord v, Substitutable v s a) => Substitutable v s (Map k a) where
    apply = fmap . apply
    fv = fv . Map.elems

instance (Ord v, Substitutable v a a) => Semigroup (Subst v a) where
    s1 <> s2 = fmap (apply s1) $ Subst $ Map.union (unSubst s2) (unSubst s1)

instance (Ord v, Substitutable v a a) => Monoid (Subst v a) where
    mempty = Subst Map.empty
    mappend = (<>)