{-# LANGUAGE OverloadedStrings #-}
module Pixy.Data.Type where

import Data.List ((!!))

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Text as T
import Data.Text (Text)

import Pixy.Data.Unique
import Pixy.Data.Subst

newtype TVar = TV { unTyVar :: Text }
    deriving (Eq, Show, Ord)

instance Uniquable TVar where
    mkUnique n = 
        let char = ['a' .. 'z'] !! (n `mod` 26)
        in TV $ T.pack $ replicate (n `div` 26 + 1) char

data Type
    = TVar TVar
    | TInt
    | TBool
    | TNil Type
    | Type :-> Type
    deriving (Eq)

instance Substitutable TVar Type Type where
    apply s = \case
        t@(TVar x) -> Map.findWithDefault t x (unSubst s)
        (t1 :-> t2) -> apply s t1 :-> apply s t2
        t -> t
    fv = \case
        (TVar x) -> Set.singleton x
        (t1 :-> t2) -> Set.union (fv t1) (fv t2)
        _ -> Set.empty

instance Uniquable Type where
    mkUnique = TVar . mkUnique

data TyScheme = ForAll [TVar] Type

instance Substitutable TVar Type TyScheme where
    apply (Subst s) (ForAll tvs t) =
        let s' = Subst $ foldr Map.delete s tvs
        in ForAll tvs (apply s' t)
    fv (ForAll tvs t) = fv t `Set.difference` Set.fromList tvs

instantiate :: (MonadUnique m) => TyScheme -> m Type
instantiate (ForAll tvs t) = do
    tvs' <- traverse (const fresh) tvs
    let subst = Subst $ Map.fromList $ zip tvs tvs'
    return $ apply subst t

generalize :: (Substitutable TVar Type env) => env  -> Type -> TyScheme
generalize env t = 
    let vs = Set.toList $ fv t `Set.difference` fv env
    in ForAll vs t

data TyConstraint = Type :~: Type

instance Substitutable TVar Type TyConstraint where
    apply s (t1 :~: t2) = (apply s t1) :~: (apply s t2)
    fv (t1 :~: t2) = Set.union (fv t1) (fv t2)