module Pixy.Pass.TypeCheckPass where

import Control.Monad.Reader
import Control.Monad.Writer.Strict hiding ((<>))
import Control.Monad.Except
import Data.Bifunctor

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Map (Map, (!))
import Data.Set (Set)

import Data.Text.Prettyprint.Doc

import Pixy.Data.Subst
import Pixy.Data.Type
import Pixy.Data.Name
import Pixy.Data.Unique

import Pixy.Syntax
import Pixy.PrettyPrint
import Pixy.Error

import Debug.Trace

data TyEnv = TyEnv
    { varTypes :: Map Name TyScheme
    , fnTypes :: Map Name TyScheme
    }

data TyInferred = InferredTy
    { tyConstraints :: [TyConstraint]
    }

instance Monoid TyInferred where
    mempty = InferredTy { tyConstraints = [] }
    (InferredTy t1) `mappend` (InferredTy t2) = InferredTy { tyConstraints = t1 ++ t2 }

data TypeError
    = UnificationError Type Type
    | UnboundVariable Name
    | InfiniteType TVar Type

instance Error TypeError where
    toError (UnificationError t1 t2) = ErrorMessage { errorDescription = pretty "Unification Error:" <+> pretty t1 <+> pretty "and" <+> pretty t2} 
    toError (UnboundVariable n) = ErrorMessage { errorDescription = pretty "Unbound Variable:" <+> pretty n }
    toError (InfiniteType tv t2) = ErrorMessage { errorDescription = pretty "Infinite Type:" <+> pretty tv <+> pretty "and" <+> pretty t2} 

lookupVar :: (MonadReader TyEnv m, MonadUnique m, MonadError TypeError m) => Name -> m Type
lookupVar x = do
    sc <- asks ((Map.lookup x) . varTypes)
    case sc of
        Just sc -> instantiate sc
        Nothing -> throwError $ UnboundVariable x

getVars :: (MonadReader TyEnv m) => m (Map Name TyScheme)
getVars = asks varTypes

valueType :: (MonadUnique m) => Value -> m Type
valueType (VInt _) = return TInt
valueType (VBool _) = return TBool
valueType (VNil) = TNil <$> fresh

binopType :: Binop -> Type
binopType Plus = TInt :-> TInt :-> TInt
binopType Minus = TInt :-> TInt :-> TInt
binopType Times = TInt :-> TInt :-> TInt
binopType Divide = TInt :-> TInt :-> TInt
binopType Modulo = TInt :-> TInt :-> TInt
binopType Equals = TInt :-> TInt :-> TBool
binopType NotEquals = TInt :-> TInt :-> TBool
binopType GreaterThan = TInt :-> TInt :-> TBool
binopType GreaterThanEquals = TInt :-> TInt :-> TBool
binopType LessThan = TInt :-> TInt :-> TBool
binopType LessThanEquals = TInt :-> TInt :-> TBool
binopType And = TBool :-> TBool :-> TBool
binopType Or = TBool :-> TBool :-> TBool

unaryType :: Unary -> Type
unaryType Not = TBool :-> TBool
unaryType _ = error "unaryType: operation not yet complete"

addConstraint :: (MonadWriter TyInferred m) => TyConstraint -> m ()
addConstraint c = tell $ InferredTy { tyConstraints = [c] }

getConstraints :: (MonadWriter TyInferred m) => m a -> m (a, [TyConstraint])
getConstraints = listens tyConstraints

generalizeFromEnv :: (MonadReader TyEnv m) => Type -> m TyScheme
generalizeFromEnv t = do
    vm <- asks varTypes
    return $ generalize vm t

withVars :: (MonadReader TyEnv m) => Map Name TyScheme -> m a -> m a
withVars vm = local (\env -> env { varTypes = Map.union vm (varTypes env) })

infer :: (MonadUnique m, MonadReader TyEnv m, MonadWriter TyInferred m, MonadError TypeError m) => Expr DelayPass -> m Type
infer = \case
    (Var x) -> lookupVar (danName x)
    (Const k) -> valueType k
    (If c t f) -> do
        ct <- infer c
        tt <- infer t
        ft <- infer f
        addConstraint (ct :~: TBool)
        addConstraint (tt :~: ft)
        return tt
    (Fby l r) -> do
        lt <- infer l
        rt <- infer r
        addConstraint (lt :~: rt)
        return lt
    (Next e) -> infer e
    (Where body bs) -> do
        -- Make type variables for each var
        let (bsVars, bsExprs) = unzip $ Map.toList $ Map.mapKeys danName bs
        tvs <- traverse (const (generalizeFromEnv =<< fresh)) bsVars
        let bsTySchemes = Map.fromList $ zip bsVars tvs
        (bsTypes, cs) <- getConstraints $ withVars bsTySchemes $ traverse infer bsExprs
        subst <- solver cs
        let schemes = fmap (\t -> generalize (apply subst bsTySchemes) (apply subst t)) bsTypes
        let bsTySchemes' = Map.fromList $ zip bsVars schemes
        withVars bsTySchemes' $ infer body
    (App fname args) -> undefined
    (Binop op l r) -> do
        lt <- infer l
        rt <- infer r
        tv <- fresh
        let u1 = lt :-> rt :-> tv
        let u2 = binopType op
        addConstraint (u1 :~: u2)
        return tv
    (Unary op e) -> do
        et <- infer e
        tv <- fresh
        let u1 = et :-> tv
        let u2 = unaryType op
        addConstraint (u1 :~: u2)
        return tv

bind :: (MonadError TypeError m) => TVar -> Type -> m (Subst TVar Type)
bind x t | TVar x == t = return emptySubst
         | x `Set.member` fv t = throwError $ InfiniteType x t
         | otherwise = return $ singletonSubst x t

unify :: (MonadError TypeError m) => Type -> Type -> m (Subst TVar Type)
unify t1 t2 | t1 == t2 = return emptySubst
unify (TVar tv) t = bind tv t
unify t (TVar tv) = bind tv t
unify (t1 :-> t2) (t3 :-> t4) = do
    s1 <- unify t1 t3
    s2 <- unify (apply s1 t2) (apply s1 t4)
    return (s2 <> s1)
unify t1 t2 = throwError $ UnificationError t1 t2

solver :: (MonadError TypeError m) => [TyConstraint] -> m (Subst TVar Type)
solver = go emptySubst 
    where
        go subst [] = return subst
        go subst (t1 :~: t2:cs) = do
            s <- unify t1 t2
            go (s <> subst) (apply s cs)

inferFn :: (MonadReader TyEnv m, MonadUnique m, MonadError TypeError m) => Function DelayPass -> m TyScheme
inferFn (Function _ args body _) = do
    tvArgs <- traverse (const (generalizeFromEnv =<< fresh)) args
    let argMap = Map.fromList $ zip (fmap danName args) tvArgs
    (t, cs) <- withVars argMap $ runWriterT $ infer body
    subst <- solver (tyConstraints cs)
    env <- getVars
    return $ generalize (apply subst env) (apply subst t)

inferFns :: [Function DelayPass] -> Either ErrorMessage [TyScheme]
inferFns fs = 
    let initEnv = TyEnv { varTypes = Map.empty, fnTypes = Map.empty }
    in first toError $ runExcept $ runUniqueT $ flip runReaderT initEnv $ traverse inferFn fs
-- tcFunction :: Function DelayPass -> Either ErrorMessage Type
-- tcFunction (Function _ args body _) = first toError $ runExcept $ runUniqueT $ flip runReaderT initEnv $ do
--     tvArgs <- traverse (const (generalizeFromEnv =<< fresh)) args
--     let argMap = Map.fromList $ zip (fmap danName args) tvArgs
--     return TInt
--     where