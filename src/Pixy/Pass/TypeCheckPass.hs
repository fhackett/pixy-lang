module Pixy.Pass.TypeCheckPass where

import Control.Monad.Reader
import Control.Monad.Writer.Strict hiding ((<>))
import Control.Monad.State.Strict
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
    { varTypes :: Map Name Type
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
    | UnboundFunction Name
    | InfiniteType TVar Type

instance Error TypeError where
    toError (UnificationError t1 t2) = ErrorMessage { errorDescription = pretty "Unification Error:" <+> pretty t1 <+> pretty "and" <+> pretty t2} 
    toError (UnboundVariable n) = ErrorMessage { errorDescription = pretty "Unbound Variable:" <+> pretty n }
    toError (UnboundFunction n) = ErrorMessage { errorDescription = pretty "Unbound Function:" <+> pretty n }
    toError (InfiniteType tv t2) = ErrorMessage { errorDescription = pretty "Infinite Type:" <+> pretty tv <+> pretty "and" <+> pretty t2} 

lookupVar :: (MonadReader TyEnv m, MonadUnique m, MonadError TypeError m) => Name -> m Type
lookupVar x = do
    sc <- asks ((Map.lookup x) . varTypes)
    case sc of
        Just sc -> return sc
        Nothing -> throwError $ UnboundVariable x

lookupFn :: (MonadReader TyEnv m, MonadUnique m, MonadError TypeError m) => Name -> m TyScheme
lookupFn x = do
    sc <- asks ((Map.lookup x) . fnTypes)
    case sc of
        Just sc -> return sc
        Nothing -> throwError $ UnboundFunction x

getVars :: (MonadReader TyEnv m) => m (Map Name Type)
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

withVars :: (MonadReader TyEnv m) => Map Name Type -> m a -> m a
withVars vm = local (\env -> env { varTypes = Map.union vm (varTypes env) })

infer :: (MonadUnique m, MonadReader TyEnv m, MonadWriter TyInferred m, MonadError TypeError m) => Expr RenamePass -> m (Expr TypeCheckPass, Type)
infer = \case
    (Var x) -> (Var x,) <$> (lookupVar x)
    (Const k) -> (Const k,) <$> valueType k
    (If c t f) -> do
        (c', ct) <- infer c
        (t', tt) <- infer t
        (f', ft) <- infer f
        addConstraint (ct :~: TBool)
        addConstraint (tt :~: ft)
        return (If c' t' f', tt)
    (Fby l r) -> do
        (l', lt) <- infer l
        (r', rt) <- infer r
        addConstraint (lt :~: rt)
        return (Fby l' r', lt)
    (Next e) -> do
        (e', ed) <- infer e
        return (Next e', ed)
    (Where body bs) -> do
        -- Make type variables for each var
        let (bsVars, bsExprs) = unzip $ Map.toList bs
        tvs <- traverse (const fresh) bsVars
        let bsTyMap = Map.fromList $ zip bsVars tvs
        ((bsExprs', bsTypes), cs) <- getConstraints $ withVars bsTyMap $ (unzip <$> traverse infer bsExprs)
        subst <- solver cs
        let bsTypes' = fmap (apply subst) bsTypes
        let bsTyMap' = Map.fromList $ zip bsVars bsTypes'
        let bsVars' = zipWith TyAnnName bsVars bsTypes' 
        let bs' = Map.fromList $ zip bsVars' bsExprs'
        (body', bodyt) <- withVars bsTyMap' $ infer body
        return (Where body' bs', bodyt)
    (App fname args) -> do
        fTy1 <- instantiate =<< lookupFn fname
        (args', argts) <- unzip <$> traverse infer args
        fv <- fresh
        let fTy2 = foldr (:->) fv argts
        addConstraint (fTy1 :~: fTy2)
        return (App fname args', fv)
    (Binop op l r) -> do
        (l', lt) <- infer l
        (r', rt) <- infer r
        tv <- fresh
        let u1 = lt :-> rt :-> tv
        let u2 = binopType op
        addConstraint (u1 :~: u2)
        return (Binop op l' r', tv)
    (Unary op e) -> do
        (e', et) <- infer e
        tv <- fresh
        let u1 = et :-> tv
        let u2 = unaryType op
        addConstraint (u1 :~: u2)
        return (Unary op e', tv)

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

inferFn :: (MonadState (Map Name TyScheme) m, MonadUnique m, MonadError TypeError m) => Function RenamePass -> m (Function TypeCheckPass)
inferFn (Function fname args body _) = do
    fTypes <- get
    tvArgs <- traverse (const fresh) args
    let argMap = Map.fromList $ zip args tvArgs
    let initEnv = TyEnv { varTypes = argMap, fnTypes = fTypes }
    ((annBody, t), cs) <- runWriterT $ runReaderT (infer body) initEnv

    subst <- solver (tyConstraints cs)
    let fnScheme = generalize $ apply subst $ foldr (:->) t tvArgs
    put (Map.insert fname fnScheme fTypes)
    return (Function fname args annBody fnScheme)

typeCheckPass :: [Function RenamePass] -> Either ErrorMessage [Function TypeCheckPass]
typeCheckPass fs = first toError $ runExcept $ runUniqueT $ evalStateT (traverse inferFn fs) Map.empty
-- inferFns :: (MonadState (Map Name TyScheme) m, MonadError TypeError m) => [Function RenamePass] -> m [TyScheme]
-- inferFns fs = do
    -- fnTypes <- get
    -- let initEnv = TyEnv { varTypes = Map.empty, fnTypes = empty }
    -- in first toError $ runExcept $ runUniqueT $ flip runReaderT initEnv $ traverse inferFn fs
-- tcFunction :: Function DelayPass -> Either ErrorMessage Type
-- tcFunction (Function _ args body _) = first toError $ runExcept $ runUniqueT $ flip runReaderT initEnv $ do
--     tvArgs <- traverse (const (generalizeFromEnv =<< fresh)) args
--     let argMap = Map.fromList $ zip (fmap danName args) tvArgs
--     return TInt
--     where