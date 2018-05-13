module Pixy.Pass.RenamePass(renamePass) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Data.Bifunctor

import Debug.Trace

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Data.Text.Prettyprint.Doc
import Data.Text (Text)

import Pixy.Syntax
import Pixy.Error
import Pixy.Data.Name
import Pixy.Data.Unique

data RenameError
    = UndefinedVariable Text
    | UndefinedFunction Text

instance Error RenameError where
    toError (UndefinedVariable v) = ErrorMessage { errorDescription =  pretty "Undefined variable:" <+> pretty v }
    toError (UndefinedFunction f) = ErrorMessage { errorDescription =  pretty "Undefined function:" <+> pretty f }


data RenameEnv = RenameEnv
    { varNames :: Map Text Name
    , functionNames :: Map Text Name
    }

type Rn = UniqueT (ReaderT RenameEnv (Except RenameError))

runRename :: Rn a -> Either RenameError a
runRename rn = 
    let initState = RenameEnv { varNames = Map.empty, functionNames = Map.empty }
    in runExcept $ flip runReaderT initState $ runUniqueT rn

lookupVar :: Text -> Rn Name
lookupVar var = do
    name <- asks (Map.lookup var . varNames)
    case name of
        Just n -> return n
        Nothing -> throwError $ UndefinedVariable var

withRenamedVars :: Map Text Name -> Rn a -> Rn a
withRenamedVars vm = local (\s -> s { varNames = Map.union vm (varNames s) })

lookupFunction :: Text -> Rn Name
lookupFunction fname = do
    name <- asks (Map.lookup fname . functionNames)
    case name of
        Just n -> return n
        Nothing -> throwError $ UndefinedFunction fname

withRenamedFunctions :: Map Text Name -> Rn a -> Rn a
withRenamedFunctions fm = local (\s -> s { functionNames = Map.union fm (functionNames s) })

renamed :: [Text] -> Rn (Map Text Name)
renamed ss = Map.fromList <$> traverse (\s -> (s, ) . ($s) <$> fresh) ss

rename :: Expr ParsePass -> Rn (Expr RenamePass)
rename (Var n) = Var <$> lookupVar n
rename (Const k) = return $ Const k
rename (If c t f) = If <$> rename c <*> rename t <*> rename f
rename (Fby l r) = Fby <$> rename l <*> rename r
rename (Next e) = Next <$> rename e
rename (Where body bs) = do
    let (vars, exprs) = unzip $ Map.toList bs
    rnVarMap <- renamed vars
    rnExprs <- withRenamedVars rnVarMap $ traverse rename exprs
    rnBody <- withRenamedVars rnVarMap $ rename body
    let rnBs = Map.fromList $ zip (Map.elems rnVarMap) rnExprs
    return $ Where rnBody rnBs
rename (App fname args) = do
    rnFname <- lookupFunction fname
    rnArgs <- traverse rename args
    return $ App rnFname rnArgs
rename (Binop op l r) = Binop op <$> rename l <*> rename r
rename (Unary op e) = Unary op <$> rename e

renameFunctions :: [Function ParsePass] -> Rn [Function RenamePass]
renameFunctions fs = do
    rnFnames <- renamed $ fnName <$> fs
    withRenamedFunctions rnFnames $ traverse renameFunction fs
    where
        renameFunction :: Function ParsePass -> Rn (Function RenamePass)
        renameFunction f = do
            rnFname <- lookupFunction $ fnName f
            rnArgs <- renamed $ fnArgs f
            rnBody <- withRenamedVars rnArgs $ rename $ fnBody f
            return Function { fnName = rnFname, fnArgs = Map.elems rnArgs, fnBody = rnBody, fnInfo = ud }

renamePass :: [Function ParsePass] -> Either ErrorMessage [Function RenamePass]
renamePass fs = first toError $ runRename $ renameFunctions fs