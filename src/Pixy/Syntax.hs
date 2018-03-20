{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
module Pixy.Syntax where

import Data.Void
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

type Var = String
type FName = String

-- data Function = Function String (Bind [Var] Expr)
data Function = Function String [Var] Expr
    deriving (Show)
data Binop
    = Plus
    | Minus
    | Times
    | Divide
    | Equals
    deriving (Show)

data Value 
    = VInt !Int
    | VBool !Bool
    | VNil
    deriving (Show)

data Expr
    = Var Var
    | Const Value
    | If Expr Expr Expr
    | Check Expr
    | Fby Expr Expr
    | Next Expr
    | Where Expr [(Var, Expr)]
    | App FName [Expr]
    | Binop Binop Expr Expr
    deriving (Show)

-- AST decorated with the state needed for execution
data ExprS
    = VarS Var
    | ConstS Value
    | IfS ExprS ExprS ExprS
    | CheckS ExprS
    | FbyS Bool ExprS ExprS
    | NextS Bool Value ExprS
    | WhereS ExprS (Map Var (ExprS, Value))
    | AppS ExprS (Map Var ExprS)
    | BinopS Binop ExprS ExprS