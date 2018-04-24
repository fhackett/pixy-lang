{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
module Pixy.Syntax where

import Data.Void
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.STRef

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
    | Modulo
    | Equals
    | NotEquals
    | GreaterThan
    | GreaterThanEquals
    | LessThan
    | LessThanEquals
    | And
    | Or
    deriving (Show)

data Unary
    = Not
    | Check
    | Trace
    deriving (Show)

data Value 
    = VInt !Int
    | VBool !Bool
    | VNil
    deriving (Show, Eq)

data Expr
    = Var Var
    | Const Value
    | If Expr Expr Expr
    | Fby Expr Expr
    | Next Expr
    | Where Expr [(Var, Expr)]
    | App FName [Expr]
    | Binop Binop Expr Expr
    | Unary Unary Expr
    deriving (Show)

-- AST decorated with the state needed for execution
data ExprS
    = VarS !Var !Int
    | ConstS !Value
    | IfS !ExprS !ExprS !ExprS
    | FbyS !Bool !ExprS !ExprS
    | WhereS !ExprS !(Map Var VarInfo)
    | AppS !ExprS !(Map Var VarInfo)
    | BinopS !Binop !ExprS !ExprS
    | UnaryS !Unary !ExprS
    deriving (Show)

data VarInfo = VarInfo 
    { varExpr :: ExprS
    , varDelay :: Int
    , varBuffer :: Seq Value
    }
    deriving (Show)




