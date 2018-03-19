module Pixy.Syntax where

type Var = String

data Statement = Print Expr
    deriving (Show)

data Function = Function String [Var] Expr
    deriving (Show)

data BinOp
    = Plus
    | Minus
    | Times
    | Divide
    | Equals
    deriving (Show, Eq)

data Expr
    = Nil
    | Var Var
    | Const !Int
    | Exists Expr
    | If Expr Expr Expr
    | Fby Expr Expr
    | Where Expr [(Var, Expr)] 
    | App String [Expr]
    | BinExpr BinOp Expr Expr
    deriving (Show, Eq)
    -- | Tuple [Expr]