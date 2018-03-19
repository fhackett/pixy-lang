module Pixy.Syntax where

type Var = String

data Statement = Print Expr
    deriving (Show)

data Function = Function String [Var] Expr

data BinOp
    = Plus
    | Minus
    | Times
    | Divide
    | Equals
    deriving (Show)

data Expr
    = Nil
    | Var Var
    | Const !Int
    | Seq Statement Expr
    | If Expr Expr Expr
    | Fby Expr Expr
    | Where Expr [(Var, Expr)] 
    | App String [Expr]
    | BinExpr BinOp Expr Expr
    deriving (Show)
    -- | Tuple [Expr]