{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
module Pixy.Syntax where

import Data.Void
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Data.Text (Text)

import Pixy.Data.Unique
import Pixy.Data.Name
import Pixy.Data.Delay


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

-- data Value 
--     = VInt !Int
--     | VBool !Bool
--     | VNil
--     deriving (Show, Eq)

-- data Expr
--     = Var Var
--     | Const Value
--     | If Expr Expr Expr
--     | Fby Expr Expr
--     | Next Expr
--     | Where Expr [(Var, Expr)]
--     | App FName [Expr]
--     | Binop Binop Expr Expr
--     | Unary Unary Expr
--     deriving (Show)

-- -- AST decorated with the state needed for execution
-- data ExprS
--     = VarS !Var !Int
--     | ConstS !Value
--     | IfS !ExprS !ExprS !ExprS
--     | FbyS !Bool !ExprS !ExprS
--     | WhereS !ExprS !(Map Var VarInfo)
--     | AppS !ExprS !(Map Var VarInfo)
--     | BinopS !Binop !ExprS !ExprS
--     | UnaryS !Unary !ExprS
--     deriving (Show)

-- data VarInfo = VarInfo 
--     { varExpr :: ExprS
--     , varDelay :: Int
--     , varBuffer :: Seq Value
--     }
--     deriving (Show)

data Value
    = VInt !Int
    | VBool !Bool
    | VNil
    deriving (Show, Eq)



data Expr p
    = Var (IdP p)
    | Const Value
    | If (Expr p) (Expr p) (Expr p)
    | Fby (Expr p) (Expr p)
    | Next (Expr p)
    | Where (Expr p) (Map (IdP p) (Expr p))
    | App (IdP p) (AppP p)
    | Binop Binop (Expr p) (Expr p)
    | Unary Unary (Expr p)

data Function p = Function
    { fnName :: IdP p
    , fnArgs :: [IdP p]
    , fnBody :: Expr p
    , fnInfo :: FnP p
    }

ud :: Void
ud = error "Tried to evaluate void!"

data DelayAnnName = DelayAnnName
    { danName :: Name
    , danDelay :: Delay
    }

data BufferAnnName = BufferAnnName
    { banName :: Name
    , banDelay :: Delay
    , banBufferSize :: Int
    }

instance Eq DelayAnnName where
    n1 == n2 = danName n1 == danName n2

instance Ord DelayAnnName where
    compare n1 n2 = compare (danName n1) (danName n2)

data ParsePass
data RenamePass
data DelayPass
data TypeCheckPass
data BufferPass

type family IdP p
type instance IdP ParsePass = Text
type instance IdP RenamePass = Name
type instance IdP DelayPass = DelayAnnName
-- type instance IdP BufferPass = DelayAnnName

type family BufferP p
type instance IdP ParsePass = Text
type instance IdP RenamePass = Name
type instance IdP DelayPass = DelayAnnName

type family AppP p
type instance AppP ParsePass = [Expr ParsePass]
type instance AppP RenamePass = [Expr RenamePass]
type instance AppP DelayPass = [Expr DelayPass]

type family FnP p
type instance FnP ParsePass = Void
type instance FnP RenamePass = Void
type instance FnP DelayPass = Constraints


