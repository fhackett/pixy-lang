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
import Pixy.Data.Type
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
    | Where (Expr p) (Map (DeclP p) (Expr p))
    | App (IdP p) (AppP p)
    | Binop Binop (Expr p) (Expr p)
    | Unary Unary (Expr p)

data Function p = Function
    { fnName :: DeclP p
    , fnArgs :: [IdP p]
    , fnBody :: Expr p
    , fnInfo :: FnP p
    }

ud :: Void
ud = error "Tried to evaluate void!"

data TyAnnName = TyAnnName
    { tyAnnName :: Name
    , tyAnnType :: TyScheme
    }

instance Eq TyAnnName where
    n1 == n2 = tyAnnName n1 == tyAnnName n2

instance Ord TyAnnName where
    compare n1 n2 = compare (tyAnnName n1) (tyAnnName n2)

data DelayAnnName = DelayAnnName
    { dAnnName :: Name
    , dAnnDelay :: Delay
    }

-- data BufferAnnName = BufferAnnName
--     { banName :: Name
--     , banDelay :: Delay
--     , banBufferSize :: Int
--     }

instance Eq DelayAnnName where
    n1 == n2 = dAnnName n1 == dAnnName n2

instance Ord DelayAnnName where
    compare n1 n2 = compare (dAnnName n1) (dAnnName n2)

data ParsePass
data RenamePass
data TypeCheckPass
data DelayPass
-- data BufferPass

type family IdP p
type instance IdP ParsePass = Text
type instance IdP RenamePass = Name
type instance IdP TypeCheckPass = Name
type instance IdP DelayPass = Name

type family DeclP p
type instance DeclP ParsePass = Text
type instance DeclP RenamePass = Name
type instance DeclP TypeCheckPass = TyAnnName
type instance DeclP DelayPass = DelayAnnName

type family AppP p
type instance AppP ParsePass = [Expr ParsePass]
type instance AppP RenamePass = [Expr RenamePass]
type instance AppP TypeCheckPass = [Expr TypeCheckPass]
type instance AppP DelayPass = [Expr DelayPass]

type family FnP p
type instance FnP ParsePass = Void
type instance FnP RenamePass = Void
type instance FnP TypeCheckPass = Void
type instance FnP DelayPass = Constraints


