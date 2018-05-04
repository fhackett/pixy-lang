module Pixy.PrettyPrint where

import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.List (foldl')

import Text.PrettyPrint

import Pixy.Syntax

class Pretty p where
    ppr :: p -> Doc

instance Pretty Int where
    ppr = integer . toInteger

instance Pretty Bool where
    ppr True = text "true"
    ppr False = text "false"

instance Pretty Value where
    ppr (VBool b) = if b then text "true" else text "false"
    ppr (VInt k) = ppr k
    ppr (VNil) = text "nil"

instance Pretty Binop where
    ppr Plus = text "+"
    ppr Minus = text "-"
    ppr Times = text "*"
    ppr Divide = text "/"
    ppr Modulo = text "%"
    ppr Equals = text "=="
    ppr NotEquals = text "!="
    ppr GreaterThan = text ">"
    ppr GreaterThanEquals = text ">="
    ppr LessThan = text "<"
    ppr LessThanEquals = text "<="
    ppr And = text "&&"
    ppr Or = text "||"

instance Pretty Unary where
    ppr Not = text "!"
    ppr Check = text "?"
    ppr Trace = text "trace"

instance Pretty a => Pretty (Seq a) where
    ppr xs = brackets $ hsep $ punctuate comma (toList $ fmap ppr xs)

instance Pretty (Var, VarInfo) where
    ppr (x, VarInfo e d buff) = text x <> parens (ppr d) <> ppr buff <+> text "=" <+> ppr e

instance Pretty ExprS where
    ppr (VarS x offset) = text x <> brackets (ppr offset)
    ppr (ConstS k) = ppr k
    ppr (IfS c t f) = text "if" <+> ppr c <+> text "then" <+> ppr t <+> text "else" <+> ppr f
    ppr (FbyS latch l r) = ppr l <+> text "fby" <> brackets (ppr latch) <+> ppr r
    ppr (WhereS body bindings) = 
        let pbindings = fmap ppr $ Map.toList bindings
        in ppr body <+> text "where" <+> braces (vcat pbindings)
    ppr (AppS body args) =
        let pargs = punctuate comma $ fmap ppr $ Map.toList args
        in braces (ppr body) <> parens (hsep pargs)
    ppr (BinopS op l r) = ppr l <+> ppr op <+> ppr r
    ppr (UnaryS op e) = ppr op <> ppr e

pp :: (Pretty a) => a -> String
pp = render . ppr
