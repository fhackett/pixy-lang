{-# LANGUAGE GADTs #-}
module Pixy.PrettyPrint where

import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Map (Map)
import Data.List (foldl')

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String

import Pixy.Data.Subst
import Pixy.Data.Name
import Pixy.Data.Delay
import Pixy.Data.Type
import Pixy.Syntax

instance Pretty Value where
    pretty (VBool b) = pretty b
    pretty (VInt k) = pretty k
    pretty (VNil) = pretty "nil"

instance Pretty Name where
    pretty n = pretty (displayName n) <> pretty "_" <> pretty (uniqueName n)

instance Pretty DelayAnnName where
    pretty n = pretty (dAnnName n) <> brackets (pretty $ dAnnDelay n)

instance (Pretty v, Pretty a) => Pretty (Subst v a) where
    pretty (Subst s) = pretty s

instance (Num a, Eq a, Pretty a, Pretty v) => Pretty (LinearEq v a) where
    pretty (LinearEq tms c) = 
        let pcoeff coeff = if coeff == 1 then emptyDoc else pretty coeff <> pretty "*"
        in Map.foldrWithKey (\x coeff acc -> pcoeff coeff <> pretty x <+> pretty "+" <+> acc) (pretty c) tms 

instance (Num a, Eq a, Pretty a, Pretty v) => Pretty (DelayEq v a) where
    pretty (Maximum [eq]) = pretty eq
    pretty (Maximum eqs) = pretty "max" <> parens (hsep $ punctuate comma (fmap pretty eqs))

instance Pretty (Constraint a) where
    pretty (x :==: y) = pretty x <+> pretty "==" <+> pretty y
    pretty (x :>=: y) = pretty x <+> pretty ">=" <+> pretty y

instance Pretty Constraints where
    pretty (Constraints ecs cs) = parens (hsep $ punctuate comma (fmap pretty ecs ++ fmap pretty cs))

instance Pretty TVar where
    pretty (TV x) = pretty x

instance Pretty Type where
    pretty (TVar x) = pretty x
    pretty (TInt) = pretty "Int"
    pretty (TBool) = pretty "Bool"
    pretty (TNil t) = pretty "Nil" <+> pretty t
    pretty (t1 :-> t2) = pretty t1 <+> pretty "->" <+> pretty t2

instance Pretty TyConstraint where
    pretty (t1 :~: t2) = pretty t1 <+> pretty "~" <+> pretty t2

instance Pretty TyScheme where
    pretty (ForAll [] t) = pretty t
    pretty (ForAll tvs t) = pretty "forall" <+> hsep (fmap pretty tvs) <> dot <+> pretty t

instance Pretty Binop where
    pretty Plus = pretty "+"
    pretty Minus = pretty "-"
    pretty Times = pretty "*"
    pretty Divide = pretty "/"
    pretty Modulo = pretty "%"
    pretty Equals = pretty "=="
    pretty NotEquals = pretty "!="
    pretty GreaterThan = pretty ">"
    pretty GreaterThanEquals = pretty ">="
    pretty LessThan = pretty "<"
    pretty LessThanEquals = pretty "<="
    pretty And = pretty "&&"
    pretty Or = pretty "||"

instance Pretty Unary where
    pretty Not = pretty "!"
    pretty Check = pretty "?"
    pretty Trace = pretty "trace"

instance Pretty a => Pretty (Seq a) where
    pretty xs = brackets $ hsep $ punctuate comma (toList $ fmap pretty xs)

instance (Pretty k, Pretty a) => Pretty (Map k a) where
    pretty m = braces $ hsep $ punctuate comma (toList $ Map.mapWithKey (\k a -> pretty k <> colon <+> pretty a) m)

instance Pretty (Expr ParsePass) where
    pretty (Var x) = pretty x
    pretty (Const k) = pretty k
    pretty (If c t f) = pretty "if" <+> pretty c <+> pretty "then" <+> pretty t <+> pretty "else" <+> pretty f
    pretty (Fby l r) = pretty l <+> pretty "fby" <+> pretty r
    pretty (Next e) = pretty "next" <+> pretty e
    pretty (Where body bs) =
        let pbindings = fmap (\(x,e) -> pretty x <+> pretty "=" <+> pretty e) $ Map.toList bs
        in pretty body <+> pretty "where" <+> braces (nest 4 (line <> vsep pbindings) <> line)
    pretty (App fname args) =
        let pargs = punctuate comma $ fmap pretty args
        in pretty fname <> parens (hsep pargs)
    pretty (Binop op l r) = pretty l <+> pretty op <+> pretty r
    pretty (Unary op e) = pretty op <> pretty e

instance Pretty (Function ParsePass) where
    pretty (Function fname args body _) = 
        let pargs = punctuate comma $ fmap pretty args
        in pretty fname <> parens (hsep pargs) <+> pretty "=" <+> pretty body

instance Pretty (Expr RenamePass) where
    pretty (Var x) = pretty x
    pretty (Const k) = pretty k
    pretty (If c t f) = pretty "if" <+> pretty c <+> pretty "then" <+> pretty t <+> pretty "else" <+> pretty f
    pretty (Fby l r) = pretty l <+> pretty "fby" <+> pretty r
    pretty (Next e) = pretty "next" <+> pretty e
    pretty (Where body bs) =
        let pbindings = fmap (\(x,e) -> pretty x <+> pretty "=" <+> pretty e) $ Map.toList bs
        in pretty body <+> pretty "where" <+> braces (nest 4 (line <> vsep pbindings) <> line)
    pretty (App fname args) =
        let pargs = punctuate comma $ fmap pretty args
        in pretty fname <> parens (hsep pargs)
    pretty (Binop op l r) = pretty l <+> pretty op <+> pretty r
    pretty (Unary op e) = pretty op <> pretty e

instance Pretty (Function RenamePass) where
    pretty (Function fname args body _) = 
        let pargs = punctuate comma $ fmap pretty args
        in pretty fname <> parens (hsep pargs) <+> pretty "=" <+> pretty body

instance Pretty (Expr DelayPass) where
    pretty (Var x) = pretty x
    pretty (Const k) = pretty k
    pretty (If c t f) = pretty "if" <+> pretty c <+> pretty "then" <+> pretty t <+> pretty "else" <+> pretty f
    pretty (Fby l r) = pretty l <+> pretty "fby" <+> pretty r
    pretty (Next e) = pretty "next" <+> pretty e
    pretty (Where body bs) =
        let pbindings = fmap (\(x,e) -> pretty x <+> pretty "=" <+> pretty e) $ Map.toList bs
        in pretty body <+> pretty "where" <+> braces (nest 4 (line <> vsep pbindings) <> line)
    pretty (App fname args) =
        let pargs = punctuate comma $ fmap pretty args
        in pretty fname <> parens (hsep pargs)
    pretty (Binop op l r) = pretty l <+> pretty op <+> pretty r
    pretty (Unary op e) = pretty op <> pretty e

instance Pretty (Function DelayPass) where
    pretty (Function fname args body cs) = 
        let pargs = punctuate comma $ fmap pretty args
        in pretty cs <+> pretty "=>" <+> pretty fname <> parens (hsep pargs) <+> pretty "=" <+> pretty body

pShow :: (Pretty a) => a -> String
pShow = renderString . layoutPretty defaultLayoutOptions . pretty

-- instance Pretty (Expr DelayPass) where
--     pretty (Var x) = pretty x
--     pretty (Const k) = pretty k
--     pretty (If c t f) = pretty "if" <+> pretty c <+> pretty "then" <+> pretty t <+> pretty "else" <+> pretty f
--     pretty (Fby l r) = pretty l <+> pretty "fby" <+> pretty r
--     pretty (Next e) = pretty "next" <+> pretty e
--     pretty (Where body bs) =
--         let pbindings = fmap (\(x,e) -> pretty x <+> pretty "=" <+> pretty e) $ Map.toList bs
--         in pretty body <+> pretty "where" <+> braces (vsep pbindings)
--     pretty (App fname args) =
--         let pargs = punctuate comma $ fmap pretty args
--         in pretty fname <> parens (hsep pargs)
--     pretty (Binop op l r) = pretty l <+> pretty op <+> pretty r
--     pretty (Unary op e) = pretty op <> pretty e


-- instance Pretty (Var, VarInfo) where
--     pretty (x, VarInfo e d buff) = pretty x <> parens (pretty d) <> pretty buff <+> pretty "=" <+> pretty e

-- instance Pretty ExprS where
--     pretty (VarS x offset) = pretty x <> brackets (pretty offset)
--     pretty (ConstS k) = pretty k
--     pretty (IfS c t f) = pretty "if" <+> pretty c <+> pretty "then" <+> pretty t <+> pretty "else" <+> pretty f
--     pretty (FbyS latch l r) = pretty l <+> pretty "fby" <> brackets (pretty latch) <+> pretty r
--     pretty (WhereS body bindings) = 
--         let pbindings = fmap pretty $ Map.toList bindings
--         in pretty body <+> pretty "where" <+> braces (vcat pbindings)
--     pretty (AppS body args) =
--         let pargs = punctuate comma $ fmap pretty $ Map.toList args
--         in braces (pretty body) <> parens (hsep pargs)
--     pretty (BinopS op l r) = pretty l <+> pretty op <+> pretty r
--     pretty (UnaryS op e) = pretty op <> pretty e