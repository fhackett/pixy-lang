module Pixy.PrettyPrint where

import Text.PrettyPrint

import Pixy.Syntax
import Pixy.Delay

class Pretty p where
    ppr :: p -> Doc

instance Pretty Int where
    ppr = integer . toInteger

instance Pretty Value where
    ppr (VBool b) = if b then text "true" else text "false"
    ppr (VInt k) = ppr k
    ppr (VNil) = text "nil"

binop :: String -> Expr -> Expr -> Doc
binop s l r = ppr l <+> text s <+> ppr r

brackets :: Doc -> Doc
brackets d = text "{" <+> d <+> text "}"

instance Pretty Expr where
    ppr (Var x) = text x
    ppr (Const k) = ppr k
    ppr (Check e) = text "?" <> ppr e
    ppr (If c t f) = text "if" <+> ppr c <+> text "then" <+> ppr t <+> text "else" <+> ppr f
    ppr (Fby s n) = ppr s <+> text "fby" <+> ppr n
    ppr (Next e) = text "next" <+> ppr e
    -- ppr (Where body bs) = ppr body <+> text "where" <> brackets (vcat pbs)
--     --     lunbind bnd $ \(unrec -> bs,bdy) -> do
--     --         pbs <- mapM ((\(v,e) -> do
--     --             pv <- ppr v
--     --             pe <- ppr (unembed e)
--     --             return $ pv <+> text "=" <+> pe)) bs
--     --         pbdy <- ppr bdy
--     --         return $ pbdy <+> text "where" <+> vcat pbs
--     ppr (App f args) = do
--         pargs <- mapM ppr args
--         return $ text f <> parens (hsep $ punctuate comma pargs)
--     ppr (Binop Plus e1 e2) = binop "+" e1 e2
--     ppr (Binop Minus e1 e2) = binop "-" e1 e2
--     ppr (Binop Times e1 e2) = binop "*" e1 e2
--     ppr (Binop Divide e1 e2) = binop "/" e1 e2
--     ppr (Binop Equals e1 e2) = binop "==" e1 e2

-- -- instance Pretty Function where
-- --     ppr (Function name bnd) =
-- --         lunbind bnd $ \(args, bdy) -> do
-- --             pargs <- mapM ppr args
-- --             pbdy <- ppr bdy
-- --             return $ text name <> parens(hsep $ punctuate comma pargs) <+> text "=" <+> pbdy

instance Pretty CVar where
    ppr (Gen s k) = text s <> ppr k
    ppr (Bound x) = text x

instance Pretty CVal where
    ppr (CVar x) = ppr x
    ppr (CVal k) = ppr k

delay :: CVal -> Doc
delay (CVar x) = text "d" <> parens (ppr x)
delay (CVal k) = ppr k

delay' :: CVar -> Doc
delay' = delay . CVar

instance Pretty Constraint where
    ppr (K x k) = delay' x <+> text "=" <+> ppr k
    ppr (E x y k) = 
        let b = delay' x <+> text "=" <+> delay' y 
        in if k == 0 then b else b <+> text "+" <+> ppr k
    ppr (LE _ x y k) = 
        let b = delay x <+> text "<=" <+> delay y 
        in if k == 0 then b else b <+> text "+" <+> ppr k
    ppr (Sub x y z k) = 
        let b = delay' x <+> text "=" <+> delay y <+> text "-" <+> delay z
        in if k == 0 then b else b <+> text "+" <+> ppr k
    ppr (Max x ys k) = 
        let b = delay' x <+> text "=" <+> text "max" <> parens (hsep $ punctuate comma $ fmap ppr ys)
        in if k == 0 then b else b <+> text "+" <+> ppr k


pp :: (Pretty a) => a -> String
pp = render . ppr
