module Pixy.PrettyPrint where

import Text.PrettyPrint

import Pixy.Syntax

-- class Pretty p where
--     ppr :: p -> Doc

-- instance Pretty Value where
--     ppr (VBool b) = if b then text "true" else text "false"
--     ppr (VInt k) = integer $ toInteger k
--     ppr (VNil) = text "nil"

-- binop :: String -> Expr -> Expr -> Doc
-- binop s l r = do
--     pl <- ppr l
--     pr <- ppr r
--     return $ pl <+> text s <+> pr

-- instance Pretty Expr where
--     ppr (Var x) = return $ text x
--     ppr (Const k) = ppr k
--     ppr (Check e) = do
--         pe <- ppr e
--         return $ text "?" <> pe
--     ppr (If c t f) = do
--         pc <- ppr c
--         pt <- ppr t
--         pf <- ppr f
--         return $ text "if" <+> pc <+> text "then" <+> pt <+> text "else" <+> pf
--     ppr (Fby s n) = do
--         ps <- ppr s
--         pn <- ppr n
--         return $ ps <+> text "fby" <+> pn
--     ppr (Next e) = do
--         pe <- ppr e
--         return $ text "next" <+> pe
--     -- ppr (Where bnd) = 
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

-- pp :: (Pretty a) => a -> String
-- pp = render . runLFreshM . ppr
