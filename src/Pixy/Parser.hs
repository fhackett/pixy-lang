module Pixy.Parser where

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Expr as P
import qualified Text.Megaparsec.Char.Lexer as L

import Pixy.Lexer
import Pixy.Syntax

function :: Parser Function
function = L.nonIndented scn (Function <$> identifier <*> (parens (P.sepBy identifier comma)) <*> expr)

expr' :: Parser Expr
expr' = P.choice
    [ P.try $ App <$> identifier <*> (parens (P.sepBy expr comma))
    , Var <$> identifier
    , reserved "nil" *> pure Nil
    , Const . fromIntegral <$> integer
    , If <$> (reserved "if" *> expr) <*> (reserved "then" *> expr) <*> (reserved "else" *> expr)
    , parens expr
    ]
    -- where
        -- tuple :: Parser [Expr]
        -- tuple = parens (sepBy)
    -- where
    --     args :: Parser [(Var,Expr)]
    --     args = undefined 

operators :: [[P.Operator Parser Expr]]
operators =
    [ [ binary "+" (BinExpr Plus)
      , binary "-" (BinExpr Minus) ]
    , [ binary "*" (BinExpr Times)
      , binary "/" (BinExpr Divide) ]
    , [ binary "==" (BinExpr Equals) ]
    , [ binary "fby" Fby ]
    , [ P.Postfix _where ]
    ]
    where
        binary name f = P.InfixL (f <$ symbol name)
        _where :: Parser (Expr -> Expr)
        _where = L.indentBlock scn p
            where
                p = do
                    reserved "where"
                    return (L.IndentSome Nothing (return . (flip Where)) ((,) <$> identifier <*> (symbol "=" *> expr)))

expr :: Parser Expr
expr = P.makeExprParser expr' operators

runParser :: Parser a -> String -> String -> Either String a
runParser p f s = case P.runParser p f s of
    Left err -> Left (P.parseErrorPretty err)
    Right a -> Right a