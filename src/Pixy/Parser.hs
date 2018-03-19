module Pixy.Parser where

import Control.Applicative ((<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Expr as P
import qualified Text.Megaparsec.Char.Lexer as L

import Pixy.Lexer
import Pixy.Syntax

function :: Parser Function
function = L.nonIndented scn (Function <$> identifier <*> (parens (P.sepBy identifier comma)) <*> (equals *> expr))

expr' :: Parser Expr
expr' = P.choice
    [ P.try $ App <$> identifier <*> (parens (P.sepBy expr comma))
    , Var <$> identifier
    , reserved "nil" *> pure Nil
    , Const . fromIntegral <$> integer
    , If <$> (reserved "if" *> expr) <*> (reserved "then" *> expr) <*> (reserved "else" *> expr)
    , parens expr
    ] >>= _where

_where :: Expr -> Parser Expr
_where e = P.try p <|> return e
    where p = L.indentBlock scn $ do
            reserved "where"
            return (L.IndentSome Nothing (return . Where e) ((,) <$> identifier <*> (symbol "=" *> expr)))


operators :: [[P.Operator Parser Expr]]
operators =
    [ [ binary "*" (BinExpr Times)
      , binary "/" (BinExpr Divide) ]
    , [ binary "+" (BinExpr Plus)
      , binary "-" (BinExpr Minus) ]
    , [ binary "==" (BinExpr Equals) ]
    , [ binary "fby" Fby ]
    , [ prefix "?" Exists ]
    ]
    where
        binary name f = P.InfixL (f <$ symbol name)
        prefix name f = P.Prefix (f <$ symbol name)

expr :: Parser Expr
expr = P.makeExprParser expr' operators

program :: Parser [Function]
program = P.many function

runParser :: Parser a -> String -> String -> Either String a
runParser p f s = case P.runParser p f s of
    Left err -> Left (P.parseErrorPretty err)
    Right a -> Right a