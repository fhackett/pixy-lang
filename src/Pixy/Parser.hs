module Pixy.Parser
    ( program
    , expr
    , runParser
    )
where

import Control.Applicative ((<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Expr as P
import qualified Text.Megaparsec.Char.Lexer as L

import Pixy.Lexer
import Pixy.Syntax

function :: Parser Function
function = L.nonIndented scn (Function <$> identifier <*> (parens (P.sepBy identifier comma)) <*> (symbol "=" *> expr))

-- variable :: Parser Var
-- variable = string2Name <$> identifier

value :: Parser Value
value = P.choice
    [ (VBool True) <$ reserved "true" 
    , (VBool False) <$ reserved "false"
    , VNil <$ reserved "nil"
    , (VInt . fromIntegral) <$> integer
    ]

term :: Parser Expr
term = P.choice
    [ P.try $ App <$> identifier <*> (parens (P.sepBy expr comma))
    , Var <$> identifier
    , Const <$> value
    , If <$> (reserved "if" *> expr) <*> (reserved "then" *> expr) <*> (reserved "else" *> expr)
    , Next <$> (reserved "next" *> expr)
    , parens expr
    ]

operators :: [[P.Operator Parser Expr]]
operators =
    [ [ binary "*" (Binop Times)
      , binary "/" (Binop Divide) ]
    , [ binary "+" (Binop Plus)
      , binary "-" (Binop Minus) ]
    , [ binary "==" (Binop Equals) ]
    , [ binary "fby" Fby ]
    , [ prefix "?" Check ]
    -- , [ P.Postfix _where ]
    ]
    where
        binary name f = P.InfixL (f <$ symbol name)
        prefix name f = P.Prefix (f <$ symbol name)

expr' :: Parser Expr
expr' = P.makeExprParser term operators

expr :: Parser Expr
expr = expr' >>= (\e -> P.try (_where e) <|> return e)
    where
        _where e = L.indentBlock scn $ do
            reserved "where"
            return (L.IndentSome Nothing (return . Where e) ((,) <$> identifier <*> (symbol "=" *> expr)))

program :: Parser [Function]
program = P.many function

runParser :: Parser a -> String -> String -> Either String a
runParser p f s = case P.runParser p f s of
    Left err -> Left (P.parseErrorPretty err)
    Right a -> Right a