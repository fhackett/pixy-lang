{-# LANGUAGE OverloadedStrings #-}
module Pixy.Parser.Parser
    ( program
    , expr
    , runParser
    )
where

import Control.Applicative ((<|>))
import qualified Text.Megaparsec as P
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec.Expr as P
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map.Strict as Map

import Data.Text (Text)

import Pixy.Parser.Lexer
import Pixy.Syntax
import Pixy.Error

function :: Parser (Function ParsePass)
function = Function <$> identifier <*> (parens (P.sepBy identifier comma)) <*> (symbol "=" *> expr) <*> pure ud
-- function = L.nonIndented scn (Function <$> identifier <*> (parens (P.sepBy identifier comma)) <*> (symbol "=" *> expr))

value :: Parser Value
value = P.choice
    [ (VBool True) <$ reserved "true"
    , (VBool False) <$ reserved "false"
    , VNil <$ reserved "nil"
    , (VInt . fromIntegral) <$> integer
    ] <?> "value"

term :: Parser (Expr ParsePass)
term = P.choice
    [ P.try $ App <$> identifier <*> (parens (P.sepBy expr comma))
    , Var <$> identifier
    , Const <$> value
    , If <$> (reserved "if" *> expr) <*> (reserved "then" *> expr) <*> (reserved "else" *> expr)
    , Next <$> (reserved "next" *> expr)
    , Unary Trace <$> (reserved "trace" *> expr)
    , parens expr
    ] <?> "expression"

operators :: [[P.Operator Parser (Expr ParsePass)]]
operators =
    [ [ binary "*" (Binop Times)
      , binary "/" (Binop Divide) ]
    , [ binary "+" (Binop Plus)
      , binary "-" (Binop Minus) ]
    , [ binary "%" (Binop Modulo) ]
    , [ prefix "?" (Unary Check) ]
    , [ binary "==" (Binop Equals) 
    ,   binary "!=" (Binop NotEquals) 
    ,   binary "<=" (Binop LessThanEquals) 
    ,   binary "<" (Binop LessThan) 
    ,   binary ">=" (Binop GreaterThanEquals) 
    ,   binary ">" (Binop GreaterThan) ]
    , [ binary "||" (Binop Or) 
      , binary "&&" (Binop And) ]
    , [ prefix "!" (Unary Not) ]
    , [ binaryr "fby" Fby ]
    , [ P.Postfix _where ]
    ]
    where
        binary name f = P.InfixL (f <$ symbol name)
        binaryr name f = P.InfixR (f <$ symbol name)
        prefix name f = P.Prefix (f <$ symbol name)
        _where = (flip Where . Map.fromList) <$> (reserved "where" *> brackets (P.some ((,) <$> identifier <*> (symbol "=" *> expr))))

expr :: Parser (Expr ParsePass)
expr = P.makeExprParser term operators

program :: Parser [(Function ParsePass)]
program = P.many function

runParser :: Parser a -> String -> Text -> Either ErrorMessage a
runParser p f s = case P.runParser p f s of
    Left err -> Left (toError $ P.parseErrorPretty err)
    Right a -> Right a