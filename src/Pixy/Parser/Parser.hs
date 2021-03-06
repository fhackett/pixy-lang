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

import Pixy.Parser.Lexer
import Pixy.Syntax

function :: Parser Function
function = Function <$> identifier <*> (parens (P.sepBy identifier comma)) <*> (symbol "=" *> expr)
-- function = L.nonIndented scn (Function <$> identifier <*> (parens (P.sepBy identifier comma)) <*> (symbol "=" *> expr))

value :: Parser Value
value = P.choice
    [ (VBool True) <$ reserved "true"
    , (VBool False) <$ reserved "false"
    , VNil <$ reserved "nil"
    , (VInt . fromIntegral) <$> integer
    ] <?> "value"

term :: Parser Expr
term = P.choice
    [ P.try $ App <$> identifier <*> (parens (P.sepBy expr comma))
    , Var <$> identifier
    , Const <$> value
    , If <$> (reserved "if" *> expr) <*> (reserved "then" *> expr) <*> (reserved "else" *> expr)
    , Next <$> (reserved "next" *> expr)
    , Unary Trace <$> (reserved "trace" *> expr)
    , parens expr
    ] <?> "expression"

operators :: [[P.Operator Parser Expr]]
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
        _where = flip Where <$> (reserved "where" *> brackets (P.some ((,) <$> identifier <*> (symbol "=" *> expr))))
        -- _where = L.indentBlock scn $ do
        --     _ <- symbol "where"
        --     return (L.IndentSome Nothing (return . flip Where) ((,) <$> identifier <*> (symbol "=" *> expr)))

expr :: Parser Expr
expr = P.makeExprParser term operators

-- expr :: Parser Expr
-- expr = expr' >>= (\e -> _where e <|> return e)
--     where
--         _where e = L.indentBlock scn $ do
--             reserved "where"
--             return (L.IndentSome Nothing (return . Where e) ((,) <$> identifier <*> (symbol "=" *> expr)))

program :: Parser [Function]
program = P.many function

runParser :: Parser a -> String -> String -> Either String a
runParser p f s = case P.runParser p f s of
    Left err -> Left (P.parseErrorPretty err)
    Right a -> Right a