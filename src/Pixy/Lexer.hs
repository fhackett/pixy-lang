module Pixy.Lexer
    ( Parser
    , scn
    , symbol
    , integer
    , parens
    , brackets
    , comma
    , semicolon
    , equals
    , identifier
    , reserved
    )
where

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative ((<|>))
import Data.Functor (void)
import Data.Void (Void)

type Parser = P.Parsec Void String

lineCmt :: Parser ()
lineCmt = L.skipLineComment "--"

blockCmt :: Parser ()
blockCmt = L.skipBlockComment "{-" "-}"

scn :: Parser ()
scn = L.space P.space1 lineCmt blockCmt

sc :: Parser ()
sc = L.space (void $ P.takeWhile1P Nothing f) lineCmt blockCmt
    where
        f x = x == ' ' || x == '\t'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

symbol' :: String -> Parser ()
symbol' = void . symbol

integer :: Parser Integer
integer = lexeme L.decimal

parens :: Parser a -> Parser a
parens = P.between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = P.between (symbol "{") (symbol "}")

semicolon :: Parser ()
semicolon = symbol' ";"

comma :: Parser ()
comma = symbol' ","

equals :: Parser ()
equals = symbol' "="

reserved :: String -> Parser ()
reserved s = lexeme (P.string s *> P.notFollowedBy P.alphaNumChar)

identifier :: Parser String
identifier = (lexeme . P.try) (ident >>= checkReserved)
    where
        ident = (:) <$> P.lowerChar <*> (P.many (P.alphaNumChar <|> P.char '\''))

checkReserved :: String -> Parser String
checkReserved i = if i `elem` reservedWords
                    then fail $ "reserved word " ++ i ++ " is not a valid identifier"
                    else return i

reservedWords :: [String]
reservedWords = 
    [ "nil"
    , "fby"
    , "where"
    , "if"
    , "then"
    , "else"
    ]

