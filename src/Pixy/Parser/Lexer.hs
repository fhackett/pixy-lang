{-# LANGUAGE OverloadedStrings #-}
module Pixy.Parser.Lexer
    ( Parser
    -- , scn
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

import qualified Data.Text as T
import Data.Text (Text)

type Parser = P.Parsec Void Text

lineCmt :: Parser ()
lineCmt = L.skipLineComment "--"

blockCmt :: Parser ()
blockCmt = L.skipBlockComment "{-" "-}"

-- scn :: Parser ()
-- scn = L.space P.space1 lineCmt blockCmt

sc :: Parser ()
sc = L.space P.space1 lineCmt blockCmt

-- sc :: Parser ()
-- sc = L.space (void $ P.takeWhile1P Nothing f) lineCmt blockCmt
--     where
--         f x = x == ' ' || x == '\t'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

symbol' :: Text -> Parser ()
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

reserved :: Text -> Parser ()
reserved s = lexeme (P.string s *> P.notFollowedBy P.alphaNumChar)

identifier :: Parser Text
identifier = (lexeme . P.try) (ident >>= checkReserved)
    where
        ident = (T.cons) <$> P.lowerChar <*> (T.pack <$> P.many (P.alphaNumChar <|> P.char '\''))

checkReserved :: Text -> Parser Text
checkReserved i = if i `elem` reservedWords
                    then fail $ "reserved word " ++ (T.unpack i) ++ " is not a valid identifier"
                    else return i

reservedWords :: [Text]
reservedWords = 
    [ "nil"
    , "fby"
    , "next"
    , "where"
    , "if"
    , "then"
    , "else"
    , "true"
    , "false"
    , "trace"
    ]

