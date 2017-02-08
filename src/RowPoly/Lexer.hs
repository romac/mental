{-# LANGUAGE OverloadedStrings #-}

module RowPoly.Lexer where

import           Protolude hiding (try, check)

import           Control.Monad (fail)

import           Data.Text (Text)
import qualified Data.Text as T

import           Text.Megaparsec
import           Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L

scn, sc :: Parser ()
scn       = L.space (void spaceChar) (L.skipLineComment "--") empty
sc        = L.space (void (oneOf ("\t " :: [Char]))) (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme    = L.lexeme sc

integer :: Parser Integer
integer   = lexeme L.integer

symbol :: Text -> Parser Text
symbol s = T.pack <$> L.symbol sc (T.unpack s)

lambda, equal, arrow :: Parser ()
lambda = void $ symbol "\\"
equal  = void $ symbol "="
arrow  = void $ symbol "->"

parens, braces, brackets :: Parser a -> Parser a
parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
brackets  = between (symbol "[") (symbol "]")

semicolon, comma, colon, dot :: Parser ()
semicolon = void $ symbol ";"
comma     = void $ symbol ","
colon     = void $ symbol ":"
dot       = void $ symbol "."

reservedWords :: [Text]
reservedWords =
  [ "if", "then", "else", "let", "in"
  , "True", "False"
  , "succ", "pred"
  , "Nat", "Bool"
  ]

reserved :: Text -> Parser ()
reserved w = string (T.unpack w) *> notFollowedBy alphaNumChar *> sc

identifier :: Parser Text
identifier = (lexeme . try) (p >>= check)
  where
    p       = T.pack <$> ((:) <$> letterChar <*> many alphaNumChar)
    check x = if x `elem` reservedWords
                 then fail $ "keyword " <> show x <> " cannot be an identifier"
                 else return x

nonIndented :: Parser a -> Parser a
nonIndented = L.nonIndented scn

indentBlock :: Parser (L.IndentOpt Parser a b) -> Parser a
indentBlock = L.indentBlock scn

