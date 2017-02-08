{-# LANGUAGE OverloadedStrings #-}

module RowPoly.Lexer where

import           Control.Applicative (empty)
import           Control.Monad (void)
import           Data.Monoid ((<>))

import           Data.Text (Text)
import qualified Data.Text as T

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L

import RowPoly.Tree
import RowPoly.Type

scn, sc :: Parser ()
scn       = L.space (void spaceChar) (L.skipLineComment "--") empty
sc        = L.space (void (oneOf ("\t " :: String))) (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme    = L.lexeme sc

integer :: Parser Integer
integer   = lexeme L.integer

symbol :: String -> Parser Text
symbol s = T.pack <$> L.symbol sc s

lambda, equal :: Parser ()
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

reservedWords :: [String]
reservedWords =
  [ "if", "then", "else", "let", "in"
  , "True", "False"
  , "succ", "pred"
  , "Nat", "Bool"
  ]

reserved :: String -> Parser ()
reserved w = string w *> notFollowedBy alphaNumChar *> sc

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` reservedWords
                 then fail $ "keyword " <> show x <> " cannot be an identifier"
                 else return x

nonIndented :: Parser a -> Parser a
nonIndented = L.nonIndented scn

indentBlock :: Parser (L.IndentOpt Parser a b) -> Parser a
indentBlock = L.indentBlock scn


