{-# LANGUAGE OverloadedStrings #-}

module Mental.Lexer where

import           Protolude                  hiding (check, try)

import           Control.Monad              (fail)

import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Mental.Name

type Parser = Parsec Void Text

reservedWords :: [Text]
reservedWords =
  [ "if", "then", "else", "let", "letrec", "in", "case", "of", "as", "fix"
  , "True", "False", "succ", "pred", "iszero" , "fst", "snd", "inl", "inr"
  , "Nat", "Bool", "module", "type", "data", "where"
  , "#intPlus", "#intMinus", "#intMul", "#intDiv", "#intEq", "#intLess", "#intNeg"
  ]

scn, sc :: Parser ()
scn       = L.space (void space1) (L.skipLineComment "--") empty
sc        = L.space (void (oneOf ("\t " :: [Char]))) (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Integer
integer = lexeme L.decimal

symbol :: Text -> Parser Text
symbol = L.symbol sc

lambda, equal, arrow, fatArrow, unit :: Parser ()
lambda   = void $ symbol "\\"
equal    = void $ symbol "= "
arrow    = void $ symbol "->"
fatArrow = void $ symbol "=>"
unit     = void $ symbol "(" >> symbol ")"

parens, braces, brackets :: Parser a -> Parser a
parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
brackets  = between (symbol "[") (symbol "]")

semicolon, comma, colon, dot, plus :: Parser ()
semicolon = void $ symbol ";"
comma     = void $ symbol ","
colon     = void $ symbol ":"
dot       = void $ symbol "."
plus      = void $ symbol "+"

reserved :: Text -> Parser ()
reserved w = text' w *> notFollowedBy alphaNumChar *> sc

prim :: Text -> Parser ()
prim w = reserved ("#" <> w)

text :: Text -> Parser Text
text = string

text' :: Text -> Parser ()
text' = void . text

-- FIXME
identifier :: Parser VarName
identifier = mkName <$> identifier'

identifier' :: Parser Text
identifier' = (lexeme . try) (p >>= check)
  where
    p       = T.pack <$> ((:) <$> letterChar <*> many alphaNumChar)
    check x = if x `elem` reservedWords
                 then fail $ "keyword " <> show x <> " cannot be an identifier"
                 else pure x

nonIndented :: Parser a -> Parser a
nonIndented = L.nonIndented scn

indentBlock :: Parser (L.IndentOpt Parser a b) -> Parser a
indentBlock = L.indentBlock scn

