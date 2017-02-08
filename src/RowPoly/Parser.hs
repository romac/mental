{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module RowPoly.Parser where

import           Control.Monad (void)
import           Data.Monoid ((<>))
import           Data.Maybe (fromMaybe)
import           Data.Foldable (foldl')

import           Data.Text (Text)
import qualified Data.Text as T

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Text

import           Unbound.Generics.LocallyNameless

import RowPoly.Tree
import RowPoly.Type
import RowPoly.Lexer

parser :: Parser Tree
parser = between sc eof term

term :: Parser Tree
term = do
  t : ts <- some factor
  return (foldl' App t ts)

factor :: Parser Tree
factor =  reserved "True"  *> pure Tru
    <|> reserved "False" *> pure Fals
    <|> pNat
    <|> reserved "succ"   *> (Succ   <$> term)
    <|> reserved "pred"   *> (Pred   <$> term)
    <|> reserved "iszero" *> (IsZero <$> term)
    <|> (Var . s2n) <$> identifier
    <|> pIf
    <|> pAbs
    <|> pLet
    <|> parens term

pIf :: Parser Tree
pIf = do
  reserved "if"
  cond <- term
  reserved "then"
  thn <- term
  reserved "else"
  els <- term
  return $ If cond thn els

pLet :: Parser Tree
pLet = do
  reserved "let"
  name <- identifier
  tp <- optional (colon *> pType)
  equal
  val <- term
  reserved "in"
  body <- term
  return $ Let tp val (bind (s2n name) body)

pAbs :: Parser Tree
pAbs = do
  lambda
  name <- identifier
  tp <- optional (colon *> pType)
  dot
  body <- term
  return $ Abs tp (bind (s2n name) body)

pNat :: Parser Tree
pNat = do
  n <- integer
  return (selfIter n Succ Zero)

selfIter :: (Eq n, Num n) => n -> (a -> a) -> a -> a
selfIter 0 _ !x = x
selfIter !n f !x = selfIter (n - 1) f (f x)

pType :: Parser Type
pType = do
  a <- pType
  b <- optional (arrow *> pType)
  return $ case b of
    Nothing -> a
    Just b' -> TyFun a b'

pBaseType :: Parser Type
pBaseType =  reserved "Nat"  *> pure TyNat
         <|> reserved "Bool" *> pure TyBool
         <|> parens pType

