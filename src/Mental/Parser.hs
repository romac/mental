{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Mental.Parser
  ( parser
  ) where

import           Protolude hiding (try)

import           Data.Foldable (foldl')

import           Text.Megaparsec
import           Text.Megaparsec.Text
import           Text.Megaparsec.Lexer (IndentOpt(..))

import           Unbound.Generics.LocallyNameless

import           Mental.Tree
import           Mental.Type
import           Mental.Lexer

parser :: Parser Tree
parser = between sc eof (nonIndented term)

term :: Parser Tree
term = do
  t : ts <- some factor
  pure (foldl' App t ts)

factor :: Parser Tree
factor =  reserved "True"  *> pure Tru
    <|> reserved "False" *> pure Fals
    <|> pNat
    <|> reserved "succ"   *> (Succ   <$> term)
    <|> reserved "pred"   *> (Pred   <$> term)
    <|> reserved "iszero" *> (IsZero <$> term)
    <|> Var <$> identifier
    <|> pIf
    <|> pAbs
    <|> pLet
    <|> pTuple
    <|> pSum "inl"
    <|> pSum "inr"
    <|> pCaseOf
    <|> parens term

pIf :: Parser Tree
pIf = do
  reserved "if"
  cond <- term
  reserved "then"
  thn <- term
  reserved "else"
  els <- term
  pure $ If cond thn els

pLet :: Parser Tree
pLet = do
  reserved "let"
  name <- identifier
  tp <- optional (colon *> pTy)
  equal
  val <- term
  reserved "in"
  body <- term
  pure $ Let tp val (bind name body)

pTuple :: Parser Tree
pTuple = parens $ do
  a <- term
  comma
  b <- term
  pure $ Tuple a b

pSum :: Text -> Parser Tree
pSum d = do
  reserved d
  val <- term
  reserved "as"
  ty <- pTy
  pure $ Inl val ty

pCase :: Text -> Parser (Bind VarName Tree)
pCase d = do
  reserved d
  name <- identifier
  fatArrow
  body <- term
  pure $ bind name body

pCaseOf :: Parser Tree
pCaseOf = do
  reserved "case"
  val <- term
  reserved "of"
  inl:_ <- indentBlock (pure (IndentSome Nothing (pure) (pCase "inl")))
  inr:_ <- indentBlock (pure (IndentSome Nothing (pure) (pCase "inr")))
  pure $ Case val inl inr

pAbs :: Parser Tree
pAbs = do
  lambda
  name <- identifier
  tp <- optional (colon *> pTy)
  dot
  body <- term
  pure $ Abs tp (bind name body)

pNat :: Parser Tree
pNat = do
  n <- integer
  pure (selfIter n Succ Zero)

selfIter :: (Eq n, Num n) => n -> (a -> a) -> a -> a
selfIter 0 _ !x = x
selfIter !n f !x = selfIter (n - 1) f (f x)

pTy :: Parser Ty
pTy = do
  a <- pSimpleTy
  b <- optional (arrow *> pTy)
  pure $ case b of
    Nothing -> a
    Just b' -> TyFun a b'

pSimpleTy :: Parser Ty
pSimpleTy =  try pTupleTy <|> try pSumTy <|> pBaseTy

pBaseTy :: Parser Ty
pBaseTy =    reserved "Nat"  *> pure TyNat
         <|> reserved "Bool" *> pure TyBool
         <|> parens pTy

pTupleTy :: Parser Ty
pTupleTy = parens $ do
  a <- pTy
  comma
  b <- pTy
  pure $ TyTuple a b

pSumTy :: Parser Ty
pSumTy = do
  a <- pBaseTy
  plus
  b <- pSimpleTy
  pure $ TySum a b

