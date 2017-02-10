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
parser = between sc eof (nonIndented pTerm)

pTerm :: Parser Tree
pTerm = do
  t : ts <- some pSimpleTerm
  pure (foldl' App t ts)

pSimpleTerm :: Parser Tree
pSimpleTerm =  reserved "True"  *> pure Tru
    <|> pNat
    <|> reserved "False" *> pure Fals
    <|> reserved "succ"   *> (Succ   <$> pTerm)
    <|> reserved "pred"   *> (Pred   <$> pTerm)
    <|> reserved "iszero" *> (IsZero <$> pTerm)
    <|> Var <$> identifier
    <|> pIf
    <|> pAbs
    <|> pFix
    <|> pLetRec
    <|> pLet
    <|> pPair
    <|> pSum "inl"
    <|> pSum "inr"
    <|> pCaseOf
    <|> parens pTerm

pIf :: Parser Tree
pIf = do
  reserved "if"
  cond <- pTerm
  reserved "then"
  thn <- pTerm
  reserved "else"
  els <- pTerm
  pure $ If cond thn els

pLet :: Parser Tree
pLet = do
  reserved "let"
  name <- identifier
  tp <- optional (colon *> pTy)
  equal
  val <- pTerm
  reserved "in"
  body <- pTerm
  pure $ Let tp val (bind name body)

pLetRec :: Parser Tree
pLetRec = do
  reserved "letrec"
  name <- identifier
  ty <- optional (colon *> pTy)
  equal
  val <- pTerm
  reserved "in"
  body <- pTerm
  let inner = Abs ty (bind name val)
  pure $ Let ty inner (bind name (Fix body))

pFix :: Parser Tree
pFix = reserved "fix" >> Fix <$> pTerm

pPair :: Parser Tree
pPair = parens $ do
  a <- pTerm
  comma
  b <- pTerm
  pure $ Pair a b

pSum :: Text -> Parser Tree
pSum d = do
  reserved d
  val <- pTerm
  reserved "as"
  ty <- pTy
  pure $ Inl val ty

pCase :: Text -> Parser (Bind VarName Tree)
pCase d = do
  reserved d
  name <- identifier
  fatArrow
  body <- pTerm
  pure $ bind name body

pCaseOf :: Parser Tree
pCaseOf = do
  reserved "case"
  val <- pTerm
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
  body <- pTerm
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
pSimpleTy =  try pPairTy <|> try pSumTy <|> pBaseTy

pBaseTy :: Parser Ty
pBaseTy =    reserved "Nat"  *> pure TyNat
         <|> reserved "Bool" *> pure TyBool
         <|> parens pTy

pPairTy :: Parser Ty
pPairTy = parens $ do
  a <- pTy
  comma
  b <- pTy
  pure $ TyPair a b

pSumTy :: Parser Ty
pSumTy = do
  a <- pBaseTy
  plus
  b <- pSimpleTy
  pure $ TySum a b

