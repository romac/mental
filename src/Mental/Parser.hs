{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- module Mental.Parser
--   ( moduleParser
--   , replEntryParser
--   , termParser
--   ) where
--
module Mental.Parser where

import           Protolude hiding (try)

import           Data.Foldable (foldl')

import           Text.Megaparsec
import           Text.Megaparsec.Text
import           Text.Megaparsec.Lexer (IndentOpt(..))

import           Unbound.Generics.LocallyNameless

import           Mental.Decl
import           Mental.Tree
import           Mental.Type
import           Mental.Primitive
import           Mental.Lexer

moduleParser :: Parser Module
moduleParser = between scn eof (nonIndented pModule)

replEntryParser :: Parser (Either Decl Tree)
replEntryParser = between sc eof $ do
  optDecl <- optional pDecl
  case optDecl of
    Just decl -> pure (Left decl)
    Nothing   -> Right <$> pTerm

termParser :: Parser Tree
termParser = between sc eof (nonIndented pTerm)

pModule :: Parser Module
pModule = do
  reserved "module"
  name <- identifier
  reserved "where"
  decls <- pDecl `sepEndBy` scn
  pure $ Module name decls

pDecl :: Parser Decl
pDecl = try pFunDecl <|> pTyDecl

pFunDecl :: Parser Decl
pFunDecl = do
  nameTy <- (optional . try) $ do
    name <- identifier'
    colon
    ty <- pTy
    scn
    pure (name, ty)

  (name, ty) <- case nameTy of
    Just (name, ty) -> (,) <$> text name <*> pure (Just ty)
    Nothing         -> (,) <$> identifier' <*> pure Nothing

  sc
  equal
  body <- pTerm
  let ident = textToName name
  pure $ FunDecl ident ty (bind ident body)
  <?> "function declaration"

pTyDecl :: Parser Decl
pTyDecl = do
  reserved "type"
  name <- identifier
  equal
  ty <- pTy
  pure $ TyDecl name (bind name ty)
  <?> "type alias"

pTerm :: Parser Tree
pTerm = do
  t : ts <- some pSimpleTerm
  pure (foldl' App t ts)
  <?> "term"

pSimpleTerm :: Parser Tree
pSimpleTerm =
        try pNat
    <|> try pBool
    <|> try pNatOp
    <|> try pVar
    <|> try pIf
    <|> try pAbs
    <|> try pFix
    <|> try pLetRec
    <|> try pLet
    <|> try pPair
    <|> try pSum
    <|> try pCaseOf
    <|> try (parens pTerm)
    <?> "term"

pVar :: Parser Tree
pVar = (Var <$> identifier) <?> "variable"

pBool :: Parser Tree
pBool = reserved "True"  *> pure Tru
    <|> reserved "False" *> pure Fals
    <?> "boolean"

pNatOp :: Parser Tree
pNatOp =  try (reserved "succ"   *> pure (Prim Succ))
      <|> try (reserved "pred"   *> pure (Prim Pred))
      <|> try (reserved "iszero" *> pure (Prim IsZero))

pIf :: Parser Tree
pIf = do
  reserved "if"
  cond <- pTerm
  reserved "then"
  thn <- pTerm
  reserved "else"
  els <- pTerm
  pure $ If cond thn els
  <?> "if-then-else"

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
  <?> "let"

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
  pure $ Let ty inner (bind name (PrimApp Fix body))
  <?> "letrec"

pFix :: Parser Tree
pFix = reserved "fix" *> pure (Prim Fix) <?> "fix"

pPair :: Parser Tree
pPair = parens (do
    a <- pTerm
    comma
    b <- pTerm
    pure $ Pair a b
  ) <?> "pair"

pSum :: Parser Tree
pSum = try (pSum' "inl") <|> pSum' "inr"

pSum' :: Text -> Parser Tree
pSum' d = do
  reserved d
  val <- pTerm
  reserved "as"
  ty <- pTy
  pure $ Inl val ty
  <?> "sum"

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
  <?> "case"

pAbs :: Parser Tree
pAbs = do
  lambda
  name <- identifier
  tp <- optional (colon *> pTy)
  arrow
  body <- pTerm
  pure $ Abs tp (bind name body)
  <?> "abs"

pNat :: Parser Tree
pNat = do
  n <- integer
  pure (iter n (PrimApp Succ) Zero)
  <?> "number"

iter :: (Eq n, Num n) => n -> (a -> a) -> a -> a
iter 0  _ !x = x
iter !n f !x = iter (n - 1) f (f x)

pTy :: Parser Ty
pTy = do
  a <- pSimpleTy
  b <- optional (arrow *> pTy)
  pure $ case b of
    Nothing -> a
    Just b' -> TyFun a b'
  <?> "type"

pSimpleTy :: Parser Ty
pSimpleTy = try pPairTy <|> try pSumTy <|> pBaseTy

pBaseTy :: Parser Ty
pBaseTy =    reserved "Nat"  *> pure TyNat
         <|> reserved "Bool" *> pure TyBool
         <|> (TyVar <$> identifier)
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

