{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Mental.Parser where

import           Protolude hiding (try)

import           Data.Foldable (foldl')
import           Control.Comonad.Cofree (Cofree(..))

import           Text.Megaparsec
import           Text.Megaparsec.Text

import           Mental.Decl
import           Mental.Lexer
import           Mental.Name
import           Mental.Primitive
import           Mental.Tree
import           Mental.Tree.Untyped
import           Mental.Type

withPos :: (forall a. Parser a -> Parser (f a))
          -> Parser (Cofree f SourcePos)
withPos f = (:<) <$> getPosition <*> f (withPos f)

withPos' :: (forall a. Parser (f a)) -> Parser (Cofree f SourcePos)
withPos' p = (:<) <$> getPosition <*> p

moduleParser :: Parser Module
moduleParser = between scn eof (nonIndented pModule)

replEntryParser :: Parser (Either Decl UntypedTree)
replEntryParser = between sc eof $ do
  optDecl <- optional pDecl
  case optDecl of
    Just decl -> pure (Left decl)
    Nothing   -> Right <$> pTerm

termParser :: Parser UntypedTree
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
  let ident = mkName name
  pure $ FunDecl ident ty body
  <?> "function declaration"

pTyDecl :: Parser Decl
pTyDecl = do
  reserved "type"
  name <- identifier
  equal
  ty <- pTy
  pure $ TyDecl name ty
  <?> "type alias"

pTerm :: Parser UntypedTree
pTerm = do
  t : ts <- some pSimpleTerm
  pure (foldl' app t ts)
  <?> "term"
    where
      app (fPos :< f) x = fPos :< App (fPos :< f) x

pSimpleTerm :: Parser UntypedTree
pSimpleTerm =
        try pBool
    <|> try pIntLit
    <|> try pVar
    <|> try pIf
    <|> try pAbs
    <|> try pPrim
    <|> try pLetRec
    <|> try pLet
    <|> try pPair
    <|> try (parens pTerm)
    <?> "term"

pVar :: Parser UntypedTree
pVar = do
  pos  <- getPosition
  name <- identifier
  pure $ pos :< Var name
  <?> "variable"

pBool :: Parser UntypedTree
pBool = withPos' p <?> "boolean"
  where p =  reserved "True"  *> pure Tru
         <|> reserved "False" *> pure Fals

pPrim :: Parser UntypedTree
pPrim = withPos' (Prim <$> p) <?> "primitive"
  where p =  prim "fst"      *> pure PFirst
         <|> prim "snd"      *> pure PSecond
         <|> prim "fix"      *> pure PFix
         <|> prim "intPlus"  *> pure PIntPlus
         <|> prim "intMinus" *> pure PIntMinus
         <|> prim "intMul"   *> pure PIntMul
         <|> prim "intDiv"   *> pure PIntDiv
         <|> prim "intEq"    *> pure PIntEq
         <|> prim "intLess"  *> pure PIntLess
         <|> prim "intNeg"   *> pure PIntNeg

pIntLit :: Parser UntypedTree
pIntLit = withPos' (IntLit <$> integer) <?> "integer"

pIf :: Parser UntypedTree
pIf = do
  pos <- getPosition
  reserved "if"
  cond <- pTerm
  reserved "then"
  thn <- pTerm
  reserved "else"
  els <- pTerm
  pure $ pos :< If cond thn els
  <?> "if-then-else"

pLet :: Parser UntypedTree
pLet = do
  pos <- getPosition
  reserved "let"
  name <- identifier
  tp <- optional (colon *> pTy)
  equal
  val <- pTerm
  reserved "in"
  body <- pTerm
  pure $ pos :< Let name tp val body
  <?> "let"

pLetRec :: Parser UntypedTree
pLetRec = do
  pos <- getPosition
  reserved "letrec"
  name <- identifier
  ty <- optional (colon *> pTy)
  equal
  val <- pTerm
  reserved "in"
  body <- pTerm
  let inner = pos :< Abs ty (name, val)
  pure $ pos :< Let name ty inner (pos :< App (pos :< Prim PFix) body)
  <?> "letrec"

pPair :: Parser UntypedTree
pPair = parens p <?> "pair"
  where
    p = do
      pos <- getPosition
      a <- pTerm
      comma
      b <- pTerm
      pure $ pos :< Pair a b

pAbs :: Parser UntypedTree
pAbs = do
  pos <- getPosition
  lambda
  name <- identifier
  tp <- optional (colon *> pTy)
  arrow
  body <- pTerm
  pure $ pos :< Abs tp (name, body)
  <?> "abs"

pTy :: Parser Ty
pTy = do
  a <- pSimpleTy
  b <- optional (arrow *> pTy)
  pure $ case b of
    Nothing -> a
    Just b' -> tyFun a b'
  <?> "type"

pSimpleTy :: Parser Ty
pSimpleTy = try pPairTy <|> pBaseTy

pBaseTy :: Parser Ty
pBaseTy =  reserved "Int"  *> pure tyInt
       <|> reserved "Bool" *> pure tyBool
       <|> (tyVar <$> identifier)
       <|> parens pTy

pPairTy :: Parser Ty
pPairTy = parens $ do
  a <- pTy
  comma
  b <- pTy
  pure $ tyPair a b

