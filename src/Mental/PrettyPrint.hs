{-# LANGUAGE OverloadedStrings #-}

module Mental.PrettyPrint
  ( prettyTree
  , prettyTy
  , prettyPrim
  , prettyDecl
  , prettyModule
  , prettyEvalError
  , prettyTypeError
  ) where

import           Protolude hiding (empty, (<>), (<$>), TypeError, First)

import qualified Data.Text.Lazy as T
import           Text.PrettyPrint.Leijen.Text.Monadic
import           Unbound.Generics.LocallyNameless (Name, name2String, unbind)
import           Unbound.Generics.LocallyNameless.Fresh (FreshM, runFreshM)

import           Mental.Decl
import           Mental.Tree
import           Mental.Type
import           Mental.Primitive
import           Mental.Error

bullet :: Applicative m => m Doc -> m Doc
bullet = (text "-" <+>)

errorDoc :: Applicative m => m Doc -> m Doc
errorDoc err = nest 4 (text "Error:" <$> bullet err)

prettyEvalError :: EvalError -> Doc
prettyEvalError err = runFreshM (nest 4 (errorDoc (pp err)))
  where
    pp (NoRuleApplies t) = "Cannot further reduce term:" <+> ppTree t
    pp (VarNotInScope n) = "Variable not in scope:" <+> ppName n

prettyTypeError :: TypeError -> Doc
prettyTypeError err = runFreshM (errorDoc (pp err))
  where
    pp (InfiniteTypeError s t) = "Cannot unify the infinite type:" <+> ppTy s <+> " = " <+> ppTy t
    pp (UnificationError s t)  = "Cannot unify:" <+> ppTy s <+> "with" <+> ppTy t
    pp (ValueNotFound n)       = "Value not found:" <+> ppName n

prettyModule :: Module -> Doc
prettyModule = runFreshM . ppModule

ppModule :: Module -> FreshM Doc
ppModule (Module name decls) =
  "module" <+> ppName name <+> "where" <> line <> line <> decls'
    where
      decls' = vcat (traverse pp decls)
      pp decl = ppDecl decl <> line

prettyDecl :: Decl -> Doc
prettyDecl = runFreshM . ppDecl

ppDecl :: Decl -> FreshM Doc
ppDecl (FunDecl name (Just ty) bnd) =
  ppName name <+> ":" <+> ppTy ty
    <$$> ppDecl (FunDecl name Nothing bnd)

ppDecl (FunDecl _ Nothing bnd) = do
  (name, body) <- unbind bnd
  ppName name <+> "=" <+> ppTree body

ppDecl (TyDecl _ bnd) = do
  (name, ty) <- unbind bnd
  "type" <+> ppName name <+> ppTy ty

prettyPrim :: Primitive -> Doc
prettyPrim = runFreshM . ppPrim

ppPrim :: Primitive -> FreshM Doc
ppPrim Succ   = "succ"
ppPrim Pred   = "pred"
ppPrim IsZero = "iszero"
ppPrim First  = "fst"
ppPrim Second = "snd"
ppPrim Fix    = "fix"

prettyTree :: Tree -> Doc
prettyTree = runFreshM . ppTree

ppName :: Name a -> FreshM Doc
ppName = text . T.pack . name2String

ppBind :: Maybe Ty -> FreshM Doc
ppBind Nothing   = empty
ppBind (Just tp) = ":" <+> ppTy tp

prettyTy :: Ty -> Doc
prettyTy = runFreshM . ppTy

ppTy :: Ty -> FreshM Doc
ppTy TyNat                   = "Nat"
ppTy TyBool                  = "Bool"
ppTy (TyVar n)               = ppName n
ppTy (TyFun a@(TyFun _ _) b) = parens (ppTy a) <+> "->" <+> ppTy b
ppTy (TyFun a b)             = ppTy a <+> "->" <+> ppTy b
ppTy (TyPair a b)            = parens (ppTy a <> "," <+> ppTy b)
ppTy (TySum a b)             = parens (ppTy a <+> "+" <+> ppTy b)

ppTree :: Tree -> FreshM Doc
ppTree Tru              = "True"
ppTree Fals             = "False"
ppTree Zero             = "0"
ppTree n@IsNumericValue = ppNat n
ppTree (Var n)          = ppName n
ppTree (Prim prim)      = ppPrim prim
ppTree (Pair a b)       = parens (ppTree a <> comma <+> ppTree b)
ppTree (Inl t as)       = "inl" <+> ppTree t <+> "as" <+> ppTy as
ppTree (Inr t as)       = "inr" <+> ppTree t <+> "as" <+> ppTy as

ppTree (Case t inl inr) = do
  (l, l') <- unbind inl
  (r, r') <- unbind inr
  let lDoc = "inl" <+> ppName l <+> "=>" <+> ppTree l'
  let rDoc = "inr" <+> ppName r <+> "=>" <+> ppTree r'
  "case" <+> ppTree t <+> "of" <$$> nest 2 (lDoc <$$> rDoc)

ppTree (If cnd thn els) =
  "if" <+> ppTree cnd
       <+> "then" <+> ppTree thn
       <+> "else" <+> ppTree els

ppTree (Abs tp bnd) = do
  (x, body) <- unbind bnd
  text "\\" <> ppName x <> ppBind tp <+> text "->" <+> ppTree body

ppTree (Let tp val bnd) = do
  (x, bdy) <- unbind bnd
  case (val, bdy) of
    (Abs _ val', PrimApp Fix body) -> do
      (_, val'') <- unbind val'
      text "letrec" <+> z x val'' body
    (_, body) ->
      text "let" <+> z x val body
  where
    z x value body =
      ppName x <+> ppBind tp <+> text "=" <+> ppTree value
        <+> text "in" <+> ppTree body

ppTree (App f x@(App _ _)) =
  ppTree f <+> parens (ppTree x)

ppTree (App f x) =
  ppTree f <+> ppTree x

ppNat :: Applicative m => Tree -> m Doc
ppNat = text . T.pack . show . natToInt
  where
    natToInt :: Tree -> Int
    natToInt Zero             = 0
    natToInt (PrimApp Succ n) = natToInt n + 1
    natToInt (PrimApp Pred n) = natToInt n - 1
    natToInt _                = -1

