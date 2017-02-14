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
import           Text.PrettyPrint.Leijen.Text

import           Mental.Decl
import           Mental.Error
import           Mental.Name
import           Mental.Primitive
import           Mental.Tree
import           Mental.Type

bullet :: Doc -> Doc
bullet = (text "-" <+>)

errorDoc :: Doc -> Doc
errorDoc err = nest 4 (text "Error:" <$> bullet err)

prettyEvalError :: EvalError -> Doc
prettyEvalError err = nest 4 (errorDoc (pp err))
  where
    pp (NoRuleApplies t) = "Cannot further reduce term:" <+> ppTree t
    pp (VarNotInScope n) = "Variable not in scope:" <+> ppName n

prettyTypeError :: TypeError -> Doc
prettyTypeError err = errorDoc (pp err)
  where
    pp (InfiniteTypeError s t) = "Cannot unify the infinite type:" <+> ppTy s <+> " = " <+> ppTy t
    pp (UnificationError s t)  = "Cannot unify:" <+> ppTy s <+> "with" <+> ppTy t
    pp (ValueNotFound n)       = "Value not found:" <+> ppName n

prettyModule :: Module -> Doc
prettyModule = ppModule

ppModule :: Module -> Doc
ppModule (Module name decls) =
  "module" <+> ppName name <+> "where" <> line <> line <> decls'
    where
      decls' = vcat (fmap pp decls)
      pp decl = ppDecl decl <> line

prettyDecl :: Decl -> Doc
prettyDecl = ppDecl

ppDecl :: Decl -> Doc
ppDecl (FunDecl name (Just ty) body) =
  ppName name <+> ":" <+> ppTy ty
    <$$> ppDecl (FunDecl name Nothing body)

ppDecl (FunDecl name Nothing body) =
  ppName name <+> "=" <+> ppTree body

ppDecl (TyDecl name ty) =
  "type" <+> ppName name <+> ppTy ty

prettyPrim :: Primitive -> Doc
prettyPrim = ppPrim

ppPrim :: Primitive -> Doc
ppPrim Succ   = "succ"
ppPrim Pred   = "pred"
ppPrim IsZero = "iszero"
ppPrim First  = "fst"
ppPrim Second = "snd"
ppPrim Fix    = "fix"

prettyTree :: Tree -> Doc
prettyTree = ppTree

ppName :: VarName -> Doc
ppName = text . show . nameText

ppBind :: Maybe Ty -> Doc
ppBind Nothing   = empty
ppBind (Just tp) = ":" <+> ppTy tp

prettyTy :: Ty -> Doc
prettyTy = ppTy

ppTy :: Ty -> Doc
ppTy TyNat                   = "Nat"
ppTy TyBool                  = "Bool"
ppTy (TyVar n)               = ppName n
ppTy (TyFun a@(TyFun _ _) b) = parens (ppTy a) <+> "->" <+> ppTy b
ppTy (TyFun a b)             = ppTy a <+> "->" <+> ppTy b
ppTy (TyPair a b)            = parens (ppTy a <> "," <+> ppTy b)
ppTy (TySum a b)             = parens (ppTy a <+> "+" <+> ppTy b)

ppTree :: Tree -> Doc
ppTree Tru              = "True"
ppTree Fals             = "False"
ppTree Zero             = "0"
ppTree n@IsNumericValue = ppNat n
ppTree (Var n)          = ppName n
ppTree (Prim prim)      = ppPrim prim
ppTree (Pair a b)       = parens (ppTree a <> comma <+> ppTree b)
ppTree (Inl t as)       = "inl" <+> ppTree t <+> "as" <+> ppTy as
ppTree (Inr t as)       = "inr" <+> ppTree t <+> "as" <+> ppTy as

ppTree (Case t l l' r r') =
  let lDoc = "inl" <+> ppName l <+> "=>" <+> ppTree l'
      rDoc = "inr" <+> ppName r <+> "=>" <+> ppTree r'
   in "case" <+> ppTree t <+> "of" <$$> nest 2 (lDoc <$$> rDoc)

ppTree (If cnd thn els) =
  "if" <+> ppTree cnd
       <+> "then" <+> ppTree thn
       <+> "else" <+> ppTree els

ppTree (Abs tp x body) =
  text "\\" <> ppName x <> ppBind tp <+> text "->" <+> ppTree body

ppTree (Let tp val x bdy) =
  case (val, bdy) of
    (Abs _ _ val', PrimApp Fix body) -> text "letrec" <+> z x val' body
    (val', body)                     -> text "let"    <+> z x val' body
  where
    z x value body =
      ppName x <+> ppBind tp <+> text "=" <+> ppTree value
        <+> text "in" <+> ppTree body

ppTree (App f x@(App _ _)) =
  ppTree f <+> parens (ppTree x)

ppTree (App f x) =
  ppTree f <+> ppTree x

ppNat :: Tree -> Doc
ppNat = text . T.pack . show . natToInt
  where
    natToInt :: Tree -> Int
    natToInt Zero             = 0
    natToInt (PrimApp Succ n) = natToInt n + 1
    natToInt (PrimApp Pred n) = natToInt n - 1
    natToInt _                = -1

