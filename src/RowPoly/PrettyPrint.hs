{-# LANGUAGE OverloadedStrings #-}

module RowPoly.PrettyPrint
  ( prettyTree
  , prettyType
  , prettyEvalError
  , prettyTypeError
  ) where

import           Protolude hiding (empty, (<>), (<$>), TypeError, First)

import qualified Data.Text.Lazy as T
import           Text.PrettyPrint.Leijen.Text.Monadic
import           Unbound.Generics.LocallyNameless (Name, name2String, unbind)
import           Unbound.Generics.LocallyNameless.Fresh (FreshM, runFreshM)

import           RowPoly.Tree
import           RowPoly.Type
import           RowPoly.Eval (EvalError(..))
import           RowPoly.Infer (TypeError(..))

ppNat :: Applicative m => Tree -> m Doc
ppNat = text . T.pack . show . natToInt
  where
    natToInt :: Tree -> Int
    natToInt Zero     = 0
    natToInt (Succ n) = natToInt n + 1
    natToInt (Pred n) = natToInt n - 1
    natToInt _        = -1

bullet :: Applicative m => m Doc -> m Doc
bullet = (text "-" <+>)

errorDoc :: Applicative m => m Doc -> m Doc
errorDoc err = nest 4 (text "Error:" <$> bullet err)

prettyEvalError :: EvalError -> Doc
prettyEvalError err = runFreshM (nest 4 (errorDoc (pp err)))
  where
    pp (NoRuleApplies t) = "Cannot further reduce term:" <+> pprint t
    pp (VarNotInScope n) = "Variable not in scope:" <+> ppName n

prettyTypeError :: TypeError -> Doc
prettyTypeError err = runFreshM (errorDoc (pp err))
  where
    pp (InfiniteTypeError s t) = "Cannot unify the infinite type:" <+> ppType s <+> " = " <+> ppType t
    pp (UnificationError s t)  = "Cannot unify:" <+> ppType s <+> "with" <+> ppType t
    pp (ValueNotFound n)       = "Value not found:" <+> ppName n

prettyTree :: Tree -> Doc
prettyTree = runFreshM . pprint

ppName :: Name a -> FreshM Doc
ppName = text . T.pack . name2String

ppBind :: Maybe Ty -> FreshM Doc
ppBind Nothing   = empty
ppBind (Just tp) = ":" <+> ppType tp

prettyType :: Ty -> Doc
prettyType = runFreshM . ppType

ppType :: Ty -> FreshM Doc
ppType TyNat                   = "Nat"
ppType TyBool                  = "Bool"
ppType (TyVar n)               = ppName n
ppType (TyFun a@(TyFun _ _) b) = parens (ppType a) <+> "->" <+> ppType b
ppType (TyFun a b)             = ppType a <+> "->" <+> ppType b
ppType (TyTuple a b)           = parens (ppType a <> "," <+> ppType b)
ppType (TySum a b)             = parens (ppType a <+> "+" <+> ppType b)

pprint :: Tree -> FreshM Doc
pprint Tru              = "True"
pprint Fals             = "False"
pprint Zero             = "0"
pprint n@IsNumericValue = ppNat n
pprint (Succ t)         = "succ" <+> pprint t
pprint (Pred t)         = "pred" <+> pprint t
pprint (IsZero t)       = "iszero" <+> pprint t
pprint (Var n)          = ppName n
pprint (Tuple a b)      = parens (pprint a <> comma <+> pprint b)
pprint (First t)        = "fst" <+> pprint t
pprint (Second t)       = "second" <+> pprint t
pprint (Inl t as)       = "inl" <+> pprint t <+> "as" <+> ppType as
pprint (Inr t as)       = "inr" <+> pprint t <+> "as" <+> ppType as
pprint (Case t inl inr) = do
  (l, l') <- unbind inl
  (r, r') <- unbind inr
  let lDoc = "inl" <+> ppName l <+> "=>" <+> pprint l'
  let rDoc = "inr" <+> ppName r <+> "=>" <+> pprint r'
  "case" <+> pprint t <+> "of" <$$> nest 2 (lDoc <$$> rDoc)

pprint (If cnd thn els) =
  "if" <+> pprint cnd
       <+> "then" <+> pprint thn
       <+> "else" <+> pprint els

pprint (Abs tp bnd) = do
  (x, body) <- unbind bnd
  parens (
    text "\\" <>
    ppName x <>
    ppBind tp <>
    text "." <+>
    pprint body)

pprint (Let tp val bnd) = do
  (x, body) <- unbind bnd
  text "let" <+>
    ppName x <+>
    ppBind tp <+>
    text "=" <+>
    pprint val <+>
    text "in" <+>
    pprint body

pprint (App f x@(App _ _)) =
  pprint f <+> parens (pprint x)

pprint (App f x) =
  pprint f <+> pprint x

