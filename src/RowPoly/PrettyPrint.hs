{-# LANGUAGE OverloadedStrings #-}

module RowPoly.PrettyPrint
  ( prettyPrint
  ) where

import           Protolude hiding (empty, (<>))

import qualified Data.Text.Lazy as T
import           Text.PrettyPrint.Leijen.Text.Monadic
import           Unbound.Generics.LocallyNameless (Name, name2String, unbind)
import           Unbound.Generics.LocallyNameless.Fresh (FreshM, runFreshM)

import           RowPoly.Tree
import           RowPoly.Type

-- ppNat :: Applicative m => Tree -> m Doc
-- ppNat = text . T.pack . show . natToInt
--   where
--     natToInt Zero     = 0
--     natToInt (Succ n) = natToInt n + 1
--     natToInt (Pred n) = natToInt n - 1
--
-- ppMaybe :: Applicative m => Maybe a -> (a -> m Doc) -> m Doc
-- ppMaybe Nothing _   = empty
-- ppMaybe (Just x) pp = pp x

prettyPrint :: Tree -> Doc
prettyPrint = runFreshM . pprint

ppName :: Applicative m => Name a -> m Doc
ppName = text . T.pack . name2String

ppBind :: Applicative m => Maybe Ty -> m Doc
ppBind Nothing   = empty
ppBind (Just tp) = text ":" <+> ppType tp

ppType :: Applicative m => Ty -> m Doc
ppType TyNat       = text "Nat"
ppType TyBool      = text "Bool"
ppType (TyVar n)   = ppName n
ppType (TyFun a b) = ppType a <+> text "->" <+> ppType b

pprint :: Tree -> FreshM Doc
pprint Tru        = "True"
pprint Fals       = "False"
pprint Zero       = "0"
pprint (Succ t)   = "succ" <+> pprint t
pprint (Pred t)   = "pred" <+> pprint t
pprint (IsZero t) = "iszero" <+> pprint t
pprint (Var n)    = ppName n

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

