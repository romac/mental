{-# LANGUAGE OverloadedStrings #-}

module RowPoly.PrettyPrint
  ( prettyTree
  , prettyType
  , prettyEvalError
  , prettyTypeError
  ) where

import           Protolude hiding (empty, (<>), (<$>), TypeError)

import qualified Data.Text.Lazy as T
import           Text.PrettyPrint.Leijen.Text.Monadic
import           Unbound.Generics.LocallyNameless (Name, name2String, unbind)
import           Unbound.Generics.LocallyNameless.Fresh (FreshM, runFreshM)

import           RowPoly.Tree
import           RowPoly.Type
import           RowPoly.Eval (EvalError(..))
import           RowPoly.Infer (TypeError(..))

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

prettyEvalError :: EvalError -> Doc
prettyEvalError err = runFreshM (nest 4 ("[ERROR]" <$> pp err))
  where
    pp (NoRuleApplies t) = "•" <+> "Cannot further reduce term:" <+> pprint t
    pp (VarNotInScope n) = "•" <+> "Variable not in scope:" <+> ppName n

prettyTypeError :: TypeError -> Doc
prettyTypeError err = runFreshM (nest 4 ("[ERROR]" <$> pp err))
  where
    pp (InfiniteTypeError s t) = "•" <+> "Cannot unify the infinite type:" <+> ppType s <+> "=" <+> ppType t
    pp (UnificationError s t) = "•" <+> "Cannot unify:" <+> ppType s <+> "with" <+> ppType t
    pp (ValueNotFound n) = "•" <+> "Value not found:" <+> ppName n

prettyTree :: Tree -> Doc
prettyTree = runFreshM . pprint

ppName :: Applicative m => Name a -> m Doc
ppName = text . T.pack . name2String

ppBind :: Applicative m => Maybe Ty -> m Doc
ppBind Nothing   = empty
ppBind (Just tp) = text ":" <+> ppType tp

prettyType :: Ty -> Doc
prettyType = runFreshM . ppType

ppType :: Applicative m => Ty -> m Doc
ppType TyNat       = text "Nat"
ppType TyBool      = text "Bool"
ppType (TyVar n)   = ppName n
ppType (TyFun a@(TyFun _ _) b) = parens (ppType a) <+> text "->" <+> ppType b
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

