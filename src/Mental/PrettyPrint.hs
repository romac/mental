{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Mental.PrettyPrint
  ( ppTree
  , ppAnnTree
  , ppTypedTree
  , ppTy
  , ppPrim
  , ppDecl
  , ppModule
  , ppEvalError
  , ppTyError
  ) where

import           Protolude hiding (empty, (<>), (<$>))

import           Data.Functor.Foldable (cata, para, project)
import           Control.Comonad.Trans.Cofree (CofreeF(..))
import           Text.PrettyPrint.Leijen.Text

import           Mental.Annotate
import           Mental.Decl
import           Mental.Error
import           Mental.Name
import           Mental.Primitive
import           Mental.Tree
import           Mental.Tree.Untyped
import           Mental.Tree.Typed
import           Mental.Type

bullet :: Doc -> Doc
bullet = (text "-" <+>)

errorDoc :: Doc -> Doc
errorDoc err = nest 4 (text "Error:" <$> bullet err)

ppEvalError :: EvalError -> Doc
ppEvalError err = nest 4 (errorDoc (pp err))
  where
    pp (NoRuleApplies t) = "Cannot further reduce term:" <+> ppTree t
    pp (VarNotInScope n) = "Variable not in scope:"      <+> ppName n

ppTyError :: TyError -> Doc
ppTyError err = errorDoc (pp err)
  where
    pp (InfiniteTypeError s t) = "Cannot unify the infinite type:" <+> ppTy s <+> " = " <+> ppTy t
    pp (UnificationError s t)  = "Cannot unify:" <+> ppTy s <+> "with" <+> ppTy t
    pp (ValueNotFound n)       = "Value not found:" <+> ppName n

ppModule :: Module -> Doc
ppModule (Module name decls) =
  "module" <+> ppName name <+> "where" <> line <> line <> decls'
    where
      decls' = vcat (fmap pp decls)
      pp decl = ppDecl decl <> line

ppDecl :: Decl -> Doc
ppDecl (FunDecl name (Just ty) body) =
  ppName name <+> ":" <+> ppTy ty
    <$$> ppDecl (FunDecl name Nothing body)

ppDecl (FunDecl name Nothing body) =
  ppName name <+> "=" <+> ppAnnTree body

ppDecl (TyDecl name ty) =
  "type" <+> ppName name <+> ppTy ty

ppPrim :: Primitive -> Doc
ppPrim PFirst  = "fst"
ppPrim PSecond = "snd"
ppPrim PFix    = "fix"

ppName :: VarName -> Doc
ppName = text . nameTextLazy

ppBind :: Maybe Ty -> Doc
ppBind Nothing   = empty
ppBind (Just tp) = ":" <+> ppTy tp

ppTy :: Ty -> Doc
ppTy = para ppTy'

ppTy' :: TyF (Ty, Doc) -> Doc
ppTy' TyNat     = "Nat"
ppTy' TyBool    = "Bool"
ppTy' (TyVar n) = ppName n

ppTy' (TyFun (project -> TyFun _ _, a) (_, b)) =
  parens (a <+> "->" <+> b)

ppTy' (TyFun (_, a) (_,b))   = a <+> "->" <+> b
ppTy' (TyPair (_, a) (_, b)) = parens (a <> "," <+> b)

ppTypedTree :: TypedTree -> Doc
ppTypedTree = cata ppTypedTree'

ppTypedTree' :: CofreeF TreeF Ty Doc -> Doc
ppTypedTree' t@(ty :< _) = parens (ppTree' t) <> ":" <+> ppTy ty

ppTree :: Tree -> Doc
ppTree = ppAnnTree . annotate (const ())

ppAnnTree :: AnnTree a -> Doc
ppAnnTree = cata ppTree'

ppTree' :: CofreeF TreeF a Doc -> Doc
ppTree' (_ :< Tru)            = "True"
ppTree' (_ :< Fals)           = "False"
ppTree' (_ :< Var n)          = ppName n
ppTree' (_ :< IntLit n)       = text (show n)
ppTree' (_ :< Prim prim)      = ppPrim prim
ppTree' (_ :< Pair a b)       = parens (a <> comma <+> b)

ppTree' (_ :< If cnd thn els) =
  "if" <+> cnd <+> "then" <+> thn <+> "else" <+> els

ppTree' (_ :< Abs tp (x, body)) =
  text "\\" <> ppName x <+> ppBind tp <+> text "->" <+> body

ppTree' (_ :< Let x tp val body) =
  text "let" <+> ppName x <+> ppBind tp <+> text "=" <+> val <+> "in" <+> body

-- ppTree' (_ :< App f x@(_ :< App _ _)) =
--   ppTree f <+> parens x

ppTree' (_ :< App f x) =
  f <+> x

