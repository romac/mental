{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mental.Infer where

#define DEBUG_INFER 0

import           Protolude hiding (Constraint, TypeError, First)

import qualified Data.Set as Set
import           Data.Set ((\\))
import qualified Data.Map.Strict as Map

import           Control.Monad.RWS.Strict hiding (First, (<>))
import           Control.Comonad.Cofree (Cofree(..))

import           Mental.Annotate
import           Mental.Decl
import           Mental.Error
import           Mental.Memoize
import           Mental.Name
import           Mental.Fresh
import           Mental.Primitive
import           Mental.Subst
import qualified Mental.Subst as Subst
import           Mental.Tree
import           Mental.Tree.Untyped
import           Mental.Tree.Typed
import           Mental.Type
import           Mental.Unify

#if DEBUG_INFER
import           Mental.PrettyPrint (prettyType)
#endif

type InferResult = Either TyError

type Env = Map VarName Scheme

type Constraint = (Ty, Ty)

type Memo = Map UntypedTree Ty

newtype Infer a
  = Infer
  { runInfer :: RWST
                  Env
                  (Set Constraint)
                  Memo
                  (ExceptT
                    TyError
                      Fresh)
                  a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadFresh
           , MonadReader Env
           , MonadWriter (Set Constraint)
           , MonadState Memo
           , MonadError TyError
           )

#if DEBUG_INFER
debugConstraints :: Monad m => Set Constraint -> m ()
debugConstraints cs = do
  traceM "\n# Constraints:"
  forM_ (Set.toList cs) pp
  traceM ""
    where
      pp (a, b) = do
        let l = show (prettyType a)
        let r = show (prettyType b)
        traceM ("# " <> l <> " <-> " <> r)

debugSubst :: Monad m => Subst -> m ()
debugSubst (Subst sub) = do
  traceM "\n# Substitutions:"
  forM_ (Map.toList sub) pp
  traceM ""
    where
      pp (a, b) = do
        let l = show a
        let r = show (prettyType b)
        traceM ("# " <> l <> " / " <> r)
#endif

typeModule :: Module -> InferResult ()
typeModule = error "typeModule"

-- inferModule (Module _ decls) = do
--   let env = mconcat (declToEnv <$> decls)
--   ((), cs) <- runInfer env (forM_ decls inferDecl)
--   void (solve cs)
--     where
--       declToEnv (FunDecl name (Just ty) _) = Map.singleton name (quantify ty)
--       declToEnv (FunDecl name Nothing _)   = Map.singleton name (Forall [mkName "a"] (TyVar (mkName "a")))
--       declToEnv (TyDecl name ty)           = Map.singleton name (quantify ty)
--       quantify ty = Forall (Set.toList (ftv ty)) ty

freshTy :: MonadFresh m => m Ty
freshTy = tyVar <$> freshName

generalize :: Ty -> Env -> Scheme
generalize ty env =
  let vars = Set.toList (ftv ty \\ ftv env)
   in Forall vars ty

instantiate :: Scheme -> Infer Ty
instantiate (Forall params ty) = do
  freshTys <- mapM (const freshTy) params
  let sub = Subst.fromList (params `zip` freshTys)
  pure $ subst sub ty

addConstraint :: Ty -> Ty -> Infer ()
addConstraint a b = tell (Set.singleton (a, b))

(<->) :: Ty -> Ty -> Infer ()
(<->) = addConstraint

withBinding :: Infer Ty -> (VarName, Scheme) -> Infer Ty
withBinding a (x, scheme) = local (Map.insert x scheme) a

-- FIXME
inferDecl :: Decl -> Infer Ty
inferDecl = error "inferDecl"
-- inferDecl (FunDecl name ty body) =
--   infer (Let ty body name body)

typeTree :: UntypedTree -> InferResult TypedTree
typeTree tree = do
  (tyTree, cs) <- annotateTree tree
  sub          <- solve cs
  pure (subst sub <$> tyTree)

annotateTree :: UntypedTree -> InferResult (TypedTree, Set Constraint)
annotateTree tree = do
  let infer' = annotateM infer tree
      except = evalRWST (runInfer infer') Map.empty Map.empty
      fresh  = runExceptT except
   in runFresh fresh

infer :: UntypedTree -> Infer Ty
infer = memoizeM infer'
  where
    infer' (_ :< tree) =
      case tree of
        Tru  -> pure tyBool
        Fals -> pure tyBool

        Prim PFirst -> do
          a <- freshTy
          b <- freshTy
          pure (tyFun (tyPair a b) a)

        Prim PSecond -> do
          a <- freshTy
          b <- freshTy
          pure (tyFun (tyPair a b) b)

        Prim PFix -> do
          a <- freshTy
          pure (tyFun (tyFun a a) a)

        Pair a b ->
          tyPair <$> infer a <*> infer b

        If cnd thn els -> do
          tc <- infer cnd
          tt <- infer thn
          te <- infer els

          tc <-> tyBool
          tt <-> te

          pure tt

        IntLit _ ->
          pure tyNat

        Var name -> do
          env <- ask
          case Map.lookup name env of
            Nothing     -> throwError (ValueNotFound name)
            Just scheme -> instantiate scheme

        Abs ty (x, body) -> do
          tp  <- fromMaybe freshTy (pure <$> ty)
          tp' <- infer body `withBinding` (x, forAll tp)

          pure (tyFun tp tp')

        App f x -> do
          tf <- infer f
          tx <- infer x
          ty <- freshTy

          tf <-> tyFun tx ty

          pure ty

        Let x Nothing v body -> do
          env       <- ask
          (tv, cv)  <- listen (infer v)
          case solve cv of
            Left err -> throwError err
            Right sub -> do
              let scheme = generalize (subst sub tv) (subst sub env)
              local (subst sub) (infer body) `withBinding` (x, scheme)

        Let x (Just tx) v body -> do
          tv <- infer v
          tx <-> tv
          infer body `withBinding` (x, forAll tx)

