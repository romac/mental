{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mental.Infer where

#define DEBUG_INFER 1

import           Protolude hiding (Constraint, TypeError, First)

import qualified Data.Text as T
import qualified Data.Set as Set
import           Data.Set ((\\))
import qualified Data.Map.Strict as Map

import           Control.Monad.RWS.Strict hiding (First, (<>))
import           Control.Comonad.Cofree (Cofree(..))
import           Data.Functor.Foldable (embed)

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
import           Mental.PrettyPrint (ppTy, ppScheme)
#endif

type InferResult = Either TyError

type Env = Map VarName Scheme

type Constraint = (Ty, Ty)

type InferState = Map UntypedTree Ty

newtype Infer a
  = Infer (RWST
            Env
            (Set Constraint)
            InferState
            (ExceptT
              TyError
                Fresh)
            a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadFresh
           , MonadReader Env
           , MonadWriter (Set Constraint)
           , MonadState InferState
           , MonadError TyError
           )

runInfer :: Env -> Infer a -> InferResult a
#if DEBUG_INFER
runInfer env (Infer a) = do
  (res, cs) <- run
  debugConstraints cs
  pure res
#else
runInfer env (Infer a) = fst <$> run
#endif
  where
    run = runFresh fresh
    fresh  = runExceptT except
    except = evalRWST a env mempty

hoistErr :: MonadError e m => Either e a -> m a
hoistErr (Left  e) = throwError e
hoistErr (Right a) = pure a

#if DEBUG_INFER
debugConstraints :: Monad m => Set Constraint -> m ()
debugConstraints cs = do
  traceM "\n# Constraints:"
  forM_ (Set.toList cs) pp
  traceM ""
    where
      pp (a, b) = do
        let l = show (ppTy a)
        let r = show (ppTy b)
        traceM ("# " <> l <> " <-> " <> r)

debugSubst :: Monad m => Substitution -> m ()
debugSubst (Substitution sub) = do
  traceM "\n# Substitutions:"
  forM_ (Map.toList sub) pp
  traceM ""
    where
      pp (a, b) = do
        let l = nameText a
        let r = show (ppTy b)
        traceM ("# " <> l <> " / " <> r)

debugEnv :: Monad m => Env -> m ()
debugEnv env = do
  traceM "\n# Environment:"
  forM_ (Map.toList env) pp
  traceM ""
    where
      pp (a, b) = do
        let l = nameText a
        let r = show (ppScheme b)
        traceM ("- " <> l <> " <=> " <> r)
#endif

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
addConstraint a b = do
  tell (Set.singleton (a, b))

(<->) :: Ty -> Ty -> Infer ()
(<->) = addConstraint

withBinding :: Infer Ty -> (VarName, Scheme) -> Infer Ty
withBinding a (x, scheme) = local (Map.insert x scheme) a

typeModule :: UntypedModule -> InferResult TypedModule
typeModule m@(Module _ decls) = do
  let env = mconcat (declToEnv <$> decls)
  debugEnv env
  runInfer env (inferModule m)

declToEnv :: Decl a -> Map VarName Scheme
declToEnv (FunDecl name (Just ty) _) =
  Map.singleton name (quantify ty)

declToEnv (FunDecl name Nothing _) =
  Map.singleton name (Forall [] (tyVar (mkName (nameText name))))
  -- Map.singleton name (Forall [mkName "a"] (tyVar (mkName "a")))

declToEnv (TyDecl name ty) =
  Map.singleton name (quantify ty)

quantify :: Ty -> Scheme
quantify ty =
  Forall (Set.toList (ftv ty)) ty

inferModule :: UntypedModule -> Infer TypedModule
inferModule (Module name decls) = do
  (preDecls, cs) <- listen (traverse preTypeDecl decls)
  sub <- hoistErr (solve cs)
  debugConstraints cs
  debugSubst sub
  let typedDecls = substDecl sub <$> preDecls
  pure (Module name typedDecls)

substDecl :: Substitution -> TypedDecl -> TypedDecl
substDecl _ (TyDecl name ty) = TyDecl name ty
substDecl sub (FunDecl name ty body) =
  FunDecl name (subst sub <$> ty) (subst sub <$> body)

preTypeDecl :: UntypedDecl -> Infer TypedDecl
preTypeDecl (TyDecl name ty) = pure (TyDecl name ty)
preTypeDecl (FunDecl name _ body) = do
  ty :< tree <- annotateTree body
  pure (FunDecl name (Just ty) (ty :< tree))

typeTree :: UntypedTree -> Infer TypedTree
typeTree tree = do
  (preTree, cs) <- listen (annotateTree tree)
  sub <- hoistErr (solve cs)
  pure (subst sub <$> preTree)

annotateTree :: UntypedTree -> Infer TypedTree
annotateTree = annotateM inferType

inferType :: UntypedTree -> Infer Ty
inferType = memoizeM infer'
  where
    infer' (_ :< tree) =
      case tree of

        Unit ->
          pure tyUnit

        Tru  ->
          pure tyBool

        Fals ->
          pure tyBool

        Pair a b ->
          tyPair <$> inferType a <*> inferType b

        If cnd thn els -> do
          tc <- inferType cnd
          tt <- inferType thn
          te <- inferType els

          tc <-> tyBool
          tt <-> te

          pure tt

        IntLit _ ->
          pure tyInt

        Var name -> do
          env <- ask
          case Map.lookup name env of
            Nothing     -> throwError (ValueNotFound name)
            Just scheme -> instantiate scheme

        Abs ty (x, body) -> do
          tp  <- fromMaybe freshTy (pure <$> ty)
          tp' <- inferType body `withBinding` (x, forAll tp)

          pure (tyFun tp tp')

        App f x -> do
          tf <- inferType f
          tx <- inferType x
          ty <- freshTy

          tf <-> tyFun tx ty

          pure ty

        Let x Nothing v body -> do
          env       <- ask
          (tv, cv)  <- listen (inferType v)
          case solve cv of
            Left err -> throwError err
            Right sub -> do
              let scheme = generalize (subst sub tv) (subst sub env)
              local (subst sub) (inferType body) `withBinding` (x, scheme)

        Let x (Just tx) v body -> do
          tv <- inferType v
          tx <-> tv
          inferType body `withBinding` (x, forAll tx)

        Prim PIntPlus ->
          pure $ tyFun2 tyInt tyInt tyInt

        Prim PIntMinus ->
          pure $ tyFun2 tyInt tyInt tyInt

        Prim PIntMul ->
          pure $ tyFun2 tyInt tyInt tyInt

        Prim PIntDiv ->
          pure $ tyFun2 tyInt tyInt tyInt

        Prim PIntEq ->
          pure $ tyFun2 tyInt tyInt tyBool

        Prim PIntLess ->
          pure $ tyFun2 tyInt tyInt tyBool

        Prim PIntNeg ->
          pure $ tyFun tyInt tyInt

        Prim PFirst -> do
          a <- freshTy
          b <- freshTy
          pure $ tyFun (tyPair a b) a

        Prim PSecond -> do
          a <- freshTy
          b <- freshTy
          pure $ tyFun (tyPair a b) b

        Prim PFix -> do
          a <- freshTy
          pure $ tyFun (tyFun a a) a

