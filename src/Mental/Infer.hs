{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mental.Infer
  ( unify
  , inferType
  , TypeError(..)
  ) where

#define DEBUG_INFER 0

import           Protolude hiding (Constraint, TypeError, First)

import qualified Data.Set as Set
import           Data.Set ((\\))
import qualified Data.Map.Strict as Map

import           Control.Monad.Writer.Strict hiding (First, (<>))

import           Unbound.Generics.LocallyNameless hiding (Subst)
import           Unbound.Generics.LocallyNameless.Internal.Fold

import           Mental.Subst (Subst(..))
import qualified Mental.Subst as Subst
import           Mental.Tree
import           Mental.Type
import           Mental.Error
import           Mental.Primitive
import           Mental.NameSupply

#if DEBUG_INFER
import           Mental.PrettyPrint (prettyType)
#endif

type InferResult = Either TypeError
type UnifyResult = Either TypeError

type Env = Map VarName Scheme

type Constraint = (Ty, Ty)

newtype Infer a
  = Infer (ReaderT
            Env
            (ExceptT
              TypeError
              (WriterT
                (Set Constraint)
                (NameSupplyT FreshM))) a)
  deriving ( Functor
           , Applicative
           , Monad
           , Fresh
           , MonadNameSupply
           , MonadReader Env
           , MonadWriter (Set Constraint)
           , MonadError TypeError
           )

type UnifyState = (Subst, [Constraint])

newtype Unify a
  = Unify (StateT
             UnifyState
             (Except TypeError)
             a)
             deriving ( Functor
                      , Applicative
                      , Monad
                      , MonadState UnifyState
                      , MonadError TypeError
                      )

data Scheme = Forall [TyName] Ty
  deriving (Eq, Show, Generic, Typeable)

instance Alpha Scheme

forAll :: Ty -> Scheme
forAll = Forall []

runInfer :: Env -> Infer a -> InferResult (a, Set Constraint)
runInfer env (Infer x) =
  case runFreshM (runNameSupplyT (runWriterT (runExceptT (runReaderT x env)))) of
    (Left err, _) -> Left err
    (Right a, cs) -> Right (a, cs)

runUnify :: UnifyState -> Unify a -> UnifyResult a
runUnify st (Unify a) = runExcept (evalStateT a st)

solve :: Set Constraint -> UnifyResult Subst
solve cs = runUnify (Subst.empty, Set.toList cs) unify

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

inferType :: Tree -> InferResult Ty
inferType tree = do
  (ty, cs) <- runInfer Map.empty (infer tree)
  sub      <- solve cs
  pure (Subst.apply sub ty)

freshTyName :: (Fresh m, MonadNameSupply m) => m TyName
freshTyName = do
  name <- s2n <$> supplyName
  fresh name

freshTy :: (Fresh m, MonadNameSupply m) => m Ty
freshTy = TyVar <$> freshTyName

ftvTy :: Ty -> Set TyName
ftvTy ty = Set.fromList (toListOf fv ty)

ftvEnv :: Env -> Set TyName
ftvEnv env = Set.fromList (toListOf fv (Map.elems env))

substEnv :: Subst -> Env -> Env
substEnv s = Map.map (substScheme s)

substScheme :: Subst -> Scheme -> Scheme
substScheme (Subst s) (Forall vars ty) = Forall vars (Subst.apply s' ty)
  where s' = Subst (foldr Map.delete s vars)

tyContains :: Ty -> TyName -> Bool
tyContains ty n = Set.member n (ftvTy ty)

generalize :: Ty -> Env -> Scheme
generalize ty env = runIdentity $ do
  let vars = Set.toList (ftvTy ty \\ ftvEnv env)
  pure $ Forall vars ty

instantiate :: Scheme -> Infer Ty
instantiate (Forall params ty) = do
  freshTys <- mapM (const freshTy) params
  let subs = params `zip` freshTys
  pure $ substs subs ty

addConstraint' :: Constraint -> Infer ()
addConstraint' = tell . Set.singleton

addConstraint :: Ty -> Ty -> Infer ()
addConstraint a b = addConstraint' (a, b)

(<->) :: Ty -> Ty -> Infer ()
(<->) = addConstraint

withBinding :: Infer Ty -> (VarName, Scheme) -> Infer Ty
withBinding a (x, scheme) = local (Map.insert x scheme) a

infer :: Tree -> Infer Ty
infer tree =
  case tree of
    Tru  -> pure TyBool
    Fals -> pure TyBool
    Zero -> pure TyNat

    Prim Succ   -> pure $ TyFun TyNat TyNat
    Prim Pred   -> pure $ TyFun TyNat TyNat
    Prim IsZero -> pure $ TyFun TyNat TyBool

    Prim First -> do
      a <- freshTy
      b <- freshTy
      pure (TyFun (TyPair a b) a)

    Prim Second -> do
      a <- freshTy
      b <- freshTy
      pure (TyFun (TyPair a b) b)

    Prim Fix -> do
      a <- freshTy
      pure (TyFun (TyFun a a) a)

    Pair a b ->
      TyPair <$> infer a <*> infer b

    Inl t ty -> do
      tr <- freshTy
      tl <- infer t

      ty <-> TySum tl tr
      pure ty

    Inr t ty -> do
      tl <- freshTy
      tr <- infer t

      ty <-> TySum tl tr
      pure ty

    Case t inl inr -> do
      ty <- infer t
      tl <- infer (Let Nothing t inl)
      tr <- infer (Let Nothing t inr)

      tl <-> tr
      ty <-> TySum tl tr

      pure tl

    If cnd thn els -> do
      tc <- infer cnd
      tt <- infer thn
      te <- infer els

      tc <-> TyBool
      tt <-> te

      pure tt

    Var name -> do
      env <- ask
      case Map.lookup name env of
        Nothing     -> throwError (ValueNotFound name)
        Just scheme -> instantiate scheme

    Abs ty bnd -> do
      (x, body) <- unbind bnd
      tp        <- fromMaybe freshTy (pure <$> ty)
      tp'       <- infer body `withBinding` (x, forAll tp)

      pure (TyFun tp tp')

    App f x -> do
      tf <- infer f
      tx <- infer x
      ty <- freshTy

      tf <-> TyFun tx ty

      pure ty

    Let Nothing v bnd -> do
      env       <- ask
      (tv, cv)  <- listen (infer v)
      case solve cv of
        Left err -> throwError err
        Right sub -> do
          let scheme = generalize (Subst.apply sub tv) (substEnv sub env)
          (x, body) <- unbind bnd
          local (substEnv sub) (infer body) `withBinding` (x, scheme)

    Let (Just tx) v bnd -> do
      (x, body) <- unbind bnd
      tv        <- infer v

      tx <-> tv

      infer body `withBinding` (x, forAll tx)

unify :: Unify Subst
unify = do
  (sub, css) <- get
  case css of
    []            -> pure sub
    ((s, t) : cs) -> do

#if DEBUG_INFER
      let x' = show (prettyType s)
      let y' = show (prettyType t)
      traceM $ " * Unifying " <> x' <> " with " <> y'
#endif

      (sub', cs') <- unify' (s, t)
      put (sub' <> sub, cs' <> (Subst.onPair sub' <$> cs))
      unify

unify' :: Constraint -> Unify UnifyState
unify' c = case c of
  (s, t) | s == t ->
    pure (mempty, mempty)

  (s@(TyVar n), t) | tyContains t n ->
    throwError (InfiniteTypeError s t)

  (s, t@(TyVar n)) | tyContains s n ->
    throwError (InfiniteTypeError t s)

  (s, TyVar n) ->
    pure (Subst.singleton n s, mempty)

  (TyVar n, t) ->
    pure (Subst.singleton n t, mempty)

  (TyFun a b, TyFun a' b') ->
    unifyPair (a, a') (b, b')

  (TyPair a b, TyPair a' b') ->
    unifyPair (a, a') (b, b')

  (TySum a b, TySum a' b') ->
    unifyPair (a, a') (b, b')

  (s, t) ->
    throwError (UnificationError s t)

  where
    unifyPair (a, a') (b, b') = do
      (s1, c1) <- unify' (a, a')
      (s2, c2) <- unify' (Subst.onPair s1 (b, b'))
      pure (s1 <> s2, c1 <> c2)

