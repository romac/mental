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
  , inferTree
  , inferModule
  ) where

#define DEBUG_INFER 0

import           Protolude hiding (Constraint, TypeError, First)

import qualified Data.Set as Set
import           Data.Set ((\\))
import qualified Data.Map.Strict as Map

import           Control.Monad.RWS.Strict hiding (First, (<>))

import           Mental.Decl
import           Mental.Error
import           Mental.Name
import           Mental.NameSupply
import           Mental.Primitive
import           Mental.Subst (Subst(..))
import qualified Mental.Subst as Subst
import           Mental.Tree
import           Mental.Type
import           Mental.Unify

#if DEBUG_INFER
import           Mental.PrettyPrint (prettyType)
#endif

type InferResult = Either TypeError

type Env = Map VarName Scheme

type Constraint = (Ty, Ty)

newtype Infer a
  = Infer (RWST
            Env
            (Set Constraint)
            ()
            (ExceptT
              TypeError
                NameSupply)
            a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadNameSupply
           , MonadReader Env
           , MonadWriter (Set Constraint)
           , MonadError TypeError
           )

data Scheme = Forall [TyName] Ty
  deriving (Eq, Ord, Show, Read, Generic, Typeable)

forAll :: Ty -> Scheme
forAll = Forall []

runInfer :: Env -> Infer a -> InferResult (a, Set Constraint)
runInfer env (Infer x) =
  case runNameSupply (runExceptT (evalRWST x env ())) of
    Left err      -> Left err
    Right (a, cs) -> Right (a, cs)

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

inferTree :: Tree -> InferResult Ty
inferTree tree = do
  (ty, cs) <- runInfer Map.empty (infer tree)
  sub      <- solve cs
  pure (Subst.apply sub ty)

-- FIXME
inferModule :: Module -> InferResult ()
inferModule (Module _ decls) = do
  let env = mconcat (declToEnv <$> decls)
  ((), cs) <- runInfer env (forM_ decls inferDecl)
  void (solve cs)
    where
      declToEnv (FunDecl name (Just ty) _) = Map.singleton name (quantify ty)
      declToEnv (FunDecl name Nothing _)   = Map.singleton name (Forall [mkName "a"] (TyVar (mkName "a")))
      -- declToEnv (TyDecl name ty)           = Map.singleton name (quantify ty)
      quantify ty = Forall (Set.toList (tyFtv ty)) ty

freshTyName :: MonadNameSupply m => m TyName
freshTyName = mkNameStr <$> supplyName

freshTy :: MonadNameSupply m => m Ty
freshTy = TyVar <$> freshTyName

ftvEnv :: Env -> Set TyName
ftvEnv _ = Set.fromList (error "(toListOf fv (Map.elems env))")

substEnv :: Subst -> Env -> Env
substEnv s = Map.map (substScheme s)

substScheme :: Subst -> Scheme -> Scheme
substScheme (Subst s) (Forall vars ty) = Forall vars (Subst.apply s' ty)
  where s' = Subst (foldr Map.delete s vars)

generalize :: Ty -> Env -> Scheme
generalize ty env = runIdentity $ do
  let vars = Set.toList (tyFtv ty \\ ftvEnv env)
  pure $ Forall vars ty

instantiate :: Scheme -> Infer Ty
instantiate (Forall params ty) = do
  freshTys <- mapM (const freshTy) params
  let subs = params `zip` freshTys
  pure $ (error "substs") subs ty

addConstraint :: Ty -> Ty -> Infer ()
addConstraint a b = tell (Set.singleton (a, b))

(<->) :: Ty -> Ty -> Infer ()
(<->) = addConstraint

withBinding :: Infer Ty -> (VarName, Scheme) -> Infer Ty
withBinding a (x, scheme) = local (Map.insert x scheme) a

-- FIXME
inferDecl :: Decl -> Infer Ty
inferDecl (FunDecl name ty body) =
  infer (Let ty body name body)

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

    -- FIXME
    -- Case t inl inr -> do
    --   ty <- infer t
    --   tl <- infer (Let Nothing t inl)
    --   tr <- infer (Let Nothing t inr)

    --   tl <-> tr
    --   ty <-> TySum tl tr

    --   pure tl

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

    Abs ty x body -> do
      tp  <- fromMaybe freshTy (pure <$> ty)
      tp' <- infer body `withBinding` (x, forAll tp)

      pure (TyFun tp tp')

    App f x -> do
      tf <- infer f
      tx <- infer x
      ty <- freshTy

      tf <-> TyFun tx ty

      pure ty

    Let Nothing v x body -> do
      env       <- ask
      (tv, cv)  <- listen (infer v)
      case solve cv of
        Left err -> throwError err
        Right sub -> do
          let scheme = generalize (Subst.apply sub tv) (substEnv sub env)
          local (substEnv sub) (infer body) `withBinding` (x, scheme)

    Let (Just tx) v x body -> do
      tv <- infer v

      tx <-> tv

      infer body `withBinding` (x, forAll tx)

