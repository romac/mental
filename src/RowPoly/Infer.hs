{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RowPoly.Infer
  ( unify
  , collect
  , infer
  , TypeError(..)
  ) where

import Protolude hiding (Constraint, TypeError)

-- import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set ((\\))
import Data.List (lookup)

import Unbound.Generics.LocallyNameless hiding (Subst)
import Unbound.Generics.LocallyNameless.Internal.Fold

import RowPoly.Tree
import RowPoly.Type
import RowPoly.NameSupply

data TypeError
  = ValueNotFound VarName
  | UnificationError Ty Ty
  | InfiniteTypeError Ty Ty

newtype InferM a = InferM (ExceptT TypeError (NameSupplyT FreshM) a)
  deriving (Functor, Applicative, Monad, Fresh, MonadNameSupply, MonadError TypeError)

type InferResult = Either TypeError

runInferM :: InferM a -> InferResult a
runInferM (InferM x) = runFreshM (runNameSupplyT (runExceptT x))

infer :: Tree -> InferResult Ty
infer tree = runInferM $ do
  (ty, cs) <- collect [] tree
  sub <- unify cs
  pure $ substs sub ty

freshTyName :: (Fresh m, MonadNameSupply m) => m TyName
freshTyName = do
  name <- s2n <$> supplyName
  fresh name

freshTyVar :: (Fresh m, MonadNameSupply m) => m Ty
freshTyVar = TyVar <$> freshTyName

type Subst = [(TyName, Ty)]

substScheme :: Subst -> Scheme -> Scheme
substScheme ss (Scheme ps tp) = Scheme ps (substs ss tp)

data Scheme = Scheme [TyName] Ty

instantiate :: Scheme -> InferM Ty
instantiate (Scheme params tp) = do
  freshVars <- replicateM (length params) freshTyVar
  let subs = params `zip` freshVars
  pure $ substs subs tp

freeTyVars :: Ty -> Set TyName
freeTyVars ty = Set.fromList (toListOf fv ty)

tyContains :: Ty -> TyName -> Bool
tyContains ty n = Set.member n (freeTyVars ty)

type Env = [(VarName, Scheme)]

envTypeVars :: Env -> Set TyName
envTypeVars env = Set.fromList $ mapMaybe tyVar env
  where tyVar (_, Scheme _ (TyVar name)) = Just name
        tyVar _                          = Nothing

substEnv :: Subst -> Env -> Env
substEnv ss env = map f env
  where f (n, ts) = (n, substScheme ss ts)

generalize :: Ty -> Env -> Scheme
generalize ty env =
  let vars = Set.toList (freeTyVars ty \\ envTypeVars env)
   in Scheme vars ty

type Constraint = (Ty, Ty)

(<->) :: a -> b -> (a, b)
a <-> b = (a, b)

collect :: Env -> Tree -> InferM (Ty, [Constraint])
collect env term =
  case term of
    Tru  -> pure (TyBool, [])
    Fals -> pure (TyBool, [])
    Zero -> pure (TyNat, [])

    Pred t -> do
      (tp, c) <- collect env t
      pure (TyNat, (tp <-> TyNat) : c)

    Succ t -> do
      (tp, c) <- collect env t
      pure (TyNat, (tp <-> TyNat : c))

    IsZero t -> do
      (tp, c) <- collect env t
      pure (TyBool, (tp <-> TyNat : c))

    If t1 t2 t3 -> do
      (tp1, c1) <- collect env t1
      (tp2, c2) <- collect env t2
      (tp3, c3) <- collect env t3

      let c = [tp1 <-> TyBool, tp2 <-> tp3]

      pure (tp2, c ++ c3 ++ c2 ++ c1)

    Var name -> do
      case lookup name env of
        Nothing     -> throwError (ValueNotFound name)
        Just scheme -> (, []) <$> instantiate scheme

    Abs Nothing bnd -> do
      (x, body) <- unbind bnd
      freshTp <- freshTyVar
      let env' = (x, Scheme [] freshTp) : env
      (tp, c) <- collect env' body

      pure (TyFun freshTp tp, c)

    Abs (Just tp) bnd -> do
      (x, body) <- unbind bnd
      let env' = (x, Scheme [] tp) : env
      (tp', c) <- collect env' body

      pure (TyFun tp tp', c)

    App t1 t2 -> do
      (tp1, c1) <- collect env t1
      s1        <- unify c1
      (tp2, c2) <- collect env t2
      s2        <- unify c2
      freshTp <- freshTyVar

      let funTp = TyFun (substs s1 tp2) freshTp
      let c3 = [(substs s2 tp1 <-> funTp)]

      pure (freshTp, c3 ++ c2 ++ c1)

    Let Nothing v bnd -> do
      (tpS, c) <- collect env v
      s       <- unify c

      let tpT    = substs s tpS
      let env'   = substEnv s env
      let scheme = generalize tpT env'

      (x, body) <- unbind bnd
      let env''  = (x, scheme) : env'

      (tp, c') <- collect env'' body
      pure (tp, c' ++ c')

    Let (Just tpX) v bnd -> do
      (x, body) <- unbind bnd
      (tpV, c) <- collect env v

      let env' = (x, Scheme [] tpX) : env
      (tp, c')  <- collect env' body

      let c'' = [tpX <-> tpV]
      pure (tp, c'' ++ c' ++ c)

unify :: [Constraint] -> InferM Subst
unify = go []
  where
    go :: Subst -> [Constraint] -> InferM Subst
    go acc [] =
      pure acc

    go acc (c : cs) =
      case c of
        (s, t) | s == t -> go acc cs

        (s@(TyVar n), t) | tyContains t n ->
          throwError (InfiniteTypeError s t)

        (s, t@(TyVar n)) | tyContains s n ->
          throwError (InfiniteTypeError t s)

        (s, TyVar n) ->
          let sub = [(n, s)]
           in go (sub ++ acc) (substPair sub <$> cs)

        (TyVar n, t) ->
          let sub = [(n, t)]
           in go (sub ++ acc) (substPair sub <$> cs)

        (TyFun a b, TyFun a' b') -> do
          s1 <- unify ((a, a') : cs)
          s2 <- unify (substPair s1 (b, b') : cs)
          let s = s1 ++ s2

          go (s ++ acc) cs

        (s, t) ->
          throwError (UnificationError s t)

substPair :: Subst -> Constraint -> Constraint
substPair sub (a, b) = (substs sub a, substs sub b)

