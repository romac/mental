
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mental.Unify
  ( unify
  , solve
  , UnifyResult
  ) where

#define DEBUG_UNIFY 0

import           Protolude hiding (Constraint)

import qualified Data.Set as Set
import           Data.Functor.Foldable (project)

import           Mental.Error (TyError(..))
import           Mental.Subst
import           Mental.Type
import qualified Mental.Subst as Subst

#if DEBUG_UNIFY
import           Mental.PrettyPrint (ppTy)
#endif

type Constraint = (Ty, Ty)

type UnifyState = (Substitution, [Constraint])

type UnifyResult = Either TyError

newtype Unify a
  = Unify (StateT
             UnifyState
             (Except TyError)
             a)
             deriving ( Functor
                      , Applicative
                      , Monad
                      , MonadState UnifyState
                      , MonadError TyError
                      )

runUnify :: UnifyState -> Unify a -> UnifyResult a
runUnify st (Unify a) = runExcept (evalStateT a st)

solve :: Set Constraint -> UnifyResult Substitution
solve cs = runUnify (Subst.empty, Set.toList cs) unify

unify :: Unify Substitution
unify = do
  (sub, css) <- get
  case css of
    []            -> pure sub
    ((s, t) : cs) -> do

#if DEBUG_UNIFY
      let x' = show (ppTy s)
      let y' = show (ppTy t)
      traceM $ " * Unifying " <> x' <> " with " <> y'
#endif

      (sub', cs') <- unify' (s, t)
      put (sub' <> sub, cs' <> (subst sub' <$> cs))
      unify

unify' :: Constraint -> Unify UnifyState
unify' (s, t) =
  case (project s, project t) of
    (s', t') | s' == t' ->
      pure (mempty, mempty)

    (TyVar n, _) | n `tyOccurs` t ->
      throwError (InfiniteTypeError s t)

    (_, TyVar n) | n `tyOccurs` s ->
      throwError (InfiniteTypeError t s)

    (_, TyVar n) ->
      pure (Subst.singleton n s, mempty)

    (TyVar n, _) ->
      pure (Subst.singleton n t, mempty)

    (TyFun a b, TyFun a' b') ->
      unifyPair (a, a') (b, b')

    (TyPair a b, TyPair a' b') ->
      unifyPair (a, a') (b, b')

    _ ->
      throwError (UnificationError s t)

  where
    unifyPair (a, a') (b, b') = do
      (s1, c1) <- unify' (a, a')
      (s2, c2) <- unify' (subst s1 (b, b'))
      pure (s1 <> s2, c1 <> c2)

