
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

import           Protolude hiding (Constraint, TypeError)

import qualified Data.Set as Set

import qualified Mental.Subst as Subst
import           Mental.Subst (Subst(..))
import           Mental.Error (TypeError(..))
import           Mental.Type

type Constraint = (Ty, Ty)

type UnifyState = (Subst, [Constraint])

type UnifyResult = Either TypeError

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

runUnify :: UnifyState -> Unify a -> UnifyResult a
runUnify st (Unify a) = runExcept (evalStateT a st)

solve :: Set Constraint -> UnifyResult Subst
solve cs = runUnify (Subst.empty, Set.toList cs) unify

unify :: Unify Subst
unify = do
  (sub, css) <- get
  case css of
    []            -> pure sub
    ((s, t) : cs) -> do

#if DEBUG_UNIFY
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

