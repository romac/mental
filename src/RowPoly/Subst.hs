{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RowPoly.Subst where

import           Protolude

import qualified Data.Map.Strict as Map

import           Unbound.Generics.LocallyNameless (substs)

import           RowPoly.Type

newtype Subst = Subst (Map TyName Ty)
  deriving (Semigroup, Monoid)

apply :: Subst -> Ty -> Ty
apply (Subst s) = substs (Map.toList s)

singleton :: TyName -> Ty -> Subst
singleton k v = Subst (Map.singleton k v)

onPair :: Subst -> (Ty, Ty) -> (Ty, Ty)
onPair s = bimap (apply s) (apply s)

onPairs :: Functor f => Subst -> f (Ty, Ty) -> f (Ty, Ty)
onPairs s = fmap (onPair s)

