{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mental.Subst where

import           Protolude hiding (empty)

import qualified Data.Map.Strict as Map

import           Mental.Name
import           Mental.Type

newtype Subst = Subst (Map TyName Ty)
  deriving (Eq, Ord, Show, Read)

instance Semigroup Subst where
  Subst s1 <> Subst s2 = Subst (Map.map (apply (Subst s1)) s2 `Map.union` s1)

instance Monoid Subst where
  mempty = empty
  mappend = (<>)

apply :: Subst -> Ty -> Ty
apply (Subst s) = undefined

empty :: Subst
empty = Subst Map.empty

singleton :: TyName -> Ty -> Subst
singleton k v = Subst (Map.singleton k v)

onPair :: Subst -> (Ty, Ty) -> (Ty, Ty)
onPair s = bimap (apply s) (apply s)

