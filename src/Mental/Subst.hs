{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mental.Subst where

import           Protolude hiding (empty)

import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Set ((\\))

import           Data.Functor.Foldable (cata, para, embed, project)
import           Control.Comonad.Trans.Cofree (CofreeF(..))

import           Mental.Name
import           Mental.Type
import           Mental.Tree

newtype Substitution = Substitution (Map TyName Ty)
  deriving (Eq, Ord, Show, Read, Semigroup, Monoid)

empty :: Substitution
empty = mempty

singleton :: TyName -> Ty -> Substitution
singleton k v = Substitution (Map.singleton k v)

fromList :: [(TyName, Ty)] -> Substitution
fromList = Substitution . Map.fromList

biApply :: (Eq e, Subst e, Bifunctor f) => Substitution -> f e e -> f e e
biApply s = bimap (subst s) (subst s)

class Subst t where
  ftv   :: t -> Set TyName
  subst' :: Substitution -> t -> t

hasFtv :: Subst t => TyName -> t -> Bool
hasFtv n e = Set.member n (ftv e)

instance Subst Ty where
  ftv = tyFtv

  subst' (Substitution s) = cata alg
    where
      alg ty@(TyVar n) = Map.findWithDefault (embed ty) n s
      alg ty           = embed ty

instance Subst Scheme where
  ftv (Forall vars ty) = ftv ty \\ Set.fromList vars

  subst' (Substitution s) (Forall vars ty) =
    Forall vars (subst s' ty)
      where s' = Substitution (foldr Map.delete s vars)

instance Subst v => Subst (Map k v) where
  ftv m = foldMap (ftv . snd) (Map.toList m)
  subst' = Map.map . subst'

subst :: (Eq t, Subst t) => Substitution -> t -> t
subst sub = pfix (subst' sub)

pfix :: Eq a => (a -> a) -> a -> a
pfix f a = let b = f a in if a == b then b else pfix f b

