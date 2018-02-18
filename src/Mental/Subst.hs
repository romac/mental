{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

module Mental.Subst where

import           Prelude                      (error)
import           Protolude                    hiding (empty)

import qualified Data.Map                     as Map
import           Data.Set                     ((\\))
import qualified Data.Set                     as Set

import           Control.Comonad.Trans.Cofree (CofreeF (..))
import           Data.Functor.Foldable        (cata, embed, para, project)

import           Mental.Name
import           Mental.Tree
import           Mental.Type

newtype Substitution = Substitution (Map TyName Ty)
  deriving (Eq, Ord, Show, Read, Semigroup, Monoid)

empty :: Substitution
empty = mempty

singleton :: TyName -> Ty -> Substitution
singleton k v = Substitution (Map.singleton k v)

fromList :: [(TyName, Ty)] -> Substitution
fromList = Substitution . Map.fromList

class Subst t where
  ftv   :: t -> Set TyName
  apply :: Substitution -> t -> t

hasFtv :: Subst t => TyName -> t -> Bool
hasFtv n e = Set.member n (ftv e)

instance (Subst e, Bifunctor f) => Subst (f e e) where
  ftv = error "implement with Foldable?"
  apply s = bimap (apply s) (apply s)

instance Subst Ty where
  ftv = tyFtv

  apply (Substitution s) = cata alg
    where
      alg ty@(TyVar n) = Map.findWithDefault (embed ty) n s
      alg ty           = embed ty

instance Subst Scheme where
  ftv (Forall vars ty) = ftv ty \\ Set.fromList vars

  apply (Substitution s) (Forall vars ty) =
    Forall vars (subst s' ty)
      where s' = Substitution (foldr Map.delete s vars)

instance Subst v => Subst (Map k v) where
  ftv m = foldMap (ftv . snd) (Map.toList m)
  apply = Map.map . apply

subst :: (Eq t, Subst t) => Substitution -> t -> t
subst sub = pfix (apply sub)

pfix :: Eq a => (a -> a) -> a -> a
pfix f a = let b = f a in if a == b then b else pfix f b

