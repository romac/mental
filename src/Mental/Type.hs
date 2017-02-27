{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Mental.Type
  ( TyF(..)
  , Ty
  , Scheme(..)
  , forAll
  , tyVar
  , tyInt
  , tyBool
  , tyFun
  , tyPair
  , tyOccurs
  , tyFtv
  ) where

import           Protolude

import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)
import           Data.Deriving

import qualified Data.Set as Set
import           Data.Functor.Foldable (Mu, embed, cata)

import           Mental.Name

data TyF a
  = TyVar !TyName
  | TyFun !a !a
  | TyInt
  | TyBool
  | TyPair !a !a
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Generic, Typeable)

$(deriveEq1   ''TyF)
$(deriveOrd1  ''TyF)
$(deriveShow1 ''TyF)
$(deriveRead1 ''TyF)

type Ty = Mu TyF

data Scheme = Forall [TyName] Ty
  deriving (Eq, Ord, Show, Read, Generic, Typeable)

forAll :: Ty -> Scheme
forAll = Forall []

tyVar :: TyName -> Ty
tyVar = embed . TyVar

tyInt :: Ty
tyInt = embed TyInt

tyBool :: Ty
tyBool = embed TyBool

tyFun :: Ty -> Ty -> Ty
tyFun a b = embed (TyFun a b)

tyPair :: Ty -> Ty -> Ty
tyPair a b = embed (TyPair a b)

tyFtv :: Ty -> Set TyName
tyFtv = cata alg
  where
    alg (TyVar n) = Set.singleton n
    alg (TyFun a b) = a <> b
    alg _ = Set.empty

tyOccurs :: TyName -> Ty -> Bool
tyOccurs n ty = Set.member n (tyFtv ty)

