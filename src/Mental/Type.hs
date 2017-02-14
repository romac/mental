{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mental.Type
  ( TyName
  , Ty(..)
  , tyContains
  , ftvTy
  ) where

import           Protolude

import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)
import           Unbound.Generics.LocallyNameless
import           Unbound.Generics.LocallyNameless.Internal.Fold

import           Mental.Primitive

type TyName = Name Ty

data Ty
  = TyVar !TyName
  | TyFun !Ty !Ty
  | TyNat
  | TyBool
  | TyPair !Ty !Ty
  | TySum !Ty !Ty
  deriving (Eq, Ord, Show, Generic, Typeable)

instance Alpha Ty

instance Subst Ty Ty where
  isvar (TyVar x) = Just (SubstName x)
  isvar _         = Nothing

instance Subst Ty Primitive where
  isvar _ = Nothing

ftvTy :: Ty -> Set TyName
ftvTy ty = Set.fromList (toListOf fv ty)

tyContains :: Ty -> TyName -> Bool
tyContains ty n = Set.member n (ftvTy ty)

