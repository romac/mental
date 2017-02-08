{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RowPoly.Type
  ( TyName
  , Ty(..)
  ) where

import           Protolude

import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)
import           Unbound.Generics.LocallyNameless

type TyName = Name Ty

data Ty
  = TyVar TyName
  | TyFun Ty Ty
  | TyNat
  | TyBool
  deriving (Eq, Ord, Show, Generic, Typeable)

instance Alpha Ty

instance Subst Ty Ty where
  isvar (TyVar x) = Just (SubstName x)
  isvar _         = Nothing

