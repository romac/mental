{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RowPoly.Type where

import Data.Typeable (Typeable)
import GHC.Generics  (Generic)
import Unbound.Generics.LocallyNameless

type TyName = Name Type

data Type
  = TyVar TyName
  | TyFun Type Type
  | TyNat
  | TyBool
  deriving (Eq, Ord, Show, Generic, Typeable)

instance Alpha Type

instance Subst Type Type where
  isvar (TyVar x) = Just (SubstName x)
  isvar _         = Nothing

