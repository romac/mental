{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Mental.Primitive where

import Protolude

import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)

import           Unbound.Generics.LocallyNameless

data Primitive
  = Succ
  | Pred
  | IsZero
  | First
  | Second
  | Fix
  deriving (Eq, Ord, Show, Generic, Typeable)

instance Alpha Primitive

