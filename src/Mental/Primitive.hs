{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Mental.Primitive where

import           Protolude

import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)

data Primitive
  = PFirst
  | PSecond
  | PFix
  deriving (Eq, Ord, Show, Read, Enum, Generic, Typeable)

