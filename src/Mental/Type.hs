{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mental.Type
  ( Ty(..)
  , tyOccurs
  , tyFtv
  ) where

import           Protolude

import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)

import           Mental.Primitive
import           Mental.Name

data Ty
  = TyVar !TyName
  | TyFun !Ty !Ty
  | TyNat
  | TyBool
  | TyPair !Ty !Ty
  | TySum !Ty !Ty
  deriving (Eq, Ord, Show, Read, Generic, Typeable)

tyFtv :: Ty -> Set TyName
tyFtv ty = undefined

tyOccurs :: TyName -> Ty -> Bool
tyOccurs n ty = Set.member n (tyFtv ty)

