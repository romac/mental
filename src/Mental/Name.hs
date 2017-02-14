{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Mental.Name
  ( mkName
  , mkNameStr
  , nameText
  , TyName
  , VarName
  , ModuleName
  ) where

import           Protolude

import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)

import qualified Data.Text as T

newtype Name = Name { nameText :: Text }
  deriving (Eq, Ord, Show, Read, Generic, Typeable)

mkName :: Text -> Name
mkName = Name

mkNameStr :: [Char] -> Name
mkNameStr = mkName . T.pack

type TyName     = Name
type VarName    = Name
type ModuleName = Name
