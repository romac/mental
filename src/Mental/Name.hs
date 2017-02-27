{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Mental.Name
  ( mkName
  , mkNameStr
  , nameText
  , nameTextLazy
  , TyName
  , VarName
  , ModuleName
  ) where

import           Protolude

import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)

import qualified Data.Text      as T
import qualified Data.Text.Lazy as L

newtype Name = Name { nameText :: Text }
  deriving (Eq, Ord, Show, Read, Generic, Typeable)

mkName :: Text -> Name
mkName = Name

mkNameStr :: [Char] -> Name
mkNameStr = mkName . T.pack

nameTextLazy :: Name -> L.Text
nameTextLazy = L.fromStrict . nameText

type TyName     = Name
type VarName    = Name
type ModuleName = Name
