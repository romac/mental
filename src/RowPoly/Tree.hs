{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RowPoly.Tree
  ( VarName
  , Tree(..)
  ) where

import           Protolude

import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)
import           Unbound.Generics.LocallyNameless

import           RowPoly.Type

type VarName = Name Tree

data Tree
  = Tru
  | Fals
  | Zero
  | Succ Tree
  | Pred Tree
  | IsZero Tree
  | If Tree Tree Tree
  | Var VarName
  | Abs (Maybe Ty) (Bind VarName Tree)
  | App Tree Tree
  | Let (Maybe Ty) Tree (Bind VarName Tree)
  deriving (Show, Generic, Typeable)

instance Alpha Tree

instance Subst Tree Tree where
  isvar (Var x) = Just (SubstName x)
  isvar _       = Nothing

instance Subst Ty Tree where
  isvar _ = Nothing

instance Subst Tree Ty where
  isvar _ = Nothing

