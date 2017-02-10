{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Mental.Tree
  ( VarName
  , Tree(..)
  , isValue
  , isNumericValue
  , pattern IsValue
  , pattern IsNumericValue
  ) where

import           Protolude

import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)
import           Unbound.Generics.LocallyNameless

import           Mental.Type

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
  | Fix Tree
  | Pair Tree Tree
  | First Tree
  | Second Tree
  | Inl Tree Ty
  | Inr Tree Ty
  | Case Tree (Bind VarName Tree) (Bind VarName Tree)
  deriving (Show, Generic, Typeable)

instance Alpha Tree

instance Subst Tree Tree where
  isvar (Var x) = Just (SubstName x)
  isvar _       = Nothing

instance Subst Ty Tree where
  isvar _ = Nothing

instance Subst Tree Ty where
  isvar _ = Nothing

isValue :: Tree -> Bool
isValue Tru         = True
isValue Fals        = True
isValue (Pair a b)  = isValue a && isValue b
isValue (Inl t _)   = isValue t
isValue (Inr t _)   = isValue t
isValue (Abs _ _)   = True
isValue v           = isNumericValue v

isNumericValue :: Tree -> Bool
isNumericValue Zero     = True
isNumericValue (Succ t) = isNumericValue t
isNumericValue _        = False

pattern IsValue :: Tree
pattern IsValue <- (isValue -> True)

pattern IsNumericValue :: Tree
pattern IsNumericValue <- (isNumericValue -> True)

