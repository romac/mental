{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Mental.Tree
  ( Tree(..)
  , isValue
  , isNumericValue
  , pattern IsValue
  , pattern IsNumericValue
  , pattern PrimApp
  ) where

import           Protolude

import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)

import           Mental.Name
import           Mental.Primitive
import           Mental.Type

data Tree
  = Tru
  | Fals
  | Zero
  | If !Tree !Tree !Tree
  | Var !VarName
  | Abs !(Maybe Ty) !VarName !Tree
  | App !Tree !Tree
  | Let !(Maybe Ty) !Tree !VarName !Tree -- FIXME
  | Pair !Tree !Tree
  | Inl !Tree !Ty
  | Inr !Tree !Ty
  | Case !Tree !VarName !Tree !VarName !Tree -- FIXME
  | Prim !Primitive
  deriving (Eq, Ord, Show, Read, Generic, Typeable)

isValue :: Tree -> Bool
isValue Tru         = True
isValue Fals        = True
isValue (Abs _ _ _) = True
isValue (Prim _)    = True
isValue (Pair a b)  = isValue a && isValue b
isValue (Inl t _)   = isValue t
isValue (Inr t _)   = isValue t
isValue v           = isNumericValue v

isNumericValue :: Tree -> Bool
isNumericValue Zero     = True
isNumericValue (App (Prim Succ) t) = isNumericValue t
isNumericValue _        = False

pattern IsValue :: Tree
pattern IsValue <- (isValue -> True)

pattern IsNumericValue :: Tree
pattern IsNumericValue <- (isNumericValue -> True)

pattern PrimApp :: Primitive -> Tree -> Tree
pattern PrimApp prim t = App (Prim prim) t

