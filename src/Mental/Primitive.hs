{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Mental.Primitive where

import           Protolude

import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)

data Primitive
  = PIntPlus
  | PIntMinus
  | PIntMul
  | PIntDiv
  | PIntEq
  | PIntLess
  | PIntNeg
  | PFirst
  | PSecond
  | PFix
  deriving (Eq, Ord, Show, Read, Enum, Generic, Typeable)

intPrims :: [Primitive]
intPrims = [ PIntNeg
           , PIntLess
           , PIntEq
           , PIntDiv
           , PIntMul
           , PIntMinus
           , PIntPlus
           ]

isIntPrim :: Primitive -> Bool
isIntPrim p = p `elem` intPrims

