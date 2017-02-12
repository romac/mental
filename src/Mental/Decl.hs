{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mental.Decl where

import           Protolude
import           Mental.Tree
import           Mental.Type

import           Unbound.Generics.LocallyNameless

data Module
  = Module
  { progDecls :: [Decl]
  }
  deriving (Show)

data Decl
  = FunDecl !VarName !(Maybe Ty) !(Bind VarName Tree)
  | TyDecl  !TyName !(Bind TyName Ty)
  deriving (Show, Generic, Typeable)

instance Alpha Decl

