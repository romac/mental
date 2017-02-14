{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mental.Decl where

import           Protolude
import           Mental.Tree
import           Mental.Type

import           Unbound.Generics.LocallyNameless

type ModuleName = Name Module

data Module
  = Module
  { _modName :: ModuleName
  , _modDecls :: [Decl]
  }
  deriving (Show, Generic, Typeable)

data Decl
  = FunDecl !VarName !(Maybe Ty) !(Bind VarName Tree)
  | TyDecl  !TyName !(Bind TyName Ty)
  deriving (Show, Generic, Typeable)

instance Alpha Decl

