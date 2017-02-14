{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mental.Decl where

import           Protolude

import           Mental.Name
import           Mental.Tree
import           Mental.Type

data Module
  = Module
  { _modName  :: ModuleName
  , _modDecls :: [Decl]
  }
  deriving (Eq, Ord, Show, Read, Generic, Typeable)

data Decl
  = FunDecl !VarName !(Maybe Ty) !Tree
  | TyDecl  !TyName !Ty
  deriving (Eq, Ord, Show, Read, Generic, Typeable)

