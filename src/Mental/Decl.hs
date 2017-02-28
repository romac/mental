{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mental.Decl
  ( Module(..)
  , Decl(..)
  , UntypedModule
  , TypedModule
  , UntypedDecl
  , TypedDecl
  ) where

import           Protolude

import           Text.Megaparsec (SourcePos)

import           Mental.Name
import           Mental.Tree
import           Mental.Type

data Module a
  = Module
  { _modName  :: ModuleName
  , _modDecls :: [Decl a]
  }
  deriving (Eq, Ord, Show, Read, Generic, Typeable)

type UntypedModule = Module SourcePos
type TypedModule   = Module Ty

data Decl a
  = FunDecl !VarName !(Maybe Ty) !(AnnTree a)
  | TyDecl  !TyName !Ty
  deriving (Eq, Ord, Show, Read, Generic, Typeable)

type UntypedDecl = Decl SourcePos
type TypedDecl   = Decl Ty

