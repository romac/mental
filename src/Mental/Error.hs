
module Mental.Error
  ( TyError(..)
  , EvalError(..)
  ) where

import           Protolude

import           Mental.Name
import           Mental.Tree
import           Mental.Type

data TyError
  = ValueNotFound     VarName
  | UnificationError  Ty Ty
  | InfiniteTypeError Ty Ty
  deriving (Eq, Ord, Show, Read)

data EvalError
  = NoRuleApplies Tree
  | VarNotInScope VarName
  deriving (Eq, Ord, Show, Read)

