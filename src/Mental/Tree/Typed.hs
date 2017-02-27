
module Mental.Tree.Typed
  ( TypedTree
  , getType
  ) where

import           Control.Comonad.Cofree (Cofree(..))

import           Mental.Tree
import           Mental.Type

type TypedTree = AnnTree Ty

getType :: TypedTree -> Ty
getType (ty :< _) = ty
