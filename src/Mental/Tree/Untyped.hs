
module Mental.Tree.Untyped
  ( UntypedTree
  , getPos
  -- , mkTrue
  -- , mkFalse
  -- , mkIntLit
  -- , mkVar
  -- , mkIf
  -- , mkAbs
  -- , mkLet
  -- , mkPair
  -- , mkPrim
  -- , mkApp
  ) where

import           Control.Comonad.Cofree (Cofree(..))
import           Text.Megaparsec (SourcePos)

import           Mental.Tree

type UntypedTree = AnnTree SourcePos

getPos :: UntypedTree -> SourcePos
getPos (pos :< _) = pos

