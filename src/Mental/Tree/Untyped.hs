
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

-- mkVar :: VarName -> UntypedTree
-- mkVar = embed . Var

-- mkTrue :: UntypedTree
-- mkTrue = embed Tru

-- mkFalse :: UntypedTree
-- mkFalse = embed Fals

-- mkIntLit :: Int -> UntypedTree
-- mkIntLit = embed . IntLit

-- mkPair :: UntypedTree -> UntypedTree -> UntypedTree
-- mkPair a b = embed (Pair a b)

-- mkLet :: VarName -> Maybe Ty -> UntypedTree -> UntypedTree -> UntypedTree
-- mkLet x ty val body = embed (Let x ty val body)

-- mkAbs :: Maybe Ty -> VarName -> UntypedTree -> UntypedTree
-- mkAbs ty x body = embed (Abs ty (x, body))

-- mkApp :: UntypedTree -> UntypedTree -> UntypedTree
-- mkApp f x = embed (App f x)

-- mkIf :: UntypedTree -> UntypedTree -> UntypedTree -> UntypedTree
-- mkIf c t e = embed (If c t e)
