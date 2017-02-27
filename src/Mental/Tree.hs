{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Mental.Tree
  ( TreeF(..)
  , Tree
  , AnnTree
  , unAnnotateTree
  , isValue
  , treeFv
  , annTreeFv
  , pattern IsValue
  -- , pattern PrimApp
  ) where

import           Protolude

import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)
import           Data.Deriving

import qualified Control.Comonad.Cofree as Cofree
import           Control.Comonad.Cofree (Cofree)
import           Control.Comonad.Trans.Cofree (CofreeF(..))
import qualified Control.Comonad.Trans.Cofree as CofreeF
import           Data.Functor.Foldable (cata, embed, Base, Corecursive, Fix, project)
import qualified Data.Set as Set
import           Data.Set ((\\))

import           Mental.Name
import           Mental.Primitive
import           Mental.Type

data TreeF a
  = Tru
  | Fals
  | IntLit Int
  | Var !VarName
  | If a a a
  | Abs !(Maybe Ty) !(VarName, a)
  | App a a
  | Let !VarName !(Maybe Ty) a a
  | Pair a a
  | Prim !Primitive
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Generic, Typeable)

$(deriveEq1   ''TreeF)
$(deriveOrd1  ''TreeF)
$(deriveShow1 ''TreeF)
$(deriveRead1 ''TreeF)

type AnnTree = Cofree TreeF
type Tree    = Fix TreeF

unCofree :: Corecursive t => Cofree (Base t) a -> t
unCofree (_ Cofree.:< f) = embed (unCofree <$> f)

unAnnotateTree :: AnnTree a -> Tree
unAnnotateTree = unCofree

annTreeFv :: AnnTree a -> Set VarName
annTreeFv = treeFv . unAnnotateTree

treeFv :: Tree -> Set VarName
treeFv = cata alg
  where
    alg :: TreeF (Set VarName) -> Set VarName
    alg (Var name)     = Set.singleton name
    alg (If a b c)     = a <> b <> c
    alg (Abs _ (x, v)) = v \\ Set.singleton x
    alg (App a b)      = a <> b
    alg (Let x _ v b)  = v <> (b \\ Set.singleton x)
    alg (Pair a b)     = a <> b
    alg _              = Set.empty

class IsValue t where
  isValue :: t -> Bool
  -- isNumericValue :: t -> Bool

instance IsValue (Cofree TreeF a) where
  isValue (_ Cofree.:< a) = isValue a

instance IsValue (Fix TreeF) where
  isValue (project -> a) = isValue a

instance IsValue a => IsValue (TreeF a) where
  isValue Tru        = True
  isValue Fals       = True
  isValue (IntLit _) = True
  isValue (Abs _ _)  = True
  isValue (Prim _)   = True
  isValue (Pair a b) = isValue a && isValue b
  isValue _          = False

pattern IsValue :: IsValue a => a
pattern IsValue <- (isValue -> True)

-- pattern PrimApp :: Primitive -> TreeF a -> TreeF a
-- pattern PrimApp prim t = App (Prim prim) t

