{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Mental.Annotate
  ( annotate
  , annotateM
  , annotateM'
  ) where

import Protolude

import Data.Functor.Foldable (Base, Recursive, project)
import Control.Comonad.Cofree (Cofree(..))
import Control.Comonad (extend)

annotate :: Recursive t
         => (t -> a)
         -> t
         -> Cofree (Base t) a
annotate alg t = alg t :< fmap (annotate alg) (project t)

annotateM :: (Traversable f, Monad m)
          => (Cofree f b -> m a)
          -> Cofree f b
          -> m (Cofree f a)
annotateM f x = sequence (extend f x)

annotateM' :: (Recursive t, Traversable (Base t), Monad m)
           => (Cofree (Base t) () -> m a)
           -> t
           -> m (Cofree (Base t) a)
annotateM' f x = annotateM f (annotate (const ()) x)
