{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mental.Fresh
  ( FreshT(..)
  , Fresh
  , runFreshT
  , runFresh
  , MonadFresh
  , freshName
  ) where

import           Protolude

import           Control.Monad.Trans.Class
import           Control.Monad.Writer.Strict (WriterT)
import           Control.Monad.RWS.Strict (RWST)

import           Mental.Name

newtype FreshT m a = FreshT (StateT [[Char]] m a)
  deriving (Functor, Applicative, Monad, MonadState [[Char]], MonadTrans)

type Fresh = FreshT Identity

alphabet :: [[Char]]
alphabet = (:[]) <$> ['a'..'z']

supply :: [[Char]] -> [[Char]]
supply syms = syms <> go syms 1
  where
    go :: [[Char]] -> Int -> [[Char]]
    go s n = ((<> show n) <$> s) <> go s (n + 1)

runFreshT :: Monad m => FreshT m a -> m a
runFreshT (FreshT st) = evalStateT st (supply alphabet)

runFresh :: Fresh a -> a
runFresh = runIdentity . runFreshT

class Monad m => MonadFresh m where
  freshName :: m VarName

instance Monad m => MonadFresh (FreshT m) where
  freshName = do
    n : ns <- get
    put ns
    pure $ mkNameStr n

instance MonadFresh m => MonadFresh (ExceptT e m) where
  freshName = lift freshName

instance MonadFresh m => MonadFresh (ReaderT r m) where
  freshName = lift freshName

instance (Monoid w, MonadFresh m) => MonadFresh (WriterT w m) where
  freshName = lift freshName

instance (Monoid w, MonadFresh m) => MonadFresh (RWST r w s m) where
  freshName = lift freshName
