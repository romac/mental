{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mental.NameSupply
  ( NameSupplyT(..)
  , NameSupply
  , runNameSupplyT
  , runNameSupply
  , MonadNameSupply
  , supplyName
  ) where

import           Protolude

import           Control.Monad.Trans.Class
import           Control.Monad.Writer.Strict (WriterT)
import           Control.Monad.RWS.Strict (RWST)

newtype NameSupplyT m a = NameSupplyT (StateT [[Char]] m a)
  deriving (Functor, Applicative, Monad, MonadState [[Char]], MonadTrans)

type NameSupply = NameSupplyT Identity

alphabet :: [[Char]]
alphabet = (:[]) <$> ['a'..'z']

supply :: [[Char]] -> [[Char]]
supply syms = syms <> go syms 1
  where
    go :: [[Char]] -> Int -> [[Char]]
    go s n = ((<> show n) <$> s) <> go s (n + 1)

runNameSupplyT :: Monad m => NameSupplyT m a -> m a
runNameSupplyT (NameSupplyT st) = evalStateT st (supply alphabet)

runNameSupply :: NameSupply a -> a
runNameSupply = runIdentity . runNameSupplyT

class Monad m => MonadNameSupply m where
  supplyName :: m [Char]

instance Monad m => MonadNameSupply (NameSupplyT m) where
  supplyName = do
    n : ns <- get
    put ns
    pure n

instance MonadNameSupply m => MonadNameSupply (ExceptT e m) where
  supplyName = lift supplyName

instance MonadNameSupply m => MonadNameSupply (ReaderT r m) where
  supplyName = lift supplyName

instance (Monoid w, MonadNameSupply m) => MonadNameSupply (WriterT w m) where
  supplyName = lift supplyName

instance (Monoid w, MonadNameSupply m) => MonadNameSupply (RWST r w s m) where
  supplyName = lift supplyName

