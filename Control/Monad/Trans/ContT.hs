{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, Rank2Types #-}

module Control.Monad.Trans.ContT where

import Control.Monad.State
import Control.Monad.Reader

newtype ContT m a = ContT { unContT :: forall w . (a -> m w) -> m w }

runContT :: Monad m => ContT m a -> m a
runContT m = unContT m return

instance MonadTrans ContT where
  lift m = ContT (\c -> m >>= c)

instance Monad m => Monad (ContT m) where
  return x = lift (return x)
  m >>=  k = ContT (\c -> unContT m (\x -> unContT (k x) c))
  fail str = lift (fail str)

instance MonadPlus m => MonadPlus (ContT m) where
  mzero       = lift mzero
  a `mplus` b = ContT (\c -> unContT a c `mplus` unContT b c)

instance Monad m => MonadState s (ContT (ReaderT s m)) where
  get   = lift ask
  put s = ContT (\c -> local (const s) (c ()))
