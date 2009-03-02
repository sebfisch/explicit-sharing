{-# LANGUAGE
     MultiParamTypeClasses,
     Rank2Types
  #-}

module Control.Monad.Sharing where

import Control.Monad

class MonadPlus m => Nondet m a
 where
  mapNondet :: (forall b . Nondet m b => m b -> m (m b)) -> a -> m a

eval :: Nondet m a => a -> m a
eval = mapNondet (\a -> a >>= eval >>= return . return)

class MonadPlus m => Sharing m
 where
  share :: Nondet m a => m a -> m (m a)

shareRec :: (Sharing m, Nondet m a) => (m a -> m a) -> m (m a)
shareRec f = let x = share (f (x >>= id)) in x

