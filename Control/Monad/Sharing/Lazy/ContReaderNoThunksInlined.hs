{-# OPTIONS -fno-warn-name-shadowing #-}
{-# LANGUAGE
     MultiParamTypeClasses,
     Rank2Types
  #-}

module Control.Monad.Sharing.Lazy.ContReaderNoThunksInlined where

import Control.Monad.State
import Control.Monad.Sharing
import Control.Monad.Sharing.Memoization ( Untyped(..), typed )

import qualified Data.IntMap as M

newtype Lazy m a = Lazy {
  fromLazy :: forall w . (a -> Store -> m w) -> Store -> m w
 }

data Store = Store Int (M.IntMap Untyped)

runLazy :: Monad m => Lazy m a -> m a
runLazy m = fromLazy m (\a _ -> return a) (Store 1 M.empty)

instance Monad m => Monad (Lazy m)
 where
  return x = Lazy (\c -> c x)
  a >>=  k = Lazy (\c s -> fromLazy a (\x -> fromLazy (k x) c) s)
  fail str = Lazy (\_ _ -> fail str)

instance MonadPlus m => MonadPlus (Lazy m)
 where
  mzero = Lazy (\_ _ -> mzero)

  a `mplus` b = Lazy (\c s -> fromLazy a c s `mplus` fromLazy b c s)

instance Monad m => MonadState Store (Lazy m)
 where
  get   = Lazy (\c s -> c s s)
  put s = Lazy (\c _ -> c () s)

instance MonadPlus m => Sharing (Lazy m)
 where
  share a = memo (a >>= mapNondet share)

memo :: Lazy m a -> Lazy m (Lazy m a)
memo a = Lazy (\c (Store key heap) ->
      c (Lazy (\c s@(Store _ heap) -> 
         case M.lookup key heap of
          Just x  -> c (typed x) s
          Nothing -> fromLazy a
           (\x (Store other heap) -> 
              c x (Store other (M.insert key (Untyped x) heap))) s))
        (Store (succ key) heap))

