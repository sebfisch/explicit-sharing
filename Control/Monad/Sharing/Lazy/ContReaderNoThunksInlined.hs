{-# OPTIONS -fno-warn-name-shadowing #-}
{-# LANGUAGE
     MultiParamTypeClasses,
     Rank2Types
  #-}

module Control.Monad.Sharing.Lazy.ContReaderNoThunksInlined where

import Control.Monad.Sharing
import Control.Monad.Sharing.Memoization ( Untyped(..), typed )

import qualified Data.IntMap as M

newtype Lazy m a = Lazy {
  fromLazy :: forall w . (a -> Bool -> Store -> m w) -> Bool -> Store -> m w
 }

data Store = Store Int (M.IntMap Untyped)

runLazy :: Monad m => Lazy m a -> m a
runLazy m = fromLazy m (\x _ _ -> return x) False (Store 1 M.empty)

instance Monad m => Monad (Lazy m)
 where
  return x = Lazy (\c -> c x)
  a >>=  k = Lazy (\c -> fromLazy a (\x _ -> fromLazy (k x) c False))
  fail str = Lazy (\_ _ _ -> fail str)

instance MonadPlus m => MonadPlus (Lazy m)
 where
  mzero = Lazy (\_ _ _ -> mzero)

  x `mplus` y = Lazy (\c b s -> fromLazy x c b s `mplus` fromLazy y c b s)

instance MonadPlus m => Sharing (Lazy m)
 where
  share a = memo (Lazy (\c -> 
            fromLazy a (\x b -> 
            if b then c x True
                 else fromLazy (mapNondet share x) c False)))

memo :: Lazy m a -> Lazy m (Lazy m a)
memo a = Lazy (\c b (Store key heap) ->
      c (Lazy (\c b s@(Store _ heap) -> 
         case M.lookup key heap of
          Just x  -> c (typed x) True s
          Nothing -> fromLazy a
           (\x _ (Store other heap) -> 
              c x True (Store other (M.insert key (Untyped x) heap))) b s))
        b (Store (succ key) heap))

