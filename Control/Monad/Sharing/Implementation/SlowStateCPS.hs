{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, Rank2Types #-}

module Control.Monad.Sharing.Implementation.SlowStateCPS (

  Lazy, evalLazy

 ) where

import Control.Monad.State

import Control.Monad.Sharing.Classes
import Control.Monad.Sharing.Implementation.SlowState hiding ( Lazy, evalLazy )

newtype Lazy m a = Lazy {
  fromLazy :: forall w . (a -> ThunkStore -> m w) -> ThunkStore -> m w
 }

evalLazy :: (Monad m, Convertible (Lazy m) a b) => Lazy m a -> m b
evalLazy m = runLazy (m >>= convert)

runLazy :: Monad m => Lazy m a -> m a
runLazy m = fromLazy m (\a _ -> return a) emptyThunks

instance Monad m => Monad (Lazy m)
 where
  return x = Lazy (\c -> c x)
  a >>= k  = Lazy (\c s -> fromLazy a (\x -> fromLazy (k x) c) s)
  fail err = Lazy (\_ _ -> fail err)

instance MonadPlus m => MonadPlus (Lazy m)
 where
  mzero       = Lazy (\_ _ -> mzero)
  a `mplus` b = Lazy (\c s -> fromLazy a c s `mplus` fromLazy b c s)

instance Monad m => MonadState ThunkStore (Lazy m)
 where
  get   = Lazy (\c s -> c s s)
  put s = Lazy (\c _ -> c () s)

instance MonadTrans Lazy
 where
  lift a = Lazy (\c s -> a >>= flip c s)

instance MonadIO m => MonadIO (Lazy m)
 where
  liftIO = lift . liftIO

instance Monad m => Sharing (Lazy m)
 where
  share a = memo (a >>= shareArgs share)

memo :: MonadState ThunkStore m => m a -> m (m a)
memo a = do key <- getFreshKey
            insertThunk key (Uneval a)
            return $ do thunk <- lookupThunk key
                        case thunk of
                          Eval x   -> return x
                          Uneval b -> do x <- b
                                         insertThunk key (Eval x)
                                         return x
