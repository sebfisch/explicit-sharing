{-# LANGUAGE
     GeneralizedNewtypeDeriving, 
     MultiParamTypeClasses, 
     FlexibleContexts
  #-}

module Control.Monad.Sharing.Lazy.ContReaderNoThunks where

import Control.Monad.Trans.ContT
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Sharing
import Control.Monad.Sharing.Memoization
 ( Untyped(..), typed, Shared(..), shared )

import qualified Data.IntMap as M


data Store = Store Int (M.IntMap Untyped)

getFreshKey :: MonadState Store m => m Int
getFreshKey = do
  Store key heap <- get
  put (Store (succ key) heap)
  return key

lookupHNF :: MonadState Store m => Int -> m (Maybe a)
lookupHNF key = do
  Store _ heap <- get
  return (fmap typed (M.lookup key heap))

insertHNF :: MonadState Store m => Int -> a -> m ()
insertHNF key val = do
  Store next heap <- get
  put (Store next (M.insert key (Untyped val) heap))


type Env = (Store, Shared)

newtype Lazy m a = Lazy { fromLazy :: ContT (ReaderT Env m) a }
 deriving MonadPlus

runLazy :: Monad m => Lazy m a -> m a
runLazy m = runReaderT (runContT (fromLazy m)) (Store 1 M.empty, Shared False)


instance Monad m => Monad (Lazy m)
 where
  return x = Lazy (return x)
  m >>= k  = Lazy (do x <- fromLazy m
                      modify (\ (ts,_) -> (ts, Shared False))
                      fromLazy (k x))

instance Monad m => MonadState Store (Lazy m)
 where
  get    = Lazy (liftM fst get)
  put ts = Lazy (modify (\ (_,s) -> (ts,s)))

instance Monad m => MonadWriter Shared (Lazy m)
 where
  tell s   = Lazy (modify (\ (ts,_) -> (ts,s)))
  listen m = Lazy (do x <- fromLazy m
                      s <- gets snd
                      return (x,s))
  pass m   = Lazy (do (x,f) <- fromLazy m
                      (ts,s) <- get
                      put (ts,f s)
                      return x)

instance MonadPlus m => Sharing (Lazy m)
 where
  share a = memo $ do (x,s) <- listen a
                      if isShared s then shared (return x)
                       else mapNondet share x

memo :: (MonadState Store m, MonadWriter Shared m) => m a -> m (m a)
memo a = do
  key <- getFreshKey
  return . shared $ do
    thunk <- lookupHNF key
    case thunk of
      Just x  -> return x
      Nothing -> do
        x <- a
        insertHNF key x
        return x
