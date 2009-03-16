{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module Control.Monad.Sharing.Lazy.ContReader where

import Control.Monad.Trans.ContT
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Sharing
import Control.Monad.Sharing.Memoization

type Env = (ThunkStore, Shared)

newtype Lazy m a = Lazy { fromLazy :: ContT (ReaderT Env m) a }
 deriving MonadPlus

instance Monad m => Monad (Lazy m)
 where
  return x = Lazy (return x)
  m >>= k  = Lazy (do x <- fromLazy m
                      modify (\ (ts,_) -> (ts,Shared False))
                      fromLazy (k x))

instance Monad m => MonadState ThunkStore (Lazy m)
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

runLazy :: Monad m => Lazy m a -> m a
runLazy m = runReaderT (runContT (fromLazy m)) (emptyThunkStore, Shared False)

instance MonadPlus m => Sharing (Lazy m)
 where
  share a = memo $ do (x,s) <- listen a
                      if isShared s then shared (return x)
                       else mapNondet share x
