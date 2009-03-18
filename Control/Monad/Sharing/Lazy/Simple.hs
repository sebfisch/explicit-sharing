{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module Control.Monad.Sharing.Lazy.Simple where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Sharing
import Control.Monad.Sharing.Memoization

newtype Lazy m a = Lazy {
  fromLazy :: WriterT Shared (StateT ThunkStore m) a
 } deriving (Monad, MonadPlus, MonadState ThunkStore, MonadWriter Shared)

runLazy :: Monad m => Lazy m a -> m a
runLazy m = do ((x,_),_) <- runStateT (runWriterT (fromLazy m)) emptyThunkStore
               return x

instance MonadPlus m => Sharing (Lazy m)
 where
  share a = memo $ do (x,s) <- listen a
                      if isShared s then shared (return x)
                       else mapNondet share x

