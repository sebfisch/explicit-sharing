module Control.Monad.Sharing.Lazy.ContReader where

import Control.Monad.Trans.ContT
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Sharing
import Control.Monad.Sharing.Memoization

newtype Lazy m a = Lazy { fromLazy :: ContT (ReaderT ThunkStore m) a }
 deriving (Monad, MonadPlus, MonadState ThunkStore)

runLazy :: Monad m => Lazy m a -> m a
runLazy m = runReaderT (runContT (fromLazy m)) emptyThunkStore

instance MonadPlus m => Sharing (Lazy m)
 where
  share a = memo (a >>= mapNondet share)

