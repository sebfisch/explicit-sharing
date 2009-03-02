module Control.Monad.Sharing.Lazy.State where

import Control.Monad.State
import Control.Monad.Sharing
import Control.Monad.Sharing.Memoization

newtype Lazy m a = Lazy { fromLazy :: StateT ThunkStore m a }
 deriving (Monad, MonadPlus, MonadState ThunkStore)

runLazy :: Monad m => Lazy m a -> m a
runLazy m = evalStateT (fromLazy m) emptyThunkStore

instance MonadPlus m => Sharing (Lazy m)
 where
  share a = memo (a >>= mapNondet share)

