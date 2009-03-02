module Control.Monad.Sharing.Lazy.ContReaderNoThunks where

import Control.Monad.Trans.ContT
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Sharing
import Control.Monad.Sharing.Memoization ( Untyped(..), typed )

import qualified Data.IntMap as M

newtype Lazy m a = Lazy { fromLazy :: ContT (ReaderT Store m) a }
 deriving (Monad, MonadPlus, MonadState Store)


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

runLazy :: Monad m => Lazy m a -> m a
runLazy m = runReaderT (runContT (fromLazy m)) (Store 1 M.empty)

instance MonadPlus m => Sharing (Lazy m)
 where
  share a = memo (a >>= mapNondet share)

memo :: MonadState Store m => m a -> m (m a)
memo a = do
  key <- getFreshKey
  return $ do
    thunk <- lookupHNF key
    case thunk of
      Just x  -> return x
      Nothing -> do
        x <- a
        insertHNF key x
        return x
