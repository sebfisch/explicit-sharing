{-# LANGUAGE
     ExistentialQuantification,
     FlexibleContexts
  #-}

module Control.Monad.Sharing.Memoization (

  Untyped(..), typed,

  ThunkStore(..), emptyThunkStore, Shared(..), shared, memo

 ) where

import qualified Data.IntMap as M

import Data.Monoid ()

import Control.Monad.State
import Control.Monad.Writer

import Unsafe.Coerce

data Untyped = forall a . Untyped a

typed :: Untyped -> a
typed (Untyped x) = unsafeCoerce x


data ThunkStore = ThunkStore { freshKey :: Int, thunks :: M.IntMap Untyped }

data Thunk m a = Uneval (m a) | Eval !a

emptyThunkStore :: ThunkStore
emptyThunkStore = ThunkStore { freshKey = 1, thunks = M.empty }

getFreshKey :: MonadState ThunkStore m => m Int
getFreshKey = do
  key <- gets freshKey
  modify (\s -> s { freshKey = succ key })
  return key

insertThunk :: MonadState ThunkStore m => Int -> Thunk m a -> m ()
insertThunk key thunk =
  modify (\s -> s { thunks = M.insert key (Untyped thunk) (thunks s) })

lookupThunk :: MonadState ThunkStore m => Int -> m (Thunk m a)
lookupThunk key = liftM (typed . M.findWithDefault err key) (gets thunks)
 where err = error $ "lookupThunk: unbound key " ++ show key


newtype Shared = Shared { isShared :: Bool }

instance Monoid Shared
 where mempty  = Shared False
       mappend = flip const

shared :: MonadWriter Shared m => m a -> m a
shared = censor (const (Shared True))

memo :: (MonadState ThunkStore m, MonadWriter Shared m) => m a -> m (m a)
memo a = do
  key <- getFreshKey
  insertThunk key (Uneval a)
  return . shared $ do
    thunk <- lookupThunk key
    case thunk of
      Eval x   -> return x
      Uneval b -> do
        x <- b
        insertThunk key (Eval x)
        return x
