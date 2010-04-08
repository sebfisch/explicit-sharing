{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Control.Monad.Sharing.Implementation.SlowState (

  Lazy, evalLazy,

  ThunkStore, Thunk(..), emptyThunks, getFreshKey, lookupThunk, insertThunk

 ) where

import Data.Maybe ( fromJust )
import Control.Monad.State

import qualified Data.IntMap as M

import Control.Monad.Sharing.Classes
import Control.Monad.Sharing.Implementation.CPS ( Untyped(..), typed )

type Lazy m = StateT ThunkStore m

evalLazy :: (Monad m, Convertible (Lazy m) a b) => Lazy m a -> m b
evalLazy m = evalStateT (m >>= convert) emptyThunks

instance Monad m => Sharing (StateT ThunkStore m)
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

data ThunkStore = ThunkStore { nextLabel :: Int, heap :: M.IntMap Untyped }

data Thunk m a = Uneval (m a) | Eval a

emptyThunks :: ThunkStore
emptyThunks = ThunkStore 1 M.empty

getFreshKey :: MonadState ThunkStore m => m Int
getFreshKey = do s <- get
                 put (s { nextLabel = nextLabel s + 1 })
                 return (nextLabel s)

lookupThunk :: MonadState ThunkStore m => Int -> m (Thunk m a)
lookupThunk k = gets (typed . fromJust . M.lookup k . heap)

insertThunk :: MonadState ThunkStore m => Int -> a -> m ()
insertThunk k v = modify (\s -> s { heap = M.insert k (Untyped v) (heap s) })

