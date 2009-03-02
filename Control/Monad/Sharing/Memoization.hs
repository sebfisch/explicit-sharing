{-# LANGUAGE
     ExistentialQuantification,
     FlexibleContexts
  #-}

module Control.Monad.Sharing.Memoization (

  Untyped(..), typed,

  ThunkStore, emptyThunkStore, memo

 ) where

import qualified Data.IntMap as M
import Control.Monad.State
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

memo :: MonadState ThunkStore m => m a -> m (m a)
memo a = do
  key <- getFreshKey
  insertThunk key (Uneval a)
  return $ do
    thunk <- lookupThunk key
    case thunk of
      Eval x   -> return x
      Uneval b -> do
        x <- b
        insertThunk key (Eval x)
        return x
