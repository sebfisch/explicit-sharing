{-# LANGUAGE ExistentialQuantification, 
             MultiParamTypeClasses,
             FlexibleContexts,
             Rank2Types 
  #-}

{-# OPTIONS -fno-warn-name-shadowing #-}

-- | Module      : Control.Monad.Sharing.Lazy
-- | Copyright   : Sebastian Fischer
-- | License     : PublicDomain
-- |
-- | Maintainer  : Sebastian Fischer (sebf@informatik.uni-kiel.de)
-- | Stability   : experimental
-- |
-- | Implements explicit sharing by passing a heap using a state monad
-- | implemented by a combination of a continuation- with a reader
-- | monad. The definitions are inlined and hand-optimized to increase
-- | performance.
module Control.Monad.Sharing.Lazy (

  module Control.Monad.Sharing,

  Lazy, evalLazy

 ) where

import Control.Monad.Trans
import Control.Monad.Sharing

-- For fast and easy implementation of typed stores..
import Unsafe.Coerce

import qualified Data.IntMap as M

-- | Continuation-based, store-passing implementation of explicit
-- | sharing. It is an inlined version of @ContT (ReaderT Store m)@
-- | where the result type of continuations is polymorphic.
newtype Lazy m a = Lazy {

  -- | Runs a computation of type @Lazy m a@ with given continuation
  -- | and store.
  fromLazy :: forall w . (a -> Store -> m w) -> Store -> m w
 }

-- | Lifts all monadic effects to the top-level and unwraps the monad
-- | transformer for explicit sharing.
evalLazy :: (Monad m, Trans (Lazy m) a b) => Lazy m a -> m b
evalLazy m = runLazy (m >>= eval)

-- private declarations

runLazy :: Monad m => Lazy m a -> m a
runLazy m = fromLazy m (\a _ -> return a) (Store 1 M.empty)

-- Stores consist of a fresh-reference counter and a heap represented
-- as IntMap.
data Store = Store Int (M.IntMap Untyped)

-- The monad instance is an inlined version of the instances for
-- continuation and reader monads.
instance Monad m => Monad (Lazy m)
 where
  return x = Lazy (\c -> c x)
  a >>= k  = Lazy (\c s -> fromLazy a (\x -> fromLazy (k x) c) s)
  fail err = Lazy (\_ _ -> fail err)

-- The @MonadPlus@ instance reuses corresponding operations of the
-- base monad.
instance MonadPlus m => MonadPlus (Lazy m)
 where
  mzero       = Lazy (\_ _ -> mzero)
  a `mplus` b = Lazy (\c s -> fromLazy a c s `mplus` fromLazy b c s)

-- @Lazy@ is a monad transformer.
instance MonadTrans Lazy
 where
  lift a = Lazy (\c s -> a >>= \x -> c x s)

-- If the underlying monad supports IO we can lift this functionality.
instance MonadIO m => MonadIO (Lazy m)
 where
  liftIO = lift . liftIO

-- The @Sharing@ instance memoizes nested monadic values recursively.
instance Monad m => Sharing (Lazy m)
 where
  share = lazy

-- The more general type is necessary to please the type checker.
lazy :: (Monad m, Trans (Lazy m) a b) => Lazy m a -> Lazy m (Lazy m b)
lazy a = memo (a >>= trans lazy)

-- This is an inlined version of the following definition:
-- 
-- > memo :: MonadState Store m => m a -> m (m a)
-- > memo a = do key <- getFreshKey
-- >             return $ do thunk <- lookupHNF key
-- >                         case thunk of
-- >                           Just x  -> return x
-- >                           Nothing -> do x <- a
-- >                                         insertHNF key x
-- >                                         return x
--
memo :: Lazy m a -> Lazy m (Lazy m a)
memo a = Lazy (\c (Store key heap) ->
      c (Lazy (\c s@(Store _ heap) -> 
         case M.lookup key heap of
          Just x  -> c (typed x) s
          Nothing -> fromLazy a
           (\x (Store other heap) -> 
              c x (Store other (M.insert key (Untyped x) heap))) s))
        (Store (succ key) heap))

-- Easy and fast hack to store typed data. An implementation using
-- Data.Typeable is possible but clutters the code with additional
-- class constraints.
data Untyped = forall a . Untyped a

typed :: Untyped -> a
typed (Untyped x) = unsafeCoerce x
