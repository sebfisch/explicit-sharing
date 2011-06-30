{-# LANGUAGE ExistentialQuantification,
             MultiParamTypeClasses,
             FlexibleContexts,
             Rank2Types
  #-}

{-# OPTIONS -fno-warn-name-shadowing #-}

-- | 
-- Module      : Control.Monad.Sharing.Implementation.CPS
-- Copyright   : Chung-chieh Shan, Oleg Kiselyov, and Sebastian Fischer
-- License     : PublicDomain
-- Maintainer  : Sebastian Fischer <mailto:sebf@informatik.uni-kiel.de>
-- Stability   : experimental
-- 
-- Implements explicit sharing by passing a heap using a state monad
-- implemented by a combination of a continuation- with a reader
-- monad. The definitions are inlined and hand-optimized to increase
-- performance.
module Control.Monad.Sharing.Implementation.CPS (

  collect,

  Store, emptyStore, freshLabel, lookupValue, storeValue,

  Untyped(..), typed

 ) where

import Control.Monad       ( MonadPlus(..) )
import Control.Monad.State ( MonadState(..), gets, modify )

import Control.Monad.Sharing.Classes

-- For fast and easy implementation of typed stores..
import Unsafe.Coerce

import qualified Data.IntMap as M

-- import Debug.Trace ( trace )

-- |
-- Continuation-based, store-passing implementation of explicit
-- sharing. It is an inlined version of @ContT n (Reader Store)@.
newtype Lazy n a = Lazy {

  -- |
  -- Runs a computation of type @Lazy n a@ with given continuation and
  -- store.
  fromLazy :: (a -> Store -> n) -> Store -> n
 }

collect :: Nondet n => (forall s. Sharing s => s n) -> n
collect a = runLazy a

-- private declarations

runLazy :: Nondet n => Lazy n n -> n
runLazy m = fromLazy m (\a _ -> a) emptyStore
--   fromLazy m
--     (\a s -> trace ("used refs: "++show (nextLabel s-1)) a)
--     emptyStore

-- Stores consist of a fresh-reference counter and a heap represented
-- as IntMap.
data Store = Store { nextLabel :: Int, heap :: M.IntMap Untyped }

emptyStore :: Store
emptyStore = Store 1 M.empty

freshLabel :: MonadState Store m => m Int
freshLabel = do s <- get
                put (s { nextLabel = nextLabel s + 1 })
                return (nextLabel s)

lookupValue :: MonadState Store m => Int -> m (Maybe a)
lookupValue k = gets (fmap typed . M.lookup k . heap)

storeValue :: MonadState Store m => Int -> a -> m ()
storeValue k v = modify (\s -> s { heap = M.insert k (Untyped v) (heap s) })

-- The monad instance is an inlined version of the instances for
-- continuation and reader monads.
instance Nondet n => Monad (Lazy n)
 where
  return x = Lazy (\c -> c x)
  a >>= k  = Lazy (\c s -> fromLazy a (\x -> fromLazy (k x) c) s)
  fail _   = Lazy (\_ _ -> failure)

-- The 'MonadPlus' instance reuses corresponding operations of the
-- underlying 'Nondet' instance.
instance Nondet n => MonadPlus (Lazy n)
 where
  mzero       = Lazy (\_ _ -> failure)
  a `mplus` b = Lazy (\c s -> fromLazy a c s ? fromLazy b c s)

-- A Cont/Reader monad is an instance of MonadState
instance Nondet n => MonadState Store (Lazy n)
 where
  get   = Lazy (\c s -> c s s)
  put s = Lazy (\c _ -> c () s)

instance Nondet n => Sharing (Lazy n)
 where
  share a = memo (a >>= shareArgs share)

-- This is an inlined version of the following definition:

-- memo :: MonadState Store m => m a -> m (m a)
-- memo a = do key <- freshLabel
--             return $ do thunk <- lookupValue key
--                         case thunk of
--                           Just x  -> return x
--                           Nothing -> do x <- a
--                                         storeValue key x
--                                         return x

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
