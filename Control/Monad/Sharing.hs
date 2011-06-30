{-# LANGUAGE RankNTypes,
             FlexibleInstances
  #-}

-- | 
-- Module      : Control.Monad.Sharing
-- Copyright   : Chung-chieh Shan, Oleg Kiselyov, and Sebastian Fischer
-- License     : PublicDomain
-- Maintainer  : Sebastian Fischer <mailto:sebf@informatik.uni-kiel.de>
-- Stability   : experimental
-- 
-- This library provides an interface to monads that support explicit
-- sharing. A project website with tutorials can be found at
-- <http://sebfisch.github.com/explicit-sharing>.
module Control.Monad.Sharing (

  module Control.Monad,

  -- * Classes

  Sharing(..), Shareable(..), Convertible(..),

  -- * Observation functions

  collect, hasResult, results, resultDist,

  resultList, unsafeResults

 ) where

import Control.Monad
import Control.Monad.Sharing.Classes
import Control.Monad.Sharing.Implementation.CPS

import qualified Data.Set as Set
import qualified Data.Map as Map

hasResult :: (forall s . Sharing s => s a) -> Bool
hasResult a = collect (liftM (const True) a)

results :: Ord a => (forall s . Sharing s => s a) -> Set.Set a
results a = collect (liftM Set.singleton a)

resultDist :: Ord a => (forall s . Sharing s => s a) -> Map.Map a Rational
resultDist a = collect (liftM (`Map.singleton`1) a)

newtype UnsafeResults a = Unsafe { unsafe :: [a] }

instance Nondet (UnsafeResults a) where
  failure = Unsafe []

  -- does not satisfy required laws
  a ? b = Unsafe (unsafe a ++ unsafe b)

unsafeResults :: (forall s . Sharing s => s a) -> [a]
unsafeResults a = unsafe (collect (liftM (Unsafe . (:[])) a))

resultList :: (forall s . Sharing s => s a) -> IO [a]
resultList a = return (unsafeResults a)
