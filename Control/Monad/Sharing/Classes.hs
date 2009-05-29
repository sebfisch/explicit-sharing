{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, Rank2Types #-}

-- | 
-- Module      : Control.Monad.Sharing.Classes
-- Copyright   : Chung-chieh Shan, Oleg Kiselyov, and Sebastian Fischer
-- License     : PublicDomain
-- Maintainer  : Sebastian Fischer <mailto:sebf@informatik.uni-kiel.de>
-- Stability   : experimental
-- 
-- This library provides type classes for explicit sharing of monadic
-- effects. Usually you don't need to import this library as it is
-- reexported by the module 'Control.Monad.Sharing'. You may want to
-- do so, however, when writing your own implementation of explicit
-- sharing.
module Control.Monad.Sharing.Classes (

  Sharing(..), Trans(..), eval

 ) where

import Control.Monad ( liftM, join )

-- | Interface of monads that support explicit sharing.
class Sharing m
 where
  -- | 
  -- Yields an action that returns the same results as the given
  -- action but whose effects are only executed once. Especially, when
  -- the resulting action is duplicated it returns the same result at
  -- every occurrence.
  share :: Trans m a a => m a -> m (m a)

-- |
-- Interface to transform nested monadic data types. The provided
-- function 'trans' is supposed to map the given function on every
-- monadic argument. The result of 'trans' may be of the same type as
-- the argument but can also be of a different type, e.g. to convert a
-- value with nested monadic arguments to a corresponding value
-- without.
-- 
-- We provide instances of the 'Trans' class for some predefined
-- Haskell types. For flat types the function 'trans' just returns its
-- argument which has no arguments to which the given function could
-- be applied.
class Trans m a b
 where
  trans :: (forall c d . Trans m c d => m c -> m (m d)) -> a -> m b

-- |
-- Lifts all monadic effects in nested monadic values to the top
-- level. If @m@ is a monad for non-determinism and the argument a
-- data structure with nested non-determinism then the result
-- corresponds to the normal form of the argument.
eval :: (Monad m, Trans m a b) => a -> m b
eval = trans (\a -> liftM return (a >>= eval))

instance Monad m => Trans m Bool Bool
 where
  trans _ = return

instance Monad m => Trans m Int Int
 where
  trans _ = return

instance Monad m => Trans m Char Char
 where
  trans _ = return

instance Monad m => Trans m Float Float
 where
  trans _ = return

instance Monad m => Trans m Double Double
 where
  trans _ = return

instance Monad m => Trans m [Bool] [Bool]
 where
  trans _ = return

instance Monad m => Trans m [Int] [Int]
 where
  trans _ = return

instance Monad m => Trans m [Char] [Char]
 where
  trans _ = return

instance Monad m => Trans m [Float] [Float]
 where
  trans _ = return

instance Monad m => Trans m [Double] [Double]
 where
  trans _ = return

-- | An instance for lists with monadic elements.
instance (Monad m, Trans m a a) => Trans m [m a] [m a]
 where
  trans f = mapM f

-- |
-- An instance for lists with monadic elements that lifts all monadic
-- effects to the top level and yields a list with non-monadic
-- elements.
instance (Monad m, Trans m a a) => Trans m [m a] [a]
 where
  trans f = mapM (join . f)
