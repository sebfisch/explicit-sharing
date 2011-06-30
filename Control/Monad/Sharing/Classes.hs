{-# LANGUAGE
      MultiParamTypeClasses,
      FlexibleInstances,
      FlexibleContexts,
      Rank2Types
  #-}

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

  Sharing(..), Shareable(..), Convertible(..), Nondet(..),

  MInt, MChar, MBool

 ) where

import Control.Monad ( MonadPlus )

import qualified Data.Set as Set
import qualified Data.Map as Map

-- | Interface of monads that support explicit sharing.
class MonadPlus s => Sharing s
 where
  -- | 
  -- Yields an action that returns the same results as the given
  -- action but whose effects are only executed once. Especially, when
  -- the resulting action is duplicated it returns the same result at
  -- every occurrence.
  share :: Shareable s a => s a -> s (s a)

-- |
-- Interface of shareable nested monadic data types. The provided
-- function 'shareArgs' is supposed to map the given function on every
-- monadic argument.
-- 
-- We provide instances of the 'Shareable' class for some predefined
-- Haskell types. For flat types the function 'shareArgs' just returns
-- its argument which has no arguments to which the given function
-- could be applied.
class Shareable m a
 where
  shareArgs :: Monad n =>
               (forall b . Shareable m b => m b -> n (m b)) -> a -> n a

type MInt  m = Int
type MChar m = Char
type MBool m = Bool

instance Monad m => Shareable m Bool
 where
  shareArgs _ = return

instance Monad m => Shareable m Int
 where
  shareArgs _ = return

instance Monad m => Shareable m Integer
 where
  shareArgs _ = return

instance Monad m => Shareable m Float
 where
  shareArgs _ = return

instance Monad m => Shareable m Double
 where
  shareArgs _ = return

instance Monad m => Shareable m Char
 where
  shareArgs _ = return

instance Monad m => Shareable m [Bool]
 where
  shareArgs _ = return

instance Monad m => Shareable m [Int]
 where
  shareArgs _ = return

instance Monad m => Shareable m [Integer]
 where
  shareArgs _ = return

instance Monad m => Shareable m [Float]
 where
  shareArgs _ = return

instance Monad m => Shareable m [Double]
 where
  shareArgs _ = return

instance Monad m => Shareable m [Char]
 where
  shareArgs _ = return

instance Monad m => Shareable m (a -> b)
 where
  shareArgs _ = return

-- | An instance for lists with monadic elements.
instance (Monad m, Shareable m a) => Shareable m [m a]
 where
  shareArgs f = mapM f

-- |
-- Interface for convertible datatypes. The provided function
-- 'convArgs' is supposed to map the given function on every argument
-- of the given value and combine the results to give the converted
-- value.
-- 
-- We provide instances of the 'Convertible' class for some predefined
-- Haskell types. For flat types the function 'convArgs' just returns
-- its argument which has no arguments to which the given function
-- could be applied.
class Convertible m a b
 where
  convert :: a -> m b

instance Monad m => Convertible m Bool Bool
 where
  convert = return

instance Monad m => Convertible m Int Int
 where
  convert = return

instance Monad m => Convertible m Integer Integer
 where
  convert = return

instance Monad m => Convertible m Float Float
 where
  convert = return

instance Monad m => Convertible m Double Double
 where
  convert = return

instance Monad m => Convertible m Char Char
 where
  convert = return

instance Monad m => Convertible m [Bool] [Bool]
 where
  convert = return

instance Monad m => Convertible m [Int] [Int]
 where
  convert = return

instance Monad m => Convertible m [Integer] [Integer]
 where
  convert = return

instance Monad m => Convertible m [Float] [Float]
 where
  convert = return

instance Monad m => Convertible m [Double] [Double]
 where
  convert = return

instance Monad m => Convertible m [Char] [Char]
 where
  convert = return

-- |
-- An instance to convert ordinary lists into lists with monadic
-- elements.
instance (Monad m, Convertible m a b) => Convertible m [a] [m b]
 where
  convert = return . map convert

-- |
-- An instance to convert lists with monadic elements into ordinary
-- lists.
instance (Monad m, Convertible m a b) => Convertible m [m a] [b]
 where
  convert = mapM (>>=convert)

-- |

-- Instances of this class can be used to observe non-deterministic
-- computations with sharing. The function '?' must satisfy the
-- following laws:
--
-- @
--                 a ? a  =  a
--     (a ? b) ? (c ? d)  =  (a ? c) ? (b ? d)
-- @
class Nondet n where
  failure :: n
  (?)     :: n -> n -> n

instance Nondet Bool where
  failure = False
  (?)     = (||)

instance Ord a => Nondet (Set.Set a) where
  failure = Set.empty
  (?)     = Set.union

instance Ord a => Nondet (Map.Map a Rational) where
  failure = Map.empty
  d1 ? d2 = Map.map (/2) $ Map.unionWith (+) d1 d2
