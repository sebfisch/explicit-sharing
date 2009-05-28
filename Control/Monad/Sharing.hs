{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, Rank2Types #-}

-- | Module      : Control.Monad.Sharing
-- | Copyright   : Sebastian Fischer
-- | License     : PublicDomain
-- |
-- | Maintainer  : Sebastian Fischer (sebf@informatik.uni-kiel.de)
-- | Stability   : experimental
-- |
-- | This library provides an interface to monads that support explicit
-- | sharing.
module Control.Monad.Sharing (

  module Control.Monad,

  -- * Classes

  Sharing(..), Trans(..),

  -- $predefined

  -- * Evaluation

  eval,

  -- * Monadic lists

  List(..), nil, cons, isEmpty, first, rest

 ) where

import Control.Monad

-- | Interface of monads that support explicit sharing.
class Sharing m
 where
  -- | Yields an action that returns the same results as the given
  -- | action but whose effects are only executed once. Especially,
  -- | when the resulting action is duplicated it returns the same
  -- | result at every occurrence.
  share :: Trans m a a => m a -> m (m a)

-- | Interface to transform nested monadic data types. The provided
-- | function @trans@ is supposed to map the given function on every
-- | monadic argument. The result of @trans@ may be of the same type
-- | as the argument but can also be of a different type, e.g. to
-- | convert a value with nested monadic arguments to a corresponding
-- | value without.
class Trans m a b
 where
  trans :: (forall c d . Trans m c d => m c -> m (m d)) -> a -> m b

-- | Lifts all monadic effects in nested monadic values to the top
-- | level. If @m@ is a monad for non-determinism and the argument a
-- | data structure with nested non-determinism then the result
-- | corresponds to the normal form of the argument.
eval :: (Monad m, Trans m a b) => a -> m b
eval = trans (\a -> liftM return (a >>= eval))

-- $predefined 
--
-- We provide instances of the @Trans@ class for some predefined
-- Haskell types. For flat types the function @trans@ just returns its
-- argument which has no arguments to which the given function could
-- be applied.

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

-- | An instance for lists with monadic elements that lifts all
-- | monadic effects to the top level and yields a list with
-- | non-monadic elements.
instance (Monad m, Trans m a a) => Trans m [m a] [a]
 where
  trans f = mapM (join . f)

-- | Data type for lists where both the head and tail are monadic.
data List m a = Nil | Cons (m a) (m (List m a))

-- | The empty monadic list.
nil :: Monad m => m (List m a)
nil = return Nil

-- | Constructs a non-empty monadic list.
cons :: Monad m => m a -> m (List m a) -> m (List m a)
cons x xs = return (Cons x xs)

-- | Checks if monadic list is empty.
isEmpty :: Monad m => m (List m a) -> m Bool
isEmpty ml = do l <- ml
                case l of
                  Nil      -> return True
                  Cons _ _ -> return False

-- | Yields the head of a monadic list. Relies on @MonadPlus@ instance
-- | to provide a failing implementation of @fail@.
first :: MonadPlus m => m (List m a) -> m a
first ml = do Cons x _ <- ml; x

-- | Yields the tail of a monadic list. Relies on @MonadPlus@ instance
-- | to provide a failing implementation of @fail@.
rest :: MonadPlus m => m (List m a) -> m (List m a)
rest ml = do Cons _ xs <- ml; xs

instance (Monad m, Trans m a b) => Trans m (List m a) (List m b)
 where
  trans _ Nil         = return Nil
  trans f (Cons x xs) = return Cons `ap` f x `ap` f xs

instance (Monad m, Trans m a b) => Trans m (List m a) [b]
 where
  trans _ Nil         = return []
  trans f (Cons x xs) = return (:) `ap` join (f x) `ap` join (f xs)

instance (Monad m, Trans m a b) => Trans m [a] (List m b)
 where
  trans _ []     = return Nil
  trans f (x:xs) = return Cons `ap` f (return x) `ap` f (return xs)
