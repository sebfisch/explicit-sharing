{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- |
-- Module      : Data.Monadic.List
-- Copyright   : Chung-chieh Shan, Oleg Kiselyov, and Sebastian Fischer
-- License     : PublicDomain
-- Maintainer  : Sebastian Fischer (sebf\@informatik.uni-kiel.de)
-- Stability   : experimental
-- 
-- This library provides lists with monadic head and tail as an
-- example for nested monadic data that can be used with the
-- combinator @share@ for explicit sharing.
module Data.Monadic.List (

  List(..), nil, cons, isEmpty, first, rest

 ) where

import Control.Monad                 ( MonadPlus, ap )
import Control.Monad.Sharing.Classes ( Shareable(..), Convertible(..) )

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

-- |
-- Yields the head of a monadic list. Relies on 'MonadPlus' instance
-- to provide a failing implementation of 'fail'.
first :: MonadPlus m => m (List m a) -> m a
first ml = do Cons x _ <- ml; x

-- |
-- Yields the tail of a monadic list. Relies on 'MonadPlus' instance
-- to provide a failing implementation of 'fail'.
rest :: MonadPlus m => m (List m a) -> m (List m a)
rest ml = do Cons _ xs <- ml; xs

-- |
-- This instance allows to use nested monadic lists as argument to the
-- 'Control.Monad.Sharing.share' combinator.
instance (Monad m, Shareable m a) => Shareable m (List m a)
 where
  shareArgs f Nil         = return Nil
  shareArgs f (Cons x xs) = do y  <- f x
                               ys <- f xs
                               return (Cons y ys)

-- |
-- This instance enables the function 'Control.Monad.Sharing.convert'
-- to transform ordinary Haskell lists into nested monadic lists.
instance (Monad m, Convertible m a b) => Convertible m [a] (List m b)
 where
  convert []     = return Nil
  convert (x:xs) = return (Cons (convert x) (convert xs))

-- |
-- This instance enables the function 'Control.Monad.Sharing.convert'
-- to transform nested monadic lists into ordinary Haskell lists.
instance (Monad m, Convertible m a b) => Convertible m (List m a) [b]
 where
  convert Nil         = return []
  convert (Cons x xs) = do y  <- x  >>= convert
                           ys <- xs >>= convert
                           return (y:ys)
