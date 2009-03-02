{-# LANGUAGE
     NoMonomorphismRestriction,
     MultiParamTypeClasses,
     FlexibleInstances
  #-}

module List where 

import Control.Monad.Sharing.Lazy

data List m a = Nil | Cons (m a) (m (List m a))

nil = return Nil
cons x xs = return (Cons x xs)

match n _ Nil = n
match _ c (Cons x xs) = c x xs

empty l = l >>= match (return True) (\_ _ -> return False)
first l = l >>= match mzero const
rest  l = l >>= match mzero (flip const)

fold n c l = l >>= match n (\x xs -> c x (fold n c xs))

instance Nondet m a => Nondet m (List m a)
 where
  mapNondet _ Nil         = return Nil
  mapNondet f (Cons x xs) = do
    y  <- f x
    ys <- f xs
    return (Cons y ys)

instance MonadPlus m => Nondet m Int
 where
  mapNondet _ = return
