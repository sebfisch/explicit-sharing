{-# LANGUAGE ExistentialQuantification, 
             MultiParamTypeClasses,
             FlexibleContexts,
             RelaxedPolyRec
  #-}

-- | 
-- Module      : Control.Monad.Sharing.Implementation.FirstOrder
-- Copyright   : Chung-chieh Shan, Oleg Kiselyov, and Sebastian Fischer
-- License     : PublicDomain
-- Maintainer  : Sebastian Fischer <mailto:sebf@informatik.uni-kiel.de>
-- Stability   : experimental
-- 
-- Implements a monad transformer for explicit sharing.
module Control.Monad.Sharing.Implementation.FirstOrder (

  Lazy, evalLazy

 ) where

import Control.Monad       ( MonadPlus(..) )
-- import Control.Monad.State ( MonadState(..), StateT, evalStateT )
import Control.Monad.Trans ( MonadTrans(..), MonadIO(..) )

import Control.Monad.Sharing.Classes

import qualified Control.Monad.Sharing.Implementation.CPS as CPS
import Control.Monad.Sharing.Implementation.CPS
 ( -- Store, emptyStore, 
   freshLabel, lookupValue, storeValue )

-- | 
-- Monad transformer that adds explicit sharing capability to every
-- monad.
newtype Lazy m a = Lazy { fromLazy :: m (Labeled m a) }

-- |
-- Lifts all monadic effects to the top-level and unwraps the monad
-- transformer for explicit sharing.
evalLazy :: (Monad m, Shareable (Lazy m) a, Convertible (Lazy m) a b)
         => Lazy m a -> m b
evalLazy m = do Lifted a <- fromLazy (evalS (gnf m) >>= convert)
                return a

-- type S m a = StateT Store m a
type S m a = CPS.Lazy m a

evalS :: Monad m => S m a -> m a
-- evalS m = evalStateT m emptyStore
evalS m = CPS.runLazy m

-- using 'CPS.Lazy' instead of 'StateT Store' is almost twice as fast.

-- private declarations             

data Labeled m a
  = Lifted a
  | WithFresh (Int -> Lazy m a)
  | forall b . Shareable (Lazy m) b => Labeled Int (Lazy m b) (b -> Lazy m a)


gnf :: (Monad m, Shareable (Lazy m) a) => Lazy m a -> S (Lazy m) a
gnf a = hnf a >>= shareArgs (\b -> gnf b >>= return . return)

hnf :: Monad m => Lazy m a -> S (Lazy m) a
hnf m = run =<< lift (lift (fromLazy m))

run :: Monad m => Labeled m a -> S (Lazy m) a
run (Lifted a)      = return a
run (WithFresh f)   = hnf . f =<< freshLabel
run (Labeled n a f) = do thunk <- lookupValue n
                         case thunk of
                           Just c  -> hnf (f c)
                           Nothing -> do x <- labelArgs a
                                         storeValue n x
                                         hnf (f x)

labelArgs :: (Monad m, Shareable (Lazy m) a) => Lazy m a -> S (Lazy m) a
labelArgs a = hnf a >>= shareArgs (\x -> do n <- freshLabel
                                            return (setLabel n x.:a))

-- some type trickery to identify monads
(.:) :: Lazy m a -> Lazy m b -> Lazy m a
(.:) = const

setLabel :: (Monad m, Shareable (Lazy m) a) => Int -> Lazy m a -> Lazy m a
setLabel n x = Lazy (return (Labeled n x return))

instance Monad m => Monad (Lazy m)
 where
  return  = Lazy . return . Lifted
  a >>= k = Lazy (fromLazy a >>= bind k)
  fail    = Lazy . fail

bind :: Monad m => (a -> Lazy m b) -> Labeled m a -> m (Labeled m b)
bind k (Lifted a)      = fromLazy (k a)
bind k (WithFresh f)   = return (WithFresh (\n -> f n >>= k))
bind k (Labeled n m f) = return (Labeled n m (\x -> f x >>= k))

-- The 'MonadPlus' instance reuses corresponding operations of the
-- base monad.
instance MonadPlus m => MonadPlus (Lazy m)
 where
  mzero       = Lazy mzero
  a `mplus` b = Lazy (fromLazy a `mplus` fromLazy b)

-- 'Lazy t' is a monad transformer.
instance MonadTrans Lazy
 where
  lift a = Lazy (a >>= return . Lifted)

-- If the underlying monad supports IO we can lift this functionality.
instance MonadIO m => MonadIO (Lazy m)
 where
  liftIO = lift . liftIO

-- The @Sharing@ instance introduces the internal sharing constructors.
instance Monad m => Sharing (Lazy m)
 where
  share a = Lazy (return (WithFresh (\n -> return (setLabel n a))))
