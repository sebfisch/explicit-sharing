{-# LANGUAGE FlexibleContexts #-}

module Control.Monad.Sharing.Lazy (

  Lazy, runLazy, evalLazy,

  module Control.Monad.Sharing

 ) where

import Control.Monad.Sharing
import Control.Monad.Sharing.Lazy.ContReaderNoThunksInlined

evalLazy :: Monad m => Nondet (Lazy m) a => Lazy m a -> m a
evalLazy m = runLazy (m >>= eval)

