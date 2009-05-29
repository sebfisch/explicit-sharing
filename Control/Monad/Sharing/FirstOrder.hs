-- | 
-- Module      : Control.Monad.Sharing.FirstOrder
-- Copyright   : Chung-chieh Shan, Oleg Kiselyov, and Sebastian Fischer
-- License     : PublicDomain
-- Maintainer  : Sebastian Fischer <mailto:sebf@informatik.uni-kiel.de>
-- Stability   : experimental
-- 
-- This library provides an interface to monads that support explicit
-- sharing based on two-level types. This implementation is not as
-- efficient as the default implementation but it avoids duplicate
-- sharing which can lead to exponential blowup of the threaded heap.
module Control.Monad.Sharing.FirstOrder (

  module Control.Monad,

  -- * Classes

  Sharing(..), Shareable(..), Convertible(..), convert,

  -- * Monad transformer

  Lazy, evalLazy

 ) where

import Control.Monad
import Control.Monad.Sharing.Classes
import Control.Monad.Sharing.Implementation.FirstOrder
