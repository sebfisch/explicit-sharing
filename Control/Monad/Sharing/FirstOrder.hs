-- | 
-- Module      : Control.Monad.Sharing.FirstOrder
-- Copyright   : Chung-chieh Shan, Oleg Kiselyov, and Sebastian Fischer
-- License     : PublicDomain
-- Maintainer  : Sebastian Fischer <mailto:sebf@informatik.uni-kiel.de>
-- Stability   : experimental
-- 
-- This library provides an interface to monads that support explicit
-- sharing based on two-level types. This implementation is not as
-- efficient as the default implementation but supports a restricted
-- form of sharing across non-determinism if a first-order data type
-- is used as underlying monad.
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
