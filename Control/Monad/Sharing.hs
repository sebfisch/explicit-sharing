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

  -- * Monad transformer

  Lazy, evalLazy

 ) where

import Control.Monad
import Control.Monad.Sharing.Classes
import Control.Monad.Sharing.Implementation.CPS
