-- | Module      : Control.Monad.Sharing
-- | Copyright   : Chung-chieh Shan, Oleg Kiselyov, and Sebastian Fischer
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

  Sharing(..), Trans(..), eval,

  -- * Monad transformer

  Lazy, evalLazy

 ) where

import Control.Monad
import Control.Monad.Sharing.Classes            ( Sharing(..), Trans(..), eval )
import Control.Monad.Sharing.Implementation.CPS ( Lazy, evalLazy )
