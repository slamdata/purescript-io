module Control.Monad.IO.Effect
  ( INFINITY
  ) where

import Control.Monad.Eff (kind Effect)

foreign import data INFINITY :: Effect
