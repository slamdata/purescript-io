module Control.Monad.IO.Class where

import Control.Monad.IO (IO)
import Prelude

class (Monad m) <= MonadIO m where
  liftIO :: IO ~> m

instance monadIOIO :: MonadIO IO where
  liftIO = id
