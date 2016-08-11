module Control.Monad.IO.Class where
  import Control.Category (id)
  import Control.Monad (class Monad)
  import Control.Monad.IO (IO)

  class Monad m <= MonadIO m where
    liftIO :: forall a. IO a -> m a

  instance monadIOIO :: MonadIO IO where
    liftIO = id
