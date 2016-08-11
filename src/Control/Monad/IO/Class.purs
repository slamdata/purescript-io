module Control.Monad.IO.Class where
  import Control.Category (id)
  import Control.Monad (class Monad)
  import Control.Monad.Aff (Aff)
  import Control.Monad.IO (IO)

  import Unsafe.Coerce (unsafeCoerce)

  class Monad m <= MonadIO m where
    liftIO :: forall a. IO a -> m a

  instance monadIOIO :: MonadIO IO where
    liftIO = id

  instance monadIOAff :: MonadIO (Aff e) where
    liftIO = unsafeCoerce
