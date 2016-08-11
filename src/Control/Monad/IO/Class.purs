module Control.Monad.IO.Class where
  import Prelude
  import Control.Monad.Aff (Aff)
  import Control.Monad.Eff (Eff)
  import Control.Monad.Eff.Class (liftEff)
  import Control.Monad.IO (IO)

  import Unsafe.Coerce (unsafeCoerce)

  class Monad m <= MonadIO m where
    liftIO :: forall a. m a -> IO a

  instance monadIOIO :: MonadIO IO where
    liftIO = id

  instance monadIOAff :: MonadIO (Aff e) where
    liftIO = unsafeCoerce

  instance monadIOEff :: MonadIO (Eff e) where
    liftIO = liftIO <<< (liftEff :: Eff e ~> Aff e)
