module Control.Monad.IO.Class where

import Data.Monoid (class Monoid)
import Control.Monad.IO (IO)
import Control.Monad.Cont.Trans (ContT)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.List.Trans (ListT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.RWS.Trans (RWST)
import Control.Monad.State.Trans (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT)
import Prelude

class (Monad m) <= MonadIO m where
  liftIO :: IO ~> m

instance monadIOIO :: MonadIO IO where
  liftIO = id

instance monadIOContT :: MonadIO m => MonadIO (ContT r m) where
  liftIO = lift <<< liftIO

instance monadIOExceptT :: MonadIO m => MonadIO (ExceptT e m) where
  liftIO = lift <<< liftIO

instance monadIOListT :: MonadIO m => MonadIO (ListT m) where
  liftIO = lift <<< liftIO

instance monadIOMaybe :: MonadIO m => MonadIO (MaybeT m) where
  liftIO = lift <<< liftIO

instance monadIOReader :: MonadIO m => MonadIO (ReaderT r m) where
  liftIO = lift <<< liftIO

instance monadIORWS :: (MonadIO m, Monoid w) => MonadIO (RWST r w s m) where
  liftIO = lift <<< liftIO

instance monadIOState :: MonadIO m => MonadIO (StateT s m) where
  liftIO = lift <<< liftIO

instance monadIOWriter :: (MonadIO m, Monoid w) => MonadIO (WriterT w m) where
  liftIO = lift <<< liftIO
