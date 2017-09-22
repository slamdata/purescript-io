module Control.Monad.IOSync.Class where

import Data.Monoid (class Monoid)
import Data.Newtype (unwrap, wrap)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.IO (IO)
import Control.Monad.IOSync (IOSync)
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

class (Monad m) <= MonadIOSync m where
  liftIOSync :: IOSync ~> m

instance monadIOSyncIOSync :: MonadIOSync IOSync where
  liftIOSync = id

instance monadIOSyncIO :: MonadIOSync IO where
  liftIOSync = wrap <<< liftEff <<< unwrap

instance monadIOSyncContT :: MonadIOSync m => MonadIOSync (ContT r m) where
  liftIOSync = lift <<< liftIOSync

instance monadIOSyncExceptT :: MonadIOSync m => MonadIOSync (ExceptT e m) where
  liftIOSync = lift <<< liftIOSync

instance monadIOSyncListT :: MonadIOSync m => MonadIOSync (ListT m) where
  liftIOSync = lift <<< liftIOSync

instance monadIOSyncMaybe :: MonadIOSync m => MonadIOSync (MaybeT m) where
  liftIOSync = lift <<< liftIOSync

instance monadIOSyncReader :: MonadIOSync m => MonadIOSync (ReaderT r m) where
  liftIOSync = lift <<< liftIOSync

instance monadIOSyncRWS :: (MonadIOSync m, Monoid w) => MonadIOSync (RWST r w s m) where
  liftIOSync = lift <<< liftIOSync

instance monadIOSyncState :: MonadIOSync m => MonadIOSync (StateT s m) where
  liftIOSync = lift <<< liftIOSync

instance monadIOSyncWriter :: (MonadIOSync m, Monoid w) => MonadIOSync (WriterT w m) where
  liftIOSync = lift <<< liftIOSync
