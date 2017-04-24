module Control.Monad.IOSync
  ( module Control.Monad.IO.Effect
  , IOSync(..)
  , runIOSync
  , runIOSync'
  ) where

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (Error, catchException, error, throwException)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.IO.Effect (INFINITY)
import Control.Monad.Rec.Class (class MonadRec)
import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap, wrap)
import Prelude

newtype IOSync a = IOSync (Eff (infinity :: INFINITY) a)

runIOSync :: IOSync ~> Eff (infinity :: INFINITY)
runIOSync = unwrap

runIOSync' :: ∀ eff. IOSync ~> Eff (infinity :: INFINITY | eff)
runIOSync' = unsafeCoerceEff <<< unwrap

derive instance newtypeIOSync :: Newtype (IOSync a) _

derive newtype instance functorIOSync     :: Functor     IOSync
derive newtype instance applyIOSync       :: Apply       IOSync
derive newtype instance applicativeIOSync :: Applicative IOSync
derive newtype instance bindIOSync        :: Bind        IOSync
derive newtype instance monadIOSync       :: Monad       IOSync

derive newtype instance monadRecIOSync :: MonadRec IOSync

instance semigroupIOSync :: (Semigroup a) => Semigroup (IOSync a) where
  append a b = append <$> a <*> b

instance monoidIOSync :: (Monoid a) => Monoid (IOSync a) where
  mempty = pure mempty

instance monadEffIOSync :: MonadEff eff IOSync where
  liftEff = wrap <<< unsafeCoerceEff

instance monadErrorIOSync :: MonadError Error IOSync where
  catchError a k = liftEff $
    catchException (\e -> unwrap $ k e) (unsafeCoerceEff $ unwrap a)

instance monadThrowIOSync :: MonadThrow Error IOSync where
  throwError = liftEff <<< throwException

instance altIOSync :: Alt IOSync where
  alt a b = a `catchError` const b

instance plusIOSync :: Plus IOSync where
  empty = throwError $ error "plusIOSync.empty"

instance alternativeIOSync :: Alternative IOSync

instance monadZeroIOSync :: MonadZero IOSync
