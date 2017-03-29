module Control.Monad.IO
  ( INFINITY
  , IO(..)
  ) where

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.IO (INFINITY)
import Control.Monad.Rec.Class (class MonadRec)
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype, wrap)
import Prelude

foreign import data INFINITY :: Effect

newtype IO a = IO (Aff (infinity :: INFINITY) a)

derive instance newtypeIO :: Newtype (IO a) _

derive newtype instance functorIO     :: Functor     IO
derive newtype instance applyIO       :: Apply       IO
derive newtype instance applicativeIO :: Applicative IO
derive newtype instance bindIO        :: Bind        IO
derive newtype instance monadIO       :: Monad       IO

derive newtype instance monadRecIO :: MonadRec IO

derive newtype instance semigroupIO :: (Semigroup a) => Semigroup (IO a)

derive newtype instance monoidIO :: (Monoid a) => Monoid (IO a)

instance monadAffIO :: MonadAff eff IO where
  liftAff = wrap <<< unsafeCoerceAff

instance monadEffIO :: MonadEff eff IO where
  liftEff = wrap <<< liftEff <<< unsafeCoerceEff

derive newtype instance monadErrorIO :: MonadError Error IO

derive newtype instance altIO :: Alt IO

derive newtype instance plusIO :: Plus IO

derive newtype instance alternativeIO :: Alternative IO

derive newtype instance monadZeroIO :: MonadZero IO

derive newtype instance monadPlusIO :: MonadPlus IO
