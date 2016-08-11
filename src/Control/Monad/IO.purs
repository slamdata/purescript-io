module Control.Monad.IO (IO, INFINITY, AffIO(..), runIO) where
  import Prelude

  import Control.Alt (class Alt, alt)
  import Control.Alternative (class Alternative)
  import Control.Monad.Eff (Eff)
  import Control.Monad.Eff.Class (class MonadEff, liftEff)
  import Control.Monad.Aff (Aff)
  import Control.Monad.Aff.Class (class MonadAff)
  import Control.Monad.Eff.Exception (Error)
  import Control.Monad.Error.Class (class MonadError, throwError, catchError)
  import Control.Monad.Rec.Class (class MonadRec, tailRecM)
  import Control.MonadPlus (class MonadZero, class MonadPlus, empty)
  import Control.Parallel.Class (class MonadRace, class MonadPar, par, race, stall)
  import Control.Plus (class Plus)

  import Data.Monoid (class Monoid, mempty)

  import Unsafe.Coerce (unsafeCoerce)

  foreign import data IO :: * -> *

  foreign import data INFINITY :: !

  type AffIO a = Aff (infinity :: INFINITY) a

  runIO :: forall a. IO a -> AffIO a
  runIO = unsafeCoerce

  toIO :: forall e a. Aff e a -> IO a
  toIO = unsafeCoerce

  instance semigroupIO :: (Semigroup a) => Semigroup (IO a) where
    append a b = toIO (append (runIO a) (runIO b))

  instance monoidIO :: (Monoid a) => Monoid (IO a) where
    mempty = toIO (pure mempty)

  instance functorIO :: Functor IO where
    map f fa = toIO (map f (runIO fa))

  instance applyIO :: Apply IO where
    apply ff fa = toIO (apply (runIO ff) (runIO fa))

  instance applicativeIO :: Applicative IO where
    pure v = toIO (pure v)

  instance bindIO :: Bind IO where
    bind fa f = toIO (bind (runIO fa) (unsafeCoerce f))

  instance monadIO :: Monad IO

  instance monadEffIO :: MonadEff e IO where
    liftEff = liftEff'
      where
        liftEff' :: forall a. Eff e a -> IO a
        liftEff' eff = toIO (liftEff eff :: Aff e a)

  instance monadAffIO :: MonadAff e IO where
    liftAff = toIO

  instance monadErrorIO :: MonadError Error IO where
    throwError e = toIO (throwError e)

    catchError io f = toIO (catchError (runIO io) (runIO <$> f))

  instance altIO :: Alt IO where
    alt a1 a2 = toIO (alt (runIO a1) (runIO a2))

  instance plusIO :: Plus IO where
    empty = toIO empty

  instance alternativeIO :: Alternative IO

  instance monadZero :: MonadZero IO

  instance monadPlusIO :: MonadPlus IO

  instance monadRecIO :: MonadRec IO where
    tailRecM f a = toIO (tailRecM (unsafeCoerce f) a)

  instance monadParIO :: MonadPar IO where
    par f ma mb = toIO (par f (runIO ma) (runIO mb))

  instance monadRaceIO :: MonadRace IO where
    stall = toIO stall
    race a1 a2 = toIO (race (runIO a1) (runIO a2))
