module Control.Carrier.Finally
  ( -- * Finally carrier
    FinallyC (..),
    runFinally,

    -- * Re-exports
    module X,
  )
where

import Control.Carrier.Reader
import Control.Monad.IO.Class (MonadIO)
import Control.Effect.Exception (finally)
import Control.Effect.Finally as X
import Control.Effect.Lift
import Data.Functor (void)
import Data.IORef
import Prelude

newtype FinallyC m a = FinallyC {runFinallyC :: ReaderC (IORef (FinallyC m ())) m a}
  deriving (Functor, Applicative, Monad, MonadIO)

runFinally :: Has (Lift IO) sig m => FinallyC m a -> m a
runFinally (FinallyC go) = do
  ref <- sendIO $ newIORef (pure ())
  runReader ref go `finally` (runFinally =<< sendIO (readIORef ref))

instance (Has (Lift IO) sig m, Algebra sig m) => Algebra (Finally :+: sig) (FinallyC m) where
  alg hdl sig ctx = FinallyC $ case sig of
    L (OnExit go) -> do
      ref <- ask
      sendIO $ atomicModifyIORef ref (\cur -> (cur *> void (hdl (go <$ ctx)), ()))
      pure ctx
    R other -> alg (runFinallyC . hdl) (R other) ctx
