module Control.Carrier.Threaded
  ( ThreadedC
  , runThreaded
  , module X
  ) where

import Control.Carrier.Lift
import qualified Control.Concurrent as Conc
import Control.Concurrent.STM
import Control.Effect.Exception
import Control.Effect.Threaded as X
import Control.Monad.IO.Class
import Data.Functor (void)
import Prelude

runThreaded :: ThreadedC m a -> m a
runThreaded = runThreadedC

newtype ThreadedC m a = ThreadedC { runThreadedC :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Has (Lift IO) sig m => Algebra (Threaded :+: sig) (ThreadedC m) where
  alg (L (Fork m k)) = liftWith @IO (\ctx runIt -> do
                                        var <- newEmptyTMVarIO
                                        tid <- mask $ \restore -> Conc.forkIO $ try (void (restore (runIt (m <$ ctx)))) >>= atomically . putTMVar var
                                        pure (Handle tid (readTMVar var) <$ ctx)
                                        ) >>= k
  alg (R other) = ThreadedC (alg (handleCoercible other))
