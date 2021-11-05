module Control.Carrier.Threaded (
  fork,
  kill,
  wait,
  Handle (..),
) where

import Control.Carrier.Lift (Has, Lift, liftWith)
import Control.Concurrent qualified as Conc
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM (
  STM,
  atomically,
  newEmptyTMVarIO,
  putTMVar,
  readTMVar,
 )
import Control.Effect.Exception (
  SomeException,
  mask,
  throwTo,
  try,
 )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor (void, ($>))

data Handle = Handle
  { handleTid :: Conc.ThreadId
  , handleWait :: STM (Either SomeException ())
  }

fork :: Has (Lift IO) sig m => m a -> m Handle
fork m = liftWith @IO $ \hdl ctx -> do
  var <- newEmptyTMVarIO
  tid <- mask $ \restore -> Conc.forkIO $ try (void (restore (hdl (m <$ ctx)))) >>= atomically . putTMVar var
  pure (Handle tid (readTMVar var) <$ ctx)

kill :: Has (Lift IO) sig m => Handle -> m ()
kill h = liftWith @IO $ \_ ctx -> liftIO (throwTo (handleTid h) Async.AsyncCancelled) $> ctx

wait :: Has (Lift IO) sig m => Handle -> m ()
wait h = liftWith @IO $ \_ ctx -> liftIO (atomically (handleWait h)) $> ctx
