module Control.Parallel
  ( runActions
  , Progress(..)
  ) where

import Prologue

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.Threaded
import Control.Concurrent.STM
import Control.Effect.Exception hiding (Handler, handle)

data Parallel m k = forall a. Par String (m a) (m k)

instance HFunctor Parallel where
  hmap f (Par name m k) = Par name (f m) (f k)

instance Effect Parallel where
  thread ctx handler (Par name m k) = Par name (handler (m <$ ctx)) (handler (k <$ ctx))

newtype ParallelC m a = ParallelC { runParallelC :: ReaderC (TVar [m ()]) m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (Parallel :+: sig) (ParallelC m) where
  -- TODO: use name?
  alg (L (Par _ m k)) = do
    var <- ParallelC ask
    liftIO $ atomically $ modifyTVar' var (runReader var (runParallelC (void m)):)
    k
  alg (R other) = ParallelC (alg (R (handleCoercible other)))

runParallel :: TVar [m ()] -> ParallelC m a -> m a
runParallel var = runReader var . runParallelC

-- | Run arbitrary actions in parallel, given:
--
-- - @numThreads@ - The number of worker threads to run
-- - @initial@ - The initial list of actions
-- - A function that can be used to report 'Progress'
--
-- All tasks will complete before this returns.
runActions :: (Has Threaded sig m, Has (Lift IO) sig m, MonadIO m)
           => Int -- number of threads
           -> [ParallelC m ()] -- initial actions
           -> (Progress -> m ()) -- get progress updates
           -> m ()
runActions numThreads initial reportProgress = do
  state <- liftIO $
    State <$> newTVarIO []
          <*> newTVarIO 0
          <*> newTVarIO 0

  -- TODO: clean up threads
  _ <- fork $ updateProgress reportProgress state

  let enqueue action = liftIO $ atomically $ modifyTVar (stQueued state) (action:)
  traverse_ (enqueue . runParallel (stQueued state)) initial

  if numThreads > 1
    then do
      replicateM_ numThreads (fork (worker state))

      -- wait for queued and running tasks to end
      liftIO $ atomically $ do
        queued  <- readTVar (stQueued state)
        check (null queued)
        running <- readTVar (stRunning state)
        check (running == 0)

    else worker state

  pure ()

updateProgress :: MonadIO m => (Progress -> m ()) -> State any -> m ()
updateProgress f st@State{..} = do
  loop (Progress 0 0 0)
  where
  loop prev = join $ liftIO $ atomically $ stopWhenDone st $ do
    running <- readTVar stRunning
    queued <- length <$> readTVar stQueued
    completed <- readTVar stCompleted

    let new = Progress running queued completed

    check (prev /= new)

    pure $ f new *> loop new

stopWhenDone :: Applicative m => State any -> STM (m ()) -> STM (m ())
stopWhenDone State{..} act = do
  queued <- readTVar stQueued
  case queued of
    [] -> do
      running <- readTVar stRunning
      if running == 0
        then pure (pure ())
        else act
    _ -> act

worker :: (MonadIO m, Has (Lift IO) sig m) => State (m ()) -> m ()
worker st@State{..} = loop
  where

  loop = join $ liftIO $ atomically $ stopWhenDone st $ do
    queued <- readTVar stQueued
    case queued of
      [] -> retry
      (x:xs) -> do
        writeTVar stQueued xs
        addRunning
        pure $ x `finally` (complete *> loop)

  addRunning :: STM ()
  addRunning = modifyTVar stRunning (+1)

  complete :: MonadIO m => m ()
  complete = liftIO $ atomically $ modifyTVar stRunning (subtract 1) *> modifyTVar stCompleted (+1)

data State action = State
  { stQueued    :: TVar [action]
  , stRunning   :: TVar Int
  , stCompleted :: TVar Int
  }

data Progress = Progress
  { pRunning   :: Int
  , pQueued    :: Int
  , pCompleted :: Int
  } deriving (Eq, Ord, Show, Generic)
