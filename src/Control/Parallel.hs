{-# language TemplateHaskell #-}
module Control.Parallel
  ( -- runActions
  -- , Progress(..)
  ) where

import Prologue

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Polysemy
import Polysemy.Final
import Polysemy.Async
import Polysemy.Input
import Polysemy.Output
import Polysemy.Resource
import qualified Polysemy.State as S

data Parallel m a where
  Fork :: Text -> m () -> Parallel m ()

makeSem ''Parallel

example :: Members '[Embed IO, Parallel] r => Sem r ()
example = (replicateM_ 100 . embed . print =<<) . S.runState @Int 0 $ do
  sequence [fork "" (go n) | n <- [1..100]]
  pure ()

go :: Members '[Embed IO, Parallel, S.State Int] r => Int -> Sem r ()
go n = do
  --embed $ putStrLn $ "hello " <> show n
  fork "" $ do
    S.modify @Int (+1)
    embed $ threadDelay 2000000
    embed . print =<< S.get @Int
    --embed $ putStrLn $ "world" <> show n
    pure ()

parallelToIO :: forall r a
                   . Members '[Embed IO, Async, Resource] r
                  => Int
                  -> (Progress -> Sem r ())
                  -> Sem (Parallel ': r) a
                  -> Sem r ()
parallelToIO numThreads reportProgress act = do
  (state :: State (Sem r ())) <- embed $
    State <$> newTVarIO []
          <*> newTVarIO 0
          <*> newTVarIO 0

  sinkIntoTVar (stQueued state) act

  _ <- async $ updateProgress reportProgress state

  if numThreads > 1
    then do
      replicateM_ numThreads (async (worker state))

      -- wait for queued and running tasks to end
      embed @IO $ atomically $ do
        queued  <- readTVar (stQueued state)
        check (null queued)
        running <- readTVar (stRunning state)
        check (running == 0)

    else worker state

sinkIntoTVar :: Member (Embed IO) r => TVar [Sem r ()] -> Sem (Parallel ': r) a -> Sem r a
sinkIntoTVar var = interpretH $ \case
  Fork name act -> do
    act' <- runT act
    fa <- embed @IO $ atomically $ modifyTVar var $ (:) $ () <$ sinkIntoTVar var act'
    pureT fa

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

{-
-- | Run arbitrary actions in parallel, given:
--
-- - @numThreads@ - The number of worker threads to run
-- - @initial@ - The initial list of actions
-- - @runAction@ - A function that runs an action, which can itself enqueue more actions
--   via the @enqueue@ function provided as an argument
-- - A function that can be used to report 'Progress'
--
-- All tasks will complete before this returns.
runActions :: forall r action
            . Members '[Embed IO, Async, Resource] r
           => Int -- number of threads
           -> [action] -- initial actions
           -> ((action -> Sem r ()) -> action -> Sem r ()) -- given an action to enqueue more actions, run an action
           -> (Progress -> Sem r ()) -- get progress updates
           -> Sem r ()
runActions numThreads initial runAction reportProgress = do
  state <- embed $
    State <$> newTVarIO initial
          <*> newTVarIO 0
          <*> newTVarIO 0

  _ <- async $ updateProgress reportProgress state

  let enqueue action = embed $ atomically $ modifyTVar (stQueued state) (action:)

  if numThreads > 1
    then do
      replicateM_ numThreads (async (worker (runAction enqueue) state))

      -- wait for queued and running tasks to end
      embed $ atomically $ do
        queued  <- readTVar (stQueued state)
        check (null queued)
        running <- readTVar (stRunning state)
        check (running == 0)

    else worker (runAction enqueue) state

  pure ()
-}

updateProgress :: Member (Embed IO) r => (Progress -> Sem r ()) -> State any -> Sem r ()
updateProgress f st@State{..} = loop (Progress 0 0 0)
  where
  loop prev = join $ embed $ atomically $ stopWhenDone st $ do
    running <- readTVar stRunning
    queued <- length <$> readTVar stQueued
    completed <- readTVar stCompleted

    let new = Progress running queued completed

    check (prev /= new)

    pure $ f new *> loop new

stopWhenDone :: State any -> STM (Sem r ()) -> STM (Sem r ())
stopWhenDone State{..} act = do
  queued <- readTVar stQueued
  case queued of
    [] -> do
      running <- readTVar stRunning
      if running == 0
        then pure (pure ())
        else act
    _ -> act

worker :: forall r. (Member (Embed IO) r, Member Resource r) => State (Sem r ()) -> Sem r ()
worker st@State{..} = loop
  where

  loop = join $ embed $ atomically $ stopWhenDone st $ do
    queued <- readTVar stQueued
    case queued of
      [] -> retry
      (x:xs) -> do
        writeTVar stQueued xs
        addRunning
        pure $ x `finally` (complete *> loop)

  addRunning :: STM ()
  addRunning = modifyTVar stRunning (+1)

  complete :: Sem r ()
  complete = embed $ atomically $ modifyTVar stRunning (subtract 1) *> modifyTVar stCompleted (+1)
