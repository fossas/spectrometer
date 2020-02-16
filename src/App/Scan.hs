
module App.Scan
  ( scanMain
  ) where

import Prologue

import Control.Carrier.Error.Either
import Control.Effect.Exception as Exc
import Control.Carrier.Lift
import Control.Carrier.Writer.Strict
import Control.Concurrent
import Data.Bool (bool)
import qualified Data.Sequence as S
import Path.IO
import System.Exit (die)

import App.Scan.Project (mkProjects)
import App.Scan.ProjectInference (InferredProject(..), inferProject)
import Control.Parallel
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Diagnostics
import Discovery
import Effect.Exec
import Effect.Logger
import Effect.ReadFS hiding (doesDirExist)
import Types

scanMain :: Path Abs Dir -> Bool -> IO ()
scanMain basedir debug = do
  exists <- doesDirExist basedir
  unless exists (die $ "ERROR: " <> show basedir <> " does not exist")

  scan basedir
    & runLogger (bool SevInfo SevDebug debug)
    & runM

scan ::
  ( Has (Lift IO) sig m
  , Has Logger sig m
  , MonadIO m
  , Effect sig
  )
  => Path Abs Dir -> m ()
scan basedir = do
  setCurrentDir basedir
  capabilities <- liftIO getNumCapabilities

  () <- runActions capabilities (map ADiscover discoverFuncs) (runTask basedir) updateProgress
    -- FIXME: async output

  logSticky "[ Combining Analyses ]"

  --let projects = mkProjects strategyGroups (S.fromList results)
  let projects = mkProjects strategyGroups (S.fromList [])
  liftIO (encodeFile "analysis.json" projects)

  inferred <- inferProject basedir
  logInfo ""
  logInfo ("Inferred project name: `" <> pretty (inferredName inferred) <> "`")
  logInfo ("Inferred revision: `" <> pretty (inferredRevision inferred) <> "`")

  logSticky ""

{-
runAction :: forall r. Members '[Final IO, Embed IO, Resource, Logger, Writer [ProjectClosure]] r => Path Abs Dir -> (Action -> Sem r ()) -> Action -> Sem r ()
runAction basedir enqueue = \case
  ADiscover Discover{..} -> do
    let prettyName = fill 20 (annotate (colorDull Cyan) (pretty discoverName <> " "))

    result <- discoverFunc basedir
      & readFSToIO
      & readFSErrToCLIErr
      & execToIO
      & execErrToCLIErr
      & errorToIOFinal @CLIErr
      & fromExceptionSem @SomeException
      & errorToIOFinal @SomeException
      & outputToIOMonoid @ProjectClosure S.singleton

    undefined -- TODO: possibly rename ADiscover and pass `enqueue` to discover functions?

    case result of
      Left someException -> do
        logWarn $ prettyName <> annotate (color Red) "Discovery failed with uncaught SomeException"
        logWarn $ pretty (show someException) <> line
      Right (Left err) -> do
        logWarn $ prettyName <> annotate (color Red) "Discovery failed"
        logDebug $ pretty (show err) <> line
      Right (Right ()) -> logDebug $ prettyName <> annotate (color Green) "Finished discovery"
-}
{-
  AStrategy (ConfiguredStrategy Strategy{..} opts) -> do
    let prettyName = annotate (color Cyan) (pretty strategyName)
        prettyPath = pretty (toFilePath (strategyModule opts))

    result <- strategyAnalyze opts
      & readFSToIO
      & readFSErrToCLIErr
      & execToIO
      & execErrToCLIErr
      & errorToIOFinal @CLIErr
      & fromExceptionSem @SomeException
      & errorToIOFinal @SomeException

    case result of
      Left someException -> do
        logWarn $ prettyPath <> " " <> prettyName <> " " <> annotate (color Yellow) "Analysis failed with uncaught SomeException"
        logDebug $ pretty (show someException) <> line
      Right (Left err) -> do
        logWarn $ prettyPath <> " " <> prettyName <> " " <> annotate (color Yellow) "Analysis failed"
        logDebug $ pretty (show err) <> line
      Right (Right graph) -> do
        logInfo $ prettyPath <> " " <> prettyName <> " " <> annotate (color Green) "Analyzed"
        logDebug (pretty (show graph))
        output (CompletedStrategy strategyName (strategyModule opts) graph strategyOptimal strategyComplete)
-}

runTask ::
  ( Has (Lift IO) sig m
  , Has Logger sig m
  , MonadIO m
  , Effect sig
  )
  -- => (Task -> m ()) -> Task -> m ()
  => Path Abs Dir -> (Action -> m ()) -> Action -> m ()
runTask dir _ = \case
  ADiscover Discover{..} -> do
  --Task name act -> do
    -- TODO: logging task names?
    let prettyName = fill 20 (annotate (colorDull Cyan) (pretty discoverName <> " "))
    -- let prettyName = fill 20 (annotate (colorDull Cyan) (pretty name <> " "))
    logDebug $ prettyName <> " starting task"

    (closures, result) <- discoverFunc dir
      & runReadFSIO
      & runExecIO
      & runError @ReadFSErr
      & runError @ExecErr
      -- FIXME: catch SomeException again
      -- & fromExceptionSem @SomeException
      -- FIXME: async && pass upward
      & runWriter @[ProjectClosure]

    case result of
      Left someException -> do
        logWarn $ prettyName <> " " <> annotate (color Yellow) "failed with uncaught SomeException"
        logWarn $ pretty (show someException) <> line
      Right (Left err) -> do
        logDebug $ prettyName <> " " <> annotate (color Yellow) "failed"
        logDebug $ pretty (show err) <> line
      Right (Right ()) -> do
        logInfo $ prettyName <> " " <> annotate (color Green) "Analyzed"

updateProgress :: Has Logger sig m => Progress -> m ()
updateProgress Progress{..} =
  logSticky ( "[ "
            <> annotate (color Cyan) (pretty pQueued)
            <> " Waiting / "
            <> annotate (color Yellow) (pretty pRunning)
            <> " Running / "
            <> annotate (color Green) (pretty pCompleted)
            <> " Completed"
            <> " ]" )

data Action =
    ADiscover Discover
  -- | AStrategy ConfiguredStrategy
