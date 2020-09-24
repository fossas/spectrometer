{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module App.Fossa.Analyze
  ( analyzeMain
  , ScanDestination(..)
  ) where

import App.Fossa.Analyze.GraphMangler (graphingToGraph)
import App.Fossa.Analyze.Project (ProjectResult(..), mkResult)
import App.Fossa.FossaAPIV1 (ProjectMetadata, UploadResponse (..), uploadAnalysis, uploadContributors)
import App.Fossa.ProjectInference (inferProject, mergeOverride)
import App.Types
import qualified Control.Carrier.Diagnostics as Diag
import Control.Carrier.Output.IO
import Control.Carrier.Finally
import Control.Carrier.TaskPool
import Control.Concurrent
import Control.Effect.Exception
import Control.Effect.Lift (sendIO)
import Control.Exception.Extra (safeCatch)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.Foldable (for_, traverse_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Effect.Exec
import Effect.Logger
import Effect.ReadFS
import Network.HTTP.Types (urlEncode)
import Path
import qualified Srclib.Converter as Srclib
import Srclib.Types (Locator (..), parseLocator)
import qualified Strategy.Archive as Archive
import qualified Strategy.Cocoapods as Cocoapods
import qualified Strategy.Gomodules as Gomodules
import qualified Strategy.Godep as Godep
import qualified Strategy.Gradle as Gradle
import qualified Strategy.Maven as Maven
import qualified Strategy.Rebar3 as Rebar3
import qualified Strategy.Python.Setuptools as Setuptools
import Text.URI (URI)
import qualified Text.URI as URI
import Types
import VCS.Git (fetchGitContributors)

data ScanDestination
  = UploadScan URI ApiKey ProjectMetadata -- ^ upload to fossa with provided api key and base url
  | OutputStdout

analyzeMain :: BaseDir -> Severity -> ScanDestination -> OverrideProject -> Bool -> IO ()
analyzeMain basedir logSeverity destination project unpackArchives = withLogger logSeverity $
  analyze basedir destination project unpackArchives

-- FIXME: move these elsewhere
runDiagnosticsIO :: Has (Lift IO) sig m => Diag.DiagnosticsC m a -> m (Either Diag.FailureBundle (Diag.ResultBundle a))
runDiagnosticsIO act = Diag.runDiagnostics $ act `safeCatch` (\(e :: SomeException) -> Diag.fatal e)

withResult :: Has Logger sig m => Severity -> Either Diag.FailureBundle (Diag.ResultBundle a) -> (a -> m ()) -> m ()
withResult sev (Left e) _ = Effect.Logger.log sev $ Diag.renderFailureBundle e
withResult _ (Right res) f = f $ Diag.resultValue res

runDependencyAnalysis ::
  (Has (Lift IO) sig m, MonadIO m, Has (Output ProjectResult) sig m, Has TaskPool sig m, Has Logger sig m, Has Finally sig m) =>
  -- | Discover functions
  [Path Abs Dir -> Diag.DiagnosticsC m [NewProject (Diag.DiagnosticsC m)]] ->
  -- | whether to unpack archives
  Bool ->
  Path Abs Dir ->
  m ()
runDependencyAnalysis newDiscovers unpackArchives =
  withDiscoveredProjects newDiscovers unpackArchives $ \project -> do
    graphResult <- runDiagnosticsIO $ projectDependencyGraph project (projectBuildTargets project)
    withResult SevWarn graphResult (output . mkResult project)

withDiscoveredProjects ::
  (Has (Lift IO) sig m, MonadIO m, Has TaskPool sig m, Has Logger sig m, Has Finally sig m) =>
  -- | Discover functions
  [Path Abs Dir -> Diag.DiagnosticsC m [NewProject (Diag.DiagnosticsC m)]] ->
  -- | whether to unpack archives
  Bool ->
  (NewProject (Diag.DiagnosticsC m) -> m ()) ->
  Path Abs Dir ->
  m ()
withDiscoveredProjects discoverFuncs unpackArchives f basedir = do
  for_ discoverFuncs $ \discover -> forkTask $ do
    projectsResult <- runDiagnosticsIO (discover basedir)
    withResult SevError projectsResult (traverse_ (forkTask . f))

  when unpackArchives $ Archive.discover (withDiscoveredProjects discoverFuncs unpackArchives f) basedir

analyze ::
  forall sig m.
  ( Has (Lift IO) sig m
  , Has Logger sig m
  , MonadIO m
  )
  => BaseDir
  -> ScanDestination
  -> OverrideProject
  -> Bool -- ^ whether to unpack archives
  -> m ()
analyze basedir destination override unpackArchives = do
  capabilities <- sendIO getNumCapabilities

  let newDiscovers = [Cocoapods.discover', Gradle.discover', Rebar3.discover', Gomodules.discover', Godep.discover', Setuptools.discover', Maven.discover']

  -- FIXME: print projects we found
  (projectResults, ()) <-
    runOutput @ProjectResult
      . runExecIO
      . runReadFSIO
      . runFinally
      . withTaskPool capabilities updateProgress
      $ runDependencyAnalysis newDiscovers unpackArchives (unBaseDir basedir)

  logSticky ""

  case destination of
    OutputStdout -> logStdout $ pretty (decodeUtf8 (Aeson.encode (buildResult projectResults)))
    UploadScan baseurl apiKey metadata -> do
      revision <- mergeOverride override <$> inferProject (unBaseDir basedir)

      logInfo ""
      logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
      logInfo ("Using revision: `" <> pretty (projectRevision revision) <> "`")
      logInfo ("Using branch: `" <> pretty (projectBranch revision) <> "`")

      uploadResult <- Diag.runDiagnostics $ uploadAnalysis basedir baseurl apiKey revision metadata projectResults
      case uploadResult of
        Left failure -> logError (Diag.renderFailureBundle failure)
        Right success -> do
          let resp = Diag.resultValue success
          logInfo $ vsep
            [ "============================================================"
            , ""
            , "    View FOSSA Report:"
            , "    " <> pretty (fossaProjectUrl baseurl (uploadLocator resp) (projectBranch revision))
            , ""
            , "============================================================"
            ]
          traverse_ (\err -> logError $ "FOSSA error: " <> viaShow err) (uploadError resp)

          contribResult <- Diag.runDiagnostics $ runExecIO $ tryUploadContributors (unBaseDir basedir) baseurl apiKey $ uploadLocator resp
          case contribResult of
            Left failure -> logDebug (Diag.renderFailureBundle failure)
            Right _ -> pure ()

tryUploadContributors ::
  ( Has Diag.Diagnostics sig m,
    Has Exec sig m,
    Has (Lift IO) sig m
  ) =>
  Path x Dir ->
  URI ->
  ApiKey ->
  -- | Locator
  Text ->
  m ()
tryUploadContributors baseDir baseUrl apiKey locator = do
  contributors <- fetchGitContributors baseDir
  uploadContributors baseUrl apiKey locator contributors

fossaProjectUrl :: URI -> Text -> Text -> Text
fossaProjectUrl baseUrl rawLocator branch = URI.render baseUrl <> "projects/" <> encodedProject <> "/refs/branch/" <> branch <> "/" <> encodedRevision
  where
    Locator{locatorFetcher, locatorProject, locatorRevision} = parseLocator rawLocator

    underBS :: (ByteString -> ByteString) -> Text -> Text
    underBS f = TE.decodeUtf8 . f . TE.encodeUtf8

    encodedProject = underBS (urlEncode True) (locatorFetcher <> "+" <> locatorProject)
    encodedRevision = underBS (urlEncode True) (fromMaybe "" locatorRevision)

-- FIXME: move these elsewhere?
buildResult :: [ProjectResult] -> Aeson.Value
buildResult projects = Aeson.object
  [ "projects" .= map buildProject projects
  , "sourceUnits" .= map Srclib.toSourceUnit projects
  ]

buildProject :: ProjectResult -> Aeson.Value
buildProject project = Aeson.object
  [ "path" .= projectResultPath project
  , "type" .= projectResultType project
  , "graph" .= graphingToGraph (projectResultGraph project)
  ]

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
