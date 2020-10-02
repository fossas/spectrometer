{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module App.Fossa.Analyze
  ( analyzeMain
  , ScanDestination(..)
  , discoverFuncs
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
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Discovery.Filters
import Discovery.Projects (withDiscoveredProjects)
import Effect.Exec
import Effect.Logger
import Effect.ReadFS
import Network.HTTP.Types (urlEncode)
import Path
import Path.IO (makeRelative)
import qualified Srclib.Converter as Srclib
import Srclib.Types (Locator (..), parseLocator)
import qualified Strategy.Bundler as Bundler
import qualified Strategy.Cargo as Cargo
import qualified Strategy.Carthage as Carthage
import qualified Strategy.Cocoapods as Cocoapods
import qualified Strategy.Composer as Composer
import qualified Strategy.Gomodules as Gomodules
import qualified Strategy.Godep as Godep
import qualified Strategy.Googlesource.RepoManifest as RepoManifest
import qualified Strategy.Gradle as Gradle
import qualified Strategy.Haskell.Cabal as Cabal
import qualified Strategy.Haskell.Stack as Stack
import qualified Strategy.Leiningen as Leiningen
import qualified Strategy.Maven as Maven
import qualified Strategy.Npm as Npm
import qualified Strategy.Python.Setuptools as Setuptools
import qualified Strategy.Rebar3 as Rebar3
import qualified Strategy.RPM as RPM
import qualified Strategy.Scala as Scala
import qualified Strategy.Yarn as Yarn
import Text.URI (URI)
import qualified Text.URI as URI
import Types
import VCS.Git (fetchGitContributors)

data ScanDestination
  = UploadScan URI ApiKey ProjectMetadata -- ^ upload to fossa with provided api key and base url
  | OutputStdout

analyzeMain :: BaseDir -> Severity -> ScanDestination -> OverrideProject -> Bool -> [BuildTargetFilter] -> IO ()
analyzeMain basedir logSeverity destination project unpackArchives filters = withLogger logSeverity $
  analyze basedir destination project unpackArchives filters

discoverFuncs ::
  ( Has (Lift IO) sig m,
    MonadIO m,
    Has ReadFS sig m,
    Has Exec sig m,
    Has Logger sig m,
    Has Diag.Diagnostics sig m
  ) =>
  -- | Discover functions
  [Path Abs Dir -> m [NewProject]]
discoverFuncs =
  [ Bundler.discover',
    Cargo.discover',
    Carthage.discover',
    Cocoapods.discover',
    Gradle.discover',
    Rebar3.discover',
    Gomodules.discover',
    Godep.discover',
    Setuptools.discover',
    Maven.discover',
    Leiningen.discover',
    Composer.discover',
    Cabal.discover',
    Stack.discover',
    Yarn.discover',
    Npm.discover',
    Scala.discover',
    RPM.discover',
    RepoManifest.discover'
  ]

runDependencyAnalysis ::
  (Has (Lift IO) sig m, Has Logger sig m, Has (Output ProjectResult) sig m) =>
  -- | Analysis base directory
  BaseDir ->
  [BuildTargetFilter] ->
  NewProject ->
  m ()
runDependencyAnalysis (BaseDir basedir) filters project = do
  case applyFiltersToProject basedir filters project of
    Nothing -> logInfo $ "Skipping " <> pretty (projectType project) <> " project at " <> viaShow (projectPath project) <> ": no filters matched"
    Just targets -> do
      logInfo $ "Analyzing " <> pretty (projectType project) <> " project at " <> pretty (toFilePath (projectPath project))
      graphResult <- sendIO . Diag.runDiagnosticsIO $ projectDependencyGraph project targets
      Diag.withResult SevWarn graphResult (output . mkResult project)

applyFiltersToProject :: Path Abs Dir -> [BuildTargetFilter] -> NewProject -> Maybe (Set BuildTarget)
applyFiltersToProject basedir filters NewProject{..} =
  case makeRelative basedir projectPath of
    Nothing -> Just projectBuildTargets -- FIXME: this is required for --unpack-archives to continue to work
    Just rel -> applyFilters filters projectType rel projectBuildTargets

analyze ::
  ( Has (Lift IO) sig m
  , Has Logger sig m
  , MonadIO m
  )
  => BaseDir
  -> ScanDestination
  -> OverrideProject
  -> Bool -- ^ whether to unpack archives
  -> [BuildTargetFilter]
  -> m ()
analyze basedir destination override unpackArchives filters = do
  capabilities <- sendIO getNumCapabilities

  (projectResults, ()) <-
    runOutput @ProjectResult
      . runExecIO
      . runReadFSIO
      . runFinally
      . withTaskPool capabilities updateProgress
      $ withDiscoveredProjects discoverFuncs unpackArchives (unBaseDir basedir) (runDependencyAnalysis basedir filters)

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
