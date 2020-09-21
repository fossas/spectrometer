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
import App.Fossa.Analyze.Project (ProjectResult(..), BestStrategy(..), Project(..), mkProjects, mkResult)
import App.Fossa.FossaAPIV1 (uploadAnalysis', ProjectMetadata, UploadResponse (..), uploadAnalysis, uploadContributors)
import App.Fossa.ProjectInference (inferProject, mergeOverride)
import App.Types
import qualified Control.Carrier.Diagnostics as Diag
import Control.Carrier.Error.Either
import Control.Carrier.Finally
import Control.Carrier.Output.IO
import Control.Carrier.TaskPool
import Control.Concurrent
import Control.Effect.Exception
import Control.Effect.Lift (sendIO)
import Control.Exception.Extra (safeCatch)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
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
import qualified Strategy.Cargo as Cargo
import qualified Strategy.Carthage as Carthage
import qualified Strategy.Clojure as Clojure
import qualified Strategy.Cocoapods as Cocoapods
import qualified Strategy.Cocoapods.Podfile as Podfile
import qualified Strategy.Cocoapods.PodfileLock as PodfileLock
import qualified Strategy.Composer as Composer
import qualified Strategy.Erlang.Rebar3Tree as Rebar3Tree
import qualified Strategy.Gomodules as Gomodules
import qualified Strategy.Godep as Godep
import qualified Strategy.Go.GlideLock as GlideLock
import qualified Strategy.Go.GoList as GoList
import qualified Strategy.Go.Gomod as Gomod
import qualified Strategy.Go.GopkgLock as GopkgLock
import qualified Strategy.Go.GopkgToml as GopkgToml
import qualified Strategy.Googlesource.RepoManifest as RepoManifest
import qualified Strategy.Gradle as Gradle
import qualified Strategy.Haskell.Cabal as Cabal
import qualified Strategy.Haskell.Stack as Stack
import qualified Strategy.Maven as Maven
import qualified Strategy.Maven.PluginStrategy as MavenPlugin
import qualified Strategy.Maven.Pom as MavenPom
import qualified Strategy.Node.NpmList as NpmList
import qualified Strategy.Node.NpmLock as NpmLock
import qualified Strategy.Node.PackageJson as PackageJson
import qualified Strategy.Node.YarnLock as YarnLock
import qualified Strategy.NuGet.Nuspec as Nuspec
import qualified Strategy.NuGet.PackageReference as PackageReference
import qualified Strategy.NuGet.PackagesConfig as PackagesConfig
import qualified Strategy.NuGet.Paket as Paket
import qualified Strategy.NuGet.ProjectAssetsJson as ProjectAssetsJson
import qualified Strategy.NuGet.ProjectJson as ProjectJson
import qualified Strategy.Rebar3 as Rebar3
import qualified Strategy.Python.Pipenv as Pipenv
import qualified Strategy.Python.ReqTxt as ReqTxt
import qualified Strategy.Python.SetupPy as SetupPy
import qualified Strategy.Python.Setuptools as Setuptools
import qualified Strategy.RPM as RPM
import qualified Strategy.Ruby.BundleShow as BundleShow
import qualified Strategy.Ruby.GemfileLock as GemfileLock
import qualified Strategy.Scala as Scala
import Text.URI (URI)
import qualified Text.URI as URI
import Types
import VCS.Git (fetchGitContributors)
import Data.Traversable (for)

data ScanDestination
  = UploadScan URI ApiKey ProjectMetadata -- ^ upload to fossa with provided api key and base url
  | OutputStdout

analyzeMain :: BaseDir -> Severity -> ScanDestination -> OverrideProject -> Bool -> IO ()
analyzeMain basedir logSeverity destination project unpackArchives = withLogger logSeverity $
  analyze' basedir destination project unpackArchives

analyze ::
  ( Has (Lift IO) sig m
  , Has Logger sig m
  , MonadIO m
  )
  => BaseDir
  -> ScanDestination
  -> OverrideProject
  -> Bool -- ^ whether to unpack archives
  -> m ()
analyze basedir destination override unpackArchives = runFinally $ do
  capabilities <- sendIO getNumCapabilities

  (closures,(failures,())) <- runOutput @ProjectClosure . runOutput @ProjectFailure . runExecIO . runReadFSIO $
    withTaskPool capabilities updateProgress $
      if unpackArchives
        then discoverWithArchives $ unBaseDir basedir
        else discover $ unBaseDir basedir

  traverse_ (logDebug . Diag.renderFailureBundle . projectFailureCause) failures

  logSticky ""

  let projects = mkProjects closures
      result = buildResult projects failures

  traverse_ (logInfo . ("Found " <>) . pretty . BestStrategy) projects

  case destination of
    OutputStdout -> logStdout $ pretty (decodeUtf8 (Aeson.encode result))
    UploadScan baseurl apiKey metadata -> do
      revision <- mergeOverride override <$> inferProject (unBaseDir basedir)

      logInfo ""
      logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
      logInfo ("Using revision: `" <> pretty (projectRevision revision) <> "`")
      logInfo ("Using branch: `" <> pretty (projectBranch revision) <> "`")

      uploadResult <- Diag.runDiagnostics $ uploadAnalysis basedir baseurl apiKey revision metadata projects
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

-- FIXME: move these elsewhere
runDiagnosticsIO :: Has (Lift IO) sig m => Diag.DiagnosticsC m a -> m (Either Diag.FailureBundle (Diag.ResultBundle a))
runDiagnosticsIO act = Diag.runDiagnostics $ act `safeCatch` (\(e :: SomeException) -> Diag.fatal e)

witherListM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
witherListM f [] = pure []
witherListM f (x:xs) = f x >>= \res -> case res of
  Nothing -> witherListM f xs
  Just x' -> (x':) <$> witherListM f xs

emitFailures :: Has Logger sig m => Either Diag.FailureBundle (Diag.ResultBundle a) -> m (Maybe a)
emitFailures (Left e) = do
  logError $ Diag.renderFailureBundle e
  pure Nothing
emitFailures (Right a) = pure . Just $ Diag.resultValue a

emitProjectFailures :: Has Logger sig m => (NewProject (Diag.DiagnosticsC m), Either Diag.FailureBundle (Diag.ResultBundle a)) -> m (Maybe (NewProject (Diag.DiagnosticsC m), a))
emitProjectFailures (prj, Left e) = do
  logDebug $ Diag.renderFailureBundle e
  pure Nothing
emitProjectFailures (prj, Right a) = pure $ Just (prj, Diag.resultValue a)

analyze' ::
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
analyze' basedir destination override unpackArchives = runFinally $ do
  capabilities <- sendIO getNumCapabilities

  let newDiscovers = [Cocoapods.discover', Rebar3.discover', Gomodules.discover', Godep.discover', Setuptools.discover', Maven.discover']

  -- FIXME: fix diagnostics situation (gross code in emit*)
  -- FIXME: parallelism on discover
  -- FIXME: parallelism on analysis of discovered projects
  -- FIXME: unpackArchives
  runExecIO . runReadFSIO . withTaskPool capabilities updateProgress $ do
    projectsAndFailures <- traverse (runDiagnosticsIO . apply (unBaseDir basedir)) newDiscovers
    projects <- concat <$> witherListM emitFailures projectsAndFailures
    depGraphsAndFailures <- for projects $ \project -> do
      depGraph <- runDiagnosticsIO $ projectDependencyGraph project
      pure (project, depGraph)
    depGraphs <- witherListM emitProjectFailures depGraphsAndFailures

    logSticky ""

    let projectResults = map (uncurry mkResult) depGraphs

    case destination of
      OutputStdout -> logStdout $ pretty (decodeUtf8 (Aeson.encode (buildResult' projectResults)))
      UploadScan baseurl apiKey metadata -> do
        revision <- mergeOverride override <$> inferProject (unBaseDir basedir)

        logInfo ""
        logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
        logInfo ("Using revision: `" <> pretty (projectRevision revision) <> "`")
        logInfo ("Using branch: `" <> pretty (projectBranch revision) <> "`")

        uploadResult <- Diag.runDiagnostics $ uploadAnalysis' basedir baseurl apiKey revision metadata projectResults
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
buildResult' :: [ProjectResult] -> Aeson.Value
buildResult' projects = Aeson.object
  [ "projects" .= map buildProject' projects
  ]

buildProject' :: ProjectResult -> Aeson.Value
buildProject' project = Aeson.object
  [ "path" .= projectResultPath project
  , "type" .= projectResultType project
  , "graph" .= graphingToGraph (projectResultGraph project)
  ]

buildResult :: [Project] -> [ProjectFailure] -> Aeson.Value
buildResult projects failures = Aeson.object
  [ "projects" .= projects
  , "failures" .= map renderFailure failures
  , "sourceUnits" .= map Srclib.toSourceUnit projects
  ]

renderFailure :: ProjectFailure -> Aeson.Value
renderFailure failure = Aeson.object
  [ "name" .= projectFailureName failure
  , "cause" .= show (Diag.renderFailureBundle (projectFailureCause failure))
  ]

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover dir = traverse_ (forkTask . apply dir) discoverFuncs

discoverWithArchives :: HasDiscover sig m => Path Abs Dir -> m ()
discoverWithArchives dir = traverse_ (forkTask . apply dir) (Archive.discover discoverWithArchives : discoverFuncs)

apply :: a -> (a -> b) -> b
apply x f = f x

discoverFuncs :: HasDiscover sig m => [Path Abs Dir -> m ()]
discoverFuncs =
  [ Rebar3Tree.discover

  , GoList.discover
  , Gomod.discover
  , GopkgToml.discover
  , GopkgLock.discover
  , GlideLock.discover

  , Gradle.discover

  , MavenPlugin.discover
  , MavenPom.discover

  , PackageJson.discover
  , NpmLock.discover
  , NpmList.discover
  , YarnLock.discover

  , PackagesConfig.discover
  , PackageReference.discover
  , ProjectAssetsJson.discover
  , ProjectJson.discover
  , Nuspec.discover
  , Paket.discover

  , Pipenv.discover
  , SetupPy.discover
  , ReqTxt.discover

  , RepoManifest.discover

  , BundleShow.discover
  , GemfileLock.discover

  , Carthage.discover

  , Podfile.discover
  , PodfileLock.discover

  , Composer.discover

  , Clojure.discover
  
  , Cargo.discover

  , RPM.discover

  , Scala.discover

  , Cabal.discover

  , Stack.discover
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
