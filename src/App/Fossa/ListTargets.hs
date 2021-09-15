{-# LANGUAGE RecordWildCards #-}

module App.Fossa.ListTargets (
  listTargetsMain,
) where

--import App.Fossa.Analyze (discoverFuncs)
import App.Types (BaseDir (..))
import Control.Carrier.AtomicCounter
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.Finally
import Control.Carrier.StickyLogger (StickyLogger, logSticky', runStickyLogger)
import Control.Carrier.TaskPool
import Control.Concurrent (getNumCapabilities)
import Control.Effect.Debug (Debug)
import Control.Effect.Lift (Lift)
import Data.Foldable (for_)
import Data.Set qualified as Set
import Data.Set.NonEmpty
import Discovery.Projects (withDiscoveredProjects)
import Effect.Exec
import Effect.Logger
import Effect.ReadFS
import Path
import Path.IO (makeRelative)
import Strategy.Bundler qualified as Bundler
import Strategy.Cargo qualified as Cargo
import Strategy.Carthage qualified as Carthage
import Strategy.Cocoapods qualified as Cocoapods
import Strategy.Composer qualified as Composer
import Strategy.Conda qualified as Conda
import Strategy.Glide qualified as Glide
import Strategy.Godep qualified as Godep
import Strategy.Gomodules qualified as Gomodules
import Strategy.Googlesource.RepoManifest qualified as RepoManifest
import Strategy.Gradle qualified as Gradle
import Strategy.Haskell.Cabal qualified as Cabal
import Strategy.Haskell.Stack qualified as Stack
import Strategy.Leiningen qualified as Leiningen
import Strategy.Maven qualified as Maven
import Strategy.Mix qualified as Mix
import Strategy.Npm qualified as Npm
import Strategy.NuGet.Nuspec qualified as Nuspec
import Strategy.NuGet.PackageReference qualified as PackageReference
import Strategy.NuGet.PackagesConfig qualified as PackagesConfig
import Strategy.NuGet.Paket qualified as Paket
import Strategy.NuGet.ProjectAssetsJson qualified as ProjectAssetsJson
import Strategy.NuGet.ProjectJson qualified as ProjectJson
import Strategy.Pub qualified as Pub
import Strategy.Python.Pipenv qualified as Pipenv
import Strategy.Python.Poetry qualified as Poetry
import Strategy.Python.Setuptools qualified as Setuptools
import Strategy.RPM qualified as RPM
import Strategy.Rebar3 qualified as Rebar3
import Strategy.Scala qualified as Scala
import Strategy.Yarn qualified as Yarn
import Types (BuildTarget (..), DiscoveredProject (..), FoundTargets (..))

listTargetsMain :: BaseDir -> IO ()
listTargetsMain (BaseDir basedir) = do
  capabilities <- getNumCapabilities

  ignoreDebug
    . withDefaultLogger SevInfo
    . runStickyLogger SevInfo
    . runFinally
    . withTaskPool capabilities updateProgress
    . runReadFSIO
    . runExecIO
    . runAtomicCounter
    $ runAll basedir

runAll ::
  ( Has ReadFS sig m
  , Has Exec sig m
  , Has Logger sig m
  , Has TaskPool sig m
  , Has (Lift IO) sig m
  , Has AtomicCounter sig m
  , Has Debug sig m
  ) =>
  Path Abs Dir ->
  m ()
runAll basedir = do
  single Bundler.discover
  single Cabal.discover
  single Cargo.discover
  single Carthage.discover
  single Cocoapods.discover
  single Composer.discover
  single Conda.discover
  single Glide.discover
  single Godep.discover
  single Gomodules.discover
  single Gradle.discover
  single Leiningen.discover
  single Maven.discover
  single Mix.discover
  single Npm.discover
  single Nuspec.discover
  single PackageReference.discover
  single PackagesConfig.discover
  single Paket.discover
  single Pipenv.discover
  single Poetry.discover
  single ProjectAssetsJson.discover
  single ProjectJson.discover
  single Pub.discover
  single RPM.discover
  single Rebar3.discover
  single RepoManifest.discover
  single Scala.discover
  single Setuptools.discover
  single Stack.discover
  single Yarn.discover
  where
    single f = withDiscoveredProjects f basedir (printSingle basedir)

printSingle :: Has Logger sig m => Path Abs Dir -> DiscoveredProject a -> m ()
printSingle basedir project = do
  let maybeRel = makeRelative basedir (projectPath project)

  case maybeRel of
    Nothing -> pure ()
    Just rel -> do
      logInfo $
        "Found project: "
          <> pretty (projectType project)
          <> "@"
          <> pretty (toFilePath rel)

      case projectBuildTargets project of
        ProjectWithoutTargets -> do
          logInfo $
            "Found target: "
              <> pretty (projectType project)
              <> "@"
              <> pretty (toFilePath rel)
        FoundTargets targets -> for_ (Set.toList $ toSet targets) $ \target -> do
          logInfo $
            "Found target: "
              <> pretty (projectType project)
              <> "@"
              <> pretty (toFilePath rel)
              <> ":"
              <> pretty (unBuildTarget target)

updateProgress :: Has StickyLogger sig m => Progress -> m ()
updateProgress Progress{..} =
  logSticky'
    ( "[ "
        <> annotate (color Cyan) (pretty pQueued)
        <> " Waiting / "
        <> annotate (color Yellow) (pretty pRunning)
        <> " Running / "
        <> annotate (color Green) (pretty pCompleted)
        <> " Completed"
        <> " ]"
    )
