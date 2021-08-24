module Strategy.Node (
  discover,
) where

import Control.Effect.Diagnostics (Diagnostics, combineSuccessful, context, fromMaybeText, recover, (<||>))

import Discovery.Walk (
  WalkStep (WalkSkipSome),
  findFileNamed,
  walk',
 )
import Effect.Exec (Exec, Has)
import Effect.ReadFS (ReadFS)
import Path (Abs, Dir, File, Path)
import Strategy.Node.NpmList qualified as NpmList
import Strategy.Node.NpmLock qualified as NpmLock
import Strategy.Node.PackageJson qualified as PackageJson
import Strategy.Node.Yarn.V1.YarnLock qualified as V1
import Strategy.Node.Yarn.V2.YarnLock qualified as V2
import Types (
  DependencyResults (..),
  DiscoveredProject (..),
  GraphBreadth (..),
 )

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has ReadFS rsig run, Has Exec rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
discover dir = context "Npm" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [NpmProject]
findProjects = walk' $ \dir _ files -> do
  case findFileNamed "package.json" files of
    Nothing -> pure ([], WalkSkipSome ["node_modules"])
    Just packageJson -> do
      let yarnlock = findFileNamed "yarn.lock" files
      let packageLock = findFileNamed "package-lock.json" files

      let project =
            NpmProject
              { npmDir = dir
              , npmPackageJson = packageJson
              , npmPackageLock = packageLock
              , npmYarnLock = yarnlock
              }

      pure ([project], WalkSkipSome ["node_modules"])

data NpmProject = NpmProject
  { npmDir :: Path Abs Dir
  , npmPackageJson :: Path Abs File
  , npmPackageLock :: Maybe (Path Abs File)
  , npmYarnLock :: Maybe (Path Abs File)
  }
  deriving (Eq, Ord, Show)

mkProject :: (Has Exec sig n, Has ReadFS sig n, Has Diagnostics sig n) => NpmProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "npm"
    , projectBuildTargets = mempty
    , projectDependencyResults = const $ getDeps project
    , projectPath = npmDir project
    , projectLicenses = pure []
    }

getDeps :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => NpmProject -> m DependencyResults
getDeps project = do
  -- This is lazily evaluated, and can be computed twice if runYarn uses it and fails.
  let packageJsonResult = analyzeNpmJson project
      runYarn = context "Yarn" $ analyzeYarn project packageJsonResult
      runNpm = context "Npm" $ analyzeNpmList project <||> analyzeNpmLock project <||> packageJsonResult
  context "Package.json" $ runYarn <||> runNpm

-- In this function, we have to be careful not to use the package.json result until it's needed.
analyzeYarn :: (Has Diagnostics sig m, Has ReadFS sig m) => NpmProject -> m DependencyResults -> m DependencyResults
analyzeYarn project pkgJsonResult = do
  -- Fail early if there's no yarn.lock file.
  yarnLock <- fromMaybeText "No yarn.lock file found, skipping yarn analysis" $ npmYarnLock project
  -- Fail early if both versions fail.
  yarnGraph <- V1.analyze yarnLock <||> V2.analyze yarnLock
  -- Now that we have successful yarn results, we can combine with package.json
  let pkgJsonGraph = dependencyGraph <$> pkgJsonResult
  graph <- combineSuccessful "Unreachable" [pure yarnGraph, pkgJsonGraph]
  -- [/path/to/package.json] if successful, otherwise []
  pkgJsonManifests <- concat <$> recover (dependencyManifestFiles <$> pkgJsonResult)
  -- Combine the successful manifests
  pure $ DependencyResults graph Complete (yarnLock : pkgJsonManifests)

analyzeNpmList :: (Has Exec sig m, Has Diagnostics sig m) => NpmProject -> m DependencyResults
analyzeNpmList project = do
  graph <- context "npm-list analysis" . NpmList.analyze' $ npmDir project
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = maybe [npmPackageJson project] pure $ npmPackageLock project
      }

analyzeNpmLock :: (Has ReadFS sig m, Has Diagnostics sig m) => NpmProject -> m DependencyResults
analyzeNpmLock project = do
  lockFile <- fromMaybeText "No package-lock.json present in the project" (npmPackageLock project)
  graph <- context "package-lock.json analysis" . NpmLock.analyze' $ lockFile
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [lockFile]
      }

analyzeNpmJson :: (Has ReadFS sig m, Has Diagnostics sig m) => NpmProject -> m DependencyResults
analyzeNpmJson project = do
  graph <- context "package.json analysis" . PackageJson.analyze' $ npmPackageJson project
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Partial
      , dependencyManifestFiles = [npmPackageJson project]
      }
