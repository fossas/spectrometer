{-# LANGUAGE RecordWildCards #-}

module Strategy.Python.Poetry.Common (
  PoetryProject (..),
  buildPyProjectGraph,
  discover,
) where

import Control.Effect.Diagnostics (Diagnostics, context)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (toLower)
import DepTypes
import Discovery.Walk (
  WalkStep (WalkContinue, WalkSkipAll),
  findFileNamed,
  walk',
 )
import Effect.Exec
import Effect.ReadFS (ReadFS, readContentsToml)
import Graphing (Graphing, edge, empty, fromList, gmap, promoteToDirect)
import Path (Abs, Dir, File, Path)
import Strategy.Python.Poetry.PoetryLock (PackageName (..), PoetryLock (..), PoetryLockPackage (..), poetryLockCodec, toMap)
import Strategy.Python.Poetry.PyProject (PyProject (..), getDependencies, pyProjectCodec, usesPoetryBuildSystem)
import Types (DiscoveredProject (..))

newtype PyProjectTomlFile = PyProjectTomlFile {pyProjectTomlPath :: Path Abs File} deriving (Eq, Ord, Show)
newtype PoetryLockFile = PoetryLockFile {poetryLockPath :: Path Abs File} deriving (Eq, Ord, Show)
newtype ProjectDir = ProjectDir {pyProjectPath :: Path Abs Dir} deriving (Eq, Ord, Show)

data PoetryProject
  = PoetryPyProjectOnly ProjectDir PyProjectTomlFile
  | PoetryPyProjectWithLock ProjectDir PyProjectTomlFile PoetryLockFile
  deriving (Eq, Ord, Show)

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has ReadFS rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
discover dir = context "Poetry" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [PoetryProject]
findProjects = walk' $ \dir _ files -> do
  let poetryLockFile = findFileNamed "poetry.lock" files
  let pyprojectFile = findFileNamed "pyproject.toml" files

  case (poetryLockFile, pyprojectFile) of
    (Nothing, Just pyproject) -> do
      poetryProject <- readContentsToml pyProjectCodec pyproject
      if usesPoetryBuildSystem poetryProject
        then pure ([PoetryPyProjectOnly (ProjectDir dir) (PyProjectTomlFile pyproject)], WalkSkipAll)
        else pure ([], WalkContinue)
    (Just poetrylock, Just pyproject) ->
      pure
        (
          [ PoetryPyProjectWithLock (ProjectDir dir) (PyProjectTomlFile pyproject) (PoetryLockFile poetrylock)
          ]
        , WalkSkipAll
        )
    _ -> pure ([], WalkContinue)

mkProject :: (Has ReadFS sig n, Has Diagnostics sig n) => PoetryProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "poetry"
    , projectBuildTargets = mempty
    , projectDependencyGraph = const $ getDeps project
    , projectPath = projectDir
    , projectLicenses = pure []
    }
  where
    projectDir = case project of
      PoetryPyProjectOnly dir _ -> pyProjectPath dir
      PoetryPyProjectWithLock dir _ _ -> pyProjectPath dir

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => PoetryProject -> m (Graphing Dependency)
getDeps project = do
  context "Poetry" $ context "Static analysis" $ analyze project

analyze ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  PoetryProject ->
  m (Graphing Dependency)
analyze project = do
  case project of
    PoetryPyProjectWithLock _ projectTomlFile poetryLockFile -> do
      pyproject <- readContentsToml pyProjectCodec $ pyProjectTomlPath projectTomlFile
      poetryLock <- readContentsToml poetryLockCodec $ poetryLockPath poetryLockFile
      context "Building dependency graph from pyproject.toml and poetry.lock" $ pure (buildGraphWithLock poetryLock pyproject)

    -- Pyproject only reports version constraints of direct dependencies
    PoetryPyProjectOnly _ projectTomlFile -> do
      pyproject <- readContentsToml pyProjectCodec $ pyProjectTomlPath projectTomlFile
      context "Building dependency graph from only pyproject.toml" $ pure (buildPyProjectGraph pyproject)

-- | Build graph of `Dependency` only from `PyProject`.
buildPyProjectGraph :: PyProject -> Graphing Dependency
buildPyProjectGraph (project) = Graphing.fromList $ getDependencies project

-- | Build graph of `Dependency` only from `PyProject` and `PoetryLock`.
buildGraphWithLock :: PoetryLock -> PyProject -> Graphing Dependency
buildGraphWithLock lockProject poetryProject = promoteToDirect (isDirect poetryProject) graph
  where
    -- Checks from `PyProject` if Dependendency is direct dependency.
    -- True if direct dependency, otherwise false.
    isDirect :: PyProject -> Dependency -> Bool
    isDirect pyproject dep = case pyprojectPoetry pyproject of
      Nothing -> False
      Just _ -> any (\n -> dependencyName n == (dependencyName dep)) (getDependencies pyproject)

    graph = gmap pkgNameToDependency (buildPackageNameGraph $ poetryLockPackages lockProject)
    mapOfDependency = toMap $ poetryLockPackages lockProject

    -- Pip packages are case insensitive, but poetry.lock may specify dependency with different casing.
    -- Try to lookup with provided casing, otherwise fallback to lower casing.
    pkgNameToDependency name = case M.lookup name mapOfDependency of
      Nothing ->
        fromMaybe
          Dependency
            { dependencyType = PipType
            , dependencyName = unPackageName name
            , dependencyVersion = Nothing
            , dependencyLocations = []
            , dependencyEnvironments = []
            , dependencyTags = M.empty
            }
          (M.lookup lowerCasePkgName mapOfDependency)
      Just d -> d
      where
        lowerCasePkgName = PackageName{unPackageName = toLower $ unPackageName name}

-- | Builds the Package name graph
-- In python, package names are unique, and only single version of a package can be used.
buildPackageNameGraph :: [PoetryLockPackage] -> Graphing PackageName
buildPackageNameGraph pkgs = foldl addToGraph empty (getPackageNameEdges pkgs)
  where
    addToGraph :: Graphing PackageName -> (PackageName, PackageName) -> Graphing PackageName
    addToGraph g pkgsParentChild = uncurry edge pkgsParentChild g
    getPackageNameEdges = concatMap getEdgesPairs
      where
        getEdgesPairs pkg = concatMap (makeEdge pkg) [allPkgs]
          where
            allPkgs = map (\x -> PackageName{unPackageName = x}) (M.keys $ poetryLockPackageDependencies pkg)
            makeEdge _ [] = []
            makeEdge parent (x : xs) = (poetryLockPackageName parent, x) : (makeEdge (parent) (xs))