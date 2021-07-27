module Strategy.Python.Poetry (
  discover,

  -- * for testing only
  buildGraphWithLock,
  buildPackageNameGraph,
  PoetryProject (..),
) where

import Control.Algebra (Has)
import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, context)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, toLower)
import DepTypes (DepType (..), Dependency (..))
import Discovery.Walk (
  WalkStep (WalkContinue, WalkSkipAll),
  findFileNamed,
  walk',
 )
import Effect.Logger (Logger (..), Pretty (pretty), logDebug)
import Effect.ReadFS (ReadFS, readContentsToml)
import Graphing (Graphing, deep, edge, empty, fromList, gmap, promoteToDirect)
import Path (Abs, Dir, File, Path)
import Strategy.Python.Poetry.Common (getPoetryBuildBackend, logIgnoredDeps, pyProjectDeps, toMap)
import Strategy.Python.Poetry.PoetryLock (PackageName (..), PoetryLock (..), PoetryLockPackage (..), poetryLockCodec)
import Strategy.Python.Poetry.PyProject (PyProject (..), pyProjectCodec)
import Types (DiscoveredProject (..))

newtype PyProjectTomlFile = PyProjectTomlFile {pyProjectTomlPath :: Path Abs File} deriving (Eq, Ord, Show)
newtype PoetryLockFile = PoetryLockFile {poetryLockPath :: Path Abs File} deriving (Eq, Ord, Show)
newtype ProjectDir = ProjectDir {pyProjectPath :: Path Abs Dir} deriving (Eq, Ord, Show)

data PoetryProject = PoetryProject
  { projectDir :: ProjectDir
  , pyProjectToml :: PyProjectTomlFile
  , poetryLock :: Maybe PoetryLockFile
  }
  deriving (Show, Eq, Ord)

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has Logger sig m, Has Logger rsig run, Has ReadFS rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
discover dir = context "Poetry" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

-- | Poetry build backend identifier required in [pyproject.toml](https://python-poetry.org/docs/pyproject/#poetry-and-pep-517).
poetryBuildBackendIdentifier :: Text
poetryBuildBackendIdentifier = "poetry.core.masonry.api"

-- | Reference message text for poetry build backend setting value required in pyproject.toml.
-- Users should configure poetry build backend in pyproject.toml for poetry project discovery.
poetryBuildBackendIdentifierHelpText :: Text
poetryBuildBackendIdentifierHelpText = "Poetry project must use poetry build backend. Please refer to https://python-poetry.org/docs/pyproject/#poetry-and-pep-517."

warnIncorrectBuildBackend :: Has Logger sig m => Text -> m ()
warnIncorrectBuildBackend currentBackend =
  (logDebug . pretty) $
    "pyproject.toml does not use poetry build backend. It uses: "
      <> currentBackend
      <> "\n"
      <> poetryBuildBackendIdentifierHelpText

-- | Finds poetry project by searching for pyproject.toml.
-- If poetry.lock file is also discovered, it is used as a supplement.
findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has Logger sig m) => Path Abs Dir -> m [PoetryProject]
findProjects = walk' $ \dir _ files -> do
  let poetryLockFile = findFileNamed "poetry.lock" files
  let pyprojectFile = findFileNamed "pyproject.toml" files

  case (poetryLockFile, pyprojectFile) of
    (poetry, Just pyproject) -> do
      poetryProject <- readContentsToml pyProjectCodec pyproject
      let project = PoetryProject (ProjectDir dir) (PyProjectTomlFile pyproject) (PoetryLockFile <$> poetry)
      let pyprojectBuildBackend = getPoetryBuildBackend poetryProject

      case pyprojectBuildBackend of
        Nothing -> pure ([], WalkContinue)
        Just pbs ->
          if pbs == poetryBuildBackendIdentifier
            then pure ([project], WalkSkipAll)
            else ([], WalkContinue) <$ warnIncorrectBuildBackend pbs

    -- Without pyproject file, it is unlikely that project is a poetry project. Poetry itself does not work
    -- without [pyproject.toml manifest](https://python-poetry.org/docs/pyproject/).
    (Just _, Nothing) -> context "poetry.lock file found without accompanying pyproject.toml!" $ pure ([], WalkContinue)
    (Nothing, Nothing) -> pure ([], WalkContinue)

mkProject :: (Has ReadFS sig n, Has Diagnostics sig n, Has Logger sig n) => PoetryProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "poetry"
    , projectBuildTargets = mempty
    , projectDependencyGraph = const $ getDeps project
    , projectPath = pyProjectPath $ projectDir project
    , projectLicenses = pure []
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m, Has Logger sig m) => PoetryProject -> m (Graphing Dependency)
getDeps project = do
  context "Poetry" $ context "Static analysis" $ analyze project

-- | Analyzes Poetry Project and creates dependency graph.
analyze ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  PoetryProject ->
  m (Graphing Dependency)
analyze PoetryProject{pyProjectToml, poetryLock} = do
  pyproject <- readContentsToml pyProjectCodec (pyProjectTomlPath pyProjectToml)
  case poetryLock of
    Just lockPath -> do
      poetryLockProject <- readContentsToml poetryLockCodec (poetryLockPath lockPath)
      _ <- logIgnoredDeps pyproject (Just poetryLockProject)
      context "Building dependency graph from pyproject.toml and poetry.lock" $ pure $ buildGraphWithLock poetryLockProject pyproject
    Nothing -> context "Building dependency graph from only pyproject.toml" $ pure $ Graphing.fromList $ pyProjectDeps pyproject

-- | Build graph of `Dependency` only from `PyProject` and `PoetryLock`.
buildGraphWithLock :: PoetryLock -> PyProject -> Graphing Dependency
buildGraphWithLock lockProject poetryProject = promoteToDirect isDirect graph
  where
    -- Dependencies in `poetry.lock` are direct if they're specified in `pyproject.toml`.
    isDirect :: Dependency -> Bool
    isDirect dep = case pyprojectPoetry poetryProject of
      Nothing -> False
      Just _ -> any (\n -> dependencyName n == dependencyName dep) (pyProjectDeps poetryProject)

    lowerCasedPkgName :: PackageName -> PackageName
    lowerCasedPkgName name = PackageName . toLower $ unPackageName name

    graph = gmap pkgNameToDependency $ buildPackageNameGraph $ poetryLockPackages lockProject
    mapOfDependency = toMap $ poetryLockPackages lockProject

    -- Pip packages are [case insensitive](https://www.python.org/dev/peps/pep-0508/#id21), but poetry.lock may use
    -- non-canonical name for reference. Try to lookup with provided casing, otherwise fallback to lower casing.
    --
    -- Poetry.lock when referencing deep dependencies uses non canonical names.
    --
    --  ```toml
    --  [package.dependencies]
    --  MarkupSafe = ">=2.0"
    --  ....
    --
    -- [[package]]
    -- name = "markupsafe"
    -- version = "2.0.1"
    -- ...
    -- ```
    --
    -- TODO: Better approach to handle casing scenarios with poetry.lock file.
    pkgNameToDependency name =
      fromMaybe
        ( Dependency
            { dependencyType = PipType
            , dependencyName = unPackageName name
            , dependencyVersion = Nothing
            , dependencyLocations = []
            , dependencyEnvironments = []
            , dependencyTags = Map.empty
            }
        )
        $ Map.lookup name mapOfDependency
          <|> Map.lookup (lowerCasedPkgName name) mapOfDependency

-- | Builds the Package Name Graph.
-- In python, package names are unique, and only single version of a package can be used.
buildPackageNameGraph :: [PoetryLockPackage] -> Graphing PackageName
buildPackageNameGraph pkgs = foldr deep edges pkgsNoDeps
  where
    pkgsNoDeps :: [PackageName]
    pkgsNoDeps = poetryLockPackageName <$> filter (null . poetryLockPackageDependencies) pkgs

    depsWithEdges :: [PoetryLockPackage]
    depsWithEdges = filter (not . null . poetryLockPackageDependencies) pkgs

    edgeOf :: PoetryLockPackage -> [(PackageName, PackageName)]
    edgeOf p = map tuplify . Map.keys $ (poetryLockPackageDependencies p)
      where
        tuplify :: Text -> (PackageName, PackageName)
        tuplify x = (poetryLockPackageName p, PackageName x)

    edges :: Graphing PackageName
    edges = foldr (uncurry edge) empty (concatMap edgeOf depsWithEdges)
