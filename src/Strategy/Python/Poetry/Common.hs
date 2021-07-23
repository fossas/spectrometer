module Strategy.Python.Poetry.Common (
  getPoetryBuildBackend,
  toMap,
  pyProjectDeps,
  logIgnoredDeps,

  -- * for testing
  supportedPyProjectDep,
  supportedPoetryLockDep,
) where

import Data.Foldable (asum)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, toLower)
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvOther, EnvProduction, EnvTesting),
  DepType (GitType, PipType, URLType),
  Dependency (..),
  VerConstraint (
    CEq
  ),
 )
import Effect.Logger (Has, Logger (..), Pretty (pretty), logDebug)
import Strategy.Python.Poetry.PoetryLock (PackageName (..), PoetryLock (..), PoetryLockPackage (..), PoetryLockPackageSource (..))
import Strategy.Python.Poetry.PyProject (
  PoetryDependency (..),
  PyProject (..),
  PyProjectBuildSystem (..),
  PyProjectPoetry (..),
  PyProjectPoetryDetailedVersionDependency (..),
  PyProjectPoetryGitDependency (..),
  PyProjectPoetryPathDependency (..),
  PyProjectPoetryUrlDependency (..),
  toDependencyVersion,
 )

-- | Gets build backend of pyproject.
getPoetryBuildBackend :: PyProject -> Maybe Text
getPoetryBuildBackend project = buildBackend <$> pyprojectBuildSystem project

-- | Supported pyproject dependencies.
supportedPyProjectDep :: PoetryDependency -> Bool
supportedPyProjectDep (PyProjectPoetryPathDependencySpec _) = False
supportedPyProjectDep _ = True

-- | Logs all ignored dependencies for debugger.
logIgnoredDeps :: Has Logger sig m => PyProject -> Maybe PoetryLock -> m ()
logIgnoredDeps pyproject poetryLock = sequence_ $ (logDebug . pretty) <$> notSupportedDepsMsgs
  where
    notSupportedDepsMsgs :: [Text]
    notSupportedDepsMsgs = (<> ": ignored in poetry project. Dependency's source is not supported!") <$> notSupportedDeps

    notSupportedDeps :: [Text]
    notSupportedDeps = case poetryLock of
      Nothing -> notSupportedPyProjectDevDeps ++ notSupportedPyProjectDeps
      Just pl -> (unPackageName . poetryLockPackageName) <$> filter (not . supportedPoetryLockDep) (poetryLockPackages pl)

    notSupportedPyProjectDevDeps :: [Text]
    notSupportedPyProjectDevDeps =
      Map.keys $
        Map.filter (not . supportedPyProjectDep) $ maybe Map.empty devDependencies (pyprojectPoetry pyproject)

    notSupportedPyProjectDeps :: [Text]
    notSupportedPyProjectDeps =
      Map.keys $
        Map.filter (not . supportedPyProjectDep) $ maybe Map.empty dependencies (pyprojectPoetry pyproject)

-- | Not supported poetry lock package.
supportedPoetryLockDep :: PoetryLockPackage -> Bool
supportedPoetryLockDep pkg = Just "file" /= (poetryLockPackageSourceType <$> poetryLockPackageSource pkg)

-- | Gets pyproject dependencies.
pyProjectDeps :: PyProject -> [Dependency]
pyProjectDeps project = filter notNamedPython $ map snd allDeps
  where
    -- pyproject typically includes python as dependency that has to be ignored
    notNamedPython = (/= "python") . dependencyName

    supportedDevDeps :: Map Text PoetryDependency
    supportedDevDeps = Map.filter supportedPyProjectDep $ maybe Map.empty devDependencies (pyprojectPoetry project)

    supportedProdDeps :: Map Text PoetryDependency
    supportedProdDeps = Map.filter supportedPyProjectDep $ maybe Map.empty dependencies (pyprojectPoetry project)

    toDependency :: [DepEnvironment] -> Map Text PoetryDependency -> Map Text Dependency
    toDependency depEnvs = Map.mapWithKey $ poetrytoDependency depEnvs

    allDeps :: [(Text, Dependency)]
    allDeps = Map.toList prodDeps ++ Map.toList devDeps
      where
        prodDeps = toDependency [EnvProduction] supportedProdDeps
        devDeps = toDependency [EnvDevelopment] supportedDevDeps

-- | Gets Dependency from `PoetryDependency` and it's `DepEnvironment`.
poetrytoDependency :: [DepEnvironment] -> Text -> PoetryDependency -> Dependency
poetrytoDependency depEnvs name deps =
  Dependency
    { dependencyType = depType
    , dependencyName = depName
    , dependencyVersion = depVersion
    , dependencyLocations = depLocations
    , dependencyEnvironments = depEnvironment
    , dependencyTags = depTags
    }
  where
    depType = case deps of
      PyProjectPoetryGitDependencySpec _ -> GitType
      PyProjectPoetryUrlDependencySpec _ -> URLType
      _ -> PipType

    depName = case deps of
      PoetryTextVersion _ -> name
      PyProjectPoetryDetailedVersionDependencySpec _ -> name
      PyProjectPoetryGitDependencySpec ds -> gitUrl ds
      PyProjectPoetryUrlDependencySpec ds -> sourceUrl ds
      PyProjectPoetryPathDependencySpec ds -> sourcePath ds

    depVersion = case deps of
      PoetryTextVersion ds -> toDependencyVersion ds
      PyProjectPoetryDetailedVersionDependencySpec ds -> toDependencyVersion (poetryDependencyVersion ds)
      PyProjectPoetryGitDependencySpec ds -> case asum [gitTag ds, gitRev ds, gitBranch ds] of
        Nothing -> Nothing
        Just version -> Just $ CEq version
      _ -> Nothing

    depEnvironment = depEnvs
    depLocations = []
    depTags = Map.empty

-- | Maps poetry lock package to map of package name and associated dependency.
toMap :: [PoetryLockPackage] -> Map.Map PackageName Dependency
toMap pkgs = Map.fromList $ (\x -> (lowerCasePkgName x, toDependency x)) <$> (filter supportedPoetryLockDep pkgs)
  where
    lowerCasePkgName :: PoetryLockPackage -> PackageName
    lowerCasePkgName pkg = PackageName (toLower $ unPackageName $ poetryLockPackageName pkg)

    toDependency :: PoetryLockPackage -> Dependency
    toDependency pkg =
      Dependency
        { dependencyType = toDepType (poetryLockPackageSource pkg)
        , dependencyName = toDepName pkg
        , dependencyVersion = toDepVersion pkg
        , dependencyLocations = toDepLocs pkg
        , dependencyEnvironments = toDepEnvironment pkg
        , dependencyTags = Map.empty
        }

    toDepName :: PoetryLockPackage -> Text
    toDepName lockPkg = case (poetryLockPackageSource lockPkg) of
      Nothing -> unPackageName $ poetryLockPackageName lockPkg
      Just lockPkgSrc -> case poetryLockPackageSourceType lockPkgSrc of
        "legacy" -> unPackageName $ poetryLockPackageName lockPkg
        _ -> poetryLockPackageSourceUrl lockPkgSrc

    toDepType :: Maybe PoetryLockPackageSource -> DepType
    toDepType Nothing = PipType
    toDepType (Just lockPkgSrc) = case poetryLockPackageSourceType lockPkgSrc of
      "git" -> GitType
      "url" -> URLType
      _ -> PipType

    toDepLocs :: PoetryLockPackage -> [Text]
    toDepLocs pkg = case poetryLockPackageSource pkg of
      Nothing -> []
      Just lockPkgSrc -> case poetryLockPackageSourceType lockPkgSrc of
        "legacy" -> [poetryLockPackageSourceUrl lockPkgSrc]
        _ -> []

    toDepVersion :: PoetryLockPackage -> Maybe VerConstraint
    toDepVersion pkg = Just $
      CEq $
        fromMaybe (poetryLockPackageVersion pkg) $ do
          lockPkgSrc <- poetryLockPackageSource pkg
          ref <- poetryLockPackageSourceReference lockPkgSrc
          if poetryLockPackageSourceType lockPkgSrc /= "legacy" then Just ref else Nothing

    toDepEnvironment :: PoetryLockPackage -> [DepEnvironment]
    toDepEnvironment pkg = case poetryLockPackageCategory pkg of
      "dev" -> [EnvDevelopment]
      "main" -> [EnvProduction]
      "test" -> [EnvTesting]
      other -> [EnvOther other]
