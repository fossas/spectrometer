module Strategy.RPM (
  discover,
  getSpecDeps,
  getTypeFromLine,

  RPMDependency(..),
  RequiresType(..),
  Dependencies(..)
) where

import Prologue

import Control.Effect.Error
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import DepTypes
import Discovery.Walk
import Effect.Grapher
import Effect.ReadFS
import Graphing (Graphing)
import Types

newtype SpecFileLabel =
  RequiresType DepEnvironment
  deriving (Eq, Ord, Show, Generic)
  
data RPMDependency = RPMDependency 
  { rpmDepName :: Text,
    rpmConstraint :: Maybe VerConstraint
  } deriving (Eq, Ord, Show, Generic)

{- These two structures should make it very easy to add other headers like `Requires`
-}
newtype RequiresType =
    BuildRequires RPMDependency
    deriving (Eq, Ord, Show, Generic)

newtype Dependencies = Dependencies
    { depBuildRequires :: [RPMDependency] 
    } deriving (Eq, Ord, Show, Generic)
{-
Yep...  Those ones right above me.  Kinda rude.  They're actually standing on my head.
-}

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \dir _ files ->
  case find (\f -> ".spec" `isSuffixOf` fileName f) files of
    Nothing -> pure WalkContinue
    Just specFile -> do
      runSimpleStrategy "rpm-spec" RustGroup $ fmap (mkProjectClosure dir) (analyze specFile)
      pure WalkSkipAll

analyze :: (Has ReadFS sig m, Has (Error ReadFSErr) sig m) => Path Rel File -> m (Graphing Dependency)
analyze specFile = do
  specFileText <- readContentsText specFile
  pure . buildGraph $ getSpecDeps specFileText

mkProjectClosure :: Path Rel Dir ->  Graphing Dependency -> ProjectClosureBody
mkProjectClosure dir graph = ProjectClosureBody
  { bodyModuleDir    = dir
  , bodyDependencies = dependencies
  , bodyLicenses     = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = graph
    , dependenciesOptimal  = NotOptimal
    , dependenciesComplete = NotComplete
    }

toDependency :: RPMDependency -> Set SpecFileLabel -> Dependency
toDependency pkg = foldr applyLabel Dependency 
  { dependencyType = CargoType 
  , dependencyName = rpmDepName pkg
  , dependencyVersion = rpmConstraint pkg
  , dependencyLocations = []
  , dependencyEnvironments = []
  , dependencyTags = M.empty
  }
  where
    applyLabel :: SpecFileLabel -> Dependency -> Dependency
    applyLabel (RequiresType env) dep = dep { dependencyEnvironments = env : dependencyEnvironments dep }

buildGraph :: Dependencies -> Graphing Dependency
buildGraph Dependencies{..} = run . withLabeling toDependency $ do
  traverse_ direct depBuildRequires
  traverse (flip label $ RequiresType EnvDevelopment) depBuildRequires

-- buildConstraint tail = RPMConstraint version <$> comparator
buildConstraint :: Text -> Maybe VerConstraint
buildConstraint tail = constraint
  where
    (comparatorStr, rawVersion) = splitOnceOn " " $ T.strip tail
    version = T.strip rawVersion
    constraint = case T.strip comparatorStr of
      "<=" -> Just $ CLessOrEq version
      "<" -> Just $ CLess version
      ">=" -> Just $ CGreaterOrEq version
      ">" -> Just $ CGreater version
      "=" -> Just $ CEq version
      "==" -> Just $ CEq version
      _ -> Nothing

splitOnceOn :: Text -> Text -> (Text, Text)
splitOnceOn needle haystack = (head, strippedTail)
  where
    len = T.length needle
    (head, tail) = T.breakOn needle haystack
    strippedTail = T.drop len tail

getTypeFromLine :: Text -> Maybe RequiresType
getTypeFromLine line = safeReq
  where
    (header, value) = splitOnceOn ": " line
    (pkgName, rawConstraint) = splitOnceOn " " $ T.strip value
    isSafeName name = not $ "%{" `T.isInfixOf` name
    safeReq = if isSafeName pkgName then req else Nothing
    req = case header of
      "BuildRequires" -> Just . BuildRequires . RPMDependency pkgName $ buildConstraint rawConstraint
      _ -> Nothing

buildDependencies :: [RequiresType] -> Dependencies
buildDependencies = foldr addDep blankDeps
  where
    addDep req deps = case req of
      BuildRequires dep -> deps { depBuildRequires = dep : depBuildRequires deps }
    blankDeps = Dependencies []

getSpecDeps :: Text -> Dependencies
getSpecDeps = buildDependencies . mapMaybe getTypeFromLine . T.lines
-- getSpecDeps = buildDependencies . concatMap (parseLine . splitLine) . T.lines
