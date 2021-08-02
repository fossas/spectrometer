module Strategy.Dart.PubSpecLock (
  analyzePubLockFile,
  PackageName (..),
  PubLockContent (..),
  PubLockPackageMetadata (..),
  logIgnoredPackages,

  -- * for testing
  buildGraph,
  PubDepSource (..),
  toDependency,
  isSupported,
) where

import Control.Applicative (optional)
import Control.Effect.Diagnostics (Diagnostics, context)
import Data.Aeson.Types (
  FromJSONKey,
 )
import Data.Aeson.Types qualified as AesonTypes
import Data.Foldable (asum, for_)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Yaml (FromJSON (parseJSON), (.:), (.:?))
import Data.Yaml qualified as Yaml
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (GitType, PubType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.Exec (Exec, Has)
import Effect.Logger (Logger (..), Pretty (pretty), logDebug)
import Effect.ReadFS (ReadFS, readContentsYaml)
import GHC.Generics (Generic)
import Graphing (Graphing, deep, direct, empty)
import Path
import Types (GraphBreadth (..))

newtype PackageName = PackageName {unPackageName :: Text} deriving (Show, Eq, Ord, FromJSONKey)
newtype PubLockContent = PubLockContent {packages :: Map PackageName PubLockPackageMetadata} deriving (Show, Eq, Ord)

-- | Represents Pub Dependency's metadata from lock file.
data PubLockPackageMetadata = PubLockPackageMetadata
  { pubLockPackageIsDirect :: Bool
  , pubLockPackageSource :: PubDepSource
  , pubLockPackageVersion :: Maybe VerConstraint
  , pubLockPackageEnvironment :: [DepEnvironment]
  }
  deriving (Generic, Show, Eq, Ord)

-- | Represents Pub Dependency's source.
data PubDepSource
  = PubLockPackageSdkSource {sdkName :: Text}
  | PubLockPackageGitSource {gitUrl :: Text, ref :: Text}
  | PubLockPackageHostedSource {hostPackageName :: Maybe Text, hostUrl :: Maybe Text}
  | PubLockPackagePathSource {hostPath :: Text}
  deriving (Show, Eq, Ord)

instance FromJSON PackageName where
  parseJSON (AesonTypes.String packageName) = pure $ PackageName packageName
  parseJSON _ = fail "failed to parse package's name"

instance FromJSON PubLockContent where
  parseJSON = Yaml.withObject "pubspec.lock content" $ \o -> do
    packages <- o .: "packages"
    pure $ PubLockContent packages

instance FromJSON PubLockPackageMetadata where
  parseJSON = Yaml.withObject "pubspec.lock content package" $ \o -> do
    pubLockPackageIsDirect <- isDirect <$> o .: "dependency"
    pubLockPackageSource <- o .: "description"
    pubLockPackageVersion <- optional (CEq <$> o .: "version")
    pubLockPackageEnvironment <- getEnvironment <$> o .: "dependency"
    pure $ PubLockPackageMetadata pubLockPackageIsDirect pubLockPackageSource pubLockPackageVersion pubLockPackageEnvironment
    where
      isDirect :: Text -> Bool
      isDirect candidate = Text.isInfixOf "direct" candidate

      getEnvironment :: Text -> [DepEnvironment]
      getEnvironment candidate
        | Text.isInfixOf "main" candidate = [EnvProduction]
        | Text.isInfixOf "dev" candidate = [EnvDevelopment]
        | otherwise = []

instance FromJSON PubDepSource where
  parseJSON (Yaml.String v) = pure $ PubLockPackageSdkSource v
  parseJSON (Yaml.Object o) =
    asum
      [ PubLockPackageGitSource <$> o .: "url" <*> o .: "ref"
      , PubLockPackagePathSource <$> o .: "path"
      , PubLockPackageHostedSource <$> o .:? "name" <*> o .:? "url"
      ]
  parseJSON _ = fail "could not parse pub dependency's metadata"

isSupported :: PubLockPackageMetadata -> Bool
isSupported meta =
  case pubLockPackageSource meta of
    PubLockPackageSdkSource _ -> False
    PubLockPackagePathSource _ -> False
    PubLockPackageHostedSource _ _ -> True
    PubLockPackageGitSource _ _ -> True

-- Transforms Package into Dependency.
toDependency :: PackageName -> PubLockPackageMetadata -> Dependency
toDependency pkg meta =
  Dependency
    { dependencyType = depType
    , dependencyName = depName
    , dependencyVersion = depVersion
    , dependencyLocations = depLocation
    , dependencyEnvironments = pubLockPackageEnvironment meta
    , dependencyTags = Map.empty
    }
  where
    depType :: DepType
    depType = case pubLockPackageSource meta of
      PubLockPackageGitSource{} -> GitType
      _ -> PubType

    depName :: Text
    depName = case pubLockPackageSource meta of
      PubLockPackageGitSource gitUrl _ -> gitUrl
      _ -> unPackageName pkg

    depVersion :: Maybe VerConstraint
    depVersion = case pubLockPackageSource meta of
      PubLockPackageGitSource _ ref -> Just $ CEq ref
      _ -> pubLockPackageVersion meta

    depLocation :: [Text]
    depLocation = case pubLockPackageSource meta of
      PubLockPackageHostedSource _ (Just hostUrl) -> [hostUrl]
      _ -> []

-- | Builds the dependency graphing from pubspec.lock's content.
-- Edges are not reported.
buildGraph :: PubLockContent -> Graphing Dependency
buildGraph lockContent = foldr deep graphOfDirects transitiveDependencies
  where
    supportedPackages :: Map PackageName PubLockPackageMetadata
    supportedPackages = Map.filter isSupported $ packages lockContent

    getDependencies :: (PubLockPackageMetadata -> Bool) -> [Dependency]
    getDependencies f = Map.elems $ Map.mapWithKey toDependency $ Map.filter f supportedPackages

    transitiveDependencies :: [Dependency]
    transitiveDependencies = getDependencies $ not . pubLockPackageIsDirect

    graphOfDirects :: Graphing Dependency
    graphOfDirects = foldr direct empty $ getDependencies pubLockPackageIsDirect

logIgnoredPackages :: Has Logger sig m => PubLockContent -> m ()
logIgnoredPackages lockContent = for_ notSupportedDependenciesMsgs (logDebug . pretty)
  where
    notSupportedDependenciesMsgs :: [Text]
    notSupportedDependenciesMsgs = map (<> " : ignored in analyses. Dependency's source is not supported!") notSupportedPackages

    notSupportedPackages :: [Text]
    notSupportedPackages = map (unPackageName . fst) (Map.toList $ Map.filter isSupported $ packages lockContent)

analyzePubLockFile ::
  (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m, Has Logger sig m) =>
  Path Abs File ->
  m (Graphing Dependency, GraphBreadth)
analyzePubLockFile lockFile = do
  lockContents <- context "Reading pubspec.lock" $ readContentsYaml lockFile
  _ <- logIgnoredPackages lockContents
  context "building graphing from pubspec.lock only" $ pure (buildGraph lockContents, Partial)
