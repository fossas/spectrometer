module Strategy.Dart.PubSpecLock (
  analyzePubLockFile,
  PackageName (..),
  PubLockContent (..),
  PubLockPkgMetadata (..),
  logIgnoredPkgs,

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
import Graphing (Graphing, deep, direct, edge, empty)
import Path
import Types (GraphBreadth (..))

newtype PackageName = PackageName {unPackageName :: Text} deriving (Show, Eq, Ord, FromJSONKey)
newtype PubLockContent = PubLockContent {packages :: Map PackageName PubLockPkgMetadata} deriving (Show, Eq, Ord)

-- | Represents Pub Dependency's metadata from lock file.
data PubLockPkgMetadata = PubLockPkgMetadata
  { pubLockPkgIsDirect :: Bool
  , pubLockPkgSource :: PubDepSource
  , pubLockPkgVersion :: Maybe VerConstraint
  , pubLockPkgEnvironment :: [DepEnvironment]
  }
  deriving (Generic, Show, Eq, Ord)

-- | Represents Pub Dependency's source.
data PubDepSource
  = PubLockPkgSdkSource {sdkName :: Text}
  | PubLockPkgGitSource {gitUrl :: Text, ref :: Text}
  | PubLockPkgHostedSource {hostPkgName :: Maybe Text, hostUrl :: Maybe Text}
  | PubLockPkgPathSource {hostPath :: Text}
  deriving (Show, Eq, Ord)

instance FromJSON PackageName where
  parseJSON (AesonTypes.String pkg) = pure $ PackageName pkg
  parseJSON _ = fail "failed to parse package's name"

instance FromJSON PubLockContent where
  parseJSON = Yaml.withObject "pubspec.lock content" $ \o -> do
    packages <- o .: "packages"
    pure $ PubLockContent packages

instance FromJSON PubLockPkgMetadata where
  parseJSON = Yaml.withObject "pubspec.lock content package" $ \o -> do
    pubLockPkgIsDirect <- isDirect <$> o .: "dependency"
    pubLockPkgSource <- o .: "description"
    pubLockPkgVersion <- optional (CEq <$> o .: "version")
    pubLockPkgEnvironment <- getEnvironment <$> o .: "dependency"
    pure $ PubLockPkgMetadata pubLockPkgIsDirect pubLockPkgSource pubLockPkgVersion pubLockPkgEnvironment
    where
      isDirect :: Text -> Bool
      isDirect candidate = Text.isInfixOf "direct" candidate

      getEnvironment :: Text -> [DepEnvironment]
      getEnvironment candidate
        | Text.isInfixOf "main" candidate = [EnvProduction]
        | Text.isInfixOf "dev" candidate = [EnvDevelopment]
        | otherwise = []

instance FromJSON PubDepSource where
  parseJSON (Yaml.String v) = pure $ PubLockPkgSdkSource v
  parseJSON (Yaml.Object o) =
    asum
      [ PubLockPkgGitSource <$> o .: "url" <*> o .: "ref"
      , PubLockPkgPathSource <$> o .: "path"
      , PubLockPkgHostedSource <$> o .:? "name" <*> o .:? "url"
      ]
  parseJSON (_) = fail "could not parse pub dependency's metadata"

isSupported :: PubLockPkgMetadata -> Bool
isSupported meta =
  case pubLockPkgSource meta of
    PubLockPkgSdkSource _ -> False
    PubLockPkgPathSource _ -> False
    PubLockPkgHostedSource _ _ -> True
    PubLockPkgGitSource _ _ -> True

-- Transforms Package into Dependency.
toDependency :: PackageName -> PubLockPkgMetadata -> Dependency
toDependency pkgName meta =
  Dependency
    { dependencyType = depType
    , dependencyName = depName
    , dependencyVersion = depVersion
    , dependencyLocations = depLocation
    , dependencyEnvironments = pubLockPkgEnvironment meta
    , dependencyTags = Map.empty
    }
  where
    depType :: DepType
    depType = case pubLockPkgSource meta of
      PubLockPkgGitSource{} -> GitType
      _ -> PubType

    depName :: Text
    depName = case pubLockPkgSource meta of
      PubLockPkgGitSource gitUrl _ -> gitUrl
      _ -> unPackageName pkgName

    depVersion :: Maybe VerConstraint
    depVersion = case pubLockPkgSource meta of
      PubLockPkgGitSource _ ref -> Just $ CEq ref
      _ -> pubLockPkgVersion meta

    depLocation :: [Text]
    depLocation = case pubLockPkgSource meta of
      PubLockPkgHostedSource _ (Just hostUrl) -> [hostUrl]
      _ -> []

-- | Builds the dependency graphing from pubspec.lock's content.
-- Edges are not reported.
buildGraph :: PubLockContent -> Graphing Dependency
buildGraph lockContent = foldr deep graphOfDirects transitiveDeps
  where
    supportedPkgs :: Map PackageName PubLockPkgMetadata
    supportedPkgs = Map.filter isSupported $ packages lockContent

    getDeps :: (PubLockPkgMetadata -> Bool) -> [Dependency]
    getDeps f = Map.elems $ Map.mapWithKey toDependency $ Map.filter f supportedPkgs

    transitiveDeps :: [Dependency]
    transitiveDeps = getDeps $ not . pubLockPkgIsDirect

    graphOfDirects :: Graphing Dependency
    graphOfDirects = foldr direct empty $ getDeps pubLockPkgIsDirect

-- | Logs packages ignored from the analyses.
logIgnoredPkgs :: Has Logger sig m => PubLockContent -> m ()
logIgnoredPkgs lockContent = for_ notSupportedDepsMsgs (logDebug . pretty)
  where
    notSupportedDepsMsgs :: [Text]
    notSupportedDepsMsgs = map (<> " : ignored in analyses. Dependency's source is not supported!") notSupportedPkgs

    notSupportedPkgs :: [Text]
    notSupportedPkgs = map (unPackageName . fst) (Map.toList $ Map.filter isSupported $ packages lockContent)

-- | Analyze Lock File.
analyzePubLockFile ::
  (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m, Has Logger sig m) =>
  Path Abs File ->
  m (Graphing Dependency, GraphBreadth)
analyzePubLockFile lockFile = do
  lockContents <- context "Reading pubspec.lock" $ readContentsYaml lockFile
  _ <- logIgnoredPkgs lockContents
  context "building graphing from pubspec.lock only" $ pure (buildGraph lockContents, Partial)
