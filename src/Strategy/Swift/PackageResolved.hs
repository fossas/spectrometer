module Strategy.Swift.PackageResolved (
  SwiftPackageResolvedFile (..),
  SwiftResolvedPackage (..),
  resolvedDependenciesOf,
) where

import Data.Aeson (
  FromJSON (parseJSON),
  Object,
  withObject,
  (.:),
  (.:?),
 )
import Data.Aeson.Types (Parser)
import Data.Foldable (asum)
import Data.Text (Text)
import DepTypes (DepType (GitType), Dependency (..), VerConstraint (CEq))

data SwiftPackageResolvedFile = SwiftPackageResolvedFile
  { version :: Integer
  , pinnedPackages :: [SwiftResolvedPackage]
  }
  deriving (Show, Eq, Ord)

data SwiftResolvedPackage = SwiftResolvedPackage
  { package :: Text
  , repositoryURL :: Text
  , repositoryBranch :: Maybe Text
  , repositoryRevision :: Maybe Text
  , repositoryVersion :: Maybe Text
  }
  deriving (Show, Eq, Ord)

instance FromJSON SwiftPackageResolvedFile where
  parseJSON = withObject "Package.resolved content" $ \obj -> do
    version <- obj .: "version"
    pinnedPackages <- obj .: "object" |> "pins"
    pure $ SwiftPackageResolvedFile version pinnedPackages

(|>) :: FromJSON a => Parser Object -> Text -> Parser a
(|>) parser key = do
  obj <- parser
  obj .: key

(|?>) :: FromJSON a => Parser (Maybe Object) -> Text -> Parser (Maybe a)
(|?>) parser key = do
  obj <- parser
  case obj of
    Nothing -> pure Nothing
    Just o -> o .:? key

instance FromJSON SwiftResolvedPackage where
  parseJSON = withObject "Package.resolved pinned object" $ \obj -> do
    package <- obj .: "package"
    repositoryURL <- obj .: "repositoryURL"
    repositoryBranch <- obj .:? "state" |?> "branch"
    repositoryRevision <- obj .:? "state" |?> "revision"
    repositoryVersion <- obj .:? "state" |?> "version"
    pure $ SwiftResolvedPackage package repositoryURL repositoryBranch repositoryRevision repositoryVersion

-- Note, Package.resolved does not include path dependencies.
resolvedDependenciesOf :: SwiftPackageResolvedFile -> [Dependency]
resolvedDependenciesOf resolvedContent = map toDependency $ pinnedPackages resolvedContent
  where
    toDependency :: SwiftResolvedPackage -> Dependency
    toDependency pkg =
      Dependency
        { dependencyType = GitType
        , dependencyName = repositoryURL pkg
        , dependencyVersion =
            CEq
              <$> asum
                [ repositoryBranch pkg
                , repositoryRevision pkg
                , repositoryVersion pkg
                ]
        , dependencyLocations = []
        , dependencyEnvironments = []
        , dependencyTags = mempty
        }
