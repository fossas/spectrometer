{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Strategy.Python.Poetry.PoetryLock (
  PoetryLock (..),
  PoetryMetadata (..),
  PoetryLockPackage (..),
  PackageName (..),
  PoetryLockPackageSource (..),
  PoetryLockDependencySpec (..),
  ObjectVersion (..),
  poetryLockCodec,
  toMap,
) where

import Control.Applicative (Alternative ((<|>)))
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text, toLower)
import DepTypes
import Toml (TomlCodec, (.=))
import Toml qualified

-- | Content of poetry lock file
data PoetryLock = PoetryLock
  { poetryLockPackages :: [PoetryLockPackage]
  , poetryLockMetadata :: PoetryMetadata
  }
  deriving (Eq, Ord, Show)

newtype PackageName = PackageName {unPackageName :: Text} deriving (Eq, Ord, Show)

poetryLockCodec :: TomlCodec PoetryLock
poetryLockCodec =
  PoetryLock
    <$> Toml.list poetryLockPackageCodec "package" .= poetryLockPackages
    <*> Toml.table poetryMetadataCodec "metadata" .= poetryLockMetadata

-- | Metadata of poetry lock file
data PoetryMetadata = PoetryMetadata
  { poetryMetadataLockVersion :: Text
  , poetryMetadataContentHash :: Text
  , poetryMetadataPythonVersions :: Text
  }
  deriving (Eq, Ord, Show)

poetryMetadataCodec :: TomlCodec PoetryMetadata
poetryMetadataCodec =
  PoetryMetadata
    <$> Toml.text "lock-version" .= poetryMetadataLockVersion
    <*> Toml.text "content-hash" .= poetryMetadataContentHash
    <*> Toml.text "python-versions" .= poetryMetadataPythonVersions

-- | Source of poetry pacakge found in lock file.
data PoetryLockPackageSource = PoetryLockPackageSource
  { poetryLockPackageSourceType :: Text
  , poetryLockPackageSourceUrl :: Text
  , poetryLockPackageSourceReference :: Maybe Text
  , poetryLockPackageSourceResolvedReference :: Maybe Text
  }
  deriving (Eq, Ord, Show)

-- | Poetry package entry found in poetry lock file
data PoetryLockPackage = PoetryLockPackage
  { poetryLockPackageName :: PackageName
  , poetryLockPackageVersion :: Text
  , poetryLockPackageCategory :: Text
  , poetryLockPackageOptional :: Bool
  , poetryLockPackagePythonVersions :: Text
  , poetryLockPackageDependencies :: Map Text PoetryLockDependencySpec
  , poetryLockPackageSource :: Maybe PoetryLockPackageSource
  }
  deriving (Eq, Ord, Show)

poetryLockPackageCodec :: TomlCodec PoetryLockPackage
poetryLockPackageCodec =
  PoetryLockPackage
    <$> Toml.diwrap (Toml.text "name") .= poetryLockPackageName
    <*> Toml.text "version" .= poetryLockPackageVersion
    <*> Toml.text "category" .= poetryLockPackageCategory
    <*> Toml.bool "optional" .= poetryLockPackageOptional
    <*> Toml.text "python-versions" .= poetryLockPackagePythonVersions
    <*> Toml.tableMap Toml._KeyText poetryLockPackagePoetryLockDependencySpecCodec "dependencies" .= poetryLockPackageDependencies
    <*> Toml.dioptional (Toml.table poetryLockPackageSourceCodec "source") .= poetryLockPackageSource

poetryLockPackageSourceCodec :: TomlCodec PoetryLockPackageSource
poetryLockPackageSourceCodec =
  PoetryLockPackageSource
    <$> Toml.text "type" .= poetryLockPackageSourceType
    <*> Toml.text "url" .= poetryLockPackageSourceUrl
    <*> Toml.dioptional (Toml.text "reference") .= poetryLockPackageSourceReference
    <*> Toml.dioptional (Toml.text "resolved_reference") .= poetryLockPackageSourceResolvedReference

data PoetryLockDependencySpec
  = TextVersion Text
  | ObjectVersionSpec ObjectVersion
  | MultipleObjectVersionSpec [ObjectVersion]
  deriving (Eq, Ord, Show)

newtype ObjectVersion = ObjectVersion
  { unObjectVersion :: Text
  }
  deriving (Eq, Ord, Show)

objectVersionCodec :: TomlCodec ObjectVersion
objectVersionCodec =
  ObjectVersion
    <$> Toml.text "version" .= unObjectVersion

matchTextVersion :: PoetryLockDependencySpec -> Maybe Text
matchTextVersion (TextVersion version) = Just version
matchTextVersion _ = Nothing

matchObjectVersionSpec :: PoetryLockDependencySpec -> Maybe ObjectVersion
matchObjectVersionSpec (ObjectVersionSpec version) = Just version
matchObjectVersionSpec _ = Nothing

matchMultipleObjectVersionSpec :: PoetryLockDependencySpec -> Maybe [ObjectVersion]
matchMultipleObjectVersionSpec (MultipleObjectVersionSpec version) = Just version
matchMultipleObjectVersionSpec _ = Nothing

poetryLockPackagePoetryLockDependencySpecCodec :: Toml.Key -> TomlCodec PoetryLockDependencySpec
poetryLockPackagePoetryLockDependencySpecCodec key =
  Toml.dimatch matchTextVersion TextVersion (Toml.text key)
    <|> Toml.dimatch matchObjectVersionSpec ObjectVersionSpec (Toml.table objectVersionCodec key)
    <|> Toml.dimatch matchMultipleObjectVersionSpec MultipleObjectVersionSpec (Toml.list objectVersionCodec key)

-- | Maps poetry lock package to map of package name and associated dependency.
toMap :: [PoetryLockPackage] -> Map PackageName Dependency
toMap pkgs =
  M.fromList
    [ ( PackageName (toLower $ unPackageName $ poetryLockPackageName pkg)
      , Dependency
          { dependencyType = toDepType (poetryLockPackageSource pkg)
          , dependencyName = toDepName pkg
          , dependencyVersion = toDepVersion pkg
          , dependencyLocations = toDepLocs pkg
          , dependencyEnvironments = toDepEnvironment pkg
          , dependencyTags = M.empty
          }
      )
    | pkg <- pkgs
    ]
  where
    toDepName :: PoetryLockPackage -> Text
    toDepName plp = case (poetryLockPackageSource plp) of
      Nothing -> unPackageName $ poetryLockPackageName plp
      Just plps -> case poetryLockPackageSourceType plps of
        "legacy" -> unPackageName $ poetryLockPackageName plp
        _ -> poetryLockPackageSourceUrl plps

    toDepType :: Maybe PoetryLockPackageSource -> DepType
    toDepType Nothing = PipType
    toDepType (Just plps) = case poetryLockPackageSourceType plps of
      "git" -> GitType
      "url" -> URLType
      "legacy" -> PipType
      _ -> UserType

    toDepLocs :: PoetryLockPackage -> [Text]
    toDepLocs pkg = case poetryLockPackageSource pkg of
      Nothing -> []
      Just plps -> case poetryLockPackageSourceType plps of
        "legacy" -> [poetryLockPackageSourceUrl plps]
        _ -> []

    -- Use resolved reference (for git sources), otherwise use resolved reference
    toDepVersion :: PoetryLockPackage -> Maybe VerConstraint
    toDepVersion pkg = case poetryLockPackageSource pkg of
      Nothing -> Just $ CEq $ poetryLockPackageVersion pkg
      Just plps -> case poetryLockPackageSourceReference plps of
        Nothing -> Just $ CEq $ poetryLockPackageVersion pkg
        Just txt -> case poetryLockPackageSourceType plps of
          "legacy" -> Just $ CEq $ poetryLockPackageVersion pkg
          _ -> Just $ CEq txt

    toDepEnvironment :: PoetryLockPackage -> [DepEnvironment]
    toDepEnvironment pkg = case poetryLockPackageCategory pkg of
      "dev" -> [EnvDevelopment]
      "main" -> [EnvProduction]
      "test" -> [EnvTesting]
      other -> [EnvOther other]
