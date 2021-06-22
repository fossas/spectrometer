{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module App.Fossa.YamlDeps (
  CustomDependency (..),
  ReferencedDependency (..),
  VendoredDependency (..),
  YamlDependencies (..),
  analyzeFossaDepsYaml,
) where

import Control.Effect.Diagnostics (Diagnostics, context, fatalText)
import Control.Monad (when)
import Data.Aeson (
  FromJSON (parseJSON),
  withObject,
  (.!=),
  (.:),
  (.:?),
 )

import App.Fossa.ArchiveUploader
import Control.Effect.Lift
import Data.Aeson.Extra
import Data.Aeson.Types (Parser)
import Data.Functor.Extra ((<$$>))
import Data.List.NonEmpty qualified as NE
import Data.String.Conversion (toText)
import Data.Text (Text, unpack)
import DepTypes (DepType (..))
import Effect.ReadFS (ReadFS, doesFileExist, readContentsYaml)
import Fossa.API.Types
import Path
import Srclib.Converter (depTypeToFetcher)
import Srclib.Types (AdditionalDepData (..), Locator (..), SourceUnit (..), SourceUnitBuild (..), SourceUnitDependency (SourceUnitDependency), SourceUserDefDep (..))

analyzeFossaDepsYaml :: (Has Diagnostics sig m, Has ReadFS sig m, Has (Lift IO) sig m) => Path Abs Dir -> Maybe ApiOpts -> m (Maybe SourceUnit)
analyzeFossaDepsYaml root maybeApiOpts = do
  maybeDepsFile <- findFossaDepsFile root
  case maybeDepsFile of
    Nothing -> pure Nothing
    Just depsFile -> do
      yamldeps <- context "Reading fossa-deps file" $ readContentsYaml depsFile
      -- If the file exists and we have no dependencies to report, that's a failure.
      when (hasNoDeps yamldeps) $ fatalText "No dependencies found in fossa-deps file"
      context "Converting fossa-deps to partial API payload" $ Just <$> toSourceUnit root yamldeps maybeApiOpts

findFossaDepsFile :: (Has Diagnostics sig m, Has ReadFS sig m) => Path Abs Dir -> m (Maybe (Path Abs File))
findFossaDepsFile root = do
  let ymlFile = root </> $(mkRelFile "fossa-deps.yml")
      yamlFile = root </> $(mkRelFile "fossa-deps.yaml")
  ymlExists <- doesFileExist ymlFile
  yamlExists <- doesFileExist yamlFile
  case (ymlExists, yamlExists) of
    (True, True) -> fatalText "Found '.yml' and '.yaml' files when searching for fossa-deps, only one is permitted if present."
    (True, False) -> pure $ Just ymlFile
    (False, True) -> pure $ Just yamlFile
    (False, False) -> pure Nothing

toSourceUnit :: (Has Diagnostics sig m, Has (Lift IO) sig m) => Path Abs Dir -> YamlDependencies -> Maybe ApiOpts -> m SourceUnit
toSourceUnit root YamlDependencies{..} maybeApiOpts = do
  archiveLocators <- case maybeApiOpts of
    Nothing -> pure $ archiveNoUploadSourceUnit vendoredDependencies
    Just apiOpts -> archiveUploadSourceUnit root apiOpts vendoredDependencies

  let renderedPath = toText root
      referenceLocators = refToLocator <$> referencedDependencies
      additional = toAdditionalData <$> NE.nonEmpty customDependencies
      build = toBuildData <$> NE.nonEmpty (referenceLocators <> archiveLocators)
  pure $
    SourceUnit
      { sourceUnitName = renderedPath
      , sourceUnitManifest = renderedPath
      , sourceUnitType = "user-specific-yaml"
      , sourceUnitBuild = build
      , additionalData = additional
      }

toBuildData :: NE.NonEmpty Locator -> SourceUnitBuild
toBuildData locators =
  SourceUnitBuild
    { buildArtifact = "default"
    , buildSucceeded = True
    , buildImports = NE.toList locators
    , buildDependencies = map addEmptyDep $ NE.toList locators
    }

refToLocator :: ReferencedDependency -> Locator
refToLocator ReferencedDependency{..} =
  Locator
    { locatorFetcher = depTypeToFetcher locDepType
    , locatorProject = locDepName
    , locatorRevision = locDepVersion
    }

addEmptyDep :: Locator -> SourceUnitDependency
addEmptyDep loc = SourceUnitDependency loc []

toAdditionalData :: NE.NonEmpty CustomDependency -> AdditionalDepData
toAdditionalData deps = AdditionalDepData{userDefinedDeps = map toSrc $ NE.toList deps}
  where
    toSrc CustomDependency{..} =
      SourceUserDefDep
        { srcUserDepName = customName
        , srcUserDepVersion = customVersion
        , srcUserDepLicense = customLicense
        , srcUserDepDescription = customDescription
        , srcUserDepUrl = customUrl
        }

hasNoDeps :: YamlDependencies -> Bool
hasNoDeps YamlDependencies{..} = null referencedDependencies && null customDependencies && null vendoredDependencies

data YamlDependencies = YamlDependencies
  { referencedDependencies :: [ReferencedDependency]
  , customDependencies :: [CustomDependency]
  , vendoredDependencies :: [VendoredDependency]
  }
  deriving (Eq, Ord, Show)

data ReferencedDependency = ReferencedDependency
  { locDepName :: Text
  , locDepType :: DepType
  , locDepVersion :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data CustomDependency = CustomDependency
  { customName :: Text
  , customVersion :: Text
  , customLicense :: Text
  , customDescription :: Maybe Text
  , customUrl :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON YamlDependencies where
  parseJSON = withObject "YamlDependencies" $ \obj ->
    YamlDependencies <$ (obj .:? "version" >>= isMissingOr1)
      <*> (obj .:? "referenced-dependencies" .!= [])
      <*> (obj .:? "custom-dependencies" .!= [])
      <*> (obj .:? "vendored-dependencies" .!= [])
    where
      isMissingOr1 :: Maybe Int -> Parser ()
      isMissingOr1 (Just x) | x /= 1 = fail $ "Invalid fossa-deps version: " <> show x
      isMissingOr1 _ = pure ()

depTypeParser :: Text -> Parser DepType
depTypeParser text = case depTypeFromText text of
  Just t -> pure t
  Nothing -> fail $ "dep type: " <> unpack text <> " not supported"

instance FromJSON ReferencedDependency where
  parseJSON = withObject "ReferencedDependency" $ \obj ->
    ReferencedDependency <$> obj .: "name"
      <*> (obj .: "type" >>= depTypeParser)
      <*> (unTextLike <$$> obj .:? "version")
      <* forbidMembers "referenced dependencies" ["license", "description", "url", "path"] obj

instance FromJSON CustomDependency where
  parseJSON = withObject "CustomDependency" $ \obj ->
    CustomDependency <$> obj .: "name"
      <*> (unTextLike <$> obj .: "version")
      <*> obj .: "license"
      <*> obj .:? "description"
      <*> obj .:? "url"
      <* forbidMembers "custom dependencies" ["type", "path"] obj

-- Parse supported dependency types into their respective type or return Nothing.
depTypeFromText :: Text -> Maybe DepType
depTypeFromText text = case text of
  "cargo" -> Just CargoType
  "carthage" -> Just CarthageType
  "composer" -> Just ComposerType
  "gem" -> Just GemType
  "git" -> Just GitType
  "go" -> Just GoType
  "hackage" -> Just HackageType
  "hex" -> Just HexType
  "maven" -> Just MavenType
  "npm" -> Just NodeJSType
  "nuget" -> Just NuGetType
  "pypi" -> Just PipType
  "cocoapods" -> Just PodType
  "url" -> Just URLType
  _ -> Nothing -- unsupported dep, need to respond with an error and skip this dependency
  -- rpm is an unsupported type. This is because we currently have 2 RPM fetchers
  -- and we should wait for a need to determine which one to use for manually
  -- specified dependencies.
