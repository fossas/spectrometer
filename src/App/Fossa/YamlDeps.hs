{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module App.Fossa.YamlDeps
  ( ManualDependencies (..),
    ManualDependency (..),
    ManagedDependency (..),
    UserDefinedDependency (..),
    analyzeFossaDepsYaml,
  )
where

import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, context, fatalText, fromMaybeText)
import Data.Aeson
  ( FromJSON (parseJSON),
    withObject,
    (.!=),
    (.:),
    (.:?),
  )
import Data.Aeson.Types (Parser, Object)
import Data.Foldable (traverse_)
import Data.List.NonEmpty qualified as NE
import Data.String.Conversion (toText, toString)
import Data.Text (Text, unpack)
import DepTypes (DepType(..))
import Effect.ReadFS (ReadFS, doesFileExist, readContentsYaml)
import Path
import Srclib.Converter (depTypeToFetcher)
import Srclib.Types (AdditionalDepData (..), Locator (..), SourceUnit (..), SourceUnitBuild (..), SourceUnitDependency (SourceUnitDependency), SourceUserDefDep (..))
import Control.Applicative (Alternative((<|>)))
import Data.HashMap.Strict (member, HashMap)

analyzeFossaDepsYaml :: (Has Diagnostics sig m, Has ReadFS sig m) => Path Abs Dir -> m (Maybe SourceUnit)
analyzeFossaDepsYaml root = do
  maybeDepsFile <- findFossaDepsFile root
  case maybeDepsFile of
    Nothing -> pure Nothing
    Just depsFile -> do
      manualDeps <- context "Reading fossa-deps file" $ readContentsYaml depsFile
      context "Converting fossa-deps to partial API payload" . toSourceUnit root $ dependencies manualDeps

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

toSourceUnit :: Has Diagnostics sig m => Path Abs Dir -> [ManualDependency] -> m (Maybe SourceUnit)
toSourceUnit root manualDeps = do
  _ <- fromMaybeText "No dependencies found to convert" $ NE.nonEmpty manualDeps
  let renderedPath = toText root
      (managed, userdef) = splitDeps manualDeps
      build = toBuildData <$> NE.nonEmpty managed
      additional = toAdditionalData <$> NE.nonEmpty userdef
  pure . Just $
    SourceUnit
      { sourceUnitName = renderedPath,
        sourceUnitManifest = renderedPath,
        sourceUnitType = "user-specific-yaml",
        sourceUnitBuild = build,
        additionalData = additional
      }

toBuildData :: NE.NonEmpty ManagedDependency -> SourceUnitBuild
toBuildData deps =
  SourceUnitBuild
    { buildArtifact = "default",
      buildSucceeded = True,
      buildImports = imports,
      buildDependencies = buildDeps
    }
  where
    (imports, buildDeps) = unzip . map (addEmptyDep . toImport) $ NE.toList deps

    toImport :: ManagedDependency -> Locator
    toImport ManagedDependency {..} =
      Locator
        { locatorFetcher = depTypeToFetcher locDepType,
          locatorProject = locDepPackage,
          locatorRevision = locDepVersion
        }

    addEmptyDep :: Locator -> (Locator, SourceUnitDependency)
    addEmptyDep loc = (loc, SourceUnitDependency loc [])

toAdditionalData :: NE.NonEmpty UserDefinedDependency -> AdditionalDepData
toAdditionalData deps = AdditionalDepData {userDefinedDeps = map tosrc $ NE.toList deps}
  where
    tosrc UserDefinedDependency {..} =
      SourceUserDefDep
        { srcUserDepName = userDepPackage,
          srcUserDepVersion = userDepVersion,
          srcUserDepLicense = userDepLicense,
          srcUserDepDescription = userDepDescription,
          srcUserDepUrl = userDepUrl
        }

type SplitDeps = ([ManagedDependency], [UserDefinedDependency])

splitDeps :: [ManualDependency] -> SplitDeps
splitDeps = foldr addToTuple ([], [])
  where
    addToTuple :: ManualDependency -> SplitDeps -> SplitDeps
    addToTuple (Managed dep) (managed, user) = (dep : managed, user)
    addToTuple (UserDefined dep) (managed, user) = (managed, dep : user)

newtype ManualDependencies = ManualDependencies
  { dependencies :: [ManualDependency]
  }

data ManualDependency
  = Managed ManagedDependency
  | UserDefined UserDefinedDependency

data ManagedDependency = ManagedDependency
  { locDepPackage :: Text,
    locDepType :: DepType,
    locDepVersion :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data UserDefinedDependency = UserDefinedDependency
  { userDepPackage :: Text,
    userDepVersion :: Text,
    userDepLicense :: Text,
    userDepDescription :: Maybe Text,
    userDepUrl :: Maybe Text
  }

instance FromJSON ManualDependencies where
  parseJSON = withObject "Dependencies" $ \obj ->
    ManualDependencies <$> obj .:? "dependencies" .!= []

depTypeParser :: Text -> Parser DepType
depTypeParser text = case depTypeFromText text of
  Just t -> pure t
  Nothing -> fail $ "dep type: " <> unpack text <> " not supported"

instance FromJSON ManualDependency where
  parseJSON value = withObject "ManualDependency" parser value
    where
      parser obj = do
        depType <- obj .: "type" :: Parser String
        case depType of
          "user" -> UserDefined <$> parseJSON value <|> fail "'user' dependency requires package, version, and license fields"
          _ -> Managed <$> parseJSON value

instance FromJSON ManagedDependency where
  parseJSON = withObject "ManualDependency" $ \obj ->
    ManagedDependency <$ noUserDefFields obj
      <*> obj .: "package"
      <*> (obj .: "type" >>= depTypeParser)
      <*> obj .:? "version"
  
    where
      badMember :: HashMap Text a -> Text -> Parser ()
      badMember hmap name = if member name hmap
        then fail (toString name <> " field is only allowed for user dependencies")
        else pure ()

      noUserDefFields :: Object -> Parser ()
      noUserDefFields jsonObject = traverse_ (badMember jsonObject) ["license", "description", "url"]
        

instance FromJSON UserDefinedDependency where
  parseJSON = withObject "UserDefinedDependency" $ \obj ->
    UserDefinedDependency <$ (obj .: "type" >>= isUserType)
      <*> obj .: "package"
      <*> obj .: "version"
      <*> obj .: "license"
      <*> obj .:? "description"
      <*> obj .:? "url"
    where
      isUserType :: String -> Parser ()
      isUserType typ = case typ of
        "user" -> pure ()
        -- unreachable, we don't parse user deps without this
        _ -> fail "UserDefinedDependency must have type: 'user'"

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
  "python" -> Just PipType
  "cocoapods" -> Just PodType
  "url" -> Just URLType
  _ -> Nothing -- unsupported dep, need to respond with an error and skip this dependency
  -- rpm is an unsupported type. This is because we currently have 2 RPM fetchers
  -- and we should wait for a need to determine which one to use for manually
  -- specified dependencies.
