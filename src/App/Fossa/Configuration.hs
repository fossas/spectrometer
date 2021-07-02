{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module App.Fossa.Configuration (
  mergeFileCmdMetadata,
  readConfigFileIO,
  readConfigFile,
  targetParser,
  pathRelDirParser,
  configTargetParser,
  ConfigFile (..),
  ConfigProject (..),
  ConfigRevision (..),
  ConfigReleaseGroup (..),
  ConfigTargets (..),
  ConfigPaths (..),
  ConfigTarget (..),
  BuildTargetFilter (..),
) where

import App.Types
import Control.Carrier.Diagnostics qualified as Diag
import Data.Aeson (FromJSON (parseJSON), withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Effect.ReadFS
import Path
import System.Exit (die)
import Text.Megaparsec
import Text.Megaparsec.Char

data ConfigFile = ConfigFile
  { configVersion :: Int
  , configServer :: Maybe Text
  , configApiKey :: Maybe Text
  , configProject :: Maybe ConfigProject
  , configRevision :: Maybe ConfigRevision
  , configTargets :: Maybe ConfigTargets
  , configPaths :: Maybe ConfigPaths
  }
  deriving (Eq, Ord, Show)

data ConfigProject = ConfigProject
  { configProjID :: Maybe Text
  , configName :: Maybe Text
  , configLink :: Maybe Text
  , configTeam :: Maybe Text
  , configJiraKey :: Maybe Text
  , configUrl :: Maybe Text
  , configPolicy :: Maybe Text
  , configReleaseGroup :: Maybe ConfigReleaseGroup
  }
  deriving (Eq, Ord, Show)

data ConfigRevision = ConfigRevision
  { configCommit :: Maybe Text
  , configBranch :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data ConfigReleaseGroup = ConfigReleaseGroup
  { configReleaseGroupName :: Maybe Text
  , configReleaseGroupRelease :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data ConfigTargets = ConfigTargets
  { targetsOnly :: [ConfigTarget]
  , targetsExclude :: [ConfigTarget]
  }
  deriving (Eq, Ord, Show)

data ConfigTarget = ConfigTarget
  { targetType :: Text
  , targetTarget :: Maybe BuildTargetFilter
  }
  deriving (Eq, Ord, Show)

{-
  The following filters separate the difference between the following filters:
    gomod@./ -> DirectoryFilter
    gradle@./::test-benchmark -> ExactFilter

  The majority of build targets consist of a strategy type and a directory.
  However, many Gradle targets consist of a strategy type, a directory,
  and an exact gradle target.
-}
data BuildTargetFilter
  = DirectoryFilter (Path Rel Dir)
  | ExactTargetFilter (Path Rel Dir) Text
  deriving (Eq, Ord, Show)

data ConfigPaths = ConfigPaths
  { pathsOnly :: [Path Rel Dir]
  , pathsExclude :: [Path Rel Dir]
  }
  deriving (Eq, Ord, Show)

instance FromJSON ConfigFile where
  parseJSON = withObject "ConfigFile" $ \obj ->
    ConfigFile <$> obj .: "version"
      <*> obj .:? "server"
      <*> obj .:? "apiKey"
      <*> obj .:? "project"
      <*> obj .:? "revision"
      <*> obj .:? "targets"
      <*> obj .:? "paths"

instance FromJSON ConfigProject where
  parseJSON = withObject "ConfigProject" $ \obj ->
    ConfigProject <$> obj .:? "id"
      <*> obj .:? "name"
      <*> obj .:? "link"
      <*> obj .:? "team"
      <*> obj .:? "jiraProjectKey"
      <*> obj .:? "url"
      <*> obj .:? "policy"
      <*> obj .:? "releaseGroup"

instance FromJSON ConfigRevision where
  parseJSON = withObject "ConfigRevision" $ \obj ->
    ConfigRevision <$> obj .:? "commit"
      <*> obj .:? "branch"

instance FromJSON ConfigReleaseGroup where
  parseJSON = withObject "ConfigReleaseGroup" $ \obj ->
    ConfigReleaseGroup <$> obj .:? "name"
      <*> obj .:? "release"

instance FromJSON ConfigTargets where
  parseJSON = withObject "ConfigTargets" $ \obj ->
    ConfigTargets <$> (obj .:? "only" .!= [])
      <*> (obj .:? "exclude" .!= [])

instance FromJSON ConfigTarget where
  parseJSON = withObject "ConfigTarget" $ \obj ->
    ConfigTarget <$> obj .: "type"
      <*> (obj .:? "target" >>= filterParser)

type MegaParser = Parsec Void Text
filterParser :: Maybe Text -> Parser (Maybe BuildTargetFilter)
filterParser Nothing = pure Nothing
filterParser (Just input) = case runParser targetParser "" input of
  Left bundle -> fail (errorBundlePretty bundle)
  Right value -> pure $ Just value

targetParser :: MegaParser BuildTargetFilter
targetParser = (try newTargetFilter <|> directoryFilter) <* eof
  where
    newTargetFilter =
      ExactTargetFilter <$> pathRelDirParser <* char ':' <*> target
    directoryFilter =
      DirectoryFilter <$> pathRelDirParser

    target :: MegaParser Text
    target = takeWhile1P Nothing (const True)

pathRelDirParser :: MegaParser (Path Rel Dir)
pathRelDirParser = do
  filepath <- some (satisfy (/= ':'))
  case parseRelDir filepath of
    Left err -> fail (show err)
    Right a -> pure a

-- Parse mvn@test::target into a ConfigTarget
-- Parse mvn@test
-- Parse mvn
configTargetParser :: MegaParser ConfigTarget
configTargetParser = (try targetFilter <|> toolFilter) <* eof
  where
    targetFilter = do
      tool <- buildtool
      _ <- char '@'
      ConfigTarget tool . Just <$> targetParser

    toolFilter = do
      tool <- buildtool
      pure $ ConfigTarget tool Nothing

    buildtool :: MegaParser Text
    buildtool = T.pack <$> some alphaNumChar

instance FromJSON ConfigPaths where
  parseJSON = withObject "ConfigPaths" $ \obj ->
    ConfigPaths <$> (obj .:? "only" .!= [])
      <*> (obj .:? "exclude" .!= [])

defaultFile :: Path Rel File
defaultFile = $(mkRelFile ".fossa.yml")

readConfigFile :: (Has ReadFS sig m, Has Diag.Diagnostics sig m) => Path Rel File -> m (Maybe ConfigFile)
readConfigFile file = do
  exists <- doesFileExist file
  if not exists
    then pure Nothing
    else do
      readConfig <- readContentsYaml @ConfigFile file
      if configVersion readConfig < 3
        then pure Nothing
        else pure $ Just readConfig

readConfigFileIO :: IO (Maybe ConfigFile)
readConfigFileIO = do
  config <- Diag.runDiagnostics $ runReadFSIO $ readConfigFile defaultFile
  case config of
    Left err -> die $ show $ Diag.renderFailureBundle err
    Right a -> pure a

mergeFileCmdMetadata :: ProjectMetadata -> ConfigFile -> ProjectMetadata
mergeFileCmdMetadata meta file =
  ProjectMetadata
    { projectTitle = projectTitle meta <|> (configProject file >>= configName)
    , projectUrl = projectUrl meta <|> (configProject file >>= configUrl)
    , projectJiraKey = projectJiraKey meta <|> (configProject file >>= configJiraKey)
    , projectLink = projectLink meta <|> (configProject file >>= configLink)
    , projectTeam = projectTeam meta <|> (configProject file >>= configTeam)
    , projectPolicy = projectPolicy meta <|> (configProject file >>= configPolicy)
    , projectReleaseGroupName = projectReleaseGroupName meta <|> (configProject file >>= configReleaseGroup >>= configReleaseGroupName)
    , projectReleaseGroupRelease = projectReleaseGroupRelease meta <|> (configProject file >>= configReleaseGroup >>= configReleaseGroupRelease)
    }
