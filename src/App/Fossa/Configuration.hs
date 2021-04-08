{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Configuration
( mergeFileCmdMetadata
, readConfigFile
, ConfigFile (..)
, ConfigProject (..)
, ConfigRevision (..)
)
where

import Data.Aeson ( FromJSON(parseJSON), (.:), (.:?), withObject )
import Data.Text (Text)
import App.Types
import Path
import qualified Control.Carrier.Diagnostics as Diag
import Effect.ReadFS
import Options.Applicative

data ConfigFile = ConfigFile
  { version :: Int
  , server :: Maybe Text
  , apiKey :: Maybe Text
  , project :: Maybe ConfigProject
  , revision :: Maybe ConfigRevision
  } deriving (Eq, Ord, Show)

data ConfigProject = ConfigProject
  { projID :: Maybe Text
  , name :: Maybe Text
  , link :: Maybe Text
  , team :: Maybe Text
  , jiraKey :: Maybe Text
  , url :: Maybe Text
  , policy :: Maybe Text
  } deriving (Eq, Ord, Show)

data ConfigRevision = ConfigRevision
  { commit :: Maybe Text
  , branch :: Maybe Text
  } deriving (Eq, Ord, Show)

instance FromJSON ConfigFile where
  parseJSON = withObject "ConfigFile" $ \obj ->
    ConfigFile <$> obj .: "version"
               <*> obj .:? "server"
               <*> obj .:? "apiKey"
               <*> obj .:? "project"
               <*> obj .:? "revision"

instance FromJSON ConfigProject where
  parseJSON = withObject "ConfigProject" $ \obj ->
    ConfigProject <$> obj .:? "id"
                  <*> obj .:? "name"
                  <*> obj .:? "link"
                  <*> obj .:? "team"
                  <*> obj .:? "jirakey"
                  <*> obj .:? "url"
                  <*> obj .:? "policy"

instance FromJSON ConfigRevision where
  parseJSON = withObject "ConfigRevision" $ \obj ->
    ConfigRevision <$> obj .:? "commit"
                   <*> obj .:? "branch"

defaultFile :: Path Rel File
defaultFile = $(mkRelFile ".fossa.yml")

readConfigFile :: (Has ReadFS sig m, Has Diag.Diagnostics sig m) => m (Maybe ConfigFile)
readConfigFile = do
      exists <- doesFileExist defaultFile
      if not exists 
        then pure Nothing 
        else (do file <- readContentsYaml @ConfigFile defaultFile
                 pure $ Just file)

mergeFileCmdMetadata :: ProjectMetadata -> ConfigFile -> ProjectMetadata
mergeFileCmdMetadata meta file =
  ProjectMetadata
    { projectTitle = projectTitle meta <|> (project file >>= name)
    , projectUrl = projectUrl meta <|> (project file >>= url)
    , projectJiraKey = projectJiraKey meta <|> (project file >>= jiraKey)
    , projectLink = projectLink meta <|> (project file >>= link)
    , projectTeam = projectTeam meta <|> (project file >>= team)
    , projectPolicy = projectPolicy meta <|> (project file >>= policy)
    }
