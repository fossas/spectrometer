module Strategy.Googlesource.RepoManifest
  ( discover
  , buildGraph
  , analyze

  , RepoManifest(..)
  , ManifestRemote(..)
  , ManifestDefault(..)
  , ManifestProject(..)

  , mkProjectClosure
  ) where

import Prologue

import Control.Carrier.Error.Either
import qualified Data.Map.Strict as M

import DepTypes
import Discovery.Walk
import Effect.ReadFS
import Graphing (Graphing, unfold)
import Parse.XML
import Types

-- TODO: This should look in a specific location, not walk through the filesystem. It lives in PROJECT_ROOT/.repo/manifest.xml
discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \_ _ files -> do
  case find (\f -> (==) "manifest.xml" (fileName f)) files of
    Nothing -> pure ()
    Just file -> runSimpleStrategy "googlesource-repomanifest" GooglesourceGroup $ analyze file

  walkContinue

analyze :: (Has ReadFS sig m, Has (Error ReadFSErr) sig m) => Path Rel File -> m ProjectClosureBody
analyze file = mkProjectClosure file <$> readContentsXML @RepoManifest file

mkProjectClosure :: Path Rel File -> RepoManifest -> ProjectClosureBody
mkProjectClosure file manifest = ProjectClosureBody
  { bodyModuleDir    = parent file
  , bodyDependencies = dependencies
  , bodyLicenses     = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph manifest
    , dependenciesOptimal  = NotOptimal
    , dependenciesComplete = NotComplete
    }

-- DTD for the Repo manifest.xml file: https://gerrit.googlesource.com/git-repo/+/master/docs/manifest-format.md
data RepoManifest = RepoManifest
  { manifestDefault  :: Maybe ManifestDefault
  , manifestRemotes  :: [ManifestRemote]
  , manifestProjects :: [ManifestProject] 
  } deriving (Eq, Ord, Show, Generic)

data ManifestRemote = ManifestRemote
  { remoteName     :: Text
  , remoteFetch    :: Text
  , remoteRevision :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

data ManifestDefault = ManifestDefault
  { defaultRemote   :: Maybe Text
  , defaultRevision :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

data ManifestProject = ManifestProject
  { projectName     :: Text
  , projectPath     :: Maybe Text
  , projectRemote   :: Maybe Text
  , projectRevision :: Text  -- TODO: This should be a Maybe. It needs to be grabbed from the remote or default if it's not set in the project
  } deriving (Eq, Ord, Show, Generic)

instance FromXML ManifestDefault where
  parseElement el = do 
    ManifestDefault <$> optional (attr "remote" el)
                    <*> optional (attr "revision" el)

instance FromXML ManifestRemote where
  parseElement el = do
    ManifestRemote <$> attr "name" el
                   <*> attr "remote" el
                   <*> optional (attr "fetch" el)

instance FromXML ManifestProject where
  parseElement el = do
    ManifestProject <$> attr "name" el
                    <*> optional (attr "path" el)
                    <*> optional (attr "remote" el)
                    <*> optional (attr "revision" el) `defaultsTo` "" -- make this a Maybe

instance FromXML RepoManifest where
  parseElement el = do
    RepoManifest <$> optional (child "default" el)
             <*> children "remote" el
             <*> children "project" el

buildGraph :: RepoManifest -> Graphing Dependency
buildGraph manifest = unfold projects (const []) toDependency
    where
    projects = manifestProjects manifest
    toDependency ManifestProject{..} =
      Dependency { dependencyType = GooglesourceType
                 , dependencyName = projectName
                 , dependencyVersion = Just (CEq projectRevision)
                 , dependencyLocations = []
                 , dependencyTags = M.empty
                 }
