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
-- Note that the DTD is only "roughly adhered to" according to the documentation. For example, the DTD says that
-- there will be zero or more project and remote tags (it uses a `*`), but the documentation specifies at least one
-- for both of these tags (which should be denoted by a `+` in the DTD).
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
  , projectRevision :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

-- If a project does not have a path, then use its name for the path
projectPathOrName :: ManifestProject -> Text
projectPathOrName (ManifestProject { projectPath = Nothing, projectName = name }) = name
projectPathOrName (ManifestProject { projectPath = Just path}) = path

-- A project's revision comes from the first of these that we encounter:
--   * The revision attribute on the project
--   * The revision attribute on the project's remote
--   * The revision attribute on the manifest's default
revisionForProject :: ManifestProject -> RepoManifest -> Text
revisionForProject project manifest =
  case projectRevision project of
    Nothing -> defaultRevisionForProject project manifest
    Just revision -> revision

-- If a project does not have a revision attribute, then we look for:
--   * The revision of the remote that it points to with its remote attribute
--   * The revision of the default remote
--   * The revision attribute of the project's remote
-- TODO: The spec is not clear on what revision a project has if a project does not have a revision attribte, uses the default remote
--       and both the <remote> and <default> have a revision attribute.
--       I need to test this by building some sample repo manifests and syncing them.
defaultRevisionForProject :: ManifestProject -> RepoManifest -> Text
defaultRevisionForProject project manifest =
  case projectRemote project of
    Nothing -> defaultRevisionForManifest manifest
    (Just remoteName) -> defaultRevisionForRemote remoteName manifest

-- If there is a <default> tag, then use its revision
-- if not, then fall back to the revision in the first remote
defaultRevisionForManifest :: RepoManifest -> Text
defaultRevisionForManifest (RepoManifest { manifestDefault = Nothing, manifestRemotes = (r:_)}) =
  remote
  where
    (Just remote) = remoteRevision r
defaultRevisionForManifest manifest =
  case defaultRemote d of
    Nothing -> r
    (Just remoteName) -> defaultRevisionForRemote remoteName manifest
  where
    (Just d) = manifestDefault manifest
    (Just r) = defaultRevision d

defaultRevisionForRemote :: Text -> RepoManifest -> Text
defaultRevisionForRemote remoteNameString manifest =
  case remoteRevision remote of
    Nothing -> forcedDefaultRevision $ forcedDefault manifest
    (Just revision) -> revision
  where
    (Just remote) = find (\r -> (remoteName r) == remoteNameString) (manifestRemotes manifest)

forcedDefault :: RepoManifest -> ManifestDefault
forcedDefault manifest =
  d
  where
    (Just d) = manifestDefault manifest

forcedDefaultRevision :: ManifestDefault -> Text
forcedDefaultRevision manifestDefault =
  r
  where
    (Just r) = defaultRevision manifestDefault

instance FromXML RepoManifest where
  parseElement el = do
    RepoManifest <$> optional (child "default" el)
                 <*> children "remote" el
                 <*> children "project" el

instance FromXML ManifestDefault where
  parseElement el = do
    ManifestDefault <$> optional (attr "remote" el)
                    <*> optional (attr "revision" el)

instance FromXML ManifestRemote where
  parseElement el = do
    ManifestRemote <$> attr "name" el
                   <*> attr "fetch" el
                   <*> optional (attr "revision" el)

instance FromXML ManifestProject where
  parseElement el = do
    ManifestProject <$> attr "name" el
                    <*> optional (attr "path" el)
                    <*> optional (attr "remote" el)
                    <*> optional (attr "revision" el)

buildGraph :: RepoManifest -> Graphing Dependency
buildGraph manifest = unfold projects (const []) toDependency
    where
    projects = manifestProjects manifest
    toDependency mp@ManifestProject{..} =
      Dependency { dependencyType = GooglesourceType
                 , dependencyName = projectName
                 , dependencyVersion = Just (CEq $ revisionForProject mp manifest)
                 , dependencyLocations = [projectPathOrName mp]
                 , dependencyTags = M.empty
                 }
