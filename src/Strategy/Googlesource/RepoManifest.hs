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
import Data.Maybe (catMaybes)

import DepTypes
import Discovery.Walk
import Effect.ReadFS
import Graphing (Graphing, unfold)
import Parse.XML
import Types

-- TODO: This should look in a specific location, not walk through the filesystem.
--       To find the file, parse .repo/manifest.xml, which will point at the true manifest,
--       which is typically found in .repo/manifests/default.xml.
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

data ValidatedProject = ValidatedProject
  { validatedProjectName     :: Text
  , validatedProjectPath     :: Text
  , validatedProjectUrl      :: Text
  , validatedProjectRevision :: Text
  } deriving (Eq, Ord, Show, Generic)

-- If a project does not have a path, then use its name for the path
projectPathOrName :: ManifestProject -> Text
projectPathOrName (ManifestProject { projectPath = Nothing, projectName = name }) = name
projectPathOrName (ManifestProject { projectPath = Just path }) = path

-- A project's revision comes from the first of these that we encounter:
--   * If the project has a revision attribute, then use that
--   * If the project has a remote attribute and the remote it points to has a revision attribute, use that
--   * If the project does not have a remote attribute and the default remote has a revision attribute, use that
--   * Otherwise, use the revision attribute on the project's default tag
--   That leaves these error cases:
--   * If the project does not have a remote attribute and there is no default remote, then blow up
--   * If the project does not have a revision and there is no default revision from either its remote or the default, then blow up
revisionForProject :: ManifestProject -> RepoManifest -> Maybe Text
revisionForProject project manifest =
      projectRevision project
  <|> (remoteForProject project manifest >>= remoteRevision)
  <|> (manifestDefault manifest >>= defaultRevision)

remoteForProject :: ManifestProject -> RepoManifest -> Maybe ManifestRemote
remoteForProject project manifest =
  remoteNameString >>= \(name :: Text) -> remoteByName name manifest
  where
    remoteNameString = projectRemote project <|> (manifestDefault manifest >>= defaultRemote)

remoteByName :: Text -> RepoManifest -> Maybe ManifestRemote
remoteByName remoteNameString manifest =
  find (\r -> remoteName r == remoteNameString) (manifestRemotes manifest)

validatedProjects :: RepoManifest -> [ValidatedProject]
validatedProjects manifest =
  catMaybes validated
  where
    validated = [validatedProject mp manifest | mp <- manifestProjects manifest]

-- TODO: We're stubbing the URL with the projectPathOrName
validatedProject :: ManifestProject -> RepoManifest -> Maybe ValidatedProject
validatedProject project manifest =
  case revision of
    Nothing -> Nothing
    (Just r) -> Just (ValidatedProject (projectName project) (projectPathOrName project) (projectPathOrName project) r)
  where
    revision = revisionForProject project manifest

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
    projects = validatedProjects manifest
    toDependency ValidatedProject{..} =
      Dependency { dependencyType = GooglesourceType
                 , dependencyName = validatedProjectName
                 , dependencyVersion = Just (CEq $ validatedProjectRevision)
                 , dependencyLocations = [validatedProjectUrl]
                 , dependencyTags = M.empty
                 }
