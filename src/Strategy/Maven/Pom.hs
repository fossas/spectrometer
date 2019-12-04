module Strategy.Maven.Pom
  ( discover
  , strategy
  , analyze
  , buildGraph
  ) where

import Prologue hiding ((.:), (.:?), optional, withObject, withText)

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output
import           Text.Read (readMaybe)
import qualified Text.XML.Light as XML

import           Diagnostics
import           Discovery.Walk
import           Effect.Exec
import           Effect.GraphBuilder
import           Effect.ReadFS
import qualified Graph as G
import           Strategy.Maven.Plugin
import           Types

discover :: Discover
discover = Discover
  { discoverName = "maven-pom"
  , discoverFunc = discover'
  }

discover' :: forall r. Members '[Embed IO, Exec, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' dir = do
  let doIt :: Members '[Embed IO, Output (Path Rel File)] r' => Sem r' ()
      doIt = flip walk dir $ \_ _ files -> do
        case find ((== "pom.xml") . fileName) files of
          Just file -> output file
          Nothing -> pure ()
        walkContinue

  (unresolved, ()) <- outputToIOMonoid @(Path Rel File) S.singleton doIt

  projects <- resolve unresolved

  undefined

resolve :: Set (Path Rel File) -> Sem r [SaturatedProject]
resolve = undefined

resolveOne :: Path Abs File -> ResolvedPom
resolveOne = undefined

data MavenResolver m a where
  Resolve :: Path Abs File -> MavenResolver m ResolvedPom
  ResolveFrom :: Path Rel File -> () {- ??? -} -> MavenResolver m ResolvedPom

data ResolvedPom = ResolvedPom
  { rpGroup        :: Text
  , rpArtifact     :: Text
  , rpVersion      :: Text
  , rpDependencies :: [ResolvedDep]
  }

data ResolvedDep = ResolvedDep
  { rdGroup    :: Text
  , rdArtifact :: Text
  , rdVersion  :: Text
  , rdScopes   :: Set String
  , rdOptional :: Bool
  }

data SaturatedProject

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "maven-pom"
  , strategyAnalyze = analyze
  , strategyModule = parent . targetFile
  , strategyOptimal = Optimal
  , strategyComplete = Complete
  }

runIt :: String -> IO (Either ReadFSErr (Maybe Pom))
runIt str = runFinal . embedToFinal @IO . errorToIOFinal . readFSToIO . blah $ BasicFileOpts path
  where
  Just path = parseRelFile str

blah :: Members '[ReadFS, Error ReadFSErr] r => BasicFileOpts -> Sem r (Maybe Pom)
blah BasicFileOpts{..} = do
  contents <- readContentsBS targetFile
  let doc = parsePom =<< XML.parseXMLDoc contents
  pure doc

analyze :: Members '[ReadFS, Error ReadFSErr] r => BasicFileOpts -> Sem r G.Graph
analyze BasicFileOpts{..} = do
  contents <- readContentsBS targetFile
  let doc = parsePom =<< XML.parseXMLDoc contents

  undefined

  where

parsePom :: XML.Element -> Maybe Pom
parsePom project = do
  guard (XML.qName (XML.elName project) == "project")
  let parentPom = parseParent =<< childByName "parent" project
      modules = map (Module . XML.strContent) . childrenByName "module" <$> childByName "modules" project
      dependencyManagement = parseDependencies =<< childByName "dependencyManagement" project
      dependencies = parseDependencies project

  pure (Pom parentPom modules dependencyManagement dependencies)

  where

  -- "although, groupId and version need not be explicitly defined if they are inherited from a parent"
  -- https://maven.apache.org/pom.html#Maven_Coordinates
  -- "Notice the relativePath element. It is not required, but may be used as a signifier to Maven to first
  -- search the path given for this project's parent, before searching the local and then remote repositories."
  -- https://maven.apache.org/pom.html#Inheritance
  parseParent :: XML.Element -> Maybe Parent
  parseParent el = do
    artifact <- stringByName "artifactId" el

    let group        = stringByName "groupId" el
        version      = stringByName "version" el
        relativePath = stringByName "relativePath" el
    pure (Parent group artifact version relativePath)

  parseDependencies :: XML.Element -> Maybe [Dependency]
  parseDependencies el = traverse parseDependency . childrenByName "dependency" =<< childByName "dependencies" el

  parseDependency :: XML.Element -> Maybe Dependency
  parseDependency el = do
    group    <- stringByName "groupId" el
    artifact <- stringByName "artifactId" el

    let version    = stringByName "version" el
        classifier = stringByName "classifier" el
        scope      = stringByName "scope" el
        optional   = readMaybe =<< stringByName "optional" el

    pure (Dependency group artifact version classifier scope optional)

  stringByName :: String -> XML.Element -> Maybe String
  stringByName name = fmap XML.strContent . childByName name

  childByName :: String -> XML.Element -> Maybe XML.Element
  childByName name = XML.filterChildName (\elName -> XML.qName elName == name)

  childrenByName :: String -> XML.Element -> [XML.Element]
  childrenByName name = XML.filterChildrenName (\elName -> XML.qName elName == name)

data Pom = Pom
  { pomParent               :: Maybe Parent
  , pomModules              :: Maybe [Module]
  , pomDependencyManagement :: Maybe [Dependency]
  , pomDependencies         :: Maybe [Dependency]
  } deriving (Eq, Ord, Show, Generic)

data Parent = Parent
  { parentGroup        :: Maybe String
  , parentArtifact     :: String
  , parentVersion      :: Maybe String
  , parentRelativePath :: Maybe String
  } deriving (Eq, Ord, Show, Generic)

data Dependency = Dependency
  { depGroup      :: String
  , depArtifact   :: String
  , depVersion    :: Maybe String
  , depClassifier :: Maybe String
  , depScope      :: Maybe String
  , depOptional   :: Maybe Bool
  } deriving (Eq, Ord, Show, Generic)

newtype Module = Module { modulePath :: String }
  deriving (Eq, Ord, Show, Generic)

buildGraph = undefined

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts
