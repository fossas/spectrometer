{-# LANGUAGE TemplateHaskell #-}

module Strategy.Gradle
  ( discover
  , discover'

  , buildGraph
  , JsonDep(..)
  ) where

import Control.Carrier.Diagnostics
import Control.Effect.Exception
import Control.Effect.Lift (sendIO)
import Control.Effect.Path (withSystemTempDir)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.Types (Parser, unexpected)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.FileEmbed (embedFile)
import Data.Foldable (find, for_)
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Extra (strippedPrefix, splitOnceOn)
import DepTypes
import Discovery.Walk
import Effect.Exec
import Effect.Grapher
import Graphing (Graphing)
import Path
import qualified System.FilePath as FP
import Types

gradleJsonDepsCmd :: Text -> FP.FilePath -> Command
gradleJsonDepsCmd baseCmd initScriptFilepath = Command
  { cmdName = baseCmd
  , cmdArgs = ["jsonDeps", "-I", T.pack initScriptFilepath]
  , cmdAllowErr = Never
  }

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \_ _ files -> do
  case find (\f -> fileName f == "build.gradle") files of
    Nothing -> pure WalkContinue
    Just file -> do
      runSimpleStrategy "gradle-cli" GradleGroup $ analyze (parent file)
      pure WalkSkipAll

discover' ::
  ( Has (Lift IO) sig m,
    MonadIO m,
    Has Exec sig m,
    Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  m [NewProject m]
discover' dir = map mkProject <$> findProjects dir

findProjects :: (Has Exec sig m, MonadIO m) => Path Abs Dir -> m [GradleProject]
findProjects = walk' $ \dir _ files -> do
  case find (\f -> "build.gradle" `isPrefixOf` fileName f) files of
    Nothing -> pure ([], WalkContinue)
    Just _ -> do

      projectsStdout <-
        runDiagnostics $
          execThrow dir (gradleProjectsCmd "./gradlew")
            <||> execThrow dir (gradleProjectsCmd "gradlew.bat")
            <||> execThrow dir (gradleProjectsCmd "gradle")

      let subprojects = either (const S.empty) (parseProjects . resultValue) projectsStdout

      let project =
            GradleProject
              { gradleDir = dir,
                gradleProjects = subprojects
              }

      pure ([project], WalkSkipAll)

data GradleProject = GradleProject
  { gradleDir :: Path Abs Dir
  , gradleProjects :: Set Text
  } deriving (Eq, Ord, Show)

gradleProjectsCmd :: Text -> Command
gradleProjectsCmd baseCmd = Command
  { cmdName = baseCmd
  , cmdArgs = ["projects"]
  , cmdAllowErr = Never
  }

-- TODO: use megaparsec here? this logic is unreadable
parseProjects :: BL.ByteString -> Set Text
parseProjects outBL = S.fromList $ mapMaybe parseLine outLines
  where
    outText = decodeUtf8 $ BL.toStrict outBL
    outLines = T.lines outText

    parseLine :: Text -> Maybe Text
    parseLine line
      | "Root project" `T.isPrefixOf` line = Just . fst . splitOnceOn "'" . strippedPrefix "Root project '" $ line
      | "---" `T.isPrefixOf` T.drop 1 (T.strip line) = Just . fst . splitOnceOn "'" . strippedPrefix "--- Project '" . T.drop 1 $ line
      | otherwise = Nothing

mkProject :: (Has (Lift IO) sig m, Has Exec sig m, Has Diagnostics sig m) => GradleProject -> NewProject m
mkProject project =
  NewProject
    { projectType = "cocoapods",
      projectBuildTargets = S.map BuildTarget $ gradleProjects project,
      projectDependencyGraph = const $ getDeps project,
      projectPath = gradleDir project,
      projectLicenses = pure []
    }

getDeps :: (Has (Lift IO) sig m, Has Exec sig m, Has Diagnostics sig m) => GradleProject -> m (Graphing Dependency)
getDeps = analyze' . gradleDir

initScript :: ByteString
initScript = $(embedFile "scripts/jsondeps.gradle")

analyze' ::
  ( Has (Lift IO) sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  )
  => Path Abs Dir -> m (Graphing Dependency)
analyze' dir = withSystemTempDir "fossa-gradle" $ \tmpDir -> do
  let initScriptFilepath = fromAbsDir tmpDir FP.</> "jsondeps.gradle"
  sendIO (BS.writeFile initScriptFilepath initScript)
  stdout <- execThrow dir (gradleJsonDepsCmd "./gradlew" initScriptFilepath)
              <||> execThrow dir (gradleJsonDepsCmd "gradlew.bat" initScriptFilepath)
              <||> execThrow dir (gradleJsonDepsCmd "gradle" initScriptFilepath)

  let text = decodeUtf8 $ BL.toStrict stdout
      textLines :: [Text]
      textLines = T.lines (T.filter (/= '\r') text)
      -- jsonDeps lines look like:
      -- JSONDEPS_:project-path_{"configName":[{"type":"package", ...}, ...], ...}
      jsonDepsLines :: [Text]
      jsonDepsLines = mapMaybe (T.stripPrefix "JSONDEPS_") textLines

      packagePathsWithJson :: [(Text,Text)]
      packagePathsWithJson = map (\line -> let (x,y) = T.breakOn "_" line in (x, T.drop 1 y {- drop the underscore; break doesn't remove it -})) jsonDepsLines

      packagePathsWithDecoded :: [(Text, [JsonDep])]
      packagePathsWithDecoded = [(name, deps) | (name, outJson) <- packagePathsWithJson
                                              , Just configs <- [decodeStrict (encodeUtf8 outJson) :: Maybe (Map Text [JsonDep])]
                                              , Just deps <- [M.lookup "default" configs]] -- FUTURE: use more than default?

      packagesToOutput :: Map Text [JsonDep]
      packagesToOutput = M.fromList packagePathsWithDecoded

  pure (buildGraph packagesToOutput)

analyze ::
  ( Has (Lift IO) sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  )
  => Path Abs Dir -> m ProjectClosureBody
analyze dir = withSystemTempDir "fossa-gradle" $ \tmpDir -> do
  let initScriptFilepath = fromAbsDir tmpDir FP.</> "jsondeps.gradle"
  sendIO (BS.writeFile initScriptFilepath initScript)
  stdout <- execThrow dir (gradleJsonDepsCmd "./gradlew" initScriptFilepath)
              <||> execThrow dir (gradleJsonDepsCmd "gradlew.bat" initScriptFilepath)
              <||> execThrow dir (gradleJsonDepsCmd "gradle" initScriptFilepath)

  let text = decodeUtf8 $ BL.toStrict stdout
      textLines :: [Text]
      textLines = T.lines (T.filter (/= '\r') text)
      -- jsonDeps lines look like:
      -- JSONDEPS_:project-path_{"configName":[{"type":"package", ...}, ...], ...}
      jsonDepsLines :: [Text]
      jsonDepsLines = mapMaybe (T.stripPrefix "JSONDEPS_") textLines

      packagePathsWithJson :: [(Text,Text)]
      packagePathsWithJson = map (\line -> let (x,y) = T.breakOn "_" line in (x, T.drop 1 y {- drop the underscore; break doesn't remove it -})) jsonDepsLines

      packagePathsWithDecoded :: [(Text, [JsonDep])]
      packagePathsWithDecoded = [(name, deps) | (name, outJson) <- packagePathsWithJson
                                              , Just configs <- [decodeStrict (encodeUtf8 outJson) :: Maybe (Map Text [JsonDep])]
                                              , Just deps <- [M.lookup "default" configs]] -- FUTURE: use more than default?

      packagesToOutput :: Map Text [JsonDep]
      packagesToOutput = M.fromList packagePathsWithDecoded

  pure (mkProjectClosure dir packagesToOutput)

mkProjectClosure :: Path Abs Dir -> Map Text [JsonDep] -> ProjectClosureBody
mkProjectClosure dir deps = ProjectClosureBody
  { bodyModuleDir    = dir
  , bodyDependencies = dependencies
  , bodyLicenses     = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph deps
    , dependenciesOptimal  = Optimal
    , dependenciesComplete = Complete
    }

-- TODO: use LabeledGraphing to add labels for environments
buildGraph :: Map Text [JsonDep] -> Graphing Dependency
buildGraph projectsAndDeps = run . evalGrapher $ M.traverseWithKey addProject projectsAndDeps
  where
  -- add top-level projects from the output
  addProject :: Has (Grapher Dependency) sig m => Text -> [JsonDep] -> m ()
  addProject projName projDeps = do
    let projAsDep = projectToDep projName
    direct projAsDep
    for_ projDeps $ \dep -> do
      edge projAsDep (jsonDepToDep dep)
      mkRecursiveEdges dep

  -- build edges between deps, recursively
  mkRecursiveEdges :: Has (Grapher Dependency) sig m => JsonDep -> m ()
  mkRecursiveEdges (ProjectDep _) = pure ()
  mkRecursiveEdges jsondep@(PackageDep _ _ deps) = do
    let packageAsDep = jsonDepToDep jsondep
    for_ deps $ \child -> do
      edge packageAsDep (jsonDepToDep child)
      mkRecursiveEdges child

  jsonDepToDep :: JsonDep -> Dependency
  jsonDepToDep (ProjectDep name) = projectToDep name
  jsonDepToDep (PackageDep name version _) =
    Dependency
      { dependencyType = MavenType
      , dependencyName = name
      , dependencyVersion = Just (CEq version)
      , dependencyLocations = []
      , dependencyEnvironments = []
      , dependencyTags = M.empty
      }

  projectToDep name = Dependency
    { dependencyType = SubprojectType
    , dependencyName = name
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyEnvironments = []
    , dependencyTags = M.empty
    }

data JsonDep =
    ProjectDep Text -- name
  | PackageDep Text Text [JsonDep] -- name version deps
  deriving (Eq, Ord, Show)

instance FromJSON JsonDep where
  parseJSON = withObject "JsonDep" $ \obj -> do
    ty <- obj .: "type" :: Parser Text
    case ty of
      "project" -> ProjectDep <$> obj .: "name"
      "package" -> PackageDep <$> obj .: "name" <*> obj .: "version" <*> obj .: "dependencies"
      _         -> unexpected (String ty)
