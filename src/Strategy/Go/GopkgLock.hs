module Strategy.Go.GopkgLock
  ( discover
  , strategy
  , analyze
  , configure

  , Project(..)

  , buildGraph
  )
  where

import Prologue hiding ((.=))

import qualified Data.Map.Strict as M
import           Data.Maybe (maybeToList)
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output
import           Toml (TomlCodec, (.=))
import qualified Toml

import           Diagnostics
import           Discovery.Walk
import           Effect.Exec
import           Effect.ReadFS
import           Effect.GraphBuilder
import qualified Graph as G
import           Strategy.Go.Transitive (fillInTransitive)
import           Types

discover :: Discover
discover = Discover
  { discoverName = "gopkglock"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files ->
  case find (\f -> fileName f == "Gopkg.lock") files of
    Nothing -> walkContinue
    Just file  -> do
      output (configure file)
      walkContinue

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "golang-gopkglock"
  , strategyAnalyze = analyze
  , strategyModule = parent . targetFile
  , strategyOptimal = Optimal
  , strategyComplete = NotComplete
  }

golockCodec :: TomlCodec GoLock
golockCodec = GoLock
  <$> Toml.list projectCodec "projects" .= lockProjects

projectCodec :: TomlCodec Project
projectCodec = Project
  <$> Toml.text "name" .= projectName
  <*> Toml.dioptional (Toml.text "source") .= projectSource
  <*> Toml.text "revision" .= projectRevision

data GoLock = GoLock
  { lockProjects :: [Project]
  } deriving (Eq, Ord, Show, Generic)

data Project = Project
  { projectName     :: Text
  , projectSource   :: Maybe Text
  , projectRevision :: Text
  } deriving (Eq, Ord, Show, Generic)

analyze :: Members '[Exec, ReadFS, Error ReadFSErr, Error ExecErr] r => BasicFileOpts -> Sem r G.Graph
analyze BasicFileOpts{..} = do
  contents <- readContentsText targetFile
  case Toml.decode golockCodec contents of
    Left err -> throw (FileParseError (fromRelFile targetFile) (Toml.prettyException err))
    Right golock -> do
      let (incompleteGraph, mapping) = buildGraph (lockProjects golock)

      graph <- fillInTransitive mapping (parent targetFile) incompleteGraph `catch` (\(_ :: ExecErr) -> pure incompleteGraph)

      pure graph

type PackageName = Text

buildGraph :: [Project] -> (G.Graph, Map PackageName G.DepRef)
buildGraph projects = run . runGraphBuilder G.empty $ do
  (assocs, _) <- runOutputList $ traverse addProject projects

  let mapping :: Map PackageName G.DepRef
      mapping = M.fromList assocs

  pure mapping

  where

  addProject :: Members '[GraphBuilder, Output (Text, G.DepRef)] r => Project -> Sem r ()
  addProject project = do
    ref <- addNode (toDependency project)
    addDirect ref
    output (projectName project, ref)

  toDependency Project{..} =
    G.Dependency { dependencyType = G.GoType
                 , dependencyName = projectName
                 , dependencyVersion = Just (G.CEq projectRevision)
                 , dependencyLocations = maybeToList projectSource
                 , dependencyTags = M.empty
                 }

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts
