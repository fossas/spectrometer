module Strategy.Go.GoList
  ( discover
  , strategy
  , analyze
  , configure

  , Require(..)
  )
  where

import Prologue hiding ((<?>))

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output

import           Diagnostics
import           Discovery.Walk
import           Effect.Exec
import           Effect.GraphBuilder
import qualified Graph as G
import           Strategy.Go.Transitive (fillInTransitive)
import           Types

discover :: Discover
discover = Discover
  { discoverName = "golist"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \dir _ files ->
  case find (\f -> fileName f == "go.mod") files of
    Nothing -> walkContinue
    Just _  -> do
      output (configure dir)
      walkContinue

strategy :: Strategy BasicDirOpts
strategy = Strategy
  { strategyName = "golang-golist"
  , strategyAnalyze = analyze
  , strategyModule = targetDir
  , strategyOptimal = Optimal
  , strategyComplete = NotComplete
  }

data Require = Require
  { reqPackage :: Text
  , reqVersion :: Text
  } deriving (Eq, Ord, Show, Generic)

golistCmd :: Command
golistCmd = Command
  { cmdNames = ["go"]
  , cmdBaseArgs = ["list", "-m", "all"]
  , cmdAllowErr = Never
  }

analyze :: Members '[Error ExecErr, Exec] r => BasicDirOpts -> Sem r G.Graph
analyze BasicDirOpts{..} = do
  stdout <- execThrow targetDir golistCmd []

  let gomodLines = drop 1 (T.lines (decodeUtf8 (BL.toStrict stdout))) -- the first line is our package
      requires = mapMaybe toRequire gomodLines

      toRequire :: Text -> Maybe Require
      toRequire line =
        case T.splitOn " " line of
          [package, version] -> Just (Require package version)
          _ -> Nothing

  let (incompleteGraph, mapping) = buildGraph requires

  graph <- fillInTransitive mapping targetDir incompleteGraph `catch` (\(_ :: ExecErr) -> pure incompleteGraph)

  pure graph

type PackageName = Text

buildGraph :: [Require] -> (G.Graph, Map PackageName G.DepRef)
buildGraph requires = run . runGraphBuilder G.empty $ do
  (assocs, _) <- runOutputList $ traverse addRequire requires

  let mapping :: Map PackageName G.DepRef
      mapping = M.fromList assocs

  pure mapping

  where

  addRequire :: Members '[GraphBuilder, Output (PackageName, G.DepRef)] r => Require -> Sem r ()
  addRequire require = do
    ref <- addNode (toDependency require)
    addDirect ref
    output (reqPackage require, ref)

  toVersion :: Text -> Text
  toVersion = last . T.splitOn "-"

  toDependency require = G.Dependency
    { dependencyType = G.GoType
    , dependencyName = reqPackage require
    , dependencyVersion = Just (G.CEq (toVersion (reqVersion require)))
    , dependencyLocations = []
    , dependencyTags = M.empty
    }

configure :: Path Rel Dir -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicDirOpts
