module Strategy.Go.GoList
  ( discover
  , analyze

  , Require(..)
  )
  where

import Prologue hiding ((<?>))

import qualified Data.ByteString.Lazy as BL
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Polysemy
import Polysemy.Error
import Polysemy.Output

import DepTypes
import Diagnostics
import Discovery.Walk
import qualified Effect.Error as E
import Effect.Exec
import Effect.LabeledGrapher
import Graphing (Graphing)
import Strategy.Go.Transitive (fillInTransitive)
import Strategy.Go.Types
import Types

discover :: Discover
discover = Discover
  { discoverName = "golist"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Exec, Output ProjectClosure] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files -> do
  case find (\f -> fileName f == "go.mod") files of
    Nothing -> pure ()
    Just file  -> do
      res <- runError @ExecErr (analyze (parent file))
      traverse_ output res

  walkContinue

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

analyze :: Members '[Error ExecErr, Exec] r => Path Rel Dir -> Sem r ProjectClosure
analyze dir = fmap (mkProjectClosure dir) . graphingGolang $ do
  stdout <- execThrow dir golistCmd []

  let gomodLines = drop 1 . T.lines . T.filter (/= '\r') . decodeUtf8 . BL.toStrict $ stdout -- the first line is our package
      requires = mapMaybe toRequire gomodLines

      toRequire :: Text -> Maybe Require
      toRequire line =
        case T.splitOn " " line of
          [package, version] -> Just (Require package version)
          _ -> Nothing

  buildGraph requires

  -- TODO: diagnostics?
  _ <- E.try @ExecErr (fillInTransitive dir)
  pure ()

mkProjectClosure :: Path Rel Dir -> Graphing Dependency -> ProjectClosure
mkProjectClosure dir graph = ProjectClosure
  { closureStrategyGroup = GolangGroup
  , closureStrategyName  = "golang-golist"
  , closureModuleDir     = dir
  , closureDependencies  = dependencies
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = graph
    , dependenciesOptimal  = Optimal
    , dependenciesComplete = NotComplete
    }

buildGraph :: Member (LabeledGrapher GolangPackage) r => [Require] -> Sem r ()
buildGraph = traverse_ go
  where

  go :: Member (LabeledGrapher GolangPackage) r => Require -> Sem r ()
  go Require{..} = do
    let pkg = mkGolangPackage reqPackage
    direct pkg
    label pkg (mkGolangVersion reqVersion)
