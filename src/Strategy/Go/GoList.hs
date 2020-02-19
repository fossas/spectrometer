module Strategy.Go.GoList
  ( discover
  , analyze

  , Require(..)
  )
  where

import Prologue hiding ((<?>))

import Control.Carrier.Error.Either
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import DepTypes
import Diagnostics
import Discovery.Walk
import Effect.Exec
import Effect.LabeledGrapher
import Graphing (Graphing)
import Strategy.Go.Transitive (fillInTransitive)
import Strategy.Go.Types
import Types

discover :: Discover
discover = Discover
  { discoverName = "golang-golist"
  , discoverFunc = discover'
  }

discover' :: HasDiscover sig m => Path Abs Dir -> m ()
discover' = walk $ \_ _ files -> do
  case find (\f -> fileName f == "go.mod") files of
    Nothing -> pure ()
    Just file  -> runSimpleStrategy "golang-golist" GolangGroup $ analyze (parent file)

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

analyze ::
  ( Has Exec sig m
  , Has (Error ExecErr) sig m
  , Effect sig
  )
  => Path Rel Dir -> m ProjectClosure
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
  _ <- try @ExecErr (fillInTransitive dir)
  pure ()

try :: Has (Error e) sig m => m a -> m (Either e a)
try act = (Right <$> act) `catchError` (pure . Left)

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

buildGraph :: Has (LabeledGrapher GolangPackage) sig m => [Require] -> m ()
buildGraph = traverse_ go
  where

  go :: Has (LabeledGrapher GolangPackage) sig m => Require -> m ()
  go Require{..} = do
    let pkg = mkGolangPackage reqPackage
    direct pkg
    label pkg (mkGolangVersion reqVersion)
