{-# LANGUAGE RecordWildCards #-}

module Strategy.Go.GoList (
  analyze',
  Require (..),
) where

import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  context,
  recover,
 )
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (traverse_)
import Data.Maybe (mapMaybe)
import Data.String.Conversion (decodeUtf8)
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (Dependency)
import Effect.Exec (
  AllowErr (Never),
  Command (Command, cmdAllowErr, cmdArgs, cmdName),
  Exec,
  execThrow,
 )
import Effect.Grapher (direct, label)
import Graphing (Graphing)
import Path (Abs, Dir, Path)
import Strategy.Go.Transitive (fillInTransitive)
import Strategy.Go.Types (
  GolangGrapher,
  graphingGolang,
  mkGolangPackage,
  mkGolangVersion,
 )
import Types (GraphBreadth (Complete))

data Require = Require
  { reqPackage :: Text
  , reqVersion :: Text
  }
  deriving (Eq, Ord, Show)

golistCmd :: Command
golistCmd =
  Command
    { cmdName = "go"
    , cmdArgs = ["list", "-m", "all"]
    , cmdAllowErr = Never
    }

analyze' ::
  ( Has Exec sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  m (Graphing Dependency, GraphBreadth)
analyze' dir = do
  graph <- graphingGolang $ do
    stdout <- context "Getting direct dependencies" $ execThrow dir golistCmd

    let gomodLines = drop 1 . Text.lines . Text.filter (/= '\r') . decodeUtf8 . BL.toStrict $ stdout -- the first line is our package
        requires = mapMaybe toRequire gomodLines

        toRequire :: Text -> Maybe Require
        toRequire line =
          case Text.splitOn " " line of
            [package, version] -> Just (Require package version)
            _ -> Nothing

    context "Adding direct dependencies" $ buildGraph requires

    _ <- recover (fillInTransitive dir)
    pure ()
  pure (graph, Complete)

buildGraph :: Has GolangGrapher sig m => [Require] -> m ()
buildGraph = traverse_ go
  where
    go :: Has GolangGrapher sig m => Require -> m ()
    go Require{..} = do
      let pkg = mkGolangPackage reqPackage
      direct pkg
      label pkg (mkGolangVersion reqVersion)
