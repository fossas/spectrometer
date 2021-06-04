module Strategy.VSI
  ( discover
  ) where

import Control.Effect.Diagnostics
import Effect.Exec
import Path
import Types
import Graphing
import Fossa.API.Types
import App.Fossa.EmbeddedBinary
import App.Fossa.VPS.Scan.RunWiggins
import qualified Data.Text as T
import Data.Aeson;
import GHC.Generics;
import Control.Effect.Lift
import Control.Monad.IO.Class

newtype VSIProject = VSIProject
  { vsiDir :: Path Abs Dir
  } deriving (Eq, Ord, Show)

newtype VSIStandaloneResults = VSIStandaloneResults
  { vsiStandaloneLocators :: [T.Text]
  } deriving (Show, Generic, FromJSON)

discover :: (Has Diagnostics sig m, Has (Lift IO) rsig run, MonadIO run, Has Exec rsig run, Has Diagnostics rsig run) => ApiOpts -> Path Abs Dir -> m [DiscoveredProject run]
discover apiOpts dir = context "VSI" $ do
  -- Right now we assume that if the VSI strategy is run, the top level root is a project to scan.
  -- In the future we will likely add heuristics to detect whether the VSI strategy will yield results.
  let wigginsOpts = generateVSIStandaloneOpts dir apiOpts
  pure [mkProject wigginsOpts (VSIProject dir)]

mkProject :: (Has (Lift IO) sig n, MonadIO n, Has Exec sig n, Has Diagnostics sig n) => WigginsOpts -> VSIProject -> DiscoveredProject n
mkProject wigginsOpts project =
  DiscoveredProject
    { projectType = "vsi"
    , projectBuildTargets = mempty
    , projectDependencyGraph = const $ analyze wigginsOpts
    , projectPath = vsiDir project
    , projectLicenses = pure []
    }

analyze :: (Has (Lift IO) sig m, MonadIO m, Has Exec sig m, Has Diagnostics sig m) => WigginsOpts -> m (Graphing Dependency)
analyze opts = do
  results <- withWigginsBinary $ runWiggins opts
  pure $ convertResults results

runWiggins :: (Has Exec sig m, Has Diagnostics sig m) => WigginsOpts -> BinaryPaths -> m (Maybe VSIStandaloneResults)
runWiggins opts binaryPaths = do
  jsonEncodedLocators <- execWigginsRaw binaryPaths opts
  pure $ decode jsonEncodedLocators

convertResults :: Maybe VSIStandaloneResults -> Graphing Dependency
convertResults results = case results of
  Just _ -> empty
  Nothing -> empty
