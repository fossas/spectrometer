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

newtype VSIProject = VSIProject
  { vsiDir :: Path Abs Dir
  } deriving (Eq, Ord, Show)

newtype VSIStandaloneResults = VSIStandaloneResults
  { vsiStandaloneLocators :: [T.Text]
  } deriving (Show, Generic, FromJSON)

discover :: (Has Diagnostics sig m, Has Exec rsig run, Has Diagnostics rsig run) => ApiOpts -> Path Abs Dir -> BinaryPaths -> m [DiscoveredProject run]
discover apiOpts dir binaryPaths = context "VSI" $ do
  -- Right now we assume that if the VSI strategy is run, the top level root is a project to scan.
  -- In the future we will likely add heuristics to detect whether the VSI strategy will yield results.
  let wigginsOpts = generateVSIStandaloneOpts dir apiOpts
  pure [mkProject binaryPaths wigginsOpts (VSIProject dir)]

mkProject :: (Has Exec sig n, Has Diagnostics sig n) => BinaryPaths -> WigginsOpts -> VSIProject -> DiscoveredProject n
mkProject binaryPaths wigginsOpts project =
  DiscoveredProject
    { projectType = "vsi"
    , projectBuildTargets = mempty
    , projectDependencyGraph = const $ analyze binaryPaths wigginsOpts
    , projectPath = vsiDir project
    , projectLicenses = pure []
    }

analyze :: (Has Exec sig m, Has Diagnostics sig m) => BinaryPaths -> WigginsOpts -> m (Graphing Dependency)
analyze binaryPaths opts = do
  results <- runWiggins binaryPaths opts
  pure $ convertResults results

runWiggins :: ( Has Exec sig m, Has Diagnostics sig m) => BinaryPaths -> WigginsOpts -> m (Maybe VSIStandaloneResults)
runWiggins binaryPaths opts = do
  jsonEncodedLocators <- execWigginsRaw binaryPaths opts
  pure $ decode jsonEncodedLocators

convertResults :: Maybe VSIStandaloneResults -> Graphing Dependency
convertResults results = case results of
  Just _ -> empty
  Nothing -> empty
