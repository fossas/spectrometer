module Strategy.VSI
  ( discover,
  )
where

import App.Fossa.EmbeddedBinary
import App.Fossa.VPS.Scan.RunWiggins
import Control.Effect.Diagnostics
import Control.Effect.Lift
import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe
import Data.Text qualified as T
import Effect.Exec
import Fossa.API.Types
import GHC.Generics
import Graphing
import Path
import Srclib.Types (Locator (..), parseLocator)
import Types

newtype VSIProject = VSIProject
  { vsiDir :: Path Abs Dir
  }
  deriving (Eq, Ord, Show)

newtype VSILocator = VSILocator
  { unVSILocator :: T.Text
  }
  deriving (Show, Generic, FromJSON)

discover :: (Has Diagnostics sig m, Has (Lift IO) rsig run, MonadIO run, Has Exec rsig run, Has Diagnostics rsig run) => ApiOpts -> Path Abs Dir -> m [DiscoveredProject run]
discover apiOpts dir = context "VSI" $ do
  -- Right now we assume that if the VSI strategy is run, the top level root is a project to scan.
  -- In the future we will likely add heuristics to detect whether the VSI strategy will yield results.
  let wigginsOpts = generateVSIStandaloneOpts dir apiOpts
  pure [mkProject wigginsOpts (VSIProject dir)]

mkProject :: (Has (Lift IO) sig n, MonadIO n, Has Exec sig n, Has Diagnostics sig n) => WigginsOpts -> VSIProject -> DiscoveredProject n
mkProject wigginsOpts project =
  DiscoveredProject
    { projectType = "vsi",
      projectBuildTargets = mempty,
      projectDependencyGraph = const $ analyze wigginsOpts,
      projectPath = vsiDir project,
      projectLicenses = pure []
    }

analyze :: (Has (Lift IO) sig m, MonadIO m, Has Exec sig m, Has Diagnostics sig m) => WigginsOpts -> m (Graphing Dependency)
analyze opts = context "VSI" $ do
  vsiLocators <- context "Running VSI binary" $ withWigginsBinary (runWiggins opts)
  context "Building dependency graph" $ pure (toGraph vsiLocators)

runWiggins :: (Has Exec sig m, Has Diagnostics sig m) => WigginsOpts -> BinaryPaths -> m (Maybe [VSILocator])
runWiggins opts binaryPaths = do
  jsonEncodedLocators <- execWigginsRaw binaryPaths opts
  pure $ decode jsonEncodedLocators

toGraph :: Maybe [VSILocator] -> Graphing Dependency
toGraph vsiLocators = case vsiLocators of
  Just locators -> Graphing.fromList $ mapMaybe toDependency locators
  Nothing -> empty

toDependency :: VSILocator -> Maybe Dependency
toDependency vsiLocator = do
  let locator = parseLocator (unVSILocator vsiLocator)
  let name = locatorProject locator
  let revision = CEq <$> locatorRevision locator

  case toDepType locator of
    Just depType -> Just $ Dependency depType name revision [] [] mempty
    Nothing -> Nothing

-- toDepType converts the fetchers that the VSI strategy may output into the appropriate DepType.
toDepType :: Locator -> Maybe DepType
toDepType locator = case locatorFetcher locator of
  "git" -> Just GitType
  "archive" -> Just GooglesourceType
  "mvn" -> Just MavenType
  "nuget" -> Just NuGetType
  _ -> Nothing