module App.Fossa.VSIDeps (
  analyzeVSIDeps,
) where

import App.Fossa.EmbeddedBinary (withWigginsBinary)
import App.Fossa.IAT.ResolveAssertions (resolveShallowGraph)
import App.Fossa.VPS.Scan.RunWiggins (WigginsOpts, execWigginsJson, generateVSIStandaloneOpts, toPathFilters)
import Control.Algebra (Has)
import Control.Carrier.Diagnostics (renderDiagnostic)
import Control.Effect.Diagnostics (Diagnostics, ToDiagnostic, context, fromEither)
import Control.Effect.Lift (Lift)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON)
import Data.Text qualified as Text
import DepTypes (Dependency (Dependency))
import Discovery.Filters (AllFilters)
import Effect.Exec (Exec)
import Effect.Logger (pretty, viaShow)
import Fossa.API.Types (ApiOpts)
import GHC.Generics (Generic)
import Path (Abs, Dir, Path)
import Srclib.Types (Locator, SourceUnit (..), locatorFetcher, locatorProject, locatorRevision, parseLocator)
import Types (DepType (GitType, GooglesourceType, IATType, MavenType, NuGetType), VerConstraint (CEq))

newtype VSILocator = VSILocator
  { unVSILocator :: Text.Text
  }
  deriving (Eq, Ord, Show, Generic, FromJSON)

data ValidVSILocator = ValidVSILocator
  { validType :: DepType
  , validName :: Text.Text
  , validRevision :: Maybe Text.Text
  }

data VSIError = UnsupportedLocatorType Locator Text.Text
  deriving (Eq, Ord, Show)

instance ToDiagnostic VSIError where
  renderDiagnostic (UnsupportedLocatorType locator ty) =
    "Unsupported locator type: " <> pretty ty <> " . Locator: " <> viaShow locator

-- | VSI analysis is sufficiently different from other analysis types that it cannot be just another strategy.
-- Instead, VSI analysis is run separately over the entire scan directory, outputting its own source unit.
analyzeVSIDeps :: (MonadIO m, Has Diagnostics sig m, Has Exec sig m, Has (Lift IO) sig m) => Path Abs Dir -> ApiOpts -> AllFilters -> m SourceUnit
analyzeVSIDeps dir apiOpts filters = do
  direct <- pluginAnalyze $ generateVSIStandaloneOpts dir (toPathFilters filters) apiOpts
  resolveShallowGraph dir apiOpts direct

-- | The VSI plugin results in a shallow graph of direct dependencies.
pluginAnalyze :: (MonadIO m, Has (Lift IO) sig m, Has Exec sig m, Has Diagnostics sig m) => WigginsOpts -> m [Dependency]
pluginAnalyze opts = context "VSI" $ do
  vsiLocators <- context "Running VSI binary" $ withWigginsBinary (execWigginsJson opts)
  context "Building dependency graph" $ fromEither (toDependencies vsiLocators)

toDependencies :: [VSILocator] -> Either VSIError [Dependency]
toDependencies rawLocators = transformed
  where
    validated :: Either VSIError [ValidVSILocator]
    validated = traverse validateLocator rawLocators

    transformed :: Either VSIError [Dependency]
    transformed = map transformLocator <$> validated

validateLocator :: VSILocator -> Either VSIError ValidVSILocator
validateLocator vsiLocator = do
  let locator = parseLocator $ unVSILocator vsiLocator
  ty <- toDepType locator
  pure (ValidVSILocator ty (locatorProject locator) (locatorRevision locator))

transformLocator :: ValidVSILocator -> Dependency
transformLocator locator =
  Dependency
    (validType locator)
    (validName locator)
    (CEq <$> validRevision locator)
    []
    []
    mempty

toDepType :: Locator -> Either VSIError DepType
toDepType locator = case locatorFetcher locator of
  "git" -> Right GitType
  "archive" -> Right GooglesourceType
  "mvn" -> Right MavenType
  "nuget" -> Right NuGetType
  "iat" -> Right IATType
  other -> Left $ UnsupportedLocatorType locator other
