module App.Fossa.BinaryDeps (analyzeBinaryDeps) where

import App.Fossa.VSI.IAT.Fingerprint (fingerprintRaw)
import App.Fossa.VSI.IAT.Types (Fingerprint (..))
import App.Fossa.VSI.Types qualified as VSI
import Control.Algebra (Has)
import Control.Carrier.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift)
import Data.Foldable (traverse_)
import Data.Maybe (catMaybes)
import Discovery.Filters (AllFilters)
import Discovery.Walk (WalkStep (WalkContinue), walk')
import Effect.Logger (Logger, Pretty (pretty), logInfo)
import Effect.ReadFS (ReadFS, fileIsBinary)
import Fossa.API.Types (ApiOpts)
import Graphing qualified
import Path (Abs, Dir, File, Path, Rel)
import Srclib.Types (SourceUnit)

data BinaryFile = BinaryFile
  { binaryPath :: Path Abs File
  , binaryFingerprint :: Fingerprint
  }
  deriving (Show)

-- | Binary detection is sufficiently different from other analysis types that it cannot be just another strategy.
-- Instead, binary detection is run separately over the entire scan directory, outputting its own source unit.
-- The goal of this feature is to enable a FOSSA user to flag all vendored binaries (as defined by git) in the project as dependencies.
-- Users may then use standard FOSSA UX flows to ignore or add license information to the detected binaries.
analyzeBinaryDeps :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has ReadFS sig m, Has Logger sig m) => Path Abs Dir -> AllFilters -> m (Maybe SourceUnit)
analyzeBinaryDeps dir filters = do
  binaries <- fingerprintBinaries filters dir

  logInfo "Found binaries in project:"
  traverse_ (logInfo . pretty . show) binaries

  pure Nothing

fingerprintBinaries :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Lift IO) sig m) => AllFilters -> Path Abs Dir -> m [BinaryFile]
fingerprintBinaries filters = walk' $ \_ _ files -> do
  someBinaries <- traverse fingerprintIfBinary files
  pure (catMaybes someBinaries, WalkContinue)

fingerprintIfBinary :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has ReadFS sig m) => Path Abs File -> m (Maybe BinaryFile)
fingerprintIfBinary file = do
  isBinary <- fileIsBinary file
  if isBinary
    then do
      fp <- fingerprintRaw file
      pure . Just $ BinaryFile file fp
    else pure Nothing

-- undefined

-- (direct, userDeps) <- pluginAnalyze $ generateVSIStandaloneOpts dir (toPathFilters filters) apiOpts

-- resolvedUserDeps <- resolveUserDefined apiOpts userDeps
-- resolvedGraph <- resolveGraph apiOpts direct
-- dependencies <- fromEither $ Graphing.gtraverse VSI.toDependency resolvedGraph

-- pure $ toSourceUnit (toProject dir dependencies) resolvedUserDeps
