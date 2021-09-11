{-# LANGUAGE RecordWildCards #-}

module App.Fossa.BinaryDeps (analyzeBinaryDeps) where

import App.Fossa.Analyze.Project (ProjectResult (..))
import App.Fossa.VSI.IAT.Fingerprint (fingerprintRaw)
import App.Fossa.VSI.IAT.Types (Fingerprint (..))
import Control.Algebra (Has)
import Control.Applicative ((<|>))
import Control.Carrier.Diagnostics (Diagnostics, fromEither)
import Control.Effect.Lift (Lift)
import Data.ByteString qualified as BS
import Data.Either (partitionEithers)
import Data.List (isSuffixOf)
import Data.Maybe (catMaybes)
import Data.String.Conversion (ToString (toString), toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Discovery.Filters (AllFilters (..), FilterCombination (combinedPaths))
import Discovery.Walk (WalkStep (WalkContinue), walk')
import Effect.ReadFS (ReadFS, readContentsBSLimit)
import Graphing (fromList)
import Path (Abs, Dir, File, Path, filename, isProperPrefixOf, stripProperPrefix, toFilePath, (</>))
import Srclib.Converter qualified as Srclib
import Srclib.Types (AdditionalDepData (..), SourceUnit (..), SourceUserDefDep (..))
import Types (Dependency (..), GraphBreadth (Complete))

-- | Binary detection is sufficiently different from other analysis types that it cannot be just another strategy.
-- Instead, binary detection is run separately over the entire scan directory, outputting its own source unit.
-- The goal of this feature is to enable a FOSSA user to flag all vendored binaries (as defined by git) in the project as dependencies.
-- Users may then use standard FOSSA UX flows to ignore or add license information to the detected binaries.
analyzeBinaryDeps :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has ReadFS sig m) => Path Abs Dir -> AllFilters -> m (Maybe SourceUnit)
analyzeBinaryDeps dir filters = do
  binaryPaths <- findBinaries (toPathFilters dir filters) dir
  if null binaryPaths
    then pure Nothing
    else do
      resolvedBinaries <- traverse (resolveBinary strategies dir) binaryPaths
      let (plain, user) = partitionEithers resolvedBinaries
      pure . Just $ toSourceUnit (toProject dir plain) user

findBinaries :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has ReadFS sig m) => PathFilters -> Path Abs Dir -> m [Path Abs File]
findBinaries filters = walk' $ \dir _ files -> do
  if shouldFingerprintDir dir filters
    then do
      someBinaries <- traverse pathIfBinary files
      pure (catMaybes someBinaries, WalkContinue)
    else pure ([], WalkContinue)

pathIfBinary :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has ReadFS sig m) => Path Abs File -> m (Maybe (Path Abs File))
pathIfBinary file = do
  isBinary <- fileIsBinary file
  if isBinary
    then pure . Just $ file
    else pure Nothing

-- | PathFilters is a specialized filter mechanism that operates only on absolute directory paths.
data PathFilters = PathFilters
  { include :: [Path Abs Dir]
  , exclude :: [Path Abs Dir]
  }
  deriving (Show)

toPathFilters :: Path Abs Dir -> AllFilters -> PathFilters
toPathFilters root filters =
  PathFilters
    { include = map (root </>) (combinedPaths $ includeFilters filters)
    , exclude = map (root </>) (combinedPaths $ excludeFilters filters)
    }

shouldFingerprintDir :: Path Abs Dir -> PathFilters -> Bool
shouldFingerprintDir dir filters = (not shouldExclude) && shouldInclude
  where
    shouldExclude = (isPrefixedOrEqual dir) `any` (exclude filters)
    shouldInclude = null (include filters) || (isPrefixedOrEqual dir) `any` (include filters)
    isPrefixedOrEqual a b = a == b || isProperPrefixOf b a -- swap order of isProperPrefixOf comparison because we want to know if dir is prefixed by any filter

toProject :: Path Abs Dir -> [Dependency] -> ProjectResult
toProject dir deps = ProjectResult "binary-deps" dir (fromList deps) Complete []

toSourceUnit :: ProjectResult -> [SourceUserDefDep] -> SourceUnit
toSourceUnit project binaries = do
  let unit = Srclib.toSourceUnit project
  unit{additionalData = Just $ AdditionalDepData (Just binaries) Nothing}

-- | Just render the first few characters of the fingerprint.
-- The goal is to provide a high confidence that future binaries with the same name won't collide,
-- and we don't need all 256 bits for that.
renderFingerprint :: Fingerprint -> Text
renderFingerprint fingerprint = Text.take 12 $ unFingerprint fingerprint

renderRelative :: Path Abs Dir -> Path Abs File -> Text
renderRelative absDir absFile =
  case stripProperPrefix absDir absFile of
    Left _ -> toText . toFilePath $ absFile
    Right relFile -> toText . toFilePath $ relFile

-- | Determine if a file is binary using the same method as git:
-- "is there a zero byte in the first 8000 bytes of the file"
fileIsBinary :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m Bool
fileIsBinary file = do
  attemptedContent <- readContentsBSLimit file 8000
  content <- fromEither attemptedContent
  pure $ BS.elem 0 content

-- | Try the next strategy in the list. If successful, evaluate to its result; if not move down the list of strategies and try again.
-- Eventually falls back to strategyRawFingerprint if no other strategy succeeds.
resolveBinary :: (Has (Lift IO) sig m, Has ReadFS sig m, Has Diagnostics sig m) => [(Path Abs Dir -> Path Abs File -> m (Maybe (Either Dependency SourceUserDefDep)))] -> Path Abs Dir -> Path Abs File -> m (Either Dependency SourceUserDefDep)
resolveBinary (resolve : remainingStrategies) root file = do
  result <- resolve root file
  case result of
    Just r -> pure r
    Nothing -> resolveBinary remainingStrategies root file
resolveBinary [] root file = strategyRawFingerprint root file

-- | Functions which may be able to resolve a binary to a dependency.
strategies :: (Has (Lift IO) sig m, Has ReadFS sig m, Has Diagnostics sig m) => [(Path Abs Dir -> Path Abs File -> m (Maybe (Either Dependency SourceUserDefDep)))]
strategies =
  [strategyJar]

-- | Fallback strategy: resolve to a user defined dependency for the binary, where the name is the relative path and the version is the fingerprint.
-- This strategy is used if no other strategy succeeds at resolving the binary.
strategyRawFingerprint :: (Has (Lift IO) sig m, Has Diagnostics sig m) => Path Abs Dir -> Path Abs File -> m (Either Dependency SourceUserDefDep)
strategyRawFingerprint root file = do
  fp <- fingerprintRaw file
  pure . Right $ SourceUserDefDep (renderRelative root file) (renderFingerprint fp) "" (Just "Binary discovered in source tree") Nothing

-- | Implement JAR resolution using a similar method to Ant analysis in CLIv1
strategyJar :: (Has (Lift IO) sig m, Has Diagnostics sig m) => Path Abs Dir -> Path Abs File -> m (Maybe (Either Dependency SourceUserDefDep))
strategyJar _ file =
  if validSuffix
    then case fromPom <|> fromMetaInf of
      Just dep -> pure . Just $ Left dep
      Nothing -> pure Nothing
    else pure Nothing
  where
    validSuffix = any (isSuffixOf (toString $ filename file)) [".jar", ".aar"]
    fromPom :: Maybe Dependency
    fromPom = Nothing
    fromMetaInf :: Maybe Dependency
    fromMetaInf = Nothing
