{-# LANGUAGE RecordWildCards #-}

module App.Fossa.IAT.ResolveAssertions (
  resolveAssertions,
) where

import App.Fossa.FossaAPIV1 qualified as Fossa
import App.Fossa.IAT.Types
import Control.Algebra
import Control.Effect.Diagnostics
import Control.Effect.Lift
import Data.Maybe
import Data.String.Conversion
import DepTypes
import Fossa.API.Types
import Path
import Srclib.Converter
import Srclib.Types
import Types

resolveAssertions :: (Has (Lift IO) sig m, Has Diagnostics sig m) => Path Abs Dir -> ApiOpts -> [Dependency] -> m (Maybe SourceUnit)
resolveAssertions _ _ [] = pure Nothing
resolveAssertions root apiOpts deps = do
  assertions <- traverse (Fossa.resolveUserDefinedBinary apiOpts) (catMaybes $ toLocator <$> deps)
  if null assertions
    then pure Nothing
    else pure (Just $ assertedDepsSrcUnit root (toUserDefDep <$> assertions))

toUserDefDep :: UserDefinedAssertionMeta -> SourceUserDefDep
toUserDefDep UserDefinedAssertionMeta{..} =
  SourceUserDefDep
    { srcUserDepName = assertedName
    , srcUserDepVersion = assertedVersion
    , srcUserDepLicense = assertedLicense
    , srcUserDepDescription = assertedDescription
    , srcUserDepHomepage = assertedUrl
    }

assertedDepsSrcUnit :: Path Abs Dir -> [SourceUserDefDep] -> SourceUnit
assertedDepsSrcUnit root deps = do
  let renderedPath = toText root
  SourceUnit
    { sourceUnitName = renderedPath
    , sourceUnitManifest = renderedPath
    , sourceUnitType = "iat-asserted-matches"
    , sourceUnitBuild = Nothing
    , sourceUnitGraphBreadth = Complete
    , additionalData = Just $ AdditionalDepData (Just deps) Nothing
    }

-- | toLocator is a special case of Dependency to Locator.
-- All asserted dependencies are of type IATType, have a name, and have a non-empty version which is constrained to a specific version.
-- Anything else shouldn't even get here, but if it does it should be ignored.
toLocator :: Dependency -> Maybe Locator
toLocator Dependency{dependencyType = IATType, dependencyName, dependencyVersion = Just (CEq v)} = Just $ Locator (depTypeToFetcher IATType) dependencyName (Just v)
toLocator _ = Nothing
