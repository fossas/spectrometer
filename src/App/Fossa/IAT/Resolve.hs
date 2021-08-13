{-# LANGUAGE RecordWildCards #-}

module App.Fossa.IAT.Resolve (
  resolveShallowGraph,
) where

import App.Fossa.Analyze.Project (ProjectResult (..))
import App.Fossa.FossaAPIV1 qualified as Fossa
import App.Fossa.IAT.Types
import Control.Algebra
import Control.Effect.Diagnostics
import Control.Effect.Lift
import Data.Maybe
import DepTypes
import Fossa.API.Types
import Graphing qualified
import Path
import Srclib.Converter qualified as Srclib
import Srclib.Types
import Types

resolveShallowGraph :: (Has (Lift IO) sig m, Has Diagnostics sig m) => Path Abs Dir -> ApiOpts -> [Dependency] -> m SourceUnit
resolveShallowGraph dir apiOpts shallow = do
  let (userDefined, direct) = extractUserDefinedBinaries shallow
  resolvedUserDefined <- resolveUserDefined apiOpts userDefined

  let project = ProjectResult "vsi" dir (Graphing.fromList direct) Complete
  pure $ toSourceUnit project resolvedUserDefined

toSourceUnit :: ProjectResult -> Maybe [SourceUserDefDep] -> SourceUnit
toSourceUnit project deps = do
  let unit = Srclib.toSourceUnit project
  unit{additionalData = fmap toDepData deps}
  where
    toDepData d = AdditionalDepData (Just d) Nothing

resolveUserDefined :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> [Dependency] -> m (Maybe [SourceUserDefDep])
resolveUserDefined _ [] = pure Nothing
resolveUserDefined apiOpts deps = do
  assertions <- traverse (Fossa.resolveUserDefinedBinary apiOpts) (catMaybes $ toLocator <$> deps)
  if null assertions
    then pure Nothing
    else pure . Just $ map toUserDefDep assertions

toUserDefDep :: UserDefinedAssertionMeta -> SourceUserDefDep
toUserDefDep UserDefinedAssertionMeta{..} =
  SourceUserDefDep
    { srcUserDepName = assertedName
    , srcUserDepVersion = assertedVersion
    , srcUserDepLicense = assertedLicense
    , srcUserDepDescription = assertedDescription
    , srcUserDepHomepage = assertedUrl
    }

-- | toLocator is a special case of Dependency to Locator.
-- All asserted dependencies are of type IATType, have a name, and have a non-empty version which is constrained to a specific version.
-- Anything else shouldn't even get here, but if it does it should be ignored.
toLocator :: Dependency -> Maybe Locator
toLocator Dependency{dependencyType = IATType, dependencyName, dependencyVersion = Just (CEq v)} =
  Just $ Locator (Srclib.depTypeToFetcher IATType) dependencyName (Just v)
toLocator _ = Nothing
