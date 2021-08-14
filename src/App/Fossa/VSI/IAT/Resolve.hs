{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VSI.IAT.Resolve (
  resolveUserDefined,
) where

import App.Fossa.FossaAPIV1 qualified as Fossa
import App.Fossa.VSI.IAT.Types (
  UserDefinedAssertionMeta (..),
  UserDep,
 )
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift)
import Fossa.API.Types (ApiOpts)
import Srclib.Types (
  SourceUserDefDep (..),
 )

resolveUserDefined :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> [UserDep] -> m (Maybe [SourceUserDefDep])
resolveUserDefined apiOpts deps = do
  assertions <- traverse (Fossa.resolveUserDefinedBinary apiOpts) deps
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
