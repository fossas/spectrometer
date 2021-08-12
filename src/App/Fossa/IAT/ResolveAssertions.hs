module App.Fossa.IAT.ResolveAssertions (
  resolveAssertions,
) where

import App.Fossa.ManualDeps
import Control.Algebra
import Control.Effect.Diagnostics
import Control.Effect.Lift
import DepTypes
import Effect.Logger
import Fossa.API.Types
import Srclib.Types

resolveAssertions :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has Logger sig m) => ApiOpts -> [Dependency] -> m (Maybe SourceUnit)
resolveAssertions apiOpts deps = undefined
