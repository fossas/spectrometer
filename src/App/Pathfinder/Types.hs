module App.Pathfinder.Types (
  LicenseAnalyzeProject (..),
) where

import Types
import Control.Effect.Diagnostics (Diagnostics)
import Effect.Logger (Logger)
import Effect.Exec (Exec)
import Effect.ReadFS (ReadFS)
import Control.Monad.IO.Class (MonadIO)
import Control.Effect.Lift (Lift)
import Control.Algebra (Has)

type TaskEffs sig m =
  ( Has (Lift IO) sig m
  , MonadIO m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has Logger sig m
  , Has Diagnostics sig m
  )

class LicenseAnalyzeProject a where
  licenseAnalyzeProject :: TaskEffs sig m => a -> m [LicenseResult]
