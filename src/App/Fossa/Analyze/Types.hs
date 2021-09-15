module App.Fossa.Analyze.Types (
  AnalyzeProject (..),
) where

import Control.Carrier.Diagnostics
import Control.Effect.Lift
import Control.Monad.IO.Class (MonadIO)
import Effect.Exec
import Effect.Logger
import Effect.ReadFS
import Types

type TaskEffs sig m =
  ( Has (Lift IO) sig m
  , MonadIO m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has Logger sig m
  , Has Diagnostics sig m
  )

class AnalyzeProject a where
  analyzeProject :: TaskEffs sig m => FoundTargets -> a -> m DependencyResults
