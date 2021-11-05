-- | Fused-effects wrapped functions from Path.IO
module Control.Effect.Path (
  withSystemTempDir,
) where

import Control.Effect.Lift (Has, Lift, liftWith)
import Path (Abs, Dir, Path)
import Path.IO qualified as PIO

withSystemTempDir ::
  Has (Lift IO) sig m =>
  -- | Directory name template
  String ->
  -- | Callback that can use the directory
  (Path Abs Dir -> m a) ->
  m a
withSystemTempDir name f = liftWith @IO $ \hdl ctx ->
  PIO.withSystemTempDir name (hdl . (<$ ctx) . f)
