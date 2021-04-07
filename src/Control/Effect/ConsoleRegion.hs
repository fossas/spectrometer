module Control.Effect.ConsoleRegion
  ( displayConsoleRegions,
    withConsoleRegion,
    setConsoleRegion,
    appendConsoleRegion,
    finishConsoleRegion,
    getConsoleRegion,
    module X,
  )
where

import Control.Effect.Lift
import Data.Text (Text)
import System.Console.Concurrent as X (Outputable)
import System.Console.Concurrent qualified as C
import System.Console.Regions as X (ConsoleRegion, RegionContent, RegionLayout (..), ToRegionContent (..))
import System.Console.Regions qualified as R

-- | See @"System.Console.Regions".'R.displayConsoleRegions'@.
displayConsoleRegions :: Has (Lift IO) sig m => m a -> m a
displayConsoleRegions act = liftWith @IO $ \hdl ctx ->
  R.displayConsoleRegions $ hdl (act <$ ctx)

-- | See @"System.Console.Regions".'R.withConsoleRegion'@.
withConsoleRegion :: Has (Lift IO) sig m => R.RegionLayout -> (R.ConsoleRegion -> m a) -> m a
withConsoleRegion layout act = liftWith @IO $ \hdl ctx ->
  R.withConsoleRegion layout $ \region -> hdl (act region <$ ctx)

-- | See @"System.Console.Regions".'R.setConsoleRegion'@.
setConsoleRegion :: (R.ToRegionContent v, Has (Lift IO) sig m) => R.ConsoleRegion -> v -> m ()
setConsoleRegion region value = sendIO $ R.setConsoleRegion region value

-- | See @"System.Console.Regions".'R.appendConsoleRegion'@.
appendConsoleRegion :: (C.Outputable v, Has (Lift IO) sig m) => R.ConsoleRegion -> v -> m ()
appendConsoleRegion region value = sendIO $ R.appendConsoleRegion region value

-- | See @"System.Console.Regions".'R.finishConsoleRegion'@.
finishConsoleRegion :: (C.Outputable v, Has (Lift IO) sig m) => R.ConsoleRegion -> v -> m ()
finishConsoleRegion region value = sendIO $ R.finishConsoleRegion region value

-- | See @"System.Console.Regions".'R.getConsoleRegion'@.
getConsoleRegion :: (Has (Lift IO) sig m) => R.ConsoleRegion -> m Text
getConsoleRegion region = sendIO $ R.getConsoleRegion region
