-- | An effect for "logging" to a sticky region in the console
module Control.Effect.StickyLogger (
  StickyLogger,
  SStickyLogger (..),
  logSticky,
  logSticky',
  module X,
) where

import Control.Algebra as X
import Control.Carrier.Simple
import Data.Text (Text)
import Prettyprinter (Doc, pretty)
import Prettyprinter.Render.Terminal (AnsiStyle)

data SStickyLogger a where
  LogSticky' :: Doc AnsiStyle -> SStickyLogger ()

type StickyLogger = Simple SStickyLogger

-- | Set the contents of the sticky region to the provided message
logSticky :: Has StickyLogger sig m => Text -> m ()
logSticky = logSticky' . pretty

logSticky' :: Has StickyLogger sig m => Doc AnsiStyle -> m ()
logSticky' = sendSimple . LogSticky'
