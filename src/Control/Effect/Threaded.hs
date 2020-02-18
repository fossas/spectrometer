module Control.Effect.Threaded
  ( Threaded(..)
  , Handle(..)
  , module X
  ) where

import Control.Algebra as X
import qualified Control.Concurrent as Conc
import Control.Concurrent.STM
import Control.Exception (SomeException)
import Prelude

data Threaded m k
  = forall a. Fork (m a) (Handle -> m k)
  | Kill Handle (m k)
  | Wait Handle (m k)

instance HFunctor Threaded where
  hmap f (Fork m k) = Fork (f m) (f . k)
  hmap f (Kill h k) = Kill h (f k)
  hmap f (Wait h k) = Wait h (f k)

instance Effect Threaded where
  thread ctx handler (Fork m k) = Fork (handler (m <$ ctx)) (handler . (<$ ctx) . k)
  thread ctx handler (Kill h k) = Kill h (handler (k <$ ctx))
  thread ctx handler (Wait h k) = Wait h (handler (k <$ ctx))

data Handle = Handle
  { handleTid  :: Conc.ThreadId
  , handleWait :: STM (Either SomeException ())
  }
