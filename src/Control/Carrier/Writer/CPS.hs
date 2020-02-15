module Control.Carrier.Writer.CPS
  ( WriterC(..)
  , runWriter
  ) where

import Prelude

import qualified Control.Monad.Trans.Writer.CPS as CPS
import Data.Tuple (swap)
import Data.Monoid (Endo (..))

newtype WriterC w m a = WriterC { runWriterC :: CPS.WriterT w m a }


runWriter :: (Functor m, Monoid w) => WriterC w m a -> m (w, a)
runWriter = fmap swap . CPS.runWriterT . runWriterC
