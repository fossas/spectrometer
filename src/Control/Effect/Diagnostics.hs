module Control.Effect.Diagnostics
  ( Diagnostics (..),
    SomeDiagnostic (..),
    fatal,
    context,
    fatalText,
    recover,
    fromEither,
    fromEitherShow,
    (<||>),
    ToDiagnostic (..),
    tagError,
    module X,
  )
where

import Control.Algebra as X
import Control.Exception (SomeException (..))
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import Prelude

data Diagnostics m k where
  Fatal :: ToDiagnostic diag => diag -> Diagnostics m a
  Recover :: m a -> Diagnostics m (Maybe a)
  Context :: Text -> m a -> Diagnostics m a

fatal :: (Has Diagnostics sig m, ToDiagnostic diag) => diag -> m a
fatal = send . Fatal

fatalText :: Has Diagnostics sig m => Text -> m a
fatalText = fatal

-- | Recover from a fatal error. The previously-fatal error will appear in 'diagWarnings'
recover :: Has Diagnostics sig m => m a -> m (Maybe a)
recover = send . Recover

-- | Push context onto the stack for "stack traces" in diagnostics.
--
-- This is spiritually similar to "errors.Wrap" from golang, but it handles the plumbing for you
context :: Has Diagnostics sig m => Text -> m a -> m a
context ctx go = send (Context ctx go)

-- | Lift an Either result into the Diagnostics effect, given a ToDiagnostic instance for the error type
fromEither :: (ToDiagnostic err, Has Diagnostics sig m) => Either err a -> m a
fromEither = either fatal pure

-- | Lift an Either result into the Diagnostics effect, given a Show instance for the error type
fromEitherShow :: (Show err, Has Diagnostics sig m) => Either err a -> m a
fromEitherShow = either (fatal . T.pack . show) pure

-- | Lift an Either result into the Diagnostics effect, given a function from the error type to another type that implements 'ToDiagnostic'
tagError :: (ToDiagnostic e', Has Diagnostics sig m) => (e -> e') -> Either e a -> m a
tagError f (Left e) = fatal (f e)
tagError _ (Right a) = pure a

infixl 3 <||>

(<||>) :: Has Diagnostics sig m => m a -> m a -> m a
(<||>) ma mb = do
  maybeA <- recover $ ma
  case maybeA of
    Nothing -> mb
    Just a -> pure a

class ToDiagnostic a where
  renderDiagnostic :: a -> Doc AnsiStyle

instance ToDiagnostic Text where
  renderDiagnostic = pretty

instance ToDiagnostic SomeException where
  renderDiagnostic (SomeException exc) =
    "An exception occurred: " <> pretty (show exc)

-- | An error with a ToDiagnostic instance and an associated stack trace
data SomeDiagnostic where
  SomeDiagnostic :: ToDiagnostic a => [Text] -> a -> SomeDiagnostic
