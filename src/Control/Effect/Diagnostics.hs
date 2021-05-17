{-# LANGUAGE GADTs #-}

-- | The Diagnostics effect is a replacement for the Error effect in most cases. It models an unchecked exceptions pattern, and provides for:
--
-- - "stack trace"-like behavior, closely resembling the golang pattern of errors.Wrap (see: 'context')
--
-- - recovery from failures, recording them as "warnings" (see: 'recover' or '<||>')
{-# LANGUAGE RecordWildCards #-}
module Control.Effect.Diagnostics
  ( -- * Diagnostics effect and operations
    Diagnostics (..),
    fatal,
    context,
    recover,
    recover',

    -- * Diagnostic result types
    FailureBundle (..),
    renderFailureBundle,
    renderSomeDiagnostic,

    -- * Diagnostic helpers
    fatalText,
    fromEither,
    fromEitherShow,
    fromMaybe,
    fromMaybeText,
    tagError,
    (<||>),
    combineSuccessful,

    -- * ToDiagnostic typeclass
    ToDiagnostic (..),
    SomeDiagnostic (..),
    module X,
  )
where

import Control.Algebra as X
import Control.Exception (SomeException (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import Data.Semigroup (sconcat)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import Prelude

data Diagnostics m k where
  Fatal :: ToDiagnostic diag => diag -> Diagnostics m a
  Recover' :: m a -> Diagnostics m (Either SomeDiagnostic a)
  Context :: Text -> m a -> Diagnostics m a

-- | A class of diagnostic types that can be rendered in a user-friendly way
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

-- | Analagous to @throwError@ from the error effect
fatal :: (Has Diagnostics sig m, ToDiagnostic diag) => diag -> m a
fatal = send . Fatal

-- | Throw an untyped string error
fatalText :: Has Diagnostics sig m => Text -> m a
fatalText = fatal

-- | Recover from a fatal error. The error will be recorded as a warning instead.
recover :: Has Diagnostics sig m => m a -> m (Maybe a)
recover = fmap (either (const Nothing) Just) . recover'

-- | Recover from a fatal error. The error will be recorded as a warning instead.
recover' :: Has Diagnostics sig m => m a -> m (Either SomeDiagnostic a)
recover' = send . Recover'

-- | Push context onto the stack for "stack traces" in diagnostics.
--
-- This is spiritually similar to @errors.Wrap@ from golang
context :: Has Diagnostics sig m => Text -> m a -> m a
context ctx go = send (Context ctx go)

-- | Lift an Either result into the Diagnostics effect, given a ToDiagnostic instance for the error type
fromEither :: (ToDiagnostic err, Has Diagnostics sig m) => Either err a -> m a
fromEither = either fatal pure

-- | Lift an Either result into the Diagnostics effect, given a Show instance for the error type
fromEitherShow :: (Show err, Has Diagnostics sig m) => Either err a -> m a
fromEitherShow = either (fatal . T.pack . show) pure

-- | Lift a Maybe result into Diagnostics, with the given diagnostic thrown for @Nothing@
fromMaybe :: (ToDiagnostic err, Has Diagnostics sig m) => err -> Maybe a -> m a
fromMaybe msg = maybe (fatal msg) pure

-- | Lift a Maybe result into Diagnostics, with a Text error message for @Nothing@
fromMaybeText :: Has Diagnostics sig m => Text -> Maybe a -> m a
fromMaybeText = fromMaybe

-- | Lift an Either result into the Diagnostics effect, given a function from the error type to another type that implements 'ToDiagnostic'
tagError :: (ToDiagnostic e', Has Diagnostics sig m) => (e -> e') -> Either e a -> m a
tagError f (Left e) = fatal (f e)
tagError _ (Right a) = pure a

infixl 3 <||>

-- | Analagous to @Alternative@'s @<|>@. Tries both actions and chooses the result that succeeds, invoking 'recover' semantics for errors.
(<||>) :: Has Diagnostics sig m => m a -> m a -> m a
(<||>) ma mb = recover ma >>= maybe mb pure

-- | Run a list of actions, combining the successful ones. If all actions fail, 'fatalText' is invoked with the provided @Text@ message.
combineSuccessful :: (Semigroup a, Has Diagnostics sig m) => Text -> [m a] -> m a
combineSuccessful msg actions = do
  results <- traverse recover actions
  let successful = NE.nonEmpty $ catMaybes results
  case successful of
    Nothing -> fatalText msg
    Just xs -> pure (sconcat xs)

data FailureBundle = FailureBundle
  { failureWarnings :: [SomeDiagnostic],
    failureCause :: SomeDiagnostic
  }

instance Show FailureBundle where
  show = show . renderFailureBundle

renderFailureBundle :: FailureBundle -> Doc AnsiStyle
renderFailureBundle FailureBundle {..} =
  vsep
    [ "----------",
      "An error occurred:",
      "",
      indent 4 (align (renderSomeDiagnostic failureCause)),
      "",
      ">>>",
      indent 2 "Relevant warnings include:",
      "",
      indent 4 (align (renderWarnings failureWarnings))
    ]

renderSomeDiagnostic :: SomeDiagnostic -> Doc AnsiStyle
renderSomeDiagnostic (SomeDiagnostic stack cause) = renderDiagnostic cause <> line <> align (indent 2 (vsep (map (pretty . ("when " <>)) stack)))

renderWarnings :: [SomeDiagnostic] -> Doc AnsiStyle
renderWarnings = align . vsep . map renderSomeDiagnostic
