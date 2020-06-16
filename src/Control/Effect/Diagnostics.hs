module Control.Effect.Diagnostics
  ( Diagnostics (..),
    DiagnosticsC (..),
    SomeDiagnostic (..),
    FailureBundle (..),
    ResultBundle (..),
    renderFailureBundle,
    renderWarnings,
    runDiagnostics,
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
import Control.Carrier.Error.Either (ErrorC, catchError, runError, throwError)
import Control.Carrier.Reader (ReaderC, ask, local, runReader)
import Control.Carrier.Writer.Church (WriterC, runWriter, tell)
import Control.Exception (SomeException (..))
import Control.Monad.IO.Class (MonadIO)
import Data.Monoid (Endo (..))
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import Prelude

data Diagnostics m k where
  Fatal :: ToDiagnostic diag => diag -> Diagnostics m a
  Recover :: m a -> Diagnostics m (Maybe a)
  Context :: Text -> m a -> Diagnostics m a

newtype DiagnosticsC m a = DiagnosticsC {runDiagnosticsC :: ReaderC [Text] (ErrorC SomeDiagnostic (WriterC (Endo [SomeDiagnostic]) m)) a}
  deriving (Functor, Applicative, Monad, MonadIO)

data FailureBundle = FailureBundle
  { failureWarnings :: [SomeDiagnostic],
    failureCause :: SomeDiagnostic
  }

instance Show FailureBundle where
  show = show . renderFailureBundle

data ResultBundle a = ResultBundle
  { resultWarnings :: [SomeDiagnostic],
    resultValue :: a
  }

renderFailureBundle :: FailureBundle -> Doc AnsiStyle
renderFailureBundle FailureBundle {..} =
  vsep
    [ "A fatal error occurred:",
      "",
      indent 4 (align (renderSomeDiagnostic failureCause)),
      "",
      "----------",
      "Relevant warnings include:",
      "",
      indent 4 (align (renderWarnings failureWarnings))
    ]

renderSomeDiagnostic :: SomeDiagnostic -> Doc AnsiStyle
renderSomeDiagnostic (SomeDiagnostic stack cause) = renderDiagnostic cause <> line <> align (indent 2 (vsep (map (pretty . ("when " <>)) stack)))

renderWarnings :: [SomeDiagnostic] -> Doc AnsiStyle
renderWarnings = align . vsep . map renderSomeDiagnostic

runDiagnostics :: Applicative m => DiagnosticsC m a -> m (Either FailureBundle (ResultBundle a))
runDiagnostics = fmap bundle . runWriter (\w a -> pure (appEndo w [], a)) . runError @SomeDiagnostic . runReader [] . runDiagnosticsC
  where
    bundle (warnings, res) =
      case res of
        Left err -> Left (FailureBundle warnings err)
        Right a -> Right (ResultBundle warnings a)

instance Algebra sig m => Algebra (Diagnostics :+: sig) (DiagnosticsC m) where
  alg hdl sig ctx = DiagnosticsC $ case sig of
    L (Fatal diag) -> ask >>= \path -> throwError (SomeDiagnostic path diag)
    L (Context path go) -> local (path :) $ runDiagnosticsC $ hdl (go <$ ctx)
    L (Recover act) -> do
      (fmap (fmap Just)) (runDiagnosticsC $ hdl (act <$ ctx)) `catchError` (\(diag :: SomeDiagnostic) -> tell (Endo (diag :)) *> pure (Nothing <$ ctx))
    R other -> alg (runDiagnosticsC . hdl) (R (R (R other))) ctx

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
