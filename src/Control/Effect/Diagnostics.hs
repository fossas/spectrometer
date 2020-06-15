module Control.Effect.Diagnostics
  ( Diagnostics (..),
    DiagnosticsC (..),
    DiagnosticBundle (..),
    runDiagnostics,
    diagnosticBundlePretty,
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
import Control.Carrier.Writer.Church (WriterC, runWriter, tell)
import Control.Carrier.Reader (ReaderC, runReader, local, ask)
import Control.Exception (Exception)
import qualified Data.Text as T
import Prologue

-- fatal
-- warning
-- distinction between "this happened in another execution path" and "this happened in the current execution path"

data Diagnostics m k where
  Fatal :: ToDiagnostic diag => diag -> Diagnostics m a
  Recover :: m a -> Diagnostics m (Maybe a)
  Context :: Text -> m a -> Diagnostics m a

-- initial attempt:
-- - there is no "path" or "context" (no ReaderC)
-- - every 'tell' via WriterC is a fatal branch that was cut
-- TODO: list is bad as a writer type
newtype DiagnosticsC m a = DiagnosticsC {runDiagnosticsC :: ReaderC [Text] (ErrorC SomeDiagnostic (WriterC [SomeDiagnostic] m)) a}
  deriving (Functor, Applicative, Monad, MonadIO)

data DiagnosticBundle = DiagnosticBundle
  deriving (Typeable, Show)

-- FIXME: remove
instance Exception DiagnosticBundle

diagnosticBundlePretty :: DiagnosticBundle -> Text
diagnosticBundlePretty _ = "<diagnosticbundlepretty>"

runDiagnostics :: Monad m => DiagnosticsC m a -> m ([SomeDiagnostic], Either SomeDiagnostic a)
runDiagnostics = runWriter (\w a -> pure (w, a)) . runError @SomeDiagnostic . runReader [] . runDiagnosticsC

-- FIXME
instance Algebra sig m => Algebra (Diagnostics :+: sig) (DiagnosticsC m) where
  alg hdl sig ctx = DiagnosticsC $ case sig of
    L (Fatal diag) -> ask >>= \path -> throwError (SomeDiagnostic path diag)
    L (Context path go) -> local (path:) $ runDiagnosticsC $ hdl (go <$ ctx)
    L (Recover act) -> do
      (fmap (fmap Just)) (runDiagnosticsC $ hdl (act <$ ctx)) `catchError` (\(diag :: SomeDiagnostic) -> tell [diag] *> pure (Nothing <$ ctx))
    R other -> alg (runDiagnosticsC . hdl) (R (R (R other))) ctx

fatal :: (Has Diagnostics sig m, ToDiagnostic diag) => diag -> m a
fatal = send . Fatal

fatalText :: Has Diagnostics sig m => Text -> m a
fatalText = fatal

-- "warning" mechanism -- catches "fatal" failures and turns them into warnings
recover :: Has Diagnostics sig m => m a -> m (Maybe a)
recover = send . Recover

-- | Push context onto the "stack" for "stack traces"
--
-- This is similar to "errors.Wrap" from golang, but it handles the plumbing for you
context :: Has Diagnostics sig m => Text -> m a -> m a
context ctx go = send (Context ctx go)

fromEither :: (ToDiagnostic err, Has Diagnostics sig m) => Either err a -> m a
fromEither = either fatal pure

fromEitherShow :: (Show err, Has Diagnostics sig m) => Either err a -> m a
fromEitherShow = either (fatal . T.pack . show) pure

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

class ToDiagnostic a

instance ToDiagnostic Text

data SomeDiagnostic where
  SomeDiagnostic :: ToDiagnostic a => [Text] -> a -> SomeDiagnostic -- ^ [Text] is the "stack trace"
