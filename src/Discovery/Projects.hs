{-# LANGUAGE UndecidableInstances #-}

module Discovery.Projects
  ( withDiscoveredProjects,
  )
where

import qualified Control.Carrier.Diagnostics as Diag
import Control.Effect.Finally
import Control.Effect.Lift
import Control.Effect.TaskPool
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (for_, traverse_)
import qualified Discovery.Archive as Archive
import Effect.Logger
import Effect.ReadFS (ReadFS)
import Path
import Types (DiscoveredProject)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Effect.Sum (Member (inj))
import qualified Console.Sticky as Sticky
import Control.Carrier.Reader
import qualified Data.Text as T
import Data.List (intersperse)

-- | Run a list of discover functions in parallel, running the provided function
-- on each discovered project. Note that the provided function is also run in
-- parallel.
withDiscoveredProjects ::
  (Has ReadFS sig m, Has (Lift IO) sig m, Has TaskPool sig m, Has Logger sig m, Has Finally sig m) =>
  -- | Discover functions
  [Path Abs Dir -> StickyDiagC (Diag.DiagnosticsC m) [DiscoveredProject run]] ->
  -- | whether to unpack archives
  Bool ->
  Path Abs Dir ->
  (DiscoveredProject run -> m ()) ->
  m ()
withDiscoveredProjects discoverFuncs unpackArchives basedir f = do
  for_ discoverFuncs $ \discover -> forkTask $ do
    projectsResult <- Diag.runDiagnosticsIO . stickyDiag $ discover basedir
    Diag.withResult SevError projectsResult (traverse_ (forkTask . f))

  when unpackArchives $ do
    res <- Diag.runDiagnosticsIO $ Archive.discover (\dir -> lift $ withDiscoveredProjects discoverFuncs unpackArchives dir f) basedir
    Diag.withResult SevError res (const (pure ()))

stickyDiag :: Has (Lift IO) sig m => StickyDiagC m a -> m a
stickyDiag act = Sticky.withStickyRegion $ \region ->
  runReader region . runReader [] . runStickyDiagC $ act

newtype StickyDiagC m a = StickyDiagC { runStickyDiagC :: ReaderC [T.Text] (ReaderC Sticky.StickyRegion m) a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans StickyDiagC where
  lift = StickyDiagC . lift . lift

instance (Algebra sig m, Member Diag.Diagnostics sig, Member (Lift IO) sig, Member Logger sig) => Algebra (Diag.Diagnostics :+: sig) (StickyDiagC m) where
  alg hdl sig ctx = StickyDiagC $ case sig of
    L thing@(Diag.Context txt _) -> local (txt:) $ do
      region <- ask @Sticky.StickyRegion
      context <- ask @[T.Text]
      let chevron = annotate (color Yellow) (pretty (" > " :: String))
      let path = map pretty (reverse context)
      let formatted = hcat (intersperse chevron path)
      Sticky.setSticky' region formatted
      alg (runStickyDiagC . hdl) (inj thing) ctx
    L somethingElse -> alg (runStickyDiagC . hdl) (inj somethingElse) ctx
    R other -> alg (runStickyDiagC . hdl) (R (R other)) ctx
