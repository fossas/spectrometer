{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.Diagnostics.StickyContext (
  StickyDiagC (..),
  stickyDiag,
) where

import Console.Sticky qualified as Sticky
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.Reader
import Control.Effect.Lift
import Control.Effect.Sum (Member (inj))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.List (intersperse)
import Data.Text qualified as T
import Effect.Logger
import Control.Effect.AtomicCounter

stickyDiag :: (Has AtomicCounter sig m, Has (Lift IO) sig m) => StickyDiagC m a -> m a
stickyDiag act = do
  taskId <- generateId
  Sticky.withStickyRegion $ \region ->
    runReader region . runReader [] . runReader (TaskId taskId) . runStickyDiagC $ act

newtype TaskId = TaskId Int

newtype StickyDiagC m a = StickyDiagC {runStickyDiagC :: ReaderC TaskId (ReaderC [T.Text] (ReaderC Sticky.StickyRegion m)) a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans StickyDiagC where
  lift = StickyDiagC . lift . lift . lift

instance (Algebra sig m, Member Diag.Diagnostics sig, Member (Lift IO) sig, Member Logger sig) => Algebra (Diag.Diagnostics :+: sig) (StickyDiagC m) where
  alg hdl sig ctx = StickyDiagC $ case sig of
    L thing@(Diag.Context txt _) -> local (txt :) $ do
      TaskId taskId <- ask @TaskId
      region <- ask @Sticky.StickyRegion
      context <- ask @[T.Text]
      let chevron = annotate (color Yellow) (pretty (" > " :: String))
      let path = map pretty (reverse context)
      let formatted = "[" <> annotate (color Green) ("TASK " <> pretty taskId) <> "] " <> hcat (intersperse chevron path)
      Sticky.setSticky' region formatted
      alg (runStickyDiagC . hdl) (inj thing) ctx
    L somethingElse -> alg (runStickyDiagC . hdl) (inj somethingElse) ctx
    R other -> alg (runStickyDiagC . hdl) (R (R (R other))) ctx
