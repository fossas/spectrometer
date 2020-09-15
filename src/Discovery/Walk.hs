{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Discovery.Walk
  ( -- * Walking the filetree
    walk,
    walk',
    WalkStep (..),
    fileName,
  )
where

import Control.Carrier.Output.IO
import Control.Carrier.Writer.Church
import Control.Effect.Lift
import Control.Monad.Trans
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Path
import Path.IO

data WalkStep
  = -- | Continue walking subdirectories
    WalkContinue
  | -- | Skip some subdirectories
    WalkSkipSome [Text]
  | -- | Skip all subdirectories
    WalkSkipAll
  | -- | Stop walking the filetree entirely
    WalkStop
  deriving (Eq, Ord, Show)

-- | Walk the filetree, rooted at an absolute directory. The passed-in function
-- takes the @currentdir@ @subdirs@ and @files@, and produces a WalkStep
-- describing what to do next.
--
-- You can inspect the names of files and directories with 'dirName' and
-- 'fileName'
walk ::
  MonadIO m =>
  (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m WalkStep) ->
  Path Abs Dir ->
  m ()
walk f = walkDir $ \dir subdirs files -> do
  -- normally, subdirs and files are _relative to dir_. We want them to be
  -- relative to the filetree walk
  step <- f dir subdirs files
  case step of
    WalkContinue -> pure $ WalkExclude []
    WalkSkipSome dirs ->
      -- we normalize the passed in [Text] as relative directories for more reliable comparisons
      let parsedDirs = mapMaybe (parseRelDir . T.unpack) dirs
       in pure . WalkExclude . filter (not . (`elem` parsedDirs) . dirname) $ subdirs
    WalkSkipAll -> pure $ WalkExclude subdirs
    WalkStop -> pure WalkFinish

walk' ::
  forall sig m o.
  (Has (Lift IO) sig m, MonadIO m, Monoid o) =>
  (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m (o, WalkStep)) ->
  Path Abs Dir ->
  m o
walk' f base = do
  foo <- runWriter (\w a -> pure (w, a)) $ walk mangled base
  pure (fst foo)
    where
      mangled :: Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> WriterC o m WalkStep
      mangled _ subdirs files = do
        (res, step) <- lift $ f base subdirs files
        tell res
        pure step

fileName :: Path a File -> String
fileName = toFilePath . filename
