module App.Util
  ( validateDir,
  )
where

import App.Types
import Control.Monad (unless)
import qualified Path.IO as P
import System.Exit (die)

-- | Validate that a filepath points to a directory and the directory exists
--
-- TODO: Why is this here? Seems like only `appMain` should use it, and `die`
-- (as opposed to an error effect) seems scary - move back in to App?
-- (Originally factored out in PR#80.)
validateDir :: FilePath -> IO BaseDir
validateDir dir = do
  absolute <- P.resolveDir' dir
  exists <- P.doesDirExist absolute

  unless exists (die $ "ERROR: Directory " <> show absolute <> " does not exist")
  pure $ BaseDir absolute
