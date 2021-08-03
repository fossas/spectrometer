module Path.Extra (
  toRelativePath,
) where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Path

-- toRelativePath returns the path of a file (Path Abs File) relative to a directory (Path Abs Dir).
-- If the file is not within the directory, then the full file path will be returned
toRelativePath :: Path Abs Dir -> Path Abs File -> FilePath
toRelativePath baseDir file =
  fromMaybe (fullPath) $ stripPrefix (fullDir) (fullPath)
  where
    fullPath = fromAbsFile file
    fullDir = fromAbsDir baseDir
