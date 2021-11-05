module Data.FileEmbed.Extra (
  embedFileIfExists,
) where

import Data.FileEmbed (embedFile)
import Language.Haskell.TH (
  Exp (LitE),
  Lit (StringL),
  Q,
  reportWarning,
 )
import Path (parseRelFile)
import Path.IO (doesFileExist)

embedFileIfExists :: FilePath -> Q Exp
embedFileIfExists inputPath = do
  case parseRelFile inputPath of
    Just path -> do
      exists <- doesFileExist path
      if exists
        then embedFile inputPath
        else do
          reportWarning $ "File " <> inputPath <> " not found"
          pure (LitE $ StringL "")
    Nothing -> fail "No filepath provided"
