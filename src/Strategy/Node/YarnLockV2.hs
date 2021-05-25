{-# LANGUAGE TemplateHaskell #-}

-- FIXME: kill TemplateHaskell
-- FIXME: kill Carrier.Diagnostics
module Strategy.Node.YarnLockV2 (
  ) where

import Strategy.Yarn.LockfileV2
import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Effect.Diagnostics
import Effect.ReadFS
import Path

parseYL = readContentsYaml @YarnLockfile

testy :: IO ()
--testy = (putStrLn =<<) . fmap (either (show . renderFailureBundle) show) . runDiagnostics . runReadFSIO . parseYL $ $(mkAbsFile "/Users/connor/Desktop/data-block-extract/yarn.lock")
testy = (putStrLn =<<) . fmap (either (show . renderFailureBundle) show) . runDiagnostics . runReadFSIO . parseYL $ $(mkAbsFile "/Users/connor/Desktop/tmp26/yarn.lock")
