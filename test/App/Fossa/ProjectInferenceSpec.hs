{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.ProjectInferenceSpec (spec) where

import App.Fossa.ProjectInference (parseGitProjectRevision)
import Control.Carrier.Diagnostics (runDiagnostics)
import Effect.ReadFS (runReadFSIO)
import Path
import Path.IO (getCurrentDir)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "git project inference" $ do
    it "parses branch names with slashes" $ do
      cwd <- getCurrentDir
      let testDir = $(mkRelDir "test/App/Fossa/testdata/dot-git/branch-with-slash")
      x <- runDiagnostics $ runReadFSIO $ parseGitProjectRevision (cwd </> testDir)
      case x of
        Right r -> r `shouldBe` (Just "release/1.2.3", "92251f05255753efca80f8318fa85b3fe110b013")
        Left err -> fail $ show err
