module Analysis.ExampleSpec (spec) where

import Test.Hspec (Expectation, Spec, describe, shouldBe, it)
import Strategy.Python.Setuptools qualified as Setuptools

spec :: Spec
spec = do
  
  -- 1. Testing Common Workflow
  describe "sample test" $
    it "should perform example test" $

        let result =
            Setuptools.discover testdir
              & runConstExec outputTrivial
              & runDiagnostics
              & run
        
        1 `shouldBe` 1

  -- 2. Asserting Warning

-- spec :: Spec
-- spec = do
--     describe "discover" $
--         it "should discover python project, when using requirements.txt" $
