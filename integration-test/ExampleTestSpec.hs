module ExampleTestSpec (
  spec,
) where

import Test.Hspec (Expectation, Spec, describe, shouldBe, it)
import Text.URI (mkURI)

spec :: Spec
spec = do
  describe "sample test" $
    it "should perform example test" $
        1 `shouldBe` 1