module Extra.TextSpec
  ( spec,
  )
where

import Data.Text.Extra
import Test.Hspec qualified as Test
import Prelude

spec :: Test.Spec
spec = do
  Test.describe "Text splitOnceOn" $
    Test.it "should split a string once from the start" $
      splitOnceOn "-" "1-2-3" `Test.shouldBe` ("1", "2-3")

  Test.describe "Text splitOnceonEnd" $
    Test.it "should split a string once from the end" $
      splitOnceOnEnd "-" "1-2-3" `Test.shouldBe` ("1-2", "3")
