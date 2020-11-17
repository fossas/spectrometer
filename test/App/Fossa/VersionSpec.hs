module App.Fossa.VersionSpec
  ( spec,
  )
where

import App.Version
import Test.Hspec
import Data.Maybe (isJust)

spec :: Spec
spec = describe "Version" $ do
  it "is not dirty" $
    isDirty `shouldBe` False

  it "should have a version" $
    isJust versionNumber `shouldBe` True