module App.Fossa.VersionSpec
  ( spec,
  )
where

import App.Version
import Data.Maybe (isJust)
import Test.Hspec

spec :: Spec
spec = describe "Version" $ do
  it "is not dirty" $
    isDirty `shouldBe` False
