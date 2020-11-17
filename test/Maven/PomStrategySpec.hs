module Maven.PomStrategySpec
  ( spec,
  )
where

import qualified Data.Map.Strict as M
import Strategy.Maven.Pom (interpolateProperties)
import Strategy.Maven.Pom.PomFile
import Test.Hspec

spec :: Spec
spec = do
  describe "interpolateProperties" $ do
    let pom = Pom (MavenCoordinate "MYGROUP" "MYARTIFACT" "MYVERSION") Nothing M.empty M.empty M.empty []
    it "should work for built-in properties" $ do
      interpolateProperties pom "${project.groupId}" `shouldBe` "MYGROUP"
      interpolateProperties pom "${project.artifactId}" `shouldBe` "MYARTIFACT"
      interpolateProperties pom "${project.version}" `shouldBe` "MYVERSION"

    it "should prefer user-specified properties over computed ones" $ do
      let pom' = pom { pomProperties = M.singleton "project.groupId" "OTHERGROUP" }
      interpolateProperties pom' "${project.groupId}" `shouldBe` "OTHERGROUP"
      
      
