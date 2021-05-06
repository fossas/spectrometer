module UserSpecified.YamlDependenciesSpec
  ( spec
  ) where

import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS
import Data.Yaml

import Control.Algebra
import DepTypes
import Effect.Grapher
import Graphing (Graphing)
import Strategy.UserSpecified.YamlDependencies

import Test.Hspec

expected :: Graphing Dependency
expected = run . evalGrapher $ do
  direct $ Dependency
               { dependencyType = GemType
               , dependencyName = "one"
               , dependencyVersion = Nothing
               , dependencyLocations = []
               , dependencyEnvironments = []
               , dependencyTags = M.empty
               }
  direct $ Dependency
               { dependencyType = RPMType
               , dependencyName = "two"
               , dependencyVersion = Just (CEq "1.0.0")
               , dependencyLocations = []
               , dependencyEnvironments = []
               , dependencyTags = M.empty
               }

spec :: Spec
spec = do
  testFile <- runIO (BS.readFile "test/UserSpecified/testdata/valid-deps.yaml")

  describe "yaml user specified dependencies" $ do
    it "works end to end" $ do
      case decodeEither' testFile of
        Right res -> buildGraph res `shouldBe` expected
        Left err -> expectationFailure $ "failed to parse: " <> show err
