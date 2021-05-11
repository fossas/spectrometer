module Conda.EnvironmentYmlSpec
  ( spec
  )
where

import Prelude
import DepTypes
import qualified Data.Map.Strict as M
import qualified Test.Hspec as T
import qualified Data.ByteString as BS
import Data.Yaml (decodeEither', prettyPrintParseException)
import GraphUtil (expectDeps)
import Test.Hspec
import Strategy.Conda.EnvironmentYml (buildGraph, EnvironmentYmlFile(..))
import Effect.Grapher
import Graphing (Graphing)

dependencyOne :: Dependency
dependencyOne = Dependency { dependencyType = CondaType 
                            , dependencyName = "name"
                            , dependencyVersion = Just (CEq "version1")
                            , dependencyLocations = []
                            , dependencyEnvironments = []
                            , dependencyTags = M.empty 
                            }
dependencyTwo :: Dependency
dependencyTwo = Dependency { dependencyType = CondaType 
                            , dependencyName = "name"
                            , dependencyVersion = Just (CEq "version2")
                            , dependencyLocations = []
                            , dependencyEnvironments = []
                            , dependencyTags = M.empty 
                            }

dependencyThree :: Dependency
dependencyThree = Dependency { dependencyType = CondaType 
                            , dependencyName = "name"
                            , dependencyVersion = Nothing
                            , dependencyLocations = []
                            , dependencyEnvironments = []
                            , dependencyTags = M.empty 
                            }

expectedGraph :: Graphing Dependency
expectedGraph = run . evalGrapher $ do
  direct $ Dependency { dependencyType = CondaType
                      , dependencyName = "biopython"
                      , dependencyVersion = Just (CEq "1.78")
                      , dependencyLocations = []
                      , dependencyEnvironments = []
                      , dependencyTags = M.empty
                      }
  direct $ Dependency { dependencyType = CondaType
                      , dependencyName = "blas"
                      , dependencyVersion = Just (CEq "1.0")
                      , dependencyLocations = []
                      , dependencyEnvironments = []
                      , dependencyTags = M.empty
                      }
  direct $ Dependency { dependencyType = CondaType
                      , dependencyName = "ca-certificates"
                      , dependencyVersion = Just (CEq "2021.1.19")
                      , dependencyLocations = []
                      , dependencyEnvironments = []
                      , dependencyTags = M.empty
                      } 



envFile :: EnvironmentYmlFile 
envFile = EnvironmentYmlFile "Name" ["name=version1=build", "name=version2", "name"]

spec :: T.Spec
spec = do
  condaEnvFile <- runIO (BS.readFile "test/Conda/testdata/environment.yml")
  T.describe "buildGraph" $
    T.it "can parse EnvironmentYmlFile" $
      expectDeps [dependencyOne, dependencyTwo, dependencyThree] $ buildGraph envFile
  T.describe "buildGraph with real environment.yml" $
    T.it "can parse environment.yml" $ do
      case decodeEither' condaEnvFile of
        Left err -> expectationFailure $ "Failed to parse: " ++ prettyPrintParseException err
        Right deps -> buildGraph deps `shouldBe` expectedGraph
    
