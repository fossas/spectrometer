{-# language TemplateHaskell #-}

module Composer.ComposerLockSpec
  ( spec
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS

import DepTypes
import GraphUtil

import Strategy.Composer

import Test.Hspec

dependencyOne :: Dependency
dependencyOne = Dependency { dependencyType = ComposerType
                        , dependencyName = "one"
                        , dependencyVersion = Just (CEq "1.0.0")
                        , dependencyLocations = []
                        , dependencyEnvironments = [EnvProduction]
                        , dependencyTags = M.empty
                        }

dependencyTwo :: Dependency
dependencyTwo = Dependency { dependencyType = ComposerType
                        , dependencyName = "two"
                        , dependencyVersion = Just (CEq "2.0.0")
                        , dependencyLocations = []
                        , dependencyEnvironments = [EnvProduction]
                        , dependencyTags = M.empty
                        }

dependencyThree :: Dependency
dependencyThree = Dependency { dependencyType = ComposerType
                        , dependencyName = "three"
                        , dependencyVersion = Just (CEq "3.0.0")
                        , dependencyLocations = []
                        , dependencyEnvironments = [EnvProduction]
                        , dependencyTags = M.empty
                        }

dependencyFour :: Dependency
dependencyFour = Dependency { dependencyType = ComposerType
                        , dependencyName = "four"
                        , dependencyVersion = Just (CEq "4.0.0")
                        , dependencyLocations = []
                        , dependencyEnvironments = [EnvProduction]
                        , dependencyTags = M.empty
                        }

dependencyFive :: Dependency
dependencyFive = Dependency { dependencyType = ComposerType
                        , dependencyName = "five"
                        , dependencyVersion = Just (CEq "5.0.0")
                        , dependencyLocations = []
                        , dependencyEnvironments = [EnvDevelopment]
                        , dependencyTags = M.empty
                        }

spec :: Spec
spec = do
  testFile <- runIO (BS.readFile "test/Composer/testdata/composer.lock")
  describe "composer.lock analyzer" $ do
    it "reads a file and constructs an accurate graph" $ do
      case eitherDecodeStrict testFile of
        Right res -> do
              let graph = buildGraph res
              expectDeps [dependencyOne, dependencyTwo, dependencyThree, dependencyFour, dependencyFive] graph
              expectDirect [dependencyOne, dependencyTwo, dependencyThree, dependencyFour, dependencyFive] graph
              expectEdges [ (dependencyOne, dependencyTwo), (dependencyOne, dependencyTwo), (dependencyTwo, dependencyFour)]  graph
        Left err -> expectationFailure $ show err