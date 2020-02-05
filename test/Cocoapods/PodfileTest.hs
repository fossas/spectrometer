module Cocoapods.PodfileTest
  where

import Prologue

import qualified Data.Map.Strict as M
import           Polysemy
import           Polysemy.Input
import qualified Data.Text.IO as TIO
import           Text.Megaparsec

import DepTypes
import Strategy.Cocoapods.Podfile
import GraphUtil

import qualified Test.Tasty.Hspec as T

dependencyOne :: Dependency
dependencyOne = Dependency { dependencyType = PodType
                           , dependencyName = "one"
                           , dependencyVersion = Just (CEq "1.0.0")
                           , dependencyLocations = ["test.repo"]
                           , dependencyTags = M.empty 
                           }

dependencyTwo :: Dependency
dependencyTwo = Dependency { dependencyType = PodType
                           , dependencyName = "two"
                           , dependencyVersion = Just (CEq "2.0.0")
                           , dependencyLocations = ["custom.repo"]
                           , dependencyTags = M.empty 
                           }

dependencyThree :: Dependency
dependencyThree = Dependency { dependencyType = PodType
                             , dependencyName = "three"
                             , dependencyVersion = Just (CEq "3.0.0")
                             , dependencyLocations = ["test.repo"]
                             , dependencyTags = M.empty 
                             }

dependencyFour :: Dependency
dependencyFour = Dependency { dependencyType = PodType
                             , dependencyName = "four"
                             , dependencyVersion = Nothing
                             , dependencyLocations = ["test.repo"]
                             , dependencyTags = M.empty 
                             }

testPodfile :: Podfile
testPodfile = Podfile testPods "test.repo"

testPods :: [Pod]
testPods = [Pod "one" (Just "1.0.0") Nothing, Pod "two" (Just "2.0.0") (Just $ Source "custom.repo"), Pod "three" (Just "3.0.0") (Just $ Path "internal/path"), Pod "four" Nothing (Just $ Git "fossa/spectrometer" (Just "12345"))]

spec_analyze :: T.Spec
spec_analyze = do
  T.describe "podfile analyzer" $
    T.it "produces the expected output" $ do
      let graph = analyze
            & runInputConst @Podfile testPodfile
            & run
      expectDeps [dependencyOne, dependencyTwo, dependencyThree, dependencyFour] graph
      expectDirect [dependencyOne, dependencyTwo, dependencyThree, dependencyFour] graph
      expectEdges [] graph
      
  podLockFile <- T.runIO (TIO.readFile "test/Cocoapods/testdata/Podfile")
  T.describe "podfile parser" $ do
    T.it "correctly parses a file" $ do
      case runParser parsePodfile "" podLockFile of
        Left _ -> T.expectationFailure "failed to parse"
        Right result -> do
            pods result `T.shouldMatchList` testPods
            source result `T.shouldBe` "test.repo"