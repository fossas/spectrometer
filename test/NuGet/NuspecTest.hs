{-# language TemplateHaskell #-}

module NuGet.NuspecTest
  ( spec_analyze
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as BL
import qualified Text.XML.Light as XML
import DepTypes
import GraphUtil
import Strategy.NuGet.Nuspec
import Test.Tasty.Hspec

dependencyOne :: Dependency
dependencyOne = Dependency { dependencyType = NuGetType
                        , dependencyName = "one"
                        , dependencyVersion = Just (CEq "1.0.0")
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        }

dependencyTwo :: Dependency
dependencyTwo = Dependency { dependencyType = NuGetType
                        , dependencyName = "two"
                        , dependencyVersion = Just (CEq "2.0.0")
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        }

dependencyThree :: Dependency
dependencyThree = Dependency { dependencyType = NuGetType
                        , dependencyName = "three"
                        , dependencyVersion = Just (CEq "3.0.0")
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        }

spec_analyze :: Spec
spec_analyze = do
  nuspecFile <- runIO (BL.readFile "test/NuGet/testdata/test.nuspec")

  describe "nuspec analyzer" $ do
    it "reads a file and constructs an accurate graph" $ do
      case parseNuspec =<< XML.parseXMLDoc nuspecFile of
        Just groups -> do
          let graph = buildGraph groups

          expectDeps [dependencyOne, dependencyTwo, dependencyThree] graph
          expectDirect [dependencyOne, dependencyTwo, dependencyThree] graph
          expectEdges [] graph

        Nothing -> expectationFailure "could not parse nuspec file"