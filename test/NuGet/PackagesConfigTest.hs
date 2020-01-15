{-# language TemplateHaskell #-}

module NuGet.PackagesConfigTest
  ( spec_analyze
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as BL
import qualified Text.XML.Light as XML
import DepTypes
import GraphUtil
import Strategy.NuGet.PackagesConfig
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

spec_analyze :: Spec
spec_analyze = do
  nuspecFile <- runIO (BL.readFile "test/NuGet/testdata/packages.config")

  describe "packages.config analyzer" $ do
    it "reads a file and constructs an accurate graph" $ do
      case parsePackagesConfig =<< XML.parseXMLDoc nuspecFile of
        Just groups -> do
          let graph = buildGraph groups

          expectDeps [dependencyOne, dependencyTwo] graph
          expectDirect [dependencyOne, dependencyTwo] graph
          expectEdges [] graph

        Nothing -> expectationFailure "could not parse packages.config file"