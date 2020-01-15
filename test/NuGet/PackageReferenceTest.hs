{-# language TemplateHaskell #-}

module NuGet.PackageReferenceTest
  ( spec_analyze
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as BL
import qualified Text.XML.Light as XML
import DepTypes
import GraphUtil
import Strategy.NuGet.PackageReference
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

dependencyFour :: Dependency
dependencyFour = Dependency { dependencyType = NuGetType
                            , dependencyName = "four"
                            , dependencyVersion = Nothing
                            , dependencyLocations = []
                            , dependencyTags = M.empty
                            }

spec_analyze :: Spec
spec_analyze = do
  refFile <- runIO (BL.readFile "test/NuGet/testdata/test.csproj")

  describe "Package Reference analyzer" $ do
    it "reads a file and constructs an accurate graph" $ do
      case parsePackageReference =<< XML.parseXMLDoc refFile of
        Just groups -> do
          let graph = buildGraph groups

          expectDeps [dependencyOne, dependencyTwo, dependencyThree, dependencyFour] graph
          expectDirect [dependencyOne, dependencyTwo, dependencyThree, dependencyFour] graph
          expectEdges [] graph

        Nothing -> expectationFailure "could not parse package reference file"