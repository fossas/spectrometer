module Node.NpmLockTest
  ( spec_npmLockBuildGraph
  ) where

import Prologue

import qualified Data.Map.Strict as M

import qualified Graph as G
import           Strategy.Node.NpmLock

import GraphUtil
import Test.Tasty.Hspec

mockInput :: NpmPackageJson
mockInput = NpmPackageJson
  { packageName = "example"
  , packageVersion = "1.0.0"
  , packageDependencies = M.fromList
    [ ("packageOne", NpmDep
        { depVersion = "1.0.0"
        , depDev = Nothing
        , depResolved = Just "https://example.com/one.tgz"
        , depRequires = Just (M.fromList [("packageTwo", "2.0.0")])
        , depDependencies = Just (M.fromList
            [ ("packageTwo", NpmDep
                { depVersion = "2.0.0"
                , depDev = Just True
                , depResolved = Just "https://example.com/two.tgz"
                , depRequires = Just (M.fromList [("packageThree", "3.0.0")])
                , depDependencies = Nothing
                })
            ])
        })
    , ("packageThree", NpmDep
        { depVersion = "3.0.0"
        , depDev = Just True
        , depResolved = Nothing
        , depRequires = Just (M.fromList [("packageOne", "1.0.0")])
        , depDependencies = Nothing
        })
    ]
  }

packageOne :: G.Dependency
packageOne = G.Dependency
  { dependencyType = G.NodeJSType
  , dependencyName = "packageOne"
  , dependencyVersion = Just (G.CEq "1.0.0")
  , dependencyLocations = ["https://example.com/one.tgz"]
  , dependencyTags = M.fromList [("environment", ["production"])]
  }

packageTwo :: G.Dependency
packageTwo = G.Dependency
  { dependencyType = G.NodeJSType
  , dependencyName = "packageTwo"
  , dependencyVersion = Just (G.CEq "2.0.0")
  , dependencyLocations = ["https://example.com/two.tgz"]
  , dependencyTags = M.fromList [("environment", ["development"])]
  }

packageThree :: G.Dependency
packageThree = G.Dependency
  { dependencyType = G.NodeJSType
  , dependencyName = "packageThree"
  , dependencyVersion = Just (G.CEq "3.0.0")
  , dependencyLocations = []
  , dependencyTags = M.fromList [("environment", ["development"])]
  }

spec_npmLockBuildGraph :: Spec
spec_npmLockBuildGraph = do
  describe "buildGraph" $ do
    it "should produce expected output" $ do
      let graph = buildGraph mockInput
      expectDeps [packageOne, packageTwo, packageThree] graph
      expectDirect [packageOne, packageThree] graph
      expectEdges [ (packageOne, packageTwo)
                  , (packageTwo, packageThree)
                  , (packageThree, packageOne)
                  ] graph
