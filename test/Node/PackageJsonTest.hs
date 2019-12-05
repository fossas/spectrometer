module Node.PackageJsonTest
  ( spec_packageJsonBuildGraph
  ) where

import Prologue

import qualified Data.Map.Strict as M

import qualified Graph as G
import           Strategy.Node.PackageJson

import GraphUtil
import Test.Tasty.Hspec

mockInput :: PackageJson
mockInput = PackageJson
  { packageDeps = M.fromList [("packageOne", "^1.0.0")]
  , packageDevDeps = M.fromList [("packageTwo", "^2.0.0")]
  }

packageOne :: G.Dependency
packageOne = G.Dependency
  { dependencyType = G.NodeJSType
  , dependencyName = "packageOne"
  , dependencyVersion = Just (G.CCompatible "^1.0.0")
  , dependencyLocations = []
  , dependencyTags = M.fromList [("environment", ["production"])]
  }

packageTwo :: G.Dependency
packageTwo = G.Dependency
  { dependencyType = G.NodeJSType
  , dependencyName = "packageTwo"
  , dependencyVersion = Just (G.CCompatible "^2.0.0")
  , dependencyLocations = []
  , dependencyTags = M.fromList [("environment", ["development"])]
  }

spec_packageJsonBuildGraph :: Spec
spec_packageJsonBuildGraph = do
  describe "buildGraph" $ do
    it "should produce expected output" $ do
      let graph = buildGraph mockInput
      expectDeps [packageOne, packageTwo] graph
      expectDirect [packageOne, packageTwo] graph
      expectEdges [] graph
