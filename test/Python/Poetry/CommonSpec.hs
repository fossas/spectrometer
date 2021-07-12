module Python.Poetry.CommonSpec (
  spec,
) where

import Data.Map qualified as Map
import Effect.Grapher
import Strategy.Python.Poetry.Common (buildPackageNameGraph)
import Strategy.Python.Poetry.PoetryLock (
  PackageName (..),
  PoetryLockDependencySpec (..),
  PoetryLockPackage (..),
 )
import Test.Hspec

spec :: Spec
spec = do
  describe "buildPackageNameGraph" $ do
    context "when package has no sub dependencies" $ do
      it "should produce pkg name graph with no edges" $ do
        let candidatePoetryPkgs =
              [ PoetryLockPackage
                  { poetryLockPackageName = PackageName{unPackageName = "somePkg"}
                  , poetryLockPackageVersion = "1.21.0"
                  , poetryLockPackageCategory = "main"
                  , poetryLockPackageOptional = False
                  , poetryLockPackageDependencies = Map.empty
                  , poetryLockPackagePythonVersions = "*"
                  , poetryLockPackageSource = Nothing
                  }
              ]
        let expectedPkgNameGraph = run . evalGrapher $ do addNode $ PackageName{unPackageName = "somePkg"}
        buildPackageNameGraph candidatePoetryPkgs `shouldBe` expectedPkgNameGraph

      context "when package has sub dependencies" $ do
        it "should produce pkg name graph with edges" $ do
          let candidatePoetryPkgs =
                [ PoetryLockPackage
                    { poetryLockPackageName = PackageName{unPackageName = "somePkg"}
                    , poetryLockPackageVersion = "1.21.0"
                    , poetryLockPackageCategory = "main"
                    , poetryLockPackageOptional = False
                    , poetryLockPackageDependencies = Map.fromList [("pkgOneChildOne", TextVersion "*")]
                    , poetryLockPackagePythonVersions = "*"
                    , poetryLockPackageSource = Nothing
                    }
                , PoetryLockPackage
                    { poetryLockPackageName = PackageName{unPackageName = "pkgOneChildOne"}
                    , poetryLockPackageVersion = "1.21.0"
                    , poetryLockPackageCategory = "main"
                    , poetryLockPackageOptional = False
                    , poetryLockPackageDependencies = Map.empty
                    , poetryLockPackagePythonVersions = "*"
                    , poetryLockPackageSource = Nothing
                    }
                ]
          let expectedPkgNameGraph = run . evalGrapher $ do edge PackageName{unPackageName = "somePkg"} PackageName{unPackageName = "pkgOneChildOne"}
          buildPackageNameGraph candidatePoetryPkgs `shouldBe` expectedPkgNameGraph
