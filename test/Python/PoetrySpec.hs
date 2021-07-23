module Python.PoetrySpec (
  spec,
) where

import Data.Map qualified as Map
import DepTypes (DepEnvironment (..), DepType (..), Dependency (..), VerConstraint (..))
import Effect.Grapher (addNode, direct, edge, evalGrapher, run)
import Graphing (Graphing)
import Strategy.Python.Poetry (buildGraphWithLock, buildPackageNameGraph)
import Strategy.Python.Poetry.PoetryLock (
  PackageName (..),
  PoetryLock (..),
  PoetryLockDependencySpec (..),
  PoetryLockPackage (..),
  PoetryMetadata (..),
 )

import Strategy.Python.Poetry.PyProject (PoetryDependency (..), PyProject (..), PyProjectBuildSystem (..), PyProjectPoetry (..))
import Test.Hspec

candidatePyProject :: PyProject
candidatePyProject =
  PyProject
    (Just $ PyProjectBuildSystem "poetry.core.masonry.api")
    (Just $ PyProjectPoetry Nothing Nothing Nothing (Map.fromList ([("flow_pipes", PoetryTextVersion "^1.21")])) Map.empty)

candidatePoetryLock :: PoetryLock
candidatePoetryLock =
  PoetryLock
    [ PoetryLockPackage
        { poetryLockPackageName = PackageName "flow_pipes"
        , poetryLockPackageVersion = "1.21.0"
        , poetryLockPackageCategory = "main"
        , poetryLockPackageOptional = False
        , poetryLockPackageDependencies = Map.fromList [("flow_pipes_gravity", TextVersion "^1.1")]
        , poetryLockPackagePythonVersions = "*"
        , poetryLockPackageSource = Nothing
        }
    , PoetryLockPackage
        { poetryLockPackageName = PackageName "flow_pipes_gravity"
        , poetryLockPackageVersion = "1.1.1"
        , poetryLockPackageCategory = "main"
        , poetryLockPackageOptional = False
        , poetryLockPackageDependencies = Map.empty
        , poetryLockPackagePythonVersions = "*"
        , poetryLockPackageSource = Nothing
        }
    ]
    $ PoetryMetadata "some-version" "some-hash" "some-poetry-version"

expectedGraph :: Graphing Dependency
expectedGraph = run . evalGrapher $ do
  edge (Dependency PipType "flow_pipes" (Just $ CEq "1.21.0") [] [EnvProduction] Map.empty) (Dependency PipType "flow_pipes_gravity" (Just $ CEq "1.1.1") [] [EnvProduction] Map.empty)
  direct (Dependency PipType "flow_pipes" (Just $ CEq "1.21.0") [] [EnvProduction] Map.empty)

spec :: Spec
spec = do
  describe "buildGraphWithLock" $
    it "should create graph with lock" $
      buildGraphWithLock candidatePoetryLock candidatePyProject `shouldBe` expectedGraph

  describe "buildPackageNameGraph" $ do
    describe "when package has no transitive dependencies" $ do
      it "should produce pkg name graph with no edges" $ do
        let candidatePoetryPkgs =
              [ PoetryLockPackage
                  { poetryLockPackageName = PackageName "somePkg"
                  , poetryLockPackageVersion = "1.21.0"
                  , poetryLockPackageCategory = "main"
                  , poetryLockPackageOptional = False
                  , poetryLockPackageDependencies = Map.empty
                  , poetryLockPackagePythonVersions = "*"
                  , poetryLockPackageSource = Nothing
                  }
              ]
        let expectedPkgNameGraph = run . evalGrapher $ addNode $ PackageName "somePkg"
        buildPackageNameGraph candidatePoetryPkgs `shouldBe` expectedPkgNameGraph

      describe "when package has deep dependencies" $ do
        it "should produce package name graph with edges" $ do
          let candidatePoetryPkgs =
                [ PoetryLockPackage
                    { poetryLockPackageName = PackageName "somePkg"
                    , poetryLockPackageVersion = "1.21.0"
                    , poetryLockPackageCategory = "main"
                    , poetryLockPackageOptional = False
                    , poetryLockPackageDependencies = Map.fromList [("pkgOneChildOne", TextVersion "*")]
                    , poetryLockPackagePythonVersions = "*"
                    , poetryLockPackageSource = Nothing
                    }
                , PoetryLockPackage
                    { poetryLockPackageName = PackageName "pkgOneChildOne"
                    , poetryLockPackageVersion = "1.21.0"
                    , poetryLockPackageCategory = "main"
                    , poetryLockPackageOptional = False
                    , poetryLockPackageDependencies = Map.empty
                    , poetryLockPackagePythonVersions = "*"
                    , poetryLockPackageSource = Nothing
                    }
                ]
          let expectedPkgNameGraph = run . evalGrapher $ edge (PackageName "somePkg") (PackageName "pkgOneChildOne")
          buildPackageNameGraph candidatePoetryPkgs `shouldBe` expectedPkgNameGraph
