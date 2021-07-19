module Python.PoetrySpec (
  spec,
) where

import Data.Map qualified as Map
import DepTypes (DepEnvironment (..), DepType (..), Dependency (..), VerConstraint (..))
import Effect.Grapher (addNode, direct, edge, evalGrapher, run)
import Graphing (Graphing)
import Strategy.Python.Poetry (buildGraphWithLock, buildPackageNameGraph, toMap)
import Strategy.Python.Poetry.PoetryLock (
  ObjectVersion (..),
  PackageName (..),
  PoetryLock (..),
  PoetryLockDependencySpec (..),
  PoetryLockPackage (..),
  PoetryLockPackageSource (..),
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
  describe "buildGraphWithLock" $ do
    it "should create graph with lock" $ do
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

  describe "toMap" $ do
    it "should map poetry lock package to dependency" $ do
      toMap
        [ PoetryLockPackage
            { poetryLockPackageName = PackageName "pkgOne"
            , poetryLockPackageVersion = "1.21.0"
            , poetryLockPackageCategory = "main"
            , poetryLockPackageOptional = False
            , poetryLockPackageDependencies =
                Map.fromList
                  [ ("pkgOneChildofOne", TextVersion "*")
                  , ("pkgTwoChildofOne", ObjectVersionSpec $ ObjectVersion "5.4")
                  , ("pkgThreeChildofOne", MultipleObjectVersionSpec [ObjectVersion ">=1.0,<2.0", ObjectVersion ">=1.6,<2.0"])
                  ]
            , poetryLockPackagePythonVersions = ">=3.7"
            , poetryLockPackageSource = Nothing
            }
        ]
        `shouldBe` Map.fromList
          [
            ( PackageName "pkgone"
            , Dependency
                { dependencyType = PipType
                , dependencyName = "pkgOne"
                , dependencyVersion = Just $ CEq "1.21.0"
                , dependencyLocations = []
                , dependencyEnvironments = [EnvProduction]
                , dependencyTags = Map.empty
                }
            )
          ]

    describe "when poetry lock dependency is from git source" $ do
      it "should replace poetry lock package name to git url" $ do
        toMap
          [ PoetryLockPackage
              { poetryLockPackageName = PackageName "pkgWithGitSource"
              , poetryLockPackageVersion = "5.22.0.post0"
              , poetryLockPackageCategory = "main"
              , poetryLockPackageOptional = False
              , poetryLockPackageDependencies = Map.empty
              , poetryLockPackagePythonVersions = "*"
              , poetryLockPackageSource = Just (PoetryLockPackageSource "git" "https://github.com/someUser/pkgWithGitSource.git" (Just "v1.1.1") (Just "598ac"))
              }
          ]
          `shouldBe` Map.fromList
            [
              ( PackageName "pkgwithgitsource"
              , Dependency
                  { dependencyType = GitType
                  , dependencyName = "https://github.com/someUser/pkgWithGitSource.git"
                  , dependencyVersion = Just $ CEq "v1.1.1"
                  , dependencyLocations = []
                  , dependencyEnvironments = [EnvProduction]
                  , dependencyTags = Map.empty
                  }
              )
            ]

    describe "when poetry lock dependency is from url source" $ do
      it "should replace poetry lock package name to url" $ do
        toMap
          [ PoetryLockPackage
              { poetryLockPackageName = PackageName "pkgSourcedFromUrl"
              , poetryLockPackageVersion = "3.92.1"
              , poetryLockPackageCategory = "main"
              , poetryLockPackageOptional = False
              , poetryLockPackageDependencies = Map.empty
              , poetryLockPackagePythonVersions = "*"
              , poetryLockPackageSource = Just (PoetryLockPackageSource "url" "https://some-url.com/some-dir/pkgThree-3.92.1.tar.gz" Nothing Nothing)
              }
          ]
          `shouldBe` Map.fromList
            [
              ( PackageName "pkgsourcedfromurl"
              , Dependency
                  { dependencyType = URLType
                  , dependencyName = "https://some-url.com/some-dir/pkgThree-3.92.1.tar.gz"
                  , dependencyVersion = Just $ CEq "3.92.1"
                  , dependencyLocations = []
                  , dependencyEnvironments = [EnvProduction]
                  , dependencyTags = Map.empty
                  }
              )
            ]

    describe "when poetry lock dependency is from file source" $ do
      it "should replace poetry lock package name to filepath" $ do
        toMap
          [ PoetryLockPackage
              { poetryLockPackageName = PackageName "pkgSourcedFromFile"
              , poetryLockPackageVersion = "1.21.0"
              , poetryLockPackageCategory = "main"
              , poetryLockPackageOptional = False
              , poetryLockPackageDependencies = Map.empty
              , poetryLockPackagePythonVersions = "*"
              , poetryLockPackageSource = Just (PoetryLockPackageSource "file" "pkgTwo-1.21.0.tar.gz" Nothing Nothing)
              }
          ]
          `shouldBe` Map.fromList
            [
              ( PackageName "pkgsourcedfromfile"
              , Dependency
                  { dependencyType = UserType
                  , dependencyName = "pkgTwo-1.21.0.tar.gz"
                  , dependencyVersion = Just $ CEq "1.21.0"
                  , dependencyLocations = []
                  , dependencyEnvironments = [EnvProduction]
                  , dependencyTags = Map.empty
                  }
              )
            ]

    describe "when poetry lock dependency is from secondary sources" $ do
      it "should include url into dependency location" $ do
        toMap
          [ PoetryLockPackage
              { poetryLockPackageName = PackageName "myprivatepkg"
              , poetryLockPackageVersion = "0.0.1"
              , poetryLockPackageCategory = "main"
              , poetryLockPackageOptional = False
              , poetryLockPackageDependencies = Map.empty
              , poetryLockPackagePythonVersions = ">=3.6"
              , poetryLockPackageSource = Just (PoetryLockPackageSource "legacy" "https://gitlab.com/api/v4/projects/packages/pypi/simple" (Just "gitlab") Nothing)
              }
          ]
          `shouldBe` Map.fromList
            [
              ( PackageName "myprivatepkg"
              , Dependency
                  { dependencyType = PipType
                  , dependencyName = "myprivatepkg"
                  , dependencyVersion = Just $ CEq "0.0.1"
                  , dependencyLocations = ["https://gitlab.com/api/v4/projects/packages/pypi/simple"]
                  , dependencyEnvironments = [EnvProduction]
                  , dependencyTags = Map.empty
                  }
              )
            ]
