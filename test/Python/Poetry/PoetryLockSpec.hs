module Python.Poetry.PoetryLockSpec (
  spec,
) where

import Data.Map qualified as Map
import Data.Text.IO qualified as TIO
import DepTypes
import Strategy.Python.Poetry.PoetryLock (
  ObjectVersion (..),
  PackageName (..),
  PoetryLock (..),
  PoetryLockDependencySpec (..),
  PoetryLockPackage (..),
  PoetryLockPackageSource (..),
  PoetryMetadata (..),
  poetryLockCodec,
  toMap,
 )
import Test.Hspec
import Toml qualified

expectedPoetryLock :: PoetryLock
expectedPoetryLock =
  PoetryLock
    { poetryLockMetadata =
        PoetryMetadata
          { poetryMetadataLockVersion = "1.1"
          , poetryMetadataContentHash = "cf14fd7e0a1a1c6c5a1ee9afe16d0abaaac531ab9d84ad3d1d5276634aa35687"
          , poetryMetadataPythonVersions = "^3.8"
          }
    , poetryLockPackages =
        [ PoetryLockPackage
            { poetryLockPackageName = PackageName{unPackageName = "pkgWithGitSource"}
            , poetryLockPackageVersion = "5.22.0.post0"
            , poetryLockPackageCategory = "main"
            , poetryLockPackageOptional = False
            , poetryLockPackageDependencies = Map.empty
            , poetryLockPackagePythonVersions = "*"
            , poetryLockPackageSource = Just PoetryLockPackageSource{poetryLockPackageSourceType = "git", poetryLockPackageSourceUrl = "https://github.com/someUser/pkgWithGitSource.git", poetryLockPackageSourceReference = Just "v1.1.1", poetryLockPackageSourceResolvedReference = Just "598ac"}
            }
        , PoetryLockPackage
            { poetryLockPackageName = PackageName{unPackageName = "pkgSourcedFromFile"}
            , poetryLockPackageVersion = "1.21.0"
            , poetryLockPackageCategory = "main"
            , poetryLockPackageOptional = False
            , poetryLockPackageDependencies = Map.empty
            , poetryLockPackagePythonVersions = "*"
            , poetryLockPackageSource = Just PoetryLockPackageSource{poetryLockPackageSourceType = "file", poetryLockPackageSourceUrl = "pkgTwo-1.21.0.tar.gz", poetryLockPackageSourceResolvedReference = Nothing, poetryLockPackageSourceReference = Nothing}
            }
        , PoetryLockPackage
            { poetryLockPackageName = PackageName{unPackageName = "pkgSourcedFromUrl"}
            , poetryLockPackageVersion = "3.92.1"
            , poetryLockPackageCategory = "main"
            , poetryLockPackageOptional = False
            , poetryLockPackageDependencies = Map.empty
            , poetryLockPackagePythonVersions = "*"
            , poetryLockPackageSource = Just PoetryLockPackageSource{poetryLockPackageSourceType = "url", poetryLockPackageSourceUrl = "https://some-url.com/some-dir/pkgThree-3.92.1.tar.gz", poetryLockPackageSourceResolvedReference = Nothing, poetryLockPackageSourceReference = Nothing}
            }
        , PoetryLockPackage
            { poetryLockPackageName = PackageName{unPackageName = "pkgOne"}
            , poetryLockPackageVersion = "1.21.0"
            , poetryLockPackageCategory = "main"
            , poetryLockPackageOptional = False
            , poetryLockPackageDependencies =
                Map.fromList
                  [ ("pkgOneChildofOne", TextVersion "*")
                  , ("pkgTwoChildofOne", ObjectVersionSpec ObjectVersion{unObjectVersion = "5.4"})
                  , ("pkgThreeChildofOne", MultipleObjectVersionSpec [ObjectVersion{unObjectVersion = ">=1.0,<2.0"}, ObjectVersion{unObjectVersion = ">=1.6,<2.0"}])
                  ]
            , poetryLockPackagePythonVersions = ">=3.7"
            , poetryLockPackageSource = Nothing
            }
        , PoetryLockPackage
            { poetryLockPackageName = PackageName{unPackageName = "pkgOneChildofOne"}
            , poetryLockPackageVersion = "11.4"
            , poetryLockPackageCategory = "main"
            , poetryLockPackageOptional = False
            , poetryLockPackageDependencies = Map.empty
            , poetryLockPackagePythonVersions = "*"
            , poetryLockPackageSource = Nothing
            }
        , PoetryLockPackage
            { poetryLockPackageName = PackageName{unPackageName = "pkgTwoChildofOne"}
            , poetryLockPackageVersion = "5.4"
            , poetryLockPackageCategory = "main"
            , poetryLockPackageOptional = False
            , poetryLockPackageDependencies = Map.empty
            , poetryLockPackagePythonVersions = "*"
            , poetryLockPackageSource = Nothing
            }
        , PoetryLockPackage
            { poetryLockPackageName = PackageName{unPackageName = "pkgThreeChildofOne"}
            , poetryLockPackageVersion = "1.6.1"
            , poetryLockPackageCategory = "main"
            , poetryLockPackageOptional = False
            , poetryLockPackageDependencies = Map.empty
            , poetryLockPackagePythonVersions = "*"
            , poetryLockPackageSource = Nothing
            }
        ]
    }

spec :: Spec
spec = do
  contents <- runIO (TIO.readFile "test/Python/Poetry/testdata/poetry.lock")
  describe "poetryLockCodec" $
    it "should produce expected output" $ do
      case Toml.decode poetryLockCodec contents of
        Left err -> expectationFailure ("decode failed: " <> show err)
        Right pkg -> do
          pkg `shouldBe` expectedPoetryLock

  describe "toMap" $ do
    it "should map poetry lock package to dependency" $ do
      toMap [poetryLockPackages expectedPoetryLock !! 3]
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

    context "when poetry lock dependency is from git source" $ do
      it "should replace poetry lock package name to git url" $ do
        toMap [head (poetryLockPackages expectedPoetryLock)]
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

    context "when poetry lock dependency is from url source" $ do
      it "should replace poetry lock package name to url" $ do
        toMap [poetryLockPackages expectedPoetryLock !! 2]
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

    context "when poetry lock dependency is from file source" $ do
      it "should replace poetry lock package name to filepath" $ do
        toMap [poetryLockPackages expectedPoetryLock !! 1]
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