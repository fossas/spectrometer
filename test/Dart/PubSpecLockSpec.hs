module Dart.PubSpecLockSpec (
  spec,
) where

import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Data.Yaml (decodeEither')
import DepTypes
import GraphUtil
import Graphing (empty)
import Strategy.Dart.PubSpecLock (
  PackageName (..),
  PubDepSource (..),
  PubLockContent (..),
  PubLockPkgMetadata (..),
  buildGraph,
  toDependency,
 )
import Test.Hspec

expectedLockFile :: PubLockContent
expectedLockFile =
  PubLockContent
    { packages =
        Map.fromList
          [
            ( PackageName "pkg_hosted"
            , PubLockPkgMetadata
                { pubLockPkgIsDirect = False
                , pubLockPkgSource = PubLockPkgHostedSource (Just "pkg_hosted") (Just "https://pub.dartlang.org")
                , pubLockPkgVersion = Just $ CEq "1.1"
                , pubLockPkgEnvironment = []
                }
            )
          ,
            ( PackageName "pkg_git"
            , PubLockPkgMetadata
                { pubLockPkgIsDirect = False
                , pubLockPkgSource = PubLockPkgGitSource "https://github.com/user/pkg" "release-0.9"
                , pubLockPkgVersion = Just $ CEq "1.2"
                , pubLockPkgEnvironment = []
                }
            )
          ,
            ( PackageName "pkg_sdk"
            , PubLockPkgMetadata
                { pubLockPkgIsDirect = False
                , pubLockPkgSource = PubLockPkgSdkSource "flutter"
                , pubLockPkgVersion = Just $ CEq "1.3"
                , pubLockPkgEnvironment = []
                }
            )
          ,
            ( PackageName "pkg_file"
            , PubLockPkgMetadata
                { pubLockPkgIsDirect = False
                , pubLockPkgSource = PubLockPkgPathSource "/Users/dir/pkg_dir"
                , pubLockPkgVersion = Just $ CEq "1.4"
                , pubLockPkgEnvironment = []
                }
            )
          ,
            ( PackageName "pkg_hosted_direct"
            , PubLockPkgMetadata
                { pubLockPkgIsDirect = True
                , pubLockPkgSource = PubLockPkgHostedSource (Just "pkg_hosted_direct") (Just "https://pub.dartlang.org")
                , pubLockPkgVersion = Just $ CEq "1.5"
                , pubLockPkgEnvironment = [EnvProduction]
                }
            )
          ,
            ( PackageName "pkg_hosted_direct_dev"
            , PubLockPkgMetadata
                { pubLockPkgIsDirect = True
                , pubLockPkgSource = PubLockPkgHostedSource (Just "pkg_hosted_direct_dev") (Just "https://pub.dartlang.org")
                , pubLockPkgVersion = Just $ CEq "1.6"
                , pubLockPkgEnvironment = [EnvDevelopment]
                }
            )
          ]
    }

spec :: Spec
spec = do
  lockFile <- runIO (BS.readFile "test/Dart/testdata/pubspec.lock")

  describe "pubspec.lock parsing" $
    it "should parse file correctly" $
      case decodeEither' lockFile of
        Right res -> res `shouldBe` expectedLockFile
        Left err -> expectationFailure $ "failed to parse: " <> show err

  describe "pubLockPkg to dependency conversion" $ do
    it "should create dependency for hosted sources" $ do
      let pkg =
            PubLockPkgMetadata
              { pubLockPkgIsDirect = True
              , pubLockPkgSource = PubLockPkgHostedSource (Just "pkg_a") (Just "https://pub.dartlang.org")
              , pubLockPkgVersion = Just $ CEq "1.1"
              , pubLockPkgEnvironment = [EnvDevelopment]
              }
      let expectedDependency =
            Dependency
              { dependencyType = PubType
              , dependencyName = "pkg_a"
              , dependencyVersion = Just $ CEq "1.1"
              , dependencyLocations = ["https://pub.dartlang.org"]
              , dependencyEnvironments = [EnvDevelopment]
              , dependencyTags = Map.empty
              }
      toDependency (PackageName "pkg_a") pkg `shouldBe` expectedDependency

    it "should create dependency for git sources" $ do
      let pkg =
            PubLockPkgMetadata
              { pubLockPkgIsDirect = True
              , pubLockPkgSource = PubLockPkgGitSource "https://github.com/user/pkg" "release-0.9"
              , pubLockPkgVersion = Just $ CEq "1.1"
              , pubLockPkgEnvironment = []
              }
      let expectedDependency =
            Dependency
              { dependencyType = GitType
              , dependencyName = "https://github.com/user/pkg"
              , dependencyVersion = Just $ CEq "release-0.9"
              , dependencyLocations = []
              , dependencyEnvironments = []
              , dependencyTags = Map.empty
              }
      toDependency (PackageName "pkg_b") pkg `shouldBe` expectedDependency

  describe "graphing from pubspec.lock" $ do
    it "should not create graphing for packages sourced from sdk" $ do
      let sdkSources =
            PubLockContent $
              Map.filterWithKey (\x _ -> (unPackageName x) == "pkg_sdk") (packages expectedLockFile)

      Map.null (packages sdkSources) `shouldBe` False
      buildGraph sdkSources `shouldBe` Graphing.empty

    it "should not create graphing for packages sourced from file" $ do
      let fileSources =
            PubLockContent $
              Map.filterWithKey (\x _ -> (unPackageName x) == "pkg_file") (packages expectedLockFile)

      Map.null (packages fileSources) `shouldBe` False
      buildGraph fileSources `shouldBe` Graphing.empty

    it "should graph direct and deep dependencies" $ do
      let lockContent =
            PubLockContent
              { packages =
                  Map.fromList
                    [
                      ( PackageName "pkg_direct"
                      , PubLockPkgMetadata
                          { pubLockPkgIsDirect = True
                          , pubLockPkgSource = PubLockPkgHostedSource (Just "pkg_direct") (Just "some-url-1")
                          , pubLockPkgVersion = Nothing
                          , pubLockPkgEnvironment = []
                          }
                      )
                    ,
                      ( PackageName "pkg_deep"
                      , PubLockPkgMetadata
                          { pubLockPkgIsDirect = False
                          , pubLockPkgSource = PubLockPkgHostedSource (Just "pkg_deep") (Just "some-url-2")
                          , pubLockPkgVersion = Nothing
                          , pubLockPkgEnvironment = []
                          }
                      )
                    ]
              }
      let graph = buildGraph lockContent

      expectDirect
        [ Dependency
            { dependencyType = PubType
            , dependencyName = "pkg_direct"
            , dependencyVersion = Nothing
            , dependencyLocations = ["some-url-1"]
            , dependencyEnvironments = []
            , dependencyTags = Map.empty
            }
        ]
        graph

      expectDeps
        [ Dependency
            { dependencyType = PubType
            , dependencyName = "pkg_direct"
            , dependencyVersion = Nothing
            , dependencyLocations = ["some-url-1"]
            , dependencyEnvironments = []
            , dependencyTags = Map.empty
            }
        , Dependency
            { dependencyType = PubType
            , dependencyName = "pkg_deep"
            , dependencyVersion = Nothing
            , dependencyLocations = ["some-url-2"]
            , dependencyEnvironments = []
            , dependencyTags = Map.empty
            }
        ]
        graph
