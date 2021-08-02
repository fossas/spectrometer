module Dart.PubDepsSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.IO qualified as TIO
import DepTypes
import GraphUtil (expectDeps, expectDirect, expectEdges)
import Strategy.Dart.PubDeps (PubDepPackage (..), buildGraph, depsCmdOutputParser)
import Strategy.Dart.PubSpecLock (
  PackageName (..),
  PubDepSource (..),
  PubLockContent (..),
  PubLockPackageMetadata (..),
 )
import Test.Hspec
import Text.Megaparsec

expectedDepsCmdOutput :: [PubDepPackage]
expectedDepsCmdOutput =
  [ PubDepPackage (PackageName "pkg_a") (Just $ CEq "5.0.0") (Just $ Set.fromList [PackageName "pkg_aa"]) True
  , PubDepPackage (PackageName "pkg_b") (Just $ CEq "4.0.0") Nothing True
  , PubDepPackage (PackageName "pkg_c") (Just $ CEq "3.0.0") (Just $ Set.fromList [PackageName "pkg_ca", PackageName "pkg_cb"]) True
  , PubDepPackage (PackageName "pkg_d") (Just $ CEq "3.0.1") Nothing True
  , PubDepPackage (PackageName "pkg_e") (Just $ CEq "2.0.0") (Just $ Set.fromList [PackageName "pkg_ea"]) True
  , PubDepPackage (PackageName "pkg_aa") (Just $ CEq "1.0.0") (Just $ Set.fromList [PackageName "pkg_ca"]) False
  , PubDepPackage (PackageName "pkg_ca") (Just $ CEq "1.2.0") Nothing False
  , PubDepPackage (PackageName "pkg_cb") (Just $ CEq "1.3.0") Nothing False
  , PubDepPackage (PackageName "pkg_ea") (Just $ CEq "1.4.0") Nothing False
  ]

spec :: Spec
spec = do
  describe "pub deps -s compact" $ do
    contents <- runIO (TIO.readFile "test/Dart/testdata/pubdeps.compact")

    it "should parse content correctly" $ do
      case runParser depsCmdOutputParser "" contents of
        Left failCode -> expectationFailure $ show failCode
        Right result -> result `shouldBe` expectedDepsCmdOutput

  describe "buildGraph" $ do
    it "should build graph with edges" $ do
      let pubDepsContent =
            [ PubDepPackage (PackageName "pkg_a") (Just $ CEq "1.8.0") (Just $ Set.fromList [PackageName "pkg_deep"]) True
            , PubDepPackage (PackageName "pkg_deep") (Just $ CEq "1.10.0") Nothing False
            ]

      let lockContent =
            PubLockContent
              { packages =
                  Map.fromList
                    [
                      ( PackageName "pkg_a"
                      , PubLockPackageMetadata
                          { pubLockPackageIsDirect = True
                          , pubLockPackageSource = PubLockPackageHostedSource (Just "pkg_a") (Just "https://pub.dartlang.org")
                          , pubLockPackageVersion = Just $ CEq "1.8.0"
                          , pubLockPackageEnvironment = []
                          }
                      )
                    ,
                      ( PackageName "pkg_deep"
                      , PubLockPackageMetadata
                          { pubLockPackageIsDirect = False
                          , pubLockPackageSource = PubLockPackageHostedSource (Just "pkg_deep") (Just "https://pub.dartlang.org")
                          , pubLockPackageVersion = Just $ CEq "1.10.0"
                          , pubLockPackageEnvironment = []
                          }
                      )
                    ]
              }
      let graph = buildGraph lockContent pubDepsContent

      expectDirect
        [ Dependency
            { dependencyType = PubType
            , dependencyName = "pkg_a"
            , dependencyVersion = Just $ CEq "1.8.0"
            , dependencyLocations = ["https://pub.dartlang.org"]
            , dependencyEnvironments = []
            , dependencyTags = Map.empty
            }
        ]
        graph

      expectDeps
        [ Dependency
            { dependencyType = PubType
            , dependencyName = "pkg_a"
            , dependencyVersion = Just $ CEq "1.8.0"
            , dependencyLocations = ["https://pub.dartlang.org"]
            , dependencyEnvironments = []
            , dependencyTags = Map.empty
            }
        , Dependency
            { dependencyType = PubType
            , dependencyName = "pkg_deep"
            , dependencyVersion = Just $ CEq "1.10.0"
            , dependencyLocations = ["https://pub.dartlang.org"]
            , dependencyEnvironments = []
            , dependencyTags = Map.empty
            }
        ]
        graph

      expectEdges
        [
          ( Dependency
              { dependencyType = PubType
              , dependencyName = "pkg_a"
              , dependencyVersion = Just $ CEq "1.8.0"
              , dependencyLocations = ["https://pub.dartlang.org"]
              , dependencyEnvironments = []
              , dependencyTags = Map.empty
              }
          , Dependency
              { dependencyType = PubType
              , dependencyName = "pkg_deep"
              , dependencyVersion = Just $ CEq "1.10.0"
              , dependencyLocations = ["https://pub.dartlang.org"]
              , dependencyEnvironments = []
              , dependencyTags = Map.empty
              }
          )
        ]
        graph

    it "should ignore path dependency" $ do
      let pubDepsContent = [PubDepPackage (PackageName "pkg_path") (Just $ CEq "1.10.0") Nothing True]
      let lockContent =
            PubLockContent
              { packages =
                  Map.fromList
                    [
                      ( PackageName "pkg_path"
                      , PubLockPackageMetadata
                          { pubLockPackageIsDirect = True
                          , pubLockPackageSource = PubLockPackagePathSource "./../dir"
                          , pubLockPackageVersion = Just $ CEq "1.8.0"
                          , pubLockPackageEnvironment = []
                          }
                      )
                    ]
              }

      let graph = buildGraph lockContent pubDepsContent
      expectDirect [] graph
      expectDeps [] graph
      expectEdges [] graph

    it "should ignore sdk dependency" $ do
      let pubDepsContent = [PubDepPackage (PackageName "pkg_sdk") (Just $ CEq "1.10.0") Nothing True]

      let lockContent =
            PubLockContent
              { packages =
                  Map.fromList
                    [
                      ( PackageName "pkg_sdk"
                      , PubLockPackageMetadata
                          { pubLockPackageIsDirect = True
                          , pubLockPackageSource = PubLockPackageSdkSource "pkg_sdk_source"
                          , pubLockPackageVersion = Just $ CEq "1.8.0"
                          , pubLockPackageEnvironment = []
                          }
                      )
                    ]
              }

      let graph = buildGraph lockContent pubDepsContent
      expectDirect [] graph
      expectDeps [] graph
      expectEdges [] graph

    it "should build graph when dependencies have no edges" $ do
      let pubDepsContent = [PubDepPackage (PackageName "pkg_a") (Just $ CEq "1.10.0") Nothing True]

      let lockContent =
            PubLockContent
              { packages =
                  Map.fromList
                    [
                      ( PackageName "pkg_a"
                      , PubLockPackageMetadata
                          { pubLockPackageIsDirect = True
                          , pubLockPackageSource = PubLockPackageHostedSource Nothing Nothing
                          , pubLockPackageVersion = Just $ CEq "1.8.0"
                          , pubLockPackageEnvironment = []
                          }
                      )
                    ]
              }

      let graph = buildGraph lockContent pubDepsContent
      expectDirect
        [ Dependency
            { dependencyType = PubType
            , dependencyName = "pkg_a"
            , dependencyVersion = Just $ CEq "1.8.0"
            , dependencyLocations = []
            , dependencyEnvironments = []
            , dependencyTags = Map.empty
            }
        ]
        graph
      expectDeps
        [ Dependency
            { dependencyType = PubType
            , dependencyName = "pkg_a"
            , dependencyVersion = Just $ CEq "1.8.0"
            , dependencyLocations = []
            , dependencyEnvironments = []
            , dependencyTags = Map.empty
            }
        ]
        graph
      expectEdges [] graph
