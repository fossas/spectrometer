module Dart.PubDepsSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.IO qualified as TIO
import DepTypes
import GraphUtil (expectDeps, expectDirect, expectEdges)
import Strategy.Dart.PubDeps (PubPkg (..), buildGraph, depsCmdOutputParser)
import Strategy.Dart.PubSpecLock (
  PackageName (..),
  PubDepSource (..),
  PubLockContent (..),
  PubLockPkgMetadata (..),
 )
import Test.Hspec
import Text.Megaparsec

expectedDepsCmdOutput :: [PubPkg]
expectedDepsCmdOutput =
  [ PubPkg (PackageName "pkg_a") (Just $ CEq "5.0.0") (Just $ Set.fromList [PackageName "pkg_aa"]) True False
  , PubPkg (PackageName "pkg_b") (Just $ CEq "4.0.0") Nothing True False
  , PubPkg (PackageName "pkg_c") (Just $ CEq "3.0.0") (Just $ Set.fromList [PackageName "pkg_ca", PackageName "pkg_cb"]) True False
  , PubPkg (PackageName "pkg_d") (Just $ CEq "3.0.1") Nothing True False
  , PubPkg (PackageName "pkg_e") (Just $ CEq "2.0.0") (Just $ Set.fromList [PackageName "pkg_ea"]) True True
  , PubPkg (PackageName "pkg_aa") (Just $ CEq "1.0.0") (Just $ Set.fromList [PackageName "pkg_ca"]) False False
  , PubPkg (PackageName "pkg_ca") (Just $ CEq "1.2.0") Nothing False False
  , PubPkg (PackageName "pkg_cb") (Just $ CEq "1.3.0") Nothing False False
  , PubPkg (PackageName "pkg_ea") (Just $ CEq "1.4.0") Nothing False False
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
            [ PubPkg (PackageName "pkg_a") (Just $ CEq "1.8.0") (Just $ Set.fromList [PackageName "pkg_deep"]) True True
            , PubPkg (PackageName "pkg_deep") (Just $ CEq "1.10.0") Nothing False False
            ]

      let lockContent =
            PubLockContent
              { packages =
                  Map.fromList
                    [
                      ( PackageName "pkg_a"
                      , PubLockPkgMetadata
                          { pubLockPkgIsDirect = True
                          , pubLockPkgSource = PubLockPkgHostedSource (Just "pkg_a") (Just "https://pub.dartlang.org")
                          , pubLockPkgVersion = Just $ CEq "1.8.0"
                          , pubLockPkgEnvironment = []
                          }
                      )
                    ,
                      ( PackageName "pkg_deep"
                      , PubLockPkgMetadata
                          { pubLockPkgIsDirect = False
                          , pubLockPkgSource = PubLockPkgHostedSource (Just "pkg_deep") (Just "https://pub.dartlang.org")
                          , pubLockPkgVersion = Just $ CEq "1.10.0"
                          , pubLockPkgEnvironment = []
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
      let pubDepsContent = [PubPkg (PackageName "pkg_path") (Just $ CEq "1.10.0") Nothing True False]
      let lockContent =
            PubLockContent
              { packages =
                  Map.fromList
                    [
                      ( PackageName "pkg_path"
                      , PubLockPkgMetadata
                          { pubLockPkgIsDirect = True
                          , pubLockPkgSource = PubLockPkgPathSource "./../dir"
                          , pubLockPkgVersion = Just $ CEq "1.8.0"
                          , pubLockPkgEnvironment = []
                          }
                      )
                    ]
              }

      let graph = buildGraph lockContent pubDepsContent
      expectDirect [] graph
      expectDeps [] graph
      expectEdges [] graph

    it "should ignore sdk dependency" $ do
      let pubDepsContent = [PubPkg (PackageName "pkg_sdk") (Just $ CEq "1.10.0") Nothing True False]

      let lockContent =
            PubLockContent
              { packages =
                  Map.fromList
                    [
                      ( PackageName "pkg_sdk"
                      , PubLockPkgMetadata
                          { pubLockPkgIsDirect = True
                          , pubLockPkgSource = PubLockPkgSdkSource "pkg_sdk_source"
                          , pubLockPkgVersion = Just $ CEq "1.8.0"
                          , pubLockPkgEnvironment = []
                          }
                      )
                    ]
              }

      let graph = buildGraph lockContent pubDepsContent
      expectDirect [] graph
      expectDeps [] graph
      expectEdges [] graph
