module Elixir.MixExsSpec (
  spec,
) where

import Strategy.Elixir.MixExs (
  DepSCM (..),
  MixExsPackage (..),
  PackageName (..),
  buildGraph,
  mixExsFileParser,
 )

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvTesting),
  DepType (GitType, HexType),
  Dependency (Dependency),
  VerConstraint (CCompatible, CEq, CGreaterOrEq),
 )
import GraphUtil (expectDeps, expectDirect, expectEdges)
import Test.Hspec (
  Spec,
  describe,
  expectationFailure,
  it,
  runIO,
  shouldBe,
 )
import Text.Megaparsec (runParser)

createExsPkg :: Text -> MixExsPackage
createExsPkg name = MixExsPackage (PackageName name) Hex (Just $ CGreaterOrEq "1.0.0") []

expectedMixExsContent :: [MixExsPackage]
expectedMixExsContent =
  [ MixExsPackage (PackageName "pkg_a") Hex (Just $ CCompatible "0.4.0") []
  , MixExsPackage (PackageName "pkg_b") Hex (Just $ CCompatible "1.0.0") []
  , MixExsPackage (PackageName "pkg_c") Hex (Just $ CCompatible "0.22") [EnvDevelopment]
  , MixExsPackage (PackageName "pkg_d") Hex (Just $ CGreaterOrEq "1.0.0") [EnvTesting]
  , MixExsPackage (PackageName "pkg_e") Hex (Just $ CCompatible "1.1.0") [EnvDevelopment, EnvTesting]
  , MixExsPackage (PackageName "pkg_f") (Git "https://github.com/f/pkg.git" (Just "1.1.1")) Nothing []
  , MixExsPackage (PackageName "pkg_g") (Git "https://github.com/g/pkg.git" (Just "005dc")) Nothing []
  , MixExsPackage (PackageName "pkg_h") (Git "https://github.com/h/pkg.git" (Just "develop")) Nothing []
  , MixExsPackage (PackageName "pkg_i") (Other "./../some-path") Nothing []
  , createExsPkg "pkg_j"
  , createExsPkg "pkg_l"
  , createExsPkg "pkg_k"
  , createExsPkg "pkg_m"
  , createExsPkg "pkg_n"
  , MixExsPackage (PackageName "pkg_o") (Git "https://github.com/o/pkg.git" Nothing) Nothing []
  , MixExsPackage (PackageName "pkg_p") (Git "https://github.com/p/pkg.git" Nothing) Nothing []
  ]

spec :: Spec
spec = do
  describe "MixExsSpec" $ do
    contents <- runIO (TIO.readFile "test/Elixir/testdata/mix.exs")

    it "should parse mix.exs file" $ do
      case runParser mixExsFileParser "" contents of
        Left failCode -> expectationFailure $ show failCode
        Right result -> result `shouldBe` expectedMixExsContent

  describe "buildGraph" $ do
    it "should build ignore path dependencies" $ do
      let mixExsContent = [MixExsPackage (PackageName "pkg_i") (Other "./../some-path") Nothing []]
      let graph = buildGraph mixExsContent
      expectDirect [] graph
      expectDeps [] graph
      expectEdges [] graph

    it "should build graph" $ do
      let mixExsContent =
            [ MixExsPackage (PackageName "pkg_o") (Git "https://github.com/o/pkg.git" Nothing) Nothing []
            , MixExsPackage (PackageName "pkg_f") (Git "https://github.com/f/pkg.git" (Just "develop")) Nothing []
            , MixExsPackage (PackageName "pkg_c") Hex (Just $ CCompatible "0.22") [EnvDevelopment]
            , MixExsPackage (PackageName "pkg_i") (Other "./../some-path") Nothing []
            ]
      let expectedDeps =
            [ Dependency GitType "https://github.com/o/pkg.git" (Nothing) [] [] Map.empty
            , Dependency GitType "https://github.com/f/pkg.git" (Just $ CEq "develop") [] [] Map.empty
            , Dependency HexType "pkg_c" (Just $ CCompatible "0.22") [] [EnvDevelopment] Map.empty
            ]
      let graph = buildGraph mixExsContent
      expectDirect expectedDeps graph
      expectDeps expectedDeps graph
      expectEdges [] graph
