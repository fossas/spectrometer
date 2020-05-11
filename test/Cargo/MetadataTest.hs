module Cargo.MetadataTest
  ( spec_parse
  ) where

import Prologue

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL

import Strategy.Cargo

import qualified Test.Tasty.Hspec as Test

expectedMetadata :: CargoMetadata
expectedMetadata = CargoMetadata [] [jfmtId] $ Resolve expectedResolveNodes

expectedResolveNodes :: [ResolveNode]
expectedResolveNodes = [ansiTermNode, clapNode, jfmtNode]

registrySource :: T.Text
registrySource = "(registry+https://github.com/rust-lang/crates.io-index)"

mkPkgId :: T.Text -> T.Text -> PackageId
mkPkgId name ver = PackageId name ver registrySource

ansiTermId :: PackageId
ansiTermId = mkPkgId "ansi_term" "0.11.0"

clapId :: PackageId
clapId = mkPkgId "clap" "2.33.0"

jfmtId :: PackageId
jfmtId = PackageId "jfmt" "1.0.0" "(path+file:///path/to/jfmt.rs)"

ansiTermNode :: ResolveNode
ansiTermNode = ResolveNode ansiTermId []

clapNode :: ResolveNode
clapNode = ResolveNode clapId [NodeDependency ansiTermId [NodeDepKind Nothing $ Just "cfg(not(windows))"]]

jfmtNode :: ResolveNode
jfmtNode = ResolveNode jfmtId [NodeDependency clapId [nullKind]]

nullKind :: NodeDepKind
nullKind = NodeDepKind Nothing Nothing

spec_parse :: Test.Spec
spec_parse = 
  Test.describe "cargo metadata parser" $ do
    metaBytes <- Test.runIO $ BL.readFile "test/Cargo/testdata/expected-metadata.json"
    Test.it "should properly construct a resolution tree" $ 
      case eitherDecode metaBytes of
        Left err -> Test.expectationFailure $ "failed to parse: " ++ err
        Right result -> result `Test.shouldBe` expectedMetadata