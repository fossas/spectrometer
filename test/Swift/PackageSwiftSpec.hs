module Swift.PackageSwiftSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import DepTypes (DepType (GitType, SwiftType), Dependency (..), VerConstraint (CEq))
import GraphUtil (expectDeps, expectDirect, expectEdges)
import Strategy.Swift.PackageSwift (
  SwiftPackage (..),
  SwiftPackageDep (..),
  SwiftPackageGitDep (..),
  SwiftPackageGitDepRequirement (..),
  buildGraph,
  parsePackageSwiftFile,
 )
import Test.Hspec
import Text.Megaparsec (runParser)

gitDepWithoutConstraint :: Text -> SwiftPackageGitDep
gitDepWithoutConstraint url = SwiftPackageGitDep url Nothing

gitDepWithBranch :: Text -> Text -> SwiftPackageGitDep
gitDepWithBranch url branch = (gitDepWithoutConstraint url){versionRequirement = Just $ Branch branch}

gitDepWithRevision :: Text -> Text -> SwiftPackageGitDep
gitDepWithRevision url revision = (gitDepWithoutConstraint url){versionRequirement = Just $ Revision revision}

gitDepFrom :: Text -> Text -> SwiftPackageGitDep
gitDepFrom url from = (gitDepWithoutConstraint url){versionRequirement = Just $ From from}

gitDepExactly :: Text -> Text -> SwiftPackageGitDep
gitDepExactly url exact = (gitDepWithoutConstraint url){versionRequirement = Just $ Exact exact}

gitDepUpToNextMajor :: Text -> Text -> SwiftPackageGitDep
gitDepUpToNextMajor url constraint = (gitDepWithoutConstraint url){versionRequirement = Just $ UpToNextMajor constraint}

gitDepUpToNextMinor :: Text -> Text -> SwiftPackageGitDep
gitDepUpToNextMinor url constraint = (gitDepWithoutConstraint url){versionRequirement = Just $ UpToNextMinor constraint}

gitDepWithClosedRange :: Text -> Text -> Text -> SwiftPackageGitDep
gitDepWithClosedRange url lhs rhs = (gitDepWithoutConstraint url){versionRequirement = Just $ ClosedInterval (lhs, rhs)}

gitDepWithRhsHalfOpenInterval :: Text -> Text -> Text -> SwiftPackageGitDep
gitDepWithRhsHalfOpenInterval url lhs rhs = (gitDepWithoutConstraint url){versionRequirement = Just $ RhsHalfOpenInterval (lhs, rhs)}

expectedSwiftPackage :: SwiftPackage
expectedSwiftPackage =
  SwiftPackage "5.3" $
    map
      GitSource
      [ -- without any constraint
        gitDepWithoutConstraint "https://github.com/kirualex/SwiftyGif.git"
      , -- from
        gitDepFrom "https://github.com/apple/example-package-playingcard.git" "3.0.0"
      , gitDepFrom "https://github.com/kaishin/Gifu.git" "3.2.2"
      , -- exact
        gitDepExactly "https://github.com/kelvin13/jpeg.git" "1.0.0"
      , gitDepExactly "https://github.com/shogo4405/HaishinKit.swift" "1.1.6"
      , -- upTo constraint
        gitDepUpToNextMajor "https://github.com/dankogai/swift-sion" "0.0.1"
      , gitDepUpToNextMinor "git@github.com:behrang/YamlSwift.git" "3.4.0"
      , -- branch
        gitDepWithBranch "https://github.com/vapor/vapor" "main"
      , gitDepWithBranch "git@github.com:vapor-community/HTMLKit.git" "function-builder"
      , -- revision
        gitDepWithRevision "https://github.com/SwiftyBeaver/SwiftyBeaver.git" "607fc8d64388652135f4dcf6a1a340e3a0641088"
      , gitDepWithRevision "https://github.com/roberthein/TinyConstraints.git" "3262e5c591d4ab6272255df2087a01bbebd138dc"
      , -- range
        gitDepWithRhsHalfOpenInterval "https://github.com/LeoNatan/LNPopupController.git" "2.5.0" "2.5.6"
      , gitDepWithClosedRange "https://github.com/Polidea/RxBluetoothKit.git" "3.0.5" "3.0.7"
      ]

spec :: Spec
spec = do
  packageDotSwiftFile <- runIO (TIO.readFile "test/Swift/testdata/Package.swift")

  describe "Parses Package.swift file" $ do
    it "should parse swift-tools-version" $ do
      case runParser parsePackageSwiftFile "" packageDotSwiftFile of
        Left failCode -> expectationFailure $ show failCode
        Right result -> result `shouldBe` expectedSwiftPackage

  describe "buildGraph" $ do
    it "should use git dependency type, when constraint is of branch, revision, or exact type" $ do
      let expectedDeps =
            [ Dependency GitType "some-url" (CEq <$> Just "some-ref") [] [] Map.empty
            , Dependency GitType "some-url" (CEq <$> Just "some-branch") [] [] Map.empty
            , Dependency GitType "some-url" (CEq <$> Just "1.0.0") [] [] Map.empty
            ]
      let graph =
            buildGraph $
              SwiftPackage
                "5.3"
                [ GitSource $ gitDepWithRevision "some-url" "some-ref"
                , GitSource $ gitDepWithBranch "some-url" "some-branch"
                , GitSource $ gitDepExactly "some-url" "1.0.0"
                ]
      expectDirect expectedDeps graph
      expectDeps expectedDeps graph
      expectEdges [] graph

    it "should use swift dependency type, when constraint uses follows, range, upToNextMajor, or upToNextMinor" $ do
      let graph =
            buildGraph $
              SwiftPackage
                "5.3"
                [ GitSource $ gitDepWithoutConstraint "some-url-dep"
                , GitSource $ gitDepFrom "some-url-dep" "3.0.0"
                , GitSource $ gitDepUpToNextMajor "some-url-dep" "2.0.0"
                , GitSource $ gitDepUpToNextMinor "some-url-dep" "1.0.0"
                , GitSource $ gitDepWithRhsHalfOpenInterval "some-url-dep" "2.5.0" "2.5.6"
                , GitSource $ gitDepWithClosedRange "some-url-dep" "3.0.5" "3.0.7"
                ]
      let expectedDeps =
            [ Dependency SwiftType "some-url-dep" Nothing [] [] Map.empty
            , Dependency SwiftType "some-url-dep" (CEq <$> Just "^3.0.0") [] [] Map.empty
            , Dependency SwiftType "some-url-dep" (CEq <$> Just "^2.0.0") [] [] Map.empty
            , Dependency SwiftType "some-url-dep" (CEq <$> Just "~1.0.0") [] [] Map.empty
            , Dependency SwiftType "some-url-dep" (CEq <$> Just ">=2.5.0 <2.5.6") [] [] Map.empty
            , Dependency SwiftType "some-url-dep" (CEq <$> Just ">=3.0.5 <=3.0.7") [] [] Map.empty
            ]
      expectDirect expectedDeps graph
      expectDeps expectedDeps graph
      expectEdges [] graph
