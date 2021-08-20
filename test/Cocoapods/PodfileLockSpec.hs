module Cocoapods.PodfileLockSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Void (Void)
import DepTypes
import GraphUtil
import Strategy.Cocoapods.PodfileLock
import Test.Hspec qualified as T
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (Parsec, errorBundlePretty, parse, runParser)

parseMatch :: (Show a, Eq a) => Parsec Void Text a -> Text -> a -> T.Expectation
parseMatch parser input expected = parse parser "" input `shouldParse` expected

depOf :: Text -> Maybe Text -> Dependency
depOf name version =
  Dependency
    { dependencyType = PodType
    , dependencyName = name
    , dependencyVersion = CEq <$> version
    , dependencyLocations = []
    , dependencyEnvironments = []
    , dependencyTags = Map.empty
    }

dependencyOne :: Dependency
dependencyOne = depOf "one" (Just "1.0.0")

dependencyTwo :: Dependency
dependencyTwo = depOf "two" (Just "2.0.0")

dependencyThree :: Dependency
dependencyThree = depOf "three" (Just "3.0.0")

dependencyFour :: Dependency
dependencyFour = depOf "four" (Just "4.0.0")

dependencyAbnormalName :: Dependency
dependencyAbnormalName = depOf "ab-normal/+name" (Just "2.0.0")

dependencyNotSoSafeName :: Dependency
dependencyNotSoSafeName = depOf "not-so-safe-name" (Just "2.0.0")

dependencyGitSourced :: Dependency
dependencyGitSourced = depOf "some-dep-sourced-from-git" (Just "2.0.0")

dependencyGitTagged :: Dependency
dependencyGitTagged = depOf "depWithTag" (Just "2.0.0")

dependencyTwoDepA :: Dependency
dependencyTwoDepA = depOf "two_dep_A" Nothing

dependencyTwoDepB :: Dependency
dependencyTwoDepB = depOf "two-dep-B" Nothing

podSection :: Section
podSection =
  PodSection
    [ Pod "one" "1.0.0" [Dep "two" Nothing, Dep "three" Nothing, Dep "ab-normal/+name" Nothing]
    , Pod "two" "2.0.0" [Dep "two_dep_A" Nothing, Dep "two-dep-B" Nothing]
    , Pod "three" "3.0.0" [Dep "four" Nothing]
    , Pod "ab-normal/+name" "2.0.0" []
    , Pod "four" "4.0.0" []
    , Pod "not-so-safe-name" "2.0.0" []
    , Pod "some-dep-sourced-from-git" "2.0.0" []
    , Pod "depWithTag" "2.0.0" []
    ]

dependencySection :: Section
dependencySection =
  DependencySection
    [ Dep "one" Nothing
    , Dep "three" Nothing
    , Dep "not-so-safe-name" Nothing
    , Dep "some-dep-sourced-from-git" (Just $ PodGitSrc "git@github.example.com:ab/ios.git" $ Just "559e6a")
    , Dep "depWithTag" (Just $ PodGitSrc "git@github.example.com:ab/cap.git" $ Just "v1.2.3")
    ]

spec :: T.Spec
spec = do
  T.describe "podfile lock analyzer" $
    T.it "produces the expected output" $ do
      let graph = buildGraph [podSection, dependencySection]
      expectDeps
        [ dependencyOne
        , dependencyTwo
        , dependencyThree
        , dependencyAbnormalName
        , dependencyFour
        , dependencyNotSoSafeName
        , dependencyGitSourced
        , dependencyGitTagged
        , dependencyTwoDepA
        , dependencyTwoDepB
        ]
        graph
      expectDirect
        [ dependencyOne
        , dependencyThree
        , dependencyNotSoSafeName
        , dependencyGitSourced
        , dependencyGitTagged
        ]
        graph
      expectEdges
        [ (dependencyOne, dependencyAbnormalName)
        , (dependencyOne, dependencyTwo)
        , (dependencyOne, dependencyThree)
        , (dependencyTwo, dependencyTwoDepA)
        , (dependencyTwo, dependencyTwoDepB)
        , (dependencyThree, dependencyFour)
        ]
        graph

  podLockFile <- T.runIO (TIO.readFile "test/Cocoapods/testdata/Podfile.lock")
  T.describe "podfile lock parser" $ do
    T.describe "dep parser" $ do
      let shouldParseInto input = parseMatch depParser input
      T.it "parses names" $ do
        "- atomFire (1.0.0)" `shouldParseInto` Dep "atomFire" Nothing
        "- \"at/+om (1.0.0)\"" `shouldParseInto` Dep "at/+om" Nothing
        "- \"not-so-safe-name (2.0.0)\"" `shouldParseInto` Dep "not-so-safe-name" Nothing

    T.describe "pod parser" $ do
      let shouldParseInto input = parseMatch podParser input
      T.it "parses names" $ do
        "- atomFire (1.0.0)" `shouldParseInto` Pod "atomFire" "1.0.0" ([])
        "- \"atomFire (1.0.0)\"" `shouldParseInto` Pod "atomFire" "1.0.0" ([])

    T.describe "pod dependency" $ do
      let shouldParseInto input = parseMatch depGitSrcParser input
      T.it "parses git dependency" $ do
        "from `gitUrl`, commit `c`" `shouldParseInto` PodGitSrc "gitUrl" (Just "c")
        "from `gitUrl`, tag `t`" `shouldParseInto` PodGitSrc "gitUrl" (Just "t")
        "from `gitUrl`, branch `b`" `shouldParseInto` PodGitSrc "gitUrl" (Just "b")
        "from `gitUrl`" `shouldParseInto` PodGitSrc "gitUrl" Nothing

    T.it "parses pod and dependency sections" $
      case runParser findSections "" podLockFile of
        Left err -> T.expectationFailure ("failed to parse: " <> errorBundlePretty err)
        Right result -> do
          result `T.shouldContain` [podSection]
          result `T.shouldContain` [dependencySection]
