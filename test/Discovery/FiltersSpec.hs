{-# LANGUAGE TemplateHaskell #-}

module Discovery.FiltersSpec (
  spec,
) where

import Data.Foldable (traverse_)
import Data.Set qualified as S
import Data.Set.NonEmpty
import Data.Text qualified as T
import Discovery.Filters
import Path
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Types (BuildTarget (..), FoundTargets (..))

spec :: Spec
spec = do
  describe "filterParser" $ do
    it "should parse both types of projects" $ do
      runParser filterParser "" "foo@bar" `shouldParse` (ProjectFilter "foo" $(mkRelDir "bar"))
      runParser filterParser "" "foo@bar/baz" `shouldParse` (ProjectFilter "foo" $(mkRelDir "bar/baz"))
      runParser filterParser "" "foo@bar/baz:quux" `shouldParse` (TargetFilter "foo" $(mkRelDir "bar/baz") (BuildTarget "quux"))
    it "should fail on malformed input" $ do
      runParser filterParser "" `shouldFailOn` "foo@bar:"
      runParser filterParser "" `shouldFailOn` "foo@"
    it "should work for a subset of weird parse cases" $ do
      runParser filterParser "" "foo@bar@baz" `shouldParse` ProjectFilter "foo" $(mkRelDir "bar@baz")
      runParser filterParser "" "foo@\127" `shouldParse` ProjectFilter "foo" $(mkRelDir "\127")

    {-
      Directory Structure
      /foo
       mvn:[]
       gradle:[foo, bar]
       /bar
        mvn:[]
        gradle:[foo, bar]
        /baz
         mvn:[]
      /quux
        mvn:[]

    -}
    describe "FilterCombination filters" $ do
      let mvnFoo = ("mvn", $(mkRelDir "foo"))
          gradleFoo = ("gradle", $(mkRelDir "foo"))
          mvnFooBar = ("mvn", $(mkRelDir "foo/bar"))
          gradleFooBar = ("gradle", $(mkRelDir "foo/bar"))
          mvnFooBarBaz = ("mvn", $(mkRelDir "foo/bar/baz"))
          mvnQuux = ("mvn", $(mkRelDir "quux"))
          gradleTargets = maybe ProjectWithoutTargets FoundTargets (nonEmpty $ S.fromList [BuildTarget "foo", BuildTarget "bar"])
          fooTargetAssertion = (Just . FoundTargets) =<< nonEmpty (S.fromList [BuildTarget "foo"])
          barTargetAssertion = (Just . FoundTargets) =<< nonEmpty (S.fromList [BuildTarget "bar"])
          fooBarTargetAssertion = (Just . FoundTargets) =<< nonEmpty (S.fromList [BuildTarget "bar", BuildTarget "foo"])

      it "includes an entire directory" $ do
        let include =
              FilterCombination
                { combinedTargets = []
                , combinedPaths = [$(mkRelDir "quux")]
                }
            exclude =
              FilterCombination
                { combinedTargets = []
                , combinedPaths = []
                }

        testHarness
          include
          exclude
          [ (mvnFoo, ProjectWithoutTargets, Nothing)
          , (gradleFoo, gradleTargets, Nothing)
          , (mvnFooBar, ProjectWithoutTargets, Nothing)
          , (gradleFooBar, gradleTargets, Nothing)
          , (mvnFooBarBaz, ProjectWithoutTargets, Nothing)
          , (mvnQuux, ProjectWithoutTargets, Just ProjectWithoutTargets)
          ]

      it "includes a subdirectory" $ do
        let include =
              FilterCombination
                { combinedTargets = []
                , combinedPaths = [$(mkRelDir "foo/bar")]
                }
            exclude =
              FilterCombination
                { combinedTargets = []
                , combinedPaths = []
                }

        testHarness
          include
          exclude
          [ (mvnFoo, ProjectWithoutTargets, Nothing)
          , (gradleFoo, gradleTargets, Nothing)
          , (mvnFooBar, ProjectWithoutTargets, Just ProjectWithoutTargets)
          , (gradleFooBar, gradleTargets, fooBarTargetAssertion)
          , (mvnFooBarBaz, ProjectWithoutTargets, Just ProjectWithoutTargets)
          , (mvnQuux, ProjectWithoutTargets, Nothing)
          ]

      it "excludes a directory" $ do
        let include =
              FilterCombination
                { combinedTargets = []
                , combinedPaths = []
                }
            exclude =
              FilterCombination
                { combinedTargets = []
                , combinedPaths = [$(mkRelDir "foo")]
                }

        testHarness
          include
          exclude
          [ (mvnFoo, ProjectWithoutTargets, Nothing)
          , (gradleFoo, gradleTargets, Nothing)
          , (mvnFooBar, ProjectWithoutTargets, Nothing)
          , (gradleFooBar, gradleTargets, Nothing)
          , (mvnFooBarBaz, ProjectWithoutTargets, Nothing)
          , (mvnQuux, ProjectWithoutTargets, Just ProjectWithoutTargets)
          ]

      it "excludes a subdirectory" $ do
        let include =
              FilterCombination
                { combinedTargets = []
                , combinedPaths = []
                }
            exclude =
              FilterCombination
                { combinedTargets = []
                , combinedPaths = [$(mkRelDir "foo/bar")]
                }

        testHarness
          include
          exclude
          [ (mvnFoo, ProjectWithoutTargets, Just ProjectWithoutTargets)
          , (gradleFoo, gradleTargets, fooBarTargetAssertion)
          , (mvnFooBar, ProjectWithoutTargets, Nothing)
          , (gradleFooBar, gradleTargets, Nothing)
          , (mvnFooBarBaz, ProjectWithoutTargets, Nothing)
          , (mvnQuux, ProjectWithoutTargets, Just ProjectWithoutTargets)
          ]

      it "excludes a target in a subdirectory" $ do
        let include =
              FilterCombination
                { combinedTargets = []
                , combinedPaths = []
                }
            exclude =
              FilterCombination
                { combinedTargets = [TypeDirTargetTarget "gradle" $(mkRelDir "foo/bar") (BuildTarget "foo")]
                , combinedPaths = []
                }

        testHarness
          include
          exclude
          [ (mvnFoo, ProjectWithoutTargets, Just ProjectWithoutTargets)
          , (gradleFoo, gradleTargets, fooBarTargetAssertion)
          , (mvnFooBar, ProjectWithoutTargets, Just ProjectWithoutTargets)
          , (gradleFooBar, gradleTargets, barTargetAssertion)
          , (mvnFooBarBaz, ProjectWithoutTargets, Just ProjectWithoutTargets)
          , (mvnQuux, ProjectWithoutTargets, Just ProjectWithoutTargets)
          ]

      it "excludes a subdirectory of an included directory" $ do
        let include =
              FilterCombination
                { combinedTargets = []
                , combinedPaths = [$(mkRelDir "foo")]
                }
            exclude =
              FilterCombination
                { combinedTargets = []
                , combinedPaths = [$(mkRelDir "foo/bar")]
                }

        testHarness
          include
          exclude
          [ (mvnFoo, ProjectWithoutTargets, Just ProjectWithoutTargets)
          , (gradleFoo, gradleTargets, fooBarTargetAssertion)
          , (mvnFooBar, ProjectWithoutTargets, Nothing)
          , (gradleFooBar, gradleTargets, Nothing)
          , (mvnFooBarBaz, ProjectWithoutTargets, Nothing)
          , (mvnQuux, ProjectWithoutTargets, Nothing)
          ]

      it "excludes a build-target in an included directory" $ do
        let include =
              FilterCombination
                { combinedTargets = []
                , combinedPaths = [$(mkRelDir "foo")]
                }
            exclude =
              FilterCombination
                { combinedTargets = [TypeDirTargetTarget "gradle" $(mkRelDir "foo") (BuildTarget "foo")]
                , combinedPaths = []
                }

        testHarness
          include
          exclude
          [ (mvnFoo, ProjectWithoutTargets, Just ProjectWithoutTargets)
          , (gradleFoo, gradleTargets, barTargetAssertion)
          , (mvnFooBar, ProjectWithoutTargets, Just ProjectWithoutTargets)
          , (gradleFooBar, gradleTargets, fooBarTargetAssertion)
          , (mvnFooBarBaz, ProjectWithoutTargets, Just ProjectWithoutTargets)
          , (mvnQuux, ProjectWithoutTargets, Nothing)
          ]

      it "does the thing" $ do
        let include =
              FilterCombination
                { combinedTargets = [TypeDirTargetTarget "gradle" $(mkRelDir "foo") (BuildTarget "foo")]
                , combinedPaths = [$(mkRelDir "foo")]
                }
            exclude =
              FilterCombination
                { combinedTargets = []
                , combinedPaths = []
                }

        testHarness
          include
          exclude
          [ (mvnFoo, ProjectWithoutTargets, Just ProjectWithoutTargets)
          , (gradleFoo, gradleTargets, fooTargetAssertion)
          , (mvnFooBar, ProjectWithoutTargets, Just ProjectWithoutTargets)
          , (gradleFooBar, gradleTargets, fooBarTargetAssertion)
          , (mvnFooBarBaz, ProjectWithoutTargets, Just ProjectWithoutTargets)
          , (mvnQuux, ProjectWithoutTargets, Nothing)
          ]

testHarness :: FilterCombination -> FilterCombination -> [((T.Text, Path Rel Dir), FoundTargets, Maybe FoundTargets)] -> Expectation
testHarness include exclude = traverse_ testSingle
  where
    testSingle ((buildtool, dir), targets, expected) = applyFilters (AllFilters [] include exclude) buildtool dir targets `shouldBe` expected
