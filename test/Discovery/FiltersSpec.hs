{-# LANGUAGE TemplateHaskell #-}

module Discovery.FiltersSpec (
  spec,
) where

import App.Fossa.Configuration
import Data.Foldable (traverse_)
import Data.Set qualified as S
import Data.Text qualified as T
import Discovery.Filters
import Path
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Types (BuildTarget (..))
import qualified Data.List.NonEmpty as NE

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

  describe "applyFilter" $ do
    it "should allow all BuildTargets when a project filter succeeds" $ do
      let filterFooAtBar = ProjectFilter "foo" $(mkRelDir "bar")
      applyFilter filterFooAtBar "foo" $(mkRelDir "bar") S.empty `shouldBe` Just S.empty
      applyFilter filterFooAtBar "foo" $(mkRelDir "bar") (S.singleton (BuildTarget "baz")) `shouldBe` (Just (S.singleton (BuildTarget "baz")))

    it "should fail with Nothing" $ do
      let filterFooAtBar = ProjectFilter "foo" $(mkRelDir "bar")
      applyFilter filterFooAtBar "not" $(mkRelDir "ok") S.empty `shouldBe` Nothing
      applyFilter filterFooAtBar "not" $(mkRelDir "ok") (S.singleton (BuildTarget "baz")) `shouldBe` Nothing

    it "should only allow a matching BuildTarget" $ do
      let filterFooAtBarBaz = TargetFilter "foo" $(mkRelDir "bar") (BuildTarget "baz")
      applyFilter filterFooAtBarBaz "foo" $(mkRelDir "bar") S.empty `shouldBe` Nothing
      applyFilter filterFooAtBarBaz "foo" $(mkRelDir "bar") (S.singleton (BuildTarget "baz")) `shouldBe` (Just (S.singleton (BuildTarget "baz")))
      applyFilter filterFooAtBarBaz "foo" $(mkRelDir "bar") (S.fromList [BuildTarget "baz", BuildTarget "quux"]) `shouldBe` (Just (S.singleton (BuildTarget "baz")))

  describe "applyFiltersOld" $ do
    it "should succeed when any filters succeed" $ do
      let filterFooAtBar = ProjectFilter "foo" $(mkRelDir "bar")
      let filterBazAtQuux = ProjectFilter "baz" $(mkRelDir "quux")
      applyFiltersOld [filterFooAtBar, filterBazAtQuux] "foo" $(mkRelDir "bar") (S.singleton (BuildTarget "ok"))
        `shouldBe` Just (S.singleton (BuildTarget "ok"))
      applyFiltersOld [filterFooAtBar, filterBazAtQuux] "foo" $(mkRelDir "bar") S.empty
        `shouldBe` Just S.empty

    it "should fail when all filters fail" $ do
      let filterFooAtBar = ProjectFilter "foo" $(mkRelDir "bar")
      let filterBazAtQuux = ProjectFilter "baz" $(mkRelDir "quux")
      applyFiltersOld [filterFooAtBar, filterBazAtQuux] "not" $(mkRelDir "ok") S.empty
        `shouldBe` Nothing

    it "should union successful filter results" $ do
      let filterFooAtBarBaz = TargetFilter "foo" $(mkRelDir "bar") (BuildTarget "baz")
      let filterFooAtBarOk = TargetFilter "foo" $(mkRelDir "bar") (BuildTarget "ok")
      applyFiltersOld [filterFooAtBarBaz, filterFooAtBarOk] "foo" $(mkRelDir "bar") (S.fromList [BuildTarget "baz", BuildTarget "ok", BuildTarget "notok"])
        `shouldBe` Just (S.fromList [BuildTarget "baz", BuildTarget "ok"])

  describe "Analysis target filtering" $ do
    describe "Target filters" $ do
      describe "Only filters" $ do
        it "returns Nothing when no targets match" $ do
          let configTarget = Just $ ConfigTargets [ConfigTarget (T.pack "mvn") Nothing] []
          let testFilters = CombinedFilters [] configTarget Nothing
          applyFilters testFilters "not" $(mkRelDir "ok") S.empty `shouldBe` Nothing

        it "should pass all when then build tool is selected have some" $ do
          let configTarget = Just $ ConfigTargets [ConfigTarget (T.pack "mvn") Nothing] []
          let testFilter = CombinedFilters [] configTarget Nothing
          applyFilters testFilter "mvn" $(mkRelDir "bar") (S.fromList [BuildTarget "baz", BuildTarget "quux"]) `shouldBe` Just (S.fromList [BuildTarget "baz", BuildTarget "quux"])

        it "should only accept the targets that match the directory target only list" $ do
          let configTarget = Just $ ConfigTargets [ConfigTarget (T.pack "mvn") (Just $ DirectoryFilter $(mkRelDir "bar"))] []
              testFilter = CombinedFilters [] configTarget Nothing
          applyFilters testFilter "mvn" $(mkRelDir "bar") (S.fromList [BuildTarget "baz", BuildTarget "quux"]) `shouldBe` Just (S.fromList [BuildTarget "baz", BuildTarget "quux"])

        it "should only accept the targets that match the exact target filter target only list" $ do
          let configTarget = Just $ ConfigTargets [ConfigTarget (T.pack "mvn") (Just $ ExactTargetFilter $(mkRelDir "bar") "quux")] []
              testFilter = CombinedFilters [] configTarget Nothing
          applyFilters testFilter "mvn" $(mkRelDir "bar") (S.fromList [BuildTarget "baz", BuildTarget "quux"]) `shouldBe` Just (S.fromList [BuildTarget "quux"])

      describe "Exclude filters" $ do
        it "should exclude the targets that match the specific target exclude list" $ do
          let testConfigOne = CombinedFilters [] configTargets1 Nothing
              configTargets1 = Just $ ConfigTargets [] [ConfigTarget (T.pack "mvn") (Just $ DirectoryFilter $(mkRelDir "bar"))]
          applyFilters testConfigOne "mvn" $(mkRelDir "bar") (S.fromList [BuildTarget "baz", BuildTarget "quux"]) `shouldBe` Nothing

      describe "Path filters" $ do
        describe "Only filters" $ do
          it "should include only the paths that match the target list" $ do
            let testConfig = CombinedFilters [] Nothing configPath
                configPath = Just $ ConfigPaths [$(mkRelDir "bar")] []
            applyFilters testConfig "mvn" $(mkRelDir "bar") (S.fromList [BuildTarget "baz", BuildTarget "quux"]) `shouldBe` Just (S.fromList [BuildTarget "baz", BuildTarget "quux"])

        describe "Exclude filters" $ do
          it "should exclude only the paths that match the target list" $ do
            let testConfig = CombinedFilters [] Nothing configPath
                configPath = Just $ ConfigPaths [] [$(mkRelDir "bar")]
            applyFilters testConfig "mvn" $(mkRelDir "bar") (S.fromList [BuildTarget "baz", BuildTarget "quux"]) `shouldBe` Nothing

    {-
      Directory Structure
      foo
       1
       2
       bar
        3
          baz
          4
      quux
       5
      Testing to validate that 2, 3, and 5 are the only ones included
    -}
    describe "Complex directory with complex filter setup" $ do
      let targetFoo = ConfigTarget (T.pack "mvn") (Just $ ExactTargetFilter $(mkRelDir "foo") "1")
          targetQuux = ConfigTarget (T.pack "mvn") (Just $ ExactTargetFilter $(mkRelDir "quux") "5")
          configTargets = Just $ ConfigTargets [targetQuux] [targetFoo]
          testPath = Just $ ConfigPaths [$(mkRelDir "foo")] [$(mkRelDir "foo/bar/baz")]
          testFilters = CombinedFilters [] configTargets testPath

      it "should include target 2 in directory foo" $ do
        applyFilters testFilters "mvn" $(mkRelDir "foo") (S.fromList [BuildTarget "1", BuildTarget "2"]) `shouldBe` Just (S.fromList [BuildTarget "2"])

      it "should include target 3 in directory bar" $ do
        applyFilters testFilters "mvn" $(mkRelDir "foo/bar") (S.fromList [BuildTarget "3"]) `shouldBe` Just (S.fromList [BuildTarget "3"])

      it "should not include anything in foo/bar/baz" $ do
        applyFilters testFilters "mvn" $(mkRelDir "foo/bar/baz") (S.fromList [BuildTarget "4"]) `shouldBe` Nothing

      it "should include target 5 in directory quux" $ do
        applyFilters testFilters "mvn" $(mkRelDir "quux") (S.fromList [BuildTarget "5"]) `shouldBe` Just (S.fromList [BuildTarget "5"])

    describe "Include all build type but exclude one target" $ do
      let targetFoo = ConfigTarget (T.pack "mvn") Nothing
          targetQuux = ConfigTarget (T.pack "mvn") (Just $ ExactTargetFilter $(mkRelDir "foo/bar") "1")
          configTargets = Just $ ConfigTargets [targetFoo] [targetQuux]
          testFilters = CombinedFilters [] configTargets Nothing

      it "should include target 2 in directory foo/bar" $ do
        applyFilters testFilters "mvn" $(mkRelDir "foo/bar") (S.fromList [BuildTarget "1", BuildTarget "2"]) `shouldBe` Just (S.fromList [BuildTarget "2"])

-- target-only: gradle@foo:foo
-- path-only: foo

    -- Include a whole directory
    -- Include only a sub directory
    -- Exclude a whole directory
    -- Exclude a sub directory
    -- Exclude a target in a sub directory
    -- Conflict between only target and only directory -- come back to this
    -- Exclude sub directory of only directory
    -- Exclude target in only directory
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
    describe "Comb filters" $ do
      let mvnFoo = ("mvn", $(mkRelDir "foo"))
          gradleFoo = ("gradle", $(mkRelDir "foo"))
          mvnFooBar = ("mvn", $(mkRelDir "foo/bar"))
          gradleFooBar = ("gradle", $(mkRelDir "foo/bar"))
          mvnFooBarBaz = ("mvn", $(mkRelDir "foo/bar/baz"))
          mvnQuux = ("mvn", $(mkRelDir "quux"))

      it "includes an entire directory" $ do
        let include =
              Comb
                { combTargets = []
                , combPaths = [$(mkRelDir "quux")]
                }
            exclude =
              Comb
                { combTargets = []
                , combPaths = []
                }

        testHarness
          include
          exclude
          [ (mvnFoo, ResultNone)
          , (gradleFoo, ResultNone)
          , (mvnFooBar, ResultNone)
          , (gradleFooBar, ResultNone)
          , (mvnFooBarBaz, ResultNone)
          , (mvnQuux, ResultAll)
          ]

      it "includes a subdirectory" $ do
        let include =
              Comb
                { combTargets = []
                , combPaths = [$(mkRelDir "foo/bar")]
                }
            exclude =
              Comb
                { combTargets = []
                , combPaths = []
                }

        testHarness
          include
          exclude
          [ (mvnFoo, ResultNone)
          , (gradleFoo, ResultNone)
          , (mvnFooBar, ResultAll)
          , (gradleFooBar, ResultAll)
          , (mvnFooBarBaz, ResultAll)
          , (mvnQuux, ResultNone)
          ]

      it "excludes a directory" $ do
        let include =
              Comb
                { combTargets = []
                , combPaths = []
                }
            exclude =
              Comb
                { combTargets = []
                , combPaths = [$(mkRelDir "foo")]
                }

        testHarness
          include
          exclude
          [ (mvnFoo, ResultNone)
          , (gradleFoo, ResultNone)
          , (mvnFooBar, ResultNone)
          , (gradleFooBar, ResultNone)
          , (mvnFooBarBaz, ResultNone)
          , (mvnQuux, ResultAll)
          ]

      it "excludes a subdirectory" $ do
        let include =
              Comb
                { combTargets = []
                , combPaths = []
                }
            exclude =
              Comb
                { combTargets = []
                , combPaths = [$(mkRelDir "foo/bar")]
                }

        testHarness
          include
          exclude
          [ (mvnFoo, ResultAll)
          , (gradleFoo, ResultAll)
          , (mvnFooBar, ResultNone)
          , (gradleFooBar, ResultNone)
          , (mvnFooBarBaz, ResultNone)
          , (mvnQuux, ResultAll)
          ]

      it "excludes a target in a subdirectory" $ do
        let include =
              Comb
                { combTargets = []
                , combPaths = []
                }
            exclude =
              Comb
                { combTargets = [TypeDirTargetTarget "gradle" $(mkRelDir "foo/bar") (BuildTarget "foo")]
                , combPaths = []
                }

        testHarness
          include
          exclude
          [ (mvnFoo, ResultAll)
          , (gradleFoo, ResultAll)
          , (mvnFooBar, ResultAll)
          , (gradleFooBar, ResultExclude (NE.fromList [BuildTarget "foo"]))
          , (mvnFooBarBaz, ResultAll)
          , (mvnQuux, ResultAll)
          ]

      it "excludes a subdirectory of an included directory" $ do
        let include =
              Comb
                { combTargets = []
                , combPaths = [$(mkRelDir "foo")]
                }
            exclude =
              Comb
                { combTargets = []
                , combPaths = [$(mkRelDir "foo/bar")]
                }

        testHarness
          include
          exclude
          [ (mvnFoo, ResultAll)
          , (gradleFoo, ResultAll)
          , (mvnFooBar, ResultNone)
          , (gradleFooBar, ResultNone)
          , (mvnFooBarBaz, ResultNone)
          , (mvnQuux, ResultNone)
          ]

      it "excludes a buildtarget in an included directory" $ do
        let include =
              Comb
                { combTargets = []
                , combPaths = [$(mkRelDir "foo")]
                }
            exclude =
              Comb
                { combTargets = [TypeDirTargetTarget "gradle" $(mkRelDir "foo") (BuildTarget "foo")]
                , combPaths = []
                }

        testHarness
          include
          exclude
          [ (mvnFoo, ResultAll)
          , (gradleFoo, ResultExclude (NE.fromList [BuildTarget "foo"]))
          , (mvnFooBar, ResultAll)
          , (gradleFooBar, ResultAll)
          , (mvnFooBarBaz, ResultAll)
          , (mvnQuux, ResultNone)
          ]

      it "does the thing" $ do
        let include =
              Comb
                { combTargets = [TypeDirTargetTarget "gradle" $(mkRelDir "foo") (BuildTarget "foo")]
                , combPaths = [$(mkRelDir "foo")]
                }
            exclude =
              Comb
                { combTargets = []
                , combPaths = []
                }

        testHarness
          include
          exclude
          [ (mvnFoo, ResultAll)
          , (gradleFoo, ResultInclude (NE.fromList [BuildTarget "foo"]))
          , (mvnFooBar, ResultAll)
          , (gradleFooBar, ResultAll)
          , (mvnFooBarBaz, ResultAll)
          , (mvnQuux, ResultNone)
          ]

testHarness :: Comb -> Comb -> [((T.Text, Path Rel Dir), FilterResult)] -> Expectation
testHarness include exclude = traverse_ testSingle
  where
    testSingle ((buildtool, dir), expected) = apply include exclude buildtool dir `shouldBe` expected
