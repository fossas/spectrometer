{-# LANGUAGE TemplateHaskell #-}

module Discovery.FiltersSpec (
  spec,
) where

import App.Fossa.Configuration
import Data.Set qualified as S
import Data.Text qualified as T
import Discovery.Filters
import Path
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Types (BuildTarget (..))

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
