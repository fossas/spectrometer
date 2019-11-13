module Ruby.GemfileLockTest
  ( spec_analyze
  ) where

import Prologue

import qualified Data.Map.Strict as M
import           Polysemy
import           Polysemy.Input
import qualified Data.Text.IO as TIO
import           Text.Megaparsec

import           Effect.GraphBuilder
import qualified Graph as G
import           Strategy.Ruby.GemfileLock
import           GraphUtil

import qualified Test.Tasty.Hspec as T

dependencyOne :: G.Dependency
dependencyOne = G.Dependency { dependencyType = G.GemType
                        , dependencyName = "dep-one"
                        , dependencyVersion = Just (G.CEq "1.0.0")
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        }

dependencyTwo :: G.Dependency
dependencyTwo = G.Dependency { dependencyType = G.GemType
                        , dependencyName = "dep-two"
                        , dependencyVersion = Just (G.CEq "2.0.0")
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        }

dependencyThree :: G.Dependency
dependencyThree = G.Dependency { dependencyType = G.GemType
                        , dependencyName = "dep-three"
                        , dependencyVersion = Just (G.CEq "3.0.0")
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        }

gitSection :: Section
gitSection = GitSection "temp" (Just "12345") (Just "branch") [ Spec { specVersion = "1.0.0"
                                                , specName = "dep-one"
                                                , specDeps = [ SpecDep { depName = "dep-three" }
                                                            , SpecDep { depName = "dep-two"}
                                                            ]
                                                }
                                             ]

gemSection :: Section
gemSection = GemSection "remote" [ Spec { specVersion = "2.0.0"
                                        , specName = "dep-two"
                                        , specDeps = [ SpecDep { depName = "dep-three" } ]
                                        }
                                 , Spec { specVersion = "3.0.0"
                                        , specName = "dep-three"
                                        , specDeps = []
                                        }
                                 ]

dependencySection :: Section
dependencySection = DependencySection [ DirectDep { directName = "dep-one" }
                                      , DirectDep { directName = "dep-two" }
                                      ]

gemfileLockSection :: [Section]
gemfileLockSection = [gitSection , gemSection, dependencySection]

spec_analyze :: T.Spec
spec_analyze = do
  gemfileLock <- T.runIO (TIO.readFile "test/Ruby/testdata/gemfileLock")

  T.describe "gemfile lock analyzer" $
    T.it "produces the expected output" $ do
      let graph = analyze
            & runInputConst @[Section] gemfileLockSection
            & run
      expectDeps [dependencyOne, dependencyTwo, dependencyThree] graph
      expectDirect [dependencyOne, dependencyTwo] graph
      expectEdges [ (dependencyOne, dependencyTwo)
                  , (dependencyOne, dependencyThree)
                  , (dependencyTwo, dependencyThree)
                  ] graph

  T.describe "gemfile lock parser" $ do
    T.it "parses error messages into an empty list" $ do
      case runParser findSections "" gemfileLock of
        Left _ -> T.expectationFailure "failed to parse"
        Right result -> do
            result `T.shouldContain` [gitSection]
            result `T.shouldContain` [dependencySection]
            result `T.shouldContain` [gemSection]
