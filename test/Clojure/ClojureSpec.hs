{-# LANGUAGE OverloadedStrings #-}

module Clojure.ClojureSpec
  ( spec,
  )
where

import qualified Data.EDN as EDN
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as TIO
import DepTypes
import GraphUtil
import Strategy.Clojure
import Test.Hspec

spec :: Spec
spec = do
  describe "clojure analyzer" $ do
    stdout <- runIO $ TIO.readFile "test/Clojure/testdata/lein-deps-stdout.txt"
    it "should work for an example output" $ do
      case EDN.decodeText "lein-deps-stdout.txt" stdout of
        Left err -> expectationFailure ("Failed to decode Deps from EDN: " <> err)
        Right deps -> do
          let graph = buildGraph deps
          expectDirect [clojureComplete, koanEngine, leinKoan, nrepl, clojure] graph
          expectDeps [clojureComplete, koanEngine, fresh, leinKoan, nrepl, clojure, clojureSpecsAlpha, clojureSpecAlpha] graph
          expectEdges [(koanEngine, fresh), (clojure, clojureSpecsAlpha), (clojure, clojureSpecAlpha)] graph

-- [clojure-complete "0.2.5" :exclusions [[org.clojure/clojure]]] nil,
clojureComplete :: Dependency
clojureComplete =
  Dependency
    { dependencyType = MavenType,
      dependencyName = "clojure-complete",
      dependencyVersion = Just (CEq "0.2.5"),
      dependencyLocations = [],
      dependencyEnvironments = [],
      dependencyTags = M.empty
    }

-- [koan-engine "0.2.5"] {[fresh "1.0.2"] nil},
koanEngine :: Dependency
koanEngine =
  Dependency
    { dependencyType = MavenType,
      dependencyName = "koan-engine",
      dependencyVersion = Just (CEq "0.2.5"),
      dependencyLocations = [],
      dependencyEnvironments = [],
      dependencyTags = M.empty
    }

fresh :: Dependency
fresh =
  Dependency
    { dependencyType = MavenType,
      dependencyName = "fresh",
      dependencyVersion = Just (CEq "1.0.2"),
      dependencyLocations = [],
      dependencyEnvironments = [],
      dependencyTags = M.empty
    }

-- [lein-koan "0.1.5" :scope "test"] nil,
leinKoan :: Dependency
leinKoan =
  Dependency
    { dependencyType = MavenType,
      dependencyName = "lein-koan",
      dependencyVersion = Just (CEq "0.1.5"),
      dependencyLocations = [],
      dependencyEnvironments = [EnvTesting],
      dependencyTags = M.empty
    }

-- [nrepl "0.6.0" :exclusions [[org.clojure/clojure]]] nil,
nrepl :: Dependency
nrepl =
  Dependency
    { dependencyType = MavenType,
      dependencyName = "nrepl",
      dependencyVersion = Just (CEq "0.6.0"),
      dependencyLocations = [],
      dependencyEnvironments = [],
      dependencyTags = M.empty
    }

-- [org.clojure/clojure "1.10.0"]
-- {[org.clojure/core.specs.alpha "0.2.44"] nil,
--  [org.clojure/spec.alpha "0.2.176"] nil}}
clojure :: Dependency
clojure =
  Dependency
    { dependencyType = MavenType,
      dependencyName = "org.clojure:clojure",
      dependencyVersion = Just (CEq "1.10.0"),
      dependencyLocations = [],
      dependencyEnvironments = [],
      dependencyTags = M.empty
    }

clojureSpecsAlpha :: Dependency
clojureSpecsAlpha =
  Dependency
    { dependencyType = MavenType,
      dependencyName = "org.clojure:core.specs.alpha",
      dependencyVersion = Just (CEq "0.2.44"),
      dependencyLocations = [],
      dependencyEnvironments = [],
      dependencyTags = M.empty
    }

clojureSpecAlpha :: Dependency
clojureSpecAlpha =
  Dependency
    { dependencyType = MavenType,
      dependencyName = "org.clojure:spec.alpha",
      dependencyVersion = Just (CEq "0.2.176"),
      dependencyLocations = [],
      dependencyEnvironments = [],
      dependencyTags = M.empty
    }
