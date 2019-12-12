module Go.GlideLockTest
  ( spec_analyze
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.Text.IO as TIO
import           Polysemy
import           Polysemy.Input
import           Text.Megaparsec

import           Effect.GraphBuilder
import qualified Graph as G
import           Strategy.Go.GlideLock

import Test.Hspec.Megaparsec
import Test.Tasty.Hspec

expected :: G.Graph
expected = run . evalGraphBuilder G.empty $ do
  ref1 <- addNode (G.Dependency
                        { dependencyType = G.GoType
                        , dependencyName = "github.com/pkg/one"
                        , dependencyVersion = Just (G.CEq "100")
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        })
  ref2 <- addNode (G.Dependency
                        { dependencyType = G.GoType
                        , dependencyName = "github.com/pkg/three/v3"
                        , dependencyVersion = Just (G.CEq "300")
                        , dependencyLocations = []
                        , dependencyTags = M.empty
                        })
  addDirect ref1
  addDirect ref2

glideLockfile :: GlideLockfile
glideLockfile = 
  GlideLockfile { hash = 123
  , updated = "now"
  , imports = 
    [ GlideDep 
        { depName = "github.com/pkg/one"
        , depVersion = 100
        , depRepo = Just "testRepo"
    }
    , GlideDep 
        { depName = "github.com/pkg/three/v3"
        , depVersion = 300
        , depRepo = Just "testRepo"
    }
  ]
  }

spec_analyze :: Spec
spec_analyze = do
  testFile <- runIO (TIO.readFile "test/Go/testdata/glide.lock")

  describe "glide lock analyzer" $
    it "produces the expected output" $ do
      let result = analyze
            & runInputConst @GlideLockfile glideLockfile
            & run
      result `shouldBe` expected
