module Go.GomodSpec (spec) where

import Control.Algebra (run)
import Data.Function ((&))
import Data.Map.Strict qualified as M
import Data.SemVer (version)
import Data.SemVer.Internal (Identifier (..))
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import DepTypes (DepType (GoType), Dependency (..), VerConstraint (CEq))
import Effect.Grapher (direct, evalGrapher)
import Graphing (Graphing (..))
import Strategy.Go.Gomod (Gomod (..), PackageVersion (..), Require (..), buildGraph, gomodParser)
import Strategy.Go.Types (graphingGolang)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (runParser)

-- Helpers for building fixtures.

semver :: Int -> Int -> Int -> PackageVersion
semver x y z = Semantic $ version x y z [] []

dep :: Text -> Text -> Dependency
dep name revision =
  Dependency
    { dependencyType = GoType,
      dependencyName = name,
      dependencyVersion = Just (CEq revision),
      dependencyLocations = [],
      dependencyEnvironments = [],
      dependencyTags = M.empty
    }

-- Fixtures for testing.

trivialGomod :: Gomod
trivialGomod =
  Gomod
    { modName = "github.com/my/package",
      modRequires =
        [ Require "github.com/pkg/one" (semver 1 0 0),
          Require "github.com/pkg/two/v2" (semver 2 0 0),
          Require "github.com/pkg/three/v3" (semver 3 0 0)
        ],
      modReplaces = M.fromList [("github.com/pkg/two/v2", Require "github.com/pkg/overridden" (NonCanonical "overridden"))],
      modLocalReplaces = M.empty,
      modExcludes = []
    }

trivialGraph :: Graphing Dependency
trivialGraph = run . evalGrapher $ do
  direct $ dep "github.com/pkg/one" "v1.0.0"
  direct $ dep "github.com/pkg/overridden" "overridden"
  direct $ dep "github.com/pkg/three/v3" "v3.0.0"

localReplaceGomod :: Gomod
localReplaceGomod =
  Gomod
    { modName = "github.com/my/package",
      modRequires =
        [ Require "github.com/pkg/one" (semver 1 0 0),
          Require "github.com/pkg/two/v2" (semver 2 0 0),
          Require "github.com/pkg/three/v3" (semver 3 0 0)
        ],
      modReplaces = M.empty,
      modLocalReplaces = M.fromList [("github.com/pkg/two/v2", "./foo")],
      modExcludes = []
    }

localReplaceGraph :: Graphing Dependency
localReplaceGraph = run . evalGrapher $ do
  direct $ dep "github.com/pkg/one" "v1.0.0"
  direct $ dep "github.com/pkg/three/v3" "v3.0.0"

edgeCaseGomod :: Gomod
edgeCaseGomod =
  Gomod
    { modName = "test/package",
      modRequires =
        [ Require "repo/name/A" (semver 1 0 0),
          Require "repo/B" (Pseudo "000000000002"),
          Require "repo/C" (semver 1 1 0),
          Require "repo/name/D" (semver 4 0 0),
          Require "repo/E" (Semantic $ version 8 0 0 [] [IText "incompatible"]),
          Require "repo/F_underscore" (semver 1 0 0)
        ],
      modReplaces =
        M.fromList
          [ ("repo/B", Require "alias/repo/B" $ semver 0 1 0),
            ("repo/C", Require "alias/repo/C" $ Pseudo "000000000003"),
            ("repo/E", Require "alias/repo/E" $ Pseudo "000000000005"),
            ("repo/F_underscore", Require "repo/F_underscore" $ semver 2 0 0)
          ],
      modLocalReplaces =
        M.fromList
          [ ("foo", "../foo"),
            ("bar", "/foo/bar/baz")
          ],
      modExcludes =
        [ Require "repo/B" (semver 0 9 0),
          Require "repo/C" (semver 1 0 0),
          Require "repo/name/D" (semver 3 0 0)
        ]
    }

versionsGomod :: Gomod
versionsGomod =
  Gomod
    { modName = "github.com/hashicorp/nomad",
      modRequires =
        [ Require
            { reqPackage = "github.com/containerd/go-cni",
              reqVersion = Pseudo "d20b7eebc7ee"
            },
          Require
            { reqPackage = "gopkg.in/tomb.v2",
              reqVersion = Pseudo "14b3d72120e8"
            },
          Require
            { reqPackage = "github.com/docker/docker",
              reqVersion = Pseudo "7f8b4b621b5d"
            },
          Require
            { reqPackage = "github.com/containernetworking/plugins",
              reqVersion = Pseudo "2d6d46d308b2"
            },
          Require
            { reqPackage = "github.com/ryanuber/columnize",
              reqVersion = Pseudo "abc90934186a"
            },
          Require
            { reqPackage = "github.com/opencontainers/runc",
              reqVersion = Semantic (version 1 0 0 [IText "rc92"] [])
            },
          Require
            { reqPackage = "honnef.co/go/tools",
              reqVersion = Semantic (version 0 0 1 [INum 2020, INum 1, INum 4] [])
            },
          Require
            { reqPackage = "hub.com/docker/distribution",
              reqVersion = Semantic (version 2 7 1 [] [IText "incompatible"])
            }
        ],
      modReplaces = mempty,
      modLocalReplaces = mempty,
      modExcludes = []
    }

versionsGraph :: Graphing Dependency
versionsGraph = run . evalGrapher $ do
  direct $ dep "github.com/containerd/go-cni" "d20b7eebc7ee"
  direct $ dep "gopkg.in/tomb.v2" "14b3d72120e8"
  direct $ dep "github.com/docker/docker" "7f8b4b621b5d"
  direct $ dep "github.com/containernetworking/plugins" "2d6d46d308b2"
  direct $ dep "github.com/ryanuber/columnize" "abc90934186a"
  direct $ dep "github.com/opencontainers/runc" "v1.0.0-rc92"
  direct $ dep "honnef.co/go/tools" "v0.0.1-2020.1.4"
  direct $ dep "hub.com/docker/distribution" "v2.7.1"

-- Actual test logic.

spec :: Spec
spec = do
  spec_buildGraph
  spec_parse

spec_buildGraph :: Spec
spec_buildGraph =
  describe "buildGraph" $ do
    it "constructs a trivial graph" $ do
      let result = buildGraph trivialGomod & graphingGolang & run
      result `shouldBe` trivialGraph

    it "renders different kinds of versions correctly" $ do
      let result = buildGraph versionsGomod & graphingGolang & run
      result `shouldBe` versionsGraph

    it "does not include locally replaced modules" $ do
      let result = buildGraph localReplaceGomod & graphingGolang & run
      result `shouldBe` localReplaceGraph

spec_parse :: Spec
spec_parse = do
  trivialInput <- runIO (TIO.readFile "test/Go/testdata/go.mod.trivial")
  edgecaseInput <- runIO (TIO.readFile "test/Go/testdata/go.mod.edgecases")
  versionsInput <- runIO (TIO.readFile "test/Go/testdata/go.mod.versions")

  describe "gomod parser" $ do
    it "parses a trivial example" $ do
      runParser gomodParser "" trivialInput `shouldParse` trivialGomod

    it "parses each edge case" $ do
      runParser gomodParser "" edgecaseInput `shouldParse` edgeCaseGomod

    it "parses different kinds of module versions" $ do
      runParser gomodParser "" versionsInput `shouldParse` versionsGomod
