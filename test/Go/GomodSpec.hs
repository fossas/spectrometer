module Go.GomodSpec (spec) where

import Control.Algebra (run)
import Data.Function ((&))
import qualified Data.Map.Strict as M
import Data.SemVer (Version, version)
import Data.SemVer.Internal (Identifier (..))
import qualified Data.Text.IO as TIO
import DepTypes (DepType (GoType), Dependency (..), VerConstraint (CEq))
import Effect.Grapher (direct, evalGrapher)
import Graphing (Graphing (..))
import Strategy.Go.Gomod (Gomod (..), PackageVersion (..), Require (..), buildGraph, gomodParser)
import Strategy.Go.Types (graphingGolang)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (runParser)

-- Fixtures for testing.

version' :: Int -> Int -> Int -> Version
version' x y z = version x y z [] []

trivialGomod :: Gomod
trivialGomod =
  Gomod
    { modName = "github.com/my/package",
      modRequires =
        [ Require "github.com/pkg/one" (Semantic $ version' 1 0 0),
          Require "github.com/pkg/two/v2" (Semantic $ version' 2 0 0),
          Require "github.com/pkg/three/v3" (Semantic $ version' 3 0 0)
        ],
      modReplaces = M.fromList [("github.com/pkg/two/v2", Require "github.com/pkg/overridden" (NonCanonical "overridden"))],
      modLocalReplaces = M.empty,
      modExcludes = []
    }

trivialGraph :: Graphing Dependency
trivialGraph = run . evalGrapher $ do
  direct $
    Dependency
      { dependencyType = GoType,
        dependencyName = "github.com/pkg/one",
        dependencyVersion = Just (CEq "v1.0.0"),
        dependencyLocations = [],
        dependencyEnvironments = [],
        dependencyTags = M.empty
      }
  direct $
    Dependency
      { dependencyType = GoType,
        dependencyName = "github.com/pkg/overridden",
        dependencyVersion = Just (CEq "overridden"),
        dependencyLocations = [],
        dependencyEnvironments = [],
        dependencyTags = M.empty
      }
  direct $
    Dependency
      { dependencyType = GoType,
        dependencyName = "github.com/pkg/three/v3",
        dependencyVersion = Just (CEq "v3.0.0"),
        dependencyLocations = [],
        dependencyEnvironments = [],
        dependencyTags = M.empty
      }

edgeCaseGomod :: Gomod
edgeCaseGomod =
  Gomod
    { modName = "test/package",
      modRequires =
        [ Require "repo/name/A" (Semantic $ version' 1 0 0),
          Require "repo/B" (Pseudo "000000000002"),
          Require "repo/C" (Semantic $ version' 1 1 0),
          Require "repo/name/D" (Semantic $ version' 4 0 0),
          Require "repo/E" (Semantic $ version 8 0 0 [] [IText "incompatible"]),
          Require "repo/F_underscore" (Semantic $ version' 1 0 0)
        ],
      modReplaces =
        M.fromList
          [ ("repo/B", Require "alias/repo/B" $ Semantic $ version' 0 1 0),
            ("repo/C", Require "alias/repo/C" $ Pseudo "000000000003"),
            ("repo/E", Require "alias/repo/E" $ Pseudo "000000000005"),
            ("repo/F_underscore", Require "repo/F_underscore" $ Semantic $ version' 2 0 0)
          ],
      modLocalReplaces =
        M.fromList
          [ ("foo", "../foo"),
            ("bar", "/foo/bar/baz")
          ],
      modExcludes =
        [ Require "repo/B" (Semantic $ version' 0 9 0),
          Require "repo/C" (Semantic $ version' 1 0 0),
          Require "repo/name/D" (Semantic $ version' 3 0 0)
        ]
    }


-- Test cases
-- - Incompatible tag
-- - Pseudo-version identification

-- nomadGomod :: Gomod
-- nomadGomod =
--   Gomod
--     { modName = "github.com/hashicorp/nomad",
--       modRequires =
--         [ Require {reqPackage = "cloud.google.com/go/storage", reqVersion = "v1.0.0"},
--           Require {reqPackage = "github.com/Azure/go-autorest/autorest", reqVersion = "v0.11.4"},
--           Require {reqPackage = "github.com/Azure/go-autorest/autorest/azure/auth", reqVersion = "v0.5.1"},
--           Require {reqPackage = "github.com/LK4D4/joincontext", reqVersion = "v0.0.0-20171026170139-1724345da6d5"},
--           Require {reqPackage = "github.com/Microsoft/go-winio", reqVersion = "v0.4.15-0.20200113171025-3fe6c5262873"},
--           Require {reqPackage = "github.com/Microsoft/hcsshim", reqVersion = "v0.8.8-0.20200312192636-fd0797d766b1"},
--           Require {reqPackage = "github.com/NVIDIA/gpu-monitoring-tools", reqVersion = "v0.0.0-20180829222009-86f2a9fac6c5"},
--           Require {reqPackage = "github.com/NYTimes/gziphandler", reqVersion = "v1.0.1"},
--           Require {reqPackage = "github.com/armon/circbuf", reqVersion = "v0.0.0-20150827004946-bbbad097214e"},
--           Require {reqPackage = "github.com/armon/go-metrics", reqVersion = "v0.3.4"},
--           Require {reqPackage = "github.com/aws/aws-sdk-go", reqVersion = "v1.35.3"},
--           Require {reqPackage = "github.com/boltdb/bolt", reqVersion = "v1.3.1"},
--           Require {reqPackage = "github.com/container-storage-interface/spec", reqVersion = "v1.4.0"},
--           Require {reqPackage = "github.com/containerd/go-cni", reqVersion = "v0.0.0-20190904155053-d20b7eebc7ee"},
--           Require {reqPackage = "github.com/containernetworking/cni", reqVersion = "v0.7.2-0.20190612152420-dc953e2fd91f"},
--           Require {reqPackage = "github.com/containernetworking/plugins", reqVersion = "v0.7.3-0.20190501191748-2d6d46d308b2"},
--           Require {reqPackage = "github.com/coreos/go-iptables", reqVersion = "v0.4.3-0.20190724151750-969b135e941d"},
--           Require {reqPackage = "github.com/coreos/go-semver", reqVersion = "v0.3.0"},
--           Require {reqPackage = "github.com/cyphar/filepath-securejoin", reqVersion = "v0.2.3-0.20190205144030-7efe413b52e1"},
--           Require {reqPackage = "github.com/docker/cli", reqVersion = "v0.0.0-20200303215952-eb310fca4956"},
--           Require {reqPackage = "github.com/docker/distribution", reqVersion = "v2.7.1+incompatible"},
--           Require {reqPackage = "github.com/docker/docker", reqVersion = "v17.12.0-ce-rc1.0.20200330121334-7f8b4b621b5d+incompatible"},
--           Require {reqPackage = "github.com/docker/docker-credential-helpers", reqVersion = "v0.6.2-0.20180719074751-73e5f5dbfea3"},
--           Require {reqPackage = "github.com/docker/go-units", reqVersion = "v0.4.0"},
--           Require {reqPackage = "github.com/docker/libnetwork", reqVersion = "v0.8.0-dev.2.0.20200612180813-9e99af28df21"},
--           Require {reqPackage = "github.com/dustin/go-humanize", reqVersion = "v1.0.0"},
--           Require {reqPackage = "github.com/elazarl/go-bindata-assetfs", reqVersion = "v1.0.1-0.20200509193318-234c15e7648f"},
--           Require {reqPackage = "github.com/fatih/color", reqVersion = "v1.9.0"},
--           Require {reqPackage = "github.com/fsnotify/fsnotify", reqVersion = "v1.4.9"},
--           Require {reqPackage = "github.com/fsouza/go-dockerclient", reqVersion = "v1.6.5"},
--           Require {reqPackage = "github.com/golang/protobuf", reqVersion = "v1.4.2"},
--           Require {reqPackage = "github.com/golang/snappy", reqVersion = "v0.0.1"},
--           Require {reqPackage = "github.com/google/go-cmp", reqVersion = "v0.4.0"},
--           Require {reqPackage = "github.com/gorilla/websocket", reqVersion = "v1.4.2"},
--           Require {reqPackage = "github.com/grpc-ecosystem/go-grpc-middleware", reqVersion = "v1.2.1-0.20200228141219-3ce3d519df39"},
--           Require {reqPackage = "github.com/hashicorp/consul", reqVersion = "v1.7.8"},
--           Require {reqPackage = "github.com/hashicorp/consul-template", reqVersion = "v0.25.1"},
--           Require {reqPackage = "github.com/hashicorp/consul/api", reqVersion = "v1.8.1"},
--           Require {reqPackage = "github.com/hashicorp/consul/sdk", reqVersion = "v0.7.0"},
--           Require {reqPackage = "github.com/hashicorp/cronexpr", reqVersion = "v1.1.1"},
--           Require {reqPackage = "github.com/hashicorp/go-checkpoint", reqVersion = "v0.0.0-20171009173528-1545e56e46de"},
--           Require {reqPackage = "github.com/hashicorp/go-cleanhttp", reqVersion = "v0.5.1"},
--           Require {reqPackage = "github.com/hashicorp/go-connlimit", reqVersion = "v0.3.0"},
--           Require {reqPackage = "github.com/hashicorp/go-cty-funcs", reqVersion = "v0.0.0-20200930094925-2721b1e36840"},
--           Require {reqPackage = "github.com/hashicorp/go-discover", reqVersion = "v0.0.0-20200812215701-c4b85f6ed31f"},
--           Require {reqPackage = "github.com/hashicorp/go-envparse", reqVersion = "v0.0.0-20180119215841-310ca1881b22"},
--           Require {reqPackage = "github.com/hashicorp/go-getter", reqVersion = "v1.5.2"},
--           Require {reqPackage = "github.com/hashicorp/go-hclog", reqVersion = "v0.12.0"},
--           Require {reqPackage = "github.com/hashicorp/go-immutable-radix", reqVersion = "v1.3.0"},
--           Require {reqPackage = "github.com/hashicorp/go-memdb", reqVersion = "v1.3.0"},
--           Require {reqPackage = "github.com/hashicorp/go-msgpack", reqVersion = "v1.1.5"},
--           Require {reqPackage = "github.com/hashicorp/go-multierror", reqVersion = "v1.1.0"},
--           Require {reqPackage = "github.com/hashicorp/go-plugin", reqVersion = "v1.0.2-0.20191004171845-809113480b55"},
--           Require {reqPackage = "github.com/hashicorp/go-sockaddr", reqVersion = "v1.0.2"},
--           Require {reqPackage = "github.com/hashicorp/go-syslog", reqVersion = "v1.0.0"},
--           Require {reqPackage = "github.com/hashicorp/go-uuid", reqVersion = "v1.0.1"},
--           Require {reqPackage = "github.com/hashicorp/go-version", reqVersion = "v1.2.1-0.20191009193637-2046c9d0f0b0"},
--           Require {reqPackage = "github.com/hashicorp/golang-lru", reqVersion = "v0.5.4"},
--           Require {reqPackage = "github.com/hashicorp/hcl", reqVersion = "v1.0.1-0.20201016140508-a07e7d50bbee"},
--           Require {reqPackage = "github.com/hashicorp/hcl/v2", reqVersion = "v2.7.1-0.20210129140708-3000d85e32a9"},
--           Require {reqPackage = "github.com/hashicorp/logutils", reqVersion = "v1.0.0"},
--           Require {reqPackage = "github.com/hashicorp/memberlist", reqVersion = "v0.2.2"},
--           Require {reqPackage = "github.com/hashicorp/net-rpc-msgpackrpc", reqVersion = "v0.0.0-20151116020338-a14192a58a69"},
--           Require {reqPackage = "github.com/hashicorp/nomad/api", reqVersion = "v0.0.0-20200529203653-c4416b26d3eb"},
--           Require {reqPackage = "github.com/hashicorp/raft", reqVersion = "v1.1.3-0.20200211192230-365023de17e6"},
--           Require {reqPackage = "github.com/hashicorp/raft-boltdb", reqVersion = "v0.0.0-20171010151810-6e5ba93211ea"},
--           Require {reqPackage = "github.com/hashicorp/serf", reqVersion = "v0.9.5"},
--           Require {reqPackage = "github.com/hashicorp/vault/api", reqVersion = "v1.0.5-0.20190730042357-746c0b111519"},
--           Require {reqPackage = "github.com/hashicorp/vault/sdk", reqVersion = "v0.1.14-0.20190730042320-0dc007d98cc8"},
--           Require {reqPackage = "github.com/hashicorp/yamux", reqVersion = "v0.0.0-20181012175058-2f1d1f20f75d"},
--           Require {reqPackage = "github.com/hpcloud/tail", reqVersion = "v1.0.1-0.20170814160653-37f427138745"},
--           Require {reqPackage = "github.com/ishidawataru/sctp", reqVersion = "v0.0.0-20191218070446-00ab2ac2db07"},
--           Require {reqPackage = "github.com/joyent/triton-go", reqVersion = "v0.0.0-20190112182421-51ffac552869"},
--           Require {reqPackage = "github.com/kr/pretty", reqVersion = "v0.2.0"},
--           Require {reqPackage = "github.com/kr/pty", reqVersion = "v1.1.5"},
--           Require {reqPackage = "github.com/kr/text", reqVersion = "v0.2.0"},
--           Require {reqPackage = "github.com/mattn/go-colorable", reqVersion = "v0.1.6"},
--           Require {reqPackage = "github.com/mitchellh/cli", reqVersion = "v1.1.0"},
--           Require {reqPackage = "github.com/mitchellh/colorstring", reqVersion = "v0.0.0-20150917214807-8631ce90f286"},
--           Require {reqPackage = "github.com/mitchellh/copystructure", reqVersion = "v1.0.0"},
--           Require {reqPackage = "github.com/mitchellh/go-ps", reqVersion = "v0.0.0-20190716172923-621e5597135b"},
--           Require {reqPackage = "github.com/mitchellh/go-testing-interface", reqVersion = "v1.0.3"},
--           Require {reqPackage = "github.com/mitchellh/hashstructure", reqVersion = "v1.0.0"},
--           Require {reqPackage = "github.com/mitchellh/mapstructure", reqVersion = "v1.3.3"},
--           Require {reqPackage = "github.com/mitchellh/reflectwalk", reqVersion = "v1.0.1"},
--           Require {reqPackage = "github.com/niemeyer/pretty", reqVersion = "v0.0.0-20200227124842-a10e7caefd8e"},
--           Require {reqPackage = "github.com/oklog/run", reqVersion = "v1.0.1-0.20180308005104-6934b124db28"},
--           Require {reqPackage = "github.com/onsi/gomega", reqVersion = "v1.9.0"},
--           Require {reqPackage = "github.com/opencontainers/runc", reqVersion = "v1.0.0-rc92"},
--           Require {reqPackage = "github.com/pkg/errors", reqVersion = "v0.9.1"},
--           Require {reqPackage = "github.com/posener/complete", reqVersion = "v1.2.3"},
--           Require {reqPackage = "github.com/prometheus/client_golang", reqVersion = "v1.4.0"},
--           Require {reqPackage = "github.com/prometheus/common", reqVersion = "v0.9.1"},
--           Require {reqPackage = "github.com/rs/cors", reqVersion = "v1.7.0"},
--           Require {reqPackage = "github.com/ryanuber/columnize", reqVersion = "v2.1.1-0.20170703205827-abc90934186a+incompatible"},
--           Require {reqPackage = "github.com/ryanuber/go-glob", reqVersion = "v1.0.0"},
--           Require {reqPackage = "github.com/sean-/seed", reqVersion = "v0.0.0-20170313163322-e2103e2c3529"},
--           Require {reqPackage = "github.com/seccomp/libseccomp-golang", reqVersion = "v0.9.2-0.20200314001724-bdab42bd5128"},
--           Require {reqPackage = "github.com/shirou/gopsutil", reqVersion = "v2.20.9+incompatible"},
--           Require {reqPackage = "github.com/skratchdot/open-golang", reqVersion = "v0.0.0-20160302144031-75fb7ed4208c"},
--           Require {reqPackage = "github.com/stretchr/objx", reqVersion = "v0.2.0"},
--           Require {reqPackage = "github.com/stretchr/testify", reqVersion = "v1.6.1"},
--           Require {reqPackage = "github.com/syndtr/gocapability", reqVersion = "v0.0.0-20180916011248-d98352740cb2"},
--           Require {reqPackage = "github.com/zclconf/go-cty", reqVersion = "v1.4.1"},
--           Require {reqPackage = "github.com/zclconf/go-cty-yaml", reqVersion = "v1.0.2"},
--           Require {reqPackage = "go.opencensus.io", reqVersion = "v0.22.1-0.20190713072201-b4a14686f0a9"},
--           Require {reqPackage = "golang.org/x/crypto", reqVersion = "v0.0.0-20200622213623-75b288015ac9"},
--           Require {reqPackage = "golang.org/x/exp", reqVersion = "v0.0.0-20191030013958-a1ab85dbe136"},
--           Require {reqPackage = "golang.org/x/lint", reqVersion = "v0.0.0-20191125180803-fdd1cda4f05f"},
--           Require {reqPackage = "golang.org/x/mod", reqVersion = "v0.3.0"},
--           Require {reqPackage = "golang.org/x/net", reqVersion = "v0.0.0-20200602114024-627f9648deb9"},
--           Require {reqPackage = "golang.org/x/sync", reqVersion = "v0.0.0-20200317015054-43a5402ce75a"},
--           Require {reqPackage = "golang.org/x/sys", reqVersion = "v0.0.0-20200728102440-3e129f6d46b1"},
--           Require {reqPackage = "golang.org/x/text", reqVersion = "v0.3.3-0.20200306154105-06d492aade88"},
--           Require {reqPackage = "golang.org/x/time", reqVersion = "v0.0.0-20191024005414-555d28b269f0"},
--           Require {reqPackage = "golang.org/x/tools", reqVersion = "v0.0.0-20200522201501-cb1345f3a375"},
--           Require {reqPackage = "google.golang.org/api", reqVersion = "v0.13.0"},
--           Require {reqPackage = "google.golang.org/genproto", reqVersion = "v0.0.0-20200302123026-7795fca6ccb1"},
--           Require {reqPackage = "google.golang.org/grpc", reqVersion = "v1.27.1"},
--           Require {reqPackage = "gopkg.in/check.v1", reqVersion = "v1.0.0-20200227125254-8fa46927fb4f"},
--           Require {reqPackage = "gopkg.in/square/go-jose.v2", reqVersion = "v2.4.1"},
--           Require {reqPackage = "gopkg.in/tomb.v1", reqVersion = "v1.0.0-20141024135613-dd632973f1e7"},
--           Require {reqPackage = "gopkg.in/tomb.v2", reqVersion = "v2.0.0-20140626144623-14b3d72120e8"},
--           Require {reqPackage = "gopkg.in/yaml.v2", reqVersion = "v2.3.0"},
--           Require {reqPackage = "gotest.tools/v3", reqVersion = "v3.0.2"},
--           Require {reqPackage = "honnef.co/go/tools", reqVersion = "v0.0.1-2020.1.4"}
--         ],
--       modReplaces =
--         M.fromList
--           [ ("github.com/Microsoft/go-winio", Require {reqPackage = "github.com/endocrimes/go-winio", reqVersion = "v0.4.13-0.20190628114223-fb47a8b41948"}),
--             ("github.com/NYTimes/gziphandler", Require {reqPackage = "github.com/NYTimes/gziphandler", reqVersion = "v1.0.0"}),
--             ("github.com/apparentlymart/go-textseg/v12", Require {reqPackage = "github.com/apparentlymart/go-textseg/v12", reqVersion = "v12.0.0"}),
--             ("github.com/godbus/dbus", Require {reqPackage = "github.com/godbus/dbus", reqVersion = "v5.0.1+incompatible"}),
--             ("github.com/golang/protobuf", Require {reqPackage = "github.com/golang/protobuf", reqVersion = "v1.3.4"}),
--             ("github.com/hashicorp/go-discover", Require {reqPackage = "github.com/hashicorp/go-discover", reqVersion = "v0.0.0-20200812215701-c4b85f6ed31f"}),
--             ("github.com/hashicorp/hcl", Require {reqPackage = "github.com/hashicorp/hcl", reqVersion = "v1.0.1-0.20201016140508-a07e7d50bbee"}),
--             ("github.com/kr/pty", Require {reqPackage = "github.com/kr/pty", reqVersion = "v1.1.5"})
--           ],
--       modLocalReplaces =
--         M.fromList
--           [ ("github.com/hashicorp/nomad/api", "./api")
--           ],
--       modExcludes = []
--     }

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

    -- See https://golang.org/ref/mod#glos-pseudo-version
    -- it "resolves pseudo-versions into commit hashes" $ do
    --   let nomadGraph = buildGraph nomadGomod & graphingGolang & run
    --   -- Get a specific known dependency pseudo-version.
    --   let directs = graphingDirect nomadGraph
    --   let hcsshim = Set.filter ((== "github.com/Microsoft/hcsshim") . dependencyName) directs
    --   Set.size hcsshim `shouldBe` 1
    --   let dep = Set.elemAt 0 hcsshim
    --   -- Check that the pseudo-version is resolved to a commit hash.
    --   dependencyVersion dep `shouldBe` Just (CEq "fd0797d766b1")

spec_parse :: Spec
spec_parse = do
  trivialInput <- runIO (TIO.readFile "test/Go/testdata/go.mod.trivial")
  edgecaseInput <- runIO (TIO.readFile "test/Go/testdata/go.mod.edgecases")
  -- nomadInput <- runIO (TIO.readFile "test/Go/testdata/go.mod.nomad")

  describe "gomod parser" $ do
    it "parses a trivial example" $ do
      runParser gomodParser "" trivialInput `shouldParse` trivialGomod

    it "parses each edge case" $ do
      runParser gomodParser "" edgecaseInput `shouldParse` edgeCaseGomod

    -- it "parses an actual Nomad go.mod file" $ do
    --   runParser gomodParser "" nomadInput `shouldParse` nomadGomod
