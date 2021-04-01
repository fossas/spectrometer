# Golang Analysis

Historically, the Golang buildtool ecosystem has been very fragmented: many
tools like dep, Glide, Govendor, etc have tried to address issues around build
reproducibility, versioning and vendoring of dependencies.

As of Golang 1.11, we have first-class support for "modules", which solves
issues previously seen around versioning and build reproducibility.

Because modules address those issues so well, the other tooling has largely gone
into maintenance mode, with the notable exception of dep. As such, golang
analysis in spectrometer primarily targets Golang 1.11+ modules and dep. Support
for Glide is also included, because it's still commonly used.

| Strategy                             | Direct Deps | Deep Deps | Edges |
| ---                                  | ---         | ---       | ---   |
| [golist][gomodules] (golang modules) | ✅          | ✅        | 🔶    |
| [gomod][gomodules] (golang modules)  | ✅          | ❌        | 🔶    |
| [gopkglock][godep] (dep)             | ✅          | ✅        | 🔶    |
| [gopkgtoml][godep] (dep)             | ✅          | 🔶        | 🔶    |
| [glide][glide]                       | ✅          | ✅        | ❌    |

[gomodules]: golang/gomodules.md
[godep]: golang/godep.md
[glide]: golang/glide.md

## 🔶 Edges and deep dependencies

Most strategies (except for gomod, where it would be redundant -- golist
supersedes gomod) use `go list -json all` to hydrate edges and transitive
dependencies. Package imports are recursively traversed, ignoring `Standard`
(system) packages.

`go list` behaves slightly differently depending on the context:

- in a gomodules project, packages can contain a `Module` field that contains a
pinned `Version`. The version will otherwise be unspecified
- `go list` includes vendored packages in both gomodules and non-gomodules projects.

----

TODO:

Documenting

- Concepts for Go
  - Packages (compilation unit) vs modules (download unit)
  - Import paths
  - History of legacy package managers
- How do we pick strategies?
  - How do you tell which strategy was picked?
- What are the strategies? How do we debug them?
  - go list
    - what command gets run? how do we parse?
  - go mod parsing
    - TODO: go.mod transitive closure
    - semantics of go.mod file
      - go.mod does not contain all transitive deps, need to compute transitive closure
    - semantics of go.sum file
      - go.sum contains extra deps
