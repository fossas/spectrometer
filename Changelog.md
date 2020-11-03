# v2.4

- Integrates `vpscli scan` as `fossa vps analyze` ([#148](https://github.com/fossas/spectrometer/pull/148))
- Removes `vpscli` binary ([#148](https://github.com/fossas/spectrometer/pull/148))
- Adds support for `--team` and other metadata flags to vps analysis ([#149](https://github.com/fossas/spectrometer/pull/149))
- Adds `fossa vps test` command, analogous to `fossa test` for vps projects ([#150](https://github.com/fossas/spectrometer/pull/150))
- Adds `fossa vps report` command, analogous to `fossa report` for vps projects ([#150](https://github.com/fossas/spectrometer/pull/150))
- Adds support for unpacking of gzipped RPMs ([#154](https://github.com/fossas/spectrometer/pull/154))

# v2.3.2

- Adds `fossa list-targets` to list "analysis-targets" (projects and subprojects) available for analysis ([#140](https://github.com/fossas/spectrometer/pull/140))
- Adds `--filter TARGET` option to `fossa analyze` ([#140](https://github.com/fossas/spectrometer/pull/140))
- Merges the dependencies of `*req*.txt` and `setup.py` files we find ([#140](https://github.com/fossas/spectrometer/pull/140))
- Improves maven project discovery ([#140](https://github.com/fossas/spectrometer/pull/140))
- Fixes gradle wrapper integration ([#140](https://github.com/fossas/spectrometer/pull/140))
- Adds support for "detached HEAD" state in git and svn ([#141](https://github.com/fossas/spectrometer/pull/141))

# v2.3.1

- RPM: Merge spec file results in the analyzer. ([#138](https://github.com/fossas/spectrometer/pull/138))
- Erlang: Resolve rebar3 aliased packages to their true names. ([#139](https://github.com/fossas/spectrometer/pull/139))
- Gradle: Accept and tag all build configuration names. ([#134](https://github.com/fossas/spectrometer/pull/134))

# v2.3.0

- Adds a user guide
- Fixes bug where the rebar3 strategy would incorrectly find dependencies as top-level projects ([#119](https://github.com/fossas/spectrometer/pull/119))
- Fixes various issues in the setup.py parser ([#119](https://github.com/fossas/spectrometer/pull/119))
- Adds an analyzer for haskell projects using cabal-install ([#122](https://github.com/fossas/spectrometer/pull/122))
- Adds an analyzer for PHP projects via composer ([#121](https://github.com/fossas/spectrometer/pull/121))

# v2.2.4

- Adds analyzer for scala via `sbt` ([#54](https://github.com/fossas/spectrometer/pull/54))

# v2.2.1

- Fixes bug where the req.txt strategy would run even when no relevant files were present ([#109](https://github.com/fossas/spectrometer/pull/109))

# v2.2.0

- Fixes `fossa test` and project links for git projects with `https` remotes ([#92](https://github.com/fossas/spectrometer/pull/92))

- Fixes strategy failures related to command-not-found errors ([#106](https://github.com/fossas/spectrometer/pull/106))

- Merges the dependencies of `*req*.txt` files we find ([#102](https://github.com/fossas/spectrometer/pull/102))

- Re-enables deep dependency gathering for golang projects ([#98](https://github.com/fossas/spectrometer/pull/98))

- Fixes directory skipping (e.g., `node_modules`) ([#100](https://github.com/fossas/spectrometer/pull/100))

- Adds CLI-side support for contributor counting ([#94](https://github.com/fossas/spectrometer/pull/94))

- Enables paket.lock strategy ([#107](https://github.com/fossas/spectrometer/pull/107))

- Improves parallelism of strategy discovery ([#93](https://github.com/fossas/spectrometer/pull/93))
