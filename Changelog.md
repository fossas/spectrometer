# v2.3.1

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
