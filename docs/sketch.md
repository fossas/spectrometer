File layout:
- README.md // Leo
- CONTRIBUTING.md // Leo (should be real quick, mostly points to developer docs)
- docs/
  - .assets/
  - contributors/
    - (Developer docs in here, move from devdocs/) // Engineers
  - reference/
    - commands/ // Engineers
      - (CLI command and flag references in here)
      - `.fossa.yml` file reference
      - `fossa-deps.json` file reference
    - strategies/ // Engineers
      - (Strategy references in here)
  - guides/ (Some of these have duplicate info which is okay - guides are denormalized based on anticipated reader needs)
    - migrating-from-v1.md // Engineers
    - debugging-an-analysis.md // Leo
      - General outline for how to debug a strategy
        - Manually verifying that your build works
        - Figuring out what targets are running and finding the docs for those targets
        - "My analysis has extra stuff!"
        - "My analysis is missing stuff!"
      - Generating a bug report (making debug logs)
    - configuring-an-analysis.md // Leo
      - How to debug an analysis in general
        - Filtering out extra analysis targets (e.g. build tools, test targets, etc.)
        - Using `list-targets`
      - Manually adding dependencies
        - Vendored dependencies
    - integrating-into-ci.md // Unsure
    - adding-dependencies-manually.md
      - fossa-deps
      - Vendored dependencies
  - concepts.md // Leo
    - FOSSA concepts
      - "Project"
      - "Dependency"
    - CLI concepts
      - "Strategy"
      - "Tactic"
  - user-guide.md // Leo
  - faq.md // Zach

README.md:
- Intro
- Badges
- Quick start
  - Quick install
  - Code snippet
- Supported languages
- Further reading (documentation)
- Contributing
- License

----

Topics:

- Concepts
  - FOSSA
    - Project
    - Dependency
  - CLI
    - Strategy
      - Strategy lookup table (language, system deps, monorepo tools, etc.)
  - Dependency resolution
    - Why are subgraphs not good enough?
    - Static vs. dynamic analysis
  - Dependency types
    - Package managed
    - Manual
    - Archive upload
    - (Experimental) Container
    - (Experimental) Monorepo

- Guides
  - Integrating into CI
    - Getting dependencies into FOSSA CLI
    - Verifying that your integration works correctly
    - Configuring private registries
    - Getting feedback into CI
      - Integrating with GitHub (via PR status check)
      - Integrating with CI (via `fossa test`)
      - Generating reports (via `fossa report`)
  - Configuring the CLI
    - Common options you'll want to configure
    - Example configuration
  - Debugging the CLI
    - How strategies usually work
      - Reference link for strategies
    - Debug mode
    - Debug logs
    - Common error messages and what they mean / how you should try to resolve them
  - Common debugging issues
    - How to forcibly add your own analysis targets
    - How to filter out analysis targets
  - Migrating from v1
    - Flag changes
    - Subcommand changes
    - Configuration file changes

- Reference
  - FOR EACH STRATEGY:
    - Language concepts glossary
      - Examples: Python dependencies, what `go.sum` means, etc.
    - FOSSA concepts glossary
      - What does "dependency" mean for this strategy?
    - How FOSSA discovers analysis targets for this strategy
    - How targets can be specified in configuration
      - Target type and path format
    - How the strategy actually works
      - Running a strategy yourself manually (to verify that your build tool works)
      - How this strategy falls back between tactics
      - Limitations of each tactic
    - Manually verifying that you got the right answer
    - How to configure private dependencies
    - Common issues when debugging
    - Alternative tactics we haven't implemented yet (and whether we plan to, or their limitations)
  - Glossary

# FOSSA CLI
Overview blurb

Quick start

Supported language matrix
- Columns
  - Static analysis
    - Source code only
  - Dynamic analysis
    - Requires CI
  - Integration methods
    - List strategies e.g. `node_modules, package.json, package-lock.json`, etc.

Docs

- Concepts
  - FOSSA projects
  - How FOSSA thinks about dependencies
  - Analyzers and how they work (overview of ideas)
- Guides
  - Installing FOSSA CLI
  - Manually adding dependencies (what to do when `fossa` doesn't work)
  - Scripting around FOSSA CLI
    - Inputs - getting deps in
    - Outputs - getting info out + API
  - Debugging analyzers
- References
  - CLI command references
    - `fossa analyze`
    - `fossa ...`
  - Language references
    - (One doc per strategy)
