File layout:
- README.md
- CONTRIBUTING.md
- docs/
  - .assets/
  - contributors/
  - reference/
    - commands/
    - strategies/
  - guides/
    - migrating-from-v1.md
    - debugging-a-strategy.md
    - integrating-into-ci.md
  - concepts.md
    - FOSSA concepts
      - "Project"
      - "Dependency"
    - CLI concepts
      - "Strategy"
  - user-guide.md

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
    - How FOSSA discovers analysis targets for this strategy
    - How strategies can be specified in configuration
    - How the strategies actually work
      - Running a strategy yourself
    - Verifying that you got the right answer
    - How to configure private dependencies
    - Common issues when debugging
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
