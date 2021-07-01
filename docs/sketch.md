documentation sketch


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
