# User Manual

For most users, Spectrometer should work out-of-the-box without any configuration. Just get an API key, run `fossa analyze`, and view your results in the FOSSA web application.

If you haven't read the [Getting Started](../README.md#getting-started) section in the README yet, we highly recommend starting there first.

This manual is organized into four sections:

1. **Concepts** explain the intent and mechanics behind FOSSA concepts (e.g. how FOSSA thinks about "projects" or "dependencies"), including important nuances and subtleties.

2. **Walkthroughs** explain how to accomplish common use cases, including common troubleshooting steps and follow-ups.

3. **Features** explain specific CLI features and how to use them.

4. **References** provide an exhaustive listing and explanation of all CLI functionality.

Every piece of documentation is accessible via hyperlink from this user manual. You should never need to manually explore `docs/` to find the page you need.

## Table of Contents

1. [Concepts](#concepts)
2. [Walkthroughs](#walkthroughs)
3. [Features](#features)
4. [References](#references)

## Concepts

Concept guides explain the nuances behind how basic FOSSA primitives work. If you're looking to accomplish a specific goal, you should probably start with [Walkthroughs](#walkthroughs), but if you come across confusing behavior, understanding Concepts can help you debug what's going on.

- [The FOSSA ontology: Projects, Revisions, Analyses, and Targets](./concepts/ontology.md)
- [What is a Dependency?](./concepts/dependencies.md)
- [Locators, Project Identity, and Dependency Identity](./concepts/locators-and-identity.md)
- [Lifecycle of an Analysis](./concepts/analysis-and-analyzers.md)

## Walkthroughs

Walkthrough guides explain how to accomplish specific tasks. They'll also include common troubleshooting steps and follow-ups, and answer common questions.

- [Integrating a project](./walkthroughs/integrating.md)
- [Excluding test or development dependencies](./walkthroughs/excluding-test-dev-deps.md)
- [Integrating with a custom build tool](./walkthroughs/integrating-with-custom-tools.md)
- [Specifying vendored dependencies](./walkthroughs/specifying-vendored-deps.md)
- [Debugging a strategy](./walkthroughs/debugging-your-integration.md)
- [Upgrading from a VCS Import](./walkthroughs/upgrading-from-vcs.md)

## Features

Feature guides explain how to use specific features. These are most useful if there's a specific feature that a walkthrough has pointed you to.

- [FOSSA project configuration](./features/project-configuration.md)
- [Analysis target configuration](./features/analysis-target-configuration.md)
- [Manual dependencies](./features/manual-dependencies.md)
- [Vendored dependencies](./features/vendored-dependencies.md)

## References

Reference guides provide an exhaustive listing of all CLI functionality. If you can't find documentation on how something works elsewhere, it should be here.

- CLI commands
  - [`fossa init`](./references/subcommands/init.md)
  - [`fossa analyze`](./references/subcommands/analyze.md)
  - [`fossa test`](./references/subcommands/test.md)
  - [`fossa report`](./references/subcommands/report.md)
  - [`fossa list-targets`](./references/subcommands/list-targets.md)
  - [Common flags and options](./references/subcommands/README.md)
- CLI configuration files
  - [`.fossa.yml`](./references/files/fossa-yml.md)
  - [`fossa-deps.{yml,json}`](./references/files/fossa-deps.md)
- [CLI analysis strategies](./references/strategies/README.md)
