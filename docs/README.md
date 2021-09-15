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

- [The FOSSA Ontology: Projects, Revisions, Analyses, and Targets](TODO)
- [Anatomy of a Dependency](TODO)
- [Locators, Project Identity, and Dependency Identity](TODO)
- [Lifecycle of an Analysis](TODO)

## Walkthroughs

Walkthrough guides explain how to accomplish specific tasks. They'll also include common troubleshooting steps and follow-ups, and answer common questions.

- Integrating a project

## Features

Feature guides explain how to use specific features. These are most useful if there's a specific feature that a walkthrough has pointed you to.

- Configuring the CLI's project
- Analysis target configuration

## References

Reference guides provide an exhaustive listing of all CLI functionality. If you can't find documentation on how something works elsewhere, it should be here.

- CLI commands
  - [`fossa init`]()
  - [`fossa analyze`]()
  - [`fossa test`]()
  - [`fossa report`]()
  - [`fossa list-targets`]()
- CLI configuration files
  - [`.fossa.yml`]()
  - [`fossa-deps.{yml,json}`]()
- [CLI analysis strategies](./references/strategies/README.md)
