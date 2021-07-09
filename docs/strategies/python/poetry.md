# Poetry

[Poetry](https://python-poetry.org/) is a tool for dependency management and packaging in Python.

## Project Discovery

Find all files named `pyproject.toml` and `poetry.lock`. Pyproject must use poetry for build system.

## Analysis

We parse `pyproject.toml` -- to find direct dependencies and thier [version constraints](https://python-poetry.org/docs/dependency-specification/):

- `[tool.poetry.dependencies]` - production dependencies
- `[tool.poetry.dev-dependencies]` - development dependencies

To infer resolved version of direct and sub dependencies, we parse `poetry.lock` file. In this file we find,

- `[package.dependencies]` - package's dependencies 
- `package.category` - package's environment
- `package.name` - name of the package
- `package.version` - resolved version of the package

For projects, where `poetry.lock` file is not present, we fallback to reporting only direct dependencies parsed from `pyproject.toml`. 

### Poetry Specifics

* For poetry project, build system's `build-backend` must be set to `poetry.core.masonry.api` in `pyproject.toml` 
* All extras specified in `[tool.poetry.extras]` are currently not reported.
* Package names are case insensitive.