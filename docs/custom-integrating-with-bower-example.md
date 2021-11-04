# Custom Integration using `fossa-deps`

With `fossa-deps.{yml, json, yaml}` file, FOSSA CLI can be integrated to support any package manager or custom and non-standard management solution, that is yet to be supported natively by FOSSA CLI. With the fossa-deps file, we can:

- [Include manual dependencies]()
- [Include vendored dependencies]()

## Example with Bower

For an example, we will look at [Bower](). Although, Bower advised now, that users should migrate to npm, or yarn for new javascript based projects. We may have legacy or active projects, which are using bower, and we would like to include them in our reporting.

In general, we can identify list of dependencies from our custom tool (in our case bower), by using configuration used or command provided. In bower case, we can inspect (1) `bower.json` or (2) output of `bower list` command. 

From an example `bower.json` file:

```json
{
  "name": "example-project",
  "authors": [
    "user <user@example.com>"
  ],
  "description": "Example project",
  "main": "main.js",
  "license": "MIT",
  "homepage": "",
  "ignore": [
    "**/.*",
    "node_modules",
    "bower_components",
    "test",
    "tests"
  ],
  "dependencies": {
    "font-awsome": "^5.0.0",
    "jquery": "^3.6.0"
  }
}
```

We can execute `bower list` command, to see the resolved bower dependencies:

```
example-project /Users/example-user/path
├── font-awsome#5.15.4
└── jquery#3.6.0
```

We have two dependencies (1) jquery and (2) font-awsome.

From [manual dependencies]() documentation, we know that `bower` type dependencies are supported type as reference dependency.

We can include all listed dependencies in fossa-deps.json. 

```json
{
  "referenced-dependencies": [
    {
      "type": "bower",
      "name": "jquery",
      "version": "3.6.0"
    },
    {
      "type": "bower",
      "name": "font-awsome",
      "version": "5.15.4"
    }
  ]
}
```

Likewise, you can write short script in your language of choice to translate dependency graph produced by bower to fossa-deps file. 

```python
"""Converts output of `bower list --json` to fossa-deps.json.

It includes, direct and deep dependencies, but *does not*, 
include edges information or filter un-used dependency.

Example:
    bower list --json | python3 bower-to-fossa-deps.py > fossa-deps.json
"""

import sys
import json

sys.setrecursionlimit(10000)


def report(dependency_graph):
    resolved_deps = set()

    if not dependency_graph:
        return resolved_deps

    dependencies = dependency_graph.get("dependencies")
    if not dependencies:
        return resolved_deps

    for _, depValue in dependencies.items():
        meta = depValue.get("pkgMeta", {})
        name, version = meta.get("name"), meta.get("version")

        if (name, version) not in resolved_deps:
            resolved_deps.add((name, version))

        transitive_deps = report(depValue)
        resolved_deps = resolved_deps.union(transitive_deps)

    return resolved_deps

def to_fossa_deps(deps):
    o = {"referenced-dependencies": []}
    for (name, version) in deps:
        o["referenced-dependencies"].append(
            {"name": name, "version": version, "type": "bower"}
        )
    return json.dumps(o)


bower_list_json = json.loads(sys.stdin.read())
resolved_deps = report(bower_list_json)
print(to_fossa_deps(resolved_deps))
```

## Limitation

Please note that, with fossa-deps, we can report dependencies with yet to be supported package manager, but we cannot:

- differentiate between direct and deep dependency
- report edge information between dependencies
