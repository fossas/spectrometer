### FOSSA 1.x to 2.x Migration Workaround Example For Bower

As mentioned in difference between 1.x and 2.x document, FOSSA 2.x does not support bower target discovery, or analysis. But `fossa-deps.{yml, json, yaml}` can be used with FOSSA 2.x to specify dependencies from bower project. 

### Specifying Direct Bower Dependencies

For given bower manifest:

```
➜  Bower cat bower.json
{
  "name": "lll",
  "authors": [
    "Megh <megh@fossa.com>"
  ],
  "description": "",
  "main": "",
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
    "jquery": "^3.6.0"
  }
}
```

You can include all listed dependencies in fossa-deps.json. 

```json
{
  "referenced-dependencies": [
    {
      "type": "bower",
      "name": "jquery",
      "version": "^3.6.0"
    }
  ]
}
```

Note: you may want to resolve your bower project first, to list resolved jquery version instead. It is done by performing `bower install && bower list`.

### Specifying Direct and Transitive Dependencies

If you want to include both direct and transitive dependencies, you can write short script on your language of choice to parse bower project's resolution graph to fossa-deps file. If you are not sure, how to retrieve resolution graph - you can refer CLI 1.x docs of your build manager. Precise analysis procedure, should be listed under analysis section.

Looking from Bower docs for CLI 1.x, we can replicate analysis step to manually create dependency graph in scripting language. 

Here is simple example python script for creating `fossa-deps.json` from bower analysis. We can also add additional filtering to exclude dev dependencies from reporting, if needed.

```python
"""Converts output of `bower list --json` to fossa-deps.json.

It includes, 
* Direct Dependencies
* Transitive Dependencies (also referred to as deep dependencies)

It does not:
- Include edges information
- Remove unused dependency (e.g. dev dependencies)

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
