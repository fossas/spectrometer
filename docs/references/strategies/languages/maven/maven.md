# Maven Analysis

For maven projects, we offer a more-accurate strategy (mavenplugin), and a strategy with zero requirements (pomxml)

| Strategy    | Direct Deps | Deep Deps | Edges |
| ---         | ---         | ---       | ---   |
| [mavenplugin][mavenplugin] | ✅          | ✅        | ✅    |
| [pomxml][pomxml]           | ✅          | ❌        | ❌    |

[mavenplugin]: maven/mavenplugin.md
[pomxml]: maven/pomxml.md

<!--

TODO: write docs, like Gradle's.

Docs outline:

- Concepts
  - Multi-module reactor builds
  - POMs and POM closures
  - settings.xml
- Discovery
  - Finding pom.xmls
- Tactics
  - dependency:tree
  - POM parsing

 -->