<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Analysis Strategies](#analysis-strategies)
    - [Python](#python)

<!-- markdown-toc end -->

# Analysis Strategies

TODO: Common tags
TODO: what the columns mean
TODO: pinned versions column?

## Python

| Strategy                       | Direct Deps | Deep Deps | Edges | Tags                        |
| ---                            | ---         | ---       | ---   | ---                         |
| [pipenv][pipenv]               | Yes         | Yes       | Yes   | Environment                 |
| [pipfile][pipenv]              | Yes         | Yes       | No    | Environment                 |
| [requirements.txt][setuptools] | Yes         | No        | No    | PEP-508 Environment Markers |
| [setup.py][setuptools]         | Yes         | No        | No    | PEP-508 Environment Markers |
| [piplist][piplist]             | Maybe       | Maybe     | No    |                             |

[pipenv]: strategies/python/pipenv.md
[setuptools]: strategies/python/setuptools.md
[piplist]: strategies/python/piplist.md
