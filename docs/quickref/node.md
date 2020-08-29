# Quick reference: node

## Requirements

**Ideal**
- `yarn.lock` file present in your project
**OR**
- `npm` buildtool installed **AND** `package.json` file present in your project

**Minimum**
- `package.json` file present in your project
OR
- `yarn.lock` file present in your project
OR
- `package-lock.json` file present in your project

## Project discovery
Directories containing any of the following files are considered node projects:

* `package.json`
* `package-lock.json`
* `yarn.lock`

Subdirectories of node projects are re-scanned for subprojects, with the
exception of the `node_modules` folder, which is ignored once any of the above
files is found.

