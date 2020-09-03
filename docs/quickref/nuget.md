# Quick reference: Nuget

## Requirements

Or one of the following in your directory:

- [`.nuspec`](https://docs.microsoft.com/en-us/nuget/reference/nuspec) formatted file in your directory.
- [Package reference](https://docs.microsoft.com/en-us/nuget/consume-packages/package-references-in-project-files) file present in your project. Commonly with an ending such as `.csproj`, `xproj`, `vbproj` and others.
- `project.assets.json`
- `packages.config`
- `project.json`
- `paket.lock`

## Project discovery

Directories containing any of the files listed above are considered Nuget projects.

Subdirectory scans are not affected by the discovery of any type of Nuget project.