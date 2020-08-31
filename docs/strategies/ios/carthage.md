# Carthage Analysis

## Project Discovery

Find any folder which contains a file named `Cartfile.resolved`.  Skip all
subdirectories if one is found.

## Analysis

The `Cartfile.resolved` file contains a list of direct dependencies, which
should each represent subfolders, contained in the `Carthage/Checkouts`
directory.  See below for an example:

```
.
├─ Cartfile.resolved  # ref to "Quick/Nimble"
└─ Carthage
  └─ Checkouts
    └─ Nimble
      └─ Cartfile.resolved # ref to ""mattgallagher/CwlPreconditionTesting"
        └─ Carthage
          └─ Checkouts
            └─ CwlPreconditionTesting
```