# Swift Package Manager

## Project Discovery

Find all swift manifest files, named: `Package.swift`

# Swift Package Analysis

| Strategy                                                             | Direct Deps        | Deep Deps          | Edges | Classifies Test Dependencies |
| -------------------------------------------------------------------- | ------------------ | ------------------ | ----- | ---------------------------- |
| parse package dependencies in `Package.swift`                        | :white_check_mark: | :x:                | :x:   | :x:                          |
| parse package dependencies in `Package.swift` and `Package.resolved` | :white_check_mark: | :white_check_mark: | :x:   | :x:                          |

- Manifest file - `Package.swift`, must begin with `// swift-tools-version:` string, followed by version number specifier. 
- We follow swift package manager's convention, and presume properties of package are defined in a single nested initializer statement, and are not modified after initialization.

## Limitation

- Path dependencies are ignored in the analysis (e.g. `package(path: "./../local-pkg")`)

## Example 

Create Package.swift file in the directory. Add dependencies, targets, products, and source code. Example Package.swift file is shown below. By convention, the properties of a Package are defined in a single nested initializer statement, and not modified after initialization.

```swift
// swift-tools-version:5.4.0

import PackageDescription

let package = Package(
    name: "Example",
    defaultLocalization: "en",
    products: [],
    dependencies: [
        .package(name: "grpc-swift", url: "https://github.com/grpc/grpc-swift.git", from: "1.0.0"),
    ]
)
```

We can resolve dependencies by performing `swift package update`. Executing this will create Package.resolved in the directory. Example file is shown below:

```json
{
  "object": {
    "pins": [
      {
        "package": "grpc-swift",
        "repositoryURL": "https://github.com/grpc/grpc-swift.git",
        "state": {
          "branch": null,
          "revision": "14e1ea3350892a864386517c037e11fb68baf818",
          "version": "1.3.0"
        }
      },
      {
        "package": "swift-log",
        "repositoryURL": "https://github.com/apple/swift-log.git",
        "state": {
          "branch": null,
          "revision": "5d66f7ba25daf4f94100e7022febf3c75e37a6c7",
          "version": "1.4.2"
        }
      }
    ]
  },
  "version": 1
}

```
Note: Only few pins are shown above for brevity.

### `Package.resolved` exist in the directory

When analyses is performed (e.g. `fossa analyze -o`), we will identify following as direct dependencies:

- https://github.com/grpc/grpc-swift.git@1.3.0

If the `Package.resolved` exists in the directory at the time of analyses, we would also identify, following deep dependencies:

- https://github.com/apple/swift-log.git@1.4.2
- and any others dependencies that appear in `Package.resolved`

We will not identify edges amongst them.

### `Package.resolved` does not exist in the directory

When analyses is performed (e.g. `fossa analyze -o`), we will identify following as direct dependencies:

- https://github.com/grpc/grpc-swift.git

We will not identify any deep dependencies.

## F.A.Q

### How do I *only perform analysis* for swift package dependencies?

You can explicitly specify analysis an target in `.fossa.yml` file. 

Example below, will exclude all analysis targets except swift. 

```yaml
# .fossa.yml 

version: 3
targets:
  only:
    - type: swift
```

## References

- [Swift Package Manager](https://github.com/apple/swift-package-manager)
- [Package.swift, must begin with version specifier](https://github.com/apple/swift-package-manager/blob/main/Documentation/PackageDescription.md#about-the-swift-tools-version)
- [Package.swift, must be defined in single nested statement, and should not be modified after initialization](https://github.com/apple/swift-package-manager/blob/main/Documentation/PackageDescription.md#package)