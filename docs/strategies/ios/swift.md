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

We can resolve dependencies by performing `swift package update`. Executing this will create Package.resolved in the directory. Example file is shown below.

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
      },
      {
        "package": "swift-nio",
        "repositoryURL": "https://github.com/apple/swift-nio.git",
        "state": {
          "branch": null,
          "revision": "94f41c4121a82fae5c7b1cb03e630e9f9e5e20f1",
          "version": "2.32.1"
        }
      },
      {
        "package": "swift-nio-extras",
        "repositoryURL": "https://github.com/apple/swift-nio-extras.git",
        "state": {
          "branch": null,
          "revision": "f72c4688f89c28502105509186eadc49a49cb922",
          "version": "1.10.0"
        }
      },
      {
        "package": "swift-nio-http2",
        "repositoryURL": "https://github.com/apple/swift-nio-http2.git",
        "state": {
          "branch": null,
          "revision": "42bdcae4ac4913507a5ee7af963c559deb60d1fc",
          "version": "1.18.2"
        }
      },
      {
        "package": "swift-nio-ssl",
        "repositoryURL": "https://github.com/apple/swift-nio-ssl.git",
        "state": {
          "branch": null,
          "revision": "4829979d9f5ed9a2f4c6efd9c1ed51d1ab4d0394",
          "version": "2.14.1"
        }
      },
      {
        "package": "swift-nio-transport-services",
        "repositoryURL": "https://github.com/apple/swift-nio-transport-services.git",
        "state": {
          "branch": null,
          "revision": "9571a61d236c5253b6a255a2d13fac536a1e2625",
          "version": "1.11.2"
        }
      },
      {
        "package": "SwiftProtobuf",
        "repositoryURL": "https://github.com/apple/swift-protobuf.git",
        "state": {
          "branch": null,
          "revision": "1f62db409f2c9b0223a3f68567b4a01333aae778",
          "version": "1.17.0"
        }
      }
    ]
  },
  "version": 1
}

```

### `Package.resolved` exist in the directory

When analyses is performed (e.g. `fossa analyze -o`), we will identify following as direct dependencies:

- https://github.com/grpc/grpc-swift.git@1.3.0

If the `Package.resolved` exists in the directory at the time of analyses, we would also identify, following deep dependencies:

- https://github.com/apple/swift-nio.git@2.32.1
- https://github.com/apple/swift-nio-http2.git@1.18.2
- https://github.com/apple/swift-nio-ssl.git@2.14.1
- https://github.com/apple/swift-nio-transport-services.git@1.11.2
- https://github.com/apple/swift-nio-extras.git@1.10.0
- https://github.com/apple/swift-log.git@1.4.2
- https://github.com/apple/swift-protobuf.git@1.17.0

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