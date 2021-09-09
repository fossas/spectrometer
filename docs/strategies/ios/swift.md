# Swift Package Manager

## Project Discovery

Find all swift manifest files, named: `Package.swift`

# Swift Package Analysis

| Strategy                                      | Direct Deps        | Deep Deps | Edges | Classifies Test Dependencies |
| --------------------------------------------- | ------------------ | --------- | ----- | ---------------------------- |
| parse package dependencies in `Package.swift` | :white_check_mark: | :x:       | :x:   | :x:                          |

- Manifest file - `Package.swift`, must begin with `// swift-tools-version:` string, followed by version number specifier. 
- We follow swift package manager's convention, and presume properties of package are defined in a single nested initializer statement, and are not modified after initialization.

## Limitation

- Path dependencies are ignored in the analysis (e.g. `package(path: "./../local-pkg")`)

## Example 

Create Package.swift file in the directory. Add dependencies, targets, products, and source code. Example Package.swift file is shown below. By convention, the properties of a Package are defined in a single nested initializer statement, and not modified after initialization.

```swift
// swift-tools-version:4.0
import PackageDescription

let package = Package(
    name: "DeckOfPlayingCards",
    products: [
        .library(name: "DeckOfPlayingCards", targets: ["DeckOfPlayingCards"]),
    ],
    dependencies: [
        .package(url: "https://github.com/apple/example-package-fisheryates.git", from: "2.0.0"),
        .package(url: "https://github.com/apple/example-package-playingcard.git", from: "3.0.0"),
    ],
    targets: [
        .target(
            name: "DeckOfPlayingCards",
            dependencies: ["FisherYates", "PlayingCard"]),
        .testTarget(
            name: "DeckOfPlayingCardsTests",
            dependencies: ["DeckOfPlayingCards"]),
    ]
)
```

When analysis is performed (e.g. `fossa analyze -o`), we will identify following as direct dependencies:
- https://github.com/apple/example-package-fisheryates.git
- https://github.com/apple/example-package-playingcard.git

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