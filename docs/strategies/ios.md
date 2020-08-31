# iOS Analysis

The iOS buildtool ecosystem consists of two major toolchains: `Carthage` and `Cocoapods`

| Strategy | Direct Deps | Deep Deps | Edges | Tags |
| --- | --- | --- | --- | --- |
| [Carthage][cart] | ✅ | ✅ | ✅ ||
| [Podfile.lock][coco] | ✅ | ✅ | ✅ ||
| [Podfile][coco] | ✅ | ❌ | ❌ ||

[cart]: ios/carthage.md
[coco]: ios/cocoapods.md
