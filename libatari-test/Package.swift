// swift-tools-version: 5.9
import PackageDescription

let package = Package(
    name: "libatari-test",
    platforms: [
        .macOS(.v13)
    ],
    products: [
        .executable(
            name: "libatari-test",
            targets: ["libatari-test"]
        ),
    ],
    targets: [
        .target(
            name: "CBridge",
            path: "Sources/CBridge"
        ),
        .executableTarget(
            name: "libatari-test",
            dependencies: ["CBridge"]
        ),
    ]
)