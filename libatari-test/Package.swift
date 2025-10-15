// swift-tools-version: 6.0
import PackageDescription

let package = Package(
    name: "libatari-test",
    platforms: [
        .macOS(.v15)
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
            path: "Sources/CBridge",
            cSettings: [
                .define("LIBATARI800")
            ],
            linkerSettings: [
                .unsafeFlags([
                    "-L..",
                    "-latari800",
                    "-Xlinker", "-rpath", "-Xlinker", "@executable_path/../../../.."
                ])
            ]
        ),
        .executableTarget(
            name: "libatari-test",
            dependencies: ["CBridge"],
            resources: [
                .process("Shaders.metal")
            ],
            swiftSettings: [
                .unsafeFlags(["-parse-as-library"])
            ]
        ),
    ]
)
