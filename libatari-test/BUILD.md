# Build Instructions for libatari-test

## Prerequisites

- macOS 13.0 or later
- Swift 5.9 or later (included with Xcode 15+)
- Xcode Command Line Tools

## Building the Project

### Step 4: Automatic Build with libatari800 Integration (Recommended)

The project now includes automatic compilation of the libatari800 library as part of the build process.

1. Navigate to the project directory:
   ```bash
   cd libatari-test
   ```

2. Build the project (automatically builds libatari800 first):
   ```bash
   ./build.sh
   ```

3. Run the project (builds and runs):
   ```bash
   ./run.sh
   ```

### Using Swift Package Manager (Manual)

For manual builds without automatic libatari800 compilation:

1. Navigate to the project directory:
   ```bash
   cd libatari-test
   ```

2. Build the project:
   ```bash
   swift build
   ```

3. Run the project:
   ```bash
   swift run
   ```

### Alternative Build Commands

- **Debug build** (default):
  ```bash
  swift build
  ```

- **Release build** (optimized):
  ```bash
  swift build -c release
  ```

- **Clean build directory**:
  ```bash
  swift package clean
  ```

## Running the Executable

After building, you can run the executable directly:

```bash
# Run via Swift Package Manager
swift run

# Or run the built executable directly
./.build/debug/libatari-test
```

## Expected Output

The program should output:
```
Hello World
```

## Project Structure

```
libatari-test/
├── Package.swift              # Swift Package Manager configuration
├── Sources/
│   └── libatari-test/
│       └── main.swift         # Main executable source
├── BUILD.md                   # This file
├── IMPLEMENTATION.md          # Implementation steps
└── .gitignore                 # Git ignore rules
```

## Troubleshooting

- **Swift not found**: Install Xcode Command Line Tools: `xcode-select --install`
- **Build fails**: Ensure you're using Swift 5.9 or later: `swift --version`
- **Permission denied**: Make sure the executable has proper permissions: `chmod +x ./.build/debug/libatari-test`