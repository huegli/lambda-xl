#!/bin/bash

# Build script for libatari800 library
# This script ensures that the libatari800.a library is built before the Swift project

set -e  # Exit on any error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ATARI800_DIR="$SCRIPT_DIR/../atari800"
LIBATARI800_A="$ATARI800_DIR/src/libatari800.a"

echo "Building libatari800 library..."

# Check if atari800 directory exists
if [ ! -d "$ATARI800_DIR" ]; then
    echo "Error: atari800 directory not found at $ATARI800_DIR"
    exit 1
fi

# Change to atari800 directory
cd "$ATARI800_DIR"

# Check if library already exists and is up to date
if [ -f "$LIBATARI800_A" ]; then
    echo "libatari800.a already exists, checking if rebuild is needed..."

    # Check if any source files are newer than the library
    if find src -name "*.c" -o -name "*.h" | xargs ls -t | head -1 | xargs test "$LIBATARI800_A" -nt; then
        echo "libatari800.a is up to date, skipping rebuild"
        exit 0
    fi
fi

echo "Building libatari800..."

# Configure if needed (check if Makefile exists and is recent)
if [ ! -f "Makefile" ] || [ "configure.ac" -nt "Makefile" ]; then
    echo "Configuring libatari800..."
    ./configure --target=libatari800
fi

# Build the library
echo "Compiling libatari800..."
make

# Verify the library was created
if [ ! -f "$LIBATARI800_A" ]; then
    echo "Error: Failed to build libatari800.a"
    exit 1
fi

echo "libatari800.a built successfully at $LIBATARI800_A"