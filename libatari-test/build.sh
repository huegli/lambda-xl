#!/bin/bash

# Main build script for libatari-test
# This script first builds libatari800, then builds the Swift project

set -e  # Exit on any error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "=== Step 4: Building libatari-test with automatic libatari800 compilation ==="

# First, build the libatari800 library
echo "Step 1: Building libatari800 library..."
"$SCRIPT_DIR/build_libatari800.sh"

# Then build the Swift project
echo "Step 2: Building Swift project..."
cd "$SCRIPT_DIR"
swift build "$@"

echo "=== Build completed successfully ==="