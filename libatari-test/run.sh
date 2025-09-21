#!/bin/bash

# Run script for libatari-test
# This script builds and runs the project

set -e  # Exit on any error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "=== Step 4: Running libatari-test with automatic compilation ==="

# Build the project
"$SCRIPT_DIR/build.sh"

# Run the project
echo "Step 3: Running the executable..."
cd "$SCRIPT_DIR"
swift run

echo "=== Run completed successfully ==="