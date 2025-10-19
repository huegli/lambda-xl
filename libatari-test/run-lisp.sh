#!/bin/bash

# Script to run the Atari emulator in LispWorks
# This assumes LispWorks is installed and accessible

# Get the directory where this script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Change to the script directory
cd "$SCRIPT_DIR"

# Ensure libatari800.dylib exists
if [ ! -f "../libatari800.dylib" ]; then
    echo "Error: libatari800.dylib not found in parent directory"
    echo "Please build it first using build.sh or build_libatari800.sh"
    exit 1
fi

# Check if LispWorks is available
if command -v lispworks &> /dev/null; then
    LISP_CMD="lispworks"
elif [ -f "/Applications/LispWorks 8.1 (64-bit)/LispWorks (64-bit).app/Contents/MacOS/lispworks-8-1-0-macos64-universal" ]; then
    LISP_CMD="/Applications/LispWorks 8.1 (64-bit)/LispWorks (64-bit).app/Contents/MacOS/lispworks-8-1-0-macos64-universal"
elif [ -f "/Applications/LispWorks 7.1 (64-bit)/LispWorks (64-bit).app/Contents/MacOS/lispworks-7-1-0-macos64-universal" ]; then
    LISP_CMD="/Applications/LispWorks 7.1 (64-bit)/LispWorks (64-bit).app/Contents/MacOS/lispworks-7-1-0-macos64-universal"
else
    echo "Error: LispWorks not found"
    echo "Please install LispWorks or update the path in this script"
    exit 1
fi

echo "Using LispWorks: $LISP_CMD"
echo "Starting Atari Emulator..."

# Run LispWorks with the emulator file
"$LISP_CMD" -init atari-emulator.lisp -eval "(in-package :atari-emulator)" -eval "(run-emulator)"
