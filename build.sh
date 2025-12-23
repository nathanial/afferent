#!/bin/bash
# Build Afferent with the correct compiler settings for macOS
# The bundled lld linker doesn't handle macOS frameworks properly,
# so we use the system clang which uses ld64.

set -e

# Use system clang for proper macOS framework linking
export LEAN_CC=/usr/bin/clang

# Add Homebrew lib path for gmp (required by wisp's shared library build)
export LIBRARY_PATH=/opt/homebrew/lib:${LIBRARY_PATH:-}

# Get the directory where this script lives
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Ensure Assimptor (Assimp wrapper) is built first when present
if [ -d "../assimptor" ]; then
    echo "Building Assimptor dependency..."
    (cd ../assimptor && ./build.sh Assimptor)
fi

# Build the specified target (default: afferent executable)
TARGET="${1:-afferent}"

echo "Building $TARGET..."
lake build "$TARGET"

echo "Build complete!"
