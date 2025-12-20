#!/bin/bash
# Build and run Afferent with the correct compiler settings for macOS
# The bundled lld linker doesn't handle macOS frameworks properly,
# so we use the system clang which uses ld64.

set -e

# Use system clang for proper macOS framework linking
export LEAN_CC=/usr/bin/clang

# Add Homebrew lib path for gmp (required by wisp's shared library build)
export LIBRARY_PATH=/opt/homebrew/lib:${LIBRARY_PATH:-}

# Build the specified target (default: afferent)
TARGET="${1:-afferent}"

echo "Building $TARGET..."
lake build "$TARGET"

echo ""
echo "Running $TARGET..."
lake exe "$TARGET"
