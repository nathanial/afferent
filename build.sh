#!/bin/bash
# Build Afferent with the correct compiler settings for macOS
# The bundled lld linker doesn't handle macOS frameworks properly,
# so we use the system clang which uses ld64.

set -e

# Use system clang for proper macOS framework linking
export LEAN_CC=/usr/bin/clang

# Build the specified target (default: all)
TARGET="${1:-}"

if [ -z "$TARGET" ]; then
    echo "Building all targets..."
    lake build
else
    echo "Building $TARGET..."
    lake build "$TARGET"
fi

echo "Build complete!"
