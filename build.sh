#!/bin/bash
# Build Afferent with the correct compiler settings for macOS
# The bundled lld linker doesn't handle macOS frameworks properly,
# so we use the system clang which uses ld64.

set -e

# Use system clang for proper macOS framework linking
export LEAN_CC=/usr/bin/clang

# Get the directory where this script lives
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Initialize and update git submodules if needed
if [ ! -f "third_party/assimp/CMakeLists.txt" ]; then
    echo "Initializing git submodules..."
    git submodule update --init --recursive
fi

# Build Assimp if not already built
if [ ! -f "third_party/assimp/build/lib/libassimp.a" ] && [ ! -f "third_party/assimp/build/lib/libassimp.dylib" ]; then
    echo "Building Assimp (this may take a few minutes on first build)..."
    mkdir -p third_party/assimp/build
    pushd third_party/assimp/build > /dev/null
    cmake .. -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=OFF -DASSIMP_BUILD_TESTS=OFF -DASSIMP_BUILD_SAMPLES=OFF
    cmake --build . --config Release -j$(sysctl -n hw.ncpu)
    popd > /dev/null
    echo "Assimp build complete!"
fi

# Build the specified target (default: afferent executable)
TARGET="${1:-afferent}"

echo "Building $TARGET..."
lake build "$TARGET"

echo "Build complete!"
