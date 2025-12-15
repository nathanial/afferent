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

# Build libcurl if not already built (or if built without TLS/HTTPS support).
# NOTE: curl 8.15+ removed Secure Transport, so we build with OpenSSL on macOS.
CURL_NEEDS_BUILD=0
if [ ! -f "third_party/curl/build/lib/libcurl.a" ]; then
    CURL_NEEDS_BUILD=1
elif [ ! -f "third_party/curl/build/lib/curl_config.h" ]; then
    CURL_NEEDS_BUILD=1
elif ! rg -q "^#define USE_OPENSSL 1$" third_party/curl/build/lib/curl_config.h; then
    CURL_NEEDS_BUILD=1
fi

if [ "$CURL_NEEDS_BUILD" -eq 1 ]; then
    echo "Building libcurl (this may take a few minutes on first build)..."

    OPENSSL_ROOT="/opt/homebrew/opt/openssl@3"
    if [ ! -d "$OPENSSL_ROOT" ]; then
        OPENSSL_ROOT="/opt/homebrew/opt/openssl"
    fi
    if [ ! -d "$OPENSSL_ROOT" ] && [ -d "/usr/local/opt/openssl@3" ]; then
        OPENSSL_ROOT="/usr/local/opt/openssl@3"
    fi

    mkdir -p third_party/curl/build
    pushd third_party/curl/build > /dev/null

    cmake .. \
        -DCMAKE_BUILD_TYPE=Release \
        -DBUILD_SHARED_LIBS=OFF \
        -DBUILD_CURL_EXE=OFF \
        -DCURL_USE_OPENSSL=ON \
        -DOPENSSL_ROOT_DIR="$OPENSSL_ROOT" \
        -DCURL_CA_BUNDLE=/etc/ssl/cert.pem \
        -DCURL_CA_PATH=/etc/ssl/certs \
        -DCURL_DISABLE_LDAP=ON \
        -DCURL_DISABLE_LDAPS=ON \
        -DCURL_USE_LIBSSH2=OFF \
        -DCURL_USE_LIBSSH=OFF \
        -DBUILD_TESTING=OFF \
        -DCURL_USE_LIBPSL=OFF \
        -DUSE_NGHTTP2=OFF \
        -DUSE_LIBIDN2=OFF \
        -DCURL_BROTLI=OFF \
        -DCURL_ZSTD=OFF

    cmake --build . --config Release -j$(sysctl -n hw.ncpu)
    popd > /dev/null
    echo "libcurl build complete!"
fi

# Build the specified target (default: afferent executable)
TARGET="${1:-afferent}"

echo "Building $TARGET..."
lake build "$TARGET"

echo "Build complete!"
