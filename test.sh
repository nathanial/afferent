#!/bin/bash
# Run Afferent tests with the correct compiler settings for macOS
# The bundled lld linker doesn't handle macOS frameworks properly,
# so we use the system clang which uses ld64.

set -e

# Use system clang for proper macOS framework linking
export LEAN_CC=/usr/bin/clang

echo "Building and running tests..."

# TODO: Add test target once tests are implemented
# lake build afferent_tests && .lake/build/bin/afferent_tests

echo "No tests implemented yet."
echo "Tests will be added in Afferent/Tests/"
