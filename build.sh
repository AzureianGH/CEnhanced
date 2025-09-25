#!/usr/bin/env bash
set -euo pipefail

# Usage: ./build.sh [preset]
# If a CMake preset is provided (default: linux-release), use it.
# Otherwise, fall back to a local configure+build in ./build using Unix Makefiles or Ninja if available.

PRESET=${1:-linux-release}

mkdir -p build
# Prefer Ninja if available, otherwise use Unix Makefiles
GENERATOR="Unix Makefiles"
if command -v ninja >/dev/null 2>&1; then
  GENERATOR="Ninja"
fi
cmake -S . -B build -G "$GENERATOR" -DCMAKE_BUILD_TYPE=Release
cmake --build build --config Release