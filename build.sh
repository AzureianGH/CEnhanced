#!/usr/bin/env bash
set -euo pipefail

# Usage: ./build.sh [preset]
# - If a preset is given, use it.
# - Otherwise auto-select a sane default per platform.

PRESET="${1:-}"

if [[ -z "$PRESET" ]]; then
  if [[ "${OS:-}" == "Windows_NT" ]] || [[ -n "${MSYSTEM:-}" ]]; then
    PRESET="mingw-release"
  fi
fi

if [[ -n "$PRESET" ]]; then
  echo "Configuring with preset: $PRESET"
  cmake --preset "$PRESET" \
    ${CHANCE_DEFAULT_RUNTIME:+-DCHANCE_DEFAULT_RUNTIME="$CHANCE_DEFAULT_RUNTIME"} \
    ${CHANCE_DEFAULT_STDLIB:+-DCHANCE_DEFAULT_STDLIB="$CHANCE_DEFAULT_STDLIB"}
  cmake --build --preset "$PRESET"
  exit 0
fi

mkdir -p build

# Fallback path for non-preset builds.
if command -v ninja >/dev/null 2>&1; then
  GENERATOR="Ninja"
elif command -v make >/dev/null 2>&1; then
  GENERATOR="Unix Makefiles"
else
  echo "error: no build tool found (need ninja or make)."
  echo "hint: on MSYS2 use a MinGW shell and run ./build.sh mingw-release"
  exit 1
fi

if ! command -v cc >/dev/null 2>&1 && ! command -v gcc >/dev/null 2>&1 && ! command -v clang >/dev/null 2>&1; then
  echo "error: no C compiler found in PATH."
  echo "hint: install a compiler toolchain (MSYS2: pacman -S mingw-w64-ucrt-x86_64-gcc)."
  exit 1
fi

need_configure=0
if [[ ! -f build/CMakeCache.txt ]]; then
  need_configure=1
fi

# install.sh passes CHANCE_DEFAULT_* so chancec embeds default runtime/stdlib paths.
# Reconfigure when they are provided to keep cache speedups without losing correctness.
if [[ -n "${CHANCE_DEFAULT_RUNTIME:-}" || -n "${CHANCE_DEFAULT_STDLIB:-}" ]]; then
  need_configure=1
fi

if [[ "$need_configure" -eq 1 ]]; then
  cmake -S . -B build -G "$GENERATOR" \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_C_FLAGS_RELEASE="-O2" \
    ${CHANCE_DEFAULT_RUNTIME:+-DCHANCE_DEFAULT_RUNTIME="$CHANCE_DEFAULT_RUNTIME"} \
    ${CHANCE_DEFAULT_STDLIB:+-DCHANCE_DEFAULT_STDLIB="$CHANCE_DEFAULT_STDLIB"}
else
  echo "Using existing CMake cache in ./build (skipping configure)"
fi

cmake --build build --config Release