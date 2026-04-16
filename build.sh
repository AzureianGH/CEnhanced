#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "$0")" && pwd)"
MAKE_BIN=""
MAKE_CORES=""

usage() {
  cat <<'EOF'
Usage: ./build.sh [--cores=<count>]

Options:
  --cores=<count>   Run make with -j <count>
  --cores <count>   Run make with -j <count>
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --cores=*)
      MAKE_CORES="${1#--cores=}"
      shift
      ;;
    --cores)
      if [[ $# -lt 2 ]]; then
        echo "error: --cores requires a value"
        usage
        exit 1
      fi
      MAKE_CORES="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "error: unknown argument: $1"
      usage
      exit 1
      ;;
  esac
done

if [[ -n "$MAKE_CORES" ]]; then
  if ! [[ "$MAKE_CORES" =~ ^[1-9][0-9]*$ ]]; then
    echo "error: --cores must be a positive integer"
    exit 1
  fi
fi

if command -v make >/dev/null 2>&1; then
  MAKE_BIN="make"
elif command -v mingw32-make >/dev/null 2>&1; then
  MAKE_BIN="mingw32-make"
fi

if [[ -z "$MAKE_BIN" ]]; then
  echo "error: no make tool found (need make or mingw32-make)."
  exit 1
fi

if ! command -v cc >/dev/null 2>&1 && ! command -v gcc >/dev/null 2>&1 && ! command -v clang >/dev/null 2>&1; then
  echo "error: no C compiler found in PATH."
  exit 1
fi

MAKE_ARGS=(
  -C "$SCRIPT_DIR"
  CHANCE_DEFAULT_RUNTIME="${CHANCE_DEFAULT_RUNTIME:-}"
  CHANCE_DEFAULT_STDLIB="${CHANCE_DEFAULT_STDLIB:-}"
  all
)

if [[ -n "$MAKE_CORES" ]]; then
  "$MAKE_BIN" -j "$MAKE_CORES" "${MAKE_ARGS[@]}"
else
  "$MAKE_BIN" "${MAKE_ARGS[@]}"
fi