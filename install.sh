#!/usr/bin/env sh
set -e

if [ -t 1 ]; then
  C_RESET="$(printf '\033[0m')"
  C_BOLD="$(printf '\033[1m')"
  C_DIM="$(printf '\033[2m')"
  C_RED="$(printf '\033[31m')"
  C_GREEN="$(printf '\033[32m')"
  C_YELLOW="$(printf '\033[33m')"
  C_BLUE="$(printf '\033[34m')"
  C_MAGENTA="$(printf '\033[35m')"
else
  C_RESET=""
  C_BOLD=""
  C_DIM=""
  C_RED=""
  C_GREEN=""
  C_YELLOW=""
  C_BLUE=""
  C_MAGENTA=""
fi

log() {
  printf "%s%s%s\n" "$C_BLUE" "$1" "$C_RESET"
}

note() {
  printf "%s%s%s\n" "$C_DIM" "$1" "$C_RESET"
}

ok() {
  printf "%s%s%s\n" "$C_GREEN" "$1" "$C_RESET"
}

warn() {
  printf "%s%s%s\n" "$C_YELLOW" "$1" "$C_RESET"
}

err() {
  printf "%s%s%s\n" "$C_RED" "$1" "$C_RESET"
}

section() {
  printf "\n%s%s== %s ==%s\n" "$C_BOLD" "$C_MAGENTA" "$1" "$C_RESET"
}

PREFIX=${PREFIX:-/usr/local}
BIN_DIR="$PREFIX/bin"
SHARE_DIR="$PREFIX/share/chance"
STDLIB_DIR="$SHARE_DIR/stdlib"
RUNTIME_DIR="$SHARE_DIR/runtime"

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
CHANCECODE_DIR="${1:-$SCRIPT_DIR/../ChanceCode}"
if [ -n "$CHANCECODE_DIR" ]; then
  if cd "$CHANCECODE_DIR" >/dev/null 2>&1; then
    CHANCECODE_DIR=$(pwd)
    cd "$SCRIPT_DIR"
  fi
fi

DEFAULT_RUNTIME="$RUNTIME_DIR/runtime.cclib"
DEFAULT_STDLIB="$STDLIB_DIR/stdlib.cclib"

section "Build"
log "Building CHance compiler"
CHANCE_DEFAULT_RUNTIME="$DEFAULT_RUNTIME" \
CHANCE_DEFAULT_STDLIB="$DEFAULT_STDLIB" \
"$SCRIPT_DIR/build.sh"
ok "Compiler build complete"

if [ -n "$CHANCECODE_DIR" ] && [ -f "$CHANCECODE_DIR/build.sh" ]; then
  log "Building ChanceCode"
  (cd "$CHANCECODE_DIR" && ./build.sh)
  ok "ChanceCode build complete"
fi

CHANCEC_BIN="$SCRIPT_DIR/build/chancec"
if [ ! -x "$CHANCEC_BIN" ]; then
  err "error: chancec not found at $CHANCEC_BIN"
  exit 1
fi

CHANCECODEC_BIN=""
if [ -x "$SCRIPT_DIR/build/chancecodec" ]; then
  CHANCECODEC_BIN="$SCRIPT_DIR/build/chancecodec"
elif [ -x "$CHANCECODE_DIR/build/chancecodec" ]; then
  CHANCECODEC_BIN="$CHANCECODE_DIR/build/chancecodec"
fi
if [ -n "$CHANCECODEC_BIN" ]; then
  note "Using chancecodec for library rebuild: $CHANCECODEC_BIN"
fi

section "Libraries"
log "Rebuilding runtime and stdlib with built compiler"
if [ -n "$CHANCECODEC_BIN" ]; then
  (cd "$SCRIPT_DIR/runtime" && "$CHANCEC_BIN" --chancecodec "$CHANCECODEC_BIN" runtime.ceproj)
  (cd "$SCRIPT_DIR/src/stdlib" && "$CHANCEC_BIN" --chancecodec "$CHANCECODEC_BIN" stdlib.ceproj)
else
  (cd "$SCRIPT_DIR/runtime" && "$CHANCEC_BIN" runtime.ceproj)
  (cd "$SCRIPT_DIR/src/stdlib" && "$CHANCEC_BIN" stdlib.ceproj)
fi
ok "Library rebuild complete"

section "Install"
note "Prefix: $PREFIX"
note "Bin:    $BIN_DIR"
note "Stdlib: $STDLIB_DIR"
note "Runtime:$RUNTIME_DIR"
log "Installing artifacts"
mkdir -p "$BIN_DIR" "$STDLIB_DIR" "$RUNTIME_DIR"

install -m 755 "$CHANCEC_BIN" "$BIN_DIR/chancec"

if [ -f "$SCRIPT_DIR/src/stdlib/stdlib.cclib" ]; then
  install -m 644 "$SCRIPT_DIR/src/stdlib/stdlib.cclib" "$STDLIB_DIR/stdlib.cclib"
fi

if [ -f "$SCRIPT_DIR/runtime/runtime.cclib" ]; then
  install -m 644 "$SCRIPT_DIR/runtime/runtime.cclib" "$RUNTIME_DIR/runtime.cclib"
fi

if [ -x "$SCRIPT_DIR/build/chancecodec" ]; then
  install -m 755 "$SCRIPT_DIR/build/chancecodec" "$BIN_DIR/chancecodec"
elif [ -x "$CHANCECODE_DIR/build/chancecodec" ]; then
  install -m 755 "$CHANCECODE_DIR/build/chancecodec" "$BIN_DIR/chancecodec"
fi

ok "Done"
