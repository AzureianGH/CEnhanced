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

find_make() {
  if command -v make >/dev/null 2>&1; then
    printf "%s" "make"
    return
  fi
  if command -v mingw32-make >/dev/null 2>&1; then
    printf "%s" "mingw32-make"
    return
  fi
  printf "%s" ""
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

persist_export() {
  profile="$1"
  var_name="$2"
  var_value="$3"

  if [ -z "$profile" ] || [ -z "$var_name" ] || [ -z "$var_value" ]; then
    return
  fi

  if [ ! -f "$profile" ]; then
    touch "$profile"
  fi

  tmp_file="$profile.chance.$$"
  if ! grep -v "^export ${var_name}=" "$profile" > "$tmp_file" 2>/dev/null; then
    : > "$tmp_file"
  fi
  printf "export %s=\"%s\"\n" "$var_name" "$var_value" >> "$tmp_file"
  mv "$tmp_file" "$profile"
}

PREFIX=${PREFIX:-/usr/local}
BIN_DIR="$PREFIX/bin"
SHARE_DIR="$PREFIX/share/chance"
STDLIB_DIR="$SHARE_DIR/stdlib"
RUNTIME_DIR="$SHARE_DIR/runtime"

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
CHANCECODE_DIR="${1:-$SCRIPT_DIR/../ChanceCode}"
CLD_DIR="${2:-$SCRIPT_DIR/../CLD}"
CHS_DIR="${3:-$SCRIPT_DIR/../CHS}"
CVM_DIR="${4:-$SCRIPT_DIR/../CVM}"
if [ -n "$CHANCECODE_DIR" ]; then
  if cd "$CHANCECODE_DIR" >/dev/null 2>&1; then
    CHANCECODE_DIR=$(pwd)
    cd "$SCRIPT_DIR"
  fi
fi
if [ -n "$CLD_DIR" ]; then
  if cd "$CLD_DIR" >/dev/null 2>&1; then
    CLD_DIR=$(pwd)
    cd "$SCRIPT_DIR"
  fi
fi
if [ -n "$CHS_DIR" ]; then
  if cd "$CHS_DIR" >/dev/null 2>&1; then
    CHS_DIR=$(pwd)
    cd "$SCRIPT_DIR"
  fi
fi
if [ -n "$CVM_DIR" ]; then
  if cd "$CVM_DIR" >/dev/null 2>&1; then
    CVM_DIR=$(pwd)
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

if [ -n "$CHS_DIR" ] && [ -f "$CHS_DIR/Makefile" ]; then
  MAKE_BIN=$(find_make)
  if [ -z "$MAKE_BIN" ]; then
    err "error: no make tool found (need make or mingw32-make)"
    exit 1
  fi
  log "Building CHS"
  (cd "$CHS_DIR" && "$MAKE_BIN")
  ok "CHS build complete"
fi

if [ -n "$CVM_DIR" ] && [ -f "$CVM_DIR/CMakeLists.txt" ]; then
  if ! command -v cmake >/dev/null 2>&1; then
    err "error: cmake is required to build CVM"
    exit 1
  fi
  log "Building CVM"
  if [ -f "$CVM_DIR/build/CMakeCache.txt" ]; then
    (cd "$CVM_DIR" && cmake --build build)
  else
    (cd "$CVM_DIR" && cmake -S . -B build && cmake --build build)
  fi
  ok "CVM build complete"
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

CHS_BIN=""
if [ -x "$SCRIPT_DIR/build/chs" ]; then
  CHS_BIN="$SCRIPT_DIR/build/chs"
elif [ -x "$CHS_DIR/build/chs" ]; then
  CHS_BIN="$CHS_DIR/build/chs"
fi
if [ -n "$CHS_BIN" ]; then
  note "Using CHS for assembly: $CHS_BIN"
fi

section "Libraries"
log "Rebuilding runtime and stdlib with built compiler"
if [ -n "$CHANCECODEC_BIN" ]; then
  if [ -n "$CHS_BIN" ]; then
    (cd "$SCRIPT_DIR/runtime" && "$CHANCEC_BIN" --chancecodec "$CHANCECODEC_BIN" --chs "$CHS_BIN" runtime.ceproj)
    (cd "$SCRIPT_DIR/src/stdlib" && "$CHANCEC_BIN" --chancecodec "$CHANCECODEC_BIN" --chs "$CHS_BIN" stdlib.ceproj)
  else
    (cd "$SCRIPT_DIR/runtime" && "$CHANCEC_BIN" --chancecodec "$CHANCECODEC_BIN" runtime.ceproj)
    (cd "$SCRIPT_DIR/src/stdlib" && "$CHANCEC_BIN" --chancecodec "$CHANCECODEC_BIN" stdlib.ceproj)
  fi
else
  if [ -n "$CHS_BIN" ]; then
    (cd "$SCRIPT_DIR/runtime" && "$CHANCEC_BIN" --chs "$CHS_BIN" runtime.ceproj)
    (cd "$SCRIPT_DIR/src/stdlib" && "$CHANCEC_BIN" --chs "$CHS_BIN" stdlib.ceproj)
  else
    (cd "$SCRIPT_DIR/runtime" && "$CHANCEC_BIN" runtime.ceproj)
    (cd "$SCRIPT_DIR/src/stdlib" && "$CHANCEC_BIN" stdlib.ceproj)
  fi
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

if [ -n "$CVM_DIR" ] && [ -x "$CVM_DIR/build/cvm" ]; then
  log "Installing CVM"
  install -m 755 "$CVM_DIR/build/cvm" "$BIN_DIR/cvm"

  if [ -f "$SCRIPT_DIR/src/stdlib/stdlib.cclib" ]; then
    install -m 644 "$SCRIPT_DIR/src/stdlib/stdlib.cclib" "$BIN_DIR/stdlib.cclib"
  else
    warn "warning: stdlib.cclib not found at $SCRIPT_DIR/src/stdlib/stdlib.cclib"
  fi

  if [ -f "$SCRIPT_DIR/runtime/runtime.cclib" ]; then
    install -m 644 "$SCRIPT_DIR/runtime/runtime.cclib" "$BIN_DIR/runtime.cclib"
  else
    warn "warning: runtime.cclib not found at $SCRIPT_DIR/runtime/runtime.cclib"
  fi

  if [ -f "$SCRIPT_DIR/src/stdlib/stdlib.ccb" ]; then
    install -m 644 "$SCRIPT_DIR/src/stdlib/stdlib.ccb" "$BIN_DIR/stdlib.ccb"
  else
    warn "warning: stdlib.ccb not found at $SCRIPT_DIR/src/stdlib/stdlib.ccb"
  fi

  if [ -f "$SCRIPT_DIR/runtime/runtime.ccb" ]; then
    install -m 644 "$SCRIPT_DIR/runtime/runtime.ccb" "$BIN_DIR/runtime.ccb"
  else
    warn "warning: runtime.ccb not found at $SCRIPT_DIR/runtime/runtime.ccb"
  fi
fi

if [ -n "$CLD_DIR" ] && [ -f "$CLD_DIR/Makefile" ]; then
  MAKE_BIN=$(find_make)
  if [ -z "$MAKE_BIN" ]; then
    err "error: no make tool found (need make or mingw32-make)"
    exit 1
  fi
  log "Installing CLD"
  (cd "$CLD_DIR" && "$MAKE_BIN" install PREFIX="$PREFIX")
fi

if [ -n "$CHS_DIR" ] && [ -f "$CHS_DIR/Makefile" ]; then
  MAKE_BIN=$(find_make)
  if [ -z "$MAKE_BIN" ]; then
    err "error: no make tool found (need make or mingw32-make)"
    exit 1
  fi
  log "Installing CHS"
  (cd "$CHS_DIR" && "$MAKE_BIN" install PREFIX="$PREFIX")
fi

section "Environment"
log "Persisting CHance tool homes"

CHANCEC_HOME_PATH="$BIN_DIR/chancec"
CHANCECODEC_HOME_PATH="$BIN_DIR/chancecodec"
CLD_HOME_PATH="$BIN_DIR/cld"
CHS_HOME_PATH="$BIN_DIR/chs"
CVM_HOME_PATH="$BIN_DIR/cvm"

HOME_ZSHRC="${HOME}/.zshrc"
HOME_BASHRC="${HOME}/.bashrc"
HOME_PROFILE="${HOME}/.profile"

persist_export "$HOME_ZSHRC" "CHANCEC_HOME" "$CHANCEC_HOME_PATH"
persist_export "$HOME_ZSHRC" "CHANCECODEC_HOME" "$CHANCECODEC_HOME_PATH"
persist_export "$HOME_ZSHRC" "CLD_HOME" "$CLD_HOME_PATH"
persist_export "$HOME_ZSHRC" "CHS_HOME" "$CHS_HOME_PATH"
persist_export "$HOME_ZSHRC" "CVM_HOME" "$CVM_HOME_PATH"

persist_export "$HOME_BASHRC" "CHANCEC_HOME" "$CHANCEC_HOME_PATH"
persist_export "$HOME_BASHRC" "CHANCECODEC_HOME" "$CHANCECODEC_HOME_PATH"
persist_export "$HOME_BASHRC" "CLD_HOME" "$CLD_HOME_PATH"
persist_export "$HOME_BASHRC" "CHS_HOME" "$CHS_HOME_PATH"
persist_export "$HOME_BASHRC" "CVM_HOME" "$CVM_HOME_PATH"

persist_export "$HOME_PROFILE" "CHANCEC_HOME" "$CHANCEC_HOME_PATH"
persist_export "$HOME_PROFILE" "CHANCECODEC_HOME" "$CHANCECODEC_HOME_PATH"
persist_export "$HOME_PROFILE" "CLD_HOME" "$CLD_HOME_PATH"
persist_export "$HOME_PROFILE" "CHS_HOME" "$CHS_HOME_PATH"
persist_export "$HOME_PROFILE" "CVM_HOME" "$CVM_HOME_PATH"

note "Wrote exports to: $HOME_ZSHRC, $HOME_BASHRC, $HOME_PROFILE"

ok "Done"
