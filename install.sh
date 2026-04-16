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

make_with_cores() {
  make_bin="$1"
  shift
  if [ -n "$MAKE_CORES" ]; then
    "$make_bin" -j "$MAKE_CORES" "$@"
  else
    "$make_bin" "$@"
  fi
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

cmake_cache_value() {
  cache_file="$1"
  cache_key="$2"

  if [ ! -f "$cache_file" ]; then
    return 1
  fi

  sed -n "s/^${cache_key}:[^=]*=//p" "$cache_file" | head -n 1
}

cmake_cache_matches_source() {
  cache_file="$1"
  source_dir="$2"

  cache_source=$(cmake_cache_value "$cache_file" "CMAKE_HOME_DIRECTORY") || return 1
  source_abs=$(CDPATH= cd -- "$source_dir" && pwd)
  [ -n "$cache_source" ] && [ "$cache_source" = "$source_abs" ]
}

cmake_cache_matches_value() {
  cache_file="$1"
  cache_key="$2"
  expected_value="$3"

  cache_value=$(cmake_cache_value "$cache_file" "$cache_key") || return 1
  [ "$cache_value" = "$expected_value" ]
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

MAKE_CORES=""
CHANCECODE_ARG=""
CLD_ARG=""
CHS_ARG=""
CVM_ARG=""
EXTRA_ARG_COUNT=0

while [ "$#" -gt 0 ]; do
  case "$1" in
    --cores=*)
      MAKE_CORES=${1#--cores=}
      shift
      ;;
    --cores)
      if [ "$#" -lt 2 ]; then
        err "error: --cores requires a value"
        exit 1
      fi
      MAKE_CORES="$2"
      shift 2
      ;;
    --)
      shift
      while [ "$#" -gt 0 ]; do
        EXTRA_ARG_COUNT=$((EXTRA_ARG_COUNT + 1))
        case "$EXTRA_ARG_COUNT" in
          1) CHANCECODE_ARG="$1" ;;
          2) CLD_ARG="$1" ;;
          3) CHS_ARG="$1" ;;
          4) CVM_ARG="$1" ;;
        esac
        shift
      done
      ;;
    -*)
      err "error: unknown option '$1'"
      exit 1
      ;;
    *)
      EXTRA_ARG_COUNT=$((EXTRA_ARG_COUNT + 1))
      case "$EXTRA_ARG_COUNT" in
        1) CHANCECODE_ARG="$1" ;;
        2) CLD_ARG="$1" ;;
        3) CHS_ARG="$1" ;;
        4) CVM_ARG="$1" ;;
      esac
      shift
      ;;
  esac
done

if [ -n "$MAKE_CORES" ]; then
  case "$MAKE_CORES" in
    ''|*[!0-9]*|0)
      err "error: --cores must be a positive integer"
      exit 1
      ;;
  esac
fi

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
CHANCECODE_DIR="${CHANCECODE_ARG:-$SCRIPT_DIR/../ChanceCode}"
CLD_DIR="${CLD_ARG:-$SCRIPT_DIR/../CLD}"
CHS_DIR="${CHS_ARG:-$SCRIPT_DIR/../CHS}"
CVM_DIR="${CVM_ARG:-$SCRIPT_DIR/../CVM}"

if [ "$EXTRA_ARG_COUNT" -gt 4 ]; then
  warn "warning: extra positional arguments after the first 4 are ignored"
fi
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
MAKE_BIN=$(find_make)
if [ -z "$MAKE_BIN" ]; then
  err "error: no make tool found (need make or mingw32-make)"
  exit 1
fi
log "Building CHance compiler (pure make)"
(cd "$SCRIPT_DIR" && rm -f build/obj/src/main.o build/chancec)
(cd "$SCRIPT_DIR" && \
  CHANCE_DEFAULT_RUNTIME="$DEFAULT_RUNTIME" \
  CHANCE_DEFAULT_STDLIB="$DEFAULT_STDLIB" \
  make_with_cores "$MAKE_BIN" all)
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
  (cd "$CHS_DIR" && make_with_cores "$MAKE_BIN")
  ok "CHS build complete"
fi

if [ -n "$CVM_DIR" ] && [ -f "$CVM_DIR/CMakeLists.txt" ]; then
  if ! command -v cmake >/dev/null 2>&1; then
    err "error: cmake is required to build CVM"
    exit 1
  fi

  CVM_BUILD_DIR="$CVM_DIR/build"
  CVM_CACHE_FILE="$CVM_BUILD_DIR/CMakeCache.txt"
  NEED_CVM_CONFIGURE=1
  if [ "${CHANCE_FORCE_CONFIGURE:-0}" != "1" ] && \
     cmake_cache_matches_source "$CVM_CACHE_FILE" "$CVM_DIR" && \
     cmake_cache_matches_value "$CVM_CACHE_FILE" "CHANCECODE_ROOT" "$CHANCECODE_DIR"; then
    NEED_CVM_CONFIGURE=0
  fi

  if [ "$NEED_CVM_CONFIGURE" -eq 1 ]; then
    log "Configuring CVM"
    (cd "$CVM_DIR" && cmake -S . -B build -DCHANCECODE_ROOT="$CHANCECODE_DIR")
  else
    note "Using existing CVM CMake cache (skipping configure)"
  fi

  log "Building CVM"
  (cd "$CVM_DIR" && cmake --build build)
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
  (cd "$CLD_DIR" && make_with_cores "$MAKE_BIN" install PREFIX="$PREFIX")
fi

if [ -n "$CHS_DIR" ] && [ -f "$CHS_DIR/Makefile" ]; then
  MAKE_BIN=$(find_make)
  if [ -z "$MAKE_BIN" ]; then
    err "error: no make tool found (need make or mingw32-make)"
    exit 1
  fi
  log "Installing CHS"
  (cd "$CHS_DIR" && make_with_cores "$MAKE_BIN" install PREFIX="$PREFIX")
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
