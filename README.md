# CHance Front-End

This repository contains the CHance language front-end and driver (`chancec`). It takes `.ce` sources, performs lexing, parsing, semantic analysis, and emits Chance bytecode (`.ccb`). When requested it hands that bytecode to [`ChanceCode`](../ChanceCode) to obtain assembly, objects, or executables, and it can package reusable metadata into `.cclib` libraries.

The codebase is intentionally compact so new language features can be added by touching a small set of files. The architecture notes in `docs/` walk through the full pipeline.

## What Lives Here
- C-like language front-end with modules, attributes, structs, enums, pointers, and integer/float arithmetic.
- Incremental preprocessor (`#include`, `extend from "C"`, conditional compilation tags).
- Semantic analysis with symbol tables, type inference for literals, and host/freestanding mode switches.
- Bytecode writer (`codegen_ccb.c`) that lowers typed ASTs to Chance bytecode v2.
- CLI driver that can:
	- Compile individual `.ce` files or multi-file manifests (`.ceproj`).
	- Launch `chancecodec` to generate NASM/Intel/AT&T assembly, object files, or PE/ELF executables.
	- Emit standalone `.ccb` or `.cclib` artifacts for consumption by other tools.
	- Scaffold new projects via `chancec new Console MyApp`.

## Toolchain Flow
```
	 source.ce / project.ceproj
							│
				 chancec (this repo)
							│    ┌─────────── emits ───────────┐
		Chance bytecode (.ccb)   optional cclib metadata
							│
			 chancecodec (ChanceCode)  ← auto-discovered or passed via --chancecodec
							│
		assembly / object / executable / ccbin
```

`chancec` only links when a backend is selected (currently `-x86`). Without a backend it stops after writing `.ccb`, which is useful for testing or feeding third-party tools.

## Prerequisites
- CMake 3.23+
- A C11 compiler (tested with MSVC 17.x, Clang, and MinGW-w64 GCC)
- Optional: NASM or a system assembler for converting backend output into binaries
- This repository expects [`ChanceCode`](https://github.com/AzureianGH/ChanceCode) to be checked out as a sibling directory so headers under `ChanceCode/include` are available.

On Windows the provided tasks and scripts assume PowerShell. `build.bat` delegates to the `mingw-release` preset and falls back to a local MinGW build.

## Building
You can drive the build with CMake presets:

```
# Configure + build (MinGW)
cmake --preset mingw-release
cmake --build --preset mingw-release

# Configure + build (Visual Studio 2022, x64)
cmake --preset vs2022-release
cmake --build --preset vs2022-release

# Run tests
ctest --preset mingw-release
```

Artifacts land under `build/<preset>/`. The executable is typically at `build/<preset>/chancec.exe` on Windows.

Helper scripts:
- `build.bat [preset]` — invokes `cmake --build --preset` and falls back to a MinGW Makefiles build.
- `build.sh [preset]` — equivalent flow for Unix-like environments.

## Using `chancec`

Basic examples:

```
# Compile and link with the x86-64 backend
chancec -x86 samples/hello.ce -o hello.exe

# Keep the Chance bytecode and stop before backend translation
chancec -Sccb samples/hello.ce -o out/hello.ccb

# Emit assembly through ChanceCode but skip linking
chancec -x86 -S --no-link samples/hello.ce --asm-syntax nasm

# Package exposed modules into a reusable library
chancec --library stdlib/stdlib.ceproj -o dist/stdlib.cclib
```

Notable switches:
- `-x86` — select the built-in x86-64 backend (required for assembly/object/exe output).
- `-S` — produce assembly via ChanceCode alongside the normal output.
- `-Sccb` — exit after writing `.ccb` (skips backend invocation entirely).
- `-c [path]` or `--no-link [path]` — compile only; optionally choose the object path.
- `--freestanding` — disable CRT assumptions for bare-metal or kernel targets.
- `--target-os windows|linux` — influence backend metadata (PE vs ELF conventions).
- `--asm-syntax intel|att|nasm` — pick the assembler dialect ChanceCode should emit.
- `--chancecodec <path>` — override the auto-detected `chancecodec` executable.
- `--library` — package modules, exported symbols, and embedded `.ccbin` blobs into a `.cclib` archive.
- `-I <dir>` — add include directories for header-style imports.

Environment variables that influence backend discovery:
- `CHANCECODEC_CMD` / `CHANCECODEC` — explicit `chancecodec` path.
- `CHANCECODE_HOME` — root folder containing `chancecodec` or `build/*/chancecodec`.

`chancec` searches relative to its own executable as a fallback, so keeping both repositories side-by-side usually works without manual configuration.

## Project Files and Templates
- `.ceproj` manifests allow multi-file builds. They let you set output names, default backend, target OS, and per-file options. Run `chancec mygame.ceproj` to build everything in one shot.
- `chancec new Console MyGame` scaffolds a project directory with `src/main.ce` and a corresponding manifest. This is handy for quickly experimenting with the language.

## Repository Layout
- `src/` — lexer, parser, semantic analysis, bytecode emitter, CLI, library tooling.
- `src/stdlib/` — prototype standard-library modules written in CHance.
- `tests/` — CTest harness plus `.ce` samples that assert specific exit codes or diagnostics.
- `docs/` — architecture overview and step-by-step guides for extending the compiler.

## Testing
CTest drives the sample suite:

```
ctest --preset mingw-release --output-on-failure
```

Tests exercise hosted and freestanding modes, ensure new language constructs round-trip through the bytecode emitter, and keep the CLI features working.

## Contributing
Open issues or pull requests are welcome. If you are adding syntax or backend hooks, follow the pattern documented in `docs/howto-*` and accompany the change with new tests under `tests/`.