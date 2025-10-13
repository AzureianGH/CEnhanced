# CHance Language (early WIP)

CHance is a C-like systems language aimed at OS and general programming. This repo currently contains a minimal bootstrap compiler written in C that can compile a tiny subset to a Windows x64 PE executable without invoking an external assembler or linker.

Highlights (current stage):
- Minimal parser for: `fun main() -> i32 { ret <int or sum>; }`
- Codegen directly to x64 PE with a tiny header and one .text section
- CLI: `chancec -x86 [-S|-Sccb] [-c [obj]] [--freestanding] [-m64] [-o out.exe] input.ce`
- CTest integration with examples returning specific exit codes

Planned:
- Full lexer/parser for structs, variables, calling conventions
- 32-bit and 64-bit bytecode backends and modular targets
- C interoperability (headers) and a small linker
- Rewrite the compiler in CHance itself (.ce)

## Build (Windows, Visual Studio or NMake)

Use CMake with a non-Ninja generator, e.g. Visual Studio 2022 or NMake Makefiles.

- Visual Studio:
  - Configure: `cmake -S . -B build -G "Visual Studio 17 2022" -A x64`
  - Build: `cmake --build build --config Release`
  - Test: `ctest --test-dir build -C Release --output-on-failure`

- NMake:
  - Configure: `cmake -S . -B build -G "NMake Makefiles"`
  - Build: `cmake --build build --config Release`
  - Test: `ctest --test-dir build -C Release --output-on-failure`

## Usage

```
# Compile and link to an executable (requires selecting a backend)
chancec -x86 program.ce
program.exe

# Emit assembly only and keep the .S file
chancec -x86 -S program.ce

# Stop after Chance bytecode emission
chancec -Sccb program.ce
```

Use `-x86` to select the current x86-64 backend whenever you need assembly, object, or executable output. Combine with `-c` to keep object files, `--freestanding` for freestanding builds, or `-O1`/`-O2` for optimization.

## Notes
- This is intentionally minimal to establish the toolchain skeleton. The backends and front-end are designed to be modular (lexer, parser, AST, codegen are separate units under `src/`).
- No Ninja is used or required.