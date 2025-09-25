# CHance Language (early WIP)

CHance is a C-like systems language aimed at OS and general programming. This repo currently contains a minimal bootstrap compiler written in C that can compile a tiny subset to a Windows x64 PE executable without invoking an external assembler or linker.

Highlights (current stage):
- Minimal parser for: `fun main() -> i32 { ret <int or sum>; }`
- Codegen directly to x64 PE with a tiny header and one .text section
- CLI: `chancec -o out.exe [-S] [--freestanding] [-m64] input.ce`
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
chancec -o out.exe program.ce
out.exe
```

Enable `-S` to emit a pseudo-asm `.S` file next to the executable for inspection.

## Notes
- This is intentionally minimal to establish the toolchain skeleton. The backends and front-end are designed to be modular (lexer, parser, AST, codegen are separate units under `src/`).
- No Ninja is used or required.