# Architecture Overview

This repository contains a small ahead-of-time compiler for a C-like language. The pipeline is classic and compact:

1) Lexing (tokenization)
- File: src/lexer.c
- Input: a SourceBuffer (raw code)
- Output: a stream of Token values (TokenKind + optional payload like int_val)

2) Parsing (AST construction)
- File: src/parser.c
- Input: tokens via the Lexer API
- Output: an AST (struct Node) for a translation unit (ND_UNIT) containing ND_FUNC nodes and statements/expressions

3) Semantic Analysis (type-checking, symbol tables)
- File: src/sema.c
- Input: AST
- Output: either diagnostics (errors) or a typed AST
- Also builds a tiny symbol table for extern functions

4) Code generation (x64, COFF/PE, portable GAS Intel syntax)
- File: src/codegen_coff_x64.c
- Emits an .S then assembles/links via `cc` (MinGW/Clang/GCC). Also has a tiny direct-PE writer for constant-return programs.

## Key Data Types

- TokenKind (src/ast.h): enumerates all tokens (keywords, punctuation, operators)
- NodeKind (src/ast.h): enumerates AST node kinds (expressions and statements)
- TypeKind / Type (src/ast.h): basic scalar types, pointer and struct
- Node (src/ast.h): AST node with union-like fields for expr/stmt variants
- Symbol/SymTable (src/ast.h + sema.c): extern functions table

## File-by-File

- src/ast.h: shared enums, structs, and function prototypes
- src/lexer.c: whitespace/comments skipping, literals, identifiers, operators
- src/parser.c: recursive-descent parser organized by precedence levels
- src/sema.c: type rules for each NodeKind, scope for locals, var decls
- src/codegen_coff_x64.c: codegen using an x64 assembly backend with Windows and Linux modes; generates and links
- src/includes.*: minimal include/extern scanning support
- src/main.c: command-line driver, builds SourceBuffer, runs pipeline

## Build and Test

- CMake-based: run the provided tasks in VS Code or use `cmake --preset`.
- Tests live under `tests/examples`. The CTest file (`tests/CMakeLists.txt`) wires `.ce` examples into executable tests using the `chancec` compiler.
- Typical iteration loop: edit source -> build -> run tests.

## Adding Language Features

Every new feature flows through the same stages:

- Token: add a TokenKind in `ast.h` and make the lexer recognize it in `lexer.c`.
- Parse: extend parser productions to build an AST node; add a NodeKind if needed.
- Sema: add type rules in `sema.c` so invalid inputs error out and valid AST gets types.
- Codegen: map the node to x64 instructions in `codegen_coff_x64.c`.
- Tests: add minimal `.ce` examples under `tests/examples` and register them in `tests/CMakeLists.txt`.

The three how-to guides show concrete, end-to-end examples from simple to complex.
