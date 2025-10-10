# How to Add a Complex Statement: `switch`

Adding a multi-branch statement like `switch` (with `case`, `default`, and fallthrough) showcases parser design, AST wiring, and control-flow codegen.

We’ll sketch a pragmatic implementation plan consistent with this codebase.

## Feature Shape

Syntax (example):
```
switch (expr) {
  case 1:
    stmt;
  case 2:
    stmt;
    break;
  default:
    stmt;
}
```
Assume integer `expr` and integer `case` labels. Fallthrough is allowed unless `break` is used. `break` exits the `switch`.

## 1) Tokens

- Add keywords/tokens: `switch`, `case`, `default`, `break`.
  - TokenKinds: TK_KW_SWITCH, TK_KW_CASE, TK_KW_DEFAULT, TK_KW_BREAK
  - Update `lexer.c` keyword table.

## 2) AST Nodes

- NodeKind additions:
  - `ND_SWITCH`, `ND_CASE`, `ND_BREAK`
- Suggested shapes:
  - `ND_SWITCH`: `lhs` = controlling expression; `body` = ND_BLOCK of case nodes or raw statements; store arrays of case labels + target labels in auxiliary fields as needed
  - `ND_CASE`: `lhs` = integer literal expr for label; `rhs` = statement that follows (could be ND_BLOCK), or simply attach into the surrounding block sequence
  - `ND_BREAK`: no children

Alternative: lower `switch` during parsing/sema to an explicit chain of labels and `if`/`goto`-like constructs. This repo doesn’t have gotos yet, so we’ll keep a higher-level node and do lowering in codegen.

## 3) Parser

- Add `parse_switch()` in `parser.c` alongside `parse_while/for/if`.
  - Expect: `switch ( expr ) { cases }`
  - Inside braces: a sequence of `case constant:`, `default:` labels interleaved with statements until `}`.
  - Build a linear `ND_BLOCK` for the body, with inline markers for `ND_CASE` and `ND_DEFAULT` (you can reuse `ND_CASE` with a flag or add `ND_DEFAULT`).
  - Recognize `break;` as `ND_BREAK` inside switch.

## 4) Sema

- `ND_SWITCH`:
  - Type-check the controlling expression to an integer type.
  - For each `ND_CASE`: constant-fold the label expression; ensure unique labels.
  - Track that `break` is only valid within `switch` (and `while` if you add it there too). Implement via a depth counter or a context flag.

## 5) Codegen

- In `codegen_coff_x64.c`, lower `ND_SWITCH` into labels and conditional jumps:
  - Evaluate the controlling expr into `rax` once.
  - For N cases, choose a simple lowering: a sequence of compares and jumps:
    ```
    cmp rax, imm
    je .Lcase0
    cmp rax, imm
    je .Lcase1
    ...
    jmp .Ldefault_or_end
    .Lcase0:
      ... body ...
      jmp .Lswitch_end
    .Lcase1:
      ... body ...
      jmp .Lswitch_end
    .Ldefault:
      ...
    .Lswitch_end:
    ```
  - If you support fallthrough, do not jump to end after each case unless you see an explicit `break` (insert `jmp .Lswitch_end` on ND_BREAK codegen).
  - Maintain a per-switch end label in the `AsmCtx` while emitting the body.

Optimization note: a jump table is possible later; start with compares.

## 6) Tests

- Add examples under `tests/examples/`:
  - `switch_basic.ce`: selection among a few constants
  - `switch_fallthrough.ce`: adjacent cases without break
  - `switch_default.ce`: only default branch executes
  - Register with `add_ce_test(...)` and run CTest.

## 7) Iteration Tips

- Start by building the parser skeleton (tokens + parse tree) and printing the AST to sanity-check shape, then add sema rules, then codegen.
- Keep the first implementation strict and simple; extend later (e.g., allow ranges, non-const labels, or non-integers) only when needed.

With `switch` you will touch scanning, parsing, type-checking, control-flow lowering, and add a new statement kind—this is representative of complex language feature work in this compiler.
