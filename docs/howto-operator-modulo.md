# How to Add a Medium Complexity Operator (%)

As a slightly more involved feature, let’s add integer modulo `%`. It’s similar to division but with a few details to get right.

Scope:
- Tokens: add `%`
- Parser: place at multiplicative precedence with `*` and `/`
- Sema: integers only, type = left operand type
- Codegen: `idiv`/`div` produce remainder in `rdx/edx`
- Tests: a few cases including negatives and unsigned

## 1) Token

- Add a `TK_PERCENT` in `TokenKind` (src/ast.h).
- In `lexer.c`, when `c == '%'`, emit `TK_PERCENT`.

## 2) AST and Parser

- Add `ND_MOD` in `NodeKind` (src/ast.h).
- In `parse_mul` (src/parser.c), treat `TK_PERCENT` like `*` and `/`, producing an `ND_MOD` node.

## 3) Sema Rules

- In `check_expr` (src/sema.c), mirror the `ND_DIV` branch:
  - Check both operands
  - Require same integer type on both sides (keep it simple like other ops)
  - Result type is the left operand type

Optional: Decide what to do with divide-by-zero at compile time if the RHS is a literal 0 (emit diagnostic), or leave it to runtime.

## 4) Codegen on x64

- In `asm_eval_expr_to_rax` (src/codegen_coff_x64.c):
  - Evaluate `lhs` -> RAX, push; evaluate `rhs` -> RAX move to RCX; pop RAX
  - For signed types (i8/i16/i32/i64): `cdq` (32-bit) or `cqo` (64-bit) then `idiv ecx/rcx`
  - For unsigned types: zero-extend into `edx/rdx` then `div ecx/rcx`
  - The remainder is produced in `edx/rdx`; move it back to `eax/rax` as the expression result
  - Call `asm_trunc_to_kind` to normalize width

Pseudo-snippet:
```
  ... lhs in rax; push rax
  ... rhs in rax; mov rcx, rax; pop rax
  if unsigned:
    xor edx, edx ; div ecx     ; remainder in edx
  else:
    cdq          ; idiv ecx    ; remainder in edx
  mov eax, edx
```
And analogously for 64-bit: `xor rdx, rdx` / `cqo`, `div rcx`/`idiv rcx`, `mov rax, rdx`.

## 5) Tests

Add examples under `tests/examples/` and register them in `tests/CMakeLists.txt`:
- `mod_small.ce`: `ret 7 % 3;` -> 1
- `mod_unsigned.ce`: declare an unsigned int variable and test modulo
- `mod_negative.ce` (if you support signed remainder): verify expected semantics

Run the CTest suite to confirm.

With `%` implemented, you’ve touched every stage and a bit of codegen nuance (RDX remainder), which is representative of many arithmetic extensions.
