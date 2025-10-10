# How to Add a Simple Operator (<< >>)

This guide walks through adding bit shifts (<< and >>) end to end. The same pattern applies to many small features.

What you’ll do:
- Tokenize `<<` and `>>`
- Parse them at the right precedence
- Type-check integers only
- Emit x64 shifts (`shl`, `shr`/`sar`)
- Add tests

## 1) Tokens

File: `src/ast.h`
- Add TokenKind entries:
  - `TK_SHL` // <<
  - `TK_SHR` // >>

File: `src/lexer.c`
- In the punctuation handling for `<` and `>`, prefer two-char forms first:
  - If next is `<`, emit `TK_SHL` (consume 2 chars)
  - Else if next is `=`, emit `TK_LTE`
  - Else single `<` -> `TK_LT`
  - Similarly for `>`: prefer `>>` then `>=`, else `>`

Gotcha: Always check 2-char tokens before 1-char tokens to avoid splitting `<<` into two `<` tokens.

## 2) AST and Parser

File: `src/ast.h`
- Add NodeKind entries:
  - `ND_SHL`, `ND_SHR`

File: `src/parser.c`
- Introduce a new precedence layer between additive (`+ -`) and relational (`< > <= >=`). Example structure:
  - primary -> postfix -> unary -> mul -> add -> shift -> relational -> equality -> logical-and -> logical-or -> conditional -> assignment
- Implement `parse_shift()` that consumes a left operand from `parse_add()` and loops while next token is `TK_SHL` or `TK_SHR`, building `ND_SHL`/`ND_SHR` nodes.
- Make `parse_rel()` consume from `parse_shift()` instead of `parse_add()`.

## 3) Sema (Type Checking)

File: `src/sema.c`
- In `check_expr`, add a case for `ND_SHL` and `ND_SHR`:
  - Type-check both sides
  - Require integer types on both operands
  - Result type = left operand type (no implicit promotion here)

Tip: The codebase already has helpers like `type_is_int`.

## 4) Codegen (x64)

File: `src/codegen_coff_x64.c`
- In `asm_eval_expr_to_rax`, add a case for `ND_SHL` and `ND_SHR`:
  - Evaluate `lhs` to RAX, push it; evaluate `rhs`, move the low 8 bits of the count to `CL`; pop RAX back.
  - Use width-specific instructions based on the left type: `al/ax/eax/rax` size.
  - For right shift, choose `shr` for unsigned types and `sar` for signed types so the sign bit propagates for signed integers.
  - Finally, call the existing `asm_trunc_to_kind` helper to normalize the result width/sign-extension according to the expression type.

## 5) Tests

- Add examples:
  - `tests/examples/shift_left.ce`:
    ```
    fun main() -> int { ret 21 << 1; }
    ```
  - `tests/examples/shift_right.ce`:
    ```
    fun main() -> int { ret 84 >> 1; }
    ```
- Register in `tests/CMakeLists.txt` with `add_ce_test(shift_left shift_left.ce 42)` and similarly for right shift. Optionally add freestanding variants.

## Validation Checklist

- Build passes.
- New tests pass in both hosted and freestanding modes.
- Negative tests: `1.0 << 2` or `"x" << 1` should fail sema.

That’s it! For many simple operators (bitwise &, |, ^, ~, etc.), you’ll follow the same steps: token -> AST -> parser precedence -> sema rule -> codegen -> tests.
