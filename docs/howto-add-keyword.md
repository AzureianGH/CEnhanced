# How to Add a New Keyword

This short guide shows how to add a new reserved word to the language. We'll add a dummy keyword `opaque` to demonstrate the steps. You can adapt the pattern to implement real features (like `break`, `switch`, etc.).

## 1) TokenKind entry

File: `src/ast.h`
- Add a `TK_KW_OPAQUE` entry to the TokenKind enum in the keywords region.

Example:
```c
    TK_KW_ENUM,
    TK_KW_ALIAS,
    TK_KW_AS,
    TK_KW_SIZEOF,
    TK_KW_TYPEOF,
    TK_KW_OPAQUE,   // new keyword
    // punctuation
```

## 2) Lexer recognition

File: `src/lexer.c`
- In `lex_ident_or_kw`, add a branch mapping the identifier text to the new token kind. Keep the list in alphabetical-ish order to stay tidy.

Example snippet inside `lex_ident_or_kw`:
```c
    else if (len == 6 && strncmp(p, "opaque", 6) == 0)
        k = TK_KW_OPAQUE;
```

Now the lexer will emit `TK_KW_OPAQUE` whenever it sees the exact word `opaque` in source (not part of a bigger identifier).

## 3) Parser usage

Decide how the keyword is used. Three common patterns:
- A new statement (like `break;`): add a case in `parse_stmt` that consumes the token and builds a new node kind (e.g., `ND_BREAK`).
- A type keyword (like `void`): add it to `is_type_start()` and `parse_type_spec()` and map to a `TypeKind`.
- A modifier or storage class (like `stack`): accept it before a type and adjust metadata on the node.

For a no-op demonstrator, you might parse `opaque;` as an empty statement and discard it:
```c
    if (t.kind == TK_KW_OPAQUE) {
        lexer_next(ps->lx); // consume 'opaque'
        expect(ps, TK_SEMI, ";");
        // build a placeholder node or ignore
        Node *es = new_node(ND_EXPR_STMT);
        es->lhs = NULL;
        return es;
    }
```

If you implement a real feature (e.g., `break`):
- Add `ND_BREAK` to `NodeKind`.
- In `parse_stmt`, produce an `ND_BREAK` node on `TK_KW_BREAK`.
- In `sema.c`, validate that `break` appears inside loop/switch contexts.
- In `codegen`, translate `ND_BREAK` to a jump to the loop/switch end label.

## 4) Sema and Codegen (as needed)

If your keyword forms a new statement or expression, add type rules in `sema.c` and emission logic in `codegen_coff_x64.c`. If it’s only syntactic sugar or a storage-class with no current effect, you can accept and ignore it (document this behavior).

## 5) Tests

Add small examples under `tests/examples/` and register them in `tests/CMakeLists.txt`. For a statement keyword, create a sample function that uses it and verify either behavior or that compilation succeeds and the program returns the expected value.

With these steps, you can add new reserved words cleanly, then layer in parser, sema, and codegen behavior incrementally.
