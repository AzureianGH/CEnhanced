# CHance Language Specification (H26)

CHance is compiled by `chancec`, the front-end compiler in this repository. It parses `.ce` source, performs semantic analysis, and emits textual ChanceCode bytecode (`.ccb`) before handing off to `chancecodec` for backend codegen.

For questions or contributions, contact Azureian on Discord or email me@nhornby.com.

This document defines the H26 language standard for CHance as implemented by `chancec`. It describes the full surface area observed in the compiler, runtime, and stdlib sources.

## 1. Toolchain and Artifacts
- **Source:** `.ce`
- **Project manifest:** `.ceproj`
- **Bytecode:** `.ccb` (textual Chance bytecode)
- **Library:** `.cclib` (metadata + optional embedded `.ccbin`)
- **Backend:** `chancecodec` consumes `.ccb` and emits assembly/object/executable.

The compiler always links in `stdlib.cclib` and `runtime.cclib` when not in freestanding mode. The language standard reserves the right to emit or depend on runtime functions provided by these libraries.

## 2. Lexical Elements
### 2.1 Whitespace and Comments
- Whitespace separates tokens and is otherwise insignificant.
- Line comments: `// ...`
- Block comments: `/* ... */` (non-nesting).

### 2.2 Identifiers
- Start: letter or `_`
- Continue: letter, digit, or `_`

### 2.3 String Literals
- Double-quoted: `"..."`
- Escapes: `\\`, `\"`, `\n`, `\r`, `\t`, `\v`, `\b`, `\a`, `\f`, `\?`, `\e`, `\xNN`, `\uNNNN`, `\UNNNNNNNN`, octal.
- Adjacent string literals concatenate.

### 2.4 Character Literals
- Single-quoted: `'a'`
- Escapes match string escapes.

### 2.5 Numeric Literals
- **Integers:**
  - Decimal or prefixed bases: `0x` (hex), `0b` (bin), `0o` (oct), `0d` (dec).
  - Optional suffixes:
    - `u`/`U` => unsigned.
    - `l`/`L` (one or more) => 64-bit width.
  - If no suffix is present, the compiler uses `i32` or `u32` by default, widening to 64-bit when the value does not fit.
- **Floats:**
  - Decimal with optional fractional and exponent parts.
  - Optional `f`/`F` suffix forces `f32`; otherwise `f64`.

## 3. Keywords
`fun`, `action`, `ret`, `stack`, `static`, `reg`, `struct`/`struc`, `extend`, `from`,
`module`, `bring`, `hide`, `expose`, `i8`, `u8`, `i16`, `u16`, `i32`, `u32`, `i64`, `u64`,
`f32`, `f64`, `f128`, `long`, `ulong`, `int`, `uint`, `short`, `ushort`, `byte`, `ubyte`,
`float`, `double`, `constant`, `void`, `char`, `bool`, `var`, `if`, `else`, `switch`,
`case`, `default`, `while`, `match`, `break`, `continue`, `enum`, `alias`, `as`,
`sizeof`, `typeof`, `alignof`, `offsetof`, `null`, `noreturn`.

Boolean literals: `true`, `false`.

## 4. Types
### 4.1 Scalar Types
- Signed: `i8`, `i16`, `i32`, `i64`
- Unsigned: `u8`, `u16`, `u32`, `u64`
- Aliases: `int`, `uint`, `short`, `ushort`, `long`, `ulong`, `byte`, `ubyte`
- Floating: `f32`, `f64`, `f128`, `float`, `double`
- Other: `char`, `bool`, `void`, `va_list`

### 4.2 Compound Types
- Pointer: `T*` (any depth)
- Array: `T[N]` or `T[]` (unsized; can be inferred from initializer)
- Struct: `struct Name { ... }`
- Enum: `enum Name { ... }`
- Function pointer: `fun* (T1, T2, -> Ret)`
- Action type: `action (T1, T2, -> Ret)`

### 4.3 Function Signatures
- `fun name(params) -> Ret`
- `action (params) -> Ret` produces a function-pointer-like type.
- Varargs: `...` or `_vaargs_` must be final and require at least one explicit parameter.

## 5. Declarations
### 5.1 Functions
```
fun name(params) -> Ret { ... }
```
- `expose` marks exported symbols.
- `noreturn` requires `void` return type.
- `inline` status is inferred by attributes; inline candidates are tracked during sema.

### 5.2 Variables
- `type name;`
- `type name = expr;`
- `constant type name = value;`
- Type inference: `var name = expr;` (initializer required).

### 5.3 Structs and Enums
```
struct S { i32 a; f64 b; };

enum E { A, B=2, C };
```
- Forward declaration allowed: `struct Name;`.

### 5.4 Aliases
- `alias Name = Type;`
- Generic alias supports only pointer-pattern form: `alias Name<T> = T*;` (repeated pointer depth allowed).

## 6. Expressions
### 6.1 Operators
- Arithmetic: `+`, `-`, `*`, `/`, `%`
- Bitwise: `&`, `|`, `^`, `~`, `<<`, `>>`
- Logical: `&&`, `||`, `!`
- Comparison: `==`, `!=`, `<`, `<=`, `>`, `>=`
- Assignment: `=`
- Compound assignment: `+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`, `<<=`, `>>=`
- Ternary: `cond ? a : b`
- Member access: `.`, `=>`, `->`
- Indexing: `arr[i]`
- Call: `f(...)` and indirect call via function pointer
- Cast: `expr as Type` and C-style `(Type) expr`
- Address-of: `&expr`
- Dereference: `*expr`

### 6.2 Operator Precedence (high to low)
1. Postfix: `()`, `[]`, `.`, `->`, `=>`, `as`
2. Unary: `+`, `-`, `!`, `~`, `*`, `&`, `++`, `--`
3. Multiplicative: `*`, `/`, `%`
4. Additive: `+`, `-`
5. Shifts: `<<`, `>>`
6. Relational: `<`, `<=`, `>`, `>=`
7. Equality: `==`, `!=`
8. Bitwise AND: `&`
9. Bitwise XOR: `^`
10. Bitwise OR: `|`
11. Logical AND: `&&`
12. Logical OR: `||`
13. Ternary: `?:` and `if (cond) expr else expr`
14. Assignment and compound assignment

### 6.3 Built-in Expressions
- `sizeof(T|expr)` returns byte size (default type `i32`, may be coerced by context when `--implicit-sizeof` is enabled).
- `alignof(T|expr)` returns alignment in bytes (default type `i32`).
- `offsetof(T, field)` returns offset in bytes (default type `i32`).
- `typeof(expr|T)` yields a type.
- `null` is the null pointer literal.
- `__FUNCTION__` yields the current function name as a string literal.

### 6.4 Literal and String Behavior
- Adjacent string literals concatenate.
- Char literals are integer nodes typed as `char`.
- Integer literals default to `i32` or `u32` and widen when required.

## 7. Statements and Control Flow
- `if (cond) stmt [else stmt]`
- `while (cond) stmt`
- `for (init; cond; step) stmt`
- `switch (expr) { case X: ... default: ... }`
- `match (expr) { pattern => expr, _ => expr }`
- `break`, `continue`, `ret`
- Block: `{ ... }`

`match` is an expression; current pattern support is expression or `_` wildcard only.

## 8. Initialization
- Zero init: `{}` or `{0}`
- Aggregate init lists: `{a, b, c}`
- Designated init: `{ .field = value, ... }`
- Arrays and structs accept nested init lists in aggregate form.

## 9. Modules and Imports
- Module declaration: `module A.B;`
- Import: `bring A.B;` or `bring A.B as Alias;`
- Visibility: `hide` and `expose`
- Qualified types `Module.Type` resolve through module registry and can remain as import placeholders until resolved.

## 10. FFI and Externs
```
extend from "C" i32 printf(char*, ...);
extend fun name(params) -> ret;
```
- Default ABI for `extend fun` is `C`.
- Varargs are allowed and must be the final parameter.

## 11. Generics and Constraints
- Function templates: `fun f<T>(...) -> ...`
- Constraints: `integral`, `floating`, `numeric`, `pointer`
- Default template types are allowed.

## 12. Attributes and Metadata
- `[EntryPoint]` program entry.
- `[Inline]`, `[ForceInline]`
- `[Preserve]`
- `[Export]`
- `[Section("name")]`
- `[ChanceCode]` literal ChanceCode body.
- `[Literal]` verbatim body.
- `[OverrideMetadata(".func ...")]`, `[OverrideMetadata(".params ...")]`, `[OverrideMetadata(".locals ...")]`
- `[MetadataCall("name")](args...)`

`ForceInline` requires a literal body. `Literal` bodies must contain at least one non-blank line.

## 13. Preprocessor
- Directives: `#define`, `#undef`, `#if`, `#elif`, `#else`, `#endif`, `#ifdef`, `#ifndef`, `#error`, `#warn`, `#note`, `#static_assert`.
- Conditional expressions support `defined`, arithmetic, comparisons, and logical operators.
- Built-in macros include: `__CHANCE__`, `__CHANCE_VERSION__` family, `__POINTER_WIDTH__`, `__IS64BIT__`, `__TARGET_ARCH__`, `__LINE__`, `__COUNTER__`, `__FILE__`, `__MODULE__`, `__DATE__`, `__TIME__`, and platform flags.

Macro expansion skips strings/chars/comments, and has recursion guards.

## 14. Project Files (`.ceproj`)
Key/value pairs, one per line. Known keys:
- `ce`, `source`, `ccb`, `cclib`, `library`, `obj`, `object`
- `include`, `includedir`
- `output`
- `type=Library|Object`
- `backend`
- `target_os`
- `stop_after=ccb|asm`
- `freestanding=true|false`
- `no_link=true|false`
- `args=...` or `control_args=...` to pass CLI options
- `after={ ... }` to run shell commands after a successful build

## 15. CLI Behavior
- `chancec` without arguments prints help.
- `--version` prints the compiler and standard version.
- The compiler can emit `.ccb`, `.ccbin`, `.cclib`, assembly, objects, or executables depending on options.

Relevant options include `--implicit-voidp` and `--implicit-sizeof`, which allow implicit conversions in pointer and sizeof contexts, respectively.

## 16. Runtime and Stdlib
- `runtime.cclib` is part of the standard environment. The compiler may emit calls to runtime helpers even if they do not appear in source code.
- Runtime exports include `__cert__malloc`, `__cert__free`, and `__cert__panic`.
- Stdlib modules are implemented in CHance and expose IO, memory, file, string, and network helpers via `extend from "C"` and `expose`.

## 17. Constraints and Notes
- Generic aliases only support pointer patterns.
- Varargs must be final and require at least one explicit parameter.
- `match` has no guard or binding syntax.
- `reg` and `stack` are accepted keywords but have no enforced semantic effect beyond parsing.
