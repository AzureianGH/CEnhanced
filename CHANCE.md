# CHance Language Reference (Full List)

A compact, C-inspired systems language used by the `chancec` toolchain. This reference enumerates the language surface as implemented by the compiler.

## Toolchain
- `chancec` compiles `.ce` to Chance bytecode (`.ccb`) or assembly via ChanceCode backends.
- `chancecodec` consumes `.ccb` and emits target-specific output.

## Lexical Forms
- **Comments:** `// line` and `/* block */`.
- **String literals:** double-quoted, supports escapes `\\`, `\"`, `\n`, `\r`, `\t`, `\v`, `\b`, `\a`, `\f`, `\?`, `\e`, `\xNN`, `\uNNNN`, `\UNNNNNNNN`, octal.
- **Char literals:** single-quoted with the same escapes as strings.
- **Integer literals:** decimal or prefixed base `0x` (hex), `0b` (bin), `0o` (oct), `0d` (dec). Optional `u`/`U` suffix for unsigned.
- **Float literals:** decimal with optional fraction/exponent; optional `f`/`F` suffix for `f32`.

## Keywords
`fun`, `action`, `ret`, `stack`, `static`, `reg`, `struct`/`struc`, `extend`, `from`,
`module`, `bring`, `hide`, `expose`, `i8`, `u8`, `i16`, `u16`, `i32`, `u32`, `i64`, `u64`,
`f32`, `f64`, `f128`, `long`, `ulong`, `int`, `uint`, `short`, `ushort`, `byte`, `ubyte`,
`float`, `double`, `constant`, `void`, `char`, `bool`, `var`, `if`, `else`, `switch`,
`case`, `default`, `while`, `match`, `break`, `continue`, `enum`, `alias`, `as`,
`sizeof`, `typeof`, `alignof`, `offsetof`, `null`, `noreturn`.

`true` and `false` are recognized as boolean literals.

## Operators & Punctuation
- **Arithmetic:** `+`, `-`, `*`, `/`, `%`, `++`, `--`, `+=`, `-=`, `*=`, `/=`, `%=`
- **Bitwise:** `&`, `|`, `^`, `~`, `<<`, `>>`, `&=`, `|=`, `^=`, `<<=`, `>>=`
- **Logical:** `&&`, `||`, `!`
- **Comparison:** `==`, `!=`, `<`, `<=`, `>`, `>=`
- **Assignment:** `=`
- **Ternary:** `?:`
- **Member access:** `.`, `=>`, `->`
- **Indexing/calls:** `[]`, `()`
- **Punctuation:** `;`, `,`, `:`

## Types
- **Integers:** `i8/i16/i32/i64`, `u8/u16/u32/u64`, `int/uint`, `short/ushort`, `long/ulong`, `byte/ubyte`
- **Floats:** `f32/f64/f128`, `float`, `double`
- **Other:** `char`, `bool`, `void`, `va_list`
- **Pointers:** `T*` (any depth)
- **Arrays:** `T[N]` and unsized `T[]` (size inferred from initializer)
- **Structs/Enums:** `struct Name { ... };`, `enum Name { ... };`
- **Function pointers:** `fun* (T1, T2, -> Ret)`
- **Actions:** `action (T1, T2, -> Ret)` (function-pointer style)

## Declarations & Control Flow
- **Functions:** `fun name(params) -> type { ... }`
- **Variables:** `type name;` or `constant type name = value;`
- **Type inference:** `var name = expr;`
- **Control flow:** `if/else`, `while`, `for`, `switch/case/default`, `break`, `continue`, `ret`
- **Match expression:** `match (expr) { pattern => expr, _ => expr }`
- **Initialization:** `{}` zero-init, `{a, b, c}`, designators `.field = value`
- **Casts:** `expr as Type`

## Built-in Expressions
- `sizeof(T|expr)` — size in bytes
- `alignof(T|expr)` — alignment in bytes
- `offsetof(T, field)` — field offset in bytes
- `typeof(expr|T)` — yields a type
- `null` — null pointer literal
- `__FUNCTION__` — current function name as string

## Varargs Builtins
- `va_start()` — create a `va_list`
- `va_arg(list, Type)` — read next argument
- `va_end(list)` — end traversal

## Preprocessor Directives
- `#define`, `#undef`
- `#if`, `#elif`, `#else`, `#endif`
- `#ifdef`, `#ifndef`, `defined(NAME)`
- `#error`, `#warn`, `#note`
- `#static_assert(expr[, "message"])`

## Built-in Preprocessor Macros
- `__CHANCE__`
- `__CHANCE_VERSION__`, `__CHANCE_VERSION_MAJOR__`, `__CHANCE_VERSION_MINOR__`, `__CHANCE_VERSION_PATCH__`, `__CHANCE_VERSION_STR__`
- `__POINTER_WIDTH__`, `__IS64BIT__`
- `__TARGET_ARCH__`, `__ARCH__`
- `__LINE__`, `__COUNTER__`, `__FILE__`, `__MODULE__`, `__DATE__`, `__TIME__`
- Platform flags: `__WIN32__`, `_WIN32`, `__LINUX__`, `__linux__`, `__APPLE__`, `__MACH__`

## Modules & FFI
- `module A.B;` — declare module name
- `bring A.B;` or `bring A.B as Alias;` — import
- `extend from "C" ...;` — C ABI extern declarations
- `hide` / `expose` — visibility control for declarations

## Generics & Constraints
- Function templates: `fun f<T>(...) -> ...`
- Constraints: `T: integral | floating | numeric | pointer`
- Default types: `T = i32` (or `T: i32`)

## Attributes & Metadata
- `[EntryPoint]` — program entry
- `[Inline]`, `[ForceInline]` — inlining hints (`ForceInline` requires literal body)
- `[Preserve]` — prevent removal
- `[Export]` — export symbol
- `[Section("name")]` — custom section
- `[ChanceCode]` — function body in ChanceCode
- `[Literal]` — literal (verbatim) function body
- `[OverrideMetadata(".func ...")]`, `[OverrideMetadata(".params ...")]`, `[OverrideMetadata(".locals ...")]`
- `[MetadataCall("name")](args...)` — emit metadata call expression

## Targets (ChanceCode Backends)
- **x86-64** — NASM-style (`x86`) or GAS `.intel_syntax` (`x86-gas`)
- **arm64 macOS** — Darwin ABI (`arm64-macos`)
- **bslash** — experimental backend (internal)

## Files
- `.ce` — CHance source
- `.ccb` — textual Chance bytecode
- `.ccbin` — binary module (optional)

## Examples (Each Uses Different Features)

### Example 1: Basics, arrays, control flow
```ce
fun sum3(i32 a, i32 b, i32 c) -> i32 { ret a + b + c; }

fun main() -> i32
{
	i32[3] arr = {1, 2, 3};
	i32 i = 0;
	i32 total = 0;
	while (i < 3) { total += arr[i]; i = i + 1; }
	if (total == 6) ret 0; else ret 1;
}
```
**Features used:** functions, arrays, init list, `while`, `if/else`, arithmetic, comparison, `ret`.

### Example 2: Structs, enums, member access
```ce
enum Color { Red, Green=5, Blue };

struct Pixel { i32 x; i32 y; Color c; };

fun is_green(Pixel p) -> bool
{
	ret p.c == Color=>Green;
}
```
**Features used:** `enum`, `struct`, enum scoped access `=>`, member access `.`, bools.

### Example 3: Function pointers, actions, lambdas
```ce
fun add(i32 a, i32 b) -> i32 { ret a + b; }

fun call_it(fun* (i32, i32, -> i32) f) -> i32
{
	ret f(2, 3);
}

fun main() -> i32
{
	action (i32, -> i32) inc = fun (i32 x) -> i32 { ret x + 1; };
	ret call_it(&add) + inc(4);
}
```
**Features used:** function pointers, `action` types, lambdas, address-of, calls.

### Example 4: Generics + constraints
```ce
fun max<T: integral>(T a, T b) -> T { if (a > b) ret a; ret b; }

fun main() -> i32
{
	ret max<i32>(10, 7);
}
```
**Features used:** generics, constraints, explicit type args, comparisons.

### Example 5: Built-ins + varargs
```ce
fun main() -> i32
{
	i32 size = sizeof(i64);
	i32 align = alignof(i64);
	i32 off = offsetof(struct { i8 a; i64 b; }, .b);
	va_list v = va_start();
	i32 x = va_arg(v, i32);
	va_end(v);
	ret size + align + off + x;
}
```
**Features used:** `sizeof`, `alignof`, `offsetof`, `va_start/va_arg/va_end`.

### Example 6: Preprocessor + built-in macros
```ce
#define LIMIT 3
#if defined(__CHANCE__) && __POINTER_WIDTH__ >= 64
#note "64-bit build"
#endif

fun main() -> i32
{
	i32 count = 0;
	for (i32 i = 0; i < LIMIT; i = i + 1) { count += i; }
	ret count; // uses __LINE__ implicitly for diagnostics
}
```
**Features used:** `#define`, `#if`, `defined`, `#note`, built-in macros, `for`.

### Example 7: Modules + FFI + attributes
```ce
module Demo.App;
bring Std;

extend from "C" i32 puts(char*);

[EntryPoint]
expose fun main() -> i32
{
	puts("Hello from CHance");
	ret 0;
}
```
**Features used:** `module`, `bring`, `extend from "C"`, attributes, `expose`.
