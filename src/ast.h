#ifndef CHANCE_AST_H
#define CHANCE_AST_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef enum
{
    TK_EOF = 0,
    TK_IDENT,
    TK_INT,
    TK_STRING,
    TK_CHAR_LIT,
    // keywords
    TK_KW_FUN,
    TK_KW_RET,
    TK_KW_STACK,
    TK_KW_REG,
    TK_KW_STRUCT,
    TK_KW_STRUC, // alias accepted
    TK_KW_EXTEND,
    TK_KW_FROM,
    TK_KW_I32,
    TK_KW_I64,
    TK_KW_U32,
    TK_KW_U64,
    TK_KW_I16,
    TK_KW_U16,
    TK_KW_I8,
    TK_KW_U8,
    TK_KW_F32,
    TK_KW_FOR,
    TK_KW_F64,
    TK_KW_F128,
    TK_KW_LONG,
    TK_KW_ULONG,
    TK_KW_INT,
    TK_KW_UINT,
    TK_KW_SHORT,
    TK_KW_USHORT,
    TK_KW_BYTE,
    TK_KW_UBYTE,
    TK_KW_FLOAT,
    TK_KW_DOUBLE,
    TK_KW_CONSTANT,
    TK_KW_VOID,
    TK_KW_CHAR,
    TK_KW_IF,
    TK_KW_ELSE,
    TK_KW_WHILE,
    TK_KW_ENUM,
    TK_KW_ALIAS,
    TK_KW_AS,
    TK_KW_SIZEOF,
    TK_KW_TYPEOF,
    TK_KW_NORETURN,
    TK_KW_MODULE,
    TK_KW_BRING,
    TK_KW_HIDE,
    TK_KW_EXPOSE,
    // punctuation
    TK_ARROW,      // ->
    TK_LPAREN,     // (
    TK_RPAREN,     // )
    TK_LBRACE,     // {
    TK_RBRACE,     // }
    TK_LBRACKET,   // [
    TK_RBRACKET,   // ]
    TK_SEMI,       // ;
    TK_COMMA,      // ,
    TK_PLUS,       // +
    TK_PLUSPLUS,   // ++
    TK_ANDAND,     // &&
    TK_OROR,       // ||
    TK_AMP,        // &
    TK_STAR,       // *
    TK_SLASH,      // /
    TK_MINUS,      // -
    TK_MINUSMINUS, // --
    TK_ASSIGN,     // =
    TK_EQEQ,       // ==
    TK_BANGEQ,     // !=
    TK_LT,         // <
    TK_GT,         // >
    TK_LTE,        // <=
    TK_GTE,        // >=
    TK_SHL,        // <<
    TK_SHR,        // >>
    TK_QUESTION,   // ?
    TK_COLON,      // :
    TK_ACCESS,     // =>
    TK_DOT,        // .
} TokenKind;

typedef struct
{
    TokenKind kind;
    const char *lexeme; // pointer into buffer
    int length;
    int64_t int_val;
    int line;
    int col;
} Token;

typedef enum
{
    TY_I8,
    TY_U8,
    TY_I16,
    TY_U16,
    TY_I32,
    TY_U32,
    TY_I64,
    TY_U64,
    TY_F32,
    TY_F64,
    TY_F128,
    TY_VOID,
    TY_CHAR,
    TY_PTR,
    TY_STRUCT,
    TY_IMPORT,
} TypeKind;

typedef struct Type
{
    TypeKind kind;
    struct Type *pointee; // for TY_PTR
    // For TY_STRUCT
    const char *struct_name;
    struct {
        const char **field_names;
        struct Type **field_types;
        int *field_offsets;
        int field_count;
        int size_bytes;
    } strct;
    int is_exposed; // visibility flag for module system
    const char *import_module;
    const char *import_type_name;
    struct Type *import_resolved;
} Type;

typedef struct ModulePath
{
    const char **parts;
    int part_count;
    const char *full_name;
    const char *alias;
} ModulePath;

typedef enum
{
    ND_INT,
    ND_ADD,
    ND_MUL,
    ND_DIV,
    ND_RET,
    ND_FUNC,
    ND_STRING,
    ND_CALL,
    ND_BLOCK,
    ND_VAR_DECL,
    ND_ASSIGN,
    ND_IF,
    ND_INDEX,
    ND_CAST,
    ND_GT_EXPR,
    ND_LT,
    ND_LE,
    ND_GE,
    ND_SUB,
    ND_WHILE,
    ND_EXPR_STMT,
    ND_VAR,
    ND_UNIT,
    ND_PREINC,
    ND_PREDEC,
    ND_POSTINC,
    ND_POSTDEC,
    ND_ADDR,
    ND_LAND,
    ND_LOR,
    ND_SIZEOF,
    ND_TYPEOF,
    ND_EQ,
    ND_NE,
    ND_COND, // ternary conditional expr: lhs ? rhs : body
    ND_MEMBER, // struct/enum member access
    ND_INIT_LIST, // brace initializer
    ND_SHL,      // <<
    ND_SHR,      // >>
} NodeKind;

const char *node_kind_name(NodeKind kind);

typedef struct
{
    const char *src; // entire source buffer
    int length;
    const char *filename;
} SourceBuffer;

typedef struct Node
{
    NodeKind kind;
    struct Node *lhs;
    struct Node *rhs;
    int64_t int_val; // for ND_INT
    // Source location for diagnostics
    int line;
    int col;
    const SourceBuffer *src;
    // Typed nodes
    Type *type; // inferred/declared type
    // For ND_FUNC
    const char *name;
    struct Node *body; // single statement for now
    Type *ret_type;
    // parameters
    Type **param_types;
    const char **param_names;
    int param_count;
    int is_chancecode;
    struct
    {
        char **lines;
        int count;
    } chancecode;
    // Metadata overrides for backend emission
    struct
    {
        char *func_line;          // overrides .func line when non-null
        char *params_line;        // overrides .params line when non-null
        char *locals_line;        // overrides .locals line when non-null
        char *backend_name;       // alternate symbol name in generated code
        char **param_type_names;  // parsed tokens from .params override
        int param_type_count;     // number of parameter tokens from override
        int declared_param_count; // parsed from .func params= value, or -1 when unspecified
        int declared_local_count; // parsed from .func locals= value, or -1 when unspecified
        char *ret_token;          // parsed from .func ret= value, if provided
    } metadata;
    // For ND_STRING
    const char *str_data;
    int str_len;
    // For ND_CALL
    const char *call_name;
    struct Node **args;
    int arg_count;
    // For ND_VAR_DECL
    const char *var_name;
    Type *var_type;
    int var_is_const; // for ND_VAR_DECL
    // For ND_BLOCK
    struct Node **stmts;
    int stmt_count;
    // For ND_VAR reference
    const char *var_ref;
    int is_noreturn;
    int is_entrypoint;
    // For ND_MEMBER
    const char *field_name;
    int field_index;
    int field_offset;
    int is_pointer_deref; // 1 if base was pointer (for -> semantics)
    // For ND_INIT_LIST
    struct {
        struct Node **elems;
        const char **designators; // NULL for positional
        int *field_indices; // computed during sema, parallel to elems
        int count;
        int is_zero; // for {} or {0} special cases
    } init;
    // Module metadata (only valid for ND_UNIT)
    ModulePath module_path;
    ModulePath *imports;
    int import_count;
    // Declaration visibility flag (used for ND_FUNC and other decl nodes)
    int is_exposed;
} Node;

typedef struct Lexer Lexer;
Lexer *lexer_create(SourceBuffer src);
void lexer_destroy(Lexer *lx);
Token lexer_next(Lexer *lx);
Token lexer_peek(Lexer *lx);
Token lexer_peek_n(Lexer *lx, int n);
// Accessor for diagnostics: returns the source buffer associated with this
// lexer
const SourceBuffer *lexer_source(Lexer *lx);

typedef struct Parser Parser;
Parser *parser_create(SourceBuffer src);
void parser_destroy(Parser *ps);
// Parse a translation unit; currently expects one function 'main' with a return
// int expression
Node *parse_unit(Parser *ps);
// Export parsed extern declarations (extend from "C" ...) into the given symbol
// table Forward-declare SymTable so it can be referenced here before its full
// definition below
typedef struct SymTable SymTable;
void parser_export_externs(Parser *ps, SymTable *st);
const struct Symbol *parser_get_externs(const Parser *ps, int *count);

// Utilities
void ast_free(Node *n);
Type *type_i32(void);
Type *type_i64(void);
Type *type_void(void);
Type *type_char(void);
Type *type_ptr(Type *to);
int type_equals(Type *a, Type *b);

// Codegen options when emitting ChanceCode bytecode (.ccb)
typedef enum
{
    ASM_INTEL = 0,
    ASM_ATT = 1,
    ASM_NASM = 2,
} AsmSyntax;

typedef enum
{
    OS_WINDOWS = 0,
    OS_LINUX = 1
} TargetOS;

struct Symbol;

typedef struct
{
    bool freestanding;
    bool m32;
    bool emit_asm;           // for -S (produces .S from chancecodec)
    bool no_link;            // don't link; emit .ccb only
    AsmSyntax asm_syntax;    // desired assembly flavor when translating via chancecodec
    const char *output_path; // Final executable (when linking) or output module path
    const char *obj_output_path; // Optional: explicit intermediate object path
    const char *ccb_output_path; // Optional: explicit .ccb output path override
    TargetOS os;
    const struct Symbol *externs;
    int extern_count;
    int opt_level;
} CodegenOptions;

int codegen_ccb_write_module(const Node *unit, const CodegenOptions *opts);
int codegen_ccb_resolve_module_path(const CodegenOptions *opts, char *buffer, size_t bufsz);

// tiny utility functions
void *xmalloc(size_t sz);
void *xcalloc(size_t n, size_t sz);
char *xstrdup(const char *s);

// Diagnostics (GCC-like): file:line:col: {error|warning|note}: message
// If SourceBuffer is provided, we also print the source line and a caret.
void diag_error_at(const SourceBuffer *src, int line, int col, const char *fmt,
                   ...);
void diag_warning_at(const SourceBuffer *src, int line, int col,
                     const char *fmt, ...);
void diag_note_at(const SourceBuffer *src, int line, int col, const char *fmt,
                  ...);
void diag_error(const char *fmt, ...);
void diag_warning(const char *fmt, ...);
void diag_note(const char *fmt, ...);
int diag_error_count(void);
int diag_warning_count(void);
void diag_reset(void);

// Semantic analysis (type checking) and symbols
typedef enum
{
    SYM_FUNC
} SymKind;
typedef struct FuncSig
{
    Type *ret;
    Type **params;
    int param_count;
    int is_varargs;
} FuncSig;
typedef struct Symbol
{
    SymKind kind;
    const char *name;
    const char *backend_name;
    int is_extern;   // 1 if extern (extend from "C")
    const char *abi; // e.g., "C"
    FuncSig sig;
    int is_noreturn;
} Symbol;

typedef struct SymTable SymTable;
SymTable *symtab_create(void);
void symtab_destroy(SymTable *st);
int symtab_add(SymTable *st, Symbol sym);
const Symbol *symtab_get(SymTable *st, const char *name);

struct Scope; // forward decl for semantic analyzer scope chain
typedef struct
{
    SymTable *syms;
    struct Scope *scope;
    const struct Node *unit;
} SemaContext;

SemaContext *sema_create(void);
void sema_destroy(SemaContext *sc);
// returns 0 on success, non-zero on error (prints diagnostics)
int sema_check_unit(SemaContext *sc, Node *unit);

#endif // CHANCE_AST_H
