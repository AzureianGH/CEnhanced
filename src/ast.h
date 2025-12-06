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
    TK_KW_ACTION,
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
    TK_KW_BOOL,
    TK_KW_VAR,
    TK_KW_IF,
    TK_KW_ELSE,
    TK_KW_SWITCH,
    TK_KW_CASE,
    TK_KW_DEFAULT,
    TK_KW_WHILE,
    TK_KW_MATCH,
    TK_KW_BREAK,
    TK_KW_CONTINUE,
    TK_KW_ENUM,
    TK_KW_ALIAS,
    TK_KW_AS,
    TK_KW_SIZEOF,
    TK_KW_TYPEOF,
    TK_KW_ALIGNOF,
    TK_KW_OFFSETOF,
    TK_KW_NULL,
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
    TK_PLUSEQ,     // +=
    TK_ANDAND,     // &&
    TK_OROR,       // ||
    TK_AMP,        // &
    TK_ANDEQ,      // &=
    TK_PIPE,       // |
    TK_OREQ,       // |=
    TK_CARET,      // ^
    TK_XOREQ,      // ^=
    TK_TILDE,      // ~
    TK_STAR,       // *
    TK_STAREQ,     // *=
    TK_SLASH,      // /
    TK_SLASHEQ,    // /=
    TK_PERCENT,    // %
    TK_PERCENTEQ,  // %=
    TK_MINUS,      // -
    TK_MINUSMINUS, // --
    TK_MINUSEQ,    // -=
    TK_ASSIGN,     // =
    TK_EQEQ,       // ==
    TK_BANG,       // !
    TK_BANGEQ,     // !=
    TK_LT,         // <
    TK_GT,         // >
    TK_LTE,        // <=
    TK_GTE,        // >=
    TK_SHL,        // <<
    TK_SHR,        // >>
    TK_SHLEQ,      // <<=
    TK_SHREQ,      // >>=
    TK_QUESTION,   // ?
    TK_COLON,      // :
    TK_ACCESS,     // =>
    TK_DOT,        // .
    TK_ELLIPSIS,   // ...
    TK_FLOAT,      // floating-point literal
} TokenKind;

typedef struct
{
    TokenKind kind;
    const char *lexeme; // pointer into buffer
    int length;
    int64_t int_val;
    double float_val;
    int float_is_f32;
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
    TY_BOOL,
    TY_FUNC,
    TY_PTR,
    TY_STRUCT,
    TY_ARRAY,
    TY_VA_LIST,
    TY_TEMPLATE_PARAM,
    TY_IMPORT,
} TypeKind;

typedef enum
{
    TEMPLATE_CONSTRAINT_NONE = 0,
    TEMPLATE_CONSTRAINT_INTEGRAL,
    TEMPLATE_CONSTRAINT_FLOATING,
    TEMPLATE_CONSTRAINT_NUMERIC,
    TEMPLATE_CONSTRAINT_POINTER,
} TemplateConstraintKind;

typedef struct Type
{
    TypeKind kind;
    struct Type *pointee; // for TY_PTR
    // For TY_STRUCT
    const char *struct_name;
    struct
    {
        const char **field_names;
        struct Type **field_types;
        int *field_offsets;
        int field_count;
        int size_bytes;
    } strct;
    struct
    {
        struct Type *elem;
        int length;
        int is_unsized;
    } array;
    struct
    {
        struct Type **params;
        int param_count;
        struct Type *ret;
        int is_varargs;
        int has_signature;
    } func;
    int is_exposed; // visibility flag for module system
    const char *import_module;
    const char *import_type_name;
    struct Type *import_resolved;
    const char *template_param_name;
    int template_param_index;
    TemplateConstraintKind template_constraint_kind;
    struct Type *template_default_type;
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
    ND_FLOAT,
    ND_ADD,
    ND_MUL,
    ND_DIV,
    ND_MOD,
    ND_RET,
    ND_FUNC,
    ND_STRING,
    ND_NULL,
    ND_CALL,
    ND_VA_START,
    ND_VA_ARG,
    ND_VA_END,
    ND_BLOCK,
    ND_VAR_DECL,
    ND_ASSIGN,
    ND_ADD_ASSIGN,
    ND_SUB_ASSIGN,
    ND_MUL_ASSIGN,
    ND_DIV_ASSIGN,
    ND_MOD_ASSIGN,
    ND_BITAND_ASSIGN,
    ND_BITOR_ASSIGN,
    ND_BITXOR_ASSIGN,
    ND_SHL_ASSIGN,
    ND_SHR_ASSIGN,
    ND_IF,
    ND_INDEX,
    ND_DEREF,
    ND_CAST,
    ND_GT_EXPR,
    ND_LT,
    ND_LE,
    ND_GE,
    ND_SUB,
    ND_NEG,
    ND_WHILE,
    ND_BREAK,
    ND_CONTINUE,
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
    ND_LNOT,
    ND_SIZEOF,
    ND_TYPEOF,
    ND_ALIGNOF,
    ND_OFFSETOF,
    ND_EQ,
    ND_NE,
    ND_COND,      // ternary conditional expr: lhs ? rhs : body
    ND_MEMBER,    // struct/enum member access
    ND_INIT_LIST, // brace initializer
    ND_SHL,       // <<
    ND_SHR,       // >>
    ND_BITAND,    // &
    ND_BITOR,     // |
    ND_BITXOR,    // ^
    ND_BITNOT,    // ~
    ND_SWITCH,
    ND_MATCH,
    ND_LAMBDA,
    ND_SEQ,
    ND_LAMBDA_CALL,
} NodeKind;

const char *node_kind_name(NodeKind kind);

typedef struct
{
    const char *src; // entire source buffer
    int length;
    const char *filename;
} SourceBuffer;

typedef struct Node Node;

typedef struct
{
    Node *value;    // NULL when representing 'default'
    Node *body;     // statement/block to execute for this case
    int is_default; // 1 when this entry is the default case
} SwitchCase;

typedef struct
{
    Node *pattern;            // expression pattern to match
    Node *guard;              // optional guard expression (NULL when unused)
    Node *body;               // statement/block executed when arm matches
    const char *binding_name; // optional binding identifier when pattern binds
} MatchArm;

struct Node
{
    NodeKind kind;
    struct Node *lhs;
    struct Node *rhs;
    int64_t int_val;  // for ND_INT
    double float_val; // for ND_FLOAT
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
    unsigned char *param_const_flags;
    int param_count;
    const char **generic_param_names;
    Type **generic_param_types;
    int generic_param_count;
    int is_varargs;
    int is_chancecode;
    int is_literal;
    int force_inline_literal;
    struct
    {
        char **lines;
        int count;
    } chancecode;
    struct
    {
        char **lines;
        int count;
    } literal;
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
    char *section_name; // optional custom section for functions/globals
    int wants_inline;
    int inline_candidate;
    int inline_cost;
    int inline_address_taken;
    int inline_recursive;
    const struct Node *inline_expr;
    int inline_needs_body;
    // For ND_STRING
    const char *str_data;
    int str_len;
    // For ND_CALL
    const char *call_name;
    struct Node **args;
    int arg_count;
    Type **call_type_args; // explicit template type arguments
    int call_type_arg_count;
    Type *call_func_type;           // canonicalized function signature when available
    int call_is_indirect;           // 1 for pointer-based calls
    int call_is_varargs;            // 1 when call accepts varargs (used for indirect calls)
    const struct Node *call_target; // resolved direct call target when available
    // For ND_VAR_DECL
    const char *var_name;
    Type *var_type;
    int var_is_const;                 // for ND_VAR_DECL
    int var_is_global;                // set on declarations/references that live at global scope
    int var_is_array;                 // for ND_VAR references to array-typed variables
    int var_is_function;              // for ND_VAR references that name a function symbol
    struct Node *referenced_function; // points at referenced function definition when resolved
    const ModulePath *module_ref;     // tracks originating module for qualified references
    int module_ref_parts;             // number of module path segments consumed in expression
    const char *module_type_name;     // resolved type name within module, when applicable
    int module_type_is_enum;          // 1 when module_type_name refers to an enum for chained lookups
    int var_is_inferred;              // set on declarations that use 'var'
    // For ND_BLOCK
    struct Node **stmts;
    int stmt_count;
    // For ND_SWITCH
    struct
    {
        Node *expr;
        SwitchCase *cases;
        int case_count;
    } switch_stmt;
    // For ND_MATCH
    struct
    {
        Node *expr;
        MatchArm *arms;
        int arm_count;
    } match_stmt;
    // For ND_VAR reference
    const char *var_ref;
    int is_noreturn;
    int is_entrypoint;
    int is_preserve;
    // For ND_MEMBER
    const char *field_name;
    int field_index;
    int field_offset;
    int is_pointer_deref; // 1 if base was pointer (for -> semantics)
    // For ND_INIT_LIST
    struct
    {
        struct Node **elems;
        const char **designators; // NULL for positional
        int *field_indices;       // computed during sema, parallel to elems
        int count;
        int is_zero;          // for {} or {0} special cases
        int is_array_literal; // track '[...]' initializers
    } init;
    // Module metadata (only valid for ND_UNIT)
    ModulePath module_path;
    ModulePath *imports;
    int import_count;
    // Declaration visibility flag (used for ND_FUNC and other decl nodes)
    int is_exposed;
    int export_name; // New export_name field to track the [Export] attribute
};

typedef struct Lexer Lexer;
Lexer *lexer_create(SourceBuffer src);
void lexer_destroy(Lexer *lx);
Token lexer_next(Lexer *lx);
Token lexer_peek(Lexer *lx);
Token lexer_peek_n(Lexer *lx, int n);
int lexer_collect_literal_block(Lexer *lx, char **out_text);
// Accessor for diagnostics: returns the source buffer associated with this
// lexer
const SourceBuffer *lexer_source(Lexer *lx);

typedef struct Parser Parser;
Parser *parser_create(SourceBuffer src);
void parser_set_disable_formatting_notes(int disable);
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
Type *type_f32(void);
Type *type_f64(void);
Type *type_void(void);
Type *type_char(void);
Type *type_bool(void);
Type *type_va_list(void);
Type *type_template_param(const char *name, int index);
Type *type_ptr(Type *to);
Type *type_array(Type *elem, int length);
Type *type_func(void);
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
    OS_LINUX = 1,
    OS_MACOS = 2
} TargetOS;

struct Symbol;

typedef struct
{
    bool freestanding;
    bool m32;
    bool debug_symbols;
    bool emit_asm;               // for -S (produces .S from chancecodec)
    bool no_link;                // don't link; emit .ccb only
    AsmSyntax asm_syntax;        // desired assembly flavor when translating via chancecodec
    const char *output_path;     // Final executable (when linking) or output module path
    const char *obj_output_path; // Optional: explicit intermediate object path
    const char *ccb_output_path; // Optional: explicit .ccb output path override
    TargetOS os;
    const struct Symbol *externs;
    int extern_count;
    const struct Symbol *imported_externs;
    int imported_extern_count;
    const struct Symbol *imported_globals;
    int imported_global_count;
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
void diag_set_use_ansi(int enable);

// Compiler-wide verbose diagnostics
void compiler_verbose_set_mode(int enable);
void compiler_verbose_set_deep(int enable);
void compiler_verbose_set_use_ansi(int enable);
int compiler_verbose_enabled(void);
int compiler_verbose_deep_enabled(void);
void compiler_verbose_logf(const char *phase, const char *fmt, ...);
void compiler_verbose_treef(const char *phase, const char *branch, const char *fmt, ...);

// Semantic analysis (type checking) and symbols
typedef enum
{
    SYM_FUNC,
    SYM_GLOBAL
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
    struct Node *ast_node; // owning AST node when symbol originates from parsed code
    int is_noreturn;
    Type *var_type; // valid when kind == SYM_GLOBAL
    int is_const;   // valid when kind == SYM_GLOBAL
} Symbol;

typedef struct SymTable SymTable;
SymTable *symtab_create(void);
void symtab_destroy(SymTable *st);
int symtab_add(SymTable *st, Symbol sym);
const Symbol *symtab_get(SymTable *st, const char *name);

struct Scope; // forward decl for semantic analyzer scope chain
struct ImportedFunctionSet;

typedef struct
{
    SymTable *syms;
    struct Scope *scope;
    struct Node *unit;
    struct ImportedFunctionSet *imported_funcs;
    int imported_func_count;
    int imported_func_cap;
    Symbol *imported_globals;
    int imported_global_count;
    int imported_global_cap;
    int loop_depth;
    int switch_depth;
    int lambda_counter;
} SemaContext;

SemaContext *sema_create(void);
void sema_destroy(SemaContext *sc);
// returns 0 on success, non-zero on error (prints diagnostics)
int sema_check_unit(SemaContext *sc, Node *unit);
void sema_register_foreign_unit_symbols(SemaContext *sc, Node *target_unit, Node *foreign_unit);
void sema_track_imported_function(SemaContext *sc, const char *name, const char *module_full, const Symbol *symbol);
Symbol *sema_copy_imported_function_symbols(const SemaContext *sc, int *out_count);
Symbol *sema_copy_imported_global_symbols(const SemaContext *sc, int *out_count);

#endif // CHANCE_AST_H
