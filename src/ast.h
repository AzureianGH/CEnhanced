#ifndef CHANCE_AST_H
#define CHANCE_AST_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

typedef enum
{
    TK_EOF = 0,
    TK_IDENT,
    TK_INT,
    TK_STRING,
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
    TK_KW_VOID,
    TK_KW_CHAR,
    TK_KW_IF,
    TK_KW_ELSE,
    TK_KW_WHILE,
    TK_KW_ALIAS,
    TK_KW_AS,
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
    TK_STAR,       // *
    TK_MINUS,      // -
    TK_MINUSMINUS, // --
    TK_ASSIGN,     // =
    TK_LT,         // <
    TK_GT,         // >
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
    TY_I32,
    TY_I64,
    TY_VOID,
    TY_CHAR,
    TY_PTR,
} TypeKind;

typedef struct Type
{
    TypeKind kind;
    struct Type *pointee; // for TY_PTR
} Type;

typedef enum
{
    ND_INT,
    ND_ADD,
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
    ND_SUB,
    ND_WHILE,
    ND_EXPR_STMT,
    ND_VAR,
    ND_UNIT,
    ND_PREINC,
    ND_PREDEC,
    ND_POSTINC,
    ND_POSTDEC,
} NodeKind;

typedef struct Node
{
    NodeKind kind;
    struct Node *lhs;
    struct Node *rhs;
    int64_t int_val; // for ND_INT
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
    // For ND_BLOCK
    struct Node **stmts;
    int stmt_count;
    // For ND_VAR reference
    const char *var_ref;
} Node;

typedef struct
{
    const char *src; // entire source buffer
    int length;
    const char *filename;
} SourceBuffer;

typedef struct Lexer Lexer;
Lexer *lexer_create(SourceBuffer src);
void lexer_destroy(Lexer *lx);
Token lexer_next(Lexer *lx);
Token lexer_peek(Lexer *lx);
// Accessor for diagnostics: returns the source buffer associated with this lexer
const SourceBuffer *lexer_source(Lexer *lx);

typedef struct Parser Parser;
Parser *parser_create(SourceBuffer src);
void parser_destroy(Parser *ps);
// Parse a translation unit; currently expects one function 'main' with a return int expression
Node *parse_unit(Parser *ps);
// Export parsed extern declarations (extend from "C" ...) into the given symbol table
// Forward-declare SymTable so it can be referenced here before its full definition below
typedef struct SymTable SymTable;
void parser_export_externs(Parser *ps, SymTable *st);

// Utilities
void ast_free(Node *n);

// Codegen to x64 COFF/PE: returns 0 on success
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

typedef struct
{
    bool freestanding;
    bool m32;
    bool emit_asm;           // for -S
    AsmSyntax asm_syntax;    // intel (GAS noprefix), att (GAS AT&T), nasm
    const char *output_path; // .exe path
    // Target OS/ABI
    TargetOS os;
} CodegenOptions;

int codegen_coff_x64_write_exe(const Node *unit, const CodegenOptions *opts);
int codegen_pe_x64_write_exe_const(int32_t retval, const CodegenOptions *opts);

// tiny utility functions
void *xmalloc(size_t sz);
void *xcalloc(size_t n, size_t sz);
char *xstrdup(const char *s);

// Diagnostics (GCC-like): file:line:col: {error|warning|note}: message
// If SourceBuffer is provided, we also print the source line and a caret.
void diag_error_at(const SourceBuffer *src, int line, int col, const char *fmt, ...);
void diag_warning_at(const SourceBuffer *src, int line, int col, const char *fmt, ...);
void diag_note_at(const SourceBuffer *src, int line, int col, const char *fmt, ...);
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
    int is_extern;   // 1 if extern (extend from "C")
    const char *abi; // e.g., "C"
    FuncSig sig;
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
} SemaContext;

SemaContext *sema_create(void);
void sema_destroy(SemaContext *sc);
// returns 0 on success, non-zero on error (prints diagnostics)
int sema_check_unit(SemaContext *sc, Node *unit);

#endif // CHANCE_AST_H
