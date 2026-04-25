#ifndef CHANCE_AST_H
#define CHANCE_AST_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

typedef enum
{
    TK_EOF = 0,
    TK_IDENT,
    TK_INT,
    TK_STRING,
    TK_CHAR_LIT,
    
    TK_KW_FUN,
    TK_KW_ACTION,
    TK_KW_RET,
    TK_KW_STACK,
    TK_KW_STATIC,
    TK_KW_REG,
    TK_KW_STRUCT,
    TK_KW_BUNDLE,
    TK_KW_STRUC, 
    TK_KW_UNION,
    TK_KW_PACKED,
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
    TK_KW_STRING,
    TK_KW_BOOL,
    TK_KW_REF,
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
    TK_KW_TRY,
    TK_KW_CATCH,
    TK_KW_FINALLY,
    TK_KW_WHERE,
    TK_KW_THROW,
    TK_KW_JUMP,
    TK_KW_CHANCECODE,
    TK_KW_LITERAL,
    TK_KW_ENTRYPOINT,
    TK_KW_JUMPTARGET,
    TK_KW_EXPORT,
    TK_KW_RAW,
    TK_KW_PRESERVE,
    TK_KW_SECTION,
    TK_KW_FORCEINLINE,
    TK_KW_INLINE,
    TK_KW_HINT,
    TK_KW_NOHINT,
    TK_KW_OVERRIDEMETADATA,
    TK_KW_ENUM,
    TK_KW_ALIAS,
    TK_KW_AS,
    TK_KW_IS,
    TK_KW_SIZEOF,
    TK_KW_TYPEOF,
    TK_KW_ALIGNOF,
    TK_KW_OFFSETOF,
    TK_KW_MANAGED,
    TK_KW_UNMANAGED,
    TK_KW_OBJECT,
    TK_KW_NULL,
    TK_KW_NORETURN,
    TK_KW_MODULE,
    TK_KW_BRING,
    TK_KW_HIDE,
    TK_KW_EXPOSE,
    TK_KW_NEW,
    TK_KW_DELETE,
    
    TK_ARROW,      
    TK_LPAREN,     
    TK_RPAREN,     
    TK_LBRACE,     
    TK_RBRACE,     
    TK_LBRACKET,   
    TK_RBRACKET,   
    TK_SEMI,       
    TK_COMMA,      
    TK_PLUS,       
    TK_PLUSPLUS,   
    TK_PLUSEQ,     
    TK_ANDAND,     
    TK_OROR,       
    TK_AMP,        
    TK_ANDEQ,      
    TK_PIPE,       
    TK_OREQ,       
    TK_CARET,      
    TK_XOREQ,      
    TK_TILDE,      
    TK_STAR,       
    TK_STAREQ,     
    TK_SLASH,      
    TK_SLASHEQ,    
    TK_PERCENT,    
    TK_PERCENTEQ,  
    TK_MINUS,      
    TK_MINUSMINUS, 
    TK_MINUSEQ,    
    TK_ASSIGN,     
    TK_EQEQEQ,     
    TK_EQEQ,       
    TK_BANG,       
    TK_BANGEQ,     
    TK_LT,         
    TK_GT,         
    TK_LTE,        
    TK_GTE,        
    TK_SHL,        
    TK_SHR,        
    TK_SHLEQ,      
    TK_SHREQ,      
    TK_QUESTION,   
    TK_COLON,      
    TK_ACCESS,     
    TK_DOT,        
    TK_ELLIPSIS,   
    TK_FLOAT,      
} TokenKind;

typedef struct
{
    TokenKind kind;
    const char *lexeme; 
    int length;
    int64_t int_val;
    uint64_t int_uval;
    int int_is_unsigned;
    int int_width;
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
    TY_REF,
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
    struct Type *pointee; 
    
    
    int ref_nullability;
    
    const char *struct_name;
    int is_union;
    struct
    {
        const char **field_names;
        struct Type **field_types;
        const char **field_default_values;
        unsigned char *field_exposed_flags;
        int *field_offsets;
        int field_count;
        int size_bytes;
        int is_packed;
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
    int is_exposed; 
    int is_object;  
    int is_bundle;
    int bundle_ctor_declared;
    int bundle_ctor_declared_exposed;
    int bundle_ctor_has_zero_arity;
    int bundle_ctor_has_exposed_zero_arity;
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
    ND_STRICT_EQ,
    ND_NE,
    ND_IS,
    ND_COND,      
    ND_MEMBER,    
    ND_INIT_LIST, 
    ND_NEW,
    ND_DELETE,
    ND_SHL,       
    ND_SHR,       
    ND_BITAND,    
    ND_BITOR,     
    ND_BITXOR,    
    ND_BITNOT,    
    ND_SWITCH,
    ND_TRY,
    ND_THROW,
    ND_MATCH,
    ND_LAMBDA,
    ND_SEQ,
    ND_MANAGED_ARRAY_ADAPT,
    ND_LAMBDA_CALL,
} NodeKind;

typedef struct Node Node;

const char *node_kind_name(NodeKind kind);
void ast_emit_json(FILE *out, const Node *unit, const char *input_path);

typedef struct
{
    const char *src; 
    int length;
    const char *filename;
} SourceBuffer;


void diag_set_use_ansi(int enable);
void diag_set_data_log(int enable);
void diag_error_at(const SourceBuffer *src, int line, int col, const char *fmt, ...);
void diag_warning_at(const SourceBuffer *src, int line, int col, const char *fmt, ...);
void diag_note_at(const SourceBuffer *src, int line, int col, const char *fmt, ...);
void diag_error(const char *fmt, ...);
void diag_warning(const char *fmt, ...);
void diag_note(const char *fmt, ...);
int diag_error_count(void);
int diag_warning_count(void);
void diag_reset(void);

typedef struct
{
    Node *value;    
    Node *body;     
    int is_default; 
} SwitchCase;

typedef struct
{
    Node *pattern;            
    Node *guard;              
    Node *body;               
    const char *binding_name; 
} MatchArm;

struct Node
{
    NodeKind kind;
    struct Node *lhs;
    struct Node *rhs;
    int64_t int_val;  
    uint64_t int_uval; 
    int int_is_unsigned;
    int int_width;
    double float_val; 
    
    int line;
    int col;
    const SourceBuffer *src;
    
    Type *type; 
    Type *is_type; 
    
    struct Node *type_expr;
    
    const char *name;
    struct Node *body; 
    Type *ret_type;
    
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
    int is_managed;
    int is_extension_method;
    int is_bundle_global_init;
    int force_inline_literal;
    int is_arrow_shorthand;
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
    
    struct
    {
        char *func_line;          
        char *params_line;        
        char *locals_line;        
        char *backend_name;       
        char **param_type_names;  
        int param_type_count;     
        int declared_param_count; 
        int declared_local_count; 
        char *ret_token;          
    } metadata;
    char *section_name; 
    int wants_inline;
    int inline_candidate;
    int inline_cost;
    int inline_address_taken;
    int inline_recursive;
    const struct Node *inline_expr;
    int inline_needs_body;
    
    const char *str_data;
    int str_len;
    
    const char *call_name;
    struct Node **args;
    int arg_count;
    Type **call_type_args; 
    int call_type_arg_count;
    Type *call_func_type;           
    int call_is_indirect;           
    int call_is_varargs;            
    int call_is_jump;               
    const struct Node *call_target; 
    
    const char *var_name;
    Type *var_type;
    int var_is_const;                 
    int var_is_static;                
    int var_is_global;                
    int var_is_array;                 
    int var_is_function;              
    struct Node *referenced_function; 
    const char *managed_length_name;  
    struct Node *managed_length_expr; 
    const ModulePath *module_ref;     
    int module_ref_parts;             
    const char *module_type_name;     
    int module_type_is_enum;          
    int var_is_inferred;              
    
    struct Node **stmts;
    int stmt_count;
    
    struct
    {
        Node *expr;
        SwitchCase *cases;
        int case_count;
    } switch_stmt;
    
    struct
    {
        Node *expr;
        MatchArm *arms;
        int arm_count;
    } match_stmt;
    
    const char *var_ref;
    int is_noreturn;
    int is_entrypoint;
    int is_jump_target;
    int is_preserve;
    
    const char *field_name;
    int field_index;
    int field_offset;
    int is_pointer_deref; 
    
    struct
    {
        struct Node **elems;
        const char **designators; 
        int *field_indices;       
        int count;
        int is_zero;          
        int is_array_literal; 
    } init;
    
    ModulePath module_path;
    int module_is_managed;
    ModulePath *imports;
    int import_count;
    
    int is_exposed;
    int export_name; 
    int raw_export_name;
};

typedef struct Lexer Lexer;
Lexer *lexer_create(SourceBuffer src);
void lexer_destroy(Lexer *lx);
Token lexer_next(Lexer *lx);
Token lexer_peek(Lexer *lx);
Token lexer_peek_n(Lexer *lx, int n);
int lexer_collect_literal_block(Lexer *lx, char **out_text);


const SourceBuffer *lexer_source(Lexer *lx);

typedef struct Parser Parser;
typedef enum
{
    CHANCE_STD_H26 = 26,
    CHANCE_STD_H27 = 27,
    CHANCE_STD_H28 = 28,
} ChanceLanguageStandard;

Parser *parser_create(SourceBuffer src);
void parser_set_disable_formatting_notes(int disable);
void parser_set_language_standard(ChanceLanguageStandard standard);
ChanceLanguageStandard parser_get_language_standard(void);
void parser_destroy(Parser *ps);


Node *parse_unit(Parser *ps);



typedef struct SymTable SymTable;
void parser_export_externs(Parser *ps, SymTable *st);
const struct Symbol *parser_get_externs(const Parser *ps, int *count);


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
Type *type_ref(Type *to, int nullability);
int type_equals(Type *a, Type *b);


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
    bool emit_asm;               
    bool no_link;                
    AsmSyntax asm_syntax;        
    const char *output_path;     
    const char *obj_output_path; 
    const char *ccb_output_path; 
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


void *xmalloc(size_t sz);
void *xcalloc(size_t n, size_t sz);
char *xstrdup(const char *s);



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


void compiler_verbose_set_mode(int enable);
void compiler_verbose_set_deep(int enable);
void compiler_verbose_set_use_ansi(int enable);
int compiler_verbose_enabled(void);
int compiler_verbose_deep_enabled(void);
void compiler_verbose_logf(const char *phase, const char *fmt, ...);
void compiler_verbose_treef(const char *phase, const char *branch, const char *fmt, ...);


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
    unsigned char *param_const_flags;
    int is_varargs;
} FuncSig;
typedef struct Symbol
{
    SymKind kind;
    const char *name;
    const char *backend_name;
    int is_extern;   
    const char *abi; 
    FuncSig sig;
    struct Node *ast_node; 
    int is_noreturn;
    int is_extension_method;
    Type *var_type; 
    int is_const;   
} Symbol;

typedef struct SymTable SymTable;
SymTable *symtab_create(void);
void symtab_destroy(SymTable *st);
int symtab_add(SymTable *st, Symbol sym);
const Symbol *symtab_get(SymTable *st, const char *name);

struct Scope; 
struct ImportedFunctionSet;

typedef struct
{
    SymTable *syms;
    struct Scope *scope;
    struct Node *unit;
    struct Node *current_function;
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

int sema_check_unit(SemaContext *sc, Node *unit);
void sema_register_foreign_unit_symbols(SemaContext *sc, Node *target_unit, Node *foreign_unit);
void sema_track_imported_function(SemaContext *sc, const char *name, const char *module_full, const Symbol *symbol);
Symbol *sema_copy_imported_function_symbols(const SemaContext *sc, int *out_count);
Symbol *sema_copy_imported_global_symbols(const SemaContext *sc, int *out_count);
void sema_set_allow_implicit_voidp(int enable);
void sema_set_allow_implicit_sizeof(int enable);
void sema_set_allow_implicit_void_function(int enable);
int sema_get_allow_implicit_void_function(void);

#endif 
