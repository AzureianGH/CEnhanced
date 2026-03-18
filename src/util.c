#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include "ast.h"

#define ANSI_RESET "\x1b[0m"
#define ANSI_BOLD_RED "\x1b[1;31m"
#define ANSI_BOLD_YELLOW "\x1b[1;33m"
#define ANSI_BOLD_CYAN "\x1b[1;36m"
#define ANSI_BOLD_WHITE "\x1b[1;37m"
#define ANSI_BOLD_GREEN "\x1b[1;32m"
#define ANSI_BOLD_BLUE "\x1b[1;34m"
#define ANSI_BOLD_MAGENTA "\x1b[1;35m"

static int diag_use_ansi = 1;
static int diag_data_log = 0;

void diag_set_use_ansi(int enable)
{
    diag_use_ansi = enable ? 1 : 0;
}

void diag_set_data_log(int enable)
{
    diag_data_log = enable ? 1 : 0;
}

typedef struct
{
    const char *phase;
    const char *symbol;
    const char *nickname;
    const char *color;
} VerbosePhaseInfo;

static int compiler_verbose_mode = 0;
static int compiler_verbose_deep = 0;
static int compiler_verbose_use_ansi = 1;

static const VerbosePhaseInfo kVerbosePhaseTable[] = {
    {"codegen", "@", "Spratcher", ANSI_BOLD_MAGENTA},
    {"sema", "#", "Enforcer", ANSI_BOLD_BLUE},
    {"inline", "*", "TreePack", ANSI_BOLD_GREEN},
    {"optimizer", "*", "TreePack", ANSI_BOLD_GREEN},
    {NULL, ">", "Compiler", ANSI_BOLD_WHITE},
};

static const VerbosePhaseInfo *compiler_verbose_lookup(const char *phase)
{
    if (phase)
    {
        for (size_t i = 0; i < sizeof(kVerbosePhaseTable) / sizeof(kVerbosePhaseTable[0]) - 1; ++i)
        {
            if (kVerbosePhaseTable[i].phase && strcmp(kVerbosePhaseTable[i].phase, phase) == 0)
                return &kVerbosePhaseTable[i];
        }
    }
    return &kVerbosePhaseTable[sizeof(kVerbosePhaseTable) / sizeof(kVerbosePhaseTable[0]) - 1];
}

static void compiler_verbose_vprint(const VerbosePhaseInfo *info, const char *suffix, const char *fmt, va_list ap)
{
    if (!fmt)
        return;
    if (!info)
        info = compiler_verbose_lookup(NULL);
    const char *color = (compiler_verbose_use_ansi && info->color) ? info->color : "";
    const char *reset = (compiler_verbose_use_ansi && info->color && *info->color) ? ANSI_RESET : "";
    const char *symbol = info->symbol ? info->symbol : ">";
    const char *nick = info->nickname ? info->nickname : (info->phase ? info->phase : "Compiler");
    const char *suffix_part = suffix ? suffix : " ";

    fprintf(stderr, "%s%s %s%s%s", color, symbol, nick, reset, suffix_part);
    vfprintf(stderr, fmt, ap);
    fputc('\n', stderr);
}

void compiler_verbose_set_mode(int enable)
{
    compiler_verbose_mode = enable ? 1 : 0;
    if (!compiler_verbose_mode)
        compiler_verbose_deep = 0;
}

void compiler_verbose_set_deep(int enable)
{
    compiler_verbose_deep = enable ? 1 : 0;
    if (compiler_verbose_deep)
        compiler_verbose_mode = 1;
}

void compiler_verbose_set_use_ansi(int enable)
{
    compiler_verbose_use_ansi = enable ? 1 : 0;
}

int compiler_verbose_enabled(void)
{
    return compiler_verbose_mode;
}

int compiler_verbose_deep_enabled(void)
{
    return compiler_verbose_mode && compiler_verbose_deep;
}

void compiler_verbose_logf(const char *phase, const char *fmt, ...)
{
    if (!compiler_verbose_mode || !fmt)
        return;
    const VerbosePhaseInfo *info = compiler_verbose_lookup(phase);
    va_list ap;
    va_start(ap, fmt);
    compiler_verbose_vprint(info, " ", fmt, ap);
    va_end(ap);
}

void compiler_verbose_treef(const char *phase, const char *branch, const char *fmt, ...)
{
    if (!compiler_verbose_deep_enabled() || !fmt)
        return;
    char suffix[64];
    const char *node = (branch && *branch) ? branch : "|-";
    snprintf(suffix, sizeof(suffix), " %s ", node);
    const VerbosePhaseInfo *info = compiler_verbose_lookup(phase);
    va_list ap;
    va_start(ap, fmt);
    compiler_verbose_vprint(info, suffix, fmt, ap);
    va_end(ap);
}

static const char *diag_color_for(const char *sev)
{
    if (!diag_use_ansi || !sev)
        return "";
    if (strcmp(sev, "error") == 0)
        return ANSI_BOLD_RED;
    if (strcmp(sev, "warning") == 0)
        return ANSI_BOLD_YELLOW;
    if (strcmp(sev, "note") == 0)
        return ANSI_BOLD_CYAN;
    return ANSI_BOLD_WHITE;
}

static void diag_json_write_string(FILE *out, const char *value)
{
    fputc('"', out);
    if (value)
    {
        const unsigned char *p = (const unsigned char *)value;
        while (*p)
        {
            unsigned char ch = *p++;
            switch (ch)
            {
            case '"':
                fputs("\\\"", out);
                break;
            case '\\':
                fputs("\\\\", out);
                break;
            case '\n':
                fputs("\\n", out);
                break;
            case '\r':
                fputs("\\r", out);
                break;
            case '\t':
                fputs("\\t", out);
                break;
            default:
                if (ch < 0x20)
                    fprintf(out, "\\u%04x", (unsigned int)ch);
                else
                    fputc((int)ch, out);
                break;
            }
        }
    }
    fputc('"', out);
}

static char *diag_format_message(const char *fmt, va_list ap)
{
    if (!fmt)
        return NULL;
    va_list ap_copy;
    va_copy(ap_copy, ap);
    int needed = vsnprintf(NULL, 0, fmt, ap_copy);
    va_end(ap_copy);
    if (needed < 0)
        return NULL;
    size_t len = (size_t)needed;
    char *buf = (char *)malloc(len + 1);
    if (!buf)
        return NULL;
    va_list ap_copy2;
    va_copy(ap_copy2, ap);
    vsnprintf(buf, len + 1, fmt, ap_copy2);
    va_end(ap_copy2);
    return buf;
}

static void diag_emit_data_log(const char *sev, const SourceBuffer *src,
                               int line, int col, const char *message)
{
    int safe_line = line > 0 ? line : 0;
    int safe_col = col > 0 ? col : 0;
    fprintf(stderr, "data-log:{\"severity\":");
    diag_json_write_string(stderr, sev ? sev : "");
    fprintf(stderr, ",\"file\":");
    if (src && src->filename)
        diag_json_write_string(stderr, src->filename);
    else
        fputs("null", stderr);
    fprintf(stderr, ",\"line\":%d,\"col\":%d,\"message\":", safe_line,
            safe_col);
    diag_json_write_string(stderr, message ? message : "");
    fputs("}\n", stderr);
}

void *xmalloc(size_t sz)
{
    void *p = malloc(sz);
    if (!p)
    {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    return p;
}
void *xcalloc(size_t n, size_t sz)
{
    void *p = calloc(n, sz);
    if (!p)
    {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    return p;
}
char *xstrdup(const char *s)
{
    size_t n = strlen(s);
    char *p = (char *)xmalloc(n + 1);
    memcpy(p, s, n + 1);
    return p;
}

static void ast_free_rec(Node *n)
{
    if (!n)
        return;
    ast_free_rec(n->lhs);
    ast_free_rec(n->rhs);
    free(n);
}

void ast_free(Node *n)
{
    ast_free_rec(n);
}


static Type TY_I32_SINGLETON = {.kind = TY_I32};
static Type TY_I64_SINGLETON = {.kind = TY_I64};
static Type TY_F32_SINGLETON = {.kind = TY_F32};
static Type TY_F64_SINGLETON = {.kind = TY_F64};
static Type TY_VOID_SINGLETON = {.kind = TY_VOID};
static Type TY_CHAR_SINGLETON = {.kind = TY_CHAR};
static Type TY_BOOL_SINGLETON = {.kind = TY_BOOL};
static Type TY_VA_LIST_SINGLETON = {.kind = TY_VA_LIST};

Type *type_i32(void) { return &TY_I32_SINGLETON; }
Type *type_i64(void) { return &TY_I64_SINGLETON; }
Type *type_f32(void) { return &TY_F32_SINGLETON; }
Type *type_f64(void) { return &TY_F64_SINGLETON; }
Type *type_void(void) { return &TY_VOID_SINGLETON; }
Type *type_char(void) { return &TY_CHAR_SINGLETON; }
Type *type_bool(void) { return &TY_BOOL_SINGLETON; }
Type *type_va_list(void) { return &TY_VA_LIST_SINGLETON; }

Type *type_template_param(const char *name, int index)
{
    Type *t = (Type *)xcalloc(1, sizeof(Type));
    t->kind = TY_TEMPLATE_PARAM;
    if (name)
        t->template_param_name = xstrdup(name);
    t->template_param_index = index;
    t->template_constraint_kind = TEMPLATE_CONSTRAINT_NONE;
    t->template_default_type = NULL;
    return t;
}

Type *type_ptr(Type *to)
{
    Type *t = (Type *)xcalloc(1, sizeof(Type));
    t->kind = TY_PTR;
    t->pointee = to;
    return t;
}

Type *type_ref(Type *to, int nullability)
{
    Type *t = (Type *)xcalloc(1, sizeof(Type));
    t->kind = TY_REF;
    t->pointee = to;
    t->ref_nullability = nullability;
    return t;
}

Type *type_func(void)
{
    Type *t = (Type *)xcalloc(1, sizeof(Type));
    t->kind = TY_FUNC;
    t->func.params = NULL;
    t->func.param_count = 0;
    t->func.ret = NULL;
    t->func.is_varargs = 0;
    t->func.has_signature = 0;
    return t;
}

Type *type_array(Type *elem, int length)
{
    Type *t = (Type *)xcalloc(1, sizeof(Type));
    t->kind = TY_ARRAY;
    t->array.elem = elem;
    t->array.length = length;
    t->array.is_unsized = (length < 0);
    return t;
}

int type_equals(Type *a, Type *b)
{
    if (a == b)
        return 1;
    if (!a || !b)
        return 0;
    if (a->kind != b->kind)
        return 0;
    if (a->kind == TY_PTR)
        return type_equals(a->pointee, b->pointee);
    if (a->kind == TY_REF)
        return (b->kind == TY_REF) && (a->ref_nullability == b->ref_nullability) && type_equals(a->pointee, b->pointee);
    if (a->kind == TY_ARRAY)
    {
        if (a->array.length != b->array.length)
            return 0;
        if (a->array.is_unsized != b->array.is_unsized)
            return 0;
        return type_equals(a->array.elem, b->array.elem);
    }
    if (a->kind == TY_FUNC)
    {
        if (!!a->func.ret != !!b->func.ret)
            return 0;
        if (a->func.ret && !type_equals(a->func.ret, b->func.ret))
            return 0;
        if (a->func.param_count != b->func.param_count)
            return 0;
        if (a->func.is_varargs != b->func.is_varargs)
            return 0;
        for (int i = 0; i < a->func.param_count; ++i)
        {
            Type *ap = a->func.params ? a->func.params[i] : NULL;
            Type *bp = b->func.params ? b->func.params[i] : NULL;
            if (!type_equals(ap, bp))
                return 0;
        }
        return 1;
    }
    if (a->kind == TY_TEMPLATE_PARAM)
    {
        if (a->template_param_index != b->template_param_index)
            return 0;
        if (!!a->template_param_name != !!b->template_param_name)
            return 0;
        if (a->template_param_name && strcmp(a->template_param_name, b->template_param_name) != 0)
            return 0;
        return 1;
    }
    return 1;
}

const char *node_kind_name(NodeKind kind)
{
    switch (kind)
    {
    case ND_INT:
        return "integer literal";
    case ND_FLOAT:
        return "floating-point literal";
    case ND_ADD:
        return "addition expression";
    case ND_MUL:
        return "multiplication expression";
    case ND_DIV:
        return "division expression";
    case ND_MOD:
        return "modulo expression";
    case ND_RET:
        return "return statement";
    case ND_FUNC:
        return "function";
    case ND_STRING:
        return "string literal";
    case ND_NULL:
        return "null literal";
    case ND_CALL:
        return "function call";
    case ND_VA_START:
        return "va_start expression";
    case ND_VA_ARG:
        return "va_arg expression";
    case ND_VA_END:
        return "va_end expression";
    case ND_BLOCK:
        return "block";
    case ND_VAR_DECL:
        return "variable declaration";
    case ND_ASSIGN:
        return "assignment";
    case ND_ADD_ASSIGN:
        return "+= expression";
    case ND_SUB_ASSIGN:
        return "-= expression";
    case ND_MUL_ASSIGN:
        return "*= expression";
    case ND_DIV_ASSIGN:
        return "/= expression";
    case ND_MOD_ASSIGN:
        return "%= expression";
    case ND_BITAND_ASSIGN:
        return "&= expression";
    case ND_BITOR_ASSIGN:
        return "|= expression";
    case ND_BITXOR_ASSIGN:
        return "^= expression";
    case ND_SHL_ASSIGN:
        return "<<= expression";
    case ND_SHR_ASSIGN:
        return ">>= expression";
    case ND_IF:
        return "if statement";
    case ND_INDEX:
        return "index expression";
    case ND_DEREF:
        return "dereference expression";
    case ND_CAST:
        return "cast expression";
    case ND_GT_EXPR:
        return "> comparison";
    case ND_LT:
        return "< comparison";
    case ND_LE:
        return "<= comparison";
    case ND_GE:
        return ">= comparison";
    case ND_SUB:
        return "subtraction expression";
    case ND_NEG:
        return "negation expression";
    case ND_WHILE:
        return "while statement";
    case ND_BREAK:
        return "break statement";
    case ND_CONTINUE:
        return "continue statement";
    case ND_EXPR_STMT:
        return "expression statement";
    case ND_VAR:
        return "variable reference";
    case ND_UNIT:
        return "translation unit";
    case ND_PREINC:
        return "pre-increment";
    case ND_PREDEC:
        return "pre-decrement";
    case ND_POSTINC:
        return "post-increment";
    case ND_POSTDEC:
        return "post-decrement";
    case ND_ADDR:
        return "address-of expression";
    case ND_LAND:
        return "logical AND";
    case ND_LOR:
        return "logical OR";
    case ND_LNOT:
        return "logical NOT";
    case ND_SIZEOF:
        return "sizeof expression";
    case ND_TYPEOF:
        return "typeof expression";
    case ND_EQ:
        return "equality comparison";
    case ND_STRICT_EQ:
        return "strict equality comparison";
    case ND_NE:
        return "inequality comparison";
    case ND_IS:
        return "runtime type comparison";
    case ND_COND:
        return "conditional expression";
    case ND_MEMBER:
        return "member access";
    case ND_INIT_LIST:
        return "initializer list";
    case ND_SHL:
        return "shift-left expression";
    case ND_SHR:
        return "shift-right expression";
    case ND_BITAND:
        return "bitwise AND expression";
    case ND_BITOR:
        return "bitwise OR expression";
    case ND_BITXOR:
        return "bitwise XOR expression";
    case ND_BITNOT:
        return "bitwise NOT expression";
    case ND_SWITCH:
        return "switch statement";
    case ND_THROW:
        return "throw statement";
    case ND_MATCH:
        return "match expression";
    case ND_LAMBDA:
        return "lambda expression";
    case ND_SEQ:
        return "sequence expression";
    case ND_MANAGED_ARRAY_ADAPT:
        return "managed array adapter";
    case ND_LAMBDA_CALL:
        return "lambda call";
    default:
        return "unknown expression";
    }
}

static void ast_json_write_string(FILE *out, const char *value)
{
    fputc('"', out);
    if (value)
    {
        const unsigned char *p = (const unsigned char *)value;
        while (*p)
        {
            unsigned char ch = *p++;
            switch (ch)
            {
            case '"':
                fputs("\\\"", out);
                break;
            case '\\':
                fputs("\\\\", out);
                break;
            case '\n':
                fputs("\\n", out);
                break;
            case '\r':
                fputs("\\r", out);
                break;
            case '\t':
                fputs("\\t", out);
                break;
            default:
                if (ch < 0x20)
                    fprintf(out, "\\u%04x", (unsigned int)ch);
                else
                    fputc((int)ch, out);
                break;
            }
        }
    }
    fputc('"', out);
}

static void ast_json_write_string_len(FILE *out, const char *value, int length)
{
    fputc('"', out);
    if (value && length > 0)
    {
        const unsigned char *p = (const unsigned char *)value;
        int remaining = length;
        while (remaining-- > 0)
        {
            unsigned char ch = *p++;
            switch (ch)
            {
            case '"':
                fputs("\\\"", out);
                break;
            case '\\':
                fputs("\\\\", out);
                break;
            case '\n':
                fputs("\\n", out);
                break;
            case '\r':
                fputs("\\r", out);
                break;
            case '\t':
                fputs("\\t", out);
                break;
            default:
                if (ch < 0x20)
                    fprintf(out, "\\u%04x", (unsigned int)ch);
                else
                    fputc((int)ch, out);
                break;
            }
        }
    }
    fputc('"', out);
}

static const char *type_kind_name(TypeKind kind)
{
    switch (kind)
    {
    case TY_I8:
        return "i8";
    case TY_U8:
        return "u8";
    case TY_I16:
        return "i16";
    case TY_U16:
        return "u16";
    case TY_I32:
        return "i32";
    case TY_U32:
        return "u32";
    case TY_I64:
        return "i64";
    case TY_U64:
        return "u64";
    case TY_F32:
        return "f32";
    case TY_F64:
        return "f64";
    case TY_F128:
        return "f128";
    case TY_VOID:
        return "void";
    case TY_CHAR:
        return "char";
    case TY_BOOL:
        return "bool";
    case TY_FUNC:
        return "func";
    case TY_PTR:
        return "ptr";
    case TY_STRUCT:
        return "struct";
    case TY_ARRAY:
        return "array";
    case TY_VA_LIST:
        return "va_list";
    case TY_TEMPLATE_PARAM:
        return "template_param";
    case TY_IMPORT:
        return "import";
    default:
        return "unknown";
    }
}

static const char *template_constraint_name(TemplateConstraintKind kind)
{
    switch (kind)
    {
    case TEMPLATE_CONSTRAINT_INTEGRAL:
        return "integral";
    case TEMPLATE_CONSTRAINT_FLOATING:
        return "floating";
    case TEMPLATE_CONSTRAINT_NUMERIC:
        return "numeric";
    case TEMPLATE_CONSTRAINT_POINTER:
        return "pointer";
    case TEMPLATE_CONSTRAINT_NONE:
    default:
        return "none";
    }
}

static void ast_json_write_type(FILE *out, const Type *type, int depth)
{
    if (!type)
    {
        fputs("null", out);
        return;
    }
    if (depth > 6)
    {
        fputs("{\"kind\":\"depth-limit\"}", out);
        return;
    }
    fprintf(out, "{\"kind\":");
    ast_json_write_string(out, type_kind_name(type->kind));
    fprintf(out, ",\"kind_id\":%d", (int)type->kind);
    if (type->kind == TY_PTR)
    {
        fprintf(out, ",\"pointee\":");
        ast_json_write_type(out, type->pointee, depth + 1);
    }
    if (type->kind == TY_ARRAY)
    {
        fprintf(out, ",\"length\":%d,\"unsized\":%s,\"elem\":",
                type->array.length,
                type->array.is_unsized ? "true" : "false");
        ast_json_write_type(out, type->array.elem, depth + 1);
    }
    if (type->kind == TY_FUNC)
    {
        fprintf(out, ",\"varargs\":%s,\"ret\":",
                type->func.is_varargs ? "true" : "false");
        ast_json_write_type(out, type->func.ret, depth + 1);
        fputs(",\"params\":[", out);
        for (int i = 0; i < type->func.param_count; ++i)
        {
            if (i)
                fputc(',', out);
            ast_json_write_type(out, type->func.params ? type->func.params[i] : NULL, depth + 1);
        }
        fputc(']', out);
    }
    if (type->kind == TY_STRUCT)
    {
        fprintf(out, ",\"name\":");
        ast_json_write_string(out, type->struct_name ? type->struct_name : "");
        fprintf(out, ",\"field_count\":%d", type->strct.field_count);
    }
    if (type->kind == TY_TEMPLATE_PARAM)
    {
        fprintf(out, ",\"param\":");
        ast_json_write_string(out, type->template_param_name ? type->template_param_name : "");
        fprintf(out, ",\"index\":%d,\"constraint\":", type->template_param_index);
        ast_json_write_string(out, template_constraint_name(type->template_constraint_kind));
        if (type->template_default_type)
        {
            fputs(",\"default\":", out);
            ast_json_write_type(out, type->template_default_type, depth + 1);
        }
    }
    if (type->kind == TY_IMPORT)
    {
        fprintf(out, ",\"module\":");
        ast_json_write_string(out, type->import_module ? type->import_module : "");
        fprintf(out, ",\"type_name\":");
        ast_json_write_string(out, type->import_type_name ? type->import_type_name : "");
    }
    fputc('}', out);
}

static void ast_json_write_module_path(FILE *out, const ModulePath *path)
{
    if (!path)
    {
        fputs("null", out);
        return;
    }
    fputs("{\"full\":", out);
    ast_json_write_string(out, path->full_name ? path->full_name : "");
    fputs(",\"alias\":", out);
    ast_json_write_string(out, path->alias ? path->alias : "");
    fputs(",\"parts\":[", out);
    for (int i = 0; i < path->part_count; ++i)
    {
        if (i)
            fputc(',', out);
        ast_json_write_string(out, path->parts && path->parts[i] ? path->parts[i] : "");
    }
    fputs("]}", out);
}

static void ast_json_write_node(FILE *out, const Node *node, int depth);

static void ast_json_write_node_array(FILE *out, Node **items, int count, int depth)
{
    fputc('[', out);
    for (int i = 0; i < count; ++i)
    {
        if (i)
            fputc(',', out);
        ast_json_write_node(out, items ? items[i] : NULL, depth + 1);
    }
    fputc(']', out);
}

static void ast_json_write_node(FILE *out, const Node *node, int depth)
{
    if (!node)
    {
        fputs("null", out);
        return;
    }
    if (depth > 64)
    {
        fputs("{\"kind\":\"depth-limit\"}", out);
        return;
    }
    fputs("{\"kind\":", out);
    ast_json_write_string(out, node_kind_name(node->kind));
    fprintf(out, ",\"kind_id\":%d", (int)node->kind);
    if (node->line > 0)
        fprintf(out, ",\"line\":%d,\"col\":%d", node->line, node->col);
    if (node->name)
    {
        fputs(",\"name\":", out);
        ast_json_write_string(out, node->name);
    }
    if (node->call_name)
    {
        fputs(",\"call_name\":", out);
        ast_json_write_string(out, node->call_name);
    }
    if (node->var_name)
    {
        fputs(",\"var_name\":", out);
        ast_json_write_string(out, node->var_name);
    }
    if (node->var_ref)
    {
        fputs(",\"var_ref\":", out);
        ast_json_write_string(out, node->var_ref);
    }
    if (node->field_name)
    {
        fputs(",\"field_name\":", out);
        ast_json_write_string(out, node->field_name);
    }
    if (node->str_data)
    {
        fputs(",\"str\":", out);
        ast_json_write_string_len(out, node->str_data, node->str_len);
        fprintf(out, ",\"str_len\":%d", node->str_len);
    }
    if (node->kind == ND_INT)
    {
        fprintf(out, ",\"int_val\":%lld,\"int_uval\":%llu,\"unsigned\":%s,\"width\":%d",
                (long long)node->int_val,
                (unsigned long long)node->int_uval,
                node->int_is_unsigned ? "true" : "false",
                node->int_width);
    }
    if (node->kind == ND_FLOAT)
    {
        fprintf(out, ",\"float_val\":%.17g", node->float_val);
    }
    if (node->type)
    {
        fputs(",\"type\":", out);
        ast_json_write_type(out, node->type, 0);
    }
    if (node->var_type)
    {
        fputs(",\"var_type\":", out);
        ast_json_write_type(out, node->var_type, 0);
    }
    if (node->ret_type)
    {
        fputs(",\"ret_type\":", out);
        ast_json_write_type(out, node->ret_type, 0);
    }
    if (node->generic_param_names && node->generic_param_count > 0)
    {
        fputs(",\"generic_params\":[", out);
        for (int i = 0; i < node->generic_param_count; ++i)
        {
            if (i)
                fputc(',', out);
            ast_json_write_string(out, node->generic_param_names[i]);
        }
        fputc(']', out);
    }
    if (node->param_names && node->param_count > 0)
    {
        fputs(",\"params\":[", out);
        for (int i = 0; i < node->param_count; ++i)
        {
            if (i)
                fputc(',', out);
            fputs("{\"name\":", out);
            ast_json_write_string(out, node->param_names[i] ? node->param_names[i] : "");
            if (node->param_types)
            {
                fputs(",\"type\":", out);
                ast_json_write_type(out, node->param_types[i], 0);
            }
            if (node->param_const_flags)
            {
                fprintf(out, ",\"const\":%s", node->param_const_flags[i] ? "true" : "false");
            }
            fputc('}', out);
        }
        fputc(']', out);
    }
    if (node->call_type_args && node->call_type_arg_count > 0)
    {
        fputs(",\"call_type_args\":[", out);
        for (int i = 0; i < node->call_type_arg_count; ++i)
        {
            if (i)
                fputc(',', out);
            ast_json_write_type(out, node->call_type_args[i], 0);
        }
        fputc(']', out);
    }
    if (node->is_varargs)
        fprintf(out, ",\"is_varargs\":%s", node->is_varargs ? "true" : "false");
    if (node->is_exposed)
        fprintf(out, ",\"is_exposed\":%s", node->is_exposed ? "true" : "false");

    if (node->lhs || node->rhs)
    {
        fputs(",\"lhs\":", out);
        ast_json_write_node(out, node->lhs, depth + 1);
        fputs(",\"rhs\":", out);
        ast_json_write_node(out, node->rhs, depth + 1);
    }
    if (node->body)
    {
        fputs(",\"body\":", out);
        ast_json_write_node(out, node->body, depth + 1);
    }
    if (node->args && node->arg_count > 0)
    {
        fputs(",\"args\":", out);
        ast_json_write_node_array(out, node->args, node->arg_count, depth + 1);
    }
    if (node->stmts && node->stmt_count > 0)
    {
        fputs(",\"stmts\":", out);
        ast_json_write_node_array(out, node->stmts, node->stmt_count, depth + 1);
    }
    if (node->kind == ND_SWITCH)
    {
        fputs(",\"switch\":{\"expr\":", out);
        ast_json_write_node(out, node->switch_stmt.expr, depth + 1);
        fputs(",\"cases\":[", out);
        for (int i = 0; i < node->switch_stmt.case_count; ++i)
        {
            SwitchCase *cs = &node->switch_stmt.cases[i];
            if (i)
                fputc(',', out);
            fprintf(out, "{\"is_default\":%s,\"value\":", cs->is_default ? "true" : "false");
            ast_json_write_node(out, cs->value, depth + 1);
            fputs(",\"body\":", out);
            ast_json_write_node(out, cs->body, depth + 1);
            fputc('}', out);
        }
        fputs("]}", out);
    }
    if (node->kind == ND_MATCH)
    {
        fputs(",\"match\":{\"expr\":", out);
        ast_json_write_node(out, node->match_stmt.expr, depth + 1);
        fputs(",\"arms\":[", out);
        for (int i = 0; i < node->match_stmt.arm_count; ++i)
        {
            MatchArm *arm = &node->match_stmt.arms[i];
            if (i)
                fputc(',', out);
            fputs("{\"pattern\":", out);
            ast_json_write_node(out, arm->pattern, depth + 1);
            fputs(",\"guard\":", out);
            ast_json_write_node(out, arm->guard, depth + 1);
            fputs(",\"body\":", out);
            ast_json_write_node(out, arm->body, depth + 1);
            fputs(",\"binding\":", out);
            ast_json_write_string(out, arm->binding_name ? arm->binding_name : "");
            fputc('}', out);
        }
        fputs("]}", out);
    }
    if (node->kind == ND_INIT_LIST)
    {
        fputs(",\"init\":{\"count\":", out);
        fprintf(out, "%d", node->init.count);
        fprintf(out, ",\"is_zero\":%s,\"is_array\":%s,\"elems\":", node->init.is_zero ? "true" : "false", node->init.is_array_literal ? "true" : "false");
        ast_json_write_node_array(out, node->init.elems, node->init.count, depth + 1);
        fputs("}", out);
    }
    if (node->kind == ND_UNIT)
    {
        fputs(",\"module\":", out);
        ast_json_write_module_path(out, &node->module_path);
        fputs(",\"imports\":[", out);
        for (int i = 0; i < node->import_count; ++i)
        {
            if (i)
                fputc(',', out);
            ast_json_write_module_path(out, &node->imports[i]);
        }
        fputc(']', out);
    }
    fputc('}', out);
}

void ast_emit_json(FILE *out, const Node *unit, const char *input_path)
{
    if (!out)
        return;
    fputs("{\"file\":", out);
    ast_json_write_string(out, input_path ? input_path : "");
    fputs(",\"unit\":", out);
    ast_json_write_node(out, unit, 0);
    fputc('}', out);
}


static int g_errs = 0;
static int g_warns = 0;

static void vdiag_at(const SourceBuffer *src, int line, int col, const char *sev, const char *fmt, va_list ap)
{
    if (diag_data_log)
    {
        char *message = diag_format_message(fmt, ap);
        diag_emit_data_log(sev, src, line, col, message);
        free(message);
        return;
    }
    const char *file = src && src->filename ? src->filename : "<input>";
    if (diag_use_ansi)
    {
        const char *color = diag_color_for(sev);
        fprintf(stderr, "%s:%d:%d: %s%s%s: ", file, line, col, color, sev, ANSI_RESET);
    }
    else
    {
        fprintf(stderr, "%s:%d:%d: %s: ", file, line, col, sev);
    }
    vfprintf(stderr, fmt, ap);
    fputc('\n', stderr);
    
    if (src && src->src && src->length > 0 && line > 0)
    {
        const char *p = src->src;
        const char *line_start = p;
        int cur = 1;
        int i = 0;
        for (i = 0; i < src->length; i++)
        {
            if (cur == line)
            {
                line_start = p;
                break;
            }
            if (*p == '\n')
            {
                cur++;
            }
            p++;
        }
        
        const char *q = line_start;
        while (q < src->src + src->length && *q != '\n' && *q != '\r')
            q++;
        if (line_start < src->src + src->length)
        {
            fwrite(line_start, 1, (size_t)(q - line_start), stderr);
            fputc('\n', stderr);
            int caret = col > 1 ? col - 1 : 0;
            for (int k = 0; k < caret; k++)
                fputc(' ', stderr);
            if (diag_use_ansi)
            {
                const char *color = diag_color_for(sev);
                fprintf(stderr, "%s^%s\n", color, ANSI_RESET);
            }
            else
            {
                fputc('^', stderr);
                fputc('\n', stderr);
            }
        }
    }
}

static void vdiag(const char *sev, const char *fmt, va_list ap)
{
    if (diag_data_log)
    {
        char *message = diag_format_message(fmt, ap);
        diag_emit_data_log(sev, NULL, 0, 0, message);
        free(message);
        return;
    }
    if (diag_use_ansi)
    {
        const char *color = diag_color_for(sev);
        fprintf(stderr, "%s%s%s: ", color, sev, ANSI_RESET);
    }
    else
    {
        fprintf(stderr, "%s: ", sev);
    }
    vfprintf(stderr, fmt, ap);
    fputc('\n', stderr);
}

void diag_error_at(const SourceBuffer *src, int line, int col, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vdiag_at(src, line, col, "error", fmt, ap);
    va_end(ap);
    g_errs++;
}
void diag_warning_at(const SourceBuffer *src, int line, int col, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vdiag_at(src, line, col, "warning", fmt, ap);
    va_end(ap);
    g_warns++;
}
void diag_note_at(const SourceBuffer *src, int line, int col, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vdiag_at(src, line, col, "note", fmt, ap);
    va_end(ap);
}
void diag_error(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vdiag("error", fmt, ap);
    va_end(ap);
    g_errs++;
}
void diag_warning(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vdiag("warning", fmt, ap);
    va_end(ap);
    g_warns++;
}
void diag_note(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vdiag("note", fmt, ap);
    va_end(ap);
}
int diag_error_count(void) { return g_errs; }
int diag_warning_count(void) { return g_warns; }
void diag_reset(void) { g_errs = g_warns = 0; }
