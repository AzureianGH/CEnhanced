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

void diag_set_use_ansi(int enable)
{
    diag_use_ansi = enable ? 1 : 0;
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

// --- Simple type helpers (keep minimal for now) ---
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
    case ND_NE:
        return "inequality comparison";
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
    case ND_MATCH:
        return "match expression";
    default:
        return "unknown expression";
    }
}

// ---------------- Diagnostics -----------------
static int g_errs = 0;
static int g_warns = 0;

static void vdiag_at(const SourceBuffer *src, int line, int col, const char *sev, const char *fmt, va_list ap)
{
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
    // Print source line and caret if possible
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
        // Find line end
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
