#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include "ast.h"

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
static Type TY_VOID_SINGLETON = {.kind = TY_VOID};
static Type TY_CHAR_SINGLETON = {.kind = TY_CHAR};

Type *type_i32(void) { return &TY_I32_SINGLETON; }
Type *type_i64(void) { return &TY_I64_SINGLETON; }
Type *type_void(void) { return &TY_VOID_SINGLETON; }
Type *type_char(void) { return &TY_CHAR_SINGLETON; }

Type *type_ptr(Type *to)
{
    Type *t = (Type *)xcalloc(1, sizeof(Type));
    t->kind = TY_PTR;
    t->pointee = to;
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
    return 1;
}

const char *node_kind_name(NodeKind kind)
{
    switch (kind)
    {
    case ND_INT:
        return "integer literal";
    case ND_ADD:
        return "addition expression";
    case ND_MUL:
        return "multiplication expression";
    case ND_DIV:
        return "division expression";
    case ND_RET:
        return "return statement";
    case ND_FUNC:
        return "function";
    case ND_STRING:
        return "string literal";
    case ND_CALL:
        return "function call";
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
    case ND_WHILE:
        return "while statement";
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
    fprintf(stderr, "%s:%d:%d: %s: ", file, line, col, sev);
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
            fputc('^', stderr);
            fputc('\n', stderr);
        }
    }
}

static void vdiag(const char *sev, const char *fmt, va_list ap)
{
    fprintf(stderr, "%s: ", sev);
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
