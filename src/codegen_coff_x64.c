#include "ast.h"
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Minimal x64 COFF object generator that emits one .text section with a
// 'main' function: mov eax, imm32; ret. We then link it using MinGW 'cc'.

// Utility: write little-endian values
static void w16(uint8_t *p, size_t off, uint16_t v)
{
    p[off] = v & 0xFF;
    p[off + 1] = (v >> 8) & 0xFF;
}
static void w32(uint8_t *p, size_t off, uint32_t v)
{
    p[off] = v & 0xFF;
    p[off + 1] = (v >> 8) & 0xFF;
    p[off + 2] = (v >> 16) & 0xFF;
    p[off + 3] = (v >> 24) & 0xFF;
}
static void w64(uint8_t *p, size_t off, uint64_t v)
{
    for (int i = 0; i < 8; i++)
        p[off + i] = (v >> (8 * i)) & 0xFF;
}

// Minimal constants for COFF
#define IMAGE_SYM_CLASS_EXTERNAL 2
#define IMAGE_SYM_CLASS_STATIC 3
#define IMAGE_SYM_DTYPE_FUNCTION 0x20
#define IMAGE_REL_AMD64_REL32 0x0004

static int64_t eval_expr(const Node *n)
{
    switch (n->kind)
    {
    case ND_INT:
        return n->int_val;
    case ND_ADD:
        return eval_expr(n->lhs) + eval_expr(n->rhs);
    case ND_STRING:
        return 0; // not numeric
    default:
        diag_error("codegen: unsupported node %d", n->kind);
        exit(1);
    }
}

static void cg_error(const Node *n, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    if (n && n->src)
    {
        char buf[512];
        vsnprintf(buf, sizeof(buf), fmt, ap);
        diag_error_at(n->src, n->line > 0 ? n->line : 1, n->col > 0 ? n->col : 1,
                      "%s", buf);
    }
    else
    {
        vfprintf(stderr, "error: ", ap);
        fputc('\n', stderr);
    }
    va_end(ap);
}

// Helper: build assembly file path from output path by replacing extension with
// .S (or appending)
static void form_asm_path(char *dst, size_t dstsz, const char *out)
{
    if (!out || !*out)
    {
        snprintf(dst, dstsz, "a.S");
        return;
    }
    // Find last path separator
    const char *slash1 = strrchr(out, '/');
    const char *slash2 = strrchr(out, '\\');
    const char *baseStart = out;
    if (slash1 && slash2)
        baseStart = (slash1 > slash2) ? slash1 + 1 : slash2 + 1;
    else if (slash1)
        baseStart = slash1 + 1;
    else if (slash2)
        baseStart = slash2 + 1;
    // Find last dot after baseStart
    const char *dot = strrchr(baseStart, '.');
    if (dot)
    {
        // Replace extension
        size_t prefixLen = (size_t)(dot - out);
        if (prefixLen >= dstsz)
            prefixLen = dstsz - 1;
        memcpy(dst, out, prefixLen);
        dst[prefixLen] = '\0';
        snprintf(dst + prefixLen, dstsz - prefixLen, ".S");
    }
    else
    {
        // Append .S
        size_t olen = strlen(out);
        if (olen >= dstsz)
            olen = dstsz - 1;
        memcpy(dst, out, olen);
        dst[olen] = '\0';
        snprintf(dst + olen, dstsz - olen, ".S");
    }
}

// Helper: derive object file path from provided output or default
static void form_obj_path(char *dst, size_t dstsz, const char *out, int is_linux)
{
    const char *fallback = is_linux ? "a.o" : "a.obj";
    if (!out || !*out)
    {
        snprintf(dst, dstsz, "%s", fallback);
        return;
    }
    const char *slash1 = strrchr(out, '/');
    const char *slash2 = strrchr(out, '\\');
    const char *baseStart = out;
    if (slash1 && slash2)
        baseStart = (slash1 > slash2) ? slash1 + 1 : slash2 + 1;
    else if (slash1)
        baseStart = slash1 + 1;
    else if (slash2)
        baseStart = slash2 + 1;
    const char *dot = strrchr(baseStart, '.');
    if (dot)
    {
        size_t prefixLen = (size_t)(dot - out);
        if (prefixLen >= dstsz)
            prefixLen = dstsz - 1;
        memcpy(dst, out, prefixLen);
        dst[prefixLen] = '\0';
    }
    else
    {
        size_t olen = strlen(out);
        if (olen >= dstsz)
            olen = dstsz - 1;
        memcpy(dst, out, olen);
        dst[olen] = '\0';
    }
    snprintf(dst + strlen(dst), dstsz - strlen(dst), "%s", is_linux ? ".o" : ".obj");
}

// -------------------- Simple Intel GAS ASM backend (Windows x64)
// --------------------
typedef struct
{
    const Node *fn;
    FILE *as;
    int next_lbl;
    struct
    {
        const char *name;
        Type *type;
        int offset;
    } locals[64];
    int local_cnt;
    struct
    {
        const char *data;
        int len;
        int id;
    } strs[256];
    int str_cnt;
    int stack_size; // bytes for locals + shadow space
    int ret_id;     // unique return label id per function
} AsmCtx;

// Return storage width in bytes for a given primitive kind
static int kind_width(int k)
{
    switch (k)
    {
    case TY_I8:
    case TY_U8:
        return 1;
    case TY_I16:
    case TY_U16:
        return 2;
    case TY_I32:
    case TY_U32:
    case TY_F32:
        return 4;
    case TY_F128:
        return 16;
    default:
        return 8; // i64/u64/pointer/f64 default to 8
    }
}

static int type_storage_size(Type *ty)
{
    if (!ty)
        return 8;
    switch (ty->kind)
    {
    case TY_I8:
    case TY_U8:
        return 1;
    case TY_I16:
    case TY_U16:
        return 2;
    case TY_I32:
    case TY_U32:
    case TY_F32:
        return 4;
    case TY_I64:
    case TY_U64:
    case TY_F64:
    case TY_PTR:
        return 8;
    case TY_F128:
        return 16;
    case TY_STRUCT:
        return ty->strct.size_bytes;
    default:
        return 8;
    }
}

static int type_scalar_width(Type *ty)
{
    if (!ty)
        return 8;
    switch (ty->kind)
    {
    case TY_I8:
    case TY_U8:
        return 1;
    case TY_I16:
    case TY_U16:
        return 2;
    case TY_I32:
    case TY_U32:
    case TY_F32:
        return 4;
    case TY_I64:
    case TY_U64:
    case TY_F64:
    case TY_PTR:
        return 8;
    default:
        return 8;
    }
}

static void asm_store_scalar_to_stack(AsmCtx *ac, Type *ty, int offset)
{
    int w = type_scalar_width(ty);
    int kind = ty ? ty->kind : TY_I64;
    if (ty && ty->kind == TY_STRUCT)
    {
        cg_error(NULL, "codegen: struct store not supported as scalar");
        exit(1);
    }
    if (w == 1)
        fprintf(ac->as, "  mov byte ptr [rbp%+d], al\n", offset);
    else if (w == 2)
        fprintf(ac->as, "  mov word ptr [rbp%+d], ax\n", offset);
    else if (w == 4)
        fprintf(ac->as, "  mov dword ptr [rbp%+d], eax\n", offset);
    else if (kind == TY_F128)
        fprintf(ac->as, "  movdqu xmmword ptr [rbp%+d], xmm0\n", offset);
    else
        fprintf(ac->as, "  mov [rbp%+d], rax\n", offset);
}

static void asm_store_scalar_to_ptr(AsmCtx *ac, Type *ty, const char *reg)
{
    int w = type_scalar_width(ty);
    int kind = ty ? ty->kind : TY_I64;
    if (ty && ty->kind == TY_STRUCT)
    {
        cg_error(NULL, "codegen: struct store not supported as scalar");
        exit(1);
    }
    if (w == 1)
        fprintf(ac->as, "  mov byte ptr [%s], al\n", reg);
    else if (w == 2)
        fprintf(ac->as, "  mov word ptr [%s], ax\n", reg);
    else if (w == 4)
        fprintf(ac->as, "  mov dword ptr [%s], eax\n", reg);
    else if (kind == TY_F128)
        fprintf(ac->as, "  movdqu xmmword ptr [%s], xmm0\n", reg);
    else
        fprintf(ac->as, "  mov [%s], rax\n", reg);
}

static void asm_zero_bytes(AsmCtx *ac, int base_off, int size)
{
    if (size <= 0)
        return;
    int off = base_off;
    int remaining = size;
    while (remaining >= 8)
    {
        fprintf(ac->as, "  mov qword ptr [rbp%+d], 0\n", off);
        off += 8;
        remaining -= 8;
    }
    if (remaining >= 4)
    {
        fprintf(ac->as, "  mov dword ptr [rbp%+d], 0\n", off);
        off += 4;
        remaining -= 4;
    }
    if (remaining >= 2)
    {
        fprintf(ac->as, "  mov word ptr [rbp%+d], 0\n", off);
        off += 2;
        remaining -= 2;
    }
    if (remaining > 0)
        fprintf(ac->as, "  mov byte ptr [rbp%+d], 0\n", off);
}

static int asm_struct_find_field(const Type *st, const char *name)
{
    if (!st || st->kind != TY_STRUCT || !name)
        return -1;
    for (int i = 0; i < st->strct.field_count; i++)
    {
        if (st->strct.field_names && st->strct.field_names[i] &&
            strcmp(st->strct.field_names[i], name) == 0)
            return i;
    }
    return -1;
}

// Truncate/sign- or zero-extend arithmetic result in RAX to match a type kind
static void asm_trunc_to_kind(FILE *as, int k)
{
    switch (k)
    {
    case TY_I8:
        // sign-extend from 8-bit
        fprintf(as, "  movsx eax, al\n");
        break;
    case TY_U8:
        // zero-extend from 8-bit
        fprintf(as, "  movzx eax, al\n");
        break;
    case TY_I16:
        // sign-extend from 16-bit
        fprintf(as, "  movsx eax, ax\n");
        break;
    case TY_U16:
        // zero-extend from 16-bit
        fprintf(as, "  movzx eax, ax\n");
        break;
    case TY_I32:
    case TY_U32:
        // ensure upper 32 bits are clean (implicitly true if we used 32-bit ops),
        // but be explicit to avoid surprises when preceding used 64-bit ops
        fprintf(as, "  mov eax, eax\n");
        break;
    default:
        // 64-bit and others: no truncation needed
        break;
    }
}

// Emit a store of an incoming argument register to a local stack slot according
// to width and OS ABI
static void asm_store_param_width(FILE *as, int is_linux, int pi, int off,
                                  int k)
{
    int w = kind_width(k);
    if (is_linux)
    {
        // Linux SysV: rdi, rsi, rdx, rcx, r8, r9
        const char *r8s[] = {"dil", "sil", "dl", "cl", "r8b", "r9b"};
        const char *r16s[] = {"di", "si", "dx", "cx", "r8w", "r9w"};
        const char *r32s[] = {"edi", "esi", "edx", "ecx", "r8d", "r9d"};
        const char *r64s[] = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};
        if (pi >= 0 && pi < 6)
        {
            if (w == 1)
                fprintf(as, "  mov byte ptr [rbp%+d], %s\n", off, r8s[pi]);
            else if (w == 2)
                fprintf(as, "  mov word ptr [rbp%+d], %s\n", off, r16s[pi]);
            else if (w == 4)
                fprintf(as, "  mov dword ptr [rbp%+d], %s\n", off, r32s[pi]);
            else
                fprintf(as, "  mov [rbp%+d], %s\n", off, r64s[pi]);
        }
        else
        {
            // TODO: stack-passed args
        }
    }
    else
    {
        // Windows x64: rcx, rdx, r8, r9
        const char *r8s[] = {"cl", "dl", "r8b", "r9b"};
        const char *r16s[] = {"cx", "dx", "r8w", "r9w"};
        const char *r32s[] = {"ecx", "edx", "r8d", "r9d"};
        const char *r64s[] = {"rcx", "rdx", "r8", "r9"};
        if (pi >= 0 && pi < 4)
        {
            if (w == 1)
                fprintf(as, "  mov byte ptr [rbp%+d], %s\n", off, r8s[pi]);
            else if (w == 2)
                fprintf(as, "  mov word ptr [rbp%+d], %s\n", off, r16s[pi]);
            else if (w == 4)
                fprintf(as, "  mov dword ptr [rbp%+d], %s\n", off, r32s[pi]);
            else
                fprintf(as, "  mov [rbp%+d], %s\n", off, r64s[pi]);
        }
        else
        {
            // TODO: stack-passed args (>4)
        }
    }
}

static int asm_find_local(AsmCtx *ac, const char *name)
{
    for (int i = 0; i < ac->local_cnt; i++)
        if (strcmp(ac->locals[i].name, name) == 0)
            return i;
    return -1;
}
static int asm_add_local(AsmCtx *ac, const char *name, Type *ty, int size)
{
    int off = ac->stack_size + size;
    ac->stack_size = off;
    if (ac->local_cnt < 64)
    {
        ac->locals[ac->local_cnt].name = name;
        ac->locals[ac->local_cnt].type = ty;
        ac->locals[ac->local_cnt].offset = -off;
        ac->local_cnt++;
    }
    return -off;
}
// ---- String literal decoding (C-like escapes) ----
static int is_hex(int c)
{
    return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') ||
           (c >= 'A' && c <= 'F');
}
static int hex_val(int c)
{
    if (c >= '0' && c <= '9')
        return c - '0';
    if (c >= 'a' && c <= 'f')
        return 10 + (c - 'a');
    if (c >= 'A' && c <= 'F')
        return 10 + (c - 'A');
    return -1;
}
static int is_oct(int c) { return c >= '0' && c <= '7'; }
static int utf8_encode(uint32_t cp, unsigned char out[4])
{
    if (cp <= 0x7F)
    {
        out[0] = (unsigned char)cp;
        return 1;
    }
    else if (cp <= 0x7FF)
    {
        out[0] = 0xC0 | (cp >> 6);
        out[1] = 0x80 | (cp & 0x3F);
        return 2;
    }
    else if (cp <= 0xFFFF)
    {
        out[0] = 0xE0 | (cp >> 12);
        out[1] = 0x80 | ((cp >> 6) & 0x3F);
        out[2] = 0x80 | (cp & 0x3F);
        return 3;
    }
    else
    {
        out[0] = 0xF0 | (cp >> 18);
        out[1] = 0x80 | ((cp >> 12) & 0x3F);
        out[2] = 0x80 | ((cp >> 6) & 0x3F);
        out[3] = 0x80 | (cp & 0x3F);
        return 4;
    }
}

static unsigned char *decode_c_escapes(const char *s, int len, int *out_len)
{
    // s is the raw characters inside the quotes (may contain backslashes)
    unsigned char *buf =
        (unsigned char *)xmalloc((size_t)len); // cooked length <= raw length
    int bi = 0;
    for (int i = 0; i < len;)
    {
        unsigned char ch = (unsigned char)s[i++];
        if (ch != '\\')
        {
            buf[bi++] = ch;
            continue;
        }
        if (i >= len)
        {
            buf[bi++] = '\\';
            break;
        }
        char e = s[i++];
        switch (e)
        {
        case '\\':
            buf[bi++] = '\\';
            break;
        case '\"':
            buf[bi++] = '\"';
            break;
        case '\'':
            buf[bi++] = '\'';
            break;
        case 'n':
            buf[bi++] = 0x0A;
            break;
        case 'r':
            buf[bi++] = 0x0D;
            break;
        case 't':
            buf[bi++] = 0x09;
            break;
        case 'v':
            buf[bi++] = 0x0B;
            break;
        case 'b':
            buf[bi++] = 0x08;
            break;
        case 'a':
            buf[bi++] = 0x07;
            break;
        case 'f':
            buf[bi++] = 0x0C;
            break;
        case '?':
            buf[bi++] = '?';
            break;
        case 'e':
            buf[bi++] = 0x1B;
            break; // ESC (non-standard but common)
        case 'x':
        {
            int v = 0, digits = 0;
            while (i < len && is_hex((unsigned char)s[i]))
            {
                v = (v << 4) | hex_val((unsigned char)s[i]);
                i++;
                digits++;
            }
            if (digits == 0)
            {
                buf[bi++] = 'x';
            }
            else
            {
                buf[bi++] = (unsigned char)(v & 0xFF);
            }
            break;
        }
        case 'u':
        {
            // exactly 4 hex digits
            if (i + 4 <= len && is_hex((unsigned char)s[i]) &&
                is_hex((unsigned char)s[i + 1]) && is_hex((unsigned char)s[i + 2]) &&
                is_hex((unsigned char)s[i + 3]))
            {
                uint32_t v = (hex_val((unsigned char)s[i]) << 12) |
                             (hex_val((unsigned char)s[i + 1]) << 8) |
                             (hex_val((unsigned char)s[i + 2]) << 4) |
                             hex_val((unsigned char)s[i + 3]);
                i += 4;
                unsigned char tmp[4];
                int n = utf8_encode(v, tmp);
                for (int k = 0; k < n; k++)
                    buf[bi++] = tmp[k];
            }
            else
            {
                buf[bi++] = 'u';
            }
            break;
        }
        case 'U':
        {
            if (i + 8 <= len)
            {
                int ok = 1;
                uint32_t v = 0;
                for (int k = 0; k < 8; k++)
                {
                    if (!is_hex((unsigned char)s[i + k]))
                    {
                        ok = 0;
                        break;
                    }
                    v = (v << 4) | hex_val((unsigned char)s[i + k]);
                }
                if (ok)
                {
                    i += 8;
                    unsigned char tmp[4];
                    int n = utf8_encode(v, tmp);
                    for (int k = 0; k < n; k++)
                        buf[bi++] = tmp[k];
                }
                else
                    buf[bi++] = 'U';
            }
            else
            {
                buf[bi++] = 'U';
            }
            break;
        }
        default:
        {
            if (e >= '0' && e <= '7')
            {
                int v = e - '0';
                int cnt = 1;
                while (cnt < 3 && i < len && is_oct((unsigned char)s[i]))
                {
                    v = (v << 3) | (s[i] - '0');
                    i++;
                    cnt++;
                }
                buf[bi++] = (unsigned char)(v & 0xFF);
            }
            else
            {
                // Unknown escape, keep char as-is
                buf[bi++] = (unsigned char)e;
            }
            break;
        }
        }
    }
    *out_len = bi;
    return buf;
}

static int asm_intern_string(AsmCtx *ac, const char *s, int len)
{
    int cooked_len = 0;
    unsigned char *cooked = decode_c_escapes(s, len, &cooked_len);
    for (int i = 0; i < ac->str_cnt; i++)
    {
        if (ac->strs[i].len == cooked_len &&
            memcmp(ac->strs[i].data, cooked, (size_t)cooked_len) == 0)
        {
            free(cooked);
            return ac->strs[i].id;
        }
    }
    int id = ac->str_cnt;
    if (id < (int)(sizeof(ac->strs) / sizeof(ac->strs[0])))
    {
        char *store = (char *)xmalloc((size_t)cooked_len);
        memcpy(store, cooked, (size_t)cooked_len);
        ac->strs[id].data = store;
        ac->strs[id].len = cooked_len;
        ac->strs[id].id = id;
        ac->str_cnt++;
    }
    free(cooked);
    return id;
}

static void asm_emit_str_section(AsmCtx *ac)
{
    if (ac->str_cnt == 0)
        return;
    // Section differs by OS
    if (ac->fn && ac->fn->type &&
        ac->fn->type->kind)
    { /* no-op to silence warnings */
    }
    // We'll choose based on a global-ish flag: emit_linux is set by prologue
    extern int __chance_emit_linux;
    if (__chance_emit_linux)
        fprintf(ac->as, ".section .rodata\n");
    else
        fprintf(ac->as, ".section .rdata,\"dr\"\n");
    for (int i = 0; i < ac->str_cnt; i++)
    {
        int lid = ac->strs[i].id;
        fprintf(ac->as, ".Lstr%d:\n  .asciz \"", lid);
        const unsigned char *p = (const unsigned char *)ac->strs[i].data;
        int n = ac->strs[i].len;
        for (int j = 0; j < n; j++)
        {
            unsigned char ch = p[j];
            switch (ch)
            {
            case '\\':
                fputs("\\\\", ac->as);
                break;
            case '"':
                fputs("\\\"", ac->as);
                break;
            case '\n':
                fputs("\\n", ac->as);
                break;
            case '\r':
                fputs("\\r", ac->as);
                break;
            case '\t':
                fputs("\\t", ac->as);
                break;
            case '\v':
                fputs("\\v", ac->as);
                break;
            case '\b':
                fputs("\\b", ac->as);
                break;
            case '\a':
                fputs("\\a", ac->as);
                break;
            case '\f':
                fputs("\\f", ac->as);
                break;
            default:
                if (ch >= 0x20 && ch < 0x7F)
                {
                    fputc((int)ch, ac->as);
                }
                else
                {
                    fprintf(ac->as, "\\x%02X", ch);
                }
            }
        }
        fprintf(ac->as, "\"\n");
    }
}

static void asm_eval_expr_to_rax(AsmCtx *ac, const Node *e);

static void asm_addr_for_lvalue_to_rax(AsmCtx *ac, const Node *lv)
{
    if (lv->kind == ND_VAR)
    {
        int idx = asm_find_local(ac, lv->var_ref);
        if (idx < 0)
        {
            cg_error(lv, "codegen: unknown local '%s'", lv->var_ref);
            exit(1);
        }
        int off = ac->locals[idx].offset;
        fprintf(ac->as, "  lea rax, [rbp%+d]\n", off);
        return;
    }
    if (lv->kind ==
        ND_INDEX)
    {
        // Compute address of element: base + index * elem_size
        // Determine base pointer type to get element size
        Type *baseTy = NULL;
        // base address -> rax
        if (lv->lhs->kind == ND_VAR)
        {
            int idx = asm_find_local(ac, lv->lhs->var_ref);
            if (idx < 0)
            {
                cg_error(lv, "codegen: unknown local in index");
                exit(1);
            }
            int off = ac->locals[idx].offset;
            baseTy = ac->locals[idx].type;
            fprintf(ac->as, "  mov rax, [rbp%+d]\n", off);
        }
        else if (lv->lhs->kind == ND_CAST && lv->lhs->lhs->kind == ND_VAR)
        {
            int idx = asm_find_local(ac, lv->lhs->lhs->var_ref);
            if (idx < 0)
            {
                cg_error(lv, "codegen: unknown local in cast index");
                exit(1);
            }
            int off = ac->locals[idx].offset;
            // Prefer the cast type if available
            baseTy = lv->lhs->type ? lv->lhs->type : ac->locals[idx].type;
            fprintf(ac->as, "  mov rax, [rbp%+d]\n", off);
        }
        else
        {
            cg_error(lv, "codegen: unsupported index base");
            exit(1);
        }
        // preserve base in rdx, compute index into rax, then add with scaling
        fprintf(ac->as, "  mov rdx, rax\n");
        asm_eval_expr_to_rax(ac, lv->rhs);
        int elem_sz = 1;
        if (baseTy && baseTy->kind == TY_PTR && baseTy->pointee)
        {
            elem_sz = kind_width(baseTy->pointee->kind);
            if (elem_sz <= 0)
                elem_sz = 1;
        }
        if (elem_sz == 1)
        {
            fprintf(ac->as, "  add rax, rdx\n");
        }
        else if (elem_sz == 2 || elem_sz == 4 || elem_sz == 8)
        {
            int scale = (elem_sz == 2) ? 1 : (elem_sz == 4) ? 2 : 3; // 2^1,2^2,2^3
            fprintf(ac->as, "  lea rax, [rdx + rax*%d]\n", 1 << scale);
        }
        else
        {
            fprintf(ac->as, "  imul rax, %d\n  add rax, rdx\n", elem_sz);
        }
        return;
    }
    if (lv->kind == ND_MEMBER)
    {
        if (lv->is_pointer_deref)
        {
            asm_eval_expr_to_rax(ac, lv->lhs);
            if (lv->field_offset)
                fprintf(ac->as, "  add rax, %d\n", lv->field_offset);
        }
        else
        {
            asm_addr_for_lvalue_to_rax(ac, lv->lhs);
            if (lv->field_offset)
                fprintf(ac->as, "  add rax, %d\n", lv->field_offset);
        }
        return;
    }
    cg_error(lv, "codegen: unsupported lvalue kind %d", lv->kind);
    exit(1);
}

static void asm_eval_expr_to_rax(AsmCtx *ac, const Node *e)
{
    // debug disabled
    switch (e->kind)
    {
    case ND_INT:
        fprintf(ac->as, "  mov eax, %d\n", (int)(int32_t)e->int_val);
        break;
    case ND_INDEX:
    {
        // Load from *(base + index) with correct element width
        asm_addr_for_lvalue_to_rax(ac, e);
        // Derive element type
        int k = TY_U8; // default to byte
        if (e->lhs)
        {
            const Node *b = e->lhs;
            Type *bt = NULL;
            if (b->kind == ND_VAR)
            {
                int idx = asm_find_local(ac, b->var_ref);
                if (idx >= 0)
                    bt = ac->locals[idx].type;
            }
            else if (b->kind == ND_CAST)
            {
                bt = b->type;
                if (!bt && b->lhs && b->lhs->kind == ND_VAR)
                {
                    int idx = asm_find_local(ac, b->lhs->var_ref);
                    if (idx >= 0)
                        bt = ac->locals[idx].type;
                }
            }
            if (bt && bt->kind == TY_PTR && bt->pointee)
                k = bt->pointee->kind;
        }
        if (k == TY_I8)
            fprintf(ac->as, "  movsx eax, byte ptr [rax]\n");
        else if (k == TY_U8)
            fprintf(ac->as, "  movzx eax, byte ptr [rax]\n");
        else if (k == TY_I16)
            fprintf(ac->as, "  movsx eax, word ptr [rax]\n");
        else if (k == TY_U16)
            fprintf(ac->as, "  movzx eax, word ptr [rax]\n");
        else if (k == TY_I32 || k == TY_U32)
            fprintf(ac->as, "  mov eax, dword ptr [rax]\n");
        else /* 64-bit or other */
            fprintf(ac->as, "  mov rax, [rax]\n");
        break;
    }
    case ND_MEMBER:
    {
        asm_addr_for_lvalue_to_rax(ac, e);
        Type *ft = e->type;
        if (ft && ft->kind == TY_STRUCT)
        {
            cg_error(e, "codegen: loading struct member by value not supported yet");
            exit(1);
        }
        int k = ft ? ft->kind : TY_I64;
        if (k == TY_I8)
            fprintf(ac->as, "  movsx eax, byte ptr [rax]\n");
        else if (k == TY_U8)
            fprintf(ac->as, "  movzx eax, byte ptr [rax]\n");
        else if (k == TY_I16)
            fprintf(ac->as, "  movsx eax, word ptr [rax]\n");
        else if (k == TY_U16)
            fprintf(ac->as, "  movzx eax, word ptr [rax]\n");
        else if (k == TY_I32 || k == TY_U32 || k == TY_F32)
            fprintf(ac->as, "  mov eax, dword ptr [rax]\n");
        else
            fprintf(ac->as, "  mov rax, [rax]\n");
        break;
    }
    case ND_ADDR:
    {
        if (!e->lhs)
        {
            cg_error(e, "codegen: '&' missing operand");
            exit(1);
        }
        asm_addr_for_lvalue_to_rax(ac, e->lhs);
        break;
    }
    case ND_VAR:
    {
        int idx = asm_find_local(ac, e->var_ref);
        if (idx < 0)
        {
            cg_error(e, "codegen: unknown local '%s'", e->var_ref);
            exit(1);
        }
        int off = ac->locals[idx].offset;
        Type *ty = ac->locals[idx].type;
        int k = ty ? ty->kind : TY_I64;
        if (k == TY_I8)
            fprintf(ac->as, "  movsx eax, byte ptr [rbp%+d]\n", off);
        else if (k == TY_U8)
            fprintf(ac->as, "  movzx eax, byte ptr [rbp%+d]\n", off);
        else if (k == TY_I16)
            fprintf(ac->as, "  movsx eax, word ptr [rbp%+d]\n", off);
        else if (k == TY_U16)
            fprintf(ac->as, "  movzx eax, word ptr [rbp%+d]\n", off);
        else if (k == TY_I32 || k == TY_U32)
            fprintf(ac->as, "  mov eax, dword ptr [rbp%+d]\n", off);
        else
            fprintf(ac->as, "  mov rax, [rbp%+d]\n", off);
        break;
    }
    case ND_ADD:
        asm_eval_expr_to_rax(ac, e->lhs);
        fprintf(ac->as, "  push rax\n");
        asm_eval_expr_to_rax(ac, e->rhs);
        fprintf(ac->as, "  mov rcx, rax\n  pop rax\n  add rax, rcx\n");
        // Keep result width consistent with expression type (no implicit promotion)
        if (e->type && e->type->kind)
            asm_trunc_to_kind(ac->as, e->type->kind);
        else if (e->lhs && e->lhs->type)
            asm_trunc_to_kind(ac->as, e->lhs->type->kind);
        break;
    case ND_SUB:
        asm_eval_expr_to_rax(ac, e->lhs);
        fprintf(ac->as, "  push rax\n");
        asm_eval_expr_to_rax(ac, e->rhs);
        fprintf(ac->as, "  mov rcx, rax\n  pop rax\n  sub rax, rcx\n");
        if (e->type && e->type->kind)
            asm_trunc_to_kind(ac->as, e->type->kind);
        else if (e->lhs && e->lhs->type)
            asm_trunc_to_kind(ac->as, e->lhs->type->kind);
        break;
    case ND_MUL:
    {
        // rax = lhs * rhs; handle width via operand size
        asm_eval_expr_to_rax(ac, e->lhs);
        fprintf(ac->as, "  push rax\n");
        asm_eval_expr_to_rax(ac, e->rhs);
        fprintf(ac->as, "  mov rcx, rax\n  pop rax\n");
        int k = e->lhs && e->lhs->type ? e->lhs->type->kind : TY_I64;
        int w = kind_width(k);
        if (w <= 4)
            fprintf(ac->as, "  imul eax, ecx\n");
        else
            fprintf(ac->as, "  imul rax, rcx\n");
        if (e->type && e->type->kind)
            asm_trunc_to_kind(ac->as, e->type->kind);
        else if (e->lhs && e->lhs->type)
            asm_trunc_to_kind(ac->as, e->lhs->type->kind);
        break;
    }
    case ND_DIV:
    {
        // rax = lhs / rhs; use idiv/div based on signedness, width 32/64.
        asm_eval_expr_to_rax(ac, e->lhs);
        fprintf(ac->as, "  push rax\n");
        asm_eval_expr_to_rax(ac, e->rhs);
        fprintf(ac->as, "  mov rcx, rax\n  pop rax\n");
        int k = e->lhs && e->lhs->type ? e->lhs->type->kind : TY_I64;
        int is_unsigned = (k == TY_U8 || k == TY_U16 || k == TY_U32 || k == TY_U64);
        int w = kind_width(k);
        if (w <= 4)
        {
            if (is_unsigned)
                fprintf(ac->as, "  xor edx, edx\n  div ecx\n");
            else
                fprintf(ac->as, "  cdq\n  idiv ecx\n");
        }
        else
        {
            if (is_unsigned)
                fprintf(ac->as, "  xor rdx, rdx\n  div rcx\n");
            else
                fprintf(ac->as, "  cqo\n  idiv rcx\n");
        }
        // quotient already in rax
        if (e->type && e->type->kind)
            asm_trunc_to_kind(ac->as, e->type->kind);
        else if (e->lhs && e->lhs->type)
            asm_trunc_to_kind(ac->as, e->lhs->type->kind);
        break;
    }
    case ND_GT_EXPR:
    {
        asm_eval_expr_to_rax(ac, e->lhs);
        fprintf(ac->as, "  push rax\n");
        asm_eval_expr_to_rax(ac, e->rhs);
        int lkind = e->lhs && e->lhs->type ? e->lhs->type->kind : TY_I64;
        int rkind = e->rhs && e->rhs->type ? e->rhs->type->kind : TY_I64;
        int lw = kind_width(lkind);
        int rw = kind_width(rkind);
        int use32 = (lw <= 4 && rw <= 4);
        int unsigned_compare = (lkind == TY_U8 || lkind == TY_U16 || lkind == TY_U32 || lkind == TY_U64 ||
                                 rkind == TY_U8 || rkind == TY_U16 || rkind == TY_U32 || rkind == TY_U64);
        fprintf(ac->as, "  mov rcx, rax\n  pop rax\n");
        if (use32)
            fprintf(ac->as, "  cmp eax, ecx\n");
        else
            fprintf(ac->as, "  cmp rax, rcx\n");
        if (unsigned_compare)
            fprintf(ac->as, "  seta al\n  movzx eax, al\n");
        else
            fprintf(ac->as, "  setg al\n  movzx eax, al\n");
        break;
    }
    case ND_LT:
    {
        asm_eval_expr_to_rax(ac, e->lhs);
        fprintf(ac->as, "  push rax\n");
        asm_eval_expr_to_rax(ac, e->rhs);
        int lkind = e->lhs && e->lhs->type ? e->lhs->type->kind : TY_I64;
        int rkind = e->rhs && e->rhs->type ? e->rhs->type->kind : TY_I64;
        int lw = kind_width(lkind);
        int rw = kind_width(rkind);
        int use32 = (lw <= 4 && rw <= 4);
        int unsigned_compare = (lkind == TY_U8 || lkind == TY_U16 || lkind == TY_U32 || lkind == TY_U64 ||
                                 rkind == TY_U8 || rkind == TY_U16 || rkind == TY_U32 || rkind == TY_U64);
        fprintf(ac->as, "  mov rcx, rax\n  pop rax\n");
        if (use32)
            fprintf(ac->as, "  cmp eax, ecx\n");
        else
            fprintf(ac->as, "  cmp rax, rcx\n");
        if (unsigned_compare)
            fprintf(ac->as, "  setb al\n  movzx eax, al\n");
        else
            fprintf(ac->as, "  setl al\n  movzx eax, al\n");
        break;
    }
    case ND_LE:
    {
        asm_eval_expr_to_rax(ac, e->lhs);
        fprintf(ac->as, "  push rax\n");
        asm_eval_expr_to_rax(ac, e->rhs);
        int lkind = e->lhs && e->lhs->type ? e->lhs->type->kind : TY_I64;
        int rkind = e->rhs && e->rhs->type ? e->rhs->type->kind : TY_I64;
        int lw = kind_width(lkind);
        int rw = kind_width(rkind);
        int use32 = (lw <= 4 && rw <= 4);
        int unsigned_compare = (lkind == TY_U8 || lkind == TY_U16 || lkind == TY_U32 || lkind == TY_U64 ||
                                 rkind == TY_U8 || rkind == TY_U16 || rkind == TY_U32 || rkind == TY_U64);
        fprintf(ac->as, "  mov rcx, rax\n  pop rax\n");
        if (use32)
            fprintf(ac->as, "  cmp eax, ecx\n");
        else
            fprintf(ac->as, "  cmp rax, rcx\n");
        if (unsigned_compare)
            fprintf(ac->as, "  setbe al\n  movzx eax, al\n");
        else
            fprintf(ac->as, "  setle al\n  movzx eax, al\n");
        break;
    }
    case ND_GE:
    {
        asm_eval_expr_to_rax(ac, e->lhs);
        fprintf(ac->as, "  push rax\n");
        asm_eval_expr_to_rax(ac, e->rhs);
        int lkind = e->lhs && e->lhs->type ? e->lhs->type->kind : TY_I64;
        int rkind = e->rhs && e->rhs->type ? e->rhs->type->kind : TY_I64;
        int lw = kind_width(lkind);
        int rw = kind_width(rkind);
        int use32 = (lw <= 4 && rw <= 4);
        int unsigned_compare = (lkind == TY_U8 || lkind == TY_U16 || lkind == TY_U32 || lkind == TY_U64 ||
                                 rkind == TY_U8 || rkind == TY_U16 || rkind == TY_U32 || rkind == TY_U64);
        fprintf(ac->as, "  mov rcx, rax\n  pop rax\n");
        if (use32)
            fprintf(ac->as, "  cmp eax, ecx\n");
        else
            fprintf(ac->as, "  cmp rax, rcx\n");
        if (unsigned_compare)
            fprintf(ac->as, "  setae al\n  movzx eax, al\n");
        else
            fprintf(ac->as, "  setge al\n  movzx eax, al\n");
        break;
    }
    case ND_EQ:
    {
        asm_eval_expr_to_rax(ac, e->lhs);
        fprintf(ac->as, "  push rax\n");
        asm_eval_expr_to_rax(ac, e->rhs);
        int lkind = e->lhs && e->lhs->type ? e->lhs->type->kind : TY_I64;
        int rkind = e->rhs && e->rhs->type ? e->rhs->type->kind : TY_I64;
        int lw = kind_width(lkind);
        int rw = kind_width(rkind);
        int use32 = (lw <= 4 && rw <= 4);
        fprintf(ac->as, "  mov rcx, rax\n  pop rax\n");
        if (use32)
            fprintf(ac->as, "  cmp eax, ecx\n");
        else
            fprintf(ac->as, "  cmp rax, rcx\n");
        fprintf(ac->as, "  sete al\n  movzx eax, al\n");
        break;
    }
    case ND_NE:
    {
        asm_eval_expr_to_rax(ac, e->lhs);
        fprintf(ac->as, "  push rax\n");
        asm_eval_expr_to_rax(ac, e->rhs);
        int lkind = e->lhs && e->lhs->type ? e->lhs->type->kind : TY_I64;
        int rkind = e->rhs && e->rhs->type ? e->rhs->type->kind : TY_I64;
        int lw = kind_width(lkind);
        int rw = kind_width(rkind);
        int use32 = (lw <= 4 && rw <= 4);
        fprintf(ac->as, "  mov rcx, rax\n  pop rax\n");
        if (use32)
            fprintf(ac->as, "  cmp eax, ecx\n");
        else
            fprintf(ac->as, "  cmp rax, rcx\n");
        fprintf(ac->as, "  setne al\n  movzx eax, al\n");
        break;
    }
    case ND_CAST:
        asm_eval_expr_to_rax(ac, e->lhs);
        if (e->type)
            asm_trunc_to_kind(ac->as, e->type->kind);
        break;
    case ND_LAND:
    {
        int lid = ac->next_lbl++;
        asm_eval_expr_to_rax(ac, e->lhs);
        fprintf(ac->as, "  cmp eax, 0\n  je .Lland_false%d\n", lid);
        asm_eval_expr_to_rax(ac, e->rhs);
        fprintf(ac->as,
                "  cmp eax, 0\n  je .Lland_false%d\n  mov eax, 1\n  jmp "
                ".Lland_end%d\n.Lland_false%d:\n  xor eax, eax\n.Lland_end%d:\n",
                lid, lid, lid, lid);
        break;
    }
    case ND_LOR:
    {
        int lid = ac->next_lbl++;
        int lend = ac->next_lbl++;
        // if (lhs != 0) goto true; else evaluate rhs
        asm_eval_expr_to_rax(ac, e->lhs);
        fprintf(ac->as, "  cmp eax, 0\n  jne .Llor_true%d\n", lid);
        asm_eval_expr_to_rax(ac, e->rhs);
    fprintf(ac->as, "  cmp eax, 0\n  jne .Llor_true%d\n  xor eax, eax\n  jmp .Llor_end%d\n.Llor_true%d:\n  mov eax, 1\n.Llor_end%d:\n",
        lid, lend, lid, lend);
        break;
    }
    case ND_COND:
    {
        // e->lhs ? e->rhs : e->body
        int lid = ac->next_lbl++;
        int lend = ac->next_lbl++;
        asm_eval_expr_to_rax(ac, e->lhs);
        fprintf(ac->as, "  cmp eax, 0\n  je .Lcond_else%d\n", lid);
        // then branch
        asm_eval_expr_to_rax(ac, e->rhs);
        fprintf(ac->as, "  jmp .Lcond_end%d\n.Lcond_else%d:\n", lend, lid);
        // else branch
        asm_eval_expr_to_rax(ac, e->body);
        fprintf(ac->as, ".Lcond_end%d:\n", lend);
        // Result already in rax; optional truncation to known type
        if (e->type)
            asm_trunc_to_kind(ac->as, e->type->kind);
        break;
    }
    case ND_PREINC:
    {
        if (!e->lhs || e->lhs->kind != ND_VAR)
        {
            cg_error(e, "codegen: ++ expects variable");
            exit(1);
        }
        int idx = asm_find_local(ac, e->lhs->var_ref);
        if (idx < 0)
        {
            cg_error(e, "codegen: unknown local in preinc");
            exit(1);
        }
        int off = ac->locals[idx].offset;
        Type *ty = ac->locals[idx].type;
        int k = ty ? ty->kind : TY_I64;
        if (k == TY_I8)
        {
            fprintf(ac->as,
                    "  mov al, byte ptr [rbp%+d]\n  add al, 1\n  mov byte ptr "
                    "[rbp%+d], al\n  movsx eax, al\n",
                    off, off);
        }
        else if (k == TY_U8)
        {
            fprintf(ac->as,
                    "  mov al, byte ptr [rbp%+d]\n  add al, 1\n  mov byte ptr "
                    "[rbp%+d], al\n  movzx eax, al\n",
                    off, off);
        }
        else if (k == TY_I16)
        {
            fprintf(ac->as,
                    "  mov ax, word ptr [rbp%+d]\n  add ax, 1\n  mov word ptr "
                    "[rbp%+d], ax\n  movsx eax, ax\n",
                    off, off);
        }
        else if (k == TY_U16)
        {
            fprintf(ac->as,
                    "  mov ax, word ptr [rbp%+d]\n  add ax, 1\n  mov word ptr "
                    "[rbp%+d], ax\n  movzx eax, ax\n",
                    off, off);
        }
        else if (k == TY_I32 || k == TY_U32)
        {
            fprintf(ac->as,
                    "  mov eax, dword ptr [rbp%+d]\n  add eax, 1\n  mov dword ptr "
                    "[rbp%+d], eax\n",
                    off, off);
        }
        else
        {
            fprintf(ac->as,
                    "  mov rax, [rbp%+d]\n  add rax, 1\n  mov [rbp%+d], rax\n", off,
                    off);
        }
        break;
    }
    case ND_PREDEC:
    {
        if (!e->lhs || e->lhs->kind != ND_VAR)
        {
            cg_error(e, "codegen: -- expects variable");
            exit(1);
        }
        int idx = asm_find_local(ac, e->lhs->var_ref);
        if (idx < 0)
        {
            cg_error(e, "codegen: unknown local in predec");
            exit(1);
        }
        int off = ac->locals[idx].offset;
        Type *ty = ac->locals[idx].type;
        int k = ty ? ty->kind : TY_I64;
        if (k == TY_I8)
        {
            fprintf(ac->as,
                    "  mov al, byte ptr [rbp%+d]\n  sub al, 1\n  mov byte ptr "
                    "[rbp%+d], al\n  movsx eax, al\n",
                    off, off);
        }
        else if (k == TY_U8)
        {
            fprintf(ac->as,
                    "  mov al, byte ptr [rbp%+d]\n  sub al, 1\n  mov byte ptr "
                    "[rbp%+d], al\n  movzx eax, al\n",
                    off, off);
        }
        else if (k == TY_I16)
        {
            fprintf(ac->as,
                    "  mov ax, word ptr [rbp%+d]\n  sub ax, 1\n  mov word ptr "
                    "[rbp%+d], ax\n  movsx eax, ax\n",
                    off, off);
        }
        else if (k == TY_U16)
        {
            fprintf(ac->as,
                    "  mov ax, word ptr [rbp%+d]\n  sub ax, 1\n  mov word ptr "
                    "[rbp%+d], ax\n  movzx eax, ax\n",
                    off, off);
        }
        else if (k == TY_I32 || k == TY_U32)
        {
            fprintf(ac->as,
                    "  mov eax, dword ptr [rbp%+d]\n  sub eax, 1\n  mov dword ptr "
                    "[rbp%+d], eax\n",
                    off, off);
        }
        else
        {
            fprintf(ac->as,
                    "  mov rax, [rbp%+d]\n  sub rax, 1\n  mov [rbp%+d], rax\n", off,
                    off);
        }
        break;
    }
    case ND_POSTINC:
    {
        if (!e->lhs || e->lhs->kind != ND_VAR)
        {
            cg_error(e, "codegen: ++ expects variable");
            exit(1);
        }
        int idx = asm_find_local(ac, e->lhs->var_ref);
        if (idx < 0)
        {
            cg_error(e, "codegen: unknown local in postinc");
            exit(1);
        }
        int off = ac->locals[idx].offset;
        Type *ty = ac->locals[idx].type;
        int k = ty ? ty->kind : TY_I64;
        if (k == TY_I8)
        {
            fprintf(ac->as,
                    "  mov al, byte ptr [rbp%+d]\n  mov cl, al\n  add al, 1\n  mov "
                    "byte ptr [rbp%+d], al\n  movsx eax, cl\n",
                    off, off);
        }
        else if (k == TY_U8)
        {
            fprintf(ac->as,
                    "  mov al, byte ptr [rbp%+d]\n  mov cl, al\n  add al, 1\n  mov "
                    "byte ptr [rbp%+d], al\n  movzx eax, cl\n",
                    off, off);
        }
        else if (k == TY_I16)
        {
            fprintf(ac->as,
                    "  mov ax, word ptr [rbp%+d]\n  mov cx, ax\n  add ax, 1\n  mov "
                    "word ptr [rbp%+d], ax\n  movsx eax, cx\n",
                    off, off);
        }
        else if (k == TY_U16)
        {
            fprintf(ac->as,
                    "  mov ax, word ptr [rbp%+d]\n  mov cx, ax\n  add ax, 1\n  mov "
                    "word ptr [rbp%+d], ax\n  movzx eax, cx\n",
                    off, off);
        }
        else if (k == TY_I32 || k == TY_U32)
        {
            fprintf(ac->as,
                    "  mov ecx, dword ptr [rbp%+d]\n  mov eax, ecx\n  add ecx, 1\n  "
                    "mov dword ptr [rbp%+d], ecx\n",
                    off, off);
        }
        else
        {
            fprintf(ac->as,
                    "  mov rcx, [rbp%+d]\n  mov rax, rcx\n  add rcx, 1\n  mov "
                    "[rbp%+d], rcx\n",
                    off, off);
        }
        break;
    }
    case ND_POSTDEC:
    {
        if (!e->lhs || e->lhs->kind != ND_VAR)
        {
            cg_error(e, "codegen: -- expects variable");
            exit(1);
        }
        int idx = asm_find_local(ac, e->lhs->var_ref);
        if (idx < 0)
        {
            cg_error(e, "codegen: unknown local in postdec");
            exit(1);
        }
        int off = ac->locals[idx].offset;
        Type *ty = ac->locals[idx].type;
        int k = ty ? ty->kind : TY_I64;
        if (k == TY_I8)
        {
            fprintf(ac->as,
                    "  mov al, byte ptr [rbp%+d]\n  mov cl, al\n  sub al, 1\n  mov "
                    "byte ptr [rbp%+d], al\n  movsx eax, cl\n",
                    off, off);
        }
        else if (k == TY_U8)
        {
            fprintf(ac->as,
                    "  mov al, byte ptr [rbp%+d]\n  mov cl, al\n  sub al, 1\n  mov "
                    "byte ptr [rbp%+d], al\n  movzx eax, cl\n",
                    off, off);
        }
        else if (k == TY_I16)
        {
            fprintf(ac->as,
                    "  mov ax, word ptr [rbp%+d]\n  mov cx, ax\n  sub ax, 1\n  mov "
                    "word ptr [rbp%+d], ax\n  movsx eax, cx\n",
                    off, off);
        }
        else if (k == TY_U16)
        {
            fprintf(ac->as,
                    "  mov ax, word ptr [rbp%+d]\n  mov cx, ax\n  sub ax, 1\n  mov "
                    "word ptr [rbp%+d], ax\n  movzx eax, cx\n",
                    off, off);
        }
        else if (k == TY_I32 || k == TY_U32)
        {
            fprintf(ac->as,
                    "  mov ecx, dword ptr [rbp%+d]\n  mov eax, ecx\n  sub ecx, 1\n  "
                    "mov dword ptr [rbp%+d], ecx\n",
                    off, off);
        }
        else
        {
            fprintf(ac->as,
                    "  mov rcx, [rbp%+d]\n  mov rax, rcx\n  sub rcx, 1\n  mov "
                    "[rbp%+d], rcx\n",
                    off, off);
        }
        break;
    }
    case ND_STRING:
    {
        int id = asm_intern_string(ac, e->str_data, e->str_len);
        fprintf(ac->as, "  lea rax, [rip + .Lstr%d]\n", id);
        break;
    }
    case ND_CALL:
    {
        // OS-specific calling convention
        extern int __chance_emit_linux;
        int is_linux = __chance_emit_linux;
        int argc = e->arg_count;
        // Evaluate all args and push to stack to avoid nested call clobber
        for (int i = 0; i < argc; i++)
        {
            const Node *a = e->args[i];
            if (a->kind == ND_STRING)
            {
                int id = asm_intern_string(ac, a->str_data, a->str_len);
                fprintf(ac->as, "  lea rax, [rip + .Lstr%d]\n", id);
            }
            else
            {
                asm_eval_expr_to_rax(ac, a);
            }
            fprintf(ac->as, "  push rax\n");
        }
        // Pop into registers in reverse order to restore stack and set arg regs
        for (int i = argc - 1; i >= 0; i--)
        {
            fprintf(ac->as, "  pop rax\n");
            if (!is_linux)
            {
                if (i == 0)
                    fprintf(ac->as, "  mov rcx, rax\n");
                else if (i == 1)
                    fprintf(ac->as, "  mov rdx, rax\n");
                else if (i == 2)
                    fprintf(ac->as, "  mov r8, rax\n");
                else if (i == 3)
                    fprintf(ac->as, "  mov r9, rax\n");
                else
                { /* TODO: spill to stack for >4 */
                }
            }
            else
            {
                if (i == 0)
                    fprintf(ac->as, "  mov rdi, rax\n");
                else if (i == 1)
                    fprintf(ac->as, "  mov rsi, rax\n");
                else if (i == 2)
                    fprintf(ac->as, "  mov rdx, rax\n");
                else if (i == 3)
                    fprintf(ac->as, "  mov rcx, rax\n");
                else if (i == 4)
                    fprintf(ac->as, "  mov r8, rax\n");
                else if (i == 5)
                    fprintf(ac->as, "  mov r9, rax\n");
                else
                { /* TODO: stack-passed args (>6) */
                }
            }
        }
        // For varargs on both ABIs, ensure AL/EAX indicates no vector regs used
        fprintf(ac->as, "  xor eax, eax\n");
        const char *callee = e->call_name ? e->call_name : "unknown";
        fprintf(ac->as, "  call %s\n", callee);
        break;
    }
    case ND_ASSIGN:
    {
        // Evaluate assignment as an expression: perform the store and leave the
        // assigned value in rax
        if (e->lhs->kind == ND_VAR)
        {
            int idx = asm_find_local(ac, e->lhs->var_ref);
            if (idx < 0)
            {
                diag_error("codegen: unknown local in assign expr");
                exit(1);
            }
            int off = ac->locals[idx].offset;
            Type *ty = ac->locals[idx].type;
            int k = ty ? ty->kind : TY_I64;
            asm_eval_expr_to_rax(ac, e->rhs);
            if (k == TY_I8 || k == TY_U8)
                fprintf(ac->as, "  mov byte ptr [rbp%+d], al\n", off);
            else if (k == TY_I16 || k == TY_U16)
                fprintf(ac->as, "  mov word ptr [rbp%+d], ax\n", off);
            else if (k == TY_I32 || k == TY_U32)
                fprintf(ac->as, "  mov dword ptr [rbp%+d], eax\n", off);
            else
                fprintf(ac->as, "  mov [rbp%+d], rax\n", off);
        }
        else if (e->lhs->kind == ND_INDEX ||
                 (e->lhs->kind == ND_CAST && e->lhs->lhs->kind == ND_INDEX))
        {
            const Node *ix = (e->lhs->kind == ND_INDEX) ? e->lhs : e->lhs->lhs;
            asm_addr_for_lvalue_to_rax(ac, ix);
            fprintf(ac->as, "  mov rdx, rax\n");
            asm_eval_expr_to_rax(ac, e->rhs);
            // Determine element type for width-correct store
            int k = TY_U8;
            const Node *b = ix->lhs;
            Type *bt = NULL;
            if (b->kind == ND_VAR)
            {
                int idx = asm_find_local(ac, b->var_ref);
                if (idx >= 0)
                    bt = ac->locals[idx].type;
            }
            else if (b->kind == ND_CAST)
            {
                bt = b->type;
                if (!bt && b->lhs && b->lhs->kind == ND_VAR)
                {
                    int idx = asm_find_local(ac, b->lhs->var_ref);
                    if (idx >= 0)
                        bt = ac->locals[idx].type;
                }
            }
            if (bt && bt->kind == TY_PTR && bt->pointee)
                k = bt->pointee->kind;
            if (k == TY_I8 || k == TY_U8)
                fprintf(ac->as, "  mov byte ptr [rdx], al\n");
            else if (k == TY_I16 || k == TY_U16)
                fprintf(ac->as, "  mov word ptr [rdx], ax\n");
            else if (k == TY_I32 || k == TY_U32)
                fprintf(ac->as, "  mov dword ptr [rdx], eax\n");
            else
                fprintf(ac->as, "  mov [rdx], rax\n");
        }
        else
        {
            diag_error("codegen: unsupported assign expr kind");
            exit(1);
        }
        break;
    }

    default:
        cg_error(e, "codegen: unsupported expr kind %d", e->kind);
        exit(1);
    }
}

static void asm_emit_initializer(AsmCtx *ac, const Node *init, Type *ty, int base_off)
{
    if (!ty)
        return;
    if (!init)
    {
        asm_zero_bytes(ac, base_off, type_storage_size(ty));
        return;
    }
    if (init->kind != ND_INIT_LIST)
    {
        asm_eval_expr_to_rax(ac, init);
        if (ty->kind == TY_STRUCT)
        {
            cg_error(init, "codegen: struct assignment from expression not supported");
            exit(1);
        }
        asm_store_scalar_to_stack(ac, ty, base_off);
        return;
    }
    if (ty->kind == TY_STRUCT)
    {
        int size = ty->strct.size_bytes;
        asm_zero_bytes(ac, base_off, size);
        if (init->init.is_zero)
            return;
        int pos_cursor = 0;
        for (int i = 0; i < init->init.count; i++)
        {
            int field_index = -1;
            if (init->init.field_indices)
                field_index = init->init.field_indices[i];
            else if (init->init.designators && init->init.designators[i])
                field_index = asm_struct_find_field(ty, init->init.designators[i]);
            else
                field_index = pos_cursor++;
            if (field_index < 0 || field_index >= ty->strct.field_count)
                continue;
            int field_off = ty->strct.field_offsets ? ty->strct.field_offsets[field_index] : 0;
            Type *ft = ty->strct.field_types ? ty->strct.field_types[field_index] : NULL;
            const Node *elem = init->init.elems ? init->init.elems[i] : NULL;
            if (!elem || !ft)
                continue;
            if (elem->kind == ND_INIT_LIST)
            {
                asm_emit_initializer(ac, elem, ft, base_off + field_off);
            }
            else
            {
                asm_eval_expr_to_rax(ac, elem);
                if (ft->kind == TY_STRUCT)
                {
                    cg_error(elem, "codegen: struct expressions not yet supported in field init");
                    exit(1);
                }
                asm_store_scalar_to_stack(ac, ft, base_off + field_off);
            }
        }
        return;
    }
    // Scalar brace initializer like {0}
    asm_zero_bytes(ac, base_off, type_storage_size(ty));
}

static void asm_emit_stmt(AsmCtx *ac, const Node *s)
{
    // debug disabled
    switch (s->kind)
    {
    case ND_VAR_DECL:
    {
        int size = s->var_type ? type_storage_size(s->var_type) : 8;
        if (size <= 0)
            size = 1;
        int off = asm_add_local(ac, s->var_name, s->var_type, size);
        if (s->rhs)
        {
            if (s->rhs->kind == ND_INIT_LIST)
                asm_emit_initializer(ac, s->rhs, s->var_type, off);
            else
            {
                asm_eval_expr_to_rax(ac, s->rhs);
                asm_store_scalar_to_stack(ac, s->var_type, off);
            }
        }
        break;
    }
    case ND_EXPR_STMT:
    {
        if (s->lhs && s->lhs->kind == ND_ASSIGN)
        {
            const Node *as = s->lhs;
            if (as->lhs->kind == ND_VAR)
            {
                int idx = asm_find_local(ac, as->lhs->var_ref);
                if (idx < 0)
                {
                    cg_error(s, "codegen: unknown local in assign");
                    exit(1);
                }
                int off = ac->locals[idx].offset;
                Type *ty = ac->locals[idx].type;
                int k = ty ? ty->kind : TY_I64;
                asm_eval_expr_to_rax(ac, as->rhs);
                if (k == TY_I8 || k == TY_U8)
                    fprintf(ac->as, "  mov byte ptr [rbp%+d], al\n", off);
                else if (k == TY_I16 || k == TY_U16)
                    fprintf(ac->as, "  mov word ptr [rbp%+d], ax\n", off);
                else if (k == TY_I32 || k == TY_U32)
                    fprintf(ac->as, "  mov dword ptr [rbp%+d], eax\n", off);
                else
                    fprintf(ac->as, "  mov [rbp%+d], rax\n", off);
            }
            else if (as->lhs->kind == ND_INDEX ||
                     (as->lhs->kind == ND_CAST && as->lhs->lhs->kind == ND_INDEX))
            {
                const Node *ix = (as->lhs->kind == ND_INDEX) ? as->lhs : as->lhs->lhs;
                // Compute address of lvalue and preserve it in rdx (since RHS eval
                // clobbers rax)
                asm_addr_for_lvalue_to_rax(ac, ix);
                fprintf(ac->as, "  mov rdx, rax\n");
                asm_eval_expr_to_rax(ac, as->rhs);
                // Width-correct store based on pointer element type
                int k = TY_U8;
                const Node *b = ix->lhs;
                Type *bt = NULL;
                if (b->kind == ND_VAR)
                {
                    int idx = asm_find_local(ac, b->var_ref);
                    if (idx >= 0)
                        bt = ac->locals[idx].type;
                }
                else if (b->kind == ND_CAST)
                {
                    bt = b->type;
                    if (!bt && b->lhs && b->lhs->kind == ND_VAR)
                    {
                        int idx = asm_find_local(ac, b->lhs->var_ref);
                        if (idx >= 0)
                            bt = ac->locals[idx].type;
                    }
                }
                if (bt && bt->kind == TY_PTR && bt->pointee)
                    k = bt->pointee->kind;
                if (k == TY_I8 || k == TY_U8)
                    fprintf(ac->as, "  mov byte ptr [rdx], al\n");
                else if (k == TY_I16 || k == TY_U16)
                    fprintf(ac->as, "  mov word ptr [rdx], ax\n");
                else if (k == TY_I32 || k == TY_U32)
                    fprintf(ac->as, "  mov dword ptr [rdx], eax\n");
                else
                    fprintf(ac->as, "  mov [rdx], rax\n");
            }
            else if (as->lhs->kind == ND_MEMBER ||
                     (as->lhs->kind == ND_CAST && as->lhs->lhs->kind == ND_MEMBER))
            {
                const Node *mem = (as->lhs->kind == ND_MEMBER) ? as->lhs : as->lhs->lhs;
                asm_addr_for_lvalue_to_rax(ac, mem);
                fprintf(ac->as, "  mov rdx, rax\n");
                asm_eval_expr_to_rax(ac, as->rhs);
                if (mem->type && mem->type->kind == TY_STRUCT)
                {
                    cg_error(mem, "codegen: struct member assignment from expression not supported");
                    exit(1);
                }
                asm_store_scalar_to_ptr(ac, mem->type, "rdx");
            }
            else
            {
                cg_error(s, "codegen: unsupported assign expr");
                exit(1);
            }
        }
        else
        {
            asm_eval_expr_to_rax(ac, s->lhs);
        }
        break;
    }
    case ND_ASSIGN:
    {
        if (s->lhs->kind == ND_VAR)
        {
            int idx = asm_find_local(ac, s->lhs->var_ref);
            if (idx < 0)
            {
                cg_error(s, "codegen: unknown local in assign");
                exit(1);
            }
            int off = ac->locals[idx].offset;
            Type *ty = ac->locals[idx].type;
            int k = ty ? ty->kind : TY_I64;
            asm_eval_expr_to_rax(ac, s->rhs);
            if (k == TY_I8 || k == TY_U8)
                fprintf(ac->as, "  mov byte ptr [rbp%+d], al\n", off);
            else if (k == TY_I16 || k == TY_U16)
                fprintf(ac->as, "  mov word ptr [rbp%+d], ax\n", off);
            else if (k == TY_I32 || k == TY_U32)
                fprintf(ac->as, "  mov dword ptr [rbp%+d], eax\n", off);
            else
                fprintf(ac->as, "  mov [rbp%+d], rax\n", off);
        }
        else if (s->lhs->kind == ND_INDEX ||
                 (s->lhs->kind == ND_CAST && s->lhs->lhs->kind == ND_INDEX))
        {
            const Node *ix = (s->lhs->kind == ND_INDEX) ? s->lhs : s->lhs->lhs;
            // Compute address of lvalue and preserve it in rdx (since RHS eval
            // clobbers rax)
            asm_addr_for_lvalue_to_rax(ac, ix);
            fprintf(ac->as, "  mov rdx, rax\n");
            // store byte of rhs to [rdx]
            asm_eval_expr_to_rax(ac, s->rhs);
            // Width-correct store based on pointer element type
            int k = TY_U8;
            const Node *b = ix->lhs;
            Type *bt = NULL;
            if (b->kind == ND_VAR)
            {
                int idx = asm_find_local(ac, b->var_ref);
                if (idx >= 0)
                    bt = ac->locals[idx].type;
            }
            else if (b->kind == ND_CAST)
            {
                bt = b->type;
                if (!bt && b->lhs && b->lhs->kind == ND_VAR)
                {
                    int idx = asm_find_local(ac, b->lhs->var_ref);
                    if (idx >= 0)
                        bt = ac->locals[idx].type;
                }
            }
            if (bt && bt->kind == TY_PTR && bt->pointee)
                k = bt->pointee->kind;
            if (k == TY_I8 || k == TY_U8)
                fprintf(ac->as, "  mov byte ptr [rdx], al\n");
            else if (k == TY_I16 || k == TY_U16)
                fprintf(ac->as, "  mov word ptr [rdx], ax\n");
            else if (k == TY_I32 || k == TY_U32)
                fprintf(ac->as, "  mov dword ptr [rdx], eax\n");
            else
                fprintf(ac->as, "  mov [rdx], rax\n");
        }
        else if (s->lhs->kind == ND_MEMBER ||
                 (s->lhs->kind == ND_CAST && s->lhs->lhs->kind == ND_MEMBER))
        {
            const Node *mem = (s->lhs->kind == ND_MEMBER) ? s->lhs : s->lhs->lhs;
            asm_addr_for_lvalue_to_rax(ac, mem);
            fprintf(ac->as, "  mov rdx, rax\n");
            asm_eval_expr_to_rax(ac, s->rhs);
            if (mem->type && mem->type->kind == TY_STRUCT)
            {
                cg_error(mem, "codegen: struct member assignment from expression not supported");
                exit(1);
            }
            asm_store_scalar_to_ptr(ac, mem->type, "rdx");
        }
        else
        {
            cg_error(s, "codegen: unsupported assign");
            exit(1);
        }
        break;
    }
    case ND_WHILE:
    {
        int lid = ac->next_lbl++;
        fprintf(ac->as, ".Lwhile%d:\n", lid);
        asm_eval_expr_to_rax(ac, s->lhs);
        fprintf(ac->as, "  cmp eax, 0\n  je .Lendw%d\n", lid);
        if (s->rhs)
            asm_emit_stmt(ac, s->rhs);
        fprintf(ac->as, "  jmp .Lwhile%d\n.Lendw%d:\n", lid, lid);
        break;
    }
    case ND_IF:
    {
        int lid = ac->next_lbl++;
        asm_eval_expr_to_rax(ac, s->lhs); // cond -> eax
        fprintf(ac->as, "  cmp eax, 0\n  je .Lelse%d\n", lid);
        asm_emit_stmt(ac, s->rhs); // then
        fprintf(ac->as, "  jmp .Lend%d\n.Lelse%d:\n", lid, lid);
        if (s->body)
            asm_emit_stmt(ac, s->body);
        fprintf(ac->as, ".Lend%d:\n", lid);
        break;
    }
    case ND_BLOCK:
        for (int i = 0; i < s->stmt_count; i++)
            asm_emit_stmt(ac, s->stmts[i]);
        break;
    case ND_RET:
        if (s->lhs)
        {
            asm_eval_expr_to_rax(ac, s->lhs);
            // Conform return value to declared function return type
            if (ac->fn && ac->fn->ret_type)
                asm_trunc_to_kind(ac->as, ac->fn->ret_type->kind);
        }
        fprintf(ac->as, "  jmp .Lret%d\n", ac->ret_id);
        break;
    default:
        cg_error(s, "codegen: unsupported stmt kind %d", s->kind);
        exit(1);
    }
}

int __chance_emit_linux = 0;

static int codegen_asm_backend_link(const Node *unit,
                                    const CodegenOptions *opts)
{
    if (!unit || unit->kind != ND_FUNC)
    {
        diag_error("codegen: expected function");
        return 1;
    }
    // Write temp .S next to output
    char asmpath[512];
    const char *out = opts && opts->output_path ? opts->output_path : "a.exe";
    form_asm_path(asmpath, sizeof(asmpath), out);
    FILE *as = fopen(asmpath, "wb");
    if (!as)
    {
        perror("fopen asm");
        return 1;
    }
    static int global_ret_seq = 1;
    AsmCtx ac = {0};
    ac.fn = unit;
    ac.as = as;
    ac.next_lbl = 1;
    ac.local_cnt = 0;
    ac.str_cnt = 0;
    ac.stack_size = 0;
    ac.ret_id = global_ret_seq++;
    // Prologue
    const char *fn_name = unit->name ? unit->name : "main";
    int is_linux = (opts && opts->os == OS_LINUX);
    __chance_emit_linux = is_linux;
    fprintf(as, ".intel_syntax noprefix\n.section .text\n.globl %s\n%s:\n",
            fn_name, fn_name);
    if (is_linux)
    {
        // System V: 16-byte alignment before call; we'll align per locals, no
        // shadow space requirement
        fprintf(as, "  push rbp\n  mov rbp, rsp\n");
    }
    else
    {
        // Windows x64
        fprintf(as, "  push rbp\n  mov rbp, rsp\n");
    }
    // Reserve 32-byte shadow space + locals (rounded up to 16)
    // We'll grow ac.stack_size as we see locals, then patch after emit? Simpler:
    // pre-scan for decls sizes would be ideal, but we can conservatively sub 96
    // and not reuse; but better: emit after locals known: trick: record position
    long stack_adj_pos = ftell(as);
    if (is_linux)
        fprintf(as, "  sub rsp, %8d\n", 16); // placeholder; will patch
    else
        fprintf(as, "  sub rsp, %8d\n", 32); // placeholder; will patch
    // Map parameters to stack locals by saving incoming registers
    // Determine parameter passing order by OS
    int pc = unit->param_count;
    // Assign stack slots for parameters first so they have stable offsets
    for (int pi = 0; pi < pc; ++pi)
    {
        int k = (unit->param_types && unit->param_types[pi])
                    ? unit->param_types[pi]->kind
                    : TY_I64;
        int size = kind_width(k);
        int off =
            asm_add_local(&ac, unit->param_names ? unit->param_names[pi] : "arg",
                          unit->param_types ? unit->param_types[pi] : NULL, size);
        (void)off;
    }
    // Now store incoming registers into those stack slots (in declared order)
    for (int pi = 0; pi < pc && pi < 64; ++pi)
    {
        int off = ac.locals[pi].offset;
        int k = ac.locals[pi].type ? ac.locals[pi].type->kind : TY_I64;
        asm_store_param_width(as, is_linux, pi, off, k);
    }
    // Emit body
    if (unit->body->kind == ND_BLOCK)
    {
        for (int i = 0; i < unit->body->stmt_count; i++)
            asm_emit_stmt(&ac, unit->body->stmts[i]);
    }
    else
        asm_emit_stmt(&ac, unit->body);
    fprintf(as, ".Lret%d:\n  mov rsp, rbp\n  pop rbp\n  ret\n", ac.ret_id);
    // Strings
    asm_emit_str_section(&ac);
    // Patch stack size: adjust per OS
    int locals_rounded = (ac.stack_size + 15) & ~15;
    int total_sub;
    if (is_linux)
    {
        // On entry after push rbp, rsp%16==8. We want (sub amount) so that before a
        // call we have alignment.
        total_sub = locals_rounded + 8; // compensate push rbp
    }
    else
    {
        total_sub = 32 + locals_rounded + 8; // shadow + locals + alignment fix
    }
    fflush(as);
    fseek(as, stack_adj_pos, SEEK_SET);
    fprintf(as, "  sub rsp, %8d\n", total_sub);
    fclose(as);
    // Assemble only or assemble+link
    if (opts && opts->emit_asm)
    {
        return 0; // user requested -S only
    }
    if (opts && opts->no_link)
    {
        char objpath[512];
        if (opts->obj_output_path && *opts->obj_output_path)
        {
            snprintf(objpath, sizeof(objpath), "%s", opts->obj_output_path);
        }
        else
        {
            form_obj_path(objpath, sizeof(objpath), out, is_linux);
        }
        char cmd[1024];
        snprintf(cmd, sizeof(cmd), "cc -c -o \"%s\" \"%s\"", objpath, asmpath);
        int rc = system(cmd);
        if (rc != 0)
        {
            fprintf(stderr, "assemble failed (rc=%d): %s\n", rc, cmd);
            return 1;
        }
        return 0;
    }
    {
        char cmd[1024];
        snprintf(cmd, sizeof(cmd), "cc -o \"%s\" \"%s\"", out, asmpath);
        int rc = system(cmd);
        if (rc != 0)
        {
            fprintf(stderr, "assemble/link failed (rc=%d): %s\n", rc, cmd);
            return 1;
        }
        return 0;
    }
}

// Emit all functions in a unit into one .S so that calls resolve within the
// same translation unit
static int codegen_asm_backend_link_all(const Node *u,
                                        const CodegenOptions *opts)
{
    if (!u || u->kind != ND_UNIT)
    {
        diag_error("codegen: expected unit for multi-func emit");
        return 1;
    }
    char asmpath[512];
    const char *out = opts && opts->output_path ? opts->output_path : "a.exe";
    form_asm_path(asmpath, sizeof(asmpath), out);
    FILE *as = fopen(asmpath, "wb");
    if (!as)
    {
        perror("fopen asm");
        return 1;
    }
    static int global_ret_seq_all = 1;
    AsmCtx ac = (AsmCtx){0};
    ac.as = as;
    ac.next_lbl = 1;
    ac.local_cnt = 0;
    ac.str_cnt = 0;
    ac.stack_size = 0;
    int is_linux = (opts && opts->os == OS_LINUX);
    __chance_emit_linux = is_linux;
    fprintf(as, ".intel_syntax noprefix\n.section .text\n");
    for (int i = 0; i < u->stmt_count; i++)
    {
        const Node *fn = u->stmts[i];
        if (!fn || fn->kind != ND_FUNC)
            continue;
        ac.fn = fn;
        ac.local_cnt = 0;
        ac.stack_size = 0;
        ac.ret_id =
            global_ret_seq_all++; // reset per function with unique ret label
        const char *fn_name = fn->name ? fn->name : "fn";
        fprintf(as, ".globl %s\n%s:\n  push rbp\n  mov rbp, rsp\n", fn_name,
                fn_name);
        long stack_adj_pos = ftell(as);
        fprintf(as, "  sub rsp, %8d\n", is_linux ? 16 : 32);
        // Save parameters to stack locals
        int pc = fn->param_count;
        for (int pi = 0; pi < pc; ++pi)
        {
            int k = (fn->param_types && fn->param_types[pi])
                        ? fn->param_types[pi]->kind
                        : TY_I64;
            int size = kind_width(k);
            int off =
                asm_add_local(&ac, fn->param_names ? fn->param_names[pi] : "arg",
                              fn->param_types ? fn->param_types[pi] : NULL, size);
            (void)off;
        }
        for (int pi = 0; pi < pc && pi < 64; ++pi)
        {
            int off = ac.locals[pi].offset;
            int k = ac.locals[pi].type ? ac.locals[pi].type->kind : TY_I64;
            asm_store_param_width(as, is_linux, pi, off, k);
        }
        if (fn->body->kind == ND_BLOCK)
        {
            for (int k = 0; k < fn->body->stmt_count; k++)
                asm_emit_stmt(&ac, fn->body->stmts[k]);
        }
        else
            asm_emit_stmt(&ac, fn->body);
        fprintf(as, ".Lret%d:\n  mov rsp, rbp\n  pop rbp\n  ret\n", ac.ret_id);
        int locals_rounded = (ac.stack_size + 15) & ~15;
        int total_sub = is_linux ? (locals_rounded + 8) : (32 + locals_rounded + 8);
        fflush(as);
        fseek(as, stack_adj_pos, SEEK_SET);
        fprintf(as, "  sub rsp, %8d\n", total_sub);
        fseek(as, 0, SEEK_END);
    }
    asm_emit_str_section(&ac);
    fclose(as);
    if (opts && opts->emit_asm)
        return 0;
    if (opts && opts->no_link)
    {
        char objpath[512];
        if (opts->obj_output_path && *opts->obj_output_path)
        {
            snprintf(objpath, sizeof(objpath), "%s", opts->obj_output_path);
        }
        else
        {
            form_obj_path(objpath, sizeof(objpath), out, is_linux);
        }
        char cmd[1024];
        snprintf(cmd, sizeof(cmd), "cc -c -o \"%s\" \"%s\"", objpath, asmpath);
        int rc = system(cmd);
        if (rc != 0)
        {
            fprintf(stderr, "assemble failed (rc=%d): %s\n", rc, cmd);
            return 1;
        }
        return 0;
    }
    {
        char cmd[1024];
        snprintf(cmd, sizeof(cmd), "cc -o \"%s\" \"%s\"", out, asmpath);
        int rc = system(cmd);
        if (rc != 0)
        {
            fprintf(stderr, "assemble/link failed (rc=%d): %s\n", rc, cmd);
            return 1;
        }
        return 0;
    }
}

// Emit COFF x64 object and link via cc to produce an .exe
int codegen_pe_x64_write_exe_const(int32_t retval, const CodegenOptions *opts)
{
    // PE with two sections: .text and .idata importing ExitProcess, call
    // ExitProcess(retval) Build .text B9 imm32                    mov ecx, imm32
    // 48 8B 05 xx xx xx xx        mov rax, [rip+disp32]   ; &IAT.ExitProcess
    // FF D0                       call rax
    // EB FE                       jmp $                   ; should not return
    uint8_t text[16];
    size_t off = 0;
    text[off++] = 0xB9;
    w32(text, off, (uint32_t)retval);
    off += 4; // mov ecx, imm32
    text[off++] = 0x48;
    text[off++] = 0x8B;
    text[off++] = 0x05; // mov rax, [rip+disp32]
    size_t disp_pos = off;
    w32(text, off, 0);
    off += 4;
    text[off++] = 0xFF;
    text[off++] = 0xD0; // call rax
    text[off++] = 0xEB;
    text[off++] = 0xFE; // jmp $
    uint32_t text_size = (uint32_t)off;

    // Build .idata content
    // Layout: [ILT (8*2)] [IAT (8*2)] [HintName] [DLLName] [ImportDesc(20*2)]
    // We'll compute addresses after placing.
    uint8_t idata[0x200];
    memset(idata, 0, sizeof(idata));
    uint32_t id_off = 0;
    uint32_t ilt_rva, iat_rva, hint_rva, dll_rva, desc_rva;
    // ILT (two entries: one ptr, one zero terminator)
    ilt_rva = 0; // defer, real RVA later
    uint32_t ilt_off = id_off;
    w64(idata, id_off, 0);
    id_off += 8;
    w64(idata, id_off, 0);
    id_off += 8;
    // IAT
    iat_rva = 0;
    uint32_t iat_off = id_off;
    w64(idata, id_off, 0);
    id_off += 8;
    w64(idata, id_off, 0);
    id_off += 8;
    // Hint/Name for ExitProcess
    hint_rva = 0;
    uint32_t hint_off = id_off;
    w16(idata, id_off, 0);
    id_off += 2;
    const char *fn = "ExitProcess";
    size_t fnl = strlen(fn) + 1;
    memcpy(idata + id_off, fn, fnl);
    id_off += (uint32_t)fnl;
    id_off = (id_off + 1) & ~1u; // align 2
    // DLL name
    dll_rva = 0;
    uint32_t dll_off = id_off;
    const char *dn = "KERNEL32.dll";
    size_t dnl = strlen(dn) + 1;
    memcpy(idata + id_off, dn, dnl);
    id_off += (uint32_t)dnl;
    id_off = (id_off + 3) & ~3u; // align 4
    // Import Descriptor (20 bytes) + null desc
    desc_rva = 0;
    uint32_t desc_off = id_off;
    memset(idata + id_off, 0, 40);
    id_off += 40;

    uint32_t idata_size = (id_off + 0x1FF) & ~0x1FFu; // pad to 0x200

    // PE headers
    const uint32_t file_align = 0x200, sect_align = 0x1000;
    const uint32_t text_va = 0x1000, idata_va = 0x2000;
    const uint32_t text_raw = 0x200, idata_raw = 0x400;
    const uint64_t image_base = 0x0000000140000000ULL;

    // Fill ILT/IAT RVAs now that we know section RVAs
    uint32_t ilt_rva_real = idata_va + ilt_off;
    uint32_t iat_rva_real = idata_va + iat_off;
    uint32_t hint_rva_real = idata_va + hint_off;
    uint32_t dll_rva_real = idata_va + dll_off;
    // Write ILT entry: RVA of Hint/Name with high bit 0 (ordinal flag not set)
    w64(idata, ilt_off + 0, (uint64_t)hint_rva_real);
    // IAT initially same as ILT
    w64(idata, iat_off + 0, (uint64_t)hint_rva_real);
    // Import Descriptor
    // struct { DWORD OriginalFirstThunk; DWORD TimeDateStamp; DWORD
    // ForwarderChain; DWORD Name; DWORD FirstThunk; }
    w32(idata, desc_off + 0, ilt_rva_real);
    w32(idata, desc_off + 12, dll_rva_real);
    w32(idata, desc_off + 16, iat_rva_real);

    // Build DOS+PE with 2 sections
    uint8_t dos[0x80];
    memset(dos, 0, sizeof(dos));
    dos[0] = 'M';
    dos[1] = 'Z';
    w32(dos, 0x3C, 0x80);
    uint8_t pe[0x200];
    memset(pe, 0, sizeof(pe));
    pe[0] = 'P';
    pe[1] = 'E';
    // COFF
    w16(pe, 4, 0x8664);
    w16(pe, 6, 2);
    w32(pe, 8, 0);
    w32(pe, 12, 0);
    w32(pe, 16, 0);
    w16(pe, 20, 0x00F0);
    w16(pe, 22, 0x2022);
    // Optional
    size_t opt = 24;
    w16(pe, opt + 0, 0x20B);
    w32(pe, opt + 4, (text_size + 0x1FF) & ~0x1FFu);
    w32(pe, opt + 8, idata_size);
    w32(pe, opt + 12, 0);
    w32(pe, opt + 16, text_va);
    w32(pe, opt + 20, text_va);
    w64(pe, opt + 24, image_base);
    w32(pe, opt + 32, sect_align);
    w32(pe, opt + 36, file_align);
    w16(pe, opt + 40, 6);
    w16(pe, opt + 42, 0);
    w16(pe, opt + 44, 0);
    w16(pe, opt + 46, 0);
    w16(pe, opt + 48, 6);
    w16(pe, opt + 50, 0);
    w32(pe, opt + 52, 0);
    uint32_t size_of_headers = 0x200;
    uint32_t size_of_image = 0x3000; // headers + two 0x1000 sections
    w32(pe, opt + 56, size_of_image);
    w32(pe, opt + 60, size_of_headers);
    w32(pe, opt + 64, 0);
    w16(pe, opt + 68, 3);
    w16(pe, opt + 70, 0);
    w64(pe, opt + 72, 0x00100000);
    w64(pe, opt + 80, 0x00001000);
    w64(pe, opt + 88, 0x00100000);
    w64(pe, opt + 96, 0x00001000);
    w32(pe, opt + 104, 0);
    w32(pe, opt + 108, 16);
    // DataDirectory[1] = imports
    w32(pe, opt + 112 + 1 * 8 + 0, idata_va + desc_off); // RVA
    w32(pe, opt + 112 + 1 * 8 + 4, 40);                  // Size (two descriptors total region)
    // Section headers start after COFF(20)+OPT(240)=264 -> offset 24+240=264
    size_t sh = 24 + 240;
    // .text
    memcpy(pe + sh + 0, ".text", 5);
    w32(pe, sh + 8, text_size);
    w32(pe, sh + 12, text_va);
    uint32_t text_raw_size = (text_size + 0x1FF) & ~0x1FFu;
    w32(pe, sh + 16, text_raw_size);
    w32(pe, sh + 20, text_raw);
    w32(pe, sh + 24, 0);
    w32(pe, sh + 28, 0);
    w16(pe, sh + 32, 0);
    w16(pe, sh + 34, 0);
    w32(pe, sh + 36, 0x60000020);
    // .idata
    size_t sh2 = sh + 40;
    memcpy(pe + sh2 + 0, ".idata", 6);
    w32(pe, sh2 + 8, idata_size);
    w32(pe, sh2 + 12, idata_va);
    w32(pe, sh2 + 16, idata_size);
    w32(pe, sh2 + 20, idata_raw);
    w32(pe, sh2 + 24, 0);
    w32(pe, sh2 + 28, 0);
    w16(pe, sh2 + 32, 0);
    w16(pe, sh2 + 34, 0);
    w32(pe, sh2 + 36, 0x40000040); // initialized data, read

    // Compute disp32 for RIP-relative load of IAT entry
    // next_instr RVA = text_va + (offset up to after disp32) = text_va + (2+4 + 3
    // + 4) = text_va + 13
    uint32_t next_rva = text_va + (uint32_t)(2 + 4 + 3 + 4);
    uint64_t next_va = image_base + next_rva;
    uint64_t iat_va = image_base + iat_rva_real;
    int32_t disp32 = (int32_t)(iat_va - next_va);
    w32(text, disp_pos, (uint32_t)disp32);

    // Write file
    size_t total = 0x200 + text_raw_size + idata_size;
    uint8_t *buf = (uint8_t *)xcalloc(1, total);
    memcpy(buf, dos, sizeof(dos));
    memcpy(buf + 0x80, pe, sizeof(pe));
    memcpy(buf + text_raw, text, text_size);
    memcpy(buf + idata_raw, idata, idata_size);

    const char *out = (opts && opts->output_path) ? opts->output_path : "a.exe";
    FILE *f = fopen(out, "wb");
    if (!f)
    {
        perror("fopen");
        free(buf);
        return 1;
    }
    fwrite(buf, 1, total, f);
    fclose(f);
    free(buf);
    if (opts && opts->emit_asm)
    {
        char dump[512];
        snprintf(dump, sizeof(dump), "%s.S", out);
        FILE *df = fopen(dump, "wb");
        if (df)
        {
            // Provide a minimal, assemble-able listing for the entry sequence
            fprintf(
                df,
                ".intel_syntax noprefix\n"
                ".section .text\n"
                ".globl main\n"
                "main:\n"
                "  mov ecx, %d\n"
                "  ; IAT setup omitted in -S listing\n"
                "  ; call ExitProcess via import thunk at runtime\n"
                "  ; (this .S is informational; link path still uses generated PE)\n",
                retval);
            fclose(df);
        }
    }
    return 0;
}

// Emit COFF x64 object and link via cc to produce an .exe
static int emit_function_as_asm_and_link(const Node *fn,
                                         const CodegenOptions *opts)
{
    return codegen_asm_backend_link(fn, opts);
}

int codegen_coff_x64_write_exe(const Node *unit, const CodegenOptions *opts)
{
    if (!unit)
    {
        fprintf(stderr, "codegen: null unit\n");
        return 1;
    }
    if (unit->kind == ND_FUNC && unit->body)
    {
        // old path

        // If expression is a printf call, emit a COFF object that calls external
        // printf.
        int64_t val = 0;
        const Node *expr = (unit->body->kind == ND_RET) ? unit->body->lhs : NULL;
        int is_simple_call = expr &&
                             (expr->kind == ND_CALL && expr->arg_count >= 1 &&
                              expr->args[0]->kind == ND_STRING) &&
                             (unit->body->kind == ND_RET);
        if (is_simple_call)
        {
            // In freestanding mode we cannot link against libc when producing
            // an executable, but compile-only (-c/--no-link) should still be
            // permitted so users can link against their own runtime later.
            if (opts && opts->freestanding && !(opts->no_link))
            {
                fprintf(stderr, "error: codegen: external calls with libc are not "
                                "available in freestanding mode\n");
                return 1;
            }
        }
        else
        {
            // For anything non-trivial, use the ASM backend
            return codegen_asm_backend_link(unit, opts);
        }
        // Otherwise, produce a COFF object with a constant return and link
        // For freestanding: still link object, but without CRT and with main as
        // entry

        // Code: mov eax, imm32; ret
        uint8_t code[6];
        code[0] = 0xB8; // mov eax, imm32
        w32(code, 1, (uint32_t)(int32_t)val);
        code[5] = 0xC3; // ret

        // Build COFF object in memory
        const uint32_t file_header_size = 20;
        const uint32_t section_header_size = 40;
        uint32_t num_sections = is_simple_call ? 2 : 1;
        uint32_t text_off = file_header_size + num_sections * section_header_size;
        uint8_t call_code[96];
        uint32_t call_size = 0;
        uint32_t reloc_off = 0;
        uint32_t reloc_count = 0;
        uint32_t rdata_off = 0;
        uint32_t rdata_size = 0;
        const char *fmt = NULL;
        int fmt_len = 0;
        int has_int_arg = 0;
        int int_arg_val = 0;
        int has_str2 = 0;
        const char *str2 = NULL;
        int str2_len = 0;
        size_t lea1_disp_pos = 0;
        size_t lea2_disp_pos = 0;
        size_t call_disp_pos = 0;
        if (is_simple_call)
        {
            // Assemble x64 Windows calling convention sequence: RCX=format, RDX=arg1;
            // align shadow space sub rsp, 40; lea rcx, [rip+disp32]; (either lea rdx,
            // [rip+disp32] or mov edx, imm32); xor eax,eax; call rel32; add rsp,40;
            // ret
            size_t o = 0;
            call_code[o++] = 0x48;
            call_code[o++] = 0x83;
            call_code[o++] = 0xEC;
            call_code[o++] = 0x28;
            call_code[o++] = 0x48;
            call_code[o++] = 0x8D;
            call_code[o++] = 0x0D;
            lea1_disp_pos = o;
            w32(call_code, o, 0);
            o += 4;
            if (expr->arg_count >= 2)
            {
                if (expr->args[1]->kind == ND_INT)
                {
                    has_int_arg = 1;
                    int_arg_val = (int)expr->args[1]->int_val;
                    call_code[o++] = 0xBA;
                    w32(call_code, o, (uint32_t)int_arg_val);
                    o += 4;
                }
                else if (expr->args[1]->kind == ND_STRING)
                {
                    has_str2 = 1;
                    str2 = expr->args[1]->str_data;
                    str2_len = expr->args[1]->str_len;
                    if (str2_len < 0)
                        str2_len = 0;
                    call_code[o++] = 0x48;
                    call_code[o++] = 0x8D;
                    call_code[o++] = 0x15;
                    lea2_disp_pos = o;
                    w32(call_code, o, 0);
                    o += 4;
                }
                else
                {
                    fprintf(stderr, "error: codegen: only int or string literal "
                                    "supported as second argument for now\n");
                    return 1;
                }
            }
            call_code[o++] = 0x31;
            call_code[o++] = 0xC0;
            call_code[o++] = 0xE8;
            call_disp_pos = o;
            w32(call_code, o, 0);
            o += 4;
            call_code[o++] = 0x48;
            call_code[o++] = 0x83;
            call_code[o++] = 0xC4;
            call_code[o++] = 0x28;
            call_code[o++] = 0xC3;
            call_size = (uint32_t)o;
            fmt = expr->args[0]->str_data;
            fmt_len = expr->args[0]->str_len;
            if (fmt_len < 0)
                fmt_len = 0;
            rdata_size = (uint32_t)fmt_len + 1;
            if (has_str2)
                rdata_size += (uint32_t)str2_len + 1;
        }
        uint32_t text_size = is_simple_call ? call_size : sizeof(code);
        uint32_t text_reloc_size =
            is_simple_call ? ((2 + (has_str2 ? 1 : 0)) * 10)
                           : 0; // reloc for lea1, call, and optional lea2
        uint32_t text_raw_off = text_off;
        reloc_off = is_simple_call ? (text_raw_off + text_size) : 0;
        rdata_off = is_simple_call ? (reloc_off + text_reloc_size) : 0;
        uint32_t symtab_off =
            is_simple_call ? (rdata_off + rdata_size) : (text_raw_off + text_size);
        uint32_t num_symbols = is_simple_call
                                   ? (has_str2 ? 4 : 3)
                                   : 1; // main, extern func, str0[, str1]
        uint32_t sym_size = 18 * num_symbols;
        uint32_t strtab_off = symtab_off + sym_size;
        uint32_t strtab_size = 4;
        uint32_t total = strtab_off + strtab_size;

        uint8_t *buf = (uint8_t *)xcalloc(1, total);

        // IMAGE_FILE_HEADER
        w16(buf, 0, 0x8664);                 // Machine x86-64
        w16(buf, 2, (uint16_t)num_sections); // NumberOfSections
        w32(buf, 4, 0);                      // TimeDateStamp
        w32(buf, 8, symtab_off);             // PointerToSymbolTable
        w32(buf, 12, num_symbols);           // NumberOfSymbols
        w16(buf, 16, 0);                     // SizeOfOptionalHeader
        w16(buf, 18, 0);                     // Characteristics

        // Section headers start here
        uint8_t *sh = buf + file_header_size;
        memset(sh, 0, section_header_size * num_sections);
        memcpy(sh + 0, ".text", 5);
        w32(sh, 8, text_size);
        w32(sh, 12, 0);
        w32(sh, 16, text_size);
        w32(sh, 20, text_raw_off);
        if (is_simple_call)
        {
            w32(sh, 24, reloc_off);
            w16(sh, 32, (uint16_t)(has_str2 ? 3 : 2));
        }
        else
        {
            w32(sh, 24, 0);
            w16(sh, 32, 0);
        }
        w32(sh, 28, 0);
        w16(sh, 34, 0);
        w32(sh, 36, 0x60000020);
        // Raw code
        if (is_simple_call)
            memcpy(buf + text_raw_off, call_code, text_size);
        else
            memcpy(buf + text_raw_off, code, text_size);

        if (is_simple_call)
        {
            // .rdata
            uint8_t *sh2 = sh + section_header_size;
            memcpy(sh2 + 0, ".rdata", 6);
            w32(sh2, 8, rdata_size);
            w32(sh2, 12, 0);
            w32(sh2, 16, rdata_size);
            w32(sh2, 20, rdata_off);
            w32(sh2, 24, 0);
            w32(sh2, 28, 0);
            w16(sh2, 32, 0);
            w16(sh2, 34, 0);
            w32(sh2, 36, 0x40000040);
            // Write strings
            memcpy(buf + rdata_off, fmt, (size_t)fmt_len);
            buf[rdata_off + fmt_len] = 0;
            uint32_t str0_off = rdata_off;
            uint32_t str1_off = 0;
            if (has_str2)
            {
                str1_off = rdata_off + (uint32_t)fmt_len + 1;
                memcpy(buf + str1_off, str2, (size_t)str2_len);
                buf[str1_off + str2_len] = 0;
            }
            // Relocations: lea rcx,[rip+disp32] -> str0, optional lea rdx -> str1,
            // call rel32 -> extern func
            uint8_t *rel = buf + reloc_off;
            // lea rcx -> str0 uses symbol index 2 (main=0, extern=1, str0=2)
            w32(rel, 0, (uint32_t)(lea1_disp_pos));
            w32(rel, 4, 2);
            w16(rel, 8, IMAGE_REL_AMD64_REL32);
            int ro = 10;
            if (has_str2)
            {
                w32(rel, ro + 0, (uint32_t)(lea2_disp_pos));
                w32(rel, ro + 4, 3);
                w16(rel, ro + 8, IMAGE_REL_AMD64_REL32);
                ro += 10;
            }
            // call rel32 -> extern function is symbol index 1
            w32(rel, ro + 0, (uint32_t)(call_disp_pos));
            w32(rel, ro + 4, 1);
            w16(rel, ro + 8, IMAGE_REL_AMD64_REL32);
        }

        // Symbol table
        uint8_t *sym = buf + symtab_off;
        memset(sym, 0, sym_size);
        memcpy(sym + 0, "main", 4);
        w32(sym, 8, 0);
        w16(sym, 12, 1);
        w16(sym, 14, IMAGE_SYM_DTYPE_FUNCTION);
        sym[16] = IMAGE_SYM_CLASS_EXTERNAL;
        sym[17] = 0;
        if (is_simple_call)
        {
            uint8_t *sy2 = sym + 18;
            memset(sy2, 0, 18);
            size_t fnl = strlen(expr->call_name);
            if (fnl <= 8)
            {
                memcpy(sy2 + 0, expr->call_name, fnl);
            }
            else
            { /* long names would use string table; keep simple for now */
                memcpy(sy2 + 0, "printf", 6);
            }
            w32(sy2, 8, 0);
            w16(sy2, 12, 0);
            w16(sy2, 14, IMAGE_SYM_DTYPE_FUNCTION);
            sy2[16] = IMAGE_SYM_CLASS_EXTERNAL;
            sy2[17] = 0;
            uint8_t *sy3 = sy2 + 18;
            memset(sy3, 0, 18);
            memcpy(sy3 + 0, "str0", 4);
            w32(sy3, 8, 0);
            w16(sy3, 12, 2);
            w16(sy3, 14, 0);
            sy3[16] = IMAGE_SYM_CLASS_STATIC;
            sy3[17] = 0;
            if (has_str2)
            {
                uint8_t *sy4 = sy3 + 18;
                memset(sy4, 0, 18);
                memcpy(sy4 + 0, "str1", 4);
                w32(sy4, 8, (uint32_t)(fmt_len + 1));
                w16(sy4, 12, 2);
                w16(sy4, 14, 0);
                sy4[16] = IMAGE_SYM_CLASS_STATIC;
                sy4[17] = 0;
            }
        }

        // String table: 4-byte length incl. itself
        w32(buf, strtab_off, 4);

        // Write .obj: prefer an explicitly requested object output path (used
        // for -c/--no-link and multi-file flows). Otherwise, place it adjacent
        // to the output filename.
        char objpath[512];
        const char *out = (opts && opts->output_path) ? opts->output_path : "a.exe";
        if (opts && opts->obj_output_path && *(opts->obj_output_path))
        {
            snprintf(objpath, sizeof(objpath), "%s", opts->obj_output_path);
        }
        else
        {
            size_t n = strlen(out);
            if (n >= 4 && (out[n - 4] == '.' || out[n - 4] == '\0'))
            {
                snprintf(objpath, sizeof(objpath), "%.*s.obj", (int)(n - 4), out);
            }
            else
            {
                snprintf(objpath, sizeof(objpath), "%s.obj", out);
            }
        }
        FILE *fo = fopen(objpath, "wb");
        if (!fo)
        {
            perror("fopen obj");
            free(buf);
            return 1;
        }
        fwrite(buf, 1, total, fo);
        fclose(fo);
        free(buf);

        // If user requested compile-only (-c/--no-link), stop after writing the
        // object file.
        if (opts && opts->no_link)
        {
            return 0;
        }

        // Link using cc
        char cmd[1024];
        if (opts && opts->freestanding)
        {
            // Freestanding: no CRT, no default libs; set entry to main
            // Add -ffreestanding and -nostartfiles for good measure
            snprintf(cmd, sizeof(cmd),
                     "cc -o \"%s\" \"%s\" -ffreestanding -nostdlib -nostartfiles -Wl,-e,main",
                     out, objpath);
        }
        else
        {
            snprintf(cmd, sizeof(cmd), "cc -o \"%s\" \"%s\"", out, objpath);
        }
        int rc = system(cmd);
        if (rc != 0)
        {
            fprintf(stderr, "link failed (rc=%d): %s\n", rc, cmd);
            return 1;
        }

        if (opts && opts->emit_asm)
        {
            char dump[512];
            snprintf(dump, sizeof(dump), "%s.S", out);
            FILE *df = fopen(dump, "wb");
            if (df)
            {
                AsmSyntax syn = opts ? opts->asm_syntax : ASM_INTEL;
                if (is_simple_call)
                {
                    const char *callee = expr->call_name ? expr->call_name : "unknown";
                    if (syn == ASM_INTEL)
                    {
                        fprintf(
                            df,
                            ".intel_syntax noprefix\n.section .text\n.globl main\nmain:\n");
                        fprintf(df, "  sub rsp, 40\n  lea rcx, [rel .Lstr0]\n");
                        if (has_int_arg)
                        {
                            fprintf(df, "  mov edx, %d\n", int_arg_val);
                        }
                        else if (has_str2)
                        {
                            fprintf(df, "  lea rdx, [rel .Lstr1]\n");
                        }
                        fprintf(df, "  xor eax, eax\n  call %s\n  add rsp, 40\n  ret\n",
                                callee);
                        fprintf(df, ".section .rdata,\"dr\"\n");
                    }
                    else if (syn == ASM_ATT)
                    {
                        // GAS AT&T: movl $imm, %edx ; leaq .Lstr0(%%rip), %%rcx; etc.
                        fprintf(df, ".section .text\n.globl main\nmain:\n");
                        fprintf(df, "  subq $40, %%rsp\n  leaq .Lstr0(%%rip), %%rcx\n");
                        if (has_int_arg)
                        {
                            fprintf(df, "  movl $%d, %%edx\n", int_arg_val);
                        }
                        else if (has_str2)
                        {
                            fprintf(df, "  leaq .Lstr1(%%rip), %%rdx\n");
                        }
                        fprintf(df,
                                "  xorl %%eax, %%eax\n  call %s@PLT\n  addq $40, %%rsp\n  "
                                "ret\n",
                                callee);
                        fprintf(df, ".section .rdata,\"dr\"\n");
                    }
                    else
                    { // NASM
                        fprintf(
                            df,
                            "default rel\nsection .text\nextern %s\nglobal main\nmain:\n",
                            callee);
                        fprintf(df, "  sub rsp, 40\n  lea rcx, [rel .Lstr0]\n");
                        if (has_int_arg)
                        {
                            fprintf(df, "  mov edx, %d\n", int_arg_val);
                        }
                        else if (has_str2)
                        {
                            fprintf(df, "  lea rdx, [rel .Lstr1]\n");
                        }
                        fprintf(df, "  xor eax, eax\n  call %s\n  add rsp, 40\n  ret\n",
                                callee);
                        fprintf(df, "section .rdata align=1\n");
                    }
                    // Strings
                    if (syn == ASM_NASM)
                    {
                        fprintf(df, ".Lstr0: db ");
                        for (int i = 0; i < fmt_len; i++)
                        {
                            unsigned char ch = (unsigned char)fmt[i];
                            if (ch == '"' || ch == '\\')
                                fprintf(df, "\\");
                            fprintf(df, "'%c',", ch);
                        }
                        fprintf(df, "0\n");
                        if (has_str2)
                        {
                            fprintf(df, ".Lstr1: db ");
                            for (int i = 0; i < str2_len; i++)
                            {
                                unsigned char ch = (unsigned char)str2[i];
                                if (ch == '"' || ch == '\\')
                                    fprintf(df, "\\");
                                fprintf(df, "'%c',", ch);
                            }
                            fprintf(df, "0\n");
                        }
                    }
                    else
                    {
                        // GAS variants
                        fprintf(df, ".Lstr0:\n  .asciz \"");
                        for (int i = 0; i < fmt_len; i++)
                        {
                            unsigned char ch = (unsigned char)fmt[i];
                            if (ch == '\\' || ch == '\"')
                            {
                                fputc('\\', df);
                                fputc(ch, df);
                            }
                            else if (ch == '\n')
                            {
                                fputs("\\n", df);
                            }
                            else if (ch == '\r')
                            {
                                fputs("\\r", df);
                            }
                            else if (ch == '\t')
                            {
                                fputs("\\t", df);
                            }
                            else
                            {
                                fputc(ch, df);
                            }
                        }
                        fprintf(df, "\"\n");
                        if (has_str2)
                        {
                            fprintf(df, ".Lstr1:\n  .asciz \"");
                            for (int i = 0; i < str2_len; i++)
                            {
                                unsigned char ch = (unsigned char)str2[i];
                                if (ch == '\\' || ch == '\"')
                                {
                                    fputc('\\', df);
                                    fputc(ch, df);
                                }
                                else if (ch == '\n')
                                {
                                    fputs("\\n", df);
                                }
                                else if (ch == '\r')
                                {
                                    fputs("\\r", df);
                                }
                                else if (ch == '\t')
                                {
                                    fputs("\\t", df);
                                }
                                else
                                {
                                    fputc(ch, df);
                                }
                            }
                            fprintf(df, "\"\n");
                        }
                    }
                }
                else
                {
                    // Constant return path
                    if (syn == ASM_INTEL)
                    {
                        fprintf(df,
                                ".intel_syntax noprefix\n.section .text\n.globl "
                                "main\nmain:\n  mov eax, %d\n  ret\n",
                                (int)(int32_t)val);
                    }
                    else if (syn == ASM_ATT)
                    {
                        fprintf(df,
                                ".section .text\n.globl main\nmain:\n  movl $%d, %%eax\n  "
                                "ret\n",
                                (int)(int32_t)val);
                    }
                    else
                    { // NASM
                        fprintf(df,
                                "section .text\nglobal main\nmain:\n  mov eax, %d\n  ret\n",
                                (int)(int32_t)val);
                    }
                }
                fclose(df);
            }
        }

        return 0;
    }
    if (unit->kind == ND_UNIT)
    {
        // Emit all functions in the translation unit so helper functions are
        // defined
        return codegen_asm_backend_link_all(unit, opts);
    }
    fprintf(stderr, "codegen: unsupported unit kind %d\n", unit->kind);
    return 1;
}
