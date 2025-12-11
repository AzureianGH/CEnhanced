#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#include "mangle.h"

void describe_type(const Type *t, char *buf, size_t bufsz)
{
    if (!buf || bufsz == 0)
        return;
    if (!t)
    {
        snprintf(buf, bufsz, "i32");
        return;
    }
    switch (t->kind)
    {
    case TY_I8:
        snprintf(buf, bufsz, "i8");
        return;
    case TY_U8:
        snprintf(buf, bufsz, "u8");
        return;
    case TY_I16:
        snprintf(buf, bufsz, "i16");
        return;
    case TY_U16:
        snprintf(buf, bufsz, "u16");
        return;
    case TY_I32:
        snprintf(buf, bufsz, "i32");
        return;
    case TY_U32:
        snprintf(buf, bufsz, "u32");
        return;
    case TY_I64:
        snprintf(buf, bufsz, "i64");
        return;
    case TY_U64:
        snprintf(buf, bufsz, "u64");
        return;
    case TY_F32:
        snprintf(buf, bufsz, "f32");
        return;
    case TY_F64:
        snprintf(buf, bufsz, "f64");
        return;
    case TY_F128:
        snprintf(buf, bufsz, "f128");
        return;
    case TY_VOID:
        snprintf(buf, bufsz, "void");
        return;
    case TY_CHAR:
        snprintf(buf, bufsz, "char");
        return;
    case TY_BOOL:
        snprintf(buf, bufsz, "bool");
        return;
    case TY_VA_LIST:
        snprintf(buf, bufsz, "va_list");
        return;
    case TY_ARRAY:
        if (!t->array.elem)
        {
            snprintf(buf, bufsz, "array[]");
            return;
        }
        else
        {
            char inner[128];
            describe_type(t->array.elem, inner, sizeof(inner));
            if (t->array.is_unsized)
                snprintf(buf, bufsz, "array[] of %s", inner);
            else
                snprintf(buf, bufsz, "array[%d] of %s", t->array.length, inner);
            return;
        }
    case TY_PTR:
        if (!t->pointee)
        {
            snprintf(buf, bufsz, "ptr");
            return;
        }
        else
        {
            char inner[128];
            describe_type(t->pointee, inner, sizeof(inner));
            snprintf(buf, bufsz, "ptr to %s", inner);
            return;
        }
    case TY_FUNC:
    {
        char ret_buf[128];
        if (t->func.ret)
            describe_type(t->func.ret, ret_buf, sizeof(ret_buf));
        else
            snprintf(ret_buf, sizeof(ret_buf), "void");
        snprintf(buf, bufsz, "fun(%d%s) -> %s", t->func.param_count,
                 t->func.is_varargs ? ", varargs" : "", ret_buf);
        return;
    }
    case TY_STRUCT:
        snprintf(buf, bufsz, "struct %s", t->struct_name ? t->struct_name : "<anonymous>");
        return;
    case TY_TEMPLATE_PARAM:
        if (t->template_param_name && *t->template_param_name)
            snprintf(buf, bufsz, "%s", t->template_param_name);
        else
            snprintf(buf, bufsz, "template_param#%d", t->template_param_index);
        return;
    default:
        snprintf(buf, bufsz, "type kind %d", t->kind);
        return;
    }
}

static char *module_name_to_prefix(const char *module_full)
{
    if (!module_full || !*module_full)
        return NULL;
    size_t len = strlen(module_full);
    char *copy = (char *)xmalloc(len + 1);
    memcpy(copy, module_full, len + 1);
    for (size_t i = 0; i < len; ++i)
    {
        if (copy[i] == '.')
            copy[i] = '_';
    }
    return copy;
}

static void append_mangled_type(char **out_buf, size_t *out_len, size_t *out_cap, const Type *ty)
{
    if (!out_buf || !out_len || !out_cap)
        return;
    if (!ty)
    {
        const char *fallback = "_v";
        size_t add = strlen(fallback);
        if (*out_len + add + 1 > *out_cap)
        {
            size_t new_cap = (*out_cap ? *out_cap * 2 : 32);
            while (new_cap < *out_len + add + 1)
                new_cap *= 2;
            char *grown = (char *)realloc(*out_buf, new_cap);
            if (!grown)
                return;
            *out_buf = grown;
            *out_cap = new_cap;
        }
        memcpy(*out_buf + *out_len, fallback, add);
        *out_len += add;
        (*out_buf)[*out_len] = '\0';
        return;
    }

    char tmp[128];
    describe_type(ty, tmp, sizeof(tmp));
    size_t need = strlen(tmp) + 2;
    if (*out_len + need + 1 > *out_cap)
    {
        size_t new_cap = (*out_cap ? *out_cap * 2 : 32);
        while (new_cap < *out_len + need + 1)
            new_cap *= 2;
        char *grown = (char *)realloc(*out_buf, new_cap);
        if (!grown)
            return;
        *out_buf = grown;
        *out_cap = new_cap;
    }
    (*out_buf)[(*out_len)++] = '_';
    for (const char *p = tmp; *p; ++p)
    {
        char c = *p;
        if ((c >= 'a' && c <= 'z') || (c >= '0' && c <= '9'))
        {
            (*out_buf)[(*out_len)++] = c;
        }
        else if (c >= 'A' && c <= 'Z')
        {
            (*out_buf)[(*out_len)++] = (char)(c - 'A' + 'a');
        }
        else
        {
            (*out_buf)[(*out_len)++] = '_';
        }
    }
    (*out_buf)[*out_len] = '\0';
}

// FNV-1a 64-bit hash to make long mangled names unique yet compact.
static uint64_t fnv1a64(const char *s)
{
    uint64_t hash = 1469598103934665603ULL;
    const uint64_t prime = 1099511628211ULL;
    if (!s)
        return hash;
    while (*s)
    {
        hash ^= (uint8_t)(*s++);
        hash *= prime;
    }
    return hash;
}

// If a mangled name grows too large for toolchains, shorten it and append a
// stable hash to avoid collisions when -g includes full type spellings.
static char *shorten_mangled(char *name)
{
    const size_t MAX_LEN = 180;   // avoid extremely long assembler symbols
    const size_t KEEP_LEN = 96;   // keep a readable prefix
    if (!name)
        return NULL;
    size_t len = strlen(name);
    if (len <= MAX_LEN)
        return name;

    uint64_t h = fnv1a64(name);
    size_t keep = (len < KEEP_LEN) ? len : KEEP_LEN;
    // prefix + '_' + 16 hex digits + null terminator
    size_t out_len = keep + 1 + 16 + 1;
    char *out = (char *)xmalloc(out_len);
    snprintf(out, out_len, "%.*s_%016llx", (int)keep, name, (unsigned long long)h);
    free(name);
    return out;
}

char *append_param_signature(const char *base_name, const Node *fn)
{
    if (!base_name || !fn)
        return NULL;
    size_t base_len = strlen(base_name);
    size_t cap = base_len + 32;
    char *buffer = (char *)xmalloc(cap);
    memcpy(buffer, base_name, base_len + 1);
    size_t len = base_len;

    if (!fn->param_count || fn->param_count <= 0)
        return shorten_mangled(buffer);

    for (int i = 0; i < fn->param_count; ++i)
    {
        Type *param_ty = NULL;
        if (fn->param_types && i < fn->param_count)
            param_ty = fn->param_types[i];
        append_mangled_type(&buffer, &len, &cap, param_ty);
    }

    buffer[len] = '\0';
    return shorten_mangled(buffer);
}

char *module_backend_name(const char *module_full, const char *fn_name, const Node *fn)
{
    if (!module_full || !*module_full || !fn_name || !*fn_name)
        return NULL;
    char *prefix = module_name_to_prefix(module_full);
    if (!prefix)
        return NULL;
    size_t prefix_len = strlen(prefix);
    size_t fn_len = strlen(fn_name);
    char *res = (char *)xmalloc(prefix_len + 1 + fn_len + 1);
    memcpy(res, prefix, prefix_len);
    res[prefix_len] = '_';
    memcpy(res + prefix_len + 1, fn_name, fn_len);
    res[prefix_len + 1 + fn_len] = '\0';
    free(prefix);
    char *with_params = fn ? append_param_signature(res, fn) : NULL;
    if (!with_params)
        return shorten_mangled(res);
    free(res);
    return shorten_mangled(with_params);
}
