#include "ast.h"
#include "cc/bytecode.h"

#include <errno.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct
{
    char **items;
    size_t count;
    size_t capacity;
} StringList;

static void string_list_init(StringList *list)
{
    if (!list)
        return;
    list->items = NULL;
    list->count = 0;
    list->capacity = 0;
}

static void string_list_free(StringList *list)
{
    if (!list)
        return;
    for (size_t i = 0; i < list->count; ++i)
        free(list->items[i]);
    free(list->items);
    list->items = NULL;
    list->count = 0;
    list->capacity = 0;
}

static bool string_list_reserve(StringList *list, size_t desired)
{
    if (!list)
        return false;
    if (list->capacity >= desired)
        return true;

    size_t new_cap = list->capacity ? list->capacity * 2 : 8;
    if (new_cap < desired)
        new_cap = desired;

    char **new_items = (char **)realloc(list->items, new_cap * sizeof(char *));
    if (!new_items)
        return false;

    list->items = new_items;
    list->capacity = new_cap;
    return true;
}

static bool string_list_append_copy(StringList *list, const char *src, size_t len)
{
    if (!list)
        return false;
    if (!string_list_reserve(list, list->count + 1))
        return false;

    char *copy = (char *)malloc(len + 1);
    if (!copy)
        return false;
    if (src && len)
        memcpy(copy, src, len);
    copy[len] = '\0';

    list->items[list->count++] = copy;
    return true;
}

static bool string_list_append(StringList *list, const char *str)
{
    if (!str)
        str = "";
    return string_list_append_copy(list, str, strlen(str));
}

static bool string_list_appendf(StringList *list, const char *fmt, ...)
{
    if (!list || !fmt)
        return false;

    va_list ap;
    va_start(ap, fmt);
    int needed = vsnprintf(NULL, 0, fmt, ap);
    va_end(ap);
    if (needed < 0)
        return false;

    size_t len = (size_t)needed;
    char *buffer = (char *)malloc(len + 1);
    if (!buffer)
        return false;

    va_start(ap, fmt);
    int written = vsnprintf(buffer, len + 1, fmt, ap);
    va_end(ap);
    if (written < 0)
    {
        free(buffer);
        return false;
    }

    bool ok = string_list_append_copy(list, buffer, len);
    free(buffer);
    return ok;
}

static void string_list_remove_range(StringList *list, size_t index, size_t count)
{
    if (!list || index >= list->count || count == 0)
        return;
    if (index + count > list->count)
        count = list->count - index;

    for (size_t i = 0; i < count; ++i)
    {
        free(list->items[index + i]);
        list->items[index + i] = NULL;
    }

    size_t tail = list->count - (index + count);
    if (tail > 0)
        memmove(&list->items[index], &list->items[index + count], tail * sizeof(char *));

    list->count -= count;
}

typedef struct
{
    StringList lines;
} CcbModule;

typedef struct
{
    const char *name;
    Type *type;
    CCValueType value_type;
    bool is_address_only;
    bool is_param;
    int index;
} CcbLocal;

typedef struct
{
    CcbModule *module;
    const Node *fn;
    StringList body;
    CCValueType ret_type;
    CcbLocal *locals;
    size_t locals_count;
    size_t locals_capacity;
    size_t param_count;
    size_t local_count;
    int next_label_id;
} CcbFunctionBuilder;

static CCValueType map_type_to_cc(const Type *ty);
static const char *cc_type_name(CCValueType ty);
static size_t ccb_value_type_size(CCValueType ty);
static bool ccb_value_type_is_integer(CCValueType ty);
static bool ccb_value_type_is_float(CCValueType ty);
static bool ccb_value_type_is_signed(CCValueType ty);
static CcbLocal *ccb_local_add(CcbFunctionBuilder *fb, const char *name, Type *type, bool address_only, bool is_param);
static int ccb_emit_expr_basic(CcbFunctionBuilder *fb, const Node *expr);
static int ccb_emit_stmt_basic(CcbFunctionBuilder *fb, const Node *stmt);
static bool ccb_emit_load_indirect(StringList *body, CCValueType ty);
static bool ccb_emit_store_indirect(StringList *body, CCValueType ty);
static int ccb_emit_convert_between(CcbFunctionBuilder *fb, CCValueType from_ty, CCValueType to_ty, const Node *node);
static int ccb_emit_index_address(CcbFunctionBuilder *fb, const Node *expr, CCValueType *out_elem_ty, const Type **out_elem_type);
static int ccb_emit_member_address(CcbFunctionBuilder *fb, const Node *expr, CCValueType *out_field_ty, const Type **out_field_type);
static size_t ccb_pointer_elem_size(const Type *ptr_type);
static int ccb_emit_pointer_add_like(CcbFunctionBuilder *fb, const Node *expr, bool is_add);
static int ccb_emit_pointer_difference(CcbFunctionBuilder *fb, const Node *expr);
static int ccb_emit_struct_zero(CcbFunctionBuilder *fb, const Node *var_decl, const char *var_name, const Type *struct_type);
static int ccb_emit_struct_initializer(CcbFunctionBuilder *fb, const Node *var_decl, const char *var_name, const Type *struct_type, const Node *init);
static bool type_is_address_only(const Type *ty);
static int ccb_emit_pointer_offset(CcbFunctionBuilder *fb, int offset, const Node *node);
static int ccb_emit_struct_copy(CcbFunctionBuilder *fb, const Type *struct_type, CcbLocal *dst_ptr, CcbLocal *src_ptr);
static bool ccb_emit_load_local(CcbFunctionBuilder *fb, const CcbLocal *local);
static bool ccb_emit_store_local(CcbFunctionBuilder *fb, const CcbLocal *local);
static size_t ccb_type_size_bytes(const Type *ty);
static bool ccb_module_append_extern(CcbModule *mod, const Symbol *sym);
static int ccb_module_emit_externs(CcbModule *mod, const CodegenOptions *opts);
static void ccb_function_optimize(CcbFunctionBuilder *fb, const CodegenOptions *opts);
static bool ccb_instruction_is_pure(const char *line);
static void ccb_opt_prune_dropped_values(CcbFunctionBuilder *fb);
static void ccb_opt_fold_const_binops(CcbFunctionBuilder *fb);

static void ccb_module_init(CcbModule *mod)
{
    if (!mod)
        return;
    string_list_init(&mod->lines);
}

static void ccb_module_free(CcbModule *mod)
{
    if (!mod)
        return;
    string_list_free(&mod->lines);
}

static bool ccb_module_append_line(CcbModule *mod, const char *line)
{
    if (!mod)
        return false;
    return string_list_append(&mod->lines, line);
}

static bool ccb_module_appendf(CcbModule *mod, const char *fmt, ...)
{
    if (!mod || !fmt)
        return false;

    va_list ap;
    va_start(ap, fmt);
    int needed = vsnprintf(NULL, 0, fmt, ap);
    va_end(ap);
    if (needed < 0)
        return false;

    size_t len = (size_t)needed;
    char *buffer = (char *)malloc(len + 1);
    if (!buffer)
        return false;

    va_start(ap, fmt);
    int written = vsnprintf(buffer, len + 1, fmt, ap);
    va_end(ap);
    if (written < 0)
    {
        free(buffer);
        return false;
    }

    bool ok = string_list_append_copy(&mod->lines, buffer, len);
    free(buffer);
    return ok;
}

static bool ccb_module_append_extern(CcbModule *mod, const Symbol *sym)
{
    if (!mod || !sym || !sym->name || !*sym->name)
        return true;

    size_t params_len = 2; // opening and closing paren
    if (sym->sig.param_count > 0 && sym->sig.params)
    {
        for (int i = 0; i < sym->sig.param_count; ++i)
        {
            const Type *param_ty = (sym->sig.params && i < sym->sig.param_count)
                                       ? sym->sig.params[i]
                                       : NULL;
            const char *ty_name = cc_type_name(map_type_to_cc(param_ty));
            params_len += strlen(ty_name);
            if (i + 1 < sym->sig.param_count)
                params_len += 1; // comma separator
        }
    }

    char *params = (char *)malloc(params_len + 1);
    if (!params)
        return false;

    size_t pos = 0;
    params[pos++] = '(';
    if (sym->sig.param_count > 0 && sym->sig.params)
    {
        for (int i = 0; i < sym->sig.param_count; ++i)
        {
            if (i > 0)
                params[pos++] = ',';
            const Type *param_ty = (sym->sig.params && i < sym->sig.param_count)
                                       ? sym->sig.params[i]
                                       : NULL;
            const char *ty_name = cc_type_name(map_type_to_cc(param_ty));
            size_t len = strlen(ty_name);
            memcpy(params + pos, ty_name, len);
            pos += len;
        }
    }
    params[pos++] = ')';
    params[pos] = '\0';

    const char *ret_name = cc_type_name(map_type_to_cc(sym->sig.ret));
    const char *abi = sym->abi;
    bool include_abi = abi && *abi && strcmp(abi, "C") != 0;
    const char *varargs_suffix = sym->sig.is_varargs ? " varargs" : "";

    bool ok;
    if (include_abi)
    {
        ok = ccb_module_appendf(mod, ".extern %s abi=%s params=%s returns=%s%s",
                                sym->name, abi, params, ret_name, varargs_suffix);
    }
    else
    {
        ok = ccb_module_appendf(mod, ".extern %s params=%s returns=%s%s",
                                sym->name, params, ret_name, varargs_suffix);
    }

    free(params);
    return ok;
}

static int ccb_module_emit_externs(CcbModule *mod, const CodegenOptions *opts)
{
    if (!mod || !opts || !opts->externs || opts->extern_count <= 0)
        return 0;

    bool any = false;
    for (int i = 0; i < opts->extern_count; ++i)
    {
        const Symbol *sym = &opts->externs[i];
        if (!sym || !sym->is_extern)
            continue;
        if (!ccb_module_append_extern(mod, sym))
            return 1;
        if (sym->is_noreturn)
        {
            if (!ccb_module_appendf(mod, ".no-return %s", sym->name))
                return 1;
        }
        any = true;
    }

    if (any)
    {
        if (!ccb_module_append_line(mod, ""))
            return 1;
    }

    return 0;
}

static void ccb_function_builder_init(CcbFunctionBuilder *fb, CcbModule *mod, const Node *fn)
{
    if (!fb)
        return;
    fb->module = mod;
    fb->fn = fn;
    fb->ret_type = map_type_to_cc(fn && fn->ret_type ? fn->ret_type : NULL);
    string_list_init(&fb->body);
    fb->locals = NULL;
    fb->locals_count = 0;
    fb->locals_capacity = 0;
    fb->param_count = 0;
    fb->local_count = 0;
    fb->next_label_id = 0;
}

static void ccb_function_builder_free(CcbFunctionBuilder *fb)
{
    if (!fb)
        return;
    string_list_free(&fb->body);
    free(fb->locals);
    fb->locals = NULL;
    fb->locals_count = 0;
    fb->locals_capacity = 0;
    fb->param_count = 0;
    fb->local_count = 0;
    fb->module = NULL;
    fb->fn = NULL;
    fb->next_label_id = 0;
}

static unsigned long long ccb_mask_for_width(unsigned width)
{
    if (width == 0)
        return 0;
    if (width >= 64)
        return ~0ULL;
    return (1ULL << width) - 1ULL;
}

static long long ccb_sign_extend(unsigned long long value, unsigned width)
{
    if (width == 0 || width >= 64)
        return (long long)value;
    unsigned long long mask = ccb_mask_for_width(width);
    unsigned long long sign_bit = 1ULL << (width - 1);
    value &= mask;
    if (value & sign_bit)
        return (long long)(value | (~mask));
    return (long long)value;
}

static unsigned ccb_type_bit_width_from_name(const char *type_name)
{
    if (!type_name)
        return 0;
    if (strcmp(type_name, "i1") == 0)
        return 1;
    if (strcmp(type_name, "i8") == 0 || strcmp(type_name, "u8") == 0)
        return 8;
    if (strcmp(type_name, "i16") == 0 || strcmp(type_name, "u16") == 0)
        return 16;
    if (strcmp(type_name, "i32") == 0 || strcmp(type_name, "u32") == 0 ||
        strcmp(type_name, "f32") == 0)
        return 32;
    if (strcmp(type_name, "i64") == 0 || strcmp(type_name, "u64") == 0 ||
        strcmp(type_name, "f64") == 0 || strcmp(type_name, "ptr") == 0)
        return 64;
    return 0;
}

static bool ccb_type_is_integer_name(const char *type_name)
{
    if (!type_name)
        return false;
    return type_name[0] == 'i' || type_name[0] == 'u';
}

static bool ccb_instruction_is_drop(const char *line)
{
    if (!line)
        return false;
    while (*line == ' ' || *line == '\t')
        ++line;
    return strncmp(line, "drop ", 5) == 0;
}

static bool ccb_instruction_is_pure(const char *line)
{
    if (!line)
        return false;
    while (*line == ' ' || *line == '\t')
        ++line;
    if (*line == '\0' || *line == '.')
        return false;
    if (strncmp(line, "const_str ", 10) == 0)
        return true;
    if (strncmp(line, "const ", 6) == 0)
        return true;
    if (strncmp(line, "load_", 5) == 0)
        return true;
    if (strncmp(line, "convert ", 8) == 0)
        return true;
    if (strncmp(line, "binop ", 6) == 0)
        return true;
    if (strncmp(line, "compare ", 8) == 0)
        return true;
    if (strncmp(line, "unop ", 5) == 0)
        return true;
    if (strncmp(line, "dup", 3) == 0)
        return true;
    if (strncmp(line, "nop", 3) == 0)
        return true;
    if (strncmp(line, "phi ", 4) == 0)
        return true;
    if (strncmp(line, "select ", 7) == 0)
        return true;
    return false;
}

typedef struct
{
    char type[16];
    unsigned width;
    bool is_signed;
    unsigned long long u;
    long long s;
} CcbConstInfo;

static bool ccb_parse_const_info(const char *line, CcbConstInfo *out)
{
    if (!line || !out)
        return false;

    while (*line == ' ' || *line == '\t')
        ++line;
    if (strncmp(line, "const_str", 9) == 0)
        return false;
    if (strncmp(line, "const ", 6) != 0)
        return false;
    line += 6;

    char type_name[16];
    char value_text[128];
    if (sscanf(line, "%15s %127s", type_name, value_text) != 2)
        return false;

    unsigned width = ccb_type_bit_width_from_name(type_name);
    if (width == 0)
        return false;
    if (!ccb_type_is_integer_name(type_name))
        return false;

    bool is_signed = (type_name[0] == 'i');
    char *endptr = NULL;
    errno = 0;
    unsigned long long uval = 0;
    long long sval = 0;
    if (is_signed)
    {
        sval = strtoll(value_text, &endptr, 0);
        if (endptr == value_text || *endptr != '\0' || errno == ERANGE)
            return false;
        uval = (unsigned long long)sval;
    }
    else
    {
        uval = strtoull(value_text, &endptr, 0);
        if (endptr == value_text || *endptr != '\0' || errno == ERANGE)
            return false;
        sval = (long long)uval;
    }

    unsigned long long mask = ccb_mask_for_width(width);
    uval &= mask;
    sval = is_signed ? ccb_sign_extend(uval, width) : (long long)uval;

    strncpy(out->type, type_name, sizeof(out->type) - 1);
    out->type[sizeof(out->type) - 1] = '\0';
    out->width = width;
    out->is_signed = is_signed;
    out->u = uval;
    out->s = sval;
    return true;
}

static bool ccb_parse_binop_info(const char *line, char *op_buf, size_t op_sz,
                                 char *type_buf, size_t type_sz, bool *is_unsigned)
{
    if (!line || !op_buf || !type_buf || !is_unsigned)
        return false;

    while (*line == ' ' || *line == '\t')
        ++line;
    if (strncmp(line, "binop ", 6) != 0)
        return false;
    line += 6;

    char op[16];
    char type_name[16];
    char extra[16];
    int parsed = sscanf(line, "%15s %15s %15s", op, type_name, extra);
    if (parsed < 2)
        return false;

    bool unsigned_hint = false;
    if (parsed >= 3 && strcmp(extra, "unsigned") == 0)
        unsigned_hint = true;

    strncpy(op_buf, op, op_sz - 1);
    op_buf[op_sz - 1] = '\0';
    strncpy(type_buf, type_name, type_sz - 1);
    type_buf[type_sz - 1] = '\0';
    *is_unsigned = unsigned_hint;
    return true;
}

static void ccb_opt_fold_const_binops(CcbFunctionBuilder *fb)
{
    if (!fb)
        return;

    StringList *body = &fb->body;
    size_t i = 0;
    while (i + 2 < body->count)
    {
        const char *line0 = body->items[i];
        const char *line1 = body->items[i + 1];
        const char *line2 = body->items[i + 2];

        CcbConstInfo lhs;
        CcbConstInfo rhs;
        if (!ccb_parse_const_info(line0, &lhs) || !ccb_parse_const_info(line1, &rhs))
        {
            ++i;
            continue;
        }
        if (strcmp(lhs.type, rhs.type) != 0)
        {
            ++i;
            continue;
        }

        char op[16];
        char type_name[16];
        bool unsigned_hint = false;
        if (!ccb_parse_binop_info(line2, op, sizeof(op), type_name, sizeof(type_name), &unsigned_hint))
        {
            ++i;
            continue;
        }
        if (strcmp(type_name, lhs.type) != 0)
        {
            ++i;
            continue;
        }
        if (!ccb_type_is_integer_name(type_name))
        {
            ++i;
            continue;
        }

        unsigned width = lhs.width;
        if (width == 0)
        {
            ++i;
            continue;
        }

        bool use_unsigned = unsigned_hint || !lhs.is_signed;
        unsigned long long lhs_u = lhs.u & ccb_mask_for_width(width);
        unsigned long long rhs_u = rhs.u & ccb_mask_for_width(width);
        long long lhs_s = ccb_sign_extend(lhs_u, width);
        long long rhs_s = ccb_sign_extend(rhs_u, width);

        unsigned long long result_u = 0;
        long long result_s = 0;
        bool handled = true;

        if (strcmp(op, "add") == 0)
        {
            if (use_unsigned)
                result_u = lhs_u + rhs_u;
            else
                result_u = (unsigned long long)(lhs_s + rhs_s);
        }
        else if (strcmp(op, "sub") == 0)
        {
            if (use_unsigned)
                result_u = lhs_u - rhs_u;
            else
                result_u = (unsigned long long)(lhs_s - rhs_s);
        }
        else if (strcmp(op, "mul") == 0)
        {
            if (use_unsigned)
                result_u = lhs_u * rhs_u;
            else
                result_u = (unsigned long long)(lhs_s * rhs_s);
        }
        else if (strcmp(op, "div") == 0)
        {
            if (rhs_u == 0)
                handled = false;
            else if (use_unsigned)
                result_u = lhs_u / rhs_u;
            else
                result_u = (unsigned long long)(lhs_s / rhs_s);
        }
        else if (strcmp(op, "mod") == 0)
        {
            if (rhs_u == 0)
                handled = false;
            else if (use_unsigned)
                result_u = lhs_u % rhs_u;
            else
                result_u = (unsigned long long)(lhs_s % rhs_s);
        }
        else if (strcmp(op, "and") == 0)
        {
            result_u = lhs_u & rhs_u;
        }
        else if (strcmp(op, "or") == 0)
        {
            result_u = lhs_u | rhs_u;
        }
        else if (strcmp(op, "xor") == 0)
        {
            result_u = lhs_u ^ rhs_u;
        }
        else if (strcmp(op, "shl") == 0)
        {
            unsigned shift = (unsigned)(rhs_u & 63U);
            if (width < 64)
                shift %= width;
            result_u = (lhs_u << shift);
        }
        else if (strcmp(op, "shr") == 0)
        {
            unsigned shift = (unsigned)(rhs_u & 63U);
            if (width < 64)
                shift %= width;
            if (use_unsigned)
                result_u = lhs_u >> shift;
            else
                result_u = (unsigned long long)(ccb_sign_extend(lhs_u, width) >> shift);
        }
        else
        {
            handled = false;
        }

        if (!handled)
        {
            ++i;
            continue;
        }

        unsigned long long mask = ccb_mask_for_width(width);
        result_u &= mask;
        result_s = lhs.is_signed ? ccb_sign_extend(result_u, width) : (long long)result_u;

        char new_line[128];
        if (lhs.is_signed)
            snprintf(new_line, sizeof(new_line), "  const %s %lld", lhs.type, result_s);
        else
            snprintf(new_line, sizeof(new_line), "  const %s %llu", lhs.type, result_u);

        size_t new_len = strlen(new_line);
        char *replacement = (char *)malloc(new_len + 1);
        if (!replacement)
        {
            ++i;
            continue;
        }
        memcpy(replacement, new_line, new_len + 1);

        free(body->items[i]);
        body->items[i] = replacement;
        string_list_remove_range(body, i + 1, 2);
        if (i > 0)
            --i;
    }
}

static void ccb_opt_prune_dropped_values(CcbFunctionBuilder *fb)
{
    if (!fb)
        return;

    StringList *body = &fb->body;
    size_t i = 0;
    while (i < body->count)
    {
        const char *line = body->items[i];
        if (!ccb_instruction_is_drop(line))
        {
            ++i;
            continue;
        }

        size_t remove_start = i;
        bool removed_any = false;
        while (remove_start > 0)
        {
            const char *prev = body->items[remove_start - 1];
            if (!ccb_instruction_is_pure(prev))
                break;
            --remove_start;
            removed_any = true;
        }

        if (removed_any)
        {
            size_t remove_count = i - remove_start + 1;
            string_list_remove_range(body, remove_start, remove_count);
            if (remove_start == 0)
                i = 0;
            else
                i = remove_start - 1;
        }
        else
        {
            ++i;
        }
    }
}

static void ccb_function_optimize(CcbFunctionBuilder *fb, const CodegenOptions *opts)
{
    if (!fb || !opts || opts->opt_level <= 0)
        return;

    ccb_opt_prune_dropped_values(fb);

    if (opts->opt_level >= 2)
        ccb_opt_fold_const_binops(fb);
}

static void ccb_function_builder_register_params(CcbFunctionBuilder *fb)
{
    if (!fb || !fb->fn || fb->fn->param_count <= 0)
        return;

    for (int i = 0; i < fb->fn->param_count; ++i)
    {
        const char *param_name = NULL;
        if (fb->fn->param_names && fb->fn->param_names[i] && fb->fn->param_names[i][0] != '\0')
            param_name = fb->fn->param_names[i];
        Type *param_type = fb->fn->param_types ? fb->fn->param_types[i] : NULL;
        bool address_only = param_type && param_type->kind == TY_STRUCT;
        if (!ccb_local_add(fb, param_name, param_type, address_only, true))
        {
            diag_error_at(fb->fn->src, fb->fn->line, fb->fn->col,
                          "failed to allocate local slot for parameter %d", i);
            return;
        }
    }
}

static CcbLocal *ccb_local_lookup(CcbFunctionBuilder *fb, const char *name)
{
    if (!fb || !name)
        return NULL;
    for (size_t i = 0; i < fb->locals_count; ++i)
    {
        if (fb->locals[i].name && strcmp(fb->locals[i].name, name) == 0)
            return &fb->locals[i];
    }
    return NULL;
}

static CcbLocal *ccb_local_add(CcbFunctionBuilder *fb, const char *name, Type *type, bool address_only, bool is_param)
{
    if (!fb)
        return NULL;
    if (fb->locals_count == fb->locals_capacity)
    {
        size_t new_cap = fb->locals_capacity ? fb->locals_capacity * 2 : 8;
        CcbLocal *new_locals = (CcbLocal *)realloc(fb->locals, new_cap * sizeof(CcbLocal));
        if (!new_locals)
            return NULL;
        fb->locals = new_locals;
        fb->locals_capacity = new_cap;
    }

    CcbLocal *slot = &fb->locals[fb->locals_count++];
    slot->name = name;
    slot->type = type;
    slot->value_type = address_only ? CC_TYPE_PTR : map_type_to_cc(type);
    slot->is_address_only = address_only;
    slot->is_param = is_param;
    slot->index = is_param ? (int)fb->param_count : (int)fb->local_count;
    if (is_param)
        fb->param_count++;
    else
        fb->local_count++;
    return slot;
}

static void ccb_make_label(CcbFunctionBuilder *fb, char *buffer, size_t bufsz, const char *prefix)
{
    if (!fb || !buffer || bufsz == 0)
        return;
    int id = fb->next_label_id++;
    snprintf(buffer, bufsz, "%s%d", prefix ? prefix : "L", id);
}

static const char *ccb_convert_kind_name(CCConvertKind kind);
static bool ccb_emit_convert(StringList *body, CCConvertKind kind, CCValueType from_ty, CCValueType to_ty);

static CCValueType ccb_type_for_expr(const Node *expr)
{
    if (!expr)
        return CC_TYPE_VOID;
    if (expr->type)
        return map_type_to_cc(expr->type);

    switch (expr->kind)
    {
    case ND_STRING:
        return CC_TYPE_PTR;
    case ND_INT:
        return CC_TYPE_I32;
    case ND_VAR:
        return CC_TYPE_I32;
    case ND_ASSIGN:
        return ccb_type_for_expr(expr->rhs);
    case ND_CALL:
        return CC_TYPE_I32;
    case ND_CAST:
        if (expr->type)
            return map_type_to_cc(expr->type);
        if (expr->lhs)
            return ccb_type_for_expr(expr->lhs);
        return CC_TYPE_VOID;
    default:
        return CC_TYPE_I32;
    }
}

static bool ccb_format_type_list(const CCValueType *types, size_t count, char **out_text)
{
    if (!out_text)
        return false;

    size_t cap = 16 + count * 8;
    char *buffer = (char *)malloc(cap);
    if (!buffer)
        return false;

    size_t len = 0;
    buffer[len++] = '(';
    for (size_t i = 0; i < count; ++i)
    {
        if (i > 0)
        {
            if (len + 1 >= cap)
            {
                cap *= 2;
                char *nbuf = (char *)realloc(buffer, cap);
                if (!nbuf)
                {
                    free(buffer);
                    return false;
                }
                buffer = nbuf;
            }
            buffer[len++] = ',';
        }
        const char *name = cc_type_name(types ? types[i] : CC_TYPE_I32);
        size_t nlen = strlen(name);
        if (len + nlen + 2 >= cap)
        {
            while (len + nlen + 2 >= cap)
                cap *= 2;
            char *nbuf = (char *)realloc(buffer, cap);
            if (!nbuf)
            {
                free(buffer);
                return false;
            }
            buffer = nbuf;
        }
        memcpy(buffer + len, name, nlen);
        len += nlen;
    }
    if (len + 2 > cap)
    {
        cap = len + 2;
        char *nbuf = (char *)realloc(buffer, cap);
        if (!nbuf)
        {
            free(buffer);
            return false;
        }
        buffer = nbuf;
    }
    buffer[len++] = ')';
    buffer[len] = '\0';
    *out_text = buffer;
    return true;
}

static bool ccb_emit_drop_for_type(StringList *body, CCValueType ty)
{
    if (!body)
        return false;
    if (ty == CC_TYPE_VOID)
        return true;
    return string_list_appendf(body, "  drop %s", cc_type_name(ty));
}

static int ccb_is_hex(int c)
{
    return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

static int ccb_hex_value(int c)
{
    if (c >= '0' && c <= '9')
        return c - '0';
    if (c >= 'a' && c <= 'f')
        return 10 + (c - 'a');
    if (c >= 'A' && c <= 'F')
        return 10 + (c - 'A');
    return 0;
}

static int ccb_is_oct(int c)
{
    return c >= '0' && c <= '7';
}

static int ccb_utf8_encode(uint32_t cp, unsigned char out[4])
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

static unsigned char *ccb_decode_c_escapes(const char *s, int len, int *out_len)
{
    if (!s || len < 0 || !out_len)
        return NULL;

    unsigned char *buf = (unsigned char *)xmalloc((size_t)(len ? len : 1));
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
        case '"':
            buf[bi++] = '"';
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
            break;
        case '0':
            buf[bi++] = 0;
            break;
        case 'x':
        {
            int v = 0, digits = 0;
            while (i < len && ccb_is_hex((unsigned char)s[i]))
            {
                v = (v << 4) | ccb_hex_value((unsigned char)s[i]);
                i++;
                digits++;
            }
            buf[bi++] = digits ? (unsigned char)(v & 0xFF) : (unsigned char)'x';
            break;
        }
        case 'u':
        {
            if (i + 4 <= len && ccb_is_hex((unsigned char)s[i]) &&
                ccb_is_hex((unsigned char)s[i + 1]) &&
                ccb_is_hex((unsigned char)s[i + 2]) &&
                ccb_is_hex((unsigned char)s[i + 3]))
            {
                uint32_t v = (ccb_hex_value((unsigned char)s[i]) << 12) |
                             (ccb_hex_value((unsigned char)s[i + 1]) << 8) |
                             (ccb_hex_value((unsigned char)s[i + 2]) << 4) |
                             ccb_hex_value((unsigned char)s[i + 3]);
                i += 4;
                unsigned char tmp[4];
                int n = ccb_utf8_encode(v, tmp);
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
                    if (!ccb_is_hex((unsigned char)s[i + k]))
                    {
                        ok = 0;
                        break;
                    }
                    v = (v << 4) | ccb_hex_value((unsigned char)s[i + k]);
                }
                if (ok)
                {
                    i += 8;
                    unsigned char tmp[4];
                    int n = ccb_utf8_encode(v, tmp);
                    for (int k = 0; k < n; k++)
                        buf[bi++] = tmp[k];
                }
                else
                {
                    buf[bi++] = 'U';
                }
            }
            else
            {
                buf[bi++] = 'U';
            }
            break;
        }
        default:
        {
            if (ccb_is_oct((unsigned char)e))
            {
                int v = e - '0';
                int cnt = 1;
                while (cnt < 3 && i < len && ccb_is_oct((unsigned char)s[i]))
                {
                    v = (v << 3) | (s[i] - '0');
                    i++;
                    cnt++;
                }
                buf[bi++] = (unsigned char)(v & 0xFF);
            }
            else
            {
                buf[bi++] = (unsigned char)e;
            }
            break;
        }
        }
    }

    *out_len = bi;
    if (bi == 0)
        buf[0] = '\0';
    return buf;
}

static char *ccb_escape_string_literal(const char *data, int len)
{
    if (!data || len < 0)
        return NULL;

    size_t cap = (size_t)len * 4 + 16;
    char *out = (char *)malloc(cap);
    if (!out)
        return NULL;

    size_t pos = 0;
    for (int i = 0; i < len; ++i)
    {
        unsigned char ch = (unsigned char)data[i];
        const char *esc = NULL;
        switch (ch)
        {
        case '\\':
            esc = "\\\\";
            break;
        case '"':
            esc = "\\\"";
            break;
        case '\n':
            esc = "\\n";
            break;
        case '\r':
            esc = "\\r";
            break;
        case '\t':
            esc = "\\t";
            break;
        case '\0':
            esc = "\\0";
            break;
        default:
            break;
        }

        if (esc)
        {
            size_t elen = strlen(esc);
            if (pos + elen + 1 >= cap)
            {
                cap = (cap * 2) + elen + 16;
                char *grown = (char *)realloc(out, cap);
                if (!grown)
                {
                    free(out);
                    return NULL;
                }
                out = grown;
            }
            memcpy(out + pos, esc, elen);
            pos += elen;
        }
        else if (ch < 0x20 || ch >= 0x7F)
        {
            if (pos + 4 + 1 >= cap)
            {
                cap = (cap * 2) + 8;
                char *grown = (char *)realloc(out, cap);
                if (!grown)
                {
                    free(out);
                    return NULL;
                }
                out = grown;
            }
            int written = snprintf(out + pos, cap - pos, "\\x%02X", ch);
            pos += (written > 0) ? (size_t)written : 0;
        }
        else
        {
            if (pos + 2 >= cap)
            {
                cap = (cap * 2) + 8;
                char *grown = (char *)realloc(out, cap);
                if (!grown)
                {
                    free(out);
                    return NULL;
                }
                out = grown;
            }
            out[pos++] = (char)ch;
        }
    }

    if (pos + 1 >= cap)
    {
        cap = pos + 1;
        char *grown = (char *)realloc(out, cap + 1);
        if (!grown)
        {
            free(out);
            return NULL;
        }
        out = grown;
    }
    out[pos] = '\0';
    return out;
}

static bool ccb_emit_const(StringList *body, CCValueType ty, int64_t value)
{
    return string_list_appendf(body, "  const %s %lld", cc_type_name(ty), (long long)value);
}

static bool ccb_emit_const_zero(StringList *body, CCValueType ty)
{
    if (!body)
        return false;
    if (ty == CC_TYPE_PTR)
        return string_list_append(body, "  const ptr null");
    return string_list_appendf(body, "  const %s 0", cc_type_name(ty));
}

static const char *ccb_convert_kind_name(CCConvertKind kind)
{
    switch (kind)
    {
    case CC_CONVERT_TRUNC:
        return "trunc";
    case CC_CONVERT_SEXT:
        return "sext";
    case CC_CONVERT_ZEXT:
        return "zext";
    case CC_CONVERT_F2I:
        return "f2i";
    case CC_CONVERT_I2F:
        return "i2f";
    case CC_CONVERT_BITCAST:
        return "bitcast";
    default:
        return "bitcast";
    }
}

static bool ccb_emit_convert(StringList *body, CCConvertKind kind, CCValueType from_ty, CCValueType to_ty)
{
    if (!body)
        return false;
    return string_list_appendf(body, "  convert %s %s %s",
                               ccb_convert_kind_name(kind),
                               cc_type_name(from_ty),
                               cc_type_name(to_ty));
}

static bool ccb_emit_load_indirect(StringList *body, CCValueType ty)
{
    if (!body)
        return false;
    if (ty == CC_TYPE_VOID)
        return false;
    return string_list_appendf(body, "  load_indirect %s", cc_type_name(ty));
}

static bool ccb_emit_store_indirect(StringList *body, CCValueType ty)
{
    if (!body)
        return false;
    if (ty == CC_TYPE_VOID)
        return false;
    return string_list_appendf(body, "  store_indirect %s", cc_type_name(ty));
}

static int ccb_emit_condition(CcbFunctionBuilder *fb, const Node *cond)
{
    if (!fb || !cond)
        return 1;

    if (ccb_emit_expr_basic(fb, cond))
        return 1;

    CCValueType cond_ty = ccb_type_for_expr(cond);
    if (cond_ty == CC_TYPE_VOID)
    {
        diag_error_at(cond->src, cond->line, cond->col,
                      "condition has void type");
        return 1;
    }

    if (ccb_value_type_is_float(cond_ty))
    {
        if (!ccb_emit_convert(&fb->body, CC_CONVERT_F2I, cond_ty, CC_TYPE_I32))
            return 1;
        cond_ty = CC_TYPE_I32;
    }
    else if (cond_ty == CC_TYPE_PTR)
    {
        if (!ccb_emit_convert(&fb->body, CC_CONVERT_BITCAST, cond_ty, CC_TYPE_U64))
            return 1;
        cond_ty = CC_TYPE_U64;
    }
    else if (!ccb_value_type_is_integer(cond_ty))
    {
        diag_error_at(cond->src, cond->line, cond->col,
                      "unsupported condition type for if statement");
        return 1;
    }

    if (!ccb_emit_const_zero(&fb->body, cond_ty))
        return 1;

    bool is_unsigned = ccb_value_type_is_integer(cond_ty) && !ccb_value_type_is_signed(cond_ty);
    if (is_unsigned)
    {
        if (!string_list_appendf(&fb->body, "  compare ne %s unsigned", cc_type_name(cond_ty)))
            return 1;
    }
    else
    {
        if (!string_list_appendf(&fb->body, "  compare ne %s", cc_type_name(cond_ty)))
            return 1;
    }

    return 0;
}

static int ccb_emit_convert_between(CcbFunctionBuilder *fb, CCValueType from_ty, CCValueType to_ty, const Node *node)
{
    if (!fb)
        return 1;
    if (to_ty == CC_TYPE_VOID || from_ty == CC_TYPE_VOID)
        return 0;
    if (from_ty == to_ty)
        return 0;
    if (from_ty == CC_TYPE_INVALID || to_ty == CC_TYPE_INVALID)
    {
        if (node && node->src)
            diag_error_at(node->src, node->line, node->col,
                          "invalid type in conversion (%s -> %s)",
                          cc_type_name(from_ty), cc_type_name(to_ty));
        return 1;
    }

    bool from_is_int = ccb_value_type_is_integer(from_ty);
    bool to_is_int = ccb_value_type_is_integer(to_ty);
    bool from_is_float = ccb_value_type_is_float(from_ty);
    bool to_is_float = ccb_value_type_is_float(to_ty);

    bool needs_convert = true;
    CCConvertKind kind = CC_CONVERT_BITCAST;

    if (from_is_int && to_is_int)
    {
        size_t from_size = ccb_value_type_size(from_ty);
        size_t to_size = ccb_value_type_size(to_ty);
        if (from_size == to_size)
            needs_convert = false;
        else if (to_size < from_size)
            kind = CC_CONVERT_TRUNC;
        else
            kind = ccb_value_type_is_signed(from_ty) ? CC_CONVERT_SEXT : CC_CONVERT_ZEXT;
    }
    else if (from_is_float && to_is_int)
    {
        kind = CC_CONVERT_F2I;
    }
    else if (from_is_int && to_is_float)
    {
        kind = CC_CONVERT_I2F;
    }
    else if (from_is_float && to_is_float)
    {
        if (from_ty == to_ty)
            needs_convert = false;
        else
            kind = CC_CONVERT_BITCAST;
    }
    else if (from_ty == CC_TYPE_PTR && to_ty == CC_TYPE_PTR)
    {
        needs_convert = false;
    }
    else if ((from_ty == CC_TYPE_PTR && to_is_int) || (to_ty == CC_TYPE_PTR && from_is_int))
    {
        kind = CC_CONVERT_BITCAST;
    }
    else
    {
        if (node && node->src)
            diag_error_at(node->src, node->line, node->col,
                          "unsupported conversion from %s to %s in bytecode backend",
                          cc_type_name(from_ty), cc_type_name(to_ty));
        else
            fprintf(stderr, "unsupported conversion from %s to %s\n",
                    cc_type_name(from_ty), cc_type_name(to_ty));
        return 1;
    }

    if (!needs_convert)
        return 0;

    if (!ccb_emit_convert(&fb->body, kind, from_ty, to_ty))
        return 1;
    return 0;
}

static size_t ccb_type_size_bytes(const Type *ty)
{
    if (!ty)
        return 8;
    switch (ty->kind)
    {
    case TY_I8:
    case TY_U8:
    case TY_CHAR:
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
        return ty->strct.size_bytes > 0 ? (size_t)ty->strct.size_bytes : 8;
    case TY_VOID:
        return 0;
    default:
        return 8;
    }
}

static size_t ccb_pointer_elem_size(const Type *ptr_type)
{
    if (!ptr_type || ptr_type->kind != TY_PTR)
        return 1;
    const Type *elem = ptr_type->pointee;
    size_t size = ccb_type_size_bytes(elem);
    if (size == 0)
        size = 1;
    return size;
}

static int ccb_emit_pointer_add_like(CcbFunctionBuilder *fb, const Node *expr, bool is_add)
{
    if (!fb || !expr || !expr->lhs || !expr->rhs)
        return 1;

    const Node *lhs = expr->lhs;
    const Node *rhs = expr->rhs;
    Type *lhs_type = lhs->type;
    Type *rhs_type = rhs->type;
    bool lhs_is_ptr = lhs_type && lhs_type->kind == TY_PTR;
    bool rhs_is_ptr = rhs_type && rhs_type->kind == TY_PTR;
    const Node *ptr_node = lhs_is_ptr ? lhs : rhs;
    const Node *idx_node = lhs_is_ptr ? rhs : lhs;
    const Type *ptr_type = ptr_node ? ptr_node->type : NULL;
    if (!ptr_type || ptr_type->kind != TY_PTR)
    {
        diag_error_at(expr->src, expr->line, expr->col,
                      "internal error: pointer arithmetic missing pointer operand");
        return 1;
    }

    size_t elem_size = ccb_pointer_elem_size(ptr_type);

    CcbLocal *lhs_local = ccb_local_add(fb, NULL, lhs_type, false, false);
    if (!lhs_local)
        return 1;
    if (ccb_emit_expr_basic(fb, lhs))
        return 1;
    if (!ccb_emit_store_local(fb, lhs_local))
        return 1;

    CcbLocal *rhs_local = ccb_local_add(fb, NULL, rhs_type, false, false);
    if (!rhs_local)
        return 1;
    if (ccb_emit_expr_basic(fb, rhs))
        return 1;
    if (!ccb_emit_store_local(fb, rhs_local))
        return 1;

    CcbLocal *ptr_local = lhs_is_ptr ? lhs_local : rhs_local;
    CcbLocal *idx_local = lhs_is_ptr ? rhs_local : lhs_local;

    if (!ccb_value_type_is_integer(idx_local->value_type))
    {
        diag_error_at(expr->src, expr->line, expr->col,
                      "pointer arithmetic requires integer offset");
        return 1;
    }

    if (!ccb_emit_load_local(fb, ptr_local))
        return 1;
    if (ccb_emit_convert_between(fb, ptr_local->value_type, CC_TYPE_I64, ptr_node))
        return 1;

    if (!ccb_emit_load_local(fb, idx_local))
        return 1;
    if (ccb_emit_convert_between(fb, idx_local->value_type, CC_TYPE_I64, idx_node))
        return 1;

    if (elem_size > 1)
    {
        if (!ccb_emit_const(&fb->body, CC_TYPE_I64, (int64_t)elem_size))
            return 1;
        if (!string_list_appendf(&fb->body, "  binop mul %s", cc_type_name(CC_TYPE_I64)))
            return 1;
    }

    const char *op = is_add ? "add" : "sub";
    if (!string_list_appendf(&fb->body, "  binop %s %s", op, cc_type_name(CC_TYPE_I64)))
        return 1;

    if (ccb_emit_convert_between(fb, CC_TYPE_I64, CC_TYPE_PTR, expr))
        return 1;

    return 0;
}

static int ccb_emit_pointer_difference(CcbFunctionBuilder *fb, const Node *expr)
{
    if (!fb || !expr || !expr->lhs || !expr->rhs)
        return 1;

    const Node *lhs = expr->lhs;
    const Node *rhs = expr->rhs;
    Type *lhs_type = lhs->type;
    Type *rhs_type = rhs->type;
    if (!lhs_type || lhs_type->kind != TY_PTR || !rhs_type || rhs_type->kind != TY_PTR)
    {
        diag_error_at(expr->src, expr->line, expr->col,
                      "pointer subtraction requires pointer operands");
        return 1;
    }

    size_t elem_size = ccb_pointer_elem_size(lhs_type);

    CcbLocal *lhs_local = ccb_local_add(fb, NULL, lhs_type, false, false);
    if (!lhs_local)
        return 1;
    if (ccb_emit_expr_basic(fb, lhs))
        return 1;
    if (!ccb_emit_store_local(fb, lhs_local))
        return 1;

    CcbLocal *rhs_local = ccb_local_add(fb, NULL, rhs_type, false, false);
    if (!rhs_local)
        return 1;
    if (ccb_emit_expr_basic(fb, rhs))
        return 1;
    if (!ccb_emit_store_local(fb, rhs_local))
        return 1;

    if (!ccb_emit_load_local(fb, lhs_local))
        return 1;
    if (ccb_emit_convert_between(fb, lhs_local->value_type, CC_TYPE_I64, lhs))
        return 1;

    if (!ccb_emit_load_local(fb, rhs_local))
        return 1;
    if (ccb_emit_convert_between(fb, rhs_local->value_type, CC_TYPE_I64, rhs))
        return 1;

    if (!string_list_appendf(&fb->body, "  binop sub %s", cc_type_name(CC_TYPE_I64)))
        return 1;

    if (elem_size > 1)
    {
        if (!ccb_emit_const(&fb->body, CC_TYPE_I64, (int64_t)elem_size))
            return 1;
        if (!string_list_appendf(&fb->body, "  binop div %s", cc_type_name(CC_TYPE_I64)))
            return 1;
    }

    return 0;
}

static int ccb_emit_index_address(CcbFunctionBuilder *fb, const Node *expr, CCValueType *out_elem_ty, const Type **out_elem_type)
{
    if (!fb || !expr || expr->kind != ND_INDEX)
        return 1;

    const Node *base = expr->lhs;
    const Node *index = expr->rhs;
    if (!base || !index)
    {
        diag_error_at(expr->src, expr->line, expr->col,
                      "invalid index expression");
        return 1;
    }

    if (ccb_emit_expr_basic(fb, base))
        return 1;

    const Type *base_type = base->type;
    CCValueType base_ty = ccb_type_for_expr(base);
    bool base_is_pointer = false;
    if (base_type && base_type->kind == TY_PTR)
    {
        base_is_pointer = true;
        base_ty = CC_TYPE_PTR;
    }
    else if (base_ty == CC_TYPE_PTR)
    {
        base_is_pointer = true;
    }
    else if (base->kind == ND_VAR && base->var_ref)
    {
        CcbLocal *base_local = ccb_local_lookup(fb, base->var_ref);
        if (base_local && base_local->value_type == CC_TYPE_PTR)
        {
            base_is_pointer = true;
            base_ty = CC_TYPE_PTR;
            if (!base_type)
                base_type = base_local->type;
        }
    }

    if (!base_is_pointer)
    {
        diag_error_at(base->src, base->line, base->col,
                      "index base is not a pointer");
        return 1;
    }

    if (ccb_emit_convert_between(fb, base_ty, CC_TYPE_I64, base))
        return 1;

    if (ccb_emit_expr_basic(fb, index))
        return 1;

    CCValueType idx_ty = ccb_type_for_expr(index);
    if (!ccb_value_type_is_integer(idx_ty))
    {
        diag_error_at(index->src, index->line, index->col,
                      "array index must be an integer");
        return 1;
    }

    if (ccb_emit_convert_between(fb, idx_ty, CC_TYPE_I64, index))
        return 1;

    const Type *elem_type = NULL;
    if (base_type && base_type->kind == TY_PTR)
        elem_type = base_type->pointee;

    CCValueType elem_cc_ty = map_type_to_cc(elem_type);
    if (elem_type && elem_type->kind == TY_STRUCT) {
        // Struct: use struct type
        // (leave elem_cc_ty as mapped)
    } else if (elem_cc_ty == CC_TYPE_INVALID || elem_cc_ty == CC_TYPE_VOID) {
        elem_cc_ty = CC_TYPE_I32;
    }

    size_t elem_size = elem_type ? ccb_type_size_bytes(elem_type) : ccb_value_type_size(elem_cc_ty);
    if (elem_size == 0)
        elem_size = 1;

    if (elem_size > 1)
    {
        if (!ccb_emit_const(&fb->body, CC_TYPE_I64, (int64_t)elem_size))
            return 1;
        if (!string_list_appendf(&fb->body, "  binop mul %s", cc_type_name(CC_TYPE_I64)))
            return 1;
    }

    if (!string_list_appendf(&fb->body, "  binop add %s", cc_type_name(CC_TYPE_I64)))
        return 1;

    if (ccb_emit_convert_between(fb, CC_TYPE_I64, CC_TYPE_PTR, expr))
        return 1;

    if (out_elem_ty)
        *out_elem_ty = elem_cc_ty;
    if (out_elem_type)
        *out_elem_type = elem_type;
    return 0;
}

static int ccb_emit_member_address(CcbFunctionBuilder *fb, const Node *expr, CCValueType *out_field_ty, const Type **out_field_type)
{
    if (!fb || !expr || expr->kind != ND_MEMBER)
        return 1;

    const Node *base = expr->lhs;
    if (!base)
    {
        diag_error_at(expr->src, expr->line, expr->col,
                      "member expression missing base");
        return 1;
    }

    if (ccb_emit_expr_basic(fb, base))
        return 1;

    const Type *struct_type = NULL;
    if (expr->is_pointer_deref)
    {
        if (!base->type || base->type->kind != TY_PTR || !base->type->pointee)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "pointer member access requires pointer-to-struct base");
            return 1;
        }
        struct_type = base->type->pointee;
    }
    else
    {
        struct_type = base->type;
    }

    if (!struct_type || struct_type->kind != TY_STRUCT)
    {
        diag_error_at(expr->src, expr->line, expr->col,
                      "member access requires struct type");
        return 1;
    }

    int field_index = expr->field_index;
    if (field_index < 0 || field_index >= struct_type->strct.field_count)
    {
        diag_error_at(expr->src, expr->line, expr->col,
                      "invalid field index %d", field_index);
        return 1;
    }

    int field_offset = struct_type->strct.field_offsets ? struct_type->strct.field_offsets[field_index] : expr->field_offset;
    const Type *field_type = struct_type->strct.field_types ? struct_type->strct.field_types[field_index] : NULL;

    if (!field_type)
    {
        diag_error_at(expr->src, expr->line, expr->col,
                      "unknown field type for member access");
        return 1;
    }

    if (field_offset != 0)
    {
        if (ccb_emit_convert_between(fb, CC_TYPE_PTR, CC_TYPE_I64, expr))
            return 1;
        if (!ccb_emit_const(&fb->body, CC_TYPE_I64, (int64_t)field_offset))
            return 1;
        if (!string_list_appendf(&fb->body, "  binop add %s", cc_type_name(CC_TYPE_I64)))
            return 1;
        if (ccb_emit_convert_between(fb, CC_TYPE_I64, CC_TYPE_PTR, expr))
            return 1;
    }

    if (out_field_ty)
        *out_field_ty = map_type_to_cc(field_type);
    if (out_field_type)
        *out_field_type = field_type;
    return 0;
}

static bool type_is_address_only(const Type *ty)
{
    return ty && ty->kind == TY_STRUCT;
}

static int ccb_emit_pointer_offset(CcbFunctionBuilder *fb, int offset, const Node *node)
{
    if (!fb)
        return 1;
    if (offset == 0)
        return 0;
    if (ccb_emit_convert_between(fb, CC_TYPE_PTR, CC_TYPE_I64, node))
        return 1;
    if (!ccb_emit_const(&fb->body, CC_TYPE_I64, (int64_t)offset))
        return 1;
    if (!string_list_appendf(&fb->body, "  binop add %s", cc_type_name(CC_TYPE_I64)))
        return 1;
    if (ccb_emit_convert_between(fb, CC_TYPE_I64, CC_TYPE_PTR, node))
        return 1;
    return 0;
}

static int ccb_emit_struct_copy(CcbFunctionBuilder *fb, const Type *struct_type, CcbLocal *dst_ptr, CcbLocal *src_ptr)
{
    if (!fb || !struct_type || struct_type->kind != TY_STRUCT || !dst_ptr || !src_ptr)
        return 0;

    int field_count = struct_type->strct.field_count;
    for (int i = 0; i < field_count; ++i)
    {
        const Type *field_type = struct_type->strct.field_types ? struct_type->strct.field_types[i] : NULL;
        if (!field_type)
            continue;

        int field_offset = struct_type->strct.field_offsets ? struct_type->strct.field_offsets[i] : 0;

        if (type_is_address_only(field_type))
        {
            Type *field_ptr_ty = type_ptr((Type *)field_type);

            CcbLocal *dst_field = ccb_local_add(fb, NULL, field_ptr_ty, false, false);
            if (!dst_field)
                return 1;
            if (!ccb_emit_load_local(fb, dst_ptr))
                return 1;
            if (ccb_emit_pointer_offset(fb, field_offset, NULL))
                return 1;
            if (!ccb_emit_store_local(fb, dst_field))
                return 1;

            CcbLocal *src_field = ccb_local_add(fb, NULL, field_ptr_ty, false, false);
            if (!src_field)
                return 1;
            if (!ccb_emit_load_local(fb, src_ptr))
                return 1;
            if (ccb_emit_pointer_offset(fb, field_offset, NULL))
                return 1;
            if (!ccb_emit_store_local(fb, src_field))
                return 1;

            if (ccb_emit_struct_copy(fb, field_type, dst_field, src_field))
                return 1;
            continue;
        }

        CCValueType field_cc_ty = map_type_to_cc(field_type);
        if (!ccb_emit_load_local(fb, src_ptr))
            return 1;
        if (ccb_emit_pointer_offset(fb, field_offset, NULL))
            return 1;
        if (!ccb_emit_load_indirect(&fb->body, field_cc_ty))
            return 1;

        CcbLocal *value_tmp = ccb_local_add(fb, NULL, (Type *)field_type, false, false);
        if (!value_tmp)
            return 1;
        if (!ccb_emit_store_local(fb, value_tmp))
            return 1;

        if (!ccb_emit_load_local(fb, dst_ptr))
            return 1;
        if (ccb_emit_pointer_offset(fb, field_offset, NULL))
            return 1;
        if (!ccb_emit_load_local(fb, value_tmp))
            return 1;
        if (!ccb_emit_store_indirect(&fb->body, field_cc_ty))
            return 1;
    }
    return 0;
}

static int ccb_emit_struct_zero(CcbFunctionBuilder *fb, const Node *var_decl, const char *var_name, const Type *struct_type)
{
    if (!fb || !var_name || !struct_type || struct_type->kind != TY_STRUCT)
        return 0;

    Node var_ref = {0};
    var_ref.kind = ND_VAR;
    var_ref.var_ref = var_name;
    var_ref.type = (Type *)struct_type;
    var_ref.src = var_decl ? var_decl->src : NULL;
    var_ref.line = var_decl ? var_decl->line : 0;
    var_ref.col = var_decl ? var_decl->col : 0;

    for (int i = 0; i < struct_type->strct.field_count; ++i)
    {
        Node member = {0};
        member.kind = ND_MEMBER;
        member.lhs = &var_ref;
        member.field_index = i;
        member.field_offset = struct_type->strct.field_offsets ? struct_type->strct.field_offsets[i] : 0;
        member.type = struct_type->strct.field_types ? struct_type->strct.field_types[i] : NULL;
        member.is_pointer_deref = 0;
        member.src = var_decl ? var_decl->src : NULL;
        member.line = var_decl ? var_decl->line : 0;
        member.col = var_decl ? var_decl->col : 0;

        CCValueType field_ty = CC_TYPE_I32;
        if (ccb_emit_member_address(fb, &member, &field_ty, NULL))
            return 1;
        if (!ccb_emit_const_zero(&fb->body, field_ty))
            return 1;
        if (!ccb_emit_store_indirect(&fb->body, field_ty))
            return 1;
    }
    return 0;
}

static int ccb_emit_struct_initializer(CcbFunctionBuilder *fb, const Node *var_decl, const char *var_name, const Type *struct_type, const Node *init)
{
    if (!fb || !var_name || !struct_type || struct_type->kind != TY_STRUCT)
        return 0;
    if (!init)
        return 0;

    Node var_ref = {0};
    var_ref.kind = ND_VAR;
    var_ref.var_ref = var_name;
    var_ref.type = (Type *)struct_type;
    var_ref.src = var_decl ? var_decl->src : NULL;
    var_ref.line = var_decl ? var_decl->line : 0;
    var_ref.col = var_decl ? var_decl->col : 0;

    if (init->kind != ND_INIT_LIST)
    {
        if (!init->type || init->type->kind != TY_STRUCT)
        {
            diag_error_at(init->src, init->line, init->col,
                          "unsupported initializer for struct local");
            return 1;
        }

        int field_count = struct_type->strct.field_count;
        for (int i = 0; i < field_count; ++i)
        {
            Node member = {0};
            member.kind = ND_MEMBER;
            member.lhs = &var_ref;
            member.field_index = i;
            member.field_offset = struct_type->strct.field_offsets ? struct_type->strct.field_offsets[i] : 0;
            member.type = struct_type->strct.field_types ? struct_type->strct.field_types[i] : NULL;
            member.is_pointer_deref = 0;
            member.src = var_decl ? var_decl->src : NULL;
            member.line = var_decl ? var_decl->line : 0;
            member.col = var_decl ? var_decl->col : 0;

            CCValueType field_ty = CC_TYPE_I32;
            const Type *field_type = NULL;
            if (ccb_emit_member_address(fb, &member, &field_ty, &field_type))
                return 1;

            if (!field_type)
            {
                diag_error_at(init->src, init->line, init->col,
                              "unknown field type for struct copy");
                return 1;
            }

            Node src_member = {0};
            src_member.kind = ND_MEMBER;
            src_member.lhs = (Node *)init;
            src_member.field_index = i;
            src_member.field_offset = struct_type->strct.field_offsets ? struct_type->strct.field_offsets[i] : 0;
            src_member.type = (Type *)field_type;
            src_member.is_pointer_deref = 0;
            src_member.src = init->src;
            src_member.line = init->line;
            src_member.col = init->col;

            if (ccb_emit_expr_basic(fb, &src_member))
                return 1;

            CCValueType value_ty = ccb_type_for_expr(&src_member);
            if (value_ty == CC_TYPE_INVALID)
                value_ty = map_type_to_cc(field_type);

            if (ccb_emit_convert_between(fb, value_ty, field_ty, &src_member))
                return 1;

            if (!ccb_emit_store_indirect(&fb->body, field_ty))
                return 1;
        }
        return 0;
    }

    if (init->init.is_zero)
    {
        return ccb_emit_struct_zero(fb, var_decl, var_name, struct_type);
    }

    if (ccb_emit_struct_zero(fb, var_decl, var_name, struct_type))
        return 1;

    for (int i = 0; i < init->init.count; ++i)
    {
        int field_index = init->init.field_indices ? init->init.field_indices[i] : i;
        if (field_index < 0 || field_index >= struct_type->strct.field_count)
        {
            diag_error_at(init->src, init->line, init->col,
                          "initializer references invalid field index %d", field_index);
            return 1;
        }

        Node member = {0};
        member.kind = ND_MEMBER;
        member.lhs = &var_ref;
        member.field_index = field_index;
        member.field_offset = struct_type->strct.field_offsets ? struct_type->strct.field_offsets[field_index] : 0;
        member.type = struct_type->strct.field_types ? struct_type->strct.field_types[field_index] : NULL;
        member.is_pointer_deref = 0;
        member.src = init->src;
        member.line = init->line;
        member.col = init->col;

        CCValueType field_ty = CC_TYPE_I32;
        if (ccb_emit_member_address(fb, &member, &field_ty, NULL))
            return 1;

        const Node *value = init->init.elems ? init->init.elems[i] : NULL;
        if (ccb_emit_expr_basic(fb, value))
            return 1;

        CCValueType value_ty = ccb_type_for_expr(value);
        if (value_ty == CC_TYPE_INVALID)
            value_ty = map_type_to_cc(value ? value->type : NULL);

        if (ccb_emit_convert_between(fb, value_ty, field_ty, value))
            return 1;

        if (!ccb_emit_store_indirect(&fb->body, field_ty))
            return 1;
    }

    return 0;
}

static bool ccb_emit_load_local(CcbFunctionBuilder *fb, const CcbLocal *local)
{
    if (!fb || !local)
        return false;
    if (local->is_param)
        return string_list_appendf(&fb->body, "  load_param %d", local->index);
    return string_list_appendf(&fb->body, "  load_local %d", local->index);
}

static bool ccb_emit_store_local(CcbFunctionBuilder *fb, const CcbLocal *local)
{
    if (!fb || !local)
        return false;
    if (local->is_param)
        return false;
    return string_list_appendf(&fb->body, "  store_local %d", local->index);
}

static int ccb_emit_expr_basic(CcbFunctionBuilder *fb, const Node *expr)
{
    if (!fb || !expr)
        return 1;

    switch (expr->kind)
    {
    case ND_INT:
    {
        CCValueType ty = map_type_to_cc(expr->type);
        if (!ccb_emit_const(&fb->body, ty, expr->int_val))
            return 1;
        return 0;
    }
    case ND_ADD:
    {
        if (!expr->lhs || !expr->rhs)
        {
            diag_error_at(expr->src, expr->line, expr->col, "addition missing operand");
            return 1;
        }

        Type *lhs_type = expr->lhs->type;
        Type *rhs_type = expr->rhs->type;
        bool lhs_is_ptr = lhs_type && lhs_type->kind == TY_PTR;
        bool rhs_is_ptr = rhs_type && rhs_type->kind == TY_PTR;
        CCValueType lhs_cc = ccb_type_for_expr(expr->lhs);
        CCValueType rhs_cc = ccb_type_for_expr(expr->rhs);
        bool lhs_is_int = ccb_value_type_is_integer(lhs_cc);
        bool rhs_is_int = ccb_value_type_is_integer(rhs_cc);

        if ((lhs_is_ptr && rhs_is_int) || (rhs_is_ptr && lhs_is_int))
            return ccb_emit_pointer_add_like(fb, expr, true);

        if (lhs_is_ptr || rhs_is_ptr)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "pointer addition requires exactly one pointer and one integer");
            return 1;
        }

        if (ccb_emit_expr_basic(fb, expr->lhs))
            return 1;
        if (ccb_emit_expr_basic(fb, expr->rhs))
            return 1;
        CCValueType ty = map_type_to_cc(expr->type ? expr->type : lhs_type);
        if (!string_list_appendf(&fb->body, "  binop add %s", cc_type_name(ty)))
            return 1;
        return 0;
    }
    case ND_MUL:
    {
        if (ccb_emit_expr_basic(fb, expr->lhs))
            return 1;
        if (ccb_emit_expr_basic(fb, expr->rhs))
            return 1;
        CCValueType ty = map_type_to_cc(expr->type ? expr->type : (expr->lhs ? expr->lhs->type : NULL));
        if (!string_list_appendf(&fb->body, "  binop mul %s", cc_type_name(ty)))
            return 1;
        return 0;
    }
    case ND_DIV:
    {
        if (ccb_emit_expr_basic(fb, expr->lhs))
            return 1;
        if (ccb_emit_expr_basic(fb, expr->rhs))
            return 1;
        CCValueType ty = map_type_to_cc(expr->type ? expr->type : (expr->lhs ? expr->lhs->type : NULL));
        bool is_unsigned = ccb_value_type_is_integer(ty) && !ccb_value_type_is_signed(ty);
        if (is_unsigned)
        {
            if (!string_list_appendf(&fb->body, "  binop div %s unsigned", cc_type_name(ty)))
                return 1;
        }
        else
        {
            if (!string_list_appendf(&fb->body, "  binop div %s", cc_type_name(ty)))
                return 1;
        }
        return 0;
    }
    case ND_SUB:
    {
        if (!expr->lhs || !expr->rhs)
        {
            diag_error_at(expr->src, expr->line, expr->col, "subtraction missing operand");
            return 1;
        }

        Type *lhs_type = expr->lhs->type;
        Type *rhs_type = expr->rhs->type;
        bool lhs_is_ptr = lhs_type && lhs_type->kind == TY_PTR;
        bool rhs_is_ptr = rhs_type && rhs_type->kind == TY_PTR;
        CCValueType lhs_cc = ccb_type_for_expr(expr->lhs);
        CCValueType rhs_cc = ccb_type_for_expr(expr->rhs);
        bool lhs_is_int = ccb_value_type_is_integer(lhs_cc);
        bool rhs_is_int = ccb_value_type_is_integer(rhs_cc);

        if (lhs_is_ptr && rhs_is_ptr)
            return ccb_emit_pointer_difference(fb, expr);

        if (lhs_is_ptr && rhs_is_int)
            return ccb_emit_pointer_add_like(fb, expr, false);

        if (rhs_is_ptr)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "cannot subtract a pointer from a non-pointer");
            return 1;
        }

        if (ccb_emit_expr_basic(fb, expr->lhs))
            return 1;
        if (ccb_emit_expr_basic(fb, expr->rhs))
            return 1;
        CCValueType ty = map_type_to_cc(expr->type ? expr->type : lhs_type);
        if (!string_list_appendf(&fb->body, "  binop sub %s", cc_type_name(ty)))
            return 1;
        return 0;
    }
    case ND_SHL:
    case ND_SHR:
    {
        if (ccb_emit_expr_basic(fb, expr->lhs))
            return 1;
        if (ccb_emit_expr_basic(fb, expr->rhs))
            return 1;
        CCValueType ty = map_type_to_cc(expr->type ? expr->type : (expr->lhs ? expr->lhs->type : NULL));
        const char *op = expr->kind == ND_SHL ? "shl" : "shr";
        bool is_unsigned = ccb_value_type_is_integer(ty) && !ccb_value_type_is_signed(ty);
        if (expr->kind == ND_SHR && is_unsigned)
        {
            if (!string_list_appendf(&fb->body, "  binop %s %s unsigned", op, cc_type_name(ty)))
                return 1;
        }
        else
        {
            if (!string_list_appendf(&fb->body, "  binop %s %s", op, cc_type_name(ty)))
                return 1;
        }
        return 0;
    }
    case ND_PREINC:
    case ND_PREDEC:
    {
        const Node *target = expr->lhs;
        if (!target || target->kind != ND_VAR || !target->var_ref)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "operand of %s must be a variable",
                          expr->kind == ND_PREINC ? "++" : "--");
            return 1;
        }

        CcbLocal *local = ccb_local_lookup(fb, target->var_ref);
        if (!local)
        {
            diag_error_at(target->src, target->line, target->col,
                          "unknown local '%s'", target->var_ref);
            return 1;
        }
        if (local->is_param)
        {
            diag_error_at(target->src, target->line, target->col,
                          "%s of parameter '%s' not supported yet",
                          expr->kind == ND_PREINC ? "increment" : "decrement",
                          target->var_ref);
            return 1;
        }

        CCValueType val_ty = local->value_type;
        bool is_ptr = (val_ty == CC_TYPE_PTR);
        if (!ccb_value_type_is_integer(val_ty) && !is_ptr)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "%s only supported on integer or pointer locals",
                          expr->kind == ND_PREINC ? "pre-increment" : "pre-decrement");
            return 1;
        }

        size_t elem_size = is_ptr ? ccb_pointer_elem_size(local->type) : 1;
        const char *op = expr->kind == ND_PREINC ? "add" : "sub";

        if (is_ptr)
        {
            if (!ccb_emit_load_local(fb, local))
                return 1;
            if (ccb_emit_convert_between(fb, val_ty, CC_TYPE_I64, expr))
                return 1;
            if (!ccb_emit_const(&fb->body, CC_TYPE_I64, (int64_t)elem_size))
                return 1;
            if (!string_list_appendf(&fb->body, "  binop %s %s", op, cc_type_name(CC_TYPE_I64)))
                return 1;
            if (ccb_emit_convert_between(fb, CC_TYPE_I64, CC_TYPE_PTR, expr))
                return 1;
        }
        else
        {
            if (!ccb_emit_load_local(fb, local))
                return 1;
            if (!ccb_emit_const(&fb->body, val_ty, 1))
                return 1;
            if (!string_list_appendf(&fb->body, "  binop %s %s", op, cc_type_name(val_ty)))
                return 1;
        }

        if (!ccb_emit_store_local(fb, local))
            return 1;
        if (!ccb_emit_load_local(fb, local))
            return 1;

        return 0;
    }
    case ND_POSTINC:
    case ND_POSTDEC:
    {
        const Node *target = expr->lhs;
        if (!target || target->kind != ND_VAR || !target->var_ref)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "operand of %s must be a variable",
                          expr->kind == ND_POSTINC ? "post-increment" : "post-decrement");
            return 1;
        }

        CcbLocal *local = ccb_local_lookup(fb, target->var_ref);
        if (!local)
        {
            diag_error_at(target->src, target->line, target->col,
                          "unknown local '%s'", target->var_ref);
            return 1;
        }
        if (local->is_param)
        {
            diag_error_at(target->src, target->line, target->col,
                          "%s of parameter '%s' not supported yet",
                          expr->kind == ND_POSTINC ? "increment" : "decrement",
                          target->var_ref);
            return 1;
        }

        CCValueType val_ty = local->value_type;
        bool is_ptr = (val_ty == CC_TYPE_PTR);
        if (!ccb_value_type_is_integer(val_ty) && !is_ptr)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "%s only supported on integer or pointer locals",
                          expr->kind == ND_POSTINC ? "post-increment" : "post-decrement");
            return 1;
        }

        size_t elem_size = is_ptr ? ccb_pointer_elem_size(local->type) : 1;
        const char *op = expr->kind == ND_POSTINC ? "add" : "sub";

        if (!ccb_emit_load_local(fb, local))
            return 1;

        CcbLocal *temp = ccb_local_add(fb, NULL, local->type, false, false);
        if (!temp)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "failed to allocate temporary for %s",
                          expr->kind == ND_POSTINC ? "post-increment" : "post-decrement");
            return 1;
        }

        if (!ccb_emit_store_local(fb, temp))
            return 1;

        if (!ccb_emit_load_local(fb, local))
            return 1;
        if (is_ptr)
        {
            if (ccb_emit_convert_between(fb, val_ty, CC_TYPE_I64, expr))
                return 1;
            if (!ccb_emit_const(&fb->body, CC_TYPE_I64, (int64_t)elem_size))
                return 1;
            if (!string_list_appendf(&fb->body, "  binop %s %s", op, cc_type_name(CC_TYPE_I64)))
                return 1;
            if (ccb_emit_convert_between(fb, CC_TYPE_I64, CC_TYPE_PTR, expr))
                return 1;
        }
        else
        {
            if (!ccb_emit_const(&fb->body, val_ty, 1))
                return 1;
            if (!string_list_appendf(&fb->body, "  binop %s %s", op, cc_type_name(val_ty)))
                return 1;
        }

        if (!ccb_emit_store_local(fb, local))
            return 1;

        if (!ccb_emit_load_local(fb, temp))
            return 1;

        return 0;
    }
    case ND_STRING:
    {
        if (!expr->str_data)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "string literal missing data");
            return 1;
        }
        int cooked_len = 0;
        unsigned char *cooked = ccb_decode_c_escapes(expr->str_data, expr->str_len, &cooked_len);
        if (!cooked)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "failed to decode string literal");
            return 1;
        }
        char *escaped = ccb_escape_string_literal((const char *)cooked, cooked_len);
        free(cooked);
        if (!escaped)
            return 1;
        bool ok = string_list_appendf(&fb->body, "  const_str \"%s\"", escaped);
        free(escaped);
        return ok ? 0 : 1;
    }
    case ND_GT_EXPR:
    case ND_LT:
    case ND_LE:
    case ND_GE:
    case ND_EQ:
    case ND_NE:
    {
        if (!expr->lhs || !expr->rhs)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "comparison missing operand");
            return 1;
        }

        if (ccb_emit_expr_basic(fb, expr->lhs))
            return 1;
        if (ccb_emit_expr_basic(fb, expr->rhs))
            return 1;

        CCValueType lhs_ty = ccb_type_for_expr(expr->lhs);
        CCValueType rhs_ty = ccb_type_for_expr(expr->rhs);

        if (lhs_ty == CC_TYPE_INVALID || rhs_ty == CC_TYPE_INVALID)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "invalid operand type for comparison");
            return 1;
        }

        bool lhs_is_ptr = lhs_ty == CC_TYPE_PTR;
        bool rhs_is_ptr = rhs_ty == CC_TYPE_PTR;
        bool lhs_is_int = ccb_value_type_is_integer(lhs_ty);
        bool rhs_is_int = ccb_value_type_is_integer(rhs_ty);
        bool lhs_is_float = ccb_value_type_is_float(lhs_ty);
        bool rhs_is_float = ccb_value_type_is_float(rhs_ty);

        if (!((lhs_is_int && rhs_is_int) || (lhs_is_float && rhs_is_float) || (lhs_is_ptr && rhs_is_ptr)))
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "operands to comparison must both be integers, floats, or pointers");
            return 1;
        }

        CCValueType op_ty = lhs_ty;
        if (lhs_is_float && !rhs_is_float)
            op_ty = lhs_ty;
        else if (!lhs_is_float && rhs_is_float)
            op_ty = rhs_ty;

        const char *cmp_op = NULL;
        switch (expr->kind)
        {
        case ND_GT_EXPR:
            cmp_op = "gt";
            break;
        case ND_LT:
            cmp_op = "lt";
            break;
        case ND_LE:
            cmp_op = "le";
            break;
        case ND_GE:
            cmp_op = "ge";
            break;
        case ND_EQ:
            cmp_op = "eq";
            break;
        case ND_NE:
            cmp_op = "ne";
            break;
        default:
            cmp_op = "eq";
            break;
        }

        bool is_unsigned = ccb_value_type_is_integer(op_ty) && !ccb_value_type_is_signed(op_ty);
        if (is_unsigned)
        {
            if (!string_list_appendf(&fb->body, "  compare %s %s unsigned", cmp_op, cc_type_name(op_ty)))
                return 1;
        }
        else
        {
            if (!string_list_appendf(&fb->body, "  compare %s %s", cmp_op, cc_type_name(op_ty)))
                return 1;
        }
        return 0;
    }
    case ND_LAND:
    {
        if (!expr->lhs || !expr->rhs)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "logical AND missing operand");
            return 1;
        }

        if (ccb_emit_condition(fb, expr->lhs))
            return 1;

        char rhs_label[32];
        char false_label[32];
        char true_label[32];
        char end_label[32];

        ccb_make_label(fb, rhs_label, sizeof(rhs_label), "land_rhs");
        ccb_make_label(fb, false_label, sizeof(false_label), "land_false");
        ccb_make_label(fb, true_label, sizeof(true_label), "land_true");
        ccb_make_label(fb, end_label, sizeof(end_label), "land_end");

        if (!string_list_appendf(&fb->body, "  branch %s %s", rhs_label, false_label))
            return 1;

        if (!string_list_appendf(&fb->body, "label %s", rhs_label))
            return 1;
        if (ccb_emit_condition(fb, expr->rhs))
            return 1;
        if (!string_list_appendf(&fb->body, "  branch %s %s", true_label, false_label))
            return 1;

        if (!string_list_appendf(&fb->body, "label %s", true_label))
            return 1;
        if (!ccb_emit_const(&fb->body, CC_TYPE_I32, 1))
            return 1;
        if (!string_list_appendf(&fb->body, "  jump %s", end_label))
            return 1;

        if (!string_list_appendf(&fb->body, "label %s", false_label))
            return 1;
        if (!ccb_emit_const_zero(&fb->body, CC_TYPE_I32))
            return 1;

        if (!string_list_appendf(&fb->body, "label %s", end_label))
            return 1;
        return 0;
    }
    case ND_LOR:
    {
        if (!expr->lhs || !expr->rhs)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "logical OR missing operand");
            return 1;
        }

        if (ccb_emit_condition(fb, expr->lhs))
            return 1;

        char true_label[32];
        char rhs_label[32];
        char false_label[32];
        char end_label[32];

        ccb_make_label(fb, true_label, sizeof(true_label), "lor_true");
        ccb_make_label(fb, rhs_label, sizeof(rhs_label), "lor_rhs");
        ccb_make_label(fb, false_label, sizeof(false_label), "lor_false");
        ccb_make_label(fb, end_label, sizeof(end_label), "lor_end");

        if (!string_list_appendf(&fb->body, "  branch %s %s", true_label, rhs_label))
            return 1;

        if (!string_list_appendf(&fb->body, "label %s", rhs_label))
            return 1;
        if (ccb_emit_condition(fb, expr->rhs))
            return 1;
        if (!string_list_appendf(&fb->body, "  branch %s %s", true_label, false_label))
            return 1;

        if (!string_list_appendf(&fb->body, "label %s", true_label))
            return 1;
        if (!ccb_emit_const(&fb->body, CC_TYPE_I32, 1))
            return 1;
        if (!string_list_appendf(&fb->body, "  jump %s", end_label))
            return 1;

        if (!string_list_appendf(&fb->body, "label %s", false_label))
            return 1;
        if (!ccb_emit_const_zero(&fb->body, CC_TYPE_I32))
            return 1;

        if (!string_list_appendf(&fb->body, "label %s", end_label))
            return 1;
        return 0;
    }
    case ND_INDEX:
    {
        CCValueType elem_ty = CC_TYPE_I32;
        const Type *elem_type = NULL;
        if (ccb_emit_index_address(fb, expr, &elem_ty, &elem_type))
            return 1;

        if (elem_type && elem_type->kind == TY_STRUCT)
        {
            // Address of struct element is already on the stack
            return 0;
        }

        if (!ccb_emit_load_indirect(&fb->body, elem_ty))
            return 1;

        CCValueType result_ty = map_type_to_cc(expr->type);
        if (result_ty == CC_TYPE_INVALID)
            result_ty = elem_ty;

        if (result_ty != CC_TYPE_VOID && result_ty != elem_ty)
        {
            if (ccb_emit_convert_between(fb, elem_ty, result_ty, expr))
                return 1;
        }
        return 0;
    }
    case ND_MEMBER:
    {
        CCValueType field_ty = CC_TYPE_I32;
        const Type *field_type = NULL;
        if (ccb_emit_member_address(fb, expr, &field_ty, &field_type))
            return 1;

        if (field_type && field_type->kind == TY_STRUCT)
        {
            // Struct fields are handled by address; caller will copy as needed
            return 0;
        }

        if (!ccb_emit_load_indirect(&fb->body, field_ty))
            return 1;

        CCValueType result_ty = map_type_to_cc(expr->type);
        if (result_ty == CC_TYPE_INVALID)
            result_ty = field_ty;

        if (result_ty != CC_TYPE_VOID && result_ty != field_ty)
        {
            if (ccb_emit_convert_between(fb, field_ty, result_ty, expr))
                return 1;
        }
        return 0;
    }
    case ND_SIZEOF:
    {
        CCValueType ty = map_type_to_cc(expr->type);
        if (ty == CC_TYPE_INVALID || ty == CC_TYPE_VOID)
            ty = CC_TYPE_U64;
        if (!ccb_emit_const(&fb->body, ty, expr->int_val))
            return 1;
        return 0;
    }
    case ND_ADDR:
    {
        if (!expr->lhs)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "address-of operator missing operand");
            return 1;
        }

        const Node *operand = expr->lhs;
        switch (operand->kind)
        {
        case ND_VAR:
        {
            if (!operand->var_ref)
            {
                diag_error_at(operand->src, operand->line, operand->col,
                              "address-of target missing name");
                return 1;
            }
            CcbLocal *local = ccb_local_lookup(fb, operand->var_ref);
            if (!local)
            {
                diag_error_at(operand->src, operand->line, operand->col,
                              "unknown local '%s'", operand->var_ref);
                return 1;
            }
            if (local->is_param)
            {
                if (!string_list_appendf(&fb->body, "  addr_param %d", local->index))
                    return 1;
                return 0;
            }
            if (local->is_address_only)
            {
                if (!ccb_emit_load_local(fb, local))
                    return 1;
                return 0;
            }
            if (!string_list_appendf(&fb->body, "  addr_local %d", local->index))
                return 1;
            return 0;
        }
        case ND_INDEX:
        {
            if (ccb_emit_index_address(fb, operand, NULL, NULL))
                return 1;
            return 0;
        }
        case ND_MEMBER:
        {
            if (ccb_emit_member_address(fb, operand, NULL, NULL))
                return 1;
            return 0;
        }
        default:
            diag_error_at(operand->src, operand->line, operand->col,
                          "address-of not supported for %s", node_kind_name(operand->kind));
            return 1;
        }
    }
    case ND_VAR:
    {
        if (!expr->var_ref)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "variable reference missing name");
            return 1;
        }
        CcbLocal *local = ccb_local_lookup(fb, expr->var_ref);
        if (!local)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "unknown local '%s'", expr->var_ref);
            return 1;
        }
        if (!ccb_emit_load_local(fb, local))
            return 1;
        return 0;
    }
    case ND_CALL:
    {
        if (!expr->call_name || !*expr->call_name)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "function call missing symbol name");
            return 1;
        }

        CCValueType *arg_types = NULL;
        if (expr->arg_count > 0)
        {
            arg_types = (CCValueType *)xcalloc((size_t)expr->arg_count, sizeof(CCValueType));
        }

        int rc = 0;
        for (int i = 0; i < expr->arg_count; ++i)
        {
            const Node *arg = expr->args ? expr->args[i] : NULL;
            if (ccb_emit_expr_basic(fb, arg))
            {
                rc = 1;
                break;
            }
            if (arg_types)
                arg_types[i] = ccb_type_for_expr(arg);
        }

        if (!rc)
        {
            CCValueType ret_ty = ccb_type_for_expr(expr);
            char *arg_text = NULL;
            if (!ccb_format_type_list(arg_types, (size_t)expr->arg_count, &arg_text))
            {
                rc = 1;
            }
            else
            {
                if (!string_list_appendf(&fb->body, "  call %s %s %s", expr->call_name,
                                         cc_type_name(ret_ty), arg_text))
                {
                    rc = 1;
                }
            }
            free(arg_text);
        }

        free(arg_types);
        return rc;
    }
    case ND_ASSIGN:
    {
        if (!expr->lhs || !expr->rhs)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "assignment missing operand");
            return 1;
        }

        const Node *target = expr->lhs;
        switch (target->kind)
        {
        case ND_VAR:
        {
            if (!target->var_ref)
            {
                diag_error_at(target->src, target->line, target->col,
                              "assignment target missing name");
                return 1;
            }
            CcbLocal *local = ccb_local_lookup(fb, target->var_ref);
            if (!local)
            {
                diag_error_at(target->src, target->line, target->col,
                              "unknown local '%s'", target->var_ref);
                return 1;
            }
            if (local->is_param)
            {
                diag_error_at(target->src, target->line, target->col,
                              "assignment to parameter '%s' not supported yet", target->var_ref);
                return 1;
            }

            if (local->is_address_only && type_is_address_only(local->type))
            {
                Type *ptr_ty = type_ptr(local->type);
                CcbLocal *dst_ptr = ccb_local_add(fb, NULL, ptr_ty, false, false);
                if (!dst_ptr)
                    return 1;
                if (!ccb_emit_load_local(fb, local))
                    return 1;
                if (!ccb_emit_store_local(fb, dst_ptr))
                    return 1;

                if (ccb_emit_expr_basic(fb, expr->rhs))
                    return 1;

                CCValueType rhs_ty = ccb_type_for_expr(expr->rhs);
                if (rhs_ty != CC_TYPE_PTR)
                {
                    if (ccb_emit_convert_between(fb, rhs_ty, CC_TYPE_PTR, expr->rhs))
                        return 1;
                }

                CcbLocal *src_ptr = ccb_local_add(fb, NULL, ptr_ty, false, false);
                if (!src_ptr)
                    return 1;
                if (!ccb_emit_store_local(fb, src_ptr))
                    return 1;

                if (ccb_emit_struct_copy(fb, local->type, dst_ptr, src_ptr))
                    return 1;

                CCValueType result_ty = ccb_type_for_expr(expr);
                if (result_ty != CC_TYPE_VOID)
                {
                    if (!ccb_emit_load_local(fb, local))
                        return 1;
                }
                return 0;
            }

            if (ccb_emit_expr_basic(fb, expr->rhs))
                return 1;
            if (!ccb_emit_store_local(fb, local))
                return 1;

            CCValueType result_ty = ccb_type_for_expr(expr);
            if (result_ty != CC_TYPE_VOID)
            {
                if (!ccb_emit_load_local(fb, local))
                    return 1;
            }
            return 0;
        }
        case ND_INDEX:
        {
            CCValueType elem_ty = CC_TYPE_I32;
            const Type *elem_type = NULL;
            if (ccb_emit_index_address(fb, target, &elem_ty, &elem_type))
                return 1;

            if (type_is_address_only(elem_type))
            {
                Type *ptr_ty = type_ptr((Type *)elem_type);
                CcbLocal *dst_ptr = ccb_local_add(fb, NULL, ptr_ty, false, false);
                if (!dst_ptr)
                    return 1;
                if (!ccb_emit_store_local(fb, dst_ptr))
                    return 1;

                if (ccb_emit_expr_basic(fb, expr->rhs))
                    return 1;

                CCValueType rhs_ty = ccb_type_for_expr(expr->rhs);
                if (rhs_ty != CC_TYPE_PTR)
                {
                    if (ccb_emit_convert_between(fb, rhs_ty, CC_TYPE_PTR, expr->rhs))
                        return 1;
                }

                CcbLocal *src_ptr = ccb_local_add(fb, NULL, ptr_ty, false, false);
                if (!src_ptr)
                    return 1;
                if (!ccb_emit_store_local(fb, src_ptr))
                    return 1;

                if (ccb_emit_struct_copy(fb, elem_type, dst_ptr, src_ptr))
                    return 1;

                CCValueType result_ty = ccb_type_for_expr(expr);
                if (result_ty != CC_TYPE_VOID)
                {
                    if (!ccb_emit_load_local(fb, dst_ptr))
                        return 1;
                }
                return 0;
            }

            if (ccb_emit_expr_basic(fb, expr->rhs))
                return 1;

            CCValueType rhs_ty = ccb_type_for_expr(expr->rhs);
            if (rhs_ty == CC_TYPE_INVALID)
                rhs_ty = elem_ty;

            if (elem_ty != CC_TYPE_VOID && rhs_ty != elem_ty)
            {
                if (ccb_emit_convert_between(fb, rhs_ty, elem_ty, expr->rhs))
                    return 1;
            }

            Type *temp_type = elem_type ? (Type *)elem_type : (expr->rhs ? expr->rhs->type : NULL);
            CcbLocal *temp = ccb_local_add(fb, NULL, temp_type, false, false);
            if (!temp)
            {
                diag_error_at(expr->src, expr->line, expr->col,
                              "failed to allocate temporary for indexed assignment");
                return 1;
            }

            if (!ccb_emit_store_local(fb, temp))
                return 1;
            if (!ccb_emit_load_local(fb, temp))
                return 1;
            if (!ccb_emit_store_indirect(&fb->body, elem_ty))
                return 1;
            if (!ccb_emit_load_local(fb, temp))
                return 1;

            CCValueType result_ty = ccb_type_for_expr(expr);
            if (result_ty == CC_TYPE_INVALID)
                result_ty = elem_ty;
            if (result_ty != CC_TYPE_VOID && result_ty != elem_ty)
            {
                if (ccb_emit_convert_between(fb, elem_ty, result_ty, expr))
                    return 1;
            }
            return 0;
        }
        case ND_MEMBER:
        {
            CCValueType field_ty = CC_TYPE_I32;
            const Type *field_type = NULL;
            if (ccb_emit_member_address(fb, target, &field_ty, &field_type))
                return 1;

            if (type_is_address_only(field_type))
            {
                Type *ptr_ty = type_ptr((Type *)field_type);
                CcbLocal *dst_ptr = ccb_local_add(fb, NULL, ptr_ty, false, false);
                if (!dst_ptr)
                    return 1;
                if (!ccb_emit_store_local(fb, dst_ptr))
                    return 1;

                if (ccb_emit_expr_basic(fb, expr->rhs))
                    return 1;

                CCValueType rhs_ty = ccb_type_for_expr(expr->rhs);
                if (rhs_ty != CC_TYPE_PTR)
                {
                    if (ccb_emit_convert_between(fb, rhs_ty, CC_TYPE_PTR, expr->rhs))
                        return 1;
                }

                CcbLocal *src_ptr = ccb_local_add(fb, NULL, ptr_ty, false, false);
                if (!src_ptr)
                    return 1;
                if (!ccb_emit_store_local(fb, src_ptr))
                    return 1;

                if (ccb_emit_struct_copy(fb, field_type, dst_ptr, src_ptr))
                    return 1;

                CCValueType result_ty = ccb_type_for_expr(expr);
                if (result_ty != CC_TYPE_VOID)
                {
                    if (!ccb_emit_load_local(fb, dst_ptr))
                        return 1;
                }
                return 0;
            }

            if (ccb_emit_expr_basic(fb, expr->rhs))
                return 1;

            CCValueType rhs_ty = ccb_type_for_expr(expr->rhs);
            if (rhs_ty == CC_TYPE_INVALID)
                rhs_ty = field_ty;

            if (field_ty != CC_TYPE_VOID && rhs_ty != field_ty)
            {
                if (ccb_emit_convert_between(fb, rhs_ty, field_ty, expr->rhs))
                    return 1;
            }

            Type *temp_type = field_type ? (Type *)field_type : (expr->rhs ? expr->rhs->type : NULL);
            CcbLocal *temp = ccb_local_add(fb, NULL, temp_type, false, false);
            if (!temp)
            {
                diag_error_at(expr->src, expr->line, expr->col,
                              "failed to allocate temporary for member assignment");
                return 1;
            }

            if (!ccb_emit_store_local(fb, temp))
                return 1;
            if (!ccb_emit_load_local(fb, temp))
                return 1;
            if (!ccb_emit_store_indirect(&fb->body, field_ty))
                return 1;
            if (!ccb_emit_load_local(fb, temp))
                return 1;

            CCValueType result_ty = ccb_type_for_expr(expr);
            if (result_ty == CC_TYPE_INVALID)
                result_ty = field_ty;
            if (result_ty != CC_TYPE_VOID && result_ty != field_ty)
            {
                if (ccb_emit_convert_between(fb, field_ty, result_ty, expr))
                    return 1;
            }
            return 0;
        }
        default:
            diag_error_at(target->src, target->line, target->col,
                          "assignment target %s not supported",
                          node_kind_name(target->kind));
            return 1;
        }
    }
    case ND_CAST:
    {
        if (!expr->lhs)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "cast expression missing operand");
            return 1;
        }

        if (ccb_emit_expr_basic(fb, expr->lhs))
            return 1;

        CCValueType from_ty = ccb_type_for_expr(expr->lhs);
        CCValueType to_ty = map_type_to_cc(expr->type);

        if (to_ty == CC_TYPE_INVALID)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "unsupported target type in cast expression");
            return 1;
        }

        if (from_ty == CC_TYPE_INVALID)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "unsupported source type in cast expression");
            return 1;
        }

        if (to_ty == CC_TYPE_VOID)
        {
            if (!ccb_emit_drop_for_type(&fb->body, from_ty))
                return 1;
            return 0;
        }

        if (from_ty == CC_TYPE_VOID)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "cannot cast from void expression");
            return 1;
        }

        if (ccb_emit_convert_between(fb, from_ty, to_ty, expr))
            return 1;
        return 0;
    }
    default:
        diag_error_at(expr->src, expr->line, expr->col,
                      "bytecode backend does not yet support %s (node kind %d)",
                      node_kind_name(expr->kind), expr->kind);
        return 1;
    }
}

static int ccb_emit_stmt_basic(CcbFunctionBuilder *fb, const Node *stmt)
{
    if (!fb || !stmt)
        return 1;

    switch (stmt->kind)
    {
    case ND_RET:
        if (stmt->lhs)
        {
            if (ccb_emit_expr_basic(fb, stmt->lhs))
                return 1;
            if (!string_list_append(&fb->body, "  ret"))
                return 1;
        }
        else
        {
            if (!string_list_append(&fb->body, "  ret void"))
                return 1;
        }
        return 0;
    case ND_BLOCK:
        for (int i = 0; i < stmt->stmt_count; ++i)
        {
            if (ccb_emit_stmt_basic(fb, stmt->stmts[i]))
                return 1;
        }
        return 0;
    case ND_EXPR_STMT:
        if (stmt->lhs)
        {
            if (ccb_emit_expr_basic(fb, stmt->lhs))
                return 1;
            CCValueType expr_ty = ccb_type_for_expr(stmt->lhs);
            if (!ccb_emit_drop_for_type(&fb->body, expr_ty))
                return 1;
        }
        return 0;
    case ND_IF:
    {
        if (!stmt->lhs || !stmt->rhs)
        {
            diag_error_at(stmt->src, stmt->line, stmt->col,
                          "if statement missing condition or body");
            return 1;
        }

        if (ccb_emit_condition(fb, stmt->lhs))
            return 1;

        char true_label[32];
        char false_label[32];
        char end_label[32];
        bool has_else = stmt->body != NULL;

        ccb_make_label(fb, true_label, sizeof(true_label), "if_true");
        if (has_else)
        {
            ccb_make_label(fb, false_label, sizeof(false_label), "if_false");
            ccb_make_label(fb, end_label, sizeof(end_label), "if_end");
        }
        else
        {
            ccb_make_label(fb, false_label, sizeof(false_label), "if_end");
        }

        if (!string_list_appendf(&fb->body, "  branch %s %s", true_label, false_label))
            return 1;

        if (!string_list_appendf(&fb->body, "label %s", true_label))
            return 1;
        if (ccb_emit_stmt_basic(fb, stmt->rhs))
            return 1;

        if (has_else)
        {
            if (!string_list_appendf(&fb->body, "  jump %s", end_label))
                return 1;
            if (!string_list_appendf(&fb->body, "label %s", false_label))
                return 1;
            if (ccb_emit_stmt_basic(fb, stmt->body))
                return 1;
            if (!string_list_appendf(&fb->body, "label %s", end_label))
                return 1;
        }
        else
        {
            if (!string_list_appendf(&fb->body, "label %s", false_label))
                return 1;
        }
        return 0;
    }
    case ND_WHILE:
    {
        if (!stmt->lhs)
        {
            diag_error_at(stmt->src, stmt->line, stmt->col,
                          "while statement missing condition");
            return 1;
        }

        char cond_label[32];
        char body_label[32];
        char end_label[32];

        ccb_make_label(fb, cond_label, sizeof(cond_label), "while_cond");
        ccb_make_label(fb, body_label, sizeof(body_label), "while_body");
        ccb_make_label(fb, end_label, sizeof(end_label), "while_end");

        if (!string_list_appendf(&fb->body, "label %s", cond_label))
            return 1;
        if (ccb_emit_condition(fb, stmt->lhs))
            return 1;
        if (!string_list_appendf(&fb->body, "  branch %s %s", body_label, end_label))
            return 1;

        if (!string_list_appendf(&fb->body, "label %s", body_label))
            return 1;
        if (stmt->rhs)
        {
            if (ccb_emit_stmt_basic(fb, stmt->rhs))
                return 1;
        }
        if (!string_list_appendf(&fb->body, "  jump %s", cond_label))
            return 1;

        if (!string_list_appendf(&fb->body, "label %s", end_label))
            return 1;
        return 0;
    }
    case ND_VAR_DECL:
    {
        if (!stmt->var_name)
        {
            diag_error_at(stmt->src, stmt->line, stmt->col,
                          "local declaration missing name");
            return 1;
        }
        if (ccb_local_lookup(fb, stmt->var_name))
        {
            diag_error_at(stmt->src, stmt->line, stmt->col,
                          "duplicate local '%s'", stmt->var_name);
            return 1;
        }

        Type *var_type = stmt->var_type;
        bool address_only = var_type && var_type->kind == TY_STRUCT;

        CcbLocal *local = ccb_local_add(fb, stmt->var_name, var_type, address_only, false);
        if (!local)
        {
            diag_error_at(stmt->src, stmt->line, stmt->col,
                          "failed to allocate storage for local '%s'", stmt->var_name);
            return 1;
        }

        const Node *init = stmt->rhs;
        if (address_only)
        {
            size_t size_bytes = ccb_type_size_bytes(var_type);
            if (size_bytes == 0)
                size_bytes = 1;
            size_t alignment = 8;
            if (!string_list_appendf(&fb->body, "  stack_alloc %zu %zu", size_bytes, alignment))
                return 1;
            if (!ccb_emit_store_local(fb, local))
                return 1;

            if (ccb_emit_struct_initializer(fb, stmt, stmt->var_name, var_type, init))
                return 1;
            return 0;
        }

        if (init)
        {
            if (init->kind == ND_INIT_LIST)
            {
                if (!init->init.is_zero)
                {
                    diag_error_at(init->src, init->line, init->col,
                                  "initializer lists not supported yet");
                    return 1;
                }
                if (!ccb_emit_const_zero(&fb->body, local->value_type))
                    return 1;
            }
            else
            {
                if (ccb_emit_expr_basic(fb, init))
                    return 1;
            }

            if (!ccb_emit_store_local(fb, local))
                return 1;
        }
        return 0;
    }
    default:
        diag_error_at(stmt->src, stmt->line, stmt->col,
                      "bytecode backend does not yet support %s (node kind %d)",
                      node_kind_name(stmt->kind), stmt->kind);
        return 1;
    }
}

static bool append_token_dynamic(char **buffer, size_t *capacity, size_t *length, const char *token)
{
    if (!buffer || !capacity || !length)
        return false;

    if (!token)
        token = "";

    size_t token_len = strlen(token);
    size_t need = *length + token_len + (*length ? 1 : 0) + 1;
    if (need > *capacity)
    {
        size_t new_cap = *capacity ? *capacity * 2 : 64;
        while (need > new_cap)
            new_cap *= 2;
        char *new_buf = (char *)realloc(*buffer, new_cap);
        if (!new_buf)
            return false;
        *buffer = new_buf;
        *capacity = new_cap;
    }

    if (*length > 0)
        (*buffer)[(*length)++] = ' ';
    memcpy(*buffer + *length, token, token_len);
    *length += token_len;
    (*buffer)[*length] = '\0';
    return true;
}

static bool ccb_append_params_line(CcbModule *mod, const Node *fn)
{
    if (!mod || !fn || fn->param_count <= 0)
        return true;

    size_t cap = 0;
    size_t len = 0;
    char *line = NULL;

    if (!append_token_dynamic(&line, &cap, &len, ".params"))
    {
        free(line);
        return false;
    }

    for (int i = 0; i < fn->param_count; ++i)
    {
        CCValueType ty = map_type_to_cc(fn->param_types ? fn->param_types[i] : NULL);
        if (!append_token_dynamic(&line, &cap, &len, cc_type_name(ty)))
        {
            free(line);
            return false;
        }
    }

    bool appended = string_list_append(&mod->lines, line);
    free(line);
    return appended;
}

static bool ccb_append_locals_line(CcbModule *mod, const CcbFunctionBuilder *fb)
{
    if (!mod || !fb || fb->local_count == 0)
        return true;

    size_t cap = 0;
    size_t len = 0;
    char *line = NULL;

    if (!append_token_dynamic(&line, &cap, &len, ".locals"))
    {
        free(line);
        return false;
    }

    for (size_t i = 0; i < fb->locals_count; ++i)
    {
        const CcbLocal *local = &fb->locals[i];
        if (local->is_param)
            continue;
        if (!append_token_dynamic(&line, &cap, &len, cc_type_name(local->value_type)))
        {
            free(line);
            return false;
        }
    }

    bool appended = string_list_append(&mod->lines, line);
    free(line);
    return appended;
}

static int ccb_function_emit_chancecode(CcbModule *mod, const Node *fn, const CodegenOptions *opts)
{
    (void)opts;
    if (!mod || !fn || fn->kind != ND_FUNC || !fn->name)
        return 1;

    const char *backend_name = fn->metadata.backend_name ? fn->metadata.backend_name : fn->name;
    const char *ret_name = cc_type_name(map_type_to_cc(fn->ret_type));
    int declared_params = (fn->metadata.declared_param_count >= 0)
                              ? fn->metadata.declared_param_count
                              : fn->param_count;
    if (declared_params < 0)
        declared_params = 0;
    size_t param_count = (size_t)declared_params;
    size_t local_count = (fn->metadata.declared_local_count >= 0)
                             ? (size_t)fn->metadata.declared_local_count
                             : 0;

    if (fn->metadata.func_line)
    {
        if (!ccb_module_append_line(mod, fn->metadata.func_line))
            return 1;
    }
    else
    {
        if (!ccb_module_appendf(mod, ".func %s ret=%s params=%zu locals=%zu",
                                backend_name, ret_name, param_count, local_count))
            return 1;
    }

    if (fn->metadata.params_line)
    {
        if (!ccb_module_append_line(mod, fn->metadata.params_line))
            return 1;
    }
    else if (fn->param_count > 0 && declared_params == fn->param_count)
    {
        if (!ccb_append_params_line(mod, fn))
            return 1;
    }

    if (fn->metadata.locals_line)
    {
        if (!ccb_module_append_line(mod, fn->metadata.locals_line))
            return 1;
    }

    if (fn->chancecode.count > 0 && fn->chancecode.lines)
    {
        for (int i = 0; i < fn->chancecode.count; ++i)
        {
            const char *line = fn->chancecode.lines[i] ? fn->chancecode.lines[i] : "";
            if (!ccb_module_append_line(mod, line))
                return 1;
        }
    }

    if (!ccb_module_append_line(mod, ".endfunc"))
        return 1;

    if (fn->is_noreturn)
    {
        if (!ccb_module_appendf(mod, ".no-return %s", backend_name))
            return 1;
    }

    return 0;
}

static int ccb_function_emit_basic(CcbModule *mod, const Node *fn, const CodegenOptions *opts)
{
    if (!mod || !fn || fn->kind != ND_FUNC || !fn->name)
        return 1;

    if (fn->is_chancecode)
        return ccb_function_emit_chancecode(mod, fn, opts);

    CcbFunctionBuilder fb;
    ccb_function_builder_init(&fb, mod, fn);
    ccb_function_builder_register_params(&fb);

    int rc = ccb_emit_stmt_basic(&fb, fn->body);
    if (!rc)
        ccb_function_optimize(&fb, opts);
    if (!rc)
    {
        const char *backend_name = fn->metadata.backend_name ? fn->metadata.backend_name : fn->name;

        if (fn->metadata.func_line)
        {
            if (!ccb_module_append_line(mod, fn->metadata.func_line))
                rc = 1;
        }
        else
        {
            if (!ccb_module_appendf(mod, ".func %s ret=%s params=%zu locals=%zu", backend_name,
                                     cc_type_name(fb.ret_type), fb.param_count, fb.local_count))
                rc = 1;
        }

        if (!rc)
        {
            if (fn->metadata.params_line)
            {
                if (!ccb_module_append_line(mod, fn->metadata.params_line))
                    rc = 1;
            }
            else if (fb.param_count > 0)
            {
                if (!ccb_append_params_line(mod, fn))
                    rc = 1;
            }
        }

        if (!rc)
        {
            if (fn->metadata.locals_line)
            {
                if (!ccb_module_append_line(mod, fn->metadata.locals_line))
                    rc = 1;
            }
            else if (fb.local_count > 0)
            {
                if (!ccb_append_locals_line(mod, &fb))
                    rc = 1;
            }
        }

        if (!rc)
        {
            for (size_t i = 0; i < fb.body.count; ++i)
            {
                if (!ccb_module_append_line(mod, fb.body.items[i]))
                {
                    rc = 1;
                    break;
                }
            }
        }

        if (!rc)
        {
            if (!ccb_module_append_line(mod, ".endfunc"))
                rc = 1;
        }

        if (!rc && fn->is_noreturn)
        {
            if (!ccb_module_appendf(mod, ".no-return %s", backend_name))
                rc = 1;
        }
    }

    ccb_function_builder_free(&fb);
    return rc;
}

static CCValueType map_type_to_cc(const Type *ty)
{
    if (!ty)
        return CC_TYPE_I32;

    switch (ty->kind)
    {
    case TY_I8:
        return CC_TYPE_I8;
    case TY_U8:
        return CC_TYPE_U8;
    case TY_I16:
        return CC_TYPE_I16;
    case TY_U16:
        return CC_TYPE_U16;
    case TY_I32:
        return CC_TYPE_I32;
    case TY_U32:
        return CC_TYPE_U32;
    case TY_I64:
        return CC_TYPE_I64;
    case TY_U64:
        return CC_TYPE_U64;
    case TY_F32:
        return CC_TYPE_F32;
    case TY_F64:
        return CC_TYPE_F64;
    case TY_PTR:
        return CC_TYPE_PTR;
    case TY_VOID:
        return CC_TYPE_VOID;
    case TY_CHAR:
        return CC_TYPE_I8;
    case TY_STRUCT:
        return CC_TYPE_PTR;
    case TY_F128:
        return CC_TYPE_F64;
    default:
        return CC_TYPE_I32;
    }
}

static const char *cc_type_name(CCValueType ty)
{
    switch (ty)
    {
    case CC_TYPE_I1:
        return "i1";
    case CC_TYPE_I8:
        return "i8";
    case CC_TYPE_U8:
        return "u8";
    case CC_TYPE_I16:
        return "i16";
    case CC_TYPE_U16:
        return "u16";
    case CC_TYPE_I32:
        return "i32";
    case CC_TYPE_U32:
        return "u32";
    case CC_TYPE_I64:
        return "i64";
    case CC_TYPE_U64:
        return "u64";
    case CC_TYPE_F32:
        return "f32";
    case CC_TYPE_F64:
        return "f64";
    case CC_TYPE_PTR:
        return "ptr";
    case CC_TYPE_VOID:
        return "void";
    default:
        return "i32";
    }
}

static size_t ccb_value_type_size(CCValueType ty)
{
    switch (ty)
    {
    case CC_TYPE_I1:
        return 1;
    case CC_TYPE_I8:
    case CC_TYPE_U8:
        return 1;
    case CC_TYPE_I16:
    case CC_TYPE_U16:
        return 2;
    case CC_TYPE_I32:
    case CC_TYPE_U32:
    case CC_TYPE_F32:
        return 4;
    case CC_TYPE_I64:
    case CC_TYPE_U64:
    case CC_TYPE_F64:
    case CC_TYPE_PTR:
        return 8;
    default:
        return 8;
    }
}

static bool ccb_value_type_is_integer(CCValueType ty)
{
    switch (ty)
    {
    case CC_TYPE_I1:
    case CC_TYPE_I8:
    case CC_TYPE_U8:
    case CC_TYPE_I16:
    case CC_TYPE_U16:
    case CC_TYPE_I32:
    case CC_TYPE_U32:
    case CC_TYPE_I64:
    case CC_TYPE_U64:
        return true;
    default:
        return false;
    }
}

static bool ccb_value_type_is_float(CCValueType ty)
{
    return ty == CC_TYPE_F32 || ty == CC_TYPE_F64;
}

static bool ccb_value_type_is_signed(CCValueType ty)
{
    switch (ty)
    {
    case CC_TYPE_I1:
    case CC_TYPE_I8:
    case CC_TYPE_I16:
    case CC_TYPE_I32:
    case CC_TYPE_I64:
        return true;
    default:
        return false;
    }
}

static int write_module_to_file(const char *path, const CcbModule *mod)
{
    FILE *out = fopen(path, "wb");
    if (!out)
    {
        fprintf(stderr, "codegen: failed to open '%s': %s\n", path, strerror(errno));
        return 1;
    }

    fprintf(out, "ccbytecode 2\n\n");
    for (size_t i = 0; i < mod->lines.count; ++i)
    {
        const char *line = mod->lines.items[i];
        if (!line)
            line = "";
        fputs(line, out);
        size_t len = strlen(line);
        if (len == 0 || line[len - 1] != '\n')
            fputc('\n', out);
    }

    fclose(out);
    return 0;
}

int codegen_ccb_resolve_module_path(const CodegenOptions *opts, char *buffer, size_t bufsz)
{
    if (!buffer || bufsz == 0)
        return 1;

    const char *preferred = NULL;
    if (opts)
    {
        if (opts->ccb_output_path && opts->ccb_output_path[0] != '\0')
            preferred = opts->ccb_output_path;
        else if (opts->output_path && opts->output_path[0] != '\0')
            preferred = opts->output_path;
    }
    if (!preferred)
        preferred = "a.ccb";

    const char *dot = strrchr(preferred, '.');
    int has_ccb_ext = dot && ((strcmp(dot, ".ccb") == 0) || (strcmp(dot, ".CCB") == 0));

    if (preferred != buffer)
        snprintf(buffer, bufsz, "%s", preferred);

    if (has_ccb_ext)
        return 0;

    if (preferred == buffer)
    {
        size_t len = strlen(buffer);
        if (len + 4 >= bufsz)
        {
            if (bufsz < 5)
                return 1;
            len = bufsz - 5;
            buffer[len] = '\0';
        }
        strncat(buffer, ".ccb", bufsz - strlen(buffer) - 1);
        return 0;
    }

    size_t len = strlen(buffer);
    if (len + 4 >= bufsz)
    {
        if (bufsz < 5)
            return 1;
        len = bufsz - 5;
        buffer[len] = '\0';
    }
    strncat(buffer, ".ccb", bufsz - strlen(buffer) - 1);
    return 0;
}

int codegen_ccb_write_module(const Node *unit, const CodegenOptions *opts)
{
    if (!unit)
    {
        fprintf(stderr, "codegen: null unit\n");
        return 1;
    }

    CcbModule mod;
    ccb_module_init(&mod);

    int rc = 0;
    if (!rc)
        rc = ccb_module_emit_externs(&mod, opts);

    if (!rc)
    {
        if (unit->kind == ND_UNIT)
        {
            for (int i = 0; !rc && i < unit->stmt_count; ++i)
            {
                if (ccb_function_emit_basic(&mod, unit->stmts[i], opts))
                    rc = 1;
            }
        }
        else if (unit->kind == ND_FUNC)
        {
            rc = ccb_function_emit_basic(&mod, unit, opts);
        }
        else
        {
            fprintf(stderr, "codegen: unsupported unit kind %d\n", unit->kind);
            rc = 1;
        }
    }

    int write_rc = 0;
    if (!rc)
    {
        char out_path[512];
        if (codegen_ccb_resolve_module_path(opts, out_path, sizeof(out_path)))
            rc = 1;
        else
            write_rc = write_module_to_file(out_path, &mod);
    }

    ccb_module_free(&mod);
    return rc || write_rc;
}
