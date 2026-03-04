#include "ccsim.h"
#include "ast.h"

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void ccsim_tracef(const char *fmt, ...)
{
    if (!compiler_verbose_deep_enabled() || !fmt)
        return;
    va_list ap;
    va_start(ap, fmt);
    int needed = vsnprintf(NULL, 0, fmt, ap);
    va_end(ap);
    if (needed <= 0)
        return;
    char *buf = (char *)malloc((size_t)needed + 1);
    if (!buf)
        return;
    va_start(ap, fmt);
    vsnprintf(buf, (size_t)needed + 1, fmt, ap);
    va_end(ap);
    compiler_verbose_treef("ccsim", "|-", "%s", buf);
    free(buf);
}

typedef enum
{
    CCSIM_VAL_UNKNOWN = 0,
    CCSIM_VAL_INT,
    CCSIM_VAL_ADDRESS
} CcsimValueKind;

typedef struct
{
    CcsimValueKind kind;
    char type[16];
    char symbol[256];
    long long offset;
    unsigned width;
    bool is_signed;
    unsigned long long u;
    long long s;
} CcsimValue;

typedef struct
{
    CcsimValue value;
    int escaped;
} CcsimLocal;

typedef struct
{
    CcsimValue *items;
    size_t count;
    size_t capacity;
} CcsimStack;

typedef struct
{
    char symbol[256];
    CcsimValue value;
} CcsimKnownGlobal;

typedef struct
{
    CcsimKnownGlobal *items;
    size_t count;
    size_t capacity;
} CcsimKnownGlobalList;

typedef struct
{
    char symbol[256];
    long long offset;
    char type[16];
    CcsimValue value;
} CcsimKnownMemoryCell;

typedef struct
{
    CcsimKnownMemoryCell *items;
    size_t count;
    size_t capacity;
} CcsimKnownMemoryList;

static bool ccsim_parse_func_header(const char *line,
                                    char *symbol, size_t symbol_sz,
                                    char *ret_type, size_t ret_type_sz,
                                    size_t *params_count,
                                    bool *is_hidden);
static bool ccsim_parse_const(const char *line, CcsimValue *out);
static bool ccsim_parse_local_index(const char *line, const char *prefix, int *out_index);
static bool ccsim_local_ensure(CcsimLocal **locals, size_t *local_capacity, size_t index);
static bool ccsim_parse_convert(const char *line, char *kind, size_t kind_sz, char *from_type, size_t from_sz, char *to_type, size_t to_sz);
static CcsimValue ccsim_eval_convert(const char *kind, const char *from_type, const char *to_type,
                                     CcsimValue input, bool *ok);
static bool ccsim_parse_binop(const char *line, char *op, size_t op_sz, char *type_name, size_t type_sz, bool *unsigned_hint);
static CcsimValue ccsim_eval_binop(const char *op, const char *type_name, bool unsigned_hint,
                                   CcsimValue lhs, CcsimValue rhs, bool *ok);
static bool ccsim_parse_unop(const char *line, char *op, size_t op_sz, char *type_name, size_t type_sz);
static CcsimValue ccsim_eval_unop(const char *op, const char *type_name, CcsimValue operand, bool *ok);
static bool ccsim_parse_compare(const char *line, char *op, size_t op_sz, char *type_name, size_t type_sz, bool *unsigned_hint);
static CcsimValue ccsim_eval_compare(const char *op, const char *type_name, bool unsigned_hint,
                                     CcsimValue lhs, CcsimValue rhs, bool *ok);
static bool ccsim_parse_test_null(const char *line, char *type_name, size_t type_sz);
static bool ccsim_parse_call_line(const char *line,
                                  char *symbol, size_t symbol_sz,
                                  char *ret_type, size_t ret_type_sz,
                                  bool *is_zero_arg);
static bool ccsim_eval_hidden_pure_call(char **module_lines, size_t module_line_count,
                                        const char *callee_symbol,
                                        const CcsimValue *args, size_t arg_count,
                                        CcsimValue *out_ret);

typedef struct
{
    char symbol[256];
    CcsimValue value;
} CcsimConstCallee;

typedef struct
{
    CcsimConstCallee *items;
    size_t count;
    size_t capacity;
} CcsimConstCalleeList;

static unsigned ccsim_type_bits(const char *type_name)
{
    if (!type_name)
        return 0;
    if (strcmp(type_name, "i1") == 0)
        return 1;
    if (strcmp(type_name, "i8") == 0 || strcmp(type_name, "u8") == 0)
        return 8;
    if (strcmp(type_name, "i16") == 0 || strcmp(type_name, "u16") == 0)
        return 16;
    if (strcmp(type_name, "i32") == 0 || strcmp(type_name, "u32") == 0 || strcmp(type_name, "f32") == 0)
        return 32;
    if (strcmp(type_name, "i64") == 0 || strcmp(type_name, "u64") == 0 || strcmp(type_name, "f64") == 0 || strcmp(type_name, "ptr") == 0)
        return 64;
    return 0;
}

static bool ccsim_type_is_integer(const char *type_name)
{
    if (!type_name)
        return false;
    return type_name[0] == 'i' || type_name[0] == 'u';
}

static bool ccsim_type_is_ptr(const char *type_name)
{
    return type_name && strcmp(type_name, "ptr") == 0;
}

static bool ccsim_type_is_signed(const char *type_name)
{
    return type_name && type_name[0] == 'i';
}

static unsigned long long ccsim_mask_for_width(unsigned width)
{
    if (width == 0)
        return 0ULL;
    if (width >= 64)
        return ~0ULL;
    return (1ULL << width) - 1ULL;
}

static long long ccsim_sign_extend(unsigned long long value, unsigned width)
{
    if (width == 0)
        return 0;
    if (width >= 64)
        return (long long)value;
    unsigned long long sign_bit = 1ULL << (width - 1);
    unsigned long long mask = ccsim_mask_for_width(width);
    value &= mask;
    if (value & sign_bit)
        return (long long)(value | (~mask));
    return (long long)value;
}

static CcsimValue ccsim_make_unknown(void)
{
    CcsimValue value;
    memset(&value, 0, sizeof(value));
    value.kind = CCSIM_VAL_UNKNOWN;
    return value;
}

static CcsimValue ccsim_make_known(const char *type_name, unsigned long long u, long long s, bool is_signed)
{
    CcsimValue value;
    memset(&value, 0, sizeof(value));
    value.kind = CCSIM_VAL_INT;
    if (type_name)
    {
        strncpy(value.type, type_name, sizeof(value.type) - 1);
        value.type[sizeof(value.type) - 1] = '\0';
    }
    value.width = ccsim_type_bits(type_name);
    value.is_signed = is_signed;
    value.u = u & ccsim_mask_for_width(value.width);
    value.s = is_signed ? ccsim_sign_extend(value.u, value.width) : (long long)value.u;
    return value;
}

static CcsimValue ccsim_make_address(const char *symbol, long long offset)
{
    CcsimValue value;
    memset(&value, 0, sizeof(value));
    value.kind = CCSIM_VAL_ADDRESS;
    strncpy(value.type, "ptr", sizeof(value.type) - 1);
    value.type[sizeof(value.type) - 1] = '\0';
    if (symbol)
    {
        strncpy(value.symbol, symbol, sizeof(value.symbol) - 1);
        value.symbol[sizeof(value.symbol) - 1] = '\0';
    }
    value.offset = offset;
    value.width = 64;
    value.is_signed = false;
    return value;
}

static void ccsim_stack_free(CcsimStack *stack)
{
    if (!stack)
        return;
    free(stack->items);
    stack->items = NULL;
    stack->count = 0;
    stack->capacity = 0;
}

static void ccsim_stack_clear(CcsimStack *stack)
{
    if (!stack)
        return;
    stack->count = 0;
}

static bool ccsim_stack_push(CcsimStack *stack, CcsimValue value)
{
    if (!stack)
        return false;
    if (stack->count == stack->capacity)
    {
        size_t new_capacity = stack->capacity ? (stack->capacity * 2) : 16;
        CcsimValue *new_items = (CcsimValue *)realloc(stack->items, new_capacity * sizeof(CcsimValue));
        if (!new_items)
            return false;
        stack->items = new_items;
        stack->capacity = new_capacity;
    }
    stack->items[stack->count++] = value;
    return true;
}

static CcsimValue ccsim_stack_pop(CcsimStack *stack)
{
    if (!stack || stack->count == 0)
        return ccsim_make_unknown();
    return stack->items[--stack->count];
}

static CcsimValue ccsim_stack_peek(const CcsimStack *stack)
{
    if (!stack || stack->count == 0)
        return ccsim_make_unknown();
    return stack->items[stack->count - 1];
}

static const char *ccsim_trim(const char *line)
{
    if (!line)
        return NULL;
    while (*line == ' ' || *line == '\t')
        ++line;
    return line;
}

static bool ccsim_parse_symbol_after_prefix(const char *line, const char *prefix,
                                            char *out_symbol, size_t out_symbol_sz)
{
    if (!line || !prefix || !out_symbol || out_symbol_sz == 0)
        return false;
    line = ccsim_trim(line);
    size_t plen = strlen(prefix);
    if (strncmp(line, prefix, plen) != 0)
        return false;
    line += plen;
    while (*line == ' ' || *line == '\t')
        ++line;
    if (*line == '\0')
        return false;

    size_t n = 0;
    while (*line && !isspace((unsigned char)*line))
    {
        if (n + 1 >= out_symbol_sz)
            return false;
        out_symbol[n++] = *line++;
    }
    if (n == 0)
        return false;
    out_symbol[n] = '\0';
    return true;
}

static bool ccsim_parse_memory_type_after_prefix(const char *line, const char *prefix,
                                                 char *out_type, size_t out_type_sz)
{
    if (!line || !prefix || !out_type || out_type_sz == 0)
        return false;
    line = ccsim_trim(line);
    size_t plen = strlen(prefix);
    if (strncmp(line, prefix, plen) != 0)
        return false;
    line += plen;
    while (*line == ' ' || *line == '\t')
        ++line;
    if (*line == '\0')
        return false;

    size_t n = 0;
    while (*line && !isspace((unsigned char)*line))
    {
        if (n + 1 >= out_type_sz)
            return false;
        out_type[n++] = *line++;
    }
    if (n == 0)
        return false;
    out_type[n] = '\0';
    return true;
}

static void ccsim_known_globals_free(CcsimKnownGlobalList *list)
{
    if (!list)
        return;
    free(list->items);
    list->items = NULL;
    list->count = 0;
    list->capacity = 0;
}

static bool ccsim_known_globals_set(CcsimKnownGlobalList *list, const char *symbol, CcsimValue value)
{
    if (!list || !symbol || !*symbol)
        return false;
    for (size_t i = 0; i < list->count; ++i)
    {
        if (strcmp(list->items[i].symbol, symbol) == 0)
        {
            list->items[i].value = value;
            return true;
        }
    }
    if (list->count == list->capacity)
    {
        size_t new_capacity = list->capacity ? list->capacity * 2 : 16;
        CcsimKnownGlobal *new_items = (CcsimKnownGlobal *)realloc(list->items, new_capacity * sizeof(CcsimKnownGlobal));
        if (!new_items)
            return false;
        list->items = new_items;
        list->capacity = new_capacity;
    }
    CcsimKnownGlobal *slot = &list->items[list->count++];
    memset(slot, 0, sizeof(*slot));
    strncpy(slot->symbol, symbol, sizeof(slot->symbol) - 1);
    slot->symbol[sizeof(slot->symbol) - 1] = '\0';
    slot->value = value;
    return true;
}

static CcsimValue ccsim_known_globals_get(const CcsimKnownGlobalList *list, const char *symbol)
{
    if (!list || !symbol)
        return ccsim_make_unknown();
    for (size_t i = 0; i < list->count; ++i)
    {
        if (strcmp(list->items[i].symbol, symbol) == 0)
            return list->items[i].value;
    }
    return ccsim_make_unknown();
}

static void ccsim_known_memory_free(CcsimKnownMemoryList *list)
{
    if (!list)
        return;
    free(list->items);
    list->items = NULL;
    list->count = 0;
    list->capacity = 0;
}

static void ccsim_known_memory_clear(CcsimKnownMemoryList *list)
{
    if (!list)
        return;
    list->count = 0;
}

static bool ccsim_known_memory_set(CcsimKnownMemoryList *list, const char *symbol,
                                   long long offset, const char *type_name,
                                   CcsimValue value)
{
    if (!list || !symbol || !*symbol || !type_name || !*type_name)
        return false;
    for (size_t i = 0; i < list->count; ++i)
    {
        CcsimKnownMemoryCell *cell = &list->items[i];
        if (cell->offset == offset && strcmp(cell->symbol, symbol) == 0 && strcmp(cell->type, type_name) == 0)
        {
            cell->value = value;
            return true;
        }
    }

    if (list->count == list->capacity)
    {
        size_t new_capacity = list->capacity ? list->capacity * 2 : 32;
        CcsimKnownMemoryCell *new_items = (CcsimKnownMemoryCell *)realloc(list->items, new_capacity * sizeof(CcsimKnownMemoryCell));
        if (!new_items)
            return false;
        list->items = new_items;
        list->capacity = new_capacity;
    }

    CcsimKnownMemoryCell *slot = &list->items[list->count++];
    memset(slot, 0, sizeof(*slot));
    strncpy(slot->symbol, symbol, sizeof(slot->symbol) - 1);
    slot->symbol[sizeof(slot->symbol) - 1] = '\0';
    slot->offset = offset;
    strncpy(slot->type, type_name, sizeof(slot->type) - 1);
    slot->type[sizeof(slot->type) - 1] = '\0';
    slot->value = value;
    return true;
}

static void ccsim_known_memory_remove(CcsimKnownMemoryList *list, const char *symbol,
                                      long long offset, const char *type_name)
{
    if (!list || !symbol || !type_name)
        return;
    for (size_t i = 0; i < list->count;)
    {
        CcsimKnownMemoryCell *cell = &list->items[i];
        if (cell->offset == offset && strcmp(cell->symbol, symbol) == 0 && strcmp(cell->type, type_name) == 0)
        {
            list->items[i] = list->items[list->count - 1];
            --list->count;
            continue;
        }
        ++i;
    }
}

static CcsimValue ccsim_known_memory_get(const CcsimKnownMemoryList *list, const char *symbol,
                                         long long offset, const char *type_name)
{
    if (!list || !symbol || !type_name)
        return ccsim_make_unknown();
    for (size_t i = 0; i < list->count; ++i)
    {
        const CcsimKnownMemoryCell *cell = &list->items[i];
        if (cell->offset == offset && strcmp(cell->symbol, symbol) == 0 && strcmp(cell->type, type_name) == 0)
            return cell->value;
    }
    return ccsim_make_unknown();
}

static void ccsim_known_memory_invalidate_symbol(CcsimKnownMemoryList *list, const char *symbol)
{
    if (!list || !symbol)
        return;
    for (size_t i = 0; i < list->count;)
    {
        if (strcmp(list->items[i].symbol, symbol) == 0)
        {
            list->items[i] = list->items[list->count - 1];
            --list->count;
            continue;
        }
        ++i;
    }
}

typedef struct
{
    char name[128];
    size_t index;
} CcsimLabel;

typedef struct
{
    CcsimLabel *items;
    size_t count;
    size_t capacity;
} CcsimLabelList;

static void ccsim_label_list_free(CcsimLabelList *list)
{
    if (!list)
        return;
    free(list->items);
    list->items = NULL;
    list->count = 0;
    list->capacity = 0;
}

static bool ccsim_label_list_add(CcsimLabelList *list, const char *name, size_t index)
{
    if (!list || !name || !*name)
        return false;
    for (size_t i = 0; i < list->count; ++i)
    {
        if (strcmp(list->items[i].name, name) == 0)
        {
            list->items[i].index = index;
            return true;
        }
    }
    if (list->count == list->capacity)
    {
        size_t new_capacity = list->capacity ? list->capacity * 2 : 32;
        CcsimLabel *new_items = (CcsimLabel *)realloc(list->items, new_capacity * sizeof(CcsimLabel));
        if (!new_items)
            return false;
        list->items = new_items;
        list->capacity = new_capacity;
    }
    CcsimLabel *slot = &list->items[list->count++];
    memset(slot, 0, sizeof(*slot));
    strncpy(slot->name, name, sizeof(slot->name) - 1);
    slot->name[sizeof(slot->name) - 1] = '\0';
    slot->index = index;
    return true;
}

static bool ccsim_label_list_find(const CcsimLabelList *list, const char *name, size_t *out_index)
{
    if (!list || !name || !out_index)
        return false;
    for (size_t i = 0; i < list->count; ++i)
    {
        if (strcmp(list->items[i].name, name) == 0)
        {
            *out_index = list->items[i].index;
            return true;
        }
    }
    return false;
}

static bool ccsim_parse_label_name(const char *line, char *out, size_t outsz)
{
    if (!line || !out || outsz == 0)
        return false;
    line = ccsim_trim(line);
    if (strncmp(line, "label ", 6) != 0)
        return false;
    line += 6;
    size_t n = 0;
    while (*line && !isspace((unsigned char)*line))
    {
        if (n + 1 >= outsz)
            return false;
        out[n++] = *line++;
    }
    if (n == 0)
        return false;
    out[n] = '\0';
    return true;
}

static bool ccsim_parse_jump_target(const char *line, char *out, size_t outsz)
{
    return ccsim_parse_symbol_after_prefix(line, "jump ", out, outsz);
}

static bool ccsim_parse_branch_targets(const char *line,
                                       char *true_label, size_t true_sz,
                                       char *false_label, size_t false_sz)
{
    if (!line || !true_label || !false_label)
        return false;
    line = ccsim_trim(line);
    if (strncmp(line, "branch ", 7) != 0)
        return false;
    line += 7;
    if (sscanf(line, "%127s %127s", true_label, false_label) != 2)
        return false;
    true_label[true_sz - 1] = '\0';
    false_label[false_sz - 1] = '\0';
    return true;
}

static int ccsim_count_call_args(const char *open_paren)
{
    if (!open_paren || *open_paren != '(')
        return -1;
    const char *close = strchr(open_paren, ')');
    if (!close || close < open_paren)
        return -1;
    if (close == open_paren + 1)
        return 0;
    int count = 1;
    for (const char *p = open_paren + 1; p < close; ++p)
    {
        if (*p == ',')
            ++count;
    }
    return count;
}

static bool ccsim_parse_call_effects(const char *line, int *out_arg_count, int *out_is_indirect)
{
    if (!line || !out_arg_count || !out_is_indirect)
        return false;
    *out_arg_count = 0;
    *out_is_indirect = 0;

    line = ccsim_trim(line);
    if (strncmp(line, "call_indirect", 13) == 0)
    {
        *out_is_indirect = 1;
        const char *open = strchr(line, '(');
        int args = ccsim_count_call_args(open);
        if (args < 0)
            return false;
        *out_arg_count = args;
        return true;
    }
    if (strncmp(line, "call ", 5) == 0)
    {
        const char *open = strchr(line, '(');
        int args = ccsim_count_call_args(open);
        if (args < 0)
            return false;
        *out_arg_count = args;
        return true;
    }
    return false;
}

static bool ccsim_collect_call_args_from_stack(const CcsimStack *stack, size_t arg_count, CcsimValue *out_args)
{
    if (!stack)
        return false;
    if (arg_count == 0)
        return true;
    if (!out_args || stack->count < arg_count)
        return false;
    size_t base = stack->count - arg_count;
    for (size_t i = 0; i < arg_count; ++i)
        out_args[i] = stack->items[base + i];
    return true;
}

static bool ccsim_emulate_intrinsic_call(const char *symbol,
                                         const char *ret_type,
                                         const CcsimValue *args,
                                         size_t arg_count,
                                         CcsimKnownMemoryList *memory,
                                         CcsimValue *out_ret,
                                         bool *out_has_ret)
{
    if (!symbol || !ret_type || !out_ret || !out_has_ret)
        return false;
    *out_has_ret = false;

    if (strcmp(symbol, "Std_Memory_memset_ptr_to_void_u8_i32") == 0)
    {
        if (!memory || !args || arg_count != 3)
            return false;
        const CcsimValue *dst = &args[0];
        const CcsimValue *val = &args[1];
        const CcsimValue *count = &args[2];
        if (dst->kind != CCSIM_VAL_ADDRESS || dst->symbol[0] == '\0')
            return false;
        if (val->kind != CCSIM_VAL_INT || count->kind != CCSIM_VAL_INT)
            return false;

        unsigned long long byte_u = val->u & 0xFFULL;
        long long n = count->s;
        if (n < 0)
            return false;

        const long long hard_limit = 16;
        if (n > hard_limit)
        {
            ccsim_known_memory_invalidate_symbol(memory, dst->symbol);
        }
        else
        {
            for (long long i = 0; i < n; ++i)
            {
                CcsimValue b = ccsim_make_known("u8", byte_u, (long long)byte_u, false);
                if (!ccsim_known_memory_set(memory, dst->symbol, dst->offset + i, "u8", b))
                    return false;
            }
        }

        if (strcmp(ret_type, "void") != 0)
        {
            *out_ret = *dst;
            *out_has_ret = true;
        }
        return true;
    }

    return false;
}

static bool ccsim_rewrite_line(char **lines, size_t line_count, size_t *rewrite_pos, const char *fmt, ...)
{
    if (!lines || !rewrite_pos || *rewrite_pos >= line_count)
        return false;
    va_list ap;
    va_start(ap, fmt);
    int needed = vsnprintf(NULL, 0, fmt, ap);
    va_end(ap);
    if (needed < 0)
        return false;
    char *buffer = (char *)malloc((size_t)needed + 1);
    if (!buffer)
        return false;
    va_start(ap, fmt);
    int written = vsnprintf(buffer, (size_t)needed + 1, fmt, ap);
    va_end(ap);
    if (written < 0)
    {
        free(buffer);
        return false;
    }
    free(lines[*rewrite_pos]);
    lines[*rewrite_pos] = buffer;
    ++(*rewrite_pos);
    return true;
}

static bool ccsim_try_parse_func_params_count(char **module_lines, size_t module_line_count,
                                              const char *function_name, size_t *out_params)
{
    if (!module_lines || !function_name || !out_params)
        return false;
    for (size_t i = 0; i < module_line_count; ++i)
    {
        char symbol[256] = {0};
        char ret_type[16] = {0};
        size_t params_count = 0;
        bool is_hidden = false;
        if (!ccsim_parse_func_header(module_lines[i], symbol, sizeof(symbol), ret_type, sizeof(ret_type), &params_count, &is_hidden))
            continue;
        if (strcmp(symbol, function_name) != 0)
            continue;
        *out_params = params_count;
        return true;
    }
    return false;
}

static bool ccsim_vm_apply_region_rewrite(char **function_lines, size_t function_line_count,
                                          size_t region_start, size_t region_end,
                                          const CcsimKnownMemoryList *memory,
                                          const CcsimKnownGlobalList *globals)
{
    if (!function_lines || region_end <= region_start)
        return false;

    size_t original_lines = region_end - region_start;
    size_t estimated_lines = 0;
    for (size_t i = 0; i < memory->count; ++i)
    {
        const CcsimKnownMemoryCell *cell = &memory->items[i];
        if (cell->value.kind != CCSIM_VAL_INT)
            return false;
        estimated_lines += (cell->offset == 0) ? 3 : 7;
    }
    for (size_t i = 0; i < globals->count; ++i)
    {
        if (globals->items[i].value.kind != CCSIM_VAL_INT)
            return false;
        estimated_lines += 2;
    }

    if (estimated_lines == 0 || estimated_lines >= original_lines)
    {
        ccsim_tracef("vm skip region [%zu,%zu): est=%zu original=%zu",
                     region_start, region_end, estimated_lines, original_lines);
        return false;
    }

    ccsim_tracef("vm collapse region [%zu,%zu): est=%zu original=%zu mem=%zu globals=%zu",
                 region_start, region_end, estimated_lines, original_lines,
                 memory->count, globals->count);

    size_t rewrite_pos = region_start;
    for (size_t i = 0; i < memory->count; ++i)
    {
        const CcsimKnownMemoryCell *cell = &memory->items[i];
        if (cell->offset == 0)
        {
            if (!ccsim_rewrite_line(function_lines, function_line_count, &rewrite_pos,
                                    "  addr_global %s", cell->symbol))
                return false;
        }
        else
        {
            if (!ccsim_rewrite_line(function_lines, function_line_count, &rewrite_pos,
                                    "  addr_global %s", cell->symbol) ||
                !ccsim_rewrite_line(function_lines, function_line_count, &rewrite_pos,
                                    "  convert bitcast ptr i64") ||
                !ccsim_rewrite_line(function_lines, function_line_count, &rewrite_pos,
                                    "  const i64 %lld", cell->offset) ||
                !ccsim_rewrite_line(function_lines, function_line_count, &rewrite_pos,
                                    "  binop add i64") ||
                !ccsim_rewrite_line(function_lines, function_line_count, &rewrite_pos,
                                    "  convert bitcast i64 ptr"))
                return false;
        }

        if (cell->value.is_signed)
        {
            if (!ccsim_rewrite_line(function_lines, function_line_count, &rewrite_pos,
                                    "  const %s %lld", cell->value.type, cell->value.s))
                return false;
        }
        else
        {
            if (!ccsim_rewrite_line(function_lines, function_line_count, &rewrite_pos,
                                    "  const %s %llu", cell->value.type, cell->value.u))
                return false;
        }

        if (!ccsim_rewrite_line(function_lines, function_line_count, &rewrite_pos,
                                "  store_indirect %s", cell->type))
            return false;
    }

    for (size_t i = 0; i < globals->count; ++i)
    {
        const CcsimKnownGlobal *g = &globals->items[i];
        if (g->value.is_signed)
        {
            if (!ccsim_rewrite_line(function_lines, function_line_count, &rewrite_pos,
                                    "  const %s %lld", g->value.type, g->value.s))
                return false;
        }
        else
        {
            if (!ccsim_rewrite_line(function_lines, function_line_count, &rewrite_pos,
                                    "  const %s %llu", g->value.type, g->value.u))
                return false;
        }
        if (!ccsim_rewrite_line(function_lines, function_line_count, &rewrite_pos,
                                "  store_global %s", g->symbol))
            return false;
    }

    while (rewrite_pos < region_end)
    {
        if (!ccsim_rewrite_line(function_lines, function_line_count, &rewrite_pos, "  nop"))
            return false;
    }
    ccsim_tracef("vm collapsed region [%zu,%zu): saved=%zu lines",
                 region_start, region_end, original_lines - estimated_lines);
    return true;
}

static int ccsim_vm_try_full_cfg_collapse(char **function_lines, size_t function_line_count,
                                          const char *function_name,
                                          char **module_lines, size_t module_line_count)
{
    CcsimLabelList labels = {0};
    CcsimStack stack = {0};
    CcsimLocal *locals = NULL;
    size_t local_capacity = 0;
    CcsimKnownGlobalList globals = {0};
    CcsimKnownMemoryList memory = {0};

    for (size_t i = 0; i < function_line_count; ++i)
    {
        char label[128] = {0};
        if (ccsim_parse_label_name(function_lines[i], label, sizeof(label)))
        {
            if (!ccsim_label_list_add(&labels, label, i))
                goto fail;
        }
    }

    size_t pc = 0;
    size_t steps = 0;
    const size_t step_limit = 200000;
    int reached_ret = 0;

    while (pc < function_line_count)
    {
        if (++steps > step_limit)
        {
            ccsim_tracef("vm full-cfg abort '%s': step limit", function_name);
            goto fail;
        }

        const char *line = ccsim_trim(function_lines[pc]);
        if (!line || *line == '\0' || strncmp(line, ".loc ", 5) == 0)
        {
            ++pc;
            continue;
        }

        char tmp_label[128] = {0};
        if (ccsim_parse_label_name(line, tmp_label, sizeof(tmp_label)))
        {
            ++pc;
            continue;
        }

        CcsimValue cval;
        if (ccsim_parse_const(line, &cval))
        {
            if (!ccsim_stack_push(&stack, cval))
                goto fail;
            ++pc;
            continue;
        }

        int local_index = -1;
        if (ccsim_parse_local_index(line, "load_local", &local_index))
        {
            if (local_index < 0 || !ccsim_local_ensure(&locals, &local_capacity, (size_t)local_index))
                goto fail;
            if (!ccsim_stack_push(&stack, locals[local_index].value))
                goto fail;
            ++pc;
            continue;
        }
        if (ccsim_parse_local_index(line, "store_local", &local_index))
        {
            if (local_index < 0 || !ccsim_local_ensure(&locals, &local_capacity, (size_t)local_index))
                goto fail;
            locals[local_index].value = ccsim_stack_pop(&stack);
            ++pc;
            continue;
        }
        if (ccsim_parse_local_index(line, "addr_local", &local_index))
            goto fail;

        char symbol_name[256] = {0};
        if (ccsim_parse_symbol_after_prefix(line, "addr_global ", symbol_name, sizeof(symbol_name)))
        {
            if (!ccsim_stack_push(&stack, ccsim_make_address(symbol_name, 0)))
                goto fail;
            ++pc;
            continue;
        }
        if (ccsim_parse_symbol_after_prefix(line, "load_global ", symbol_name, sizeof(symbol_name)))
        {
            if (!ccsim_stack_push(&stack, ccsim_known_globals_get(&globals, symbol_name)))
                goto fail;
            ++pc;
            continue;
        }
        if (ccsim_parse_symbol_after_prefix(line, "store_global ", symbol_name, sizeof(symbol_name)))
        {
            CcsimValue gv = ccsim_stack_pop(&stack);
            if (!ccsim_known_globals_set(&globals, symbol_name, gv))
                goto fail;
            ccsim_known_memory_invalidate_symbol(&memory, symbol_name);
            ++pc;
            continue;
        }

        if (strncmp(line, "load_param ", 11) == 0 || strncmp(line, "addr_param ", 11) == 0)
            goto fail;

        char kind[16] = {0};
        char from_type[16] = {0};
        char to_type[16] = {0};
        if (ccsim_parse_convert(line, kind, sizeof(kind), from_type, sizeof(from_type), to_type, sizeof(to_type)))
        {
            CcsimValue input = ccsim_stack_pop(&stack);
            bool ok = false;
            CcsimValue out = ccsim_eval_convert(kind, from_type, to_type, input, &ok);
            if (!ok)
                goto fail;
            if (!ccsim_stack_push(&stack, out))
                goto fail;
            ++pc;
            continue;
        }

        char op[16] = {0};
        char type_name[16] = {0};
        bool unsigned_hint = false;
        if (ccsim_parse_binop(line, op, sizeof(op), type_name, sizeof(type_name), &unsigned_hint))
        {
            CcsimValue rhs = ccsim_stack_pop(&stack);
            CcsimValue lhs = ccsim_stack_pop(&stack);
            bool ok = false;
            CcsimValue out = ccsim_eval_binop(op, type_name, unsigned_hint, lhs, rhs, &ok);
            if (!ok)
                goto fail;
            if (!ccsim_stack_push(&stack, out))
                goto fail;
            ++pc;
            continue;
        }
        if (ccsim_parse_unop(line, op, sizeof(op), type_name, sizeof(type_name)))
        {
            CcsimValue operand = ccsim_stack_pop(&stack);
            bool ok = false;
            CcsimValue out = ccsim_eval_unop(op, type_name, operand, &ok);
            if (!ok)
                goto fail;
            if (!ccsim_stack_push(&stack, out))
                goto fail;
            ++pc;
            continue;
        }
        if (ccsim_parse_compare(line, op, sizeof(op), type_name, sizeof(type_name), &unsigned_hint))
        {
            CcsimValue rhs = ccsim_stack_pop(&stack);
            CcsimValue lhs = ccsim_stack_pop(&stack);
            bool ok = false;
            CcsimValue out = ccsim_eval_compare(op, type_name, unsigned_hint, lhs, rhs, &ok);
            if (!ok)
                goto fail;
            if (!ccsim_stack_push(&stack, out))
                goto fail;
            ++pc;
            continue;
        }
        if (ccsim_parse_test_null(line, type_name, sizeof(type_name)))
        {
            CcsimValue input = ccsim_stack_pop(&stack);
            if (input.kind == CCSIM_VAL_INT && strcmp(input.type, "ptr") == 0)
            {
                if (!ccsim_stack_push(&stack, ccsim_make_known("i1", input.u == 0 ? 1ULL : 0ULL,
                                                               input.u == 0 ? 1LL : 0LL, true)))
                    goto fail;
                ++pc;
                continue;
            }
            goto fail;
        }

        char mem_type[16] = {0};
        if (ccsim_parse_memory_type_after_prefix(line, "load_indirect ", mem_type, sizeof(mem_type)))
        {
            CcsimValue addr = ccsim_stack_pop(&stack);
            if (addr.kind != CCSIM_VAL_ADDRESS || addr.symbol[0] == '\0')
                goto fail;
            CcsimValue loaded = ccsim_known_memory_get(&memory, addr.symbol, addr.offset, mem_type);
            if (loaded.kind == CCSIM_VAL_UNKNOWN)
                goto fail;
            if (!ccsim_stack_push(&stack, loaded))
                goto fail;
            ++pc;
            continue;
        }
        if (ccsim_parse_memory_type_after_prefix(line, "store_indirect ", mem_type, sizeof(mem_type)))
        {
            CcsimValue value = ccsim_stack_pop(&stack);
            CcsimValue addr = ccsim_stack_pop(&stack);
            if (addr.kind != CCSIM_VAL_ADDRESS || addr.symbol[0] == '\0')
                goto fail;
            if (value.kind != CCSIM_VAL_INT || strcmp(value.type, mem_type) != 0)
                goto fail;
            if (!ccsim_known_memory_set(&memory, addr.symbol, addr.offset, mem_type, value))
                goto fail;
            ++pc;
            continue;
        }

        if (strncmp(line, "dup", 3) == 0)
        {
            CcsimValue top = ccsim_stack_peek(&stack);
            if (!ccsim_stack_push(&stack, top))
                goto fail;
            ++pc;
            continue;
        }
        if (strncmp(line, "drop", 4) == 0)
        {
            (void)ccsim_stack_pop(&stack);
            ++pc;
            continue;
        }

        char jump_target[128] = {0};
        if (ccsim_parse_jump_target(line, jump_target, sizeof(jump_target)))
        {
            size_t target = 0;
            if (!ccsim_label_list_find(&labels, jump_target, &target))
                goto fail;
            pc = target;
            continue;
        }

        char true_label[128] = {0};
        char false_label[128] = {0};
        if (ccsim_parse_branch_targets(line, true_label, sizeof(true_label), false_label, sizeof(false_label)))
        {
            CcsimValue cond = ccsim_stack_pop(&stack);
            if (cond.kind != CCSIM_VAL_INT)
                goto fail;
            size_t target = 0;
            if (!ccsim_label_list_find(&labels, cond.u ? true_label : false_label, &target))
                goto fail;
            pc = target;
            continue;
        }

        if (strncmp(line, "ret", 3) == 0)
        {
            reached_ret = 1;
            break;
        }

        if (strncmp(line, "call ", 5) == 0 || strncmp(line, "call_indirect", 13) == 0)
        {
            int arg_count = 0;
            int is_indirect = 0;
            if (!ccsim_parse_call_effects(line, &arg_count, &is_indirect))
                goto fail;
            if (is_indirect)
                goto fail;

            char call_symbol[256] = {0};
            char call_ret_type[16] = {0};
            bool is_zero_arg = false;
            if (!ccsim_parse_call_line(line, call_symbol, sizeof(call_symbol),
                                       call_ret_type, sizeof(call_ret_type),
                                       &is_zero_arg))
                goto fail;

            size_t declared_params = 0;
            if (ccsim_try_parse_func_params_count(module_lines, module_line_count,
                                                  call_symbol, &declared_params))
                arg_count = (int)declared_params;

            int to_pop = arg_count;
            if (to_pop > (int)stack.count)
                to_pop = (int)stack.count;
            CcsimValue *args = NULL;
            if (to_pop > 0)
            {
                args = (CcsimValue *)malloc((size_t)to_pop * sizeof(CcsimValue));
                if (!args)
                    goto fail;
                if (!ccsim_collect_call_args_from_stack(&stack, (size_t)to_pop, args))
                {
                    free(args);
                    goto fail;
                }
            }

            CcsimValue call_ret = ccsim_make_unknown();
            bool has_ret = false;
            bool emulated = ccsim_emulate_intrinsic_call(call_symbol, call_ret_type,
                                                         args, (size_t)to_pop,
                                                         &memory, &call_ret, &has_ret);
            if (!emulated)
                emulated = ccsim_eval_hidden_pure_call(module_lines, module_line_count,
                                                       call_symbol,
                                                       args, (size_t)to_pop,
                                                       &call_ret);
            free(args);
            if (!emulated)
                goto fail;

            for (int i = 0; i < to_pop; ++i)
                (void)ccsim_stack_pop(&stack);

            if (strcmp(call_ret_type, "void") != 0)
            {
                if (call_ret.kind == CCSIM_VAL_UNKNOWN)
                {
                    strncpy(call_ret.type, call_ret_type, sizeof(call_ret.type) - 1);
                    call_ret.type[sizeof(call_ret.type) - 1] = '\0';
                }
                if (!ccsim_stack_push(&stack, call_ret))
                    goto fail;
            }
            ++pc;
            continue;
        }

        goto fail;
    }

    if (!reached_ret || stack.count != 0)
        goto fail;

    if (!ccsim_vm_apply_region_rewrite(function_lines, function_line_count,
                                       0, function_line_count,
                                       &memory, &globals))
        goto fail;

    ccsim_tracef("vm full-cfg collapsed '%s'", function_name);
    ccsim_label_list_free(&labels);
    ccsim_stack_free(&stack);
    free(locals);
    ccsim_known_globals_free(&globals);
    ccsim_known_memory_free(&memory);
    return 1;

fail:
    ccsim_label_list_free(&labels);
    ccsim_stack_free(&stack);
    free(locals);
    ccsim_known_globals_free(&globals);
    ccsim_known_memory_free(&memory);
    return 0;
}

int ccsim_vm_collapse_hidden_function(char **function_lines, size_t function_line_count,
                                      const char *function_name,
                                      char **module_lines, size_t module_line_count,
                                      const CcsimOptions *options,
                                      CcsimStats *stats)
{
    if (!function_lines || function_line_count == 0 || !function_name || !*function_name)
        return 0;
    if (!options || options->opt_level < 3 || !options->aggressive)
        return 0;

    ccsim_tracef("vm begin '%s' lines=%zu", function_name, function_line_count);

    if (ccsim_vm_try_full_cfg_collapse(function_lines, function_line_count, function_name,
                                       module_lines, module_line_count))
    {
        if (stats)
            stats->vm_collapsed_functions++;
        return 1;
    }
    ccsim_tracef("vm full-cfg fallback to region mode for '%s'", function_name);

    (void)module_lines;
    (void)module_line_count;

    int collapsed_any = 0;
    CcsimStack stack = {0};
    CcsimLocal *locals = NULL;
    size_t local_capacity = 0;
    CcsimKnownGlobalList globals = {0};
    CcsimKnownMemoryList memory = {0};
    size_t region_start = 0;
    int region_invalid = 0;

#define CCSIM_VM_FLUSH_REGION(BARRIER_INDEX)                                                        \
    do                                                                                                \
    {                                                                                                 \
        size_t region_end = (BARRIER_INDEX);                                                         \
        ccsim_tracef("vm flush at line=%zu region=[%zu,%zu) invalid=%d stack=%zu",                 \
                     (size_t)(BARRIER_INDEX), region_start, region_end, region_invalid, stack.count); \
        if (!region_invalid && stack.count == 0 && region_end > region_start)                    \
        {                                                                                             \
            if (ccsim_vm_apply_region_rewrite(function_lines, function_line_count,                   \
                                              region_start, region_end, &memory, &globals))          \
            {                                                                                         \
                collapsed_any = 1;                                                                    \
                if (stats)                                                                            \
                    stats->vm_collapsed_functions++;                                                  \
            }                                                                                         \
        }                                                                                             \
        ccsim_stack_clear(&stack);                                                                    \
        ccsim_known_globals_free(&globals);                                                           \
        ccsim_known_memory_free(&memory);                                                             \
        for (size_t __li = 0; __li < local_capacity; ++__li)                                         \
        {                                                                                             \
            locals[__li].value = ccsim_make_unknown();                                                \
            locals[__li].escaped = 0;                                                                 \
        }                                                                                             \
        region_invalid = 0;                                                                           \
        region_start = (BARRIER_INDEX) + 1;                                                           \
    } while (0)

    for (size_t pc = 0; pc < function_line_count; ++pc)
    {
        const char *line = ccsim_trim(function_lines[pc]);
        if (!line || *line == '\0' || strncmp(line, ".loc ", 5) == 0)
            continue;

        char tmp_label[128] = {0};
        if (ccsim_parse_label_name(line, tmp_label, sizeof(tmp_label)))
        {
            ccsim_tracef("vm barrier label at line=%zu (%s)", pc, tmp_label);
            CCSIM_VM_FLUSH_REGION(pc);
            continue;
        }

        CcsimValue cval;
        if (ccsim_parse_const(line, &cval))
        {
            if (!ccsim_stack_push(&stack, cval))
                region_invalid = 1;
            else
                ccsim_tracef("vm push const at line=%zu type=%s", pc, cval.type);
            continue;
        }

        int local_index = -1;
        if (ccsim_parse_local_index(line, "load_local", &local_index))
        {
            if (local_index < 0 || !ccsim_local_ensure(&locals, &local_capacity, (size_t)local_index))
            {
                region_invalid = 1;
                continue;
            }
            if (!ccsim_stack_push(&stack, locals[local_index].value))
                region_invalid = 1;
            else
                ccsim_tracef("vm load_local %d at line=%zu", local_index, pc);
            continue;
        }

        if (ccsim_parse_local_index(line, "store_local", &local_index))
        {
            if (local_index < 0 || !ccsim_local_ensure(&locals, &local_capacity, (size_t)local_index))
            {
                region_invalid = 1;
                continue;
            }
            locals[local_index].value = ccsim_stack_pop(&stack);
            ccsim_tracef("vm store_local %d at line=%zu", local_index, pc);
            continue;
        }

        if (ccsim_parse_local_index(line, "addr_local", &local_index))
        {
            if (!ccsim_stack_push(&stack, ccsim_make_unknown()))
                region_invalid = 1;
            else
                ccsim_tracef("vm addr_local %d at line=%zu -> unknown", local_index, pc);
            continue;
        }

        char symbol_name[256] = {0};
        if (ccsim_parse_symbol_after_prefix(line, "addr_global ", symbol_name, sizeof(symbol_name)))
        {
            if (!ccsim_stack_push(&stack, ccsim_make_address(symbol_name, 0)))
                region_invalid = 1;
            else
                ccsim_tracef("vm addr_global %s at line=%zu", symbol_name, pc);
            continue;
        }

        if (ccsim_parse_symbol_after_prefix(line, "load_global ", symbol_name, sizeof(symbol_name)))
        {
            if (!ccsim_stack_push(&stack, ccsim_known_globals_get(&globals, symbol_name)))
                region_invalid = 1;
            else
                ccsim_tracef("vm load_global %s at line=%zu", symbol_name, pc);
            continue;
        }

        if (ccsim_parse_symbol_after_prefix(line, "store_global ", symbol_name, sizeof(symbol_name)))
        {
            CcsimValue gv = ccsim_stack_pop(&stack);
            if (!ccsim_known_globals_set(&globals, symbol_name, gv))
                region_invalid = 1;
            ccsim_known_memory_invalidate_symbol(&memory, symbol_name);
            ccsim_tracef("vm store_global %s at line=%zu", symbol_name, pc);
            continue;
        }

        if (strncmp(line, "load_param ", 11) == 0 || strncmp(line, "addr_param ", 11) == 0)
        {
            if (!ccsim_stack_push(&stack, ccsim_make_unknown()))
                region_invalid = 1;
            else
                ccsim_tracef("vm param op at line=%zu -> unknown", pc);
            continue;
        }

        char kind[16] = {0};
        char from_type[16] = {0};
        char to_type[16] = {0};
        if (ccsim_parse_convert(line, kind, sizeof(kind), from_type, sizeof(from_type), to_type, sizeof(to_type)))
        {
            CcsimValue input = ccsim_stack_pop(&stack);
            bool ok = false;
            CcsimValue out = ccsim_eval_convert(kind, from_type, to_type, input, &ok);
            if (!ok)
                ccsim_tracef("vm convert unknown at line=%zu kind=%s %s->%s", pc, kind, from_type, to_type);
            if (!ccsim_stack_push(&stack, out))
                region_invalid = 1;
            continue;
        }

        char op[16] = {0};
        char type_name[16] = {0};
        bool unsigned_hint = false;
        if (ccsim_parse_binop(line, op, sizeof(op), type_name, sizeof(type_name), &unsigned_hint))
        {
            CcsimValue rhs = ccsim_stack_pop(&stack);
            CcsimValue lhs = ccsim_stack_pop(&stack);
            bool ok = false;
            CcsimValue out = ccsim_eval_binop(op, type_name, unsigned_hint, lhs, rhs, &ok);
            if (!ok)
                ccsim_tracef("vm binop unknown at line=%zu op=%s type=%s", pc, op, type_name);
            if (!ccsim_stack_push(&stack, out))
                region_invalid = 1;
            continue;
        }

        if (ccsim_parse_unop(line, op, sizeof(op), type_name, sizeof(type_name)))
        {
            CcsimValue operand = ccsim_stack_pop(&stack);
            bool ok = false;
            CcsimValue out = ccsim_eval_unop(op, type_name, operand, &ok);
            if (!ok)
                ccsim_tracef("vm unop unknown at line=%zu op=%s type=%s", pc, op, type_name);
            if (!ccsim_stack_push(&stack, out))
                region_invalid = 1;
            continue;
        }

        if (ccsim_parse_compare(line, op, sizeof(op), type_name, sizeof(type_name), &unsigned_hint))
        {
            CcsimValue rhs = ccsim_stack_pop(&stack);
            CcsimValue lhs = ccsim_stack_pop(&stack);
            bool ok = false;
            CcsimValue out = ccsim_eval_compare(op, type_name, unsigned_hint, lhs, rhs, &ok);
            if (!ok)
                ccsim_tracef("vm compare unknown at line=%zu op=%s type=%s", pc, op, type_name);
            if (!ccsim_stack_push(&stack, out))
                region_invalid = 1;
            continue;
        }

        if (ccsim_parse_test_null(line, type_name, sizeof(type_name)))
        {
            CcsimValue input = ccsim_stack_pop(&stack);
            if (input.kind == CCSIM_VAL_INT && strcmp(input.type, "ptr") == 0)
            {
                if (!ccsim_stack_push(&stack, ccsim_make_known("i1", input.u == 0 ? 1ULL : 0ULL,
                                                               input.u == 0 ? 1LL : 0LL, true)))
                    region_invalid = 1;
                continue;
            }
            if (!ccsim_stack_push(&stack, ccsim_make_unknown()))
                region_invalid = 1;
            else
                ccsim_tracef("vm test_null unknown at line=%zu", pc);
            continue;
        }

        char mem_type[16] = {0};
        if (ccsim_parse_memory_type_after_prefix(line, "load_indirect ", mem_type, sizeof(mem_type)))
        {
            CcsimValue addr = ccsim_stack_pop(&stack);
            if (addr.kind != CCSIM_VAL_ADDRESS || addr.symbol[0] == '\0')
            {
                if (!ccsim_stack_push(&stack, ccsim_make_unknown()))
                    region_invalid = 1;
                continue;
            }
            CcsimValue loaded = ccsim_known_memory_get(&memory, addr.symbol, addr.offset, mem_type);
            if (loaded.kind == CCSIM_VAL_UNKNOWN)
            {
                if (!ccsim_stack_push(&stack, ccsim_make_unknown()))
                    region_invalid = 1;
                else
                    ccsim_tracef("vm load_indirect unknown %s at line=%zu from %s+%lld", mem_type, pc, addr.symbol, addr.offset);
                continue;
            }
            if (!ccsim_stack_push(&stack, loaded))
                region_invalid = 1;
            else
                ccsim_tracef("vm load_indirect %s at line=%zu from %s+%lld", mem_type, pc, addr.symbol, addr.offset);
            continue;
        }

        if (ccsim_parse_memory_type_after_prefix(line, "store_indirect ", mem_type, sizeof(mem_type)))
        {
            CcsimValue value = ccsim_stack_pop(&stack);
            CcsimValue addr = ccsim_stack_pop(&stack);
            if (addr.kind != CCSIM_VAL_ADDRESS || addr.symbol[0] == '\0')
            {
                ccsim_tracef("vm store_indirect unknown addr at line=%zu", pc);
                continue;
            }
            if (value.kind != CCSIM_VAL_INT || strcmp(value.type, mem_type) != 0)
            {
                ccsim_known_memory_remove(&memory, addr.symbol, addr.offset, mem_type);
                ccsim_tracef("vm store_indirect unknown value %s at line=%zu to %s+%lld", mem_type, pc, addr.symbol, addr.offset);
                continue;
            }
            if (!ccsim_known_memory_set(&memory, addr.symbol, addr.offset, mem_type, value))
                region_invalid = 1;
            else
                ccsim_tracef("vm store_indirect %s at line=%zu to %s+%lld", mem_type, pc, addr.symbol, addr.offset);
            continue;
        }

        if (strncmp(line, "dup", 3) == 0)
        {
            CcsimValue top = ccsim_stack_peek(&stack);
            if (!ccsim_stack_push(&stack, top))
                region_invalid = 1;
            continue;
        }

        if (strncmp(line, "drop", 4) == 0)
        {
            (void)ccsim_stack_pop(&stack);
            ccsim_tracef("vm drop at line=%zu", pc);
            continue;
        }

        if (strncmp(line, "const_str ", 10) == 0)
        {
            CcsimValue str_ptr = ccsim_make_unknown();
            str_ptr.kind = CCSIM_VAL_ADDRESS;
            strncpy(str_ptr.symbol, "<const_str>", sizeof(str_ptr.symbol) - 1);
            str_ptr.symbol[sizeof(str_ptr.symbol) - 1] = '\0';
            str_ptr.offset = 0;
            if (!ccsim_stack_push(&stack, str_ptr))
                region_invalid = 1;
            else
                ccsim_tracef("vm const_str at line=%zu", pc);
            continue;
        }

        if (ccsim_parse_branch_targets(line, tmp_label, sizeof(tmp_label), symbol_name, sizeof(symbol_name)))
        {
            if (stack.count > 0)
            {
                (void)ccsim_stack_pop(&stack);
                ccsim_tracef("vm prepare branch at line=%zu", pc);
            }
            ccsim_tracef("vm barrier control/call at line=%zu", pc);
            CCSIM_VM_FLUSH_REGION(pc);
            continue;
        }

        if (strncmp(line, "call ", 5) == 0 || strncmp(line, "call_indirect", 13) == 0)
        {
            int arg_count = 0;
            int is_indirect = 0;
            if (ccsim_parse_call_effects(line, &arg_count, &is_indirect))
            {
                char call_symbol[256] = {0};
                char call_ret_type[16] = {0};
                bool is_zero_arg = false;
                bool is_direct = ccsim_parse_call_line(line, call_symbol, sizeof(call_symbol),
                                                       call_ret_type, sizeof(call_ret_type),
                                                       &is_zero_arg);

                if (!is_indirect && is_direct)
                {
                    size_t declared_params = 0;
                    if (ccsim_try_parse_func_params_count(module_lines, module_line_count,
                                                          call_symbol, &declared_params))
                        arg_count = (int)declared_params;
                }

                if (!is_indirect && is_direct)
                {
                    int to_pop = arg_count;
                    CcsimValue *args = NULL;
                    if (to_pop > 0)
                        args = (CcsimValue *)malloc((size_t)to_pop * sizeof(CcsimValue));

                    CcsimValue call_ret = ccsim_make_unknown();
                    bool emulated = false;
                    bool has_args = (to_pop == 0) || (args && ccsim_collect_call_args_from_stack(&stack, (size_t)to_pop, args));
                    bool has_ret = false;

                    if (has_args &&
                        ccsim_emulate_intrinsic_call(call_symbol, call_ret_type,
                                                    args, (size_t)to_pop,
                                                    &memory, &call_ret, &has_ret))
                    {
                        emulated = true;
                        for (int i = 0; i < to_pop; ++i)
                            (void)ccsim_stack_pop(&stack);
                        if (has_ret)
                        {
                            if (!ccsim_stack_push(&stack, call_ret))
                                region_invalid = 1;
                        }
                        ccsim_tracef("vm emulated intrinsic call %s at line=%zu args=%d", call_symbol, pc, to_pop);
                    }
                    else if (has_args &&
                        ccsim_eval_hidden_pure_call(module_lines, module_line_count,
                                                    call_symbol,
                                                    args, (size_t)to_pop,
                                                    &call_ret))
                    {
                        emulated = true;
                        for (int i = 0; i < to_pop; ++i)
                            (void)ccsim_stack_pop(&stack);
                        if (strcmp(call_ret_type, "void") != 0)
                        {
                            if (call_ret.kind == CCSIM_VAL_UNKNOWN)
                            {
                                strncpy(call_ret.type, call_ret_type, sizeof(call_ret.type) - 1);
                                call_ret.type[sizeof(call_ret.type) - 1] = '\0';
                            }
                            if (!ccsim_stack_push(&stack, call_ret))
                                region_invalid = 1;
                        }
                        ccsim_tracef("vm emulated call %s at line=%zu args=%d", call_symbol, pc, to_pop);
                    }
                    free(args);

                    if (emulated)
                        continue;
                }

                int to_pop = arg_count + (is_indirect ? 1 : 0);
                if (to_pop > (int)stack.count)
                    to_pop = (int)stack.count;
                for (int i = 0; i < to_pop; ++i)
                    (void)ccsim_stack_pop(&stack);
                ccsim_tracef("vm prepare call at line=%zu pop=%d", pc, to_pop);
            }
            ccsim_tracef("vm barrier control/call at line=%zu", pc);
            CCSIM_VM_FLUSH_REGION(pc);
            continue;
        }

        if (ccsim_parse_jump_target(line, tmp_label, sizeof(tmp_label)) ||
            strncmp(line, "ret", 3) == 0)
        {
            ccsim_tracef("vm barrier control/call at line=%zu", pc);
            CCSIM_VM_FLUSH_REGION(pc);
            continue;
        }

        ccsim_tracef("vm unsupported instruction at line=%zu: %s", pc, line);
        CCSIM_VM_FLUSH_REGION(pc);
    }

    if (region_start < function_line_count)
    {
        size_t region_end = function_line_count;
        if (!region_invalid && stack.count == 0 && region_end > region_start)
        {
            if (ccsim_vm_apply_region_rewrite(function_lines, function_line_count,
                                              region_start, region_end, &memory, &globals))
            {
                collapsed_any = 1;
                if (stats)
                    stats->vm_collapsed_functions++;
            }
        }
    }

#undef CCSIM_VM_FLUSH_REGION

    ccsim_stack_free(&stack);
    free(locals);
    ccsim_known_globals_free(&globals);
    ccsim_known_memory_free(&memory);
    ccsim_tracef("vm end '%s' collapsed=%d", function_name, collapsed_any);
    return collapsed_any;
}

static bool ccsim_parse_local_index(const char *line, const char *prefix, int *out_index)
{
    if (!line || !prefix || !out_index)
        return false;
    line = ccsim_trim(line);
    size_t len = strlen(prefix);
    if (strncmp(line, prefix, len) != 0)
        return false;
    const char *cursor = line + len;
    while (*cursor == ' ' || *cursor == '\t')
        ++cursor;
    if (*cursor == '\0')
        return false;
    char *endptr = NULL;
    long value = strtol(cursor, &endptr, 10);
    if (endptr == cursor || (*endptr && !isspace((unsigned char)*endptr)))
        return false;
    *out_index = (int)value;
    return true;
}

static bool ccsim_parse_const(const char *line, CcsimValue *out)
{
    if (!line || !out)
        return false;
    line = ccsim_trim(line);
    if (strncmp(line, "const ", 6) != 0)
        return false;
    line += 6;
    if (strncmp(line, "str ", 4) == 0)
        return false;

    char type_name[16] = {0};
    char value_text[128] = {0};
    if (sscanf(line, "%15s %127s", type_name, value_text) != 2)
        return false;

    unsigned width = ccsim_type_bits(type_name);
    if (width == 0)
        return false;

    bool is_ptr = ccsim_type_is_ptr(type_name);
    if (!ccsim_type_is_integer(type_name) && !is_ptr)
        return false;

    bool is_signed = ccsim_type_is_signed(type_name);
    unsigned long long uval = 0;
    long long sval = 0;

    if (is_ptr && strcmp(value_text, "null") == 0)
    {
        uval = 0;
        sval = 0;
    }
    else if (is_signed)
    {
        char *endptr = NULL;
        errno = 0;
        sval = strtoll(value_text, &endptr, 0);
        if (endptr == value_text || *endptr != '\0' || errno == ERANGE)
            return false;
        uval = (unsigned long long)sval;
    }
    else
    {
        char *endptr = NULL;
        errno = 0;
        uval = strtoull(value_text, &endptr, 0);
        if (endptr == value_text || *endptr != '\0' || errno == ERANGE)
            return false;
        sval = (long long)uval;
    }

    *out = ccsim_make_known(type_name, uval, sval, is_signed);
    return true;
}

static bool ccsim_parse_binop(const char *line, char *op, size_t op_sz, char *type_name, size_t type_sz, bool *unsigned_hint)
{
    if (!line || !op || !type_name || !unsigned_hint)
        return false;
    line = ccsim_trim(line);
    if (strncmp(line, "binop ", 6) != 0)
        return false;
    line += 6;

    char op_buf[16] = {0};
    char ty_buf[16] = {0};
    char extra[16] = {0};
    int parsed = sscanf(line, "%15s %15s %15s", op_buf, ty_buf, extra);
    if (parsed < 2)
        return false;

    strncpy(op, op_buf, op_sz - 1);
    op[op_sz - 1] = '\0';
    strncpy(type_name, ty_buf, type_sz - 1);
    type_name[type_sz - 1] = '\0';
    *unsigned_hint = (parsed >= 3 && strcmp(extra, "unsigned") == 0);
    return true;
}

static bool ccsim_parse_unop(const char *line, char *op, size_t op_sz, char *type_name, size_t type_sz)
{
    if (!line || !op || !type_name)
        return false;
    line = ccsim_trim(line);
    if (strncmp(line, "unop ", 5) != 0)
        return false;
    line += 5;

    char op_buf[16] = {0};
    char ty_buf[16] = {0};
    if (sscanf(line, "%15s %15s", op_buf, ty_buf) != 2)
        return false;

    strncpy(op, op_buf, op_sz - 1);
    op[op_sz - 1] = '\0';
    strncpy(type_name, ty_buf, type_sz - 1);
    type_name[type_sz - 1] = '\0';
    return true;
}

static bool ccsim_parse_compare(const char *line, char *op, size_t op_sz, char *type_name, size_t type_sz, bool *unsigned_hint)
{
    if (!line || !op || !type_name || !unsigned_hint)
        return false;
    line = ccsim_trim(line);
    if (strncmp(line, "compare ", 8) != 0)
        return false;
    line += 8;

    char op_buf[16] = {0};
    char ty_buf[16] = {0};
    char extra[16] = {0};
    int parsed = sscanf(line, "%15s %15s %15s", op_buf, ty_buf, extra);
    if (parsed < 2)
        return false;

    strncpy(op, op_buf, op_sz - 1);
    op[op_sz - 1] = '\0';
    strncpy(type_name, ty_buf, type_sz - 1);
    type_name[type_sz - 1] = '\0';
    *unsigned_hint = (parsed >= 3 && strcmp(extra, "unsigned") == 0);
    return true;
}

static bool ccsim_parse_convert(const char *line, char *kind, size_t kind_sz, char *from_type, size_t from_sz, char *to_type, size_t to_sz)
{
    if (!line || !kind || !from_type || !to_type)
        return false;
    line = ccsim_trim(line);
    if (strncmp(line, "convert ", 8) != 0)
        return false;
    line += 8;

    char kind_buf[16] = {0};
    char from_buf[16] = {0};
    char to_buf[16] = {0};
    if (sscanf(line, "%15s %15s %15s", kind_buf, from_buf, to_buf) != 3)
        return false;

    strncpy(kind, kind_buf, kind_sz - 1);
    kind[kind_sz - 1] = '\0';
    strncpy(from_type, from_buf, from_sz - 1);
    from_type[from_sz - 1] = '\0';
    strncpy(to_type, to_buf, to_sz - 1);
    to_type[to_sz - 1] = '\0';
    return true;
}

static bool ccsim_parse_test_null(const char *line, char *type_name, size_t type_sz)
{
    if (!line || !type_name)
        return false;
    line = ccsim_trim(line);
    if (strncmp(line, "test_null", 9) != 0)
        return false;
    line += 9;
    while (*line == ' ' || *line == '\t')
        ++line;
    if (*line == '\0')
        return false;

    char ty_buf[16] = {0};
    if (sscanf(line, "%15s", ty_buf) != 1)
        return false;

    strncpy(type_name, ty_buf, type_sz - 1);
    type_name[type_sz - 1] = '\0';
    return true;
}

static bool ccsim_replace_linef(char **lines, size_t line_count, size_t index, const char *fmt, ...)
{
    if (!lines || !fmt || index >= line_count)
        return false;

    va_list ap;
    va_start(ap, fmt);
    int needed = vsnprintf(NULL, 0, fmt, ap);
    va_end(ap);
    if (needed < 0)
        return false;

    char *buffer = (char *)malloc((size_t)needed + 1);
    if (!buffer)
        return false;

    va_start(ap, fmt);
    int written = vsnprintf(buffer, (size_t)needed + 1, fmt, ap);
    va_end(ap);
    if (written < 0)
    {
        free(buffer);
        return false;
    }

    free(lines[index]);
    lines[index] = buffer;
    return true;
}

static bool ccsim_line_is_exact(const char *line, const char *text)
{
    if (!line || !text)
        return false;
    line = ccsim_trim(line);
    return strcmp(line, text) == 0;
}

static bool ccsim_try_fold_duplicate_addr_chain(char **lines, size_t line_count, size_t i)
{
    if (!lines || i + 9 >= line_count)
        return false;

    char sym1[256] = {0};
    char sym2[256] = {0};
    if (!ccsim_parse_symbol_after_prefix(lines[i], "addr_global ", sym1, sizeof(sym1)))
        return false;
    if (!ccsim_line_is_exact(lines[i + 1], "convert bitcast ptr i64"))
        return false;
    if (!ccsim_line_is_exact(lines[i + 3], "binop add i64"))
        return false;
    if (!ccsim_line_is_exact(lines[i + 4], "convert bitcast i64 ptr"))
        return false;

    if (!ccsim_parse_symbol_after_prefix(lines[i + 5], "addr_global ", sym2, sizeof(sym2)))
        return false;
    if (strcmp(sym1, sym2) != 0)
        return false;
    if (!ccsim_line_is_exact(lines[i + 6], "convert bitcast ptr i64"))
        return false;
    if (!ccsim_line_is_exact(lines[i + 8], "binop add i64"))
        return false;
    if (!ccsim_line_is_exact(lines[i + 9], "convert bitcast i64 ptr"))
        return false;

    CcsimValue c1;
    CcsimValue c2;
    if (!ccsim_parse_const(lines[i + 2], &c1) || !ccsim_parse_const(lines[i + 7], &c2))
        return false;
    if (c1.kind != CCSIM_VAL_INT || c2.kind != CCSIM_VAL_INT)
        return false;
    if (strcmp(c1.type, "i64") != 0 || strcmp(c2.type, "i64") != 0)
        return false;
    if (c1.s != c2.s)
        return false;

    if (!ccsim_replace_linef(lines, line_count, i + 5, "  dup ptr"))
        return false;
    for (size_t k = i + 6; k <= i + 9; ++k)
    {
        if (!ccsim_replace_linef(lines, line_count, k, "  nop"))
            return false;
    }
    return true;
}

static void ccsim_const_callee_list_free(CcsimConstCalleeList *list)
{
    if (!list)
        return;
    free(list->items);
    list->items = NULL;
    list->count = 0;
    list->capacity = 0;
}

static bool ccsim_const_callee_list_append(CcsimConstCalleeList *list, const char *symbol, CcsimValue value)
{
    if (!list || !symbol || !*symbol)
        return false;

    for (size_t i = 0; i < list->count; ++i)
    {
        if (strcmp(list->items[i].symbol, symbol) == 0)
        {
            list->items[i].value = value;
            return true;
        }
    }

    if (list->count == list->capacity)
    {
        size_t new_capacity = list->capacity ? list->capacity * 2 : 16;
        CcsimConstCallee *new_items = (CcsimConstCallee *)realloc(list->items, new_capacity * sizeof(CcsimConstCallee));
        if (!new_items)
            return false;
        list->items = new_items;
        list->capacity = new_capacity;
    }

    CcsimConstCallee *slot = &list->items[list->count++];
    memset(slot, 0, sizeof(*slot));
    strncpy(slot->symbol, symbol, sizeof(slot->symbol) - 1);
    slot->symbol[sizeof(slot->symbol) - 1] = '\0';
    slot->value = value;
    return true;
}

static const CcsimConstCallee *ccsim_const_callee_find(const CcsimConstCalleeList *list, const char *symbol)
{
    if (!list || !symbol)
        return NULL;
    for (size_t i = 0; i < list->count; ++i)
    {
        if (strcmp(list->items[i].symbol, symbol) == 0)
            return &list->items[i];
    }
    return NULL;
}

static bool ccsim_parse_func_header(const char *line,
                                    char *symbol, size_t symbol_sz,
                                    char *ret_type, size_t ret_type_sz,
                                    size_t *params_count,
                                    bool *is_hidden)
{
    if (!line || !symbol || !ret_type || !params_count || !is_hidden)
        return false;
    line = ccsim_trim(line);
    if (strncmp(line, ".func ", 6) != 0)
        return false;
    line += 6;

    char sym[256] = {0};
    size_t pcount = 0;
    char rtype[16] = {0};
    if (sscanf(line, "%255s ret=%15s params=%zu", sym, rtype, &pcount) < 3)
        return false;

    bool hidden = strstr(line, " hidden") != NULL;

    strncpy(symbol, sym, symbol_sz - 1);
    symbol[symbol_sz - 1] = '\0';
    strncpy(ret_type, rtype, ret_type_sz - 1);
    ret_type[ret_type_sz - 1] = '\0';
    *params_count = pcount;
    *is_hidden = hidden;
    return true;
}

static bool ccsim_parse_call_line(const char *line,
                                  char *symbol, size_t symbol_sz,
                                  char *ret_type, size_t ret_type_sz,
                                  bool *is_zero_arg)
{
    if (!line || !symbol || !ret_type || !is_zero_arg)
        return false;
    line = ccsim_trim(line);
    if (strncmp(line, "call ", 5) != 0)
        return false;
    line += 5;

    char sym[256] = {0};
    char rtype[16] = {0};
    char args[256] = {0};
    if (sscanf(line, "%255s %15s %255s", sym, rtype, args) < 3)
        return false;

    strncpy(symbol, sym, symbol_sz - 1);
    symbol[symbol_sz - 1] = '\0';
    strncpy(ret_type, rtype, ret_type_sz - 1);
    ret_type[ret_type_sz - 1] = '\0';
    *is_zero_arg = (strcmp(args, "()") == 0);
    return true;
}

static bool ccsim_find_function_range(char **module_lines, size_t module_line_count,
                                      const char *function_name,
                                      size_t *out_body_start,
                                      size_t *out_endfunc_line,
                                      char *ret_type, size_t ret_type_sz,
                                      size_t *out_params_count,
                                      bool *out_is_hidden)
{
    if (!module_lines || !function_name || !out_body_start || !out_endfunc_line ||
        !ret_type || !out_params_count || !out_is_hidden)
        return false;

    for (size_t i = 0; i < module_line_count; ++i)
    {
        char sym[256] = {0};
        char rtype[16] = {0};
        size_t pcount = 0;
        bool is_hidden = false;
        if (!ccsim_parse_func_header(module_lines[i], sym, sizeof(sym), rtype, sizeof(rtype), &pcount, &is_hidden))
            continue;
        if (strcmp(sym, function_name) != 0)
            continue;

        size_t j = i + 1;
        while (j < module_line_count)
        {
            const char *line = ccsim_trim(module_lines[j]);
            if (line && strcmp(line, ".endfunc") == 0)
                break;
            ++j;
        }
        if (j >= module_line_count)
            return false;

        *out_body_start = i + 1;
        *out_endfunc_line = j;
        strncpy(ret_type, rtype, ret_type_sz - 1);
        ret_type[ret_type_sz - 1] = '\0';
        *out_params_count = pcount;
        *out_is_hidden = is_hidden;
        return true;
    }
    return false;
}

static bool ccsim_eval_hidden_pure_call(char **module_lines, size_t module_line_count,
                                        const char *callee_symbol,
                                        const CcsimValue *args, size_t arg_count,
                                        CcsimValue *out_ret)
{
    if (!module_lines || !callee_symbol || !out_ret)
        return false;

    size_t body_start = 0;
    size_t endfunc_line = 0;
    char ret_type[16] = {0};
    size_t params_count = 0;
    bool is_hidden = false;
    if (!ccsim_find_function_range(module_lines, module_line_count,
                                   callee_symbol,
                                   &body_start, &endfunc_line,
                                   ret_type, sizeof(ret_type),
                                   &params_count, &is_hidden))
        return false;
    if (!is_hidden || params_count != arg_count)
        return false;

    CcsimStack stack = {0};
    CcsimValue ret = ccsim_make_unknown();
    bool saw_ret = false;

    for (size_t i = body_start; i < endfunc_line; ++i)
    {
        const char *line = ccsim_trim(module_lines[i]);
        if (!line || *line == '\0')
            continue;
        if (strncmp(line, ".", 1) == 0)
            continue;
        if (strncmp(line, "label ", 6) == 0 || strncmp(line, "jump ", 5) == 0 || strncmp(line, "branch ", 7) == 0)
        {
            ccsim_stack_free(&stack);
            return false;
        }
        if (strncmp(line, "call ", 5) == 0 || strncmp(line, "call_indirect", 13) == 0)
        {
            ccsim_stack_free(&stack);
            return false;
        }
        if (ccsim_parse_symbol_after_prefix(line, "addr_global ", ret_type, sizeof(ret_type)) ||
            ccsim_parse_symbol_after_prefix(line, "load_global ", ret_type, sizeof(ret_type)) ||
            ccsim_parse_symbol_after_prefix(line, "store_global ", ret_type, sizeof(ret_type)) ||
            ccsim_parse_memory_type_after_prefix(line, "load_indirect ", ret_type, sizeof(ret_type)) ||
            ccsim_parse_memory_type_after_prefix(line, "store_indirect ", ret_type, sizeof(ret_type)))
        {
            ccsim_stack_free(&stack);
            return false;
        }

        int pidx = -1;
        if (ccsim_parse_local_index(line, "load_param", &pidx))
        {
            CcsimValue v = (pidx >= 0 && (size_t)pidx < arg_count) ? args[pidx] : ccsim_make_unknown();
            if (!ccsim_stack_push(&stack, v))
            {
                ccsim_stack_free(&stack);
                return false;
            }
            continue;
        }
        if (strncmp(line, "addr_param ", 11) == 0)
        {
            if (!ccsim_stack_push(&stack, ccsim_make_unknown()))
            {
                ccsim_stack_free(&stack);
                return false;
            }
            continue;
        }

        CcsimValue cval;
        if (ccsim_parse_const(line, &cval))
        {
            if (!ccsim_stack_push(&stack, cval))
            {
                ccsim_stack_free(&stack);
                return false;
            }
            continue;
        }
        if (strncmp(line, "const_str ", 10) == 0)
        {
            if (!ccsim_stack_push(&stack, ccsim_make_unknown()))
            {
                ccsim_stack_free(&stack);
                return false;
            }
            continue;
        }
        if (strncmp(line, "dup", 3) == 0)
        {
            CcsimValue top = ccsim_stack_peek(&stack);
            if (!ccsim_stack_push(&stack, top))
            {
                ccsim_stack_free(&stack);
                return false;
            }
            continue;
        }
        if (strncmp(line, "drop", 4) == 0)
        {
            (void)ccsim_stack_pop(&stack);
            continue;
        }

        char op[16] = {0};
        char type_name[16] = {0};
        bool unsigned_hint = false;
        if (ccsim_parse_binop(line, op, sizeof(op), type_name, sizeof(type_name), &unsigned_hint))
        {
            CcsimValue rhs = ccsim_stack_pop(&stack);
            CcsimValue lhs = ccsim_stack_pop(&stack);
            bool ok = false;
            CcsimValue out = ccsim_eval_binop(op, type_name, unsigned_hint, lhs, rhs, &ok);
            if (!ccsim_stack_push(&stack, out))
            {
                ccsim_stack_free(&stack);
                return false;
            }
            continue;
        }
        if (ccsim_parse_unop(line, op, sizeof(op), type_name, sizeof(type_name)))
        {
            CcsimValue in = ccsim_stack_pop(&stack);
            bool ok = false;
            CcsimValue out = ccsim_eval_unop(op, type_name, in, &ok);
            if (!ccsim_stack_push(&stack, out))
            {
                ccsim_stack_free(&stack);
                return false;
            }
            continue;
        }
        if (ccsim_parse_compare(line, op, sizeof(op), type_name, sizeof(type_name), &unsigned_hint))
        {
            CcsimValue rhs = ccsim_stack_pop(&stack);
            CcsimValue lhs = ccsim_stack_pop(&stack);
            bool ok = false;
            CcsimValue out = ccsim_eval_compare(op, type_name, unsigned_hint, lhs, rhs, &ok);
            if (!ccsim_stack_push(&stack, out))
            {
                ccsim_stack_free(&stack);
                return false;
            }
            continue;
        }

        char kind[16] = {0};
        char from_type[16] = {0};
        char to_type[16] = {0};
        if (ccsim_parse_convert(line, kind, sizeof(kind), from_type, sizeof(from_type), to_type, sizeof(to_type)))
        {
            CcsimValue in = ccsim_stack_pop(&stack);
            bool ok = false;
            CcsimValue out = ccsim_eval_convert(kind, from_type, to_type, in, &ok);
            if (!ccsim_stack_push(&stack, out))
            {
                ccsim_stack_free(&stack);
                return false;
            }
            continue;
        }

        if (ccsim_parse_test_null(line, type_name, sizeof(type_name)))
        {
            CcsimValue input = ccsim_stack_pop(&stack);
            if (input.kind == CCSIM_VAL_INT && strcmp(input.type, "ptr") == 0)
            {
                CcsimValue out = ccsim_make_known("i1", input.u == 0 ? 1ULL : 0ULL,
                                                  input.u == 0 ? 1LL : 0LL, true);
                if (!ccsim_stack_push(&stack, out))
                {
                    ccsim_stack_free(&stack);
                    return false;
                }
            }
            else
            {
                if (!ccsim_stack_push(&stack, ccsim_make_unknown()))
                {
                    ccsim_stack_free(&stack);
                    return false;
                }
            }
            continue;
        }

        if (strncmp(line, "ret", 3) == 0)
        {
            if (strstr(line, "void") != NULL)
                ret = ccsim_make_unknown();
            else
                ret = ccsim_stack_pop(&stack);
            saw_ret = true;
            break;
        }

        ccsim_stack_free(&stack);
        return false;
    }

    ccsim_stack_free(&stack);
    if (!saw_ret)
        return false;
    *out_ret = ret;
    return true;
}

static bool ccsim_collect_hidden_const_callees(char **module_lines, size_t module_line_count,
                                               CcsimConstCalleeList *out)
{
    if (!module_lines || !out)
        return false;

    size_t i = 0;
    while (i < module_line_count)
    {
        const char *line = module_lines[i];
        char symbol[256] = {0};
        char ret_type[16] = {0};
        size_t params_count = 0;
        bool is_hidden = false;
        if (!ccsim_parse_func_header(line, symbol, sizeof(symbol), ret_type, sizeof(ret_type), &params_count, &is_hidden))
        {
            ++i;
            continue;
        }

        ++i;
        bool saw_endfunc = false;
        bool valid = is_hidden && params_count == 0 && strcmp(ret_type, "void") != 0;
        bool saw_ret = false;
        CcsimValue const_value = ccsim_make_unknown();

        while (i < module_line_count)
        {
            const char *body_line = ccsim_trim(module_lines[i]);
            if (!body_line)
            {
                ++i;
                continue;
            }
            if (strcmp(body_line, ".endfunc") == 0)
            {
                saw_endfunc = true;
                ++i;
                break;
            }
            if (strncmp(body_line, ".", 1) == 0)
            {
                ++i;
                continue;
            }

            if (!valid)
            {
                ++i;
                continue;
            }

            if (strncmp(body_line, "ret", 3) == 0)
            {
                saw_ret = true;
                ++i;
                continue;
            }

            CcsimValue maybe_const;
            if (ccsim_parse_const(body_line, &maybe_const))
            {
                if (const_value.kind != CCSIM_VAL_UNKNOWN)
                    valid = false;
                else if (strcmp(maybe_const.type, ret_type) != 0)
                    valid = false;
                else
                    const_value = maybe_const;
                ++i;
                continue;
            }

            valid = false;
            ++i;
        }

        if (!saw_endfunc)
            break;

        if (valid && saw_ret && const_value.kind == CCSIM_VAL_INT)
        {
            if (!ccsim_const_callee_list_append(out, symbol, const_value))
                return false;
        }
    }

    return true;
}

void ccsim_collapse_hidden_calls(char **function_lines, size_t function_line_count,
                                 char **module_lines, size_t module_line_count,
                                 const CcsimOptions *options,
                                 CcsimStats *stats)
{
    if (!function_lines || function_line_count == 0 || !module_lines || module_line_count == 0)
        return;

    CcsimOptions local_options;
    local_options.opt_level = options ? options->opt_level : 0;
    local_options.aggressive = options ? options->aggressive : 0;
    if (local_options.opt_level < 3)
        return;

    CcsimConstCalleeList callees;
    memset(&callees, 0, sizeof(callees));
    if (!ccsim_collect_hidden_const_callees(module_lines, module_line_count, &callees))
    {
        ccsim_const_callee_list_free(&callees);
        return;
    }

    for (size_t i = 0; i < function_line_count; ++i)
    {
        const char *line = function_lines[i];
        char symbol[256] = {0};
        char ret_type[16] = {0};
        bool is_zero_arg = false;
        if (!ccsim_parse_call_line(line, symbol, sizeof(symbol), ret_type, sizeof(ret_type), &is_zero_arg))
            continue;
        if (!is_zero_arg)
            continue;

        const CcsimConstCallee *callee = ccsim_const_callee_find(&callees, symbol);
        if (!callee || callee->value.kind != CCSIM_VAL_INT)
            continue;
        if (strcmp(callee->value.type, ret_type) != 0)
            continue;

        bool replaced = false;
        if (callee->value.is_signed)
            replaced = ccsim_replace_linef(function_lines, function_line_count, i,
                                          "  const %s %lld", callee->value.type, callee->value.s);
        else
            replaced = ccsim_replace_linef(function_lines, function_line_count, i,
                                          "  const %s %llu", callee->value.type, callee->value.u);
        if (replaced && stats)
            stats->collapsed_hidden_calls++;
    }

    ccsim_const_callee_list_free(&callees);
}

static void ccsim_locals_invalidate(CcsimLocal *locals, size_t local_count, bool escaped_only)
{
    if (!locals)
        return;
    for (size_t i = 0; i < local_count; ++i)
    {
        if (escaped_only && !locals[i].escaped)
            continue;
        locals[i].value = ccsim_make_unknown();
    }
}

static bool ccsim_local_ensure(CcsimLocal **locals, size_t *local_capacity, size_t index)
{
    if (!locals || !local_capacity)
        return false;
    if (index < *local_capacity)
        return true;

    size_t new_capacity = *local_capacity ? *local_capacity : 16;
    while (new_capacity <= index)
    {
        if (new_capacity > (SIZE_MAX / 2))
            return false;
        new_capacity *= 2;
    }

    CcsimLocal *new_locals = (CcsimLocal *)realloc(*locals, new_capacity * sizeof(CcsimLocal));
    if (!new_locals)
        return false;

    for (size_t i = *local_capacity; i < new_capacity; ++i)
    {
        new_locals[i].value = ccsim_make_unknown();
        new_locals[i].escaped = 0;
    }

    *locals = new_locals;
    *local_capacity = new_capacity;
    return true;
}

static CcsimValue ccsim_eval_binop(const char *op, const char *type_name, bool unsigned_hint,
                                   CcsimValue lhs, CcsimValue rhs, bool *ok)
{
    *ok = false;
    if ((lhs.kind == CCSIM_VAL_ADDRESS || rhs.kind == CCSIM_VAL_ADDRESS) &&
        (strcmp(op, "add") == 0 || strcmp(op, "sub") == 0))
    {
        if (lhs.kind == CCSIM_VAL_ADDRESS && rhs.kind == CCSIM_VAL_INT)
        {
            long long delta = rhs.s;
            if (strcmp(op, "sub") == 0)
                delta = -delta;
            *ok = true;
            return ccsim_make_address(lhs.symbol, lhs.offset + delta);
        }
        if (rhs.kind == CCSIM_VAL_ADDRESS && lhs.kind == CCSIM_VAL_INT && strcmp(op, "add") == 0)
        {
            *ok = true;
            return ccsim_make_address(rhs.symbol, rhs.offset + lhs.s);
        }
        return ccsim_make_unknown();
    }

    if (lhs.kind != CCSIM_VAL_INT || rhs.kind != CCSIM_VAL_INT)
        return ccsim_make_unknown();
    if (!type_name || type_name[0] == '\0')
        return ccsim_make_unknown();

    unsigned width = ccsim_type_bits(type_name);
    if (width == 0)
        return ccsim_make_unknown();

    unsigned long long mask = ccsim_mask_for_width(width);
    bool signed_arith = ccsim_type_is_signed(type_name) && !unsigned_hint;
    unsigned long long lu = lhs.u & mask;
    unsigned long long ru = rhs.u & mask;
    long long ls = ccsim_sign_extend(lu, width);
    long long rs = ccsim_sign_extend(ru, width);
    unsigned long long out_u = 0;

    if (strcmp(op, "add") == 0)
        out_u = (lu + ru) & mask;
    else if (strcmp(op, "sub") == 0)
        out_u = (lu - ru) & mask;
    else if (strcmp(op, "mul") == 0)
        out_u = (lu * ru) & mask;
    else if (strcmp(op, "div") == 0)
    {
        if (ru == 0)
            return ccsim_make_unknown();
        if (signed_arith)
        {
            if (width == 64 && ls == LLONG_MIN && rs == -1)
                return ccsim_make_unknown();
            out_u = (unsigned long long)(ls / rs) & mask;
        }
        else
        {
            out_u = (lu / ru) & mask;
        }
    }
    else if (strcmp(op, "mod") == 0)
    {
        if (ru == 0)
            return ccsim_make_unknown();
        if (signed_arith)
            out_u = (unsigned long long)(ls % rs) & mask;
        else
            out_u = (lu % ru) & mask;
    }
    else if (strcmp(op, "and") == 0)
        out_u = (lu & ru) & mask;
    else if (strcmp(op, "or") == 0)
        out_u = (lu | ru) & mask;
    else if (strcmp(op, "xor") == 0)
        out_u = (lu ^ ru) & mask;
    else if (strcmp(op, "shl") == 0)
    {
        unsigned sh = (unsigned)(ru & 63ULL);
        if (sh >= width)
            out_u = 0;
        else
            out_u = (lu << sh) & mask;
    }
    else if (strcmp(op, "shr") == 0)
    {
        unsigned sh = (unsigned)(ru & 63ULL);
        if (sh >= width)
            out_u = signed_arith ? (ls < 0 ? mask : 0ULL) : 0ULL;
        else if (signed_arith)
            out_u = (unsigned long long)(ls >> sh) & mask;
        else
            out_u = (lu >> sh) & mask;
    }
    else
    {
        return ccsim_make_unknown();
    }

    *ok = true;
    return ccsim_make_known(type_name, out_u, ccsim_sign_extend(out_u, width), ccsim_type_is_signed(type_name));
}

static CcsimValue ccsim_eval_unop(const char *op, const char *type_name, CcsimValue operand, bool *ok)
{
    *ok = false;
    if (operand.kind != CCSIM_VAL_INT)
        return ccsim_make_unknown();

    unsigned width = ccsim_type_bits(type_name);
    if (width == 0)
        return ccsim_make_unknown();

    unsigned long long mask = ccsim_mask_for_width(width);
    unsigned long long u = operand.u & mask;
    long long s = ccsim_sign_extend(u, width);
    unsigned long long out_u = 0;

    if (strcmp(op, "neg") == 0)
        out_u = (unsigned long long)(-s) & mask;
    else if (strcmp(op, "not") == 0)
        out_u = (u == 0) ? 1ULL : 0ULL;
    else if (strcmp(op, "bitnot") == 0)
        out_u = (~u) & mask;
    else
        return ccsim_make_unknown();

    *ok = true;
    return ccsim_make_known(type_name, out_u, ccsim_sign_extend(out_u, width), ccsim_type_is_signed(type_name));
}

static CcsimValue ccsim_eval_compare(const char *op, const char *type_name, bool unsigned_hint,
                                     CcsimValue lhs, CcsimValue rhs, bool *ok)
{
    *ok = false;
    if (strcmp(type_name, "ptr") == 0)
    {
        if ((lhs.kind == CCSIM_VAL_ADDRESS && rhs.kind == CCSIM_VAL_INT && rhs.u == 0) ||
            (rhs.kind == CCSIM_VAL_ADDRESS && lhs.kind == CCSIM_VAL_INT && lhs.u == 0))
        {
            bool eq = false;
            if (strcmp(op, "eq") == 0)
                eq = false;
            else if (strcmp(op, "ne") == 0)
                eq = true;
            else
                return ccsim_make_unknown();
            *ok = true;
            return ccsim_make_known("i1", eq ? 1ULL : 0ULL, eq ? 1LL : 0LL, true);
        }
    }

    if (lhs.kind != CCSIM_VAL_INT || rhs.kind != CCSIM_VAL_INT)
        return ccsim_make_unknown();

    unsigned width = ccsim_type_bits(type_name);
    if (width == 0)
        return ccsim_make_unknown();

    unsigned long long mask = ccsim_mask_for_width(width);
    unsigned long long lu = lhs.u & mask;
    unsigned long long ru = rhs.u & mask;
    long long ls = ccsim_sign_extend(lu, width);
    long long rs = ccsim_sign_extend(ru, width);

    bool is_unsigned_cmp = unsigned_hint || type_name[0] == 'u' || strcmp(type_name, "ptr") == 0;
    int result = 0;

    if (strcmp(op, "eq") == 0)
        result = (lu == ru);
    else if (strcmp(op, "ne") == 0)
        result = (lu != ru);
    else if (strcmp(op, "lt") == 0)
        result = is_unsigned_cmp ? (lu < ru) : (ls < rs);
    else if (strcmp(op, "le") == 0)
        result = is_unsigned_cmp ? (lu <= ru) : (ls <= rs);
    else if (strcmp(op, "gt") == 0)
        result = is_unsigned_cmp ? (lu > ru) : (ls > rs);
    else if (strcmp(op, "ge") == 0)
        result = is_unsigned_cmp ? (lu >= ru) : (ls >= rs);
    else
        return ccsim_make_unknown();

    *ok = true;
    return ccsim_make_known("i1", (unsigned long long)(result ? 1 : 0), (long long)(result ? 1 : 0), true);
}

static CcsimValue ccsim_eval_convert(const char *kind, const char *from_type, const char *to_type,
                                     CcsimValue input, bool *ok)
{
    *ok = false;
    if (input.kind == CCSIM_VAL_ADDRESS)
    {
        if (strcmp(kind, "bitcast") == 0 &&
            ((strcmp(from_type, "ptr") == 0 && strcmp(to_type, "i64") == 0) ||
             (strcmp(from_type, "i64") == 0 && strcmp(to_type, "ptr") == 0)))
        {
            *ok = true;
            return input;
        }
        return ccsim_make_unknown();
    }

    if (input.kind != CCSIM_VAL_INT)
        return ccsim_make_unknown();

    unsigned from_width = ccsim_type_bits(from_type);
    unsigned to_width = ccsim_type_bits(to_type);
    if (from_width == 0 || to_width == 0)
        return ccsim_make_unknown();

    unsigned long long in_u = input.u & ccsim_mask_for_width(from_width);
    long long in_s = ccsim_sign_extend(in_u, from_width);
    unsigned long long out_u = 0;

    if (strcmp(kind, "trunc") == 0)
    {
        out_u = in_u;
    }
    else if (strcmp(kind, "zext") == 0)
    {
        out_u = in_u;
    }
    else if (strcmp(kind, "sext") == 0)
    {
        out_u = (unsigned long long)in_s;
    }
    else if (strcmp(kind, "bitcast") == 0)
    {
        out_u = in_u;
    }
    else
    {
        return ccsim_make_unknown();
    }

    CcsimValue out = ccsim_make_known(to_type, out_u, ccsim_sign_extend(out_u, to_width), ccsim_type_is_signed(to_type));
    *ok = true;
    return out;
}

static bool ccsim_run_pass(char **lines, size_t line_count, const CcsimOptions *options, CcsimStats *stats)
{
    bool changed = false;
    CcsimStack stack = {0};
    CcsimLocal *locals = NULL;
    size_t local_capacity = 0;
    CcsimKnownGlobalList globals = {0};
    CcsimKnownMemoryList memory = {0};

    for (size_t i = 0; i < line_count; ++i)
    {
        const char *raw = lines[i];
        const char *line = ccsim_trim(raw);
        if (!line || *line == '\0')
            continue;

        if (ccsim_try_fold_duplicate_addr_chain(lines, line_count, i))
        {
            changed = true;
            ccsim_tracef("rewrite duplicate addr chain at line=%zu", i);
            line = ccsim_trim(lines[i]);
        }

        if (strncmp(line, ".loc ", 5) == 0)
            continue;

        if (strncmp(line, "label ", 6) == 0)
        {
            ccsim_stack_clear(&stack);
            ccsim_locals_invalidate(locals, local_capacity, false);
            ccsim_known_memory_clear(&memory);
            ccsim_tracef("pass barrier label at line=%zu", i);
            continue;
        }

        if (strncmp(line, "jump ", 5) == 0 || strncmp(line, "branch ", 7) == 0 || strncmp(line, "ret", 3) == 0)
        {
            ccsim_stack_clear(&stack);
            ccsim_locals_invalidate(locals, local_capacity, false);
            ccsim_known_memory_clear(&memory);
            if (stats)
                stats->simulation_barriers++;
            ccsim_tracef("pass barrier control at line=%zu", i);
            continue;
        }

        if (strncmp(line, "call ", 5) == 0 || strncmp(line, "call_indirect", 13) == 0)
        {
            ccsim_stack_clear(&stack);
            ccsim_locals_invalidate(locals, local_capacity, false);
            ccsim_known_memory_clear(&memory);
            ccsim_known_globals_free(&globals);
            if (stats)
                stats->simulation_barriers++;
            ccsim_tracef("pass barrier call at line=%zu", i);
            continue;
        }

        CcsimValue cval;
        if (ccsim_parse_const(line, &cval))
        {
            ccsim_stack_push(&stack, cval);
            continue;
        }

        int local_index = -1;
        if (ccsim_parse_local_index(line, "load_local", &local_index))
        {
            size_t idx = (size_t)local_index;
            CcsimValue loaded = ccsim_make_unknown();
            if (ccsim_local_ensure(&locals, &local_capacity, idx))
            {
                CcsimLocal *local = &locals[idx];
                if (!local->escaped)
                {
                    loaded = local->value;
                    if (loaded.kind == CCSIM_VAL_INT && loaded.type[0] != '\0')
                    {
                        if (loaded.is_signed)
                        {
                            if (ccsim_replace_linef(lines, line_count, i, "  const %s %lld", loaded.type, loaded.s))
                            {
                                changed = true;
                                if (stats)
                                    stats->rewritten_load_locals++;
                                ccsim_tracef("rewrite load_local->const at line=%zu (%s)", i, loaded.type);
                            }
                        }
                        else
                        {
                            if (ccsim_replace_linef(lines, line_count, i, "  const %s %llu", loaded.type, loaded.u))
                            {
                                changed = true;
                                if (stats)
                                    stats->rewritten_load_locals++;
                                ccsim_tracef("rewrite load_local->const at line=%zu (%s)", i, loaded.type);
                            }
                        }
                    }
                }
            }
            ccsim_stack_push(&stack, loaded);
            continue;
        }

        if (ccsim_parse_local_index(line, "store_local", &local_index))
        {
            CcsimValue v = ccsim_stack_pop(&stack);
            if (local_index >= 0)
            {
                size_t idx = (size_t)local_index;
                if (ccsim_local_ensure(&locals, &local_capacity, idx))
                {
                    if (locals[idx].escaped)
                        locals[idx].value = ccsim_make_unknown();
                    else
                        locals[idx].value = v;
                }
            }
            continue;
        }

        if (ccsim_parse_local_index(line, "addr_local", &local_index))
        {
            if (local_index >= 0)
            {
                size_t idx = (size_t)local_index;
                if (ccsim_local_ensure(&locals, &local_capacity, idx))
                {
                    locals[idx].escaped = 1;
                    locals[idx].value = ccsim_make_unknown();
                }
            }
            ccsim_stack_push(&stack, ccsim_make_unknown());
            continue;
        }

        if (strncmp(line, "load_param ", 11) == 0 || strncmp(line, "addr_param ", 11) == 0 ||
            strncmp(line, "stack_alloc", 11) == 0 || strncmp(line, "const_str ", 10) == 0)
        {
            ccsim_stack_push(&stack, ccsim_make_unknown());
            continue;
        }

        char symbol_name[256] = {0};
        if (ccsim_parse_symbol_after_prefix(line, "addr_global ", symbol_name, sizeof(symbol_name)))
        {
            ccsim_stack_push(&stack, ccsim_make_address(symbol_name, 0));
            continue;
        }

        if (ccsim_parse_symbol_after_prefix(line, "load_global ", symbol_name, sizeof(symbol_name)))
        {
            CcsimValue gv = ccsim_known_globals_get(&globals, symbol_name);
            if (gv.kind == CCSIM_VAL_INT && gv.type[0] != '\0')
            {
                bool replaced = false;
                if (gv.is_signed)
                    replaced = ccsim_replace_linef(lines, line_count, i, "  const %s %lld", gv.type, gv.s);
                else
                    replaced = ccsim_replace_linef(lines, line_count, i, "  const %s %llu", gv.type, gv.u);
                if (replaced)
                {
                    changed = true;
                    ccsim_tracef("rewrite load_global->const at line=%zu (%s)", i, symbol_name);
                }
            }
            ccsim_stack_push(&stack, gv);
            continue;
        }

        if (ccsim_parse_symbol_after_prefix(line, "store_global ", symbol_name, sizeof(symbol_name)))
        {
            CcsimValue gv = ccsim_stack_pop(&stack);
            if (gv.kind == CCSIM_VAL_INT)
                ccsim_known_globals_set(&globals, symbol_name, gv);
            else
                ccsim_known_globals_set(&globals, symbol_name, ccsim_make_unknown());
            ccsim_known_memory_invalidate_symbol(&memory, symbol_name);
            ccsim_locals_invalidate(locals, local_capacity, options && options->aggressive ? false : true);
            if (stats)
                stats->simulation_barriers++;
            continue;
        }

        char mem_type[16] = {0};
        if (ccsim_parse_memory_type_after_prefix(line, "load_indirect ", mem_type, sizeof(mem_type)))
        {
            CcsimValue addr = ccsim_stack_pop(&stack);
            CcsimValue loaded = ccsim_make_unknown();
            if (addr.kind == CCSIM_VAL_ADDRESS && addr.symbol[0] != '\0')
            {
                loaded = ccsim_known_memory_get(&memory, addr.symbol, addr.offset, mem_type);
                if (loaded.kind == CCSIM_VAL_INT && loaded.type[0] != '\0')
                {
                    bool replaced = false;
                    if (loaded.is_signed)
                        replaced = ccsim_replace_linef(lines, line_count, i, "  const %s %lld", loaded.type, loaded.s);
                    else
                        replaced = ccsim_replace_linef(lines, line_count, i, "  const %s %llu", loaded.type, loaded.u);
                    if (replaced)
                    {
                        changed = true;
                        ccsim_tracef("rewrite load_indirect->const at line=%zu (%s+%lld %s)", i,
                                     addr.symbol, addr.offset, mem_type);
                    }
                }
            }
            else if (stats)
            {
                stats->simulation_barriers++;
            }
            ccsim_stack_push(&stack, loaded);
            continue;
        }

        if (ccsim_parse_memory_type_after_prefix(line, "store_indirect ", mem_type, sizeof(mem_type)))
        {
            CcsimValue value = ccsim_stack_pop(&stack);
            CcsimValue addr = ccsim_stack_pop(&stack);
            if (addr.kind == CCSIM_VAL_ADDRESS && addr.symbol[0] != '\0')
            {
                if (value.kind == CCSIM_VAL_INT && strcmp(value.type, mem_type) == 0)
                    ccsim_known_memory_set(&memory, addr.symbol, addr.offset, mem_type, value);
                else
                    ccsim_known_memory_remove(&memory, addr.symbol, addr.offset, mem_type);
            }
            else
            {
                ccsim_known_memory_clear(&memory);
                ccsim_locals_invalidate(locals, local_capacity, options && options->aggressive ? false : true);
                if (stats)
                    stats->simulation_barriers++;
            }
            continue;
        }

        if (strncmp(line, "drop", 4) == 0)
        {
            (void)ccsim_stack_pop(&stack);
            continue;
        }

        if (strncmp(line, "dup", 3) == 0)
        {
            CcsimValue top = ccsim_stack_peek(&stack);
            ccsim_stack_push(&stack, top);
            continue;
        }

        char op[16] = {0};
        char type_name[16] = {0};
        bool unsigned_hint = false;
        if (ccsim_parse_binop(line, op, sizeof(op), type_name, sizeof(type_name), &unsigned_hint))
        {
            CcsimValue rhs = ccsim_stack_pop(&stack);
            CcsimValue lhs = ccsim_stack_pop(&stack);
            bool folded = false;
            CcsimValue out = ccsim_eval_binop(op, type_name, unsigned_hint, lhs, rhs, &folded);
            if (folded && stats)
                stats->const_folds++;
            if (folded)
                ccsim_tracef("fold binop %s %s at line=%zu", op, type_name, i);
            ccsim_stack_push(&stack, out);
            continue;
        }

        if (ccsim_parse_unop(line, op, sizeof(op), type_name, sizeof(type_name)))
        {
            CcsimValue operand = ccsim_stack_pop(&stack);
            bool folded = false;
            CcsimValue out = ccsim_eval_unop(op, type_name, operand, &folded);
            if (folded && stats)
                stats->const_folds++;
            if (folded)
                ccsim_tracef("fold unop %s %s at line=%zu", op, type_name, i);
            ccsim_stack_push(&stack, out);
            continue;
        }

        if (ccsim_parse_compare(line, op, sizeof(op), type_name, sizeof(type_name), &unsigned_hint))
        {
            CcsimValue rhs = ccsim_stack_pop(&stack);
            CcsimValue lhs = ccsim_stack_pop(&stack);
            bool folded = false;
            CcsimValue out = ccsim_eval_compare(op, type_name, unsigned_hint, lhs, rhs, &folded);
            if (folded && stats)
                stats->const_folds++;
            if (folded)
                ccsim_tracef("fold compare %s %s at line=%zu", op, type_name, i);
            ccsim_stack_push(&stack, out);
            continue;
        }

        char kind[16] = {0};
        char from_type[16] = {0};
        char to_type[16] = {0};
        if (ccsim_parse_convert(line, kind, sizeof(kind), from_type, sizeof(from_type), to_type, sizeof(to_type)))
        {
            CcsimValue input = ccsim_stack_pop(&stack);
            bool folded = false;
            CcsimValue out = ccsim_eval_convert(kind, from_type, to_type, input, &folded);
            if (folded && stats)
                stats->const_folds++;
            if (folded)
                ccsim_tracef("fold convert %s %s->%s at line=%zu", kind, from_type, to_type, i);
            ccsim_stack_push(&stack, out);
            continue;
        }

        if (ccsim_parse_test_null(line, type_name, sizeof(type_name)))
        {
            CcsimValue input = ccsim_stack_pop(&stack);
            if (input.kind == CCSIM_VAL_INT && strcmp(input.type, "ptr") == 0)
            {
                CcsimValue out = ccsim_make_known("i1", input.u == 0 ? 1ULL : 0ULL,
                                                  input.u == 0 ? 1LL : 0LL, true);
                if (stats)
                    stats->const_folds++;
                ccsim_stack_push(&stack, out);
            }
            else
            {
                ccsim_stack_push(&stack, ccsim_make_unknown());
            }
            continue;
        }

        ccsim_stack_clear(&stack);
        ccsim_locals_invalidate(locals, local_capacity, false);
        ccsim_known_memory_clear(&memory);
        if (stats)
            stats->simulation_barriers++;
    }

    ccsim_stack_free(&stack);
    free(locals);
    ccsim_known_globals_free(&globals);
    ccsim_known_memory_free(&memory);
    return changed;
}

void ccsim_optimize_lines(char **lines, size_t line_count, const CcsimOptions *options, CcsimStats *stats)
{
    if (!lines || line_count == 0)
        return;

    CcsimOptions local_options;
    local_options.opt_level = options ? options->opt_level : 0;
    local_options.aggressive = options ? options->aggressive : 0;

    if (local_options.opt_level < 2)
        return;

    CcsimStats local_stats;
    memset(&local_stats, 0, sizeof(local_stats));

    int max_passes = local_options.aggressive ? 8 : 3;
    for (int pass = 0; pass < max_passes; ++pass)
    {
        bool changed = ccsim_run_pass(lines, line_count, &local_options, &local_stats);
        local_stats.passes++;
        if (!changed)
            break;
    }

    if (stats)
        *stats = local_stats;
}
