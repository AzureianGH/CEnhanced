#include "ast.h"
#include "cc/bytecode.h"

#include <errno.h>
#include <ctype.h>
#include <limits.h>
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
    uint32_t *debug_files;
    uint32_t *debug_lines;
    uint32_t *debug_columns;
    bool track_debug;
    bool has_active_debug;
    uint32_t active_debug_file;
    uint32_t active_debug_line;
    uint32_t active_debug_column;
} StringList;

static void string_list_init(StringList *list)
{
    if (!list)
        return;
    list->items = NULL;
    list->count = 0;
    list->capacity = 0;
    list->debug_files = NULL;
    list->debug_lines = NULL;
    list->debug_columns = NULL;
    list->track_debug = false;
    list->has_active_debug = false;
    list->active_debug_file = 0;
    list->active_debug_line = 0;
    list->active_debug_column = 0;
}

static void string_list_free(StringList *list)
{
    if (!list)
        return;
    for (size_t i = 0; i < list->count; ++i)
        free(list->items[i]);
    free(list->items);
    free(list->debug_files);
    free(list->debug_lines);
    free(list->debug_columns);
    list->items = NULL;
    list->count = 0;
    list->capacity = 0;
    list->debug_files = NULL;
    list->debug_lines = NULL;
    list->debug_columns = NULL;
    list->track_debug = false;
    list->has_active_debug = false;
    list->active_debug_file = 0;
    list->active_debug_line = 0;
    list->active_debug_column = 0;
}

typedef struct
{
    uint32_t file;
    uint32_t line;
    uint32_t column;
    bool has_location;
} StringListDebugState;

static bool string_list_resize_debug_storage(StringList *list, size_t old_capacity, size_t new_capacity)
{
    if (!list || !list->track_debug)
        return true;
    if (new_capacity == 0)
    {
        free(list->debug_files);
        free(list->debug_lines);
        free(list->debug_columns);
        list->debug_files = NULL;
        list->debug_lines = NULL;
        list->debug_columns = NULL;
        return true;
    }

    uint32_t *files = (uint32_t *)malloc(new_capacity * sizeof(uint32_t));
    uint32_t *lines = (uint32_t *)malloc(new_capacity * sizeof(uint32_t));
    uint32_t *columns = (uint32_t *)malloc(new_capacity * sizeof(uint32_t));
    if (!files || !lines || !columns)
    {
        free(files);
        free(lines);
        free(columns);
        return false;
    }

    size_t copy = old_capacity;
    if (copy > new_capacity)
        copy = new_capacity;
    if (copy > 0)
    {
        if (list->debug_files)
            memcpy(files, list->debug_files, copy * sizeof(uint32_t));
        if (list->debug_lines)
            memcpy(lines, list->debug_lines, copy * sizeof(uint32_t));
        if (list->debug_columns)
            memcpy(columns, list->debug_columns, copy * sizeof(uint32_t));
    }
    if (new_capacity > copy)
    {
        size_t delta = new_capacity - copy;
        memset(files + copy, 0, delta * sizeof(uint32_t));
        memset(lines + copy, 0, delta * sizeof(uint32_t));
        memset(columns + copy, 0, delta * sizeof(uint32_t));
    }

    free(list->debug_files);
    free(list->debug_lines);
    free(list->debug_columns);
    list->debug_files = files;
    list->debug_lines = lines;
    list->debug_columns = columns;
    return true;
}

static bool string_list_enable_debug_tracking(StringList *list)
{
    if (!list)
        return false;
    if (list->track_debug)
        return true;
    list->track_debug = true;
    if (list->capacity == 0)
        return true;
    return string_list_resize_debug_storage(list, 0, list->capacity);
}

static void string_list_set_debug_location(StringList *list, uint32_t file, uint32_t line, uint32_t column)
{
    if (!list)
        return;
    list->active_debug_file = file;
    list->active_debug_line = line;
    list->active_debug_column = column;
    list->has_active_debug = (file != 0 && line != 0);
}

static void string_list_clear_debug_location(StringList *list)
{
    if (!list)
        return;
    list->has_active_debug = false;
    list->active_debug_file = 0;
    list->active_debug_line = 0;
    list->active_debug_column = 0;
}

static StringListDebugState string_list_capture_debug_state(const StringList *list)
{
    StringListDebugState state;
    if (!list)
    {
        state.file = 0;
        state.line = 0;
        state.column = 0;
        state.has_location = false;
        return state;
    }
    state.file = list->active_debug_file;
    state.line = list->active_debug_line;
    state.column = list->active_debug_column;
    state.has_location = list->has_active_debug;
    return state;
}

static void string_list_restore_debug_state(StringList *list, const StringListDebugState *state)
{
    if (!list || !state)
        return;
    if (!state->has_location)
    {
        string_list_clear_debug_location(list);
        return;
    }
    string_list_set_debug_location(list, state->file, state->line, state->column);
}

static uint32_t string_list_get_debug_file(const StringList *list, size_t index)
{
    if (!list || !list->track_debug || !list->debug_files || index >= list->count)
        return 0;
    return list->debug_files[index];
}

static uint32_t string_list_get_debug_line(const StringList *list, size_t index)
{
    if (!list || !list->track_debug || !list->debug_lines || index >= list->count)
        return 0;
    return list->debug_lines[index];
}

static uint32_t string_list_get_debug_column(const StringList *list, size_t index)
{
    if (!list || !list->track_debug || !list->debug_columns || index >= list->count)
        return 0;
    return list->debug_columns[index];
}

static bool string_list_reserve(StringList *list, size_t desired)
{
    if (!list)
        return false;
    if (list->capacity >= desired)
        return true;

    size_t old_cap = list->capacity;
    size_t new_cap = old_cap ? old_cap * 2 : 8;
    if (new_cap < desired)
        new_cap = desired;

    char **new_items = (char **)realloc(list->items, new_cap * sizeof(char *));
    if (!new_items)
        return false;

    list->items = new_items;
    if (list->track_debug)
    {
        if (!string_list_resize_debug_storage(list, old_cap, new_cap))
            return false;
    }
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

    size_t index = list->count;
    list->items[index] = copy;
    if (list->track_debug && list->debug_files && list->debug_lines && list->debug_columns)
    {
        if (list->has_active_debug)
        {
            list->debug_files[index] = list->active_debug_file;
            list->debug_lines[index] = list->active_debug_line;
            list->debug_columns[index] = list->active_debug_column;
        }
        else
        {
            list->debug_files[index] = 0;
            list->debug_lines[index] = 0;
            list->debug_columns[index] = 0;
        }
    }
    list->count++;
    return true;
}

static bool string_list_append(StringList *list, const char *str)
{
    if (!str)
        str = "";
    return string_list_append_copy(list, str, strlen(str));
}

static int string_list_contains(const StringList *list, const char *value)
{
    if (!list || !value)
        return 0;
    for (size_t i = 0; i < list->count; ++i)
    {
        const char *entry = list->items[i];
        if (entry && strcmp(entry, value) == 0)
            return 1;
    }
    return 0;
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
    if (list->track_debug && list->debug_files && list->debug_lines && list->debug_columns)
    {
        if (tail > 0)
        {
            memmove(&list->debug_files[index], &list->debug_files[index + count], tail * sizeof(uint32_t));
            memmove(&list->debug_lines[index], &list->debug_lines[index + count], tail * sizeof(uint32_t));
            memmove(&list->debug_columns[index], &list->debug_columns[index + count], tail * sizeof(uint32_t));
        }
        memset(&list->debug_files[list->count - count], 0, count * sizeof(uint32_t));
        memset(&list->debug_lines[list->count - count], 0, count * sizeof(uint32_t));
        memset(&list->debug_columns[list->count - count], 0, count * sizeof(uint32_t));
    }

    list->count -= count;
}

typedef struct
{
    StringList lines;
    StringList debug_files;
    bool emit_debug;
} CcbModule;

typedef struct
{
    const char *name;
    Type *type;
    CCValueType value_type;
    bool is_address_only;
    bool is_param;
    int index;
    int scope_depth;
    bool is_active;
} CcbLocal;

typedef struct
{
    char break_label[32];
    char continue_label[32];
    bool is_loop;
} LoopContext;

typedef struct
{
    CcbModule *module;
    const Node *fn;
    StringList prologue;
    StringList body;
    CCValueType ret_type;
    CcbLocal *locals;
    size_t locals_count;
    size_t locals_capacity;
    size_t param_count;
    size_t local_count;
    int next_label_id;
    int scope_depth;
    LoopContext *loop_stack;
    size_t loop_depth;
    size_t loop_capacity;
} CcbFunctionBuilder;

static CCValueType map_type_to_cc(const Type *ty);
static const char *cc_type_name(CCValueType ty);
static size_t ccb_value_type_size(CCValueType ty);
static bool ccb_value_type_is_integer(CCValueType ty);
static bool ccb_value_type_is_float(CCValueType ty);
static bool ccb_value_type_is_signed(CCValueType ty);
static bool ccb_eval_const_int64(const Node *expr, int64_t *out_value);
static bool ccb_eval_const_float64(const Node *expr, double *out_value);
static bool ccb_store_scalar_bytes(const Type *elem_type, const Node *expr, uint8_t *dst, size_t dst_size);
static bool ccb_store_constant_value(const Type *type, const Node *expr, uint8_t *dst, size_t dst_size);
static bool ccb_store_struct_bytes(const Type *struct_type, const Node *expr, uint8_t *dst, size_t dst_size);
static bool ccb_store_array_bytes(const Type *array_type, const Node *expr, uint8_t *dst, size_t dst_size);
static bool ccb_flatten_array_initializer(const Type *array_type, const Node *init, uint8_t **out_bytes, size_t *out_size);
static bool ccb_flatten_struct_initializer(const Type *struct_type, const Node *init, uint8_t **out_bytes, size_t *out_size);
static char *ccb_encode_bytes_literal(const uint8_t *data, size_t len);
static CcbLocal *ccb_local_add(CcbFunctionBuilder *fb, const char *name, Type *type, bool address_only, bool is_param);
static int ccb_emit_expr_basic_impl(CcbFunctionBuilder *fb, const Node *expr);
static int ccb_emit_expr_basic(CcbFunctionBuilder *fb, const Node *expr);
static int ccb_emit_stmt_basic_impl(CcbFunctionBuilder *fb, const Node *stmt);
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
static int ccb_emit_array_zero(CcbFunctionBuilder *fb, const Node *var_decl, const char *var_name, const Type *array_type);
static int ccb_emit_array_initializer(CcbFunctionBuilder *fb, const Node *var_decl, const char *var_name, const Type *array_type, const Node *init);
static bool type_is_address_only(const Type *ty);
static int ccb_emit_pointer_offset(CcbFunctionBuilder *fb, int offset, const Node *node);
static int ccb_emit_struct_copy(CcbFunctionBuilder *fb, const Type *struct_type, CcbLocal *dst_ptr, CcbLocal *src_ptr);
static bool ccb_append_load_local(StringList *list, const CcbLocal *local);
static bool ccb_append_store_local(StringList *list, const CcbLocal *local);
static bool ccb_rewrite_param_loads(StringList *list, int param_index, int local_index);
static bool ccb_emit_load_local(CcbFunctionBuilder *fb, const CcbLocal *local);
static bool ccb_emit_store_local(CcbFunctionBuilder *fb, const CcbLocal *local);
static bool ccb_emit_load_global(StringList *body, const char *name);
static bool ccb_emit_store_global(StringList *body, const char *name);
static int ccb_promote_param_to_local(CcbFunctionBuilder *fb, const Node *site, CcbLocal **inout_local);
static void ccb_scope_enter(CcbFunctionBuilder *fb);
static void ccb_scope_leave(CcbFunctionBuilder *fb);
static CCValueType ccb_type_for_expr(const Node *expr);
static size_t ccb_type_size_bytes(const Type *ty);
static bool ccb_module_append_extern(CcbModule *mod, const Symbol *sym);
static int ccb_module_emit_externs(CcbModule *mod, const CodegenOptions *opts);
static void ccb_function_optimize(CcbFunctionBuilder *fb, const CodegenOptions *opts);
static bool ccb_instruction_is_pure(const char *line);
static void ccb_opt_prune_dropped_values(CcbFunctionBuilder *fb);
static void ccb_opt_fold_const_binops(CcbFunctionBuilder *fb);
static const char *ccb_binop_symbol(const char *op);
static int ccb_emit_inline_call(CcbFunctionBuilder *fb, const Node *call_expr, const Node *target_fn);

static void ccb_module_init(CcbModule *mod)
{
    if (!mod)
        return;
    string_list_init(&mod->lines);
    string_list_init(&mod->debug_files);
    mod->emit_debug = false;
}

static void ccb_module_free(CcbModule *mod)
{
    if (!mod)
        return;
    string_list_free(&mod->lines);
    string_list_free(&mod->debug_files);
    mod->emit_debug = false;
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
    if (!mod || !sym)
        return true;

    const char *symbol_name = (sym->backend_name && *sym->backend_name) ? sym->backend_name : sym->name;
    if (!symbol_name || !*symbol_name)
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
                                symbol_name, abi, params, ret_name, varargs_suffix);
    }
    else
    {
        ok = ccb_module_appendf(mod, ".extern %s params=%s returns=%s%s",
                                symbol_name, params, ret_name, varargs_suffix);
    }

    free(params);
    return ok;
}

static const char *ccb_effective_global_name(const Node *decl, const char *module_prefix)
{
    if (!decl)
        return NULL;

    if (decl->metadata.backend_name && decl->metadata.backend_name[0])
        return decl->metadata.backend_name;

    if (decl->export_name)
        return decl->var_name;

    (void)module_prefix;
    return decl->var_name;
}

static int ccb_emit_symbol_list(CcbModule *mod, const Symbol *syms, int count, StringList *emitted, int *any)
{
    if (!mod || !syms || count <= 0)
        return 0;

    for (int i = 0; i < count; ++i)
    {
        const Symbol *sym = &syms[i];
        if (!sym || !sym->is_extern)
            continue;
        const char *symbol_name = (sym->backend_name && *sym->backend_name) ? sym->backend_name : sym->name;
        if (!symbol_name || !*symbol_name)
            continue;
        if (string_list_contains(emitted, symbol_name))
            continue;
        if (!ccb_module_append_extern(mod, sym))
            return 1;
        if (sym->is_noreturn)
        {
            if (!ccb_module_appendf(mod, ".no-return %s", symbol_name))
                return 1;
        }
        if (!string_list_append(emitted, symbol_name))
            return 1;
        if (any)
            *any = 1;
    }

    return 0;
}

static int ccb_module_emit_externs(CcbModule *mod, const CodegenOptions *opts)
{
    if (!mod || !opts)
        return 0;

    int total = 0;
    if (opts->externs && opts->extern_count > 0)
        total += opts->extern_count;
    if (opts->imported_externs && opts->imported_extern_count > 0)
        total += opts->imported_extern_count;
    if (total <= 0)
        return 0;

    StringList emitted;
    string_list_init(&emitted);

    int any = 0;
    int rc = ccb_emit_symbol_list(mod, opts->externs, opts->extern_count, &emitted, &any);
    if (!rc)
        rc = ccb_emit_symbol_list(mod, opts->imported_externs, opts->imported_extern_count, &emitted, &any);

    string_list_free(&emitted);
    if (rc)
        return rc;

    if (any)
    {
        if (!ccb_module_append_line(mod, ""))
            return 1;
    }

    return 0;
}

static int ccb_module_emit_imported_globals(CcbModule *mod, const CodegenOptions *opts)
{
    if (!mod || !opts || !opts->imported_globals || opts->imported_global_count <= 0)
        return 0;

    StringList emitted;
    string_list_init(&emitted);
    int any = 0;

    for (int i = 0; i < opts->imported_global_count; ++i)
    {
        const Symbol *sym = &opts->imported_globals[i];
        if (!sym || sym->kind != SYM_GLOBAL || !sym->is_extern)
            continue;
        const char *name = (sym->backend_name && *sym->backend_name) ? sym->backend_name : sym->name;
        if (!name || !*name)
            continue;
        if (string_list_contains(&emitted, name))
            continue;
        CCValueType cc_ty = map_type_to_cc(sym->var_type);
        if (cc_ty == CC_TYPE_VOID)
        {
            fprintf(stderr, "codegen: imported global '%s' has unsupported type\n", name);
            string_list_free(&emitted);
            return 1;
        }
        const char *const_suffix = sym->is_const ? " const" : "";
        if (!ccb_module_appendf(mod, ".global %s type=%s extern%s", name, cc_type_name(cc_ty), const_suffix))
        {
            string_list_free(&emitted);
            return 1;
        }
        if (!string_list_append(&emitted, name))
        {
            string_list_free(&emitted);
            return 1;
        }
        any = 1;
    }

    string_list_free(&emitted);
    if (any)
    {
        if (!ccb_module_append_line(mod, ""))
            return 1;
    }
    return 0;
}

static void ccb_function_builder_init(CcbFunctionBuilder *fb, CcbModule *mod, const Node *fn, bool enable_debug)
{
    if (!fb)
        return;
    fb->module = mod;
    fb->fn = fn;
    fb->ret_type = map_type_to_cc(fn && fn->ret_type ? fn->ret_type : NULL);
    string_list_init(&fb->prologue);
    string_list_init(&fb->body);
    if (enable_debug)
        string_list_enable_debug_tracking(&fb->body);
    fb->locals = NULL;
    fb->locals_count = 0;
    fb->locals_capacity = 0;
    fb->param_count = 0;
    fb->local_count = 0;
    fb->next_label_id = 0;
    fb->scope_depth = 0;
    fb->loop_stack = NULL;
    fb->loop_depth = 0;
    fb->loop_capacity = 0;
}

static void ccb_function_builder_free(CcbFunctionBuilder *fb)
{
    if (!fb)
        return;
    string_list_free(&fb->prologue);
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
    free(fb->loop_stack);
    fb->loop_stack = NULL;
    fb->loop_depth = 0;
    fb->loop_capacity = 0;
}

static char *ccb_dup_absolute_path(const char *path)
{
    if (!path || !*path)
        return NULL;
#if defined(_WIN32)
#ifndef _MAX_PATH
#define _MAX_PATH 32768
#endif
    char buffer[_MAX_PATH];
    if (_fullpath(buffer, path, _MAX_PATH))
        return xstrdup(buffer);
#else
    char *resolved = realpath(path, NULL);
    if (resolved)
        return resolved;
#endif
    return NULL;
}

static uint32_t ccb_module_register_debug_file(CcbModule *mod, const SourceBuffer *src)
{
    if (!mod || !mod->emit_debug || !src || !src->filename || src->filename[0] == '\0')
        return 0;
    char *absolute = ccb_dup_absolute_path(src->filename);
    const char *path = absolute ? absolute : src->filename;
    for (size_t i = 0; i < mod->debug_files.count; ++i)
    {
        const char *existing = mod->debug_files.items[i];
        if (existing && strcmp(existing, path) == 0)
        {
            free(absolute);
            return (uint32_t)(i + 1);
        }
    }
    bool ok = string_list_append(&mod->debug_files, path);
    if (absolute)
        free(absolute);
    if (!ok)
        return 0;
    return (uint32_t)mod->debug_files.count;
}

static void ccb_set_debug_site(CcbFunctionBuilder *fb, const Node *node)
{
    if (!fb)
        return;
    if (!node)
    {
        string_list_clear_debug_location(&fb->body);
        return;
    }
    uint32_t file_id = ccb_module_register_debug_file(fb->module, node->src);
    if (file_id == 0)
    {
        string_list_clear_debug_location(&fb->body);
        return;
    }
    uint32_t line = (node->line > 0) ? (uint32_t)node->line : 1;
    uint32_t column = (node->col > 0) ? (uint32_t)node->col : 1;
    string_list_set_debug_location(&fb->body, file_id, line, column);
}

static int ccb_emit_expr_basic(CcbFunctionBuilder *fb, const Node *expr)
{
    if (!fb)
        return 1;
    StringListDebugState prev = string_list_capture_debug_state(&fb->body);
    ccb_set_debug_site(fb, expr);
    int rc = ccb_emit_expr_basic_impl(fb, expr);
    string_list_restore_debug_state(&fb->body, &prev);
    return rc;
}

static int ccb_emit_stmt_basic(CcbFunctionBuilder *fb, const Node *stmt)
{
    if (!fb)
        return 1;
    StringListDebugState prev = string_list_capture_debug_state(&fb->body);
    ccb_set_debug_site(fb, stmt);
    int rc = ccb_emit_stmt_basic_impl(fb, stmt);
    string_list_restore_debug_state(&fb->body, &prev);
    return rc;
}

static int ccb_append_function_body(CcbModule *mod, const CcbFunctionBuilder *fb)
{
    if (!mod || !fb)
        return 1;
    for (size_t i = 0; i < fb->prologue.count; ++i)
    {
        if (!ccb_module_append_line(mod, fb->prologue.items[i]))
            return 1;
    }
    uint32_t current_file = 0;
    uint32_t current_line = 0;
    uint32_t current_column = 0;

    for (size_t i = 0; i < fb->body.count; ++i)
    {
        if (mod->emit_debug)
        {
            uint32_t file = string_list_get_debug_file(&fb->body, i);
            uint32_t line = string_list_get_debug_line(&fb->body, i);
            uint32_t column = string_list_get_debug_column(&fb->body, i);

            if (file != 0 && (file != current_file || line != current_line || column != current_column))
            {
                if (!ccb_module_appendf(mod, "  .loc %u %u %u", file, line, column))
                    return 1;
                current_file = file;
                current_line = line;
                current_column = column;
            }
            else if (file == 0)
            {
                current_file = 0;
                current_line = 0;
                current_column = 0;
            }
        }

        if (!ccb_module_append_line(mod, fb->body.items[i]))
            return 1;
    }

    return 0;
}

static int ccb_loop_push(CcbFunctionBuilder *fb, const char *break_label, const char *continue_label, bool is_loop)
{
    if (!fb || !break_label)
        return 0;
    if (fb->loop_depth == fb->loop_capacity)
    {
        size_t new_cap = fb->loop_capacity ? fb->loop_capacity * 2 : 4;
        LoopContext *grown = (LoopContext *)realloc(fb->loop_stack, new_cap * sizeof(LoopContext));
        if (!grown)
            return 0;
        fb->loop_stack = grown;
        fb->loop_capacity = new_cap;
    }
    LoopContext *ctx = &fb->loop_stack[fb->loop_depth++];
    strncpy(ctx->break_label, break_label, sizeof(ctx->break_label) - 1);
    ctx->break_label[sizeof(ctx->break_label) - 1] = '\0';
    if (continue_label)
    {
        strncpy(ctx->continue_label, continue_label, sizeof(ctx->continue_label) - 1);
        ctx->continue_label[sizeof(ctx->continue_label) - 1] = '\0';
    }
    else
    {
        ctx->continue_label[0] = '\0';
    }
    ctx->is_loop = is_loop;
    return 1;
}

static void ccb_loop_pop(CcbFunctionBuilder *fb)
{
    if (!fb || fb->loop_depth == 0)
        return;
    fb->loop_depth--;
}

static LoopContext *ccb_loop_top(CcbFunctionBuilder *fb)
{
    if (!fb || fb->loop_depth == 0)
        return NULL;
    return &fb->loop_stack[fb->loop_depth - 1];
}

static LoopContext *ccb_continue_target(CcbFunctionBuilder *fb)
{
    if (!fb)
        return NULL;
    for (size_t idx = fb->loop_depth; idx-- > 0;)
    {
        LoopContext *ctx = &fb->loop_stack[idx];
        if (ctx->is_loop)
            return ctx;
    }
    return NULL;
}

static int ccb_emit_inline_call(CcbFunctionBuilder *fb, const Node *call_expr, const Node *target_fn)
{
    enum
    {
        INLINE_OK = 0,
        INLINE_FATAL = 1,
        INLINE_SKIP = 2
    };

    if (!fb || !call_expr || !target_fn)
        return INLINE_SKIP;

    Node *mutable_target = (Node *)target_fn;
    const char *fn_name = target_fn->name ? target_fn->name : "<anon>";

    compiler_verbose_logf("inline", "consider inline of '%s' (candidate=%d)", fn_name,
                          target_fn->inline_candidate);

#define INLINE_SKIP_WITH_REASON(MSG)                                              \
    do                                                                            \
    {                                                                             \
        if (mutable_target)                                                       \
            mutable_target->inline_needs_body = 1;                                \
        compiler_verbose_logf("inline", "skip inline of '%s': %s", fn_name, MSG); \
        if (compiler_verbose_deep_enabled())                                      \
            compiler_verbose_treef("inline", "|_", "reason: %s", MSG);            \
        return INLINE_SKIP;                                                       \
    } while (0)

    if (!target_fn->inline_candidate || !target_fn->inline_expr)
        INLINE_SKIP_WITH_REASON("no precomputed inline expression");
    if (call_expr->call_is_indirect)
        INLINE_SKIP_WITH_REASON("call is indirect");
    if (target_fn->is_varargs || call_expr->call_is_varargs)
        INLINE_SKIP_WITH_REASON("varargs not supported");
    if (compiler_verbose_deep_enabled())
    {
        compiler_verbose_treef("inline", "|-", "arg count %d vs %d", call_expr->arg_count, target_fn->param_count);
    }
    if (call_expr->arg_count != target_fn->param_count)
        INLINE_SKIP_WITH_REASON("argument count mismatch");

    for (int i = 0; i < target_fn->param_count; ++i)
    {
        Type *param_type = NULL;
        if (target_fn->param_types && i < target_fn->param_count)
            param_type = target_fn->param_types[i];
        if (type_is_address_only(param_type))
        {
            INLINE_SKIP_WITH_REASON("address-only parameter requires byref");
        }
    }

    ccb_scope_enter(fb);
    int status = INLINE_FATAL;

    for (int i = 0; i < target_fn->param_count; ++i)
    {
        const Node *arg = (call_expr->args && i < call_expr->arg_count) ? call_expr->args[i] : NULL;
        if (!arg)
        {
            diag_error_at(call_expr->src, call_expr->line, call_expr->col,
                          "missing argument for inline call");
            if (mutable_target)
                mutable_target->inline_needs_body = 1;
            goto cleanup;
        }

        Type *param_type = NULL;
        if (target_fn->param_types && i < target_fn->param_count)
            param_type = target_fn->param_types[i];
        const char *param_name = NULL;
        if (target_fn->param_names && i < target_fn->param_count)
        {
            const char *name = target_fn->param_names[i];
            if (name && name[0] != '\0')
                param_name = name;
        }

        CCValueType param_ty = map_type_to_cc(param_type);

        if (ccb_emit_expr_basic(fb, arg))
        {
            if (mutable_target)
                mutable_target->inline_needs_body = 1;
            goto cleanup;
        }

        CCValueType arg_ty = ccb_type_for_expr(arg);
        if (param_ty == CC_TYPE_INVALID || param_ty == CC_TYPE_VOID)
            param_ty = arg_ty;

        if (arg_ty != param_ty)
        {
            if (ccb_emit_convert_between(fb, arg_ty, param_ty, arg))
            {
                if (mutable_target)
                    mutable_target->inline_needs_body = 1;
                goto cleanup;
            }
            arg_ty = param_ty;
        }

        CcbLocal *param_local = ccb_local_add(fb, param_name, param_type, false, false);
        if (!param_local)
        {
            diag_error_at(target_fn->src, target_fn->line, target_fn->col,
                          "failed to allocate local for inline parameter %d", i);
            if (mutable_target)
                mutable_target->inline_needs_body = 1;
            goto cleanup;
        }
        param_local->value_type = param_ty;

        if (!ccb_emit_store_local(fb, param_local))
        {
            diag_error_at(arg->src, arg->line, arg->col,
                          "failed to store inline argument");
            if (mutable_target)
                mutable_target->inline_needs_body = 1;
            goto cleanup;
        }
    }

    if (ccb_emit_expr_basic(fb, target_fn->inline_expr))
    {
        compiler_verbose_logf("inline", "abort inline of '%s': failed to emit inline expression", fn_name);
        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("inline", "|_", "failed emitting inline body");
        goto cleanup;
    }

    CCValueType produced_ty = ccb_type_for_expr(target_fn->inline_expr);
    CCValueType expected_ty = ccb_type_for_expr(call_expr);
    if (expected_ty != CC_TYPE_VOID && produced_ty != expected_ty)
    {
        if (ccb_emit_convert_between(fb, produced_ty, expected_ty, call_expr))
        {
            compiler_verbose_logf("inline", "abort inline of '%s': result conversion failed", fn_name);
            if (compiler_verbose_deep_enabled())
                compiler_verbose_treef("inline", "|_", "conversion %d -> %d failed", produced_ty, expected_ty);
            goto cleanup;
        }
    }

    status = INLINE_OK;

cleanup:
    ccb_scope_leave(fb);
#undef INLINE_SKIP_WITH_REASON
    if (status == INLINE_OK)
    {
        compiler_verbose_logf("inline", "inlined call to '%s'", fn_name);
        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("inline", "`-", "successfully spliced body");
    }
    if (status != INLINE_OK && mutable_target)
        mutable_target->inline_needs_body = 1;
    return status;
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

static const char *ccb_binop_symbol(const char *op)
{
    if (!op)
        return "?";
    if (strcmp(op, "add") == 0)
        return "+";
    if (strcmp(op, "sub") == 0)
        return "-";
    if (strcmp(op, "mul") == 0)
        return "*";
    if (strcmp(op, "div") == 0)
        return "/";
    if (strcmp(op, "mod") == 0)
        return "%";
    if (strcmp(op, "and") == 0)
        return "&";
    if (strcmp(op, "or") == 0)
        return "|";
    if (strcmp(op, "xor") == 0)
        return "^";
    if (strcmp(op, "shl") == 0)
        return "<<";
    if (strcmp(op, "shr") == 0)
        return ">>";
    return op;
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

        if (compiler_verbose_deep_enabled())
        {
            char lhs_repr[64];
            char rhs_repr[64];
            char res_repr[64];
            if (lhs.is_signed)
                snprintf(lhs_repr, sizeof(lhs_repr), "(%s)%lld", lhs.type, (long long)lhs_s);
            else
                snprintf(lhs_repr, sizeof(lhs_repr), "(%s)%llu", lhs.type, (unsigned long long)lhs_u);
            if (rhs.is_signed)
                snprintf(rhs_repr, sizeof(rhs_repr), "(%s)%lld", rhs.type, (long long)rhs_s);
            else
                snprintf(rhs_repr, sizeof(rhs_repr), "(%s)%llu", rhs.type, (unsigned long long)rhs_u);
            if (lhs.is_signed)
                snprintf(res_repr, sizeof(res_repr), "(%s)%lld", lhs.type, (long long)result_s);
            else
                snprintf(res_repr, sizeof(res_repr), "(%s)%llu", lhs.type, (unsigned long long)result_u);
            const char *symbol = ccb_binop_symbol(op);
            compiler_verbose_treef("optimizer", "|-", "fold %s %s %s => %s", lhs_repr, symbol, rhs_repr, res_repr);
        }

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
            if (compiler_verbose_deep_enabled())
            {
                const char *preview = body->items[i];
                if (preview)
                {
                    while (*preview == ' ' || *preview == '\t')
                        ++preview;
                }
                char preview_buf[64];
                if (preview && *preview)
                    snprintf(preview_buf, sizeof(preview_buf), "%.48s", preview);
                else
                    snprintf(preview_buf, sizeof(preview_buf), "drop");
                compiler_verbose_treef("optimizer", "|-", "prune %zu instruction%s near '%s'",
                                       remove_count, remove_count == 1 ? "" : "s", preview_buf);
            }
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

    const char *fn_name = (fb->fn && fb->fn->name) ? fb->fn->name : "<anon>";
    if (compiler_verbose_enabled())
        compiler_verbose_logf("optimizer", "optimizing '%s' (O%d)", fn_name, opts->opt_level);
    if (compiler_verbose_deep_enabled())
        compiler_verbose_treef("optimizer", "+-", "pass prune dropped values");
    if (compiler_verbose_enabled())
        compiler_verbose_logf("optimizer", "pass prune dropped values");
    ccb_opt_prune_dropped_values(fb);

    if (opts->opt_level >= 2)
    {
        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass fold constant binops");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass fold constant binops");
        ccb_opt_fold_const_binops(fb);
    }

    if (compiler_verbose_enabled())
        compiler_verbose_logf("optimizer", "completed '%s'", fn_name);
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
    for (size_t i = fb->locals_count; i-- > 0;)
    {
        CcbLocal *local = &fb->locals[i];
        if (!local->name || !local->is_active)
            continue;
        if (local->scope_depth > fb->scope_depth)
            continue;
        if (strcmp(local->name, name) == 0)
            return local;
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
    slot->scope_depth = fb->scope_depth;
    slot->is_active = true;
    if (is_param)
        fb->param_count++;
    else
        fb->local_count++;
    return slot;
}

static int ccb_promote_param_to_local(CcbFunctionBuilder *fb, const Node *site, CcbLocal **inout_local)
{
    if (!fb || !inout_local || !*inout_local)
        return 1;

    CcbLocal *param = *inout_local;
    if (!param->is_param)
        return 0;

    const char *param_name = (param->name && *param->name) ? param->name : "<anonymous>";

    CcbLocal *shadow = ccb_local_add(fb, param->name, param->type, param->is_address_only, false);
    if (!shadow)
    {
        diag_error_at(site ? site->src : NULL, site ? site->line : 0, site ? site->col : 0,
                      "failed to create local copy for parameter '%s'", param_name);
        return 1;
    }

    shadow->value_type = param->value_type;
    shadow->scope_depth = param->scope_depth;

    if (!ccb_append_load_local(&fb->prologue, param))
    {
        diag_error_at(site ? site->src : NULL, site ? site->line : 0, site ? site->col : 0,
                      "failed to read parameter '%s'", param_name);
        return 1;
    }
    if (!ccb_append_store_local(&fb->prologue, shadow))
    {
        diag_error_at(site ? site->src : NULL, site ? site->line : 0, site ? site->col : 0,
                      "failed to copy parameter '%s' into local storage", param_name);
        return 1;
    }

    if (!ccb_rewrite_param_loads(&fb->body, param->index, shadow->index))
    {
        diag_error_at(site ? site->src : NULL, site ? site->line : 0, site ? site->col : 0,
                      "failed to update prior reads of parameter '%s'", param_name);
        return 1;
    }

    *inout_local = shadow;
    return 0;
}

static CcbLocal *ccb_local_from_slot(CcbFunctionBuilder *fb, ptrdiff_t slot)
{
    if (!fb || !fb->locals || slot < 0)
        return NULL;
    size_t idx = (size_t)slot;
    if (idx >= fb->locals_count)
        return NULL;
    return &fb->locals[idx];
}

static bool ccb_struct_copy_refresh(CcbFunctionBuilder *fb, ptrdiff_t dst_slot, ptrdiff_t src_slot, CcbLocal **dst_ptr, CcbLocal **src_ptr)
{
    if (!fb)
        return false;
    if (dst_ptr && dst_slot >= 0)
    {
        CcbLocal *updated = ccb_local_from_slot(fb, dst_slot);
        if (!updated)
            return false;
        *dst_ptr = updated;
    }
    if (src_ptr && src_slot >= 0)
    {
        CcbLocal *updated = ccb_local_from_slot(fb, src_slot);
        if (!updated)
            return false;
        *src_ptr = updated;
    }
    return true;
}

static bool ccb_local_in_current_scope(CcbFunctionBuilder *fb, const char *name)
{
    if (!fb || !name)
        return false;
    for (size_t i = fb->locals_count; i-- > 0;)
    {
        CcbLocal *local = &fb->locals[i];
        if (!local->name || !local->is_active)
            continue;
        if (local->scope_depth != fb->scope_depth)
            continue;
        if (strcmp(local->name, name) == 0)
            return true;
    }
    return false;
}

static void ccb_scope_enter(CcbFunctionBuilder *fb)
{
    if (!fb)
        return;
    fb->scope_depth++;
}

static void ccb_scope_leave(CcbFunctionBuilder *fb)
{
    if (!fb)
        return;
    int depth = fb->scope_depth;
    if (depth <= 0)
        return;
    for (size_t i = fb->locals_count; i-- > 0;)
    {
        CcbLocal *local = &fb->locals[i];
        if (!local->is_active)
            continue;
        if (local->is_param)
            continue;
        if (local->scope_depth == depth)
            local->is_active = false;
    }
    fb->scope_depth = depth - 1;
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
    case ND_NULL:
        return CC_TYPE_PTR;
    case ND_INT:
        return CC_TYPE_I32;
    case ND_FLOAT:
        return expr->type ? map_type_to_cc(expr->type) : CC_TYPE_F64;
    case ND_NEG:
        if (expr->lhs)
            return ccb_type_for_expr(expr->lhs);
        return CC_TYPE_I32;
    case ND_VAR:
        if (expr->var_type && expr->var_type->kind == TY_ARRAY)
        {
            return CC_TYPE_PTR;
        }
        return CC_TYPE_I32;
    case ND_ASSIGN:
        return ccb_type_for_expr(expr->rhs);
    case ND_ADD_ASSIGN:
    case ND_SUB_ASSIGN:
    case ND_MUL_ASSIGN:
    case ND_DIV_ASSIGN:
    case ND_MOD_ASSIGN:
    case ND_BITAND_ASSIGN:
    case ND_BITOR_ASSIGN:
    case ND_BITXOR_ASSIGN:
    case ND_SHL_ASSIGN:
    case ND_SHR_ASSIGN:
        if (expr->lhs)
            return ccb_type_for_expr(expr->lhs);
        return ccb_type_for_expr(expr->rhs);
    case ND_CALL:
        if (expr->type)
            return map_type_to_cc(expr->type);
        return CC_TYPE_I32;
    case ND_CAST:
        if (expr->type)
            return map_type_to_cc(expr->type);
        if (expr->lhs)
            return ccb_type_for_expr(expr->lhs);
        return CC_TYPE_VOID;
    case ND_SEQ:
        if (expr->rhs)
            return ccb_type_for_expr(expr->rhs);
        if (expr->lhs)
            return ccb_type_for_expr(expr->lhs);
        return CC_TYPE_VOID;
    default:
        return CC_TYPE_I32;
    }
}

static int ccb_emit_block(CcbFunctionBuilder *fb, const Node *block, bool push_scope)
{
    if (!fb || !block)
        return 1;
    if (block->kind != ND_BLOCK)
        return ccb_emit_stmt_basic(fb, block);
    if (push_scope)
        ccb_scope_enter(fb);
    for (int i = 0; i < block->stmt_count; ++i)
    {
        if (ccb_emit_stmt_basic(fb, block->stmts[i]))
        {
            if (push_scope)
                ccb_scope_leave(fb);
            return 1;
        }
    }
    if (push_scope)
        ccb_scope_leave(fb);
    return 0;
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

static bool ccb_emit_const_float(StringList *body, CCValueType ty, double value)
{
    if (!body)
        return false;

    char literal[64];
    if (ty == CC_TYPE_F32)
    {
        float fv = (float)value;
        snprintf(literal, sizeof(literal), "%.9g", fv);
    }
    else
    {
        snprintf(literal, sizeof(literal), "%.17g", value);
    }

    if (strcspn(literal, ".eE") == strlen(literal))
    {
        size_t len = strlen(literal);
        if (len + 2 < sizeof(literal))
        {
            literal[len] = '.';
            literal[len + 1] = '0';
            literal[len + 2] = '\0';
        }
    }

    return string_list_appendf(body, "  const %s %s", cc_type_name(ty), literal);
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
    if (ccb_value_type_is_float(ty))
        return ccb_emit_const_float(body, ty, 0.0);
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
    case TY_ARRAY:
        if (ty->array.is_unsized || !ty->array.elem)
            return 8;
        if (ty->array.length <= 0)
            return 0;
        return (size_t)ty->array.length * ccb_type_size_bytes(ty->array.elem);
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

static int ccb_emit_deref_address(CcbFunctionBuilder *fb, const Node *expr, CCValueType *out_elem_ty, const Type **out_elem_type)
{
    if (!fb || !expr || expr->kind != ND_DEREF)
        return 1;

    const Node *base = expr->lhs;
    if (!base)
    {
        diag_error_at(expr->src, expr->line, expr->col,
                      "invalid dereference expression");
        return 1;
    }

    if (ccb_emit_expr_basic(fb, base))
        return 1;

    const Type *base_type = base->type;
    CCValueType base_ty = ccb_type_for_expr(base);
    if (base_type && base_type->kind == TY_PTR)
    {
        base_ty = CC_TYPE_PTR;
    }
    else if (base_ty != CC_TYPE_PTR)
    {
        diag_error_at(base->src, base->line, base->col,
                      "dereference operand is not a pointer");
        return 1;
    }

    if (ccb_emit_convert_between(fb, base_ty, CC_TYPE_PTR, base))
        return 1;

    const Type *elem_type = (base_type && base_type->kind == TY_PTR) ? base_type->pointee : NULL;
    CCValueType elem_cc_ty = map_type_to_cc(elem_type);
    if (elem_type && elem_type->kind == TY_STRUCT)
    {
        // struct loads handled by caller via address-only path
    }
    else if (elem_cc_ty == CC_TYPE_INVALID || elem_cc_ty == CC_TYPE_VOID)
    {
        elem_cc_ty = CC_TYPE_I32;
    }

    if (out_elem_ty)
        *out_elem_ty = elem_cc_ty;
    if (out_elem_type)
        *out_elem_type = elem_type;
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
    if (elem_type && elem_type->kind == TY_STRUCT)
    {
        // Struct: use struct type
        // (leave elem_cc_ty as mapped)
    }
    else if (elem_cc_ty == CC_TYPE_INVALID || elem_cc_ty == CC_TYPE_VOID)
    {
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

static const char *ccb_compound_op_name(NodeKind kind)
{
    switch (kind)
    {
    case ND_ADD_ASSIGN:
        return "add";
    case ND_SUB_ASSIGN:
        return "sub";
    case ND_MUL_ASSIGN:
        return "mul";
    case ND_DIV_ASSIGN:
        return "div";
    case ND_MOD_ASSIGN:
        return "mod";
    case ND_BITAND_ASSIGN:
        return "and";
    case ND_BITOR_ASSIGN:
        return "or";
    case ND_BITXOR_ASSIGN:
        return "xor";
    case ND_SHL_ASSIGN:
        return "shl";
    case ND_SHR_ASSIGN:
        return "shr";
    default:
        return "add";
    }
}

static int ccb_emit_compound_binop_instr(CcbFunctionBuilder *fb, const Node *expr, CCValueType value_ty)
{
    if (!fb || !expr)
        return 1;
    const char *op = ccb_compound_op_name(expr->kind);
    bool is_div_or_mod = (expr->kind == ND_DIV_ASSIGN) || (expr->kind == ND_MOD_ASSIGN);
    bool is_shift_right = (expr->kind == ND_SHR_ASSIGN);
    bool is_int = ccb_value_type_is_integer(value_ty);
    bool use_unsigned = (is_div_or_mod || is_shift_right) && is_int && !ccb_value_type_is_signed(value_ty);
    if (use_unsigned)
    {
        if (!string_list_appendf(&fb->body, "  binop %s %s unsigned", op, cc_type_name(value_ty)))
            return 1;
    }
    else
    {
        if (!string_list_appendf(&fb->body, "  binop %s %s", op, cc_type_name(value_ty)))
            return 1;
    }
    return 0;
}

static int ccb_compound_convert_rhs(CcbFunctionBuilder *fb, const Node *rhs, CCValueType desired_ty)
{
    if (!fb || !rhs)
        return 1;
    CCValueType rhs_ty = ccb_type_for_expr(rhs);
    if (rhs_ty == CC_TYPE_INVALID)
        rhs_ty = desired_ty;
    if (rhs_ty == desired_ty)
        return 0;
    return ccb_emit_convert_between(fb, rhs_ty, desired_ty, rhs);
}

static int ccb_emit_compound_assign_via_pointer(CcbFunctionBuilder *fb, const Node *expr, CcbLocal *addr_local, CCValueType value_ty, Type *value_type)
{
    if (!fb || !expr || !addr_local)
        return 1;

    ptrdiff_t addr_slot = addr_local ? (ptrdiff_t)(addr_local - fb->locals) : -1;

    if (!ccb_emit_load_local(fb, addr_local))
        return 1;
    if (!ccb_emit_load_indirect(&fb->body, value_ty))
        return 1;

    CcbLocal *lhs_tmp = ccb_local_add(fb, NULL, value_type, false, false);
    if (!lhs_tmp)
        return 1;
    ptrdiff_t lhs_slot = lhs_tmp ? (ptrdiff_t)(lhs_tmp - fb->locals) : -1;
    if (!ccb_emit_store_local(fb, lhs_tmp))
        return 1;

    if (ccb_emit_expr_basic(fb, expr->rhs))
        return 1;
    if (ccb_compound_convert_rhs(fb, expr->rhs, value_ty))
        return 1;

    CcbLocal *rhs_tmp = ccb_local_add(fb, NULL, value_type, false, false);
    if (!rhs_tmp)
        return 1;
    ptrdiff_t rhs_slot = rhs_tmp ? (ptrdiff_t)(rhs_tmp - fb->locals) : -1;
    if (!ccb_emit_store_local(fb, rhs_tmp))
        return 1;

    if (addr_slot >= 0)
    {
        addr_local = ccb_local_from_slot(fb, addr_slot);
        if (!addr_local)
            return 1;
    }
    if (lhs_slot >= 0)
    {
        lhs_tmp = ccb_local_from_slot(fb, lhs_slot);
        if (!lhs_tmp)
            return 1;
    }
    if (rhs_slot >= 0)
    {
        rhs_tmp = ccb_local_from_slot(fb, rhs_slot);
        if (!rhs_tmp)
            return 1;
    }

    if (!ccb_emit_load_local(fb, lhs_tmp))
        return 1;
    if (!ccb_emit_load_local(fb, rhs_tmp))
        return 1;
    if (ccb_emit_compound_binop_instr(fb, expr, value_ty))
        return 1;

    if (!ccb_emit_store_local(fb, lhs_tmp))
        return 1;
    if (!ccb_emit_load_local(fb, addr_local))
        return 1;
    if (!ccb_emit_load_local(fb, lhs_tmp))
        return 1;
    if (!ccb_emit_store_indirect(&fb->body, value_ty))
        return 1;
    if (!ccb_emit_load_local(fb, lhs_tmp))
        return 1;

    return 0;
}

static int ccb_emit_compound_assign_local(CcbFunctionBuilder *fb, const Node *expr, const Node *target, CcbLocal *local)
{
    if (!fb || !expr || !target || !local)
        return 1;

    if (local->is_param)
    {
        if (ccb_promote_param_to_local(fb, target, &local))
            return 1;
    }

    if (local->is_address_only || type_is_address_only(local->type))
    {
        diag_error_at(target->src, target->line, target->col,
                      "compound assignment not supported on aggregate locals");
        return 1;
    }

    ptrdiff_t local_slot = local ? (ptrdiff_t)(local - fb->locals) : -1;

    CCValueType value_ty = ccb_type_for_expr(expr);
    if (value_ty == CC_TYPE_INVALID)
        value_ty = local->value_type;

    Type *local_type = local->type ? local->type : (Type *)expr->type;

    CcbLocal *lhs_tmp = ccb_local_add(fb, NULL, local_type, false, false);
    if (!lhs_tmp)
        return 1;
    ptrdiff_t lhs_slot = lhs_tmp ? (ptrdiff_t)(lhs_tmp - fb->locals) : -1;

    if (local_slot >= 0)
    {
        local = ccb_local_from_slot(fb, local_slot);
        if (!local)
            return 1;
    }
    if (lhs_slot >= 0)
    {
        lhs_tmp = ccb_local_from_slot(fb, lhs_slot);
        if (!lhs_tmp)
            return 1;
    }

    if (!ccb_emit_load_local(fb, local))
        return 1;
    if (!ccb_emit_store_local(fb, lhs_tmp))
        return 1;

    if (ccb_emit_expr_basic(fb, expr->rhs))
        return 1;
    if (ccb_compound_convert_rhs(fb, expr->rhs, value_ty))
        return 1;

    CcbLocal *rhs_tmp = ccb_local_add(fb, NULL, local_type, false, false);
    if (!rhs_tmp)
        return 1;
    ptrdiff_t rhs_slot = rhs_tmp ? (ptrdiff_t)(rhs_tmp - fb->locals) : -1;

    if (local_slot >= 0)
    {
        local = ccb_local_from_slot(fb, local_slot);
        if (!local)
            return 1;
    }
    if (lhs_slot >= 0)
    {
        lhs_tmp = ccb_local_from_slot(fb, lhs_slot);
        if (!lhs_tmp)
            return 1;
    }
    if (rhs_slot >= 0)
    {
        rhs_tmp = ccb_local_from_slot(fb, rhs_slot);
        if (!rhs_tmp)
            return 1;
    }

    if (!ccb_emit_store_local(fb, rhs_tmp))
        return 1;

    if (!ccb_emit_load_local(fb, lhs_tmp))
        return 1;
    if (!ccb_emit_load_local(fb, rhs_tmp))
        return 1;
    if (ccb_emit_compound_binop_instr(fb, expr, value_ty))
        return 1;

    if (!ccb_emit_store_local(fb, local))
        return 1;
    if (!ccb_emit_load_local(fb, local))
        return 1;

    return 0;
}

static int ccb_emit_compound_assign_global(CcbFunctionBuilder *fb, const Node *expr, const Node *target)
{
    if (!fb || !expr || !target || !target->var_ref)
        return 1;

    CCValueType value_ty = ccb_type_for_expr(expr);
    Type *target_type = target->type ? target->type : target->var_type;
    if (type_is_address_only(target_type))
    {
        diag_error_at(target->src, target->line, target->col,
                      "compound assignment not supported on aggregate globals");
        return 1;
    }
    if (value_ty == CC_TYPE_INVALID)
        value_ty = map_type_to_cc(target_type);

    CcbLocal *lhs_tmp = ccb_local_add(fb, NULL, target_type, false, false);
    if (!lhs_tmp)
        return 1;
    ptrdiff_t lhs_slot = lhs_tmp ? (ptrdiff_t)(lhs_tmp - fb->locals) : -1;

    if (!ccb_emit_load_global(&fb->body, target->var_ref))
        return 1;
    if (lhs_slot >= 0)
    {
        lhs_tmp = ccb_local_from_slot(fb, lhs_slot);
        if (!lhs_tmp)
            return 1;
    }
    if (!ccb_emit_store_local(fb, lhs_tmp))
        return 1;

    if (ccb_emit_expr_basic(fb, expr->rhs))
        return 1;
    if (ccb_compound_convert_rhs(fb, expr->rhs, value_ty))
        return 1;

    CcbLocal *rhs_tmp = ccb_local_add(fb, NULL, target_type, false, false);
    if (!rhs_tmp)
        return 1;
    ptrdiff_t rhs_slot = rhs_tmp ? (ptrdiff_t)(rhs_tmp - fb->locals) : -1;

    if (lhs_slot >= 0)
    {
        lhs_tmp = ccb_local_from_slot(fb, lhs_slot);
        if (!lhs_tmp)
            return 1;
    }
    if (rhs_slot >= 0)
    {
        rhs_tmp = ccb_local_from_slot(fb, rhs_slot);
        if (!rhs_tmp)
            return 1;
    }

    if (!ccb_emit_store_local(fb, rhs_tmp))
        return 1;

    if (!ccb_emit_load_local(fb, lhs_tmp))
        return 1;
    if (!ccb_emit_load_local(fb, rhs_tmp))
        return 1;
    if (ccb_emit_compound_binop_instr(fb, expr, value_ty))
        return 1;

    if (!ccb_emit_store_global(&fb->body, target->var_ref))
        return 1;
    if (!ccb_emit_load_global(&fb->body, target->var_ref))
        return 1;

    return 0;
}

static int ccb_emit_compound_assign(CcbFunctionBuilder *fb, const Node *expr)
{
    if (!fb || !expr || !expr->lhs || !expr->rhs)
    {
        diag_error_at(expr ? expr->src : NULL, expr ? expr->line : 0, expr ? expr->col : 0,
                      "compound assignment missing operand");
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
            if (target->var_is_global)
                return ccb_emit_compound_assign_global(fb, expr, target);
            diag_error_at(target->src, target->line, target->col,
                          "unknown local '%s'", target->var_ref);
            return 1;
        }
        return ccb_emit_compound_assign_local(fb, expr, target, local);
    }
    case ND_INDEX:
    {
        CCValueType elem_ty = CC_TYPE_I32;
        const Type *elem_type = NULL;
        if (ccb_emit_index_address(fb, target, &elem_ty, &elem_type))
            return 1;
        if (type_is_address_only(elem_type))
        {
            diag_error_at(target->src, target->line, target->col,
                          "compound assignment not supported on this element type");
            return 1;
        }
        Type *ptr_ty = type_ptr(elem_type ? (Type *)elem_type : (Type *)expr->type);
        CcbLocal *addr_local = ccb_local_add(fb, NULL, ptr_ty, false, false);
        if (!addr_local)
            return 1;
        if (!ccb_emit_store_local(fb, addr_local))
            return 1;
        Type *value_type = elem_type ? (Type *)elem_type : (Type *)expr->type;
        return ccb_emit_compound_assign_via_pointer(fb, expr, addr_local, elem_ty, value_type);
    }
    case ND_DEREF:
    {
        CCValueType elem_ty = CC_TYPE_I32;
        const Type *elem_type = NULL;
        if (ccb_emit_deref_address(fb, target, &elem_ty, &elem_type))
            return 1;
        if (type_is_address_only(elem_type))
        {
            diag_error_at(target->src, target->line, target->col,
                          "compound assignment not supported on this pointee type");
            return 1;
        }
        Type *ptr_ty = type_ptr(elem_type ? (Type *)elem_type : (Type *)expr->type);
        CcbLocal *addr_local = ccb_local_add(fb, NULL, ptr_ty, false, false);
        if (!addr_local)
            return 1;
        if (!ccb_emit_store_local(fb, addr_local))
            return 1;
        Type *value_type = elem_type ? (Type *)elem_type : (Type *)expr->type;
        return ccb_emit_compound_assign_via_pointer(fb, expr, addr_local, elem_ty, value_type);
    }
    case ND_MEMBER:
    {
        CCValueType field_ty = CC_TYPE_I32;
        const Type *field_type = NULL;
        if (ccb_emit_member_address(fb, target, &field_ty, &field_type))
            return 1;
        if (type_is_address_only(field_type))
        {
            diag_error_at(target->src, target->line, target->col,
                          "compound assignment not supported on this field type");
            return 1;
        }
        Type *ptr_ty = type_ptr(field_type ? (Type *)field_type : (Type *)expr->type);
        CcbLocal *addr_local = ccb_local_add(fb, NULL, ptr_ty, false, false);
        if (!addr_local)
            return 1;
        if (!ccb_emit_store_local(fb, addr_local))
            return 1;
        Type *value_type = field_type ? (Type *)field_type : (Type *)expr->type;
        return ccb_emit_compound_assign_via_pointer(fb, expr, addr_local, field_ty, value_type);
    }
    default:
        diag_error_at(target->src, target->line, target->col,
                      "assignment target %s not supported",
                      node_kind_name(target->kind));
        return 1;
    }
}

static bool type_is_address_only(const Type *ty)
{
    if (!ty)
        return false;
    if (ty->kind == TY_STRUCT)
        return true;
    if (ty->kind == TY_ARRAY)
        return !ty->array.is_unsized;
    return false;
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

    ptrdiff_t dst_slot = dst_ptr ? (ptrdiff_t)(dst_ptr - fb->locals) : -1;
    ptrdiff_t src_slot = src_ptr ? (ptrdiff_t)(src_ptr - fb->locals) : -1;

    if (!ccb_struct_copy_refresh(fb, dst_slot, src_slot, &dst_ptr, &src_ptr))
        return 1;

    int field_count = struct_type->strct.field_count;
    for (int i = 0; i < field_count; ++i)
    {
        if (!ccb_struct_copy_refresh(fb, dst_slot, src_slot, &dst_ptr, &src_ptr))
            return 1;
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
            ptrdiff_t dst_field_slot = dst_field - fb->locals;
            if (!ccb_struct_copy_refresh(fb, dst_slot, src_slot, &dst_ptr, &src_ptr))
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
            ptrdiff_t src_field_slot = src_field - fb->locals;
            if (!ccb_struct_copy_refresh(fb, dst_slot, src_slot, &dst_ptr, &src_ptr))
                return 1;
            if (!ccb_emit_load_local(fb, src_ptr))
                return 1;
            if (ccb_emit_pointer_offset(fb, field_offset, NULL))
                return 1;
            if (!ccb_emit_store_local(fb, src_field))
                return 1;

            dst_field = ccb_local_from_slot(fb, dst_field_slot);
            src_field = ccb_local_from_slot(fb, src_field_slot);
            if (!dst_field || !src_field)
                return 1;

            if (ccb_emit_struct_copy(fb, field_type, dst_field, src_field))
                return 1;
            if (!ccb_struct_copy_refresh(fb, dst_slot, src_slot, &dst_ptr, &src_ptr))
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

        if (!ccb_struct_copy_refresh(fb, dst_slot, src_slot, &dst_ptr, &src_ptr))
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

        Type *ptr_ty = type_ptr((Type *)struct_type);

        CcbLocal *src_ptr = ccb_local_add(fb, NULL, ptr_ty, false, false);
        CcbLocal *dst_ptr = ccb_local_add(fb, NULL, ptr_ty, false, false);
        if (!src_ptr || !dst_ptr)
            return 1;

        if (ccb_emit_expr_basic(fb, init))
            return 1;

        CCValueType init_ty = ccb_type_for_expr(init);
        if (init_ty != CC_TYPE_PTR)
        {
            if (ccb_emit_convert_between(fb, init_ty, CC_TYPE_PTR, init))
                return 1;
        }
        if (!ccb_emit_store_local(fb, src_ptr))
            return 1;

        if (ccb_emit_expr_basic(fb, &var_ref))
            return 1;
        if (!ccb_emit_store_local(fb, dst_ptr))
            return 1;

        if (ccb_emit_struct_copy(fb, struct_type, dst_ptr, src_ptr))
            return 1;
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

static int ccb_emit_struct_literal_expr(CcbFunctionBuilder *fb, const Node *literal)
{
    if (!fb || !literal)
        return 1;
    if (!literal->type || literal->type->kind != TY_STRUCT)
    {
        diag_error_at(literal->src, literal->line, literal->col,
                      "initializer list literal requires struct type");
        return 1;
    }

    char temp_name_buf[32];
    ccb_make_label(fb, temp_name_buf, sizeof(temp_name_buf), "lit");
    char *temp_name = (char *)xmalloc(strlen(temp_name_buf) + 1);
    strcpy(temp_name, temp_name_buf);

    CcbLocal *temp_local = ccb_local_add(fb, temp_name, literal->type, true, false);
    if (!temp_local)
    {
        diag_error_at(literal->src, literal->line, literal->col,
                      "failed to allocate storage for struct literal");
        return 1;
    }

    size_t size_bytes = ccb_type_size_bytes(literal->type);
    if (size_bytes == 0)
        size_bytes = 1;
    size_t alignment = 8;
    if (!string_list_appendf(&fb->body, "  stack_alloc %zu %zu", size_bytes, alignment))
        return 1;
    if (!ccb_emit_store_local(fb, temp_local))
        return 1;

    if (ccb_emit_struct_initializer(fb, literal, temp_name, literal->type, literal))
        return 1;

    if (!ccb_emit_load_local(fb, temp_local))
        return 1;
    return 0;
}

static int ccb_emit_array_zero(CcbFunctionBuilder *fb, const Node *var_decl, const char *var_name, const Type *array_type)
{
    if (!fb || !var_name || !array_type || array_type->kind != TY_ARRAY || array_type->array.is_unsized)
        return 0;

    int length = array_type->array.length;
    if (length <= 0)
        return 0;

    const Type *elem_type = array_type->array.elem;
    if (!elem_type)
        elem_type = type_i32();

    if (type_is_address_only(elem_type))
    {
        diag_error_at(var_decl ? var_decl->src : NULL, var_decl ? var_decl->line : 0, var_decl ? var_decl->col : 0,
                      "array elements with aggregate types cannot be zero-initialized yet");
        return 1;
    }

    Node base_ref = {0};
    base_ref.kind = ND_VAR;
    base_ref.var_ref = var_name;
    base_ref.type = type_ptr((Type *)elem_type);
    base_ref.var_type = (Type *)array_type;
    base_ref.var_is_array = 1;
    base_ref.var_is_const = var_decl ? var_decl->var_is_const : 0;
    base_ref.var_is_global = var_decl ? var_decl->var_is_global : 0;
    base_ref.src = var_decl ? var_decl->src : NULL;
    base_ref.line = var_decl ? var_decl->line : 0;
    base_ref.col = var_decl ? var_decl->col : 0;

    for (int i = 0; i < length; ++i)
    {
        Node idx_lit = {0};
        idx_lit.kind = ND_INT;
        idx_lit.int_val = i;
        idx_lit.type = type_i32();
        idx_lit.src = base_ref.src;
        idx_lit.line = base_ref.line;
        idx_lit.col = base_ref.col;

        Node idx_expr = {0};
        idx_expr.kind = ND_INDEX;
        idx_expr.lhs = &base_ref;
        idx_expr.rhs = &idx_lit;
        idx_expr.type = (Type *)elem_type;
        idx_expr.src = base_ref.src;
        idx_expr.line = base_ref.line;
        idx_expr.col = base_ref.col;

        CCValueType elem_ty = map_type_to_cc(elem_type);
        if (elem_ty == CC_TYPE_INVALID || elem_ty == CC_TYPE_VOID)
            elem_ty = CC_TYPE_I32;

        if (ccb_emit_index_address(fb, &idx_expr, &elem_ty, NULL))
            return 1;
        if (!ccb_emit_const_zero(&fb->body, elem_ty))
            return 1;
        if (!ccb_emit_store_indirect(&fb->body, elem_ty))
            return 1;
    }

    return 0;
}

static int ccb_emit_array_initializer(CcbFunctionBuilder *fb, const Node *var_decl, const char *var_name, const Type *array_type, const Node *init)
{
    if (!fb || !var_name || !array_type || array_type->kind != TY_ARRAY || array_type->array.is_unsized)
        return 0;
    if (!init)
        return 0;
    if (init->kind != ND_INIT_LIST)
    {
        diag_error_at(init->src, init->line, init->col,
                      "unsupported initializer for fixed-size array");
        return 1;
    }

    const Type *elem_type = array_type->array.elem;
    if (!elem_type)
        elem_type = type_i32();

    if (type_is_address_only(elem_type) && !init->init.is_zero)
    {
        diag_error_at(init->src, init->line, init->col,
                      "array initializer lists for aggregate element types are not supported yet");
        return 1;
    }

    if (init->init.is_zero || init->init.count == 0)
        return ccb_emit_array_zero(fb, var_decl, var_name, array_type);

    if (ccb_emit_array_zero(fb, var_decl, var_name, array_type))
        return 1;

    Node base_ref = {0};
    base_ref.kind = ND_VAR;
    base_ref.var_ref = var_name;
    base_ref.type = type_ptr((Type *)elem_type);
    base_ref.var_type = (Type *)array_type;
    base_ref.var_is_array = 1;
    base_ref.var_is_const = var_decl ? var_decl->var_is_const : 0;
    base_ref.var_is_global = var_decl ? var_decl->var_is_global : 0;
    base_ref.src = var_decl ? var_decl->src : NULL;
    base_ref.line = var_decl ? var_decl->line : 0;
    base_ref.col = var_decl ? var_decl->col : 0;

    int limit = init->init.count;
    if (array_type->array.length >= 0 && limit > array_type->array.length)
        limit = array_type->array.length;

    for (int i = 0; i < limit; ++i)
    {
        const Node *value = (init->init.elems && i < init->init.count) ? init->init.elems[i] : NULL;
        if (!value)
        {
            diag_error_at(init->src, init->line, init->col,
                          "missing initializer expression for array element %d", i);
            return 1;
        }

        Node idx_lit = {0};
        idx_lit.kind = ND_INT;
        idx_lit.int_val = i;
        idx_lit.type = type_i32();
        idx_lit.src = base_ref.src;
        idx_lit.line = base_ref.line;
        idx_lit.col = base_ref.col;

        Node idx_expr = {0};
        idx_expr.kind = ND_INDEX;
        idx_expr.lhs = &base_ref;
        idx_expr.rhs = &idx_lit;
        idx_expr.type = (Type *)elem_type;
        idx_expr.src = value->src;
        idx_expr.line = value->line;
        idx_expr.col = value->col;

        CCValueType elem_ty = map_type_to_cc(elem_type);
        if (ccb_emit_index_address(fb, &idx_expr, &elem_ty, NULL))
            return 1;

        if (ccb_emit_expr_basic(fb, value))
            return 1;

        CCValueType value_ty = ccb_type_for_expr(value);
        if (ccb_emit_convert_between(fb, value_ty, elem_ty, value))
            return 1;

        if (!ccb_emit_store_indirect(&fb->body, elem_ty))
            return 1;
    }

    return 0;
}

static bool ccb_append_load_local(StringList *list, const CcbLocal *local)
{
    if (!list || !local)
        return false;
    if (local->is_param)
        return string_list_appendf(list, "  load_param %d", local->index);
    return string_list_appendf(list, "  load_local %d", local->index);
}

static bool ccb_append_store_local(StringList *list, const CcbLocal *local)
{
    if (!list || !local)
        return false;
    if (local->is_param)
        return false;
    return string_list_appendf(list, "  store_local %d", local->index);
}

static bool ccb_emit_load_local(CcbFunctionBuilder *fb, const CcbLocal *local)
{
    if (!fb)
        return false;
    return ccb_append_load_local(&fb->body, local);
}

static bool ccb_emit_store_local(CcbFunctionBuilder *fb, const CcbLocal *local)
{
    if (!fb)
        return false;
    return ccb_append_store_local(&fb->body, local);
}

static bool ccb_rewrite_param_loads(StringList *list, int param_index, int local_index)
{
    if (!list)
        return false;
    const char *keyword = "load_param";
    size_t keyword_len = strlen(keyword);
    for (size_t i = 0; i < list->count; ++i)
    {
        char *line = list->items[i];
        if (!line)
            continue;

        const char *trim = line;
        while (*trim == ' ' || *trim == '\t')
            ++trim;
        if (strncmp(trim, keyword, keyword_len) != 0)
            continue;

        const char *cursor = trim + keyword_len;
        while (*cursor == ' ' || *cursor == '\t')
            ++cursor;

        char *endptr = NULL;
        long found = strtol(cursor, &endptr, 10);
        if (endptr == cursor)
            continue;
        if ((int)found != param_index)
            continue;

        size_t indent_len = (size_t)(trim - line);
        int needed = snprintf(NULL, 0, "%.*sload_local %d", (int)indent_len, line, local_index);
        if (needed < 0)
            return false;
        char *replacement = (char *)malloc((size_t)needed + 1);
        if (!replacement)
            return false;
        snprintf(replacement, (size_t)needed + 1, "%.*sload_local %d", (int)indent_len, line, local_index);
        free(list->items[i]);
        list->items[i] = replacement;
    }
    return true;
}

static bool ccb_emit_load_global(StringList *body, const char *name)
{
    if (!body || !name || !*name)
        return false;
    return string_list_appendf(body, "  load_global %s", name);
}

static bool ccb_emit_store_global(StringList *body, const char *name)
{
    if (!body || !name || !*name)
        return false;
    return string_list_appendf(body, "  store_global %s", name);
}

static bool ccb_emit_addr_global(StringList *body, const char *name)
{
    if (!body || !name || !*name)
        return false;
    return string_list_appendf(body, "  addr_global %s", name);
}

static int ccb_emit_global_incdec(CcbFunctionBuilder *fb, const Node *expr, bool is_increment, bool is_prefix)
{
    if (!fb || !expr || !expr->lhs || !expr->lhs->var_ref)
    {
        diag_error_at(expr ? expr->src : NULL, expr ? expr->line : 0, expr ? expr->col : 0,
                      "malformed global %s operation",
                      is_increment ? "increment" : "decrement");
        return 1;
    }

    const Node *target = expr->lhs;
    const char *name = target->var_ref;
    const Type *target_type = target->type ? target->type : expr->type;
    CCValueType val_ty = map_type_to_cc(target_type);
    bool is_ptr = (val_ty == CC_TYPE_PTR);

    if (!is_ptr && !ccb_value_type_is_integer(val_ty))
    {
        diag_error_at(expr->src, expr->line, expr->col,
                      "%s is only supported on integer or pointer globals",
                      is_increment ? "increment" : "decrement");
        return 1;
    }

    CcbLocal *temp = NULL;
    if (!is_prefix)
    {
        temp = ccb_local_add(fb, NULL, (Type *)target_type, false, false);
        if (!temp)
            return 1;
    }

    if (!ccb_emit_load_global(&fb->body, name))
        return 1;

    if (!is_prefix)
    {
        if (!ccb_emit_store_local(fb, temp))
            return 1;
        if (!ccb_emit_load_local(fb, temp))
            return 1;
    }

    if (is_ptr)
    {
        if (ccb_emit_convert_between(fb, val_ty, CC_TYPE_I64, expr))
            return 1;
        size_t elem_size = ccb_pointer_elem_size(target_type);
        if (elem_size == 0)
            elem_size = 1;
        if (!ccb_emit_const(&fb->body, CC_TYPE_I64, (int64_t)elem_size))
            return 1;
        const char *op = is_increment ? "add" : "sub";
        if (!string_list_appendf(&fb->body, "  binop %s %s", op, cc_type_name(CC_TYPE_I64)))
            return 1;
        if (ccb_emit_convert_between(fb, CC_TYPE_I64, CC_TYPE_PTR, expr))
            return 1;
    }
    else
    {
        if (!ccb_emit_const(&fb->body, val_ty, 1))
            return 1;
        const char *op = is_increment ? "add" : "sub";
        if (!string_list_appendf(&fb->body, "  binop %s %s", op, cc_type_name(val_ty)))
            return 1;
    }

    if (!ccb_emit_store_global(&fb->body, name))
        return 1;

    if (is_prefix)
    {
        if (!ccb_emit_load_global(&fb->body, name))
            return 1;
    }
    else if (temp)
    {
        if (!ccb_emit_load_local(fb, temp))
            return 1;
    }

    return 0;
}

static int ccb_emit_call_like(CcbFunctionBuilder *fb, const Node *expr, bool force_indirect)
{
    if (!fb || !expr)
        return 1;

    const int is_indirect = force_indirect ? 1 : expr->call_is_indirect;

    if (!is_indirect && compiler_verbose_enabled())
    {
        const char *call_name = expr->call_name && *expr->call_name ? expr->call_name
                                                                    : (expr->call_target && expr->call_target->name
                                                                           ? expr->call_target->name
                                                                           : "<call>");
        if (expr->call_target)
        {
            const char *status = expr->call_target->inline_candidate ? "inline candidate" : "will emit call";
            compiler_verbose_logf("codegen", "evaluating call '%s' (%s)", call_name, status);
        }
        else
        {
            compiler_verbose_logf("codegen", "evaluating call '%s' (no direct target metadata)", call_name);
        }
    }

    if (!is_indirect && expr->call_target && expr->call_target->inline_candidate)
    {
        int inline_rc = ccb_emit_inline_call(fb, expr, expr->call_target);
        if (inline_rc == 0)
            return 0;
        if (inline_rc == 1)
            return 1;
    }

    CCValueType *arg_types = NULL;
    if (expr->arg_count > 0)
        arg_types = (CCValueType *)xcalloc((size_t)expr->arg_count, sizeof(CCValueType));

    const int fixed_param_count = expr->call_target ? expr->call_target->param_count : -1;
    const bool call_is_varargs = expr->call_is_varargs || (expr->call_target && expr->call_target->is_varargs);

    int rc = 0;
    for (int i = 0; i < expr->arg_count; ++i)
    {
        const Node *arg = expr->args ? expr->args[i] : NULL;
        if (ccb_emit_expr_basic(fb, arg))
        {
            rc = 1;
            break;
        }

        CCValueType arg_ty = ccb_type_for_expr(arg);
        bool is_vararg_slot = call_is_varargs && (fixed_param_count < 0 || i >= fixed_param_count);
        if (is_vararg_slot)
        {
            // Default promotions: float -> double, small ints -> int
            if (arg_ty == CC_TYPE_F32)
            {
                if (ccb_emit_convert_between(fb, CC_TYPE_F32, CC_TYPE_F64, arg))
                {
                    rc = 1;
                    break;
                }
                arg_ty = CC_TYPE_F64;
            }
            else if (arg_ty == CC_TYPE_I8 || arg_ty == CC_TYPE_U8 || arg_ty == CC_TYPE_I16 || arg_ty == CC_TYPE_U16 || arg_ty == CC_TYPE_I1)
            {
                if (ccb_emit_convert_between(fb, arg_ty, CC_TYPE_I32, arg))
                {
                    rc = 1;
                    break;
                }
                arg_ty = CC_TYPE_I32;
            }
        }

        if (arg_types)
            arg_types[i] = arg_ty;
    }

    if (!rc && is_indirect)
    {
        if (!expr->lhs)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "indirect call missing target expression");
            rc = 1;
        }
        else if (ccb_emit_expr_basic(fb, expr->lhs))
        {
            rc = 1;
        }
        else
        {
            CCValueType target_ty = ccb_type_for_expr(expr->lhs);
            if (target_ty != CC_TYPE_PTR)
            {
                if (ccb_emit_convert_between(fb, target_ty, CC_TYPE_PTR, expr->lhs))
                    rc = 1;
            }
        }
    }

    if (!rc)
    {
        CCValueType ret_ty = ccb_type_for_expr(expr);
        const char *ret_name = cc_type_name(ret_ty);
        char *arg_text = NULL;
        if (!ccb_format_type_list(arg_types, (size_t)expr->arg_count, &arg_text))
        {
            rc = 1;
        }
        else if (is_indirect)
        {
            const char *suffix = expr->call_is_varargs ? " varargs" : "";
            if (!string_list_appendf(&fb->body, "  call_indirect %s %s%s", ret_name, arg_text, suffix))
                rc = 1;
        }
        else
        {
            if (!expr->call_name || !*expr->call_name)
            {
                diag_error_at(expr->src, expr->line, expr->col,
                              "function call missing symbol name");
                rc = 1;
            }
            else if (!string_list_appendf(&fb->body, "  call %s %s %s", expr->call_name, ret_name, arg_text))
            {
                rc = 1;
            }
        }
        free(arg_text);
    }

    free(arg_types);
    return rc;
}

static int ccb_emit_expr_basic_impl(CcbFunctionBuilder *fb, const Node *expr)
{
    if (!fb || !expr)
        return 1;

    switch (expr->kind)
    {
    case ND_NULL:
    {
        if (!ccb_emit_const_zero(&fb->body, CC_TYPE_PTR))
            return 1;
        return 0;
    }
    case ND_INIT_LIST:
    {
        if (!expr->type || expr->type->kind != TY_STRUCT)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "initializer expressions are only supported for struct literals");
            return 1;
        }
        return ccb_emit_struct_literal_expr(fb, expr);
    }
    case ND_INT:
    {
        CCValueType ty = map_type_to_cc(expr->type);
        if (!ccb_emit_const(&fb->body, ty, expr->int_val))
            return 1;
        return 0;
    }
    case ND_FLOAT:
    {
        CCValueType ty = map_type_to_cc(expr->type);
        double value = expr->float_val;
        if (ty == CC_TYPE_F32)
            value = (double)(float)value;
        if (!ccb_emit_const_float(&fb->body, ty, value))
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
    case ND_MOD:
    {
        if (ccb_emit_expr_basic(fb, expr->lhs))
            return 1;
        if (ccb_emit_expr_basic(fb, expr->rhs))
            return 1;
        CCValueType ty = map_type_to_cc(expr->type ? expr->type : (expr->lhs ? expr->lhs->type : NULL));
        if (!ccb_value_type_is_integer(ty))
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "unsupported operand type for '%%'");
            return 1;
        }
        bool is_unsigned = !ccb_value_type_is_signed(ty);
        if (is_unsigned)
        {
            if (!string_list_appendf(&fb->body, "  binop mod %s unsigned", cc_type_name(ty)))
                return 1;
        }
        else
        {
            if (!string_list_appendf(&fb->body, "  binop mod %s", cc_type_name(ty)))
                return 1;
        }
        return 0;
    }
    case ND_NEG:
    {
        if (!expr->lhs)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "negation missing operand");
            return 1;
        }

        CCValueType operand_ty = ccb_type_for_expr(expr->lhs);
        if (operand_ty == CC_TYPE_INVALID || operand_ty == CC_TYPE_PTR)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "unsupported operand type for unary '-'");
            return 1;
        }

        if (!ccb_emit_const_zero(&fb->body, operand_ty))
            return 1;
        if (ccb_emit_expr_basic(fb, expr->lhs))
            return 1;
        if (!string_list_appendf(&fb->body, "  binop sub %s", cc_type_name(operand_ty)))
            return 1;
        return 0;
    }
    case ND_BITNOT:
    {
        if (!expr->lhs)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "bitwise '~' missing operand");
            return 1;
        }

        CCValueType operand_ty = ccb_type_for_expr(expr->lhs);
        if (operand_ty == CC_TYPE_INVALID || !ccb_value_type_is_integer(operand_ty))
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "unsupported operand type for bitwise '~'");
            return 1;
        }

        if (ccb_emit_expr_basic(fb, expr->lhs))
            return 1;
        if (!string_list_appendf(&fb->body, "  unop bitnot %s", cc_type_name(operand_ty)))
            return 1;
        return 0;
    }
    case ND_LNOT:
    {
        if (!expr->lhs)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "logical '!' missing operand");
            return 1;
        }
        if (ccb_emit_condition(fb, expr->lhs))
            return 1;
        if (!ccb_emit_const_zero(&fb->body, CC_TYPE_I32))
            return 1;
        if (!string_list_appendf(&fb->body, "  compare eq %s", cc_type_name(CC_TYPE_I32)))
            return 1;
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
    case ND_VA_START:
    {
        // va_start() -> produce an addr_param pointing at the first vararg
        if (!fb || !fb->fn)
        {
            diag_error_at(expr->src, expr->line, expr->col, "va_start used outside function context");
            return 1;
        }
        // index of first vararg == number of declared parameters
        size_t first_vararg_index = fb->param_count;
        if (!string_list_appendf(&fb->body, "  addr_param %zu", first_vararg_index))
            return 1;
        return 0;
    }
    case ND_VA_ARG:
    {
        // va_arg(list, T) -> load value at pointer in 'list' and advance pointer
        if (!expr->lhs)
        {
            diag_error_at(expr->src, expr->line, expr->col, "va_arg requires a va_list expression");
            return 1;
        }

        const Node *list_expr = expr->lhs;
        CcbLocal *list_local = NULL;          // direct va_list local/param
        CcbLocal *list_addr_local = NULL;     // address holding a va_list slot for indirection
        bool lhs_is_indirect = false;

        if (list_expr->kind == ND_VAR && list_expr->var_ref)
        {
            list_local = ccb_local_lookup(fb, list_expr->var_ref);
            if (!list_local)
            {
                diag_error_at(list_expr->src, list_expr->line, list_expr->col, "unknown va_list variable '%s'", list_expr->var_ref);
                return 1;
            }
        }
        else if (list_expr->kind == ND_DEREF)
        {
            // Support passing va_list by pointer (e.g., va_list* param)
            CCValueType elem_ty = CC_TYPE_PTR;
            const Type *elem_type = NULL;
            if (ccb_emit_deref_address(fb, list_expr, &elem_ty, &elem_type))
                return 1;

            const Type *va_slot_type = elem_type ? elem_type : type_va_list();
            if (va_slot_type && va_slot_type->kind != TY_VA_LIST)
            {
                diag_error_at(list_expr->src, list_expr->line, list_expr->col, "va_arg first argument must be a va_list or pointer to va_list");
                return 1;
            }

            Type *addr_ty = type_ptr((Type *)va_slot_type);
            list_addr_local = ccb_local_add(fb, NULL, addr_ty, false, false);
            if (!list_addr_local)
                return 1;
            if (!ccb_emit_store_local(fb, list_addr_local))
                return 1;
            lhs_is_indirect = true;
        }
        else
        {
            diag_error_at(list_expr->src, list_expr->line, list_expr->col, "va_arg first argument must be a va_list variable");
            return 1;
        }

        // Determine the requested result type from semantic analysis (falls back to parser hint)
        const Type *target_type = expr->type ? expr->type : expr->var_type;
        CCValueType val_ty = map_type_to_cc(target_type);
        if (val_ty == CC_TYPE_INVALID && type_is_address_only(target_type))
            val_ty = CC_TYPE_PTR;

        // Load current pointer to the next argument and align to value size
        if (lhs_is_indirect)
        {
            if (!ccb_emit_load_local(fb, list_addr_local))
                return 1;
            if (!ccb_emit_load_indirect(&fb->body, CC_TYPE_PTR))
                return 1;
        }
        else
        {
            if (!ccb_emit_load_local(fb, list_local))
                return 1;
        }

        if (ccb_emit_convert_between(fb, CC_TYPE_PTR, CC_TYPE_I64, expr))
            return 1;

        size_t size_bytes = ccb_value_type_size(val_ty);
        if (size_bytes == 0)
            size_bytes = sizeof(void *);
        if (size_bytes < 1)
            size_bytes = 1;

        // align pointer up to size_bytes
        if (!ccb_emit_const(&fb->body, CC_TYPE_I64, (int64_t)(size_bytes - 1)))
            return 1;
        if (!string_list_appendf(&fb->body, "  binop add %s", cc_type_name(CC_TYPE_I64)))
            return 1;
        if (!ccb_emit_const(&fb->body, CC_TYPE_I64, (int64_t)(~((int64_t)size_bytes - 1))))
            return 1;
        if (!string_list_appendf(&fb->body, "  binop and %s", cc_type_name(CC_TYPE_I64)))
            return 1;

        // convert aligned ptr back
        if (ccb_emit_convert_between(fb, CC_TYPE_I64, CC_TYPE_PTR, expr))
            return 1;

        // stash aligned pointer in a temp so we can both load and advance
        CcbLocal *aligned_ptr = ccb_local_add(fb, NULL, type_ptr(type_void()), false, false);
        if (!aligned_ptr)
            return 1;
        if (!ccb_emit_store_local(fb, aligned_ptr))
            return 1;

        // load value from aligned pointer
        if (!ccb_emit_load_local(fb, aligned_ptr))
            return 1;
        if (!ccb_emit_load_indirect(&fb->body, val_ty))
            return 1;

        // prepare to advance pointer using the stashed aligned ptr
        if (!ccb_emit_load_local(fb, aligned_ptr))
            return 1;
        if (ccb_emit_convert_between(fb, CC_TYPE_PTR, CC_TYPE_I64, expr))
            return 1;

        // add size constant
        size_t slot_size = ccb_value_type_size(CC_TYPE_PTR);
        if (slot_size == 0)
            slot_size = sizeof(void *);
        size_t size_advance = size_bytes;
        if (size_advance == 0)
            size_advance = slot_size;
        if (size_advance < slot_size)
            size_advance = slot_size;
        else if (size_advance % slot_size != 0)
            size_advance += slot_size - (size_advance % slot_size);
        if (!ccb_emit_const(&fb->body, CC_TYPE_I64, (int64_t)size_advance))
            return 1;
        if (!string_list_appendf(&fb->body, "  binop add %s", cc_type_name(CC_TYPE_I64)))
            return 1;

        // convert i64 -> ptr
        if (ccb_emit_convert_between(fb, CC_TYPE_I64, CC_TYPE_PTR, expr))
            return 1;

        if (lhs_is_indirect)
        {
            // store updated pointer back through the va_list*
            CcbLocal *tmp_ptr = ccb_local_add(fb, NULL, type_va_list(), false, false);
            if (!tmp_ptr)
                return 1;
            if (!ccb_emit_store_local(fb, tmp_ptr))
                return 1;
            if (!ccb_emit_load_local(fb, list_addr_local))
                return 1;
            if (!ccb_emit_load_local(fb, tmp_ptr))
                return 1;
            if (!ccb_emit_store_indirect(&fb->body, CC_TYPE_PTR))
                return 1;
        }
        else
        {
            // store updated pointer back into the va_list variable
            if (!ccb_emit_store_local(fb, list_local))
                return 1;
        }

        return 0;
    }
    case ND_VA_END:
    {
        // va_end(list) is a no-op in our IR
        if (!expr->lhs)
        {
            diag_error_at(expr->src, expr->line, expr->col, "va_end requires a va_list expression");
            return 1;
        }
        // Evaluate the operand for side-effects (if any)
        if (ccb_emit_expr_basic(fb, expr->lhs))
            return 1;
        return 0;
    }
    case ND_BITAND:
    case ND_BITOR:
    case ND_BITXOR:
    {
        if (ccb_emit_expr_basic(fb, expr->lhs))
            return 1;
        if (ccb_emit_expr_basic(fb, expr->rhs))
            return 1;
        CCValueType ty = map_type_to_cc(expr->type ? expr->type : (expr->lhs ? expr->lhs->type : NULL));
        if (ty == CC_TYPE_INVALID)
            ty = ccb_type_for_expr(expr);
        if (ty == CC_TYPE_INVALID || !ccb_value_type_is_integer(ty))
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "bitwise operator requires integer operands");
            return 1;
        }
        const char *op = (expr->kind == ND_BITAND) ? "and" : (expr->kind == ND_BITOR) ? "or"
                                                                                      : "xor";
        if (!string_list_appendf(&fb->body, "  binop %s %s", op, cc_type_name(ty)))
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
            if (target->var_is_global)
            {
                bool is_increment = (expr->kind == ND_PREINC);
                return ccb_emit_global_incdec(fb, expr, is_increment, true);
            }
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
            if (!string_list_appendf(&fb->body, "  binop %s i64", op))
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
            if (target->var_is_global)
            {
                bool is_increment = (expr->kind == ND_POSTINC);
                return ccb_emit_global_incdec(fb, expr, is_increment, false);
            }
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

        // Adding a temporary can reallocate the locals array; refresh the pointer
        local = ccb_local_lookup(fb, target->var_ref);
        if (!local)
        {
            diag_error_at(target->src, target->line, target->col,
                          "lost track of local '%s' after temp allocation",
                          target->var_ref);
            return 1;
        }

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

        CCValueType common_ty = lhs_ty;
        bool homogenize = false;
        if (lhs_is_int && rhs_is_int)
        {
            if (lhs_ty != rhs_ty)
            {
                size_t lhs_sz = ccb_value_type_size(lhs_ty);
                size_t rhs_sz = ccb_value_type_size(rhs_ty);
                if (lhs_sz > rhs_sz)
                    common_ty = lhs_ty;
                else if (rhs_sz > lhs_sz)
                    common_ty = rhs_ty;
                else
                {
                    if (ccb_value_type_is_signed(lhs_ty) == ccb_value_type_is_signed(rhs_ty))
                        common_ty = lhs_ty;
                    else
                        common_ty = ccb_value_type_is_signed(lhs_ty) ? rhs_ty : lhs_ty;
                }
                homogenize = true;
            }
        }
        else if (lhs_is_float && rhs_is_float)
        {
            if (lhs_ty != rhs_ty)
            {
                common_ty = (lhs_ty == CC_TYPE_F64 || rhs_ty == CC_TYPE_F64) ? CC_TYPE_F64 : CC_TYPE_F32;
                homogenize = true;
            }
        }

        if (ccb_emit_expr_basic(fb, expr->lhs))
            return 1;
        if (homogenize && lhs_ty != common_ty)
        {
            if (ccb_emit_convert_between(fb, lhs_ty, common_ty, expr->lhs))
                return 1;
            lhs_ty = common_ty;
        }

        if (ccb_emit_expr_basic(fb, expr->rhs))
            return 1;
        if (homogenize && rhs_ty != common_ty)
        {
            if (ccb_emit_convert_between(fb, rhs_ty, common_ty, expr->rhs))
                return 1;
            rhs_ty = common_ty;
        }

        lhs_is_ptr = lhs_ty == CC_TYPE_PTR;
        rhs_is_ptr = rhs_ty == CC_TYPE_PTR;
        lhs_is_int = ccb_value_type_is_integer(lhs_ty);
        rhs_is_int = ccb_value_type_is_integer(rhs_ty);
        lhs_is_float = ccb_value_type_is_float(lhs_ty);
        rhs_is_float = ccb_value_type_is_float(rhs_ty);

        CCValueType op_ty = lhs_ty;
        if (lhs_is_int && rhs_is_int)
            op_ty = common_ty;
        else if (lhs_is_float && rhs_is_float)
            op_ty = common_ty;

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

        CCValueType land_result_ty = map_type_to_cc(expr->type);
        if (land_result_ty == CC_TYPE_VOID || land_result_ty == CC_TYPE_INVALID)
            land_result_ty = CC_TYPE_I32;

        if (!string_list_appendf(&fb->body, "label %s", true_label))
            return 1;
        if (!ccb_emit_const(&fb->body, land_result_ty, 1))
            return 1;
        if (!string_list_appendf(&fb->body, "  jump %s", end_label))
            return 1;

        if (!string_list_appendf(&fb->body, "label %s", false_label))
            return 1;
        if (!ccb_emit_const_zero(&fb->body, land_result_ty))
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

        CCValueType lor_result_ty = map_type_to_cc(expr->type);
        if (lor_result_ty == CC_TYPE_VOID || lor_result_ty == CC_TYPE_INVALID)
            lor_result_ty = CC_TYPE_I32;

        if (!string_list_appendf(&fb->body, "label %s", true_label))
            return 1;
        if (!ccb_emit_const(&fb->body, lor_result_ty, 1))
            return 1;
        if (!string_list_appendf(&fb->body, "  jump %s", end_label))
            return 1;

        if (!string_list_appendf(&fb->body, "label %s", false_label))
            return 1;
        if (!ccb_emit_const_zero(&fb->body, lor_result_ty))
            return 1;

        if (!string_list_appendf(&fb->body, "label %s", end_label))
            return 1;
        return 0;
    }
    case ND_COND:
    {
        if (!expr->lhs || !expr->rhs || !expr->body)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "conditional expression missing branch");
            return 1;
        }

        if (ccb_emit_condition(fb, expr->lhs))
            return 1;

        char true_label[32];
        char false_label[32];
        char end_label[32];

        ccb_make_label(fb, true_label, sizeof(true_label), "cond_true");
        ccb_make_label(fb, false_label, sizeof(false_label), "cond_false");
        ccb_make_label(fb, end_label, sizeof(end_label), "cond_end");

        if (!string_list_appendf(&fb->body, "  branch %s %s", true_label, false_label))
            return 1;

        Type *result_type = expr->type ? expr->type : (expr->rhs ? expr->rhs->type : NULL);
        if (result_type && result_type->kind == TY_STRUCT)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "struct-valued conditional expressions are not supported by the bytecode backend yet");
            return 1;
        }

        CcbLocal *temp = ccb_local_add(fb, NULL, result_type, false, false);
        if (!temp)
            return 1;
        CCValueType result_cc = temp->value_type;

        if (!string_list_appendf(&fb->body, "label %s", true_label))
            return 1;
        if (ccb_emit_expr_basic(fb, expr->rhs))
            return 1;
        if (ccb_emit_convert_between(fb, ccb_type_for_expr(expr->rhs), result_cc, expr->rhs))
            return 1;
        if (!ccb_emit_store_local(fb, temp))
            return 1;
        if (!string_list_appendf(&fb->body, "  jump %s", end_label))
            return 1;

        if (!string_list_appendf(&fb->body, "label %s", false_label))
            return 1;
        if (ccb_emit_expr_basic(fb, expr->body))
            return 1;
        if (ccb_emit_convert_between(fb, ccb_type_for_expr(expr->body), result_cc, expr->body))
            return 1;
        if (!ccb_emit_store_local(fb, temp))
            return 1;

        if (!string_list_appendf(&fb->body, "label %s", end_label))
            return 1;
        if (!ccb_emit_load_local(fb, temp))
            return 1;
        return 0;
    }
    case ND_MATCH:
    {
        if (!expr->match_stmt.expr || expr->match_stmt.arm_count <= 0 || !expr->match_stmt.arms)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "match expression requires at least one arm");
            return 1;
        }

        if (ccb_emit_expr_basic(fb, expr->match_stmt.expr))
            return 1;

        CcbLocal *scrutinee = ccb_local_add(fb, NULL, expr->match_stmt.expr->type, false, false);
        if (!scrutinee)
            return 1;
        if (!ccb_emit_store_local(fb, scrutinee))
            return 1;

        CCValueType scrut_ty = scrutinee->value_type;
        if (scrut_ty == CC_TYPE_INVALID)
        {
            scrut_ty = ccb_type_for_expr(expr->match_stmt.expr);
            if (scrut_ty == CC_TYPE_INVALID)
                scrut_ty = CC_TYPE_I32;
        }

        if (expr->type && expr->type->kind == TY_STRUCT)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "match expression results of struct type are not supported yet");
            return 1;
        }

        CcbLocal *result_local = NULL;
        CCValueType result_ty = CC_TYPE_VOID;
        if (expr->type && expr->type->kind != TY_VOID)
        {
            result_local = ccb_local_add(fb, NULL, expr->type, false, false);
            if (!result_local)
                return 1;
            result_ty = result_local->value_type;
        }

        int arm_count = expr->match_stmt.arm_count;
        MatchArm *arms = expr->match_stmt.arms;
        char **arm_labels = (char **)calloc((size_t)arm_count, sizeof(char *));
        char **miss_labels = (char **)calloc((size_t)arm_count, sizeof(char *));
        if (!arm_labels || !miss_labels)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "out of memory while lowering match expression");
            free(arm_labels);
            free(miss_labels);
            return 1;
        }

        int wildcard_index = -1;
        for (int i = 0; i < arm_count; ++i)
        {
            char label_buf[32];
            ccb_make_label(fb, label_buf, sizeof(label_buf), "match_arm");
            arm_labels[i] = xstrdup(label_buf);
            miss_labels[i] = NULL;
            if (!arms[i].pattern)
                wildcard_index = i;
        }
        if (wildcard_index < 0)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "match expression missing fallback '_' arm");
            goto match_fail;
        }

        for (int i = 0; i < arm_count; ++i)
        {
            MatchArm *arm = &arms[i];
            if (!arm->pattern)
                continue;

            const char *miss_target = NULL;
            for (int j = i + 1; j < arm_count; ++j)
            {
                if (arms[j].pattern)
                {
                    char miss_buf[32];
                    ccb_make_label(fb, miss_buf, sizeof(miss_buf), "match_next");
                    miss_labels[i] = xstrdup(miss_buf);
                    miss_target = miss_labels[i];
                    break;
                }
            }
            if (!miss_target)
                miss_target = arm_labels[wildcard_index];

            if (!ccb_emit_load_local(fb, scrutinee))
                goto match_fail;
            if (ccb_emit_expr_basic(fb, arm->pattern))
                goto match_fail;
            CCValueType pat_ty = ccb_type_for_expr(arm->pattern);
            if (pat_ty != scrut_ty)
            {
                if (ccb_emit_convert_between(fb, pat_ty, scrut_ty, arm->pattern))
                    goto match_fail;
            }
            if (!string_list_appendf(&fb->body, "  compare eq %s", cc_type_name(scrut_ty)))
                goto match_fail;
            if (!string_list_appendf(&fb->body, "  branch %s %s", arm_labels[i], miss_target))
                goto match_fail;
            if (miss_labels[i])
            {
                if (!string_list_appendf(&fb->body, "label %s", miss_labels[i]))
                    goto match_fail;
            }
        }

        char end_label[32];
        ccb_make_label(fb, end_label, sizeof(end_label), "match_end");

        for (int i = 0; i < arm_count; ++i)
        {
            if (!arm_labels[i])
                continue;
            if (!string_list_appendf(&fb->body, "label %s", arm_labels[i]))
                goto match_fail;
            if (arms[i].body)
            {
                if (ccb_emit_expr_basic(fb, arms[i].body))
                    goto match_fail;
                if (result_local)
                {
                    CCValueType arm_ty = ccb_type_for_expr(arms[i].body);
                    if (arm_ty != result_ty)
                    {
                        if (ccb_emit_convert_between(fb, arm_ty, result_ty, arms[i].body))
                            goto match_fail;
                    }
                    if (!ccb_emit_store_local(fb, result_local))
                        goto match_fail;
                }
                else
                {
                    CCValueType arm_ty = ccb_type_for_expr(arms[i].body);
                    if (!ccb_emit_drop_for_type(&fb->body, arm_ty))
                        goto match_fail;
                }
            }
            if (!string_list_appendf(&fb->body, "  jump %s", end_label))
                goto match_fail;
        }

        if (!string_list_appendf(&fb->body, "label %s", end_label))
            goto match_fail;
        if (result_local)
        {
            if (!ccb_emit_load_local(fb, result_local))
                goto match_fail;
        }
        for (int i = 0; i < arm_count; ++i)
        {
            free(arm_labels[i]);
            free(miss_labels[i]);
        }
        free(arm_labels);
        free(miss_labels);
        return 0;

    match_fail:
        for (int i = 0; i < arm_count; ++i)
        {
            free(arm_labels ? arm_labels[i] : NULL);
            free(miss_labels ? miss_labels[i] : NULL);
        }
        free(arm_labels);
        free(miss_labels);
        return 1;
    }
    case ND_DEREF:
    {
        CCValueType elem_ty = CC_TYPE_I32;
        const Type *elem_type = NULL;
        if (ccb_emit_deref_address(fb, expr, &elem_ty, &elem_type))
            return 1;

        if (elem_type && elem_type->kind == TY_STRUCT)
        {
            // Caller will handle address-only struct accesses
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
    case ND_SEQ:
    {
        if (expr->lhs)
        {
            if (ccb_emit_expr_basic(fb, expr->lhs))
                return 1;
            CCValueType lhs_ty = ccb_type_for_expr(expr->lhs);
            if (!ccb_emit_drop_for_type(&fb->body, lhs_ty))
                return 1;
        }

        if (expr->rhs)
            return ccb_emit_expr_basic(fb, expr->rhs);
        return 0;
    }
    case ND_SIZEOF:
    case ND_ALIGNOF:
    case ND_OFFSETOF:
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
                if (operand->var_is_function || operand->var_is_global)
                {
                    if (!ccb_emit_addr_global(&fb->body, operand->var_ref))
                        return 1;
                    return 0;
                }
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
        case ND_DEREF:
        {
            if (ccb_emit_deref_address(fb, operand, NULL, NULL))
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
            if (expr->var_is_global)
            {
                int is_array_addr = expr->var_is_array ||
                                    (expr->var_type && expr->var_type->kind == TY_ARRAY);
                if (type_is_address_only(expr->type) || is_array_addr)
                {
                    if (!ccb_emit_addr_global(&fb->body, expr->var_ref))
                        return 1;
                }
                else
                {
                    if (!ccb_emit_load_global(&fb->body, expr->var_ref))
                        return 1;
                }
                return 0;
            }
            diag_error_at(expr->src, expr->line, expr->col,
                          "unknown local '%s'", expr->var_ref);
            return 1;
        }
        if (!ccb_emit_load_local(fb, local))
            return 1;
        return 0;
    }
    case ND_CALL:
        return ccb_emit_call_like(fb, expr, false);
    case ND_ADD_ASSIGN:
    case ND_SUB_ASSIGN:
    case ND_MUL_ASSIGN:
    case ND_DIV_ASSIGN:
    case ND_MOD_ASSIGN:
    case ND_BITAND_ASSIGN:
    case ND_BITOR_ASSIGN:
    case ND_BITXOR_ASSIGN:
    case ND_SHL_ASSIGN:
    case ND_SHR_ASSIGN:
        return ccb_emit_compound_assign(fb, expr);
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
                if (target->var_is_global)
                {
                    if (type_is_address_only(target->type))
                    {
                        Type *ptr_ty = type_ptr((Type *)target->type);
                        CcbLocal *dst_ptr = ccb_local_add(fb, NULL, ptr_ty, false, false);
                        if (!dst_ptr)
                            return 1;
                        if (!ccb_emit_addr_global(&fb->body, target->var_ref))
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

                        if (ccb_emit_struct_copy(fb, target->type, dst_ptr, src_ptr))
                            return 1;

                        CCValueType result_ty = ccb_type_for_expr(expr);
                        if (result_ty != CC_TYPE_VOID)
                        {
                            if (!ccb_emit_addr_global(&fb->body, target->var_ref))
                                return 1;
                        }
                        return 0;
                    }

                    if (ccb_emit_expr_basic(fb, expr->rhs))
                        return 1;

                    if (!ccb_emit_store_global(&fb->body, target->var_ref))
                        return 1;

                    CCValueType result_ty = ccb_type_for_expr(expr);
                    if (result_ty != CC_TYPE_VOID)
                    {
                        if (!ccb_emit_load_global(&fb->body, target->var_ref))
                            return 1;
                    }
                    return 0;
                }

                diag_error_at(target->src, target->line, target->col,
                              "unknown local '%s'", target->var_ref);
                return 1;
            }
            if (local->is_param)
            {
                if (ccb_promote_param_to_local(fb, target, &local))
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
        case ND_DEREF:
        {
            CCValueType elem_ty = CC_TYPE_I32;
            const Type *elem_type = NULL;
            if (ccb_emit_deref_address(fb, target, &elem_ty, &elem_type))
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
                              "failed to allocate temporary for dereference assignment");
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

static int ccb_emit_stmt_basic_impl(CcbFunctionBuilder *fb, const Node *stmt)
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
        return ccb_emit_block(fb, stmt, true);
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
    case ND_SEQ:
    {
        if (ccb_emit_expr_basic(fb, stmt))
            return 1;
        CCValueType seq_ty = ccb_type_for_expr(stmt);
        if (!ccb_emit_drop_for_type(&fb->body, seq_ty))
            return 1;
        return 0;
    }
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
        char post_label[32];
        bool has_post = stmt->body != NULL;

        ccb_make_label(fb, cond_label, sizeof(cond_label), "while_cond");
        ccb_make_label(fb, body_label, sizeof(body_label), "while_body");
        ccb_make_label(fb, end_label, sizeof(end_label), "while_end");
        if (has_post)
            ccb_make_label(fb, post_label, sizeof(post_label), "while_post");

        const char *continue_target = has_post ? post_label : cond_label;
        if (!ccb_loop_push(fb, end_label, continue_target, true))
        {
            diag_error_at(stmt->src, stmt->line, stmt->col,
                          "failed to allocate loop context");
            return 1;
        }

        int rc = 0;
        if (!string_list_appendf(&fb->body, "label %s", cond_label))
        {
            rc = 1;
            goto while_cleanup;
        }
        if (ccb_emit_condition(fb, stmt->lhs))
        {
            rc = 1;
            goto while_cleanup;
        }
        if (!string_list_appendf(&fb->body, "  branch %s %s", body_label, end_label))
        {
            rc = 1;
            goto while_cleanup;
        }

        if (!string_list_appendf(&fb->body, "label %s", body_label))
        {
            rc = 1;
            goto while_cleanup;
        }
        if (stmt->rhs)
        {
            if (ccb_emit_stmt_basic(fb, stmt->rhs))
            {
                rc = 1;
                goto while_cleanup;
            }
        }
        if (has_post)
        {
            if (!string_list_appendf(&fb->body, "label %s", post_label))
            {
                rc = 1;
                goto while_cleanup;
            }
            if (ccb_emit_stmt_basic(fb, stmt->body))
            {
                rc = 1;
                goto while_cleanup;
            }
        }
        if (!string_list_appendf(&fb->body, "  jump %s", cond_label))
        {
            rc = 1;
            goto while_cleanup;
        }

        if (!string_list_appendf(&fb->body, "label %s", end_label))
        {
            rc = 1;
            goto while_cleanup;
        }

    while_cleanup:
        ccb_loop_pop(fb);
        return rc;
    }
    case ND_SWITCH:
    {
        if (!stmt->switch_stmt.expr)
        {
            diag_error_at(stmt->src, stmt->line, stmt->col,
                          "switch statement missing selector");
            return 1;
        }
        if (stmt->switch_stmt.case_count <= 0 || !stmt->switch_stmt.cases)
        {
            diag_error_at(stmt->src, stmt->line, stmt->col,
                          "switch statement requires at least one case");
            return 1;
        }

        if (ccb_emit_expr_basic(fb, stmt->switch_stmt.expr))
            return 1;

        CcbLocal *selector = ccb_local_add(fb, NULL, stmt->switch_stmt.expr->type, false, false);
        if (!selector)
            return 1;
        if (!ccb_emit_store_local(fb, selector))
            return 1;

        CCValueType selector_ty = selector->value_type;
        if (selector_ty == CC_TYPE_INVALID)
        {
            selector_ty = ccb_type_for_expr(stmt->switch_stmt.expr);
            if (selector_ty == CC_TYPE_INVALID)
                selector_ty = CC_TYPE_I32;
        }

        int case_count = stmt->switch_stmt.case_count;
        SwitchCase *cases = stmt->switch_stmt.cases;
        char **case_labels = (char **)calloc((size_t)case_count, sizeof(char *));
        char **miss_labels = (char **)calloc((size_t)case_count, sizeof(char *));
        if (!case_labels || !miss_labels)
        {
            diag_error_at(stmt->src, stmt->line, stmt->col,
                          "out of memory while lowering switch statement");
            free(case_labels);
            free(miss_labels);
            return 1;
        }

        char end_label[32];
        ccb_make_label(fb, end_label, sizeof(end_label), "switch_end");

        const char *default_label = NULL;
        for (int i = 0; i < case_count; ++i)
        {
            char label_buf[32];
            ccb_make_label(fb, label_buf, sizeof(label_buf), "switch_case");
            case_labels[i] = xstrdup(label_buf);
            miss_labels[i] = NULL;
            if (cases[i].is_default)
                default_label = case_labels[i];
        }
        if (!default_label)
            default_label = end_label;

        for (int i = 0; i < case_count; ++i)
        {
            SwitchCase *entry = &cases[i];
            if (entry->is_default)
                continue;
            if (!entry->value)
            {
                diag_error_at(stmt->src, stmt->line, stmt->col,
                              "switch case missing value");
                goto switch_fail;
            }

            const char *miss_target = default_label;
            for (int j = i + 1; j < case_count; ++j)
            {
                if (!cases[j].is_default)
                {
                    char miss_buf[32];
                    ccb_make_label(fb, miss_buf, sizeof(miss_buf), "switch_next");
                    miss_labels[i] = xstrdup(miss_buf);
                    miss_target = miss_labels[i];
                    break;
                }
            }
            if (!miss_target)
                miss_target = end_label;

            if (!ccb_emit_load_local(fb, selector))
                goto switch_fail;
            if (ccb_emit_expr_basic(fb, entry->value))
                goto switch_fail;
            CCValueType case_ty = ccb_type_for_expr(entry->value);
            if (case_ty != selector_ty)
            {
                if (ccb_emit_convert_between(fb, case_ty, selector_ty, entry->value))
                    goto switch_fail;
            }
            if (!string_list_appendf(&fb->body, "  compare eq %s", cc_type_name(selector_ty)))
                goto switch_fail;
            if (!string_list_appendf(&fb->body, "  branch %s %s", case_labels[i], miss_target))
                goto switch_fail;
            if (miss_labels[i])
            {
                if (!string_list_appendf(&fb->body, "label %s", miss_labels[i]))
                    goto switch_fail;
            }
        }

        if (!ccb_loop_push(fb, end_label, NULL, false))
            goto switch_fail;

        for (int i = 0; i < case_count; ++i)
        {
            if (!case_labels[i])
                continue;
            if (!string_list_appendf(&fb->body, "label %s", case_labels[i]))
            {
                ccb_loop_pop(fb);
                goto switch_fail;
            }
            if (cases[i].body)
            {
                if (ccb_emit_block(fb, cases[i].body, true))
                {
                    ccb_loop_pop(fb);
                    goto switch_fail;
                }
            }
        }

        if (!string_list_appendf(&fb->body, "label %s", end_label))
        {
            ccb_loop_pop(fb);
            goto switch_fail;
        }

        ccb_loop_pop(fb);
        for (int i = 0; i < case_count; ++i)
        {
            free(case_labels[i]);
            free(miss_labels[i]);
        }
        free(case_labels);
        free(miss_labels);
        return 0;

    switch_fail:
        for (int i = 0; i < case_count; ++i)
        {
            free(case_labels ? case_labels[i] : NULL);
            free(miss_labels ? miss_labels[i] : NULL);
        }
        free(case_labels);
        free(miss_labels);
        return 1;
    }
    case ND_BREAK:
    {
        LoopContext *ctx = ccb_loop_top(fb);
        if (!ctx)
        {
            diag_error_at(stmt->src, stmt->line, stmt->col,
                          "'break' not within a loop or switch");
            return 1;
        }
        if (!string_list_appendf(&fb->body, "  jump %s", ctx->break_label))
            return 1;
        return 0;
    }
    case ND_CONTINUE:
    {
        LoopContext *ctx = ccb_continue_target(fb);
        if (!ctx || ctx->continue_label[0] == '\0')
        {
            diag_error_at(stmt->src, stmt->line, stmt->col,
                          "'continue' not within a loop");
            return 1;
        }
        if (!string_list_appendf(&fb->body, "  jump %s", ctx->continue_label))
            return 1;
        return 0;
    }
    case ND_VAR_DECL:
    {
        if (stmt->var_is_global)
            return 0;
        if (!stmt->var_name)
        {
            diag_error_at(stmt->src, stmt->line, stmt->col,
                          "local declaration missing name");
            return 1;
        }
        if (ccb_local_in_current_scope(fb, stmt->var_name))
        {
            diag_error_at(stmt->src, stmt->line, stmt->col,
                          "duplicate local '%s'", stmt->var_name);
            return 1;
        }

        Type *var_type = stmt->var_type;
        bool address_only = type_is_address_only(var_type);

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

            if (var_type && var_type->kind == TY_ARRAY && !var_type->array.is_unsized)
            {
                if (init)
                {
                    if (ccb_emit_array_initializer(fb, stmt, stmt->var_name, var_type, init))
                        return 1;
                }
                return 0;
            }
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
                                  "initializer lists not supported for scalars yet");
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

static bool ccb_format_global_initializer(const Node *expr, const Type *ty, char *buffer, size_t bufsz)
{
    if (!buffer || bufsz == 0)
        return false;

    if (!expr)
    {
        if (ty && ty->kind == TY_PTR)
            snprintf(buffer, bufsz, "null");
        else
            snprintf(buffer, bufsz, "0");
        return true;
    }

    switch (expr->kind)
    {
    case ND_INIT_LIST:
        if (expr->init.is_zero)
        {
            if (ty && ty->kind == TY_PTR)
                snprintf(buffer, bufsz, "null");
            else
                snprintf(buffer, bufsz, "0");
            return true;
        }
        return false;
    case ND_NULL:
        snprintf(buffer, bufsz, "null");
        return true;
    case ND_INT:
        if (ty && ty->kind == TY_PTR && expr->int_val == 0)
        {
            snprintf(buffer, bufsz, "null");
            return true;
        }
        snprintf(buffer, bufsz, "%lld", (long long)expr->int_val);
        return true;
    case ND_FLOAT:
    {
        double value = expr->float_val;
        if (ty && ty->kind == TY_F32)
            value = (double)(float)value;
        snprintf(buffer, bufsz, "%.17g", value);
        return true;
    }
    case ND_ADDR:
    {
        // Encode address-of in data initializers as a zero placeholder; backend/linker may patch later.
        snprintf(buffer, bufsz, "0");
        return true;
    }
    case ND_STRING:
        // String literals decay to pointers; encode as placeholder for now.
        snprintf(buffer, bufsz, "0");
        return true;
    case ND_NEG:
    {
        const Node *inner = expr->lhs;
        while (inner && inner->kind == ND_CAST)
            inner = inner->lhs;
        if (!inner)
            return false;
        if (inner->kind == ND_INT)
        {
            long long value = -(inner->int_val);
            snprintf(buffer, bufsz, "%lld", value);
            return true;
        }
        if (inner->kind == ND_FLOAT)
        {
            double value = -(inner->float_val);
            if (ty && ty->kind == TY_F32)
                value = (double)(float)value;
            snprintf(buffer, bufsz, "%.17g", value);
            return true;
        }
        return false;
    }
    case ND_CAST:
        return ccb_format_global_initializer(expr->lhs, ty, buffer, bufsz);
    default:
        return false;
    }
}

static int ccb_module_append_global(CcbModule *mod, const Node *decl)
{
    if (!mod || !decl || decl->kind != ND_VAR_DECL || !decl->var_is_global)
        return 1;

    int status = 1;
    char *section_literal = NULL;
    if (decl->section_name && decl->section_name[0])
    {
        section_literal = ccb_encode_bytes_literal((const uint8_t *)decl->section_name,
                                                   strlen(decl->section_name));
        if (!section_literal)
            return 1;
    }

    const char *name = ccb_effective_global_name(decl, NULL);
    if (!name || !*name)
    {
        diag_error_at(decl->src, decl->line, decl->col,
                      "global variable missing backend name");
        goto cleanup;
    }

    const char *const_attr = decl->var_is_const ? " const" : "";
    int hide = decl->var_is_static || !decl->is_exposed;
    const char *hidden_attr = hide ? " hidden" : "";

    if (decl->var_type && type_is_address_only(decl->var_type))
    {
        int size_bytes = (int)ccb_type_size_bytes(decl->var_type);
        if (size_bytes <= 0)
        {
            diag_error_at(decl->src, decl->line, decl->col,
                          "global '%s' has unknown size", decl->var_name);
            goto cleanup;
        }

        int align_bytes = 8;
        const Node *init = decl->rhs;
        if (decl->var_type->kind == TY_ARRAY && init && init->kind == ND_INIT_LIST && !init->init.is_zero)
        {
            uint8_t *bytes = NULL;
            size_t byte_len = 0;
            if (!ccb_flatten_array_initializer(decl->var_type, init, &bytes, &byte_len) || !bytes)
            {
                diag_error_at(init->src, init->line, init->col,
                              "failed to encode initializer for global '%s'", decl->var_name);
                free(bytes);
                goto cleanup;
            }
            if ((size_t)size_bytes != byte_len)
            {
                diag_error_at(init->src, init->line, init->col,
                              "initializer size mismatch for global '%s'", decl->var_name);
                free(bytes);
                goto cleanup;
            }
            char *literal = ccb_encode_bytes_literal(bytes, byte_len);
            free(bytes);
            if (!literal)
                goto cleanup;
            bool ok = section_literal
                          ? ccb_module_appendf(mod, ".global %s type=%s size=%zu align=%d data=%s section=%s%s%s",
                                               name, cc_type_name(CC_TYPE_U8), byte_len, align_bytes, literal,
                                               section_literal, const_attr, hidden_attr)
                          : ccb_module_appendf(mod, ".global %s type=%s size=%zu align=%d data=%s%s%s",
                                               name, cc_type_name(CC_TYPE_U8), byte_len, align_bytes, literal,
                                               const_attr, hidden_attr);
            free(literal);
            if (!ok)
                goto cleanup;
        }
        else if (decl->var_type->kind == TY_STRUCT && init && init->kind == ND_INIT_LIST && !init->init.is_zero)
        {
            uint8_t *bytes = NULL;
            size_t byte_len = 0;
            if (!ccb_flatten_struct_initializer(decl->var_type, init, &bytes, &byte_len) || !bytes)
            {
                diag_error_at(init->src, init->line, init->col,
                              "failed to encode initializer for global '%s'", decl->var_name);
                free(bytes);
                goto cleanup;
            }
            if ((size_t)size_bytes != byte_len)
            {
                diag_error_at(init->src, init->line, init->col,
                              "initializer size mismatch for global '%s'", decl->var_name);
                free(bytes);
                goto cleanup;
            }
            char *literal = ccb_encode_bytes_literal(bytes, byte_len);
            free(bytes);
            if (!literal)
                goto cleanup;
            bool ok = section_literal
                          ? ccb_module_appendf(mod, ".global %s type=%s size=%zu align=%d data=%s section=%s%s%s",
                                               name, cc_type_name(CC_TYPE_U8), byte_len, align_bytes, literal,
                                               section_literal, const_attr, hidden_attr)
                          : ccb_module_appendf(mod, ".global %s type=%s size=%zu align=%d data=%s%s%s",
                                               name, cc_type_name(CC_TYPE_U8), byte_len, align_bytes, literal,
                                               const_attr, hidden_attr);
            free(literal);
            if (!ok)
                goto cleanup;
        }
        else
        {
            bool ok = section_literal
                          ? ccb_module_appendf(mod, ".global %s type=%s size=%d align=%d section=%s%s%s",
                                               name, cc_type_name(CC_TYPE_U8), size_bytes, align_bytes,
                                               section_literal, const_attr, hidden_attr)
                          : ccb_module_appendf(mod, ".global %s type=%s size=%d align=%d%s%s",
                                               name, cc_type_name(CC_TYPE_U8), size_bytes, align_bytes,
                                               const_attr, hidden_attr);
            if (!ok)
                goto cleanup;
        }
        status = 0;
        goto cleanup;
    }

    CCValueType cc_ty = map_type_to_cc(decl->var_type);
    if (cc_ty == CC_TYPE_VOID)
    {
        diag_error_at(decl->src, decl->line, decl->col,
                      "global '%s' cannot have void storage", decl->var_name);
        goto cleanup;
    }

    char init_buf[128];
    if (!ccb_format_global_initializer(decl->rhs, decl->var_type, init_buf, sizeof(init_buf)))
    {
        diag_error_at(decl->src, decl->line, decl->col,
                      "unsupported initializer for global '%s'", decl->var_name);
        goto cleanup;
    }

    bool ok = section_literal
                  ? ccb_module_appendf(mod, ".global %s type=%s init=%s section=%s%s%s",
                                       name, cc_type_name(cc_ty), init_buf, section_literal, const_attr, hidden_attr)
                  : ccb_module_appendf(mod, ".global %s type=%s init=%s%s%s",
                                       name, cc_type_name(cc_ty), init_buf, const_attr, hidden_attr);
    if (!ok)
        goto cleanup;

    status = 0;

cleanup:
    free(section_literal);
    return status;
}

static int ccb_function_emit_chancecode(CcbModule *mod, const Node *fn, const CodegenOptions *opts)
{
    (void)opts;
    if (!mod || !fn || fn->kind != ND_FUNC || !fn->name)
        return 1;

    const char *backend_name = fn->metadata.backend_name ? fn->metadata.backend_name : fn->name;
    if (fn->export_name)
        backend_name = fn->name;
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

    if (fn->section_name && fn->metadata.func_line)
    {
        diag_error_at(fn->src, fn->line, fn->col,
                      "'Section' attribute is not supported when '.func' metadata overrides are present");
        return 1;
    }

    if (fn->metadata.func_line)
    {
        if (!ccb_module_append_line(mod, fn->metadata.func_line))
            return 1;
    }
    else
    {
        const char *varargs_suffix = fn->is_varargs ? " varargs" : "";
        const char *force_inline_suffix = fn->force_inline_literal ? " force-inline-literal" : "";
        const char *hidden_suffix = fn->is_exposed ? "" : " hidden";
        char *section_literal = NULL;
        if (fn->section_name && fn->section_name[0])
        {
            section_literal = ccb_encode_bytes_literal((const uint8_t *)fn->section_name,
                                                       strlen(fn->section_name));
            if (!section_literal)
                return 1;
        }
        bool ok = section_literal
                      ? ccb_module_appendf(mod, ".func %s ret=%s params=%zu locals=%zu section=%s%s%s%s",
                                           backend_name, ret_name, param_count, local_count, section_literal,
                                           varargs_suffix, force_inline_suffix, hidden_suffix)
                      : ccb_module_appendf(mod, ".func %s ret=%s params=%zu locals=%zu%s%s%s",
                                           backend_name, ret_name, param_count, local_count,
                                           varargs_suffix, force_inline_suffix, hidden_suffix);
        free(section_literal);
        if (!ok)
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

    if (fn->force_inline_literal)
    {
        if (!ccb_module_appendf(mod, ".force-inline-literal %s", backend_name))
            return 1;
    }

    if (fn->is_preserve)
    {
        if (!ccb_module_appendf(mod, ".preserve %s", backend_name))
            return 1;
    }

    if (fn->is_noreturn)
    {
        if (!ccb_module_appendf(mod, ".no-return %s", backend_name))
            return 1;
    }

    return 0;
}

static int ccb_function_emit_literal(CcbModule *mod, const Node *fn, const CodegenOptions *opts)
{
    (void)opts;
    if (!mod || !fn || fn->kind != ND_FUNC || !fn->name)
        return 1;

    if (!fn->literal.lines || fn->literal.count <= 0)
    {
        diag_error_at(fn->src, fn->line, fn->col,
                      "literal function '%s' does not contain any code", fn->name);
        return 1;
    }

    const char *backend_name = fn->metadata.backend_name ? fn->metadata.backend_name : fn->name;
    if (fn->export_name)
        backend_name = fn->name;
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
        const char *varargs_suffix = fn->is_varargs ? " varargs" : "";
        const char *force_inline_suffix = fn->force_inline_literal ? " force-inline-literal" : "";
        const char *hidden_suffix = fn->is_exposed ? "" : " hidden";
        if (!ccb_module_appendf(mod, ".func %s ret=%s params=%zu locals=%zu%s%s%s",
                                backend_name, ret_name, param_count, local_count, varargs_suffix, force_inline_suffix, hidden_suffix))
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

    if (!ccb_module_append_line(mod, ".literal"))
        return 1;

    for (int i = 0; i < fn->literal.count; ++i)
    {
        const char *line = fn->literal.lines[i] ? fn->literal.lines[i] : "";
        if (!ccb_module_append_line(mod, line))
            return 1;
    }

    if (!ccb_module_append_line(mod, ".endliteral"))
        return 1;

    if (!ccb_module_append_line(mod, ".endfunc"))
        return 1;

    if (fn->force_inline_literal)
    {
        if (!ccb_module_appendf(mod, ".force-inline-literal %s", backend_name))
            return 1;
    }

    if (fn->is_preserve)
    {
        if (!ccb_module_appendf(mod, ".preserve %s", backend_name))
            return 1;
    }

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
    if (fn->is_literal)
        return ccb_function_emit_literal(mod, fn, opts);

    CcbFunctionBuilder fb;
    bool enable_debug = mod && mod->emit_debug;
    ccb_function_builder_init(&fb, mod, fn, enable_debug);
    ccb_function_builder_register_params(&fb);

    int rc;
    if (fn->body && fn->body->kind == ND_BLOCK)
        rc = ccb_emit_block(&fb, fn->body, false);
    else
        rc = ccb_emit_stmt_basic(&fb, fn->body);
    if (!rc)
        ccb_function_optimize(&fb, opts);
    if (!rc)
    {
        const char *backend_name = fn->metadata.backend_name ? fn->metadata.backend_name : fn->name;
        if (fn->export_name)
            backend_name = fn->name;

        if (fn->metadata.func_line)
        {
            if (!ccb_module_append_line(mod, fn->metadata.func_line))
                rc = 1;
        }
        else
        {
            const char *varargs_suffix = fn->is_varargs ? " varargs" : "";
            const char *hidden_suffix = fn->is_exposed ? "" : " hidden";
            if (!ccb_module_appendf(mod, ".func %s ret=%s params=%zu locals=%zu%s%s", backend_name,
                                    cc_type_name(fb.ret_type), fb.param_count, fb.local_count, varargs_suffix, hidden_suffix))
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
            if (ccb_append_function_body(mod, &fb))
                rc = 1;
        }

        if (!rc)
        {
            if (!ccb_module_append_line(mod, ".endfunc"))
                rc = 1;
            else if (fn->is_preserve)
            {
                if (!ccb_module_appendf(mod, ".preserve %s", backend_name))
                    rc = 1;
            }
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
    case TY_ARRAY:
        return CC_TYPE_PTR;
    case TY_VOID:
        return CC_TYPE_VOID;
    case TY_CHAR:
        return CC_TYPE_I8;
    case TY_BOOL:
        return CC_TYPE_I1;
    case TY_STRUCT:
        return CC_TYPE_PTR;
    case TY_VA_LIST:
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

static bool ccb_eval_const_int64(const Node *expr, int64_t *out_value)
{
    if (!expr || !out_value)
        return false;
    switch (expr->kind)
    {
    case ND_INT:
        *out_value = expr->int_val;
        return true;
    case ND_NULL:
        *out_value = 0;
        return true;
    case ND_NEG:
        if (!ccb_eval_const_int64(expr->lhs, out_value))
            return false;
        *out_value = -*out_value;
        return true;
    case ND_CAST:
        return ccb_eval_const_int64(expr->lhs, out_value);
    case ND_ADDR:
        // Treat address-of globals/functions as link-time constants; encode as 0 placeholder.
        *out_value = 0;
        return true;
    case ND_STRING:
        // String literals in data initializers decay to pointers; encode as placeholder.
        *out_value = 0;
        return true;
    default:
        return false;
    }
}

static bool ccb_eval_const_float64(const Node *expr, double *out_value)
{
    if (!expr || !out_value)
        return false;
    switch (expr->kind)
    {
    case ND_FLOAT:
        *out_value = expr->float_val;
        return true;
    case ND_INT:
        *out_value = (double)expr->int_val;
        return true;
    case ND_NULL:
        *out_value = 0.0;
        return true;
    case ND_NEG:
        if (!ccb_eval_const_float64(expr->lhs, out_value))
            return false;
        *out_value = -*out_value;
        return true;
    case ND_CAST:
        return ccb_eval_const_float64(expr->lhs, out_value);
    default:
        return false;
    }
}

static bool ccb_store_scalar_bytes(const Type *elem_type, const Node *expr, uint8_t *dst, size_t dst_size)
{
    if (!elem_type || !expr || !dst || dst_size == 0)
        return false;

    switch (elem_type->kind)
    {
    case TY_I8:
    case TY_U8:
    case TY_I16:
    case TY_U16:
    case TY_I32:
    case TY_U32:
    case TY_I64:
    case TY_U64:
    case TY_CHAR:
    case TY_BOOL:
    case TY_PTR:
    {
        int64_t value = 0;
        if (!ccb_eval_const_int64(expr, &value))
            return false;
        uint64_t bits = (uint64_t)value;
        for (size_t i = 0; i < dst_size; ++i)
        {
            dst[i] = (uint8_t)(bits & 0xFFu);
            bits >>= 8;
        }
        return true;
    }
    case TY_F32:
    {
        double dv = 0.0;
        if (!ccb_eval_const_float64(expr, &dv))
            return false;
        float fv = (float)dv;
        uint32_t bits = 0;
        memcpy(&bits, &fv, sizeof(bits));
        for (size_t i = 0; i < dst_size && i < sizeof(bits); ++i)
        {
            dst[i] = (uint8_t)(bits & 0xFFu);
            bits >>= 8;
        }
        return true;
    }
    case TY_F64:
    {
        double dv = 0.0;
        if (!ccb_eval_const_float64(expr, &dv))
            return false;
        uint64_t bits = 0;
        memcpy(&bits, &dv, sizeof(bits));
        for (size_t i = 0; i < dst_size && i < sizeof(bits); ++i)
        {
            dst[i] = (uint8_t)(bits & 0xFFu);
            bits >>= 8;
        }
        return true;
    }
    default:
        return false;
    }
}

static bool ccb_store_constant_value(const Type *type, const Node *expr, uint8_t *dst, size_t dst_size)
{
    if (!type)
        return false;
    size_t type_size = ccb_type_size_bytes(type);
    if (type_size == 0)
        return true;
    if (!dst || dst_size < type_size)
        return false;

    memset(dst, 0, type_size);

    if (!expr)
        return true;
    if (expr->kind == ND_INIT_LIST && (expr->init.is_zero || expr->init.count == 0))
        return true;

    switch (type->kind)
    {
    case TY_STRUCT:
        return ccb_store_struct_bytes(type, expr, dst, type_size);
    case TY_ARRAY:
        return ccb_store_array_bytes(type, expr, dst, type_size);
    default:
        return ccb_store_scalar_bytes(type, expr, dst, type_size);
    }
}

static bool ccb_store_struct_bytes(const Type *struct_type, const Node *expr, uint8_t *dst, size_t dst_size)
{
    if (!struct_type || struct_type->kind != TY_STRUCT || !dst)
        return false;

    size_t struct_size = ccb_type_size_bytes(struct_type);
    if (struct_size == 0)
        return true;
    if (dst_size < struct_size)
        return false;

    if (!expr || expr->kind != ND_INIT_LIST || expr->init.is_zero || expr->init.count == 0)
        return true;

    int field_count = struct_type->strct.field_count;
    const int *offsets = struct_type->strct.field_offsets;
    Type *const *field_types = struct_type->strct.field_types;
    if (!field_types || !offsets)
        return false;

    for (int i = 0; i < expr->init.count; ++i)
    {
        const Node *value = (expr->init.elems && i < expr->init.count) ? expr->init.elems[i] : NULL;
        if (!value)
            return false;
        int field_index = expr->init.field_indices ? expr->init.field_indices[i] : i;
        if (field_index < 0 || field_index >= field_count)
            return false;
        const Type *field_type = field_types[field_index];
        if (!field_type)
            return false;
        size_t field_size = ccb_type_size_bytes(field_type);
        size_t field_offset = (size_t)offsets[field_index];
        if (field_offset + field_size > struct_size)
            return false;
        if (!ccb_store_constant_value(field_type, value, dst + field_offset, field_size))
            return false;
    }

    return true;
}

static bool ccb_store_array_bytes(const Type *array_type, const Node *expr, uint8_t *dst, size_t dst_size)
{
    if (!array_type || array_type->kind != TY_ARRAY || array_type->array.is_unsized || !dst)
        return false;

    size_t total_size = ccb_type_size_bytes(array_type);
    if (total_size == 0)
        return true;
    if (dst_size < total_size)
        return false;

    if (!expr || expr->kind != ND_INIT_LIST || expr->init.is_zero || expr->init.count == 0)
        return true;

    const Type *elem_type = array_type->array.elem;
    size_t elem_size = ccb_type_size_bytes(elem_type);

    int limit = expr->init.count;
    if (array_type->array.length >= 0 && limit > array_type->array.length)
        limit = array_type->array.length;

    for (int i = 0; i < limit; ++i)
    {
        const Node *value = (expr->init.elems && i < expr->init.count) ? expr->init.elems[i] : NULL;
        if (!value)
            return false;
        if (!ccb_store_constant_value(elem_type, value, dst + (size_t)i * elem_size, elem_size))
            return false;
    }

    return true;
}

static bool ccb_flatten_array_initializer(const Type *array_type, const Node *init, uint8_t **out_bytes, size_t *out_size)
{
    if (!array_type || array_type->kind != TY_ARRAY || array_type->array.is_unsized || !out_bytes || !out_size)
        return false;

    if (array_type->array.length <= 0)
        return false;

    size_t elem_size = ccb_type_size_bytes(array_type->array.elem);
    if (elem_size == 0)
        return false;
    size_t total_size = elem_size * (size_t)array_type->array.length;

    uint8_t *buffer = (uint8_t *)calloc(total_size, 1);
    if (!buffer)
        return false;

    if (init && init->kind == ND_INIT_LIST && !init->init.is_zero)
    {
        int limit = init->init.count;
        if (array_type->array.length >= 0 && limit > array_type->array.length)
            limit = array_type->array.length;
        for (int i = 0; i < limit; ++i)
        {
            const Node *elem = (init->init.elems && i < init->init.count) ? init->init.elems[i] : NULL;
            if (!elem || !ccb_store_scalar_bytes(array_type->array.elem, elem, buffer + (size_t)i * elem_size, elem_size))
            {
                free(buffer);
                return false;
            }
        }
    }

    *out_bytes = buffer;
    *out_size = total_size;
    return true;
}

static bool ccb_flatten_struct_initializer(const Type *struct_type, const Node *init, uint8_t **out_bytes, size_t *out_size)
{
    if (!struct_type || struct_type->kind != TY_STRUCT || !out_bytes || !out_size)
        return false;

    size_t total_size = ccb_type_size_bytes(struct_type);
    if (total_size == 0)
        return false;

    uint8_t *buffer = (uint8_t *)calloc(total_size, 1);
    if (!buffer)
        return false;

    if (!ccb_store_constant_value(struct_type, init, buffer, total_size))
    {
        free(buffer);
        return false;
    }

    *out_bytes = buffer;
    *out_size = total_size;
    return true;
}

static char *ccb_encode_bytes_literal(const uint8_t *data, size_t len)
{
    if (!data)
        len = 0;
    size_t capacity = len * 4 + 3;
    char *out = (char *)malloc(capacity);
    if (!out)
        return NULL;

    char *cursor = out;
    *cursor++ = '"';
    for (size_t i = 0; i < len; ++i)
    {
        uint8_t byte = data[i];
        snprintf(cursor, 5, "\\x%02X", byte);
        cursor += 4;
    }
    *cursor++ = '"';
    *cursor = '\0';
    return out;
}

static void ccb_write_quoted(FILE *out, const char *text)
{
    if (!out)
        return;
    fputc('"', out);
    if (text)
    {
        for (const char *p = text; *p; ++p)
        {
            if (*p == '"' || *p == '\\')
                fputc('\\', out);
            fputc(*p, out);
        }
    }
    fputc('"', out);
}

static int write_module_to_file(const char *path, const CcbModule *mod)
{
    FILE *out = fopen(path, "wb");
    if (!out)
    {
        fprintf(stderr, "codegen: failed to open '%s': %s\n", path, strerror(errno));
        return 1;
    }

    fprintf(out, "ccbytecode 3\n\n");
    if (mod && mod->emit_debug && mod->debug_files.count > 0)
    {
        for (size_t i = 0; i < mod->debug_files.count; ++i)
        {
            const char *path_str = mod->debug_files.items[i];
            fprintf(out, ".file %zu ", i + 1);
            ccb_write_quoted(out, path_str ? path_str : "");
            fputc('\n', out);
        }
        fputc('\n', out);
    }
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
    if (opts)
        mod.emit_debug = opts->debug_symbols;

    int rc = 0;
    if (!rc)
        rc = ccb_module_emit_externs(&mod, opts);
    if (!rc)
        rc = ccb_module_emit_imported_globals(&mod, opts);

    if (!rc)
    {
        if (unit->kind == ND_UNIT)
        {
            int inline_count = 0;
            for (int i = 0; i < unit->stmt_count; ++i)
            {
                const Node *decl = unit->stmts[i];
                if (decl && decl->kind == ND_FUNC && decl->inline_candidate)
                    inline_count++;
            }

            const Node **inline_funcs = NULL;
            char *inline_emitted = NULL;
            if (!rc && inline_count > 0)
            {
                inline_funcs = (const Node **)calloc((size_t)inline_count, sizeof(*inline_funcs));
                inline_emitted = (char *)calloc((size_t)inline_count, sizeof(*inline_emitted));
                if (!inline_funcs || !inline_emitted)
                {
                    free(inline_funcs);
                    free(inline_emitted);
                    inline_funcs = NULL;
                    inline_emitted = NULL;
                    inline_count = 0;
                    rc = 1;
                }
                else
                {
                    int idx = 0;
                    for (int i = 0; i < unit->stmt_count; ++i)
                    {
                        const Node *decl = unit->stmts[i];
                        if (!decl || decl->kind != ND_FUNC || !decl->inline_candidate)
                            continue;
                        inline_funcs[idx++] = decl;
                    }
                }
            }

            for (int i = 0; !rc && i < unit->stmt_count; ++i)
            {
                const Node *decl = unit->stmts[i];
                if (!decl)
                    continue;
                if (decl->kind == ND_VAR_DECL && decl->var_is_global)
                {
                    if (ccb_module_append_global(&mod, decl))
                        rc = 1;
                }
            }
            for (int i = 0; !rc && i < unit->stmt_count; ++i)
            {
                const Node *decl = unit->stmts[i];
                if (!decl || decl->kind != ND_FUNC)
                    continue;
                if (decl->inline_candidate)
                    continue;
                if (ccb_function_emit_basic(&mod, decl, opts))
                    rc = 1;
            }
            if (!rc && inline_count > 0)
            {
                int emitted_in_pass;
                do
                {
                    emitted_in_pass = 0;
                    for (int i = 0; !rc && i < inline_count; ++i)
                    {
                        if (inline_emitted[i])
                            continue;
                        const Node *decl = inline_funcs[i];
                        if (!decl || !decl->inline_needs_body)
                            continue;
                        if (ccb_function_emit_basic(&mod, decl, opts))
                        {
                            rc = 1;
                            break;
                        }
                        inline_emitted[i] = 1;
                        emitted_in_pass = 1;
                    }
                } while (!rc && emitted_in_pass);
            }

            free(inline_emitted);
            free(inline_funcs);
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
