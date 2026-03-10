#include "ast.h"
#include "ccsim.h"
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
    StringList defined_funcs;
    StringList interned_string_keys;
    StringList interned_string_symbols;
    int next_interned_string_id;
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
    bool needs_gc_prep;
    LoopContext *loop_stack;
    size_t loop_depth;
    size_t loop_capacity;
    const char *active_try_error_label;
} CcbFunctionBuilder;

typedef enum
{
    CCB_ENTRY_SHIM_NONE = 0,
    CCB_ENTRY_SHIM_ZERO_ARG,
    CCB_ENTRY_SHIM_STRING_ARRAY
} CcbEntrypointShimKind;

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
static const char *ccb_module_intern_hidden_byte_string(CcbModule *mod, const char *base, const uint8_t *data, size_t len);
static bool ccb_is_string_ptr_type(const Type *ty);
static char *ccb_make_string_symbol(const char *base, int index);
static bool ccb_emit_string_ptr_array_global(CcbModule *mod, const char *name, const Type *array_type, const Node *init,
                                             const char *section_literal, const char *const_attr, const char *hidden_attr);
static CcbLocal *ccb_local_add(CcbFunctionBuilder *fb, const char *name, Type *type, bool address_only, bool is_param);
static CcbLocal *ccb_local_add_u64(CcbFunctionBuilder *fb, const char *name, bool is_param);
static int ccb_emit_expr_basic_impl(CcbFunctionBuilder *fb, const Node *expr);
static int ccb_emit_expr_basic(CcbFunctionBuilder *fb, const Node *expr);
static int ccb_emit_stmt_basic_impl(CcbFunctionBuilder *fb, const Node *stmt);
static int ccb_emit_stmt_basic(CcbFunctionBuilder *fb, const Node *stmt);
static int ccb_emit_active_try_pending_branch(CcbFunctionBuilder *fb);
static void ccb_make_label(CcbFunctionBuilder *fb, char *buffer, size_t bufsz, const char *prefix);
static bool ccb_emit_load_indirect(StringList *body, CCValueType ty);
static bool ccb_emit_store_indirect(StringList *body, CCValueType ty);
static bool ccb_emit_const_zero(StringList *body, CCValueType ty);
static bool ccb_emit_convert(StringList *body, CCConvertKind kind, CCValueType from_ty, CCValueType to_ty);
static bool ccb_module_append_line(CcbModule *mod, const char *line);
static bool ccb_module_appendf(CcbModule *mod, const char *fmt, ...);
static int ccb_emit_convert_between(CcbFunctionBuilder *fb, CCValueType from_ty, CCValueType to_ty, const Node *node);
static int ccb_emit_index_address(CcbFunctionBuilder *fb, const Node *expr, CCValueType *out_elem_ty, const Type **out_elem_type);
static int ccb_emit_member_address(CcbFunctionBuilder *fb, const Node *expr, CCValueType *out_field_ty, const Type **out_field_type);
static size_t ccb_pointer_elem_size(const Type *ptr_type);
static char *ccb_escape_string_literal(const char *data, int len);
static int ccb_emit_pointer_add_like(CcbFunctionBuilder *fb, const Node *expr, bool is_add);
static int ccb_emit_pointer_difference(CcbFunctionBuilder *fb, const Node *expr);
static int ccb_emit_string_equality_compare(CcbFunctionBuilder *fb, const Node *expr);
static int ccb_emit_string_length_expr(CcbFunctionBuilder *fb, const Node *expr);
static int ccb_emit_array_length_expr(CcbFunctionBuilder *fb, const Node *expr);
static int ccb_emit_ref_arg_value(CcbFunctionBuilder *fb, const Node *expr);
static int ccb_emit_struct_zero(CcbFunctionBuilder *fb, const Node *var_decl, const char *var_name, const Type *struct_type);
static int ccb_emit_struct_initializer(CcbFunctionBuilder *fb, const Node *var_decl, const char *var_name, const Type *struct_type, const Node *init);
static int ccb_emit_struct_default_fields(CcbFunctionBuilder *fb, const Node *var_decl, const char *var_name, const Type *struct_type, bool pointer_base);
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
static bool ccb_node_uses_tracked_alloc(const Node *node);
static bool ccb_is_unsized_array_type(const Type *ty);
static const Type *ccb_node_array_source_type(const Node *node);
static bool ccb_function_param_has_managed_length(const Node *fn, int index);
static int ccb_function_hidden_managed_param_count(const Node *fn);
static bool ccb_symbol_param_has_managed_length(const Symbol *sym, int index);
static char *ccb_make_managed_length_name(const char *name);
static int ccb_emit_managed_array_length_value(CcbFunctionBuilder *fb, const Node *expr);
static int ccb_emit_store_managed_array_length(CcbFunctionBuilder *fb, const Node *site, const char *name, const Node *value_expr);
static CCValueType ccb_type_for_expr(const Node *expr);
static size_t ccb_type_size_bytes(const Type *ty);
static bool ccb_emit_const_u64(StringList *body, CCValueType ty, uint64_t value);
static bool ccb_make_default_literal_expr(const Type *field_type, const char *spec, Node *out_expr);
static bool ccb_struct_has_field_defaults(const Type *struct_type);
static bool ccb_store_struct_default_bytes(const Type *struct_type, uint8_t *dst, size_t dst_size);
static bool ccb_module_append_extern(CcbModule *mod, const Symbol *sym);
static int ccb_module_emit_externs(CcbModule *mod, const CodegenOptions *opts);
static CcbEntrypointShimKind ccb_entrypoint_shim_kind(const Node *fn, const CodegenOptions *opts);
static Node *ccb_prepare_hosted_entrypoint(CcbModule *mod, const Node *unit, const CodegenOptions *opts,
                                           CcbEntrypointShimKind *out_kind, const char **out_public_name,
                                           const char **out_hidden_name);
static int ccb_emit_hosted_entrypoint_shim(CcbModule *mod, const Node *fn, CcbEntrypointShimKind kind,
                                           const char *public_name, const char *hidden_name);
static void ccb_module_optimize(CcbModule *mod, const CodegenOptions *opts);
static void ccb_function_optimize(CcbFunctionBuilder *fb, const CodegenOptions *opts);
static bool ccb_instruction_is_pure(const char *line);
static void ccb_opt_prune_dropped_values(CcbFunctionBuilder *fb);
static void ccb_opt_fold_const_binops(CcbFunctionBuilder *fb);
static void ccb_opt_fold_const_unops(CcbFunctionBuilder *fb);
static void ccb_opt_fold_const_compares(CcbFunctionBuilder *fb);
static void ccb_opt_fold_const_test_null(CcbFunctionBuilder *fb);
static void ccb_opt_fold_const_converts(CcbFunctionBuilder *fb);
static void ccb_opt_strength_reduce_binops(CcbFunctionBuilder *fb);
static void ccb_opt_simplify_noop_arith_and_bitcasts(CcbFunctionBuilder *fb);
static void ccb_opt_fold_const_or_store_chains(CcbFunctionBuilder *fb);
static void ccb_opt_fold_dup_rmw_or_chains(CcbFunctionBuilder *fb);
static void ccb_opt_pack_byte_store_runs(CcbFunctionBuilder *fb);
static void ccb_opt_remove_overwritten_indirect_stores(CcbFunctionBuilder *fb);
static void ccb_opt_simplify_store_load_store(CcbFunctionBuilder *fb);
static void ccb_opt_promote_local_values(CcbFunctionBuilder *fb);
static void ccb_opt_propagate_local_values(CcbFunctionBuilder *fb);
static void ccb_opt_remove_dead_local_stores(CcbFunctionBuilder *fb);
static void ccb_opt_remove_unused_local_slots(CcbFunctionBuilder *fb);
static void ccb_opt_remove_dead_local_copies(CcbFunctionBuilder *fb);
static void ccb_opt_simplify_addr_local_temp(CcbFunctionBuilder *fb);
static void ccb_opt_simplify_bool_normalization(CcbFunctionBuilder *fb);
static bool ccb_instruction_is_control_barrier(const char *line);
static void ccb_opt_simplify_const_branches(CcbFunctionBuilder *fb);
static void ccb_opt_remove_unreachable_fallthrough(CcbFunctionBuilder *fb);
static void ccb_opt_remove_redundant_jumps(CcbFunctionBuilder *fb);
static void ccb_opt_remove_unused_labels(CcbFunctionBuilder *fb);
static void ccb_opt_inline_const_str_locals(CcbFunctionBuilder *fb);
static void ccb_opt_remove_nops(CcbFunctionBuilder *fb);
static const char *ccb_trim_leading_ws(const char *line);
static const char *ccb_binop_symbol(const char *op);
static int ccb_emit_inline_call(CcbFunctionBuilder *fb, const Node *call_expr, const Node *target_fn);

static void ccb_module_init(CcbModule *mod)
{
    if (!mod)
        return;
    string_list_init(&mod->lines);
    string_list_init(&mod->debug_files);
    string_list_init(&mod->defined_funcs);
    string_list_init(&mod->interned_string_keys);
    string_list_init(&mod->interned_string_symbols);
    mod->next_interned_string_id = 0;
    mod->emit_debug = false;
}

// forward declarations
static bool ccb_emit_addr_global(StringList *body, const char *name);

static void ccb_module_free(CcbModule *mod)
{
    if (!mod)
        return;
    string_list_free(&mod->lines);
    string_list_free(&mod->debug_files);
    string_list_free(&mod->defined_funcs);
    string_list_free(&mod->interned_string_keys);
    string_list_free(&mod->interned_string_symbols);
    mod->next_interned_string_id = 0;
    mod->emit_debug = false;
}

static const char *ccb_effective_function_name(const Node *fn)
{
    if (!fn || !fn->name)
        return NULL;
    const char *backend = fn->metadata.backend_name ? fn->metadata.backend_name : fn->name;
    if (fn->export_name && !fn->raw_export_name)
        backend = fn->name;
    return backend;
}

static void ccb_module_collect_defined_functions(CcbModule *mod, const Node *unit)
{
    if (!mod || !unit || unit->kind != ND_UNIT)
        return;
    for (int i = 0; i < unit->stmt_count; ++i)
    {
        const Node *decl = unit->stmts[i];
        if (!decl || decl->kind != ND_FUNC)
            continue;
        const char *name = ccb_effective_function_name(decl);
        if (!name || !*name)
            continue;
        if (!string_list_contains(&mod->defined_funcs, name))
            string_list_append(&mod->defined_funcs, name);
    }
}

static bool ccb_type_is_string_array_param(const Type *ty)
{
    if (!ty || ty->kind != TY_ARRAY || !ty->array.is_unsized || !ty->array.elem)
        return false;
    return ccb_is_string_ptr_type(ty->array.elem);
}

static bool ccb_type_is_c_argv_param(const Type *ty)
{
    return ty && ty->kind == TY_PTR && ccb_is_string_ptr_type(ty->pointee);
}

static char *ccb_make_entrypoint_hidden_name(const char *public_name)
{
    const char *base = (public_name && *public_name) ? public_name : "main";
    size_t need = strlen(base) + strlen("__cert__entry_") + 1;
    char *buffer = (char *)xmalloc(need);
    snprintf(buffer, need, "__cert__entry_%s", base);
    return buffer;
}

static CcbEntrypointShimKind ccb_entrypoint_shim_kind(const Node *fn, const CodegenOptions *opts)
{
    if (!fn || fn->kind != ND_FUNC || !fn->is_entrypoint)
        return CCB_ENTRY_SHIM_NONE;
    if (opts && opts->freestanding)
        return CCB_ENTRY_SHIM_NONE;

    if (fn->param_count == 0)
        return CCB_ENTRY_SHIM_ZERO_ARG;

    if (fn->param_count == 1 && ccb_type_is_string_array_param(fn->param_types ? fn->param_types[0] : NULL))
        return CCB_ENTRY_SHIM_STRING_ARRAY;

    if (fn->param_count == 2 && fn->param_types)
    {
        Type *arg0 = fn->param_types[0];
        Type *arg1 = fn->param_types[1];
        if (arg0 && arg0->kind == TY_I32 && ccb_type_is_c_argv_param(arg1))
            return CCB_ENTRY_SHIM_NONE;
    }

    return CCB_ENTRY_SHIM_NONE;
}

static bool ccb_append_entrypoint_result_as_i32(StringList *body, CCValueType ret_ty)
{
    if (!body)
        return false;
    if (ret_ty == CC_TYPE_VOID)
        return ccb_emit_const_zero(body, CC_TYPE_I32);
    if (ret_ty == CC_TYPE_I32 || ret_ty == CC_TYPE_U32)
        return true;
    if (!ccb_value_type_is_integer(ret_ty))
        return false;

    size_t from_size = ccb_value_type_size(ret_ty);
    if (from_size > ccb_value_type_size(CC_TYPE_I32))
        return ccb_emit_convert(body, CC_CONVERT_TRUNC, ret_ty, CC_TYPE_I32);
    if (from_size < ccb_value_type_size(CC_TYPE_I32))
    {
        CCConvertKind kind = ccb_value_type_is_signed(ret_ty) ? CC_CONVERT_SEXT : CC_CONVERT_ZEXT;
        return ccb_emit_convert(body, kind, ret_ty, CC_TYPE_I32);
    }
    return true;
}

static Node *ccb_prepare_hosted_entrypoint(CcbModule *mod, const Node *unit, const CodegenOptions *opts,
                                           CcbEntrypointShimKind *out_kind, const char **out_public_name,
                                           const char **out_hidden_name)
{
    if (out_kind)
        *out_kind = CCB_ENTRY_SHIM_NONE;
    if (out_public_name)
        *out_public_name = NULL;
    if (out_hidden_name)
        *out_hidden_name = NULL;
    if (!mod || !unit || unit->kind != ND_UNIT)
        return NULL;

    for (int i = 0; i < unit->stmt_count; ++i)
    {
        Node *fn = (Node *)unit->stmts[i];
        CcbEntrypointShimKind kind = ccb_entrypoint_shim_kind(fn, opts);
        if (!fn || kind == CCB_ENTRY_SHIM_NONE)
            continue;

        const char *public_name = ccb_effective_function_name(fn);
        char *hidden_name = ccb_make_entrypoint_hidden_name(public_name);
        fn->metadata.backend_name = hidden_name;
        fn->export_name = 0;
        fn->is_exposed = 0;

        if (!string_list_contains(&mod->defined_funcs, hidden_name))
            string_list_append(&mod->defined_funcs, hidden_name);
        if (!string_list_contains(&mod->defined_funcs, public_name))
            string_list_append(&mod->defined_funcs, public_name);

        if (out_kind)
            *out_kind = kind;
        if (out_public_name)
            *out_public_name = public_name;
        if (out_hidden_name)
            *out_hidden_name = hidden_name;
        return fn;
    }

    return NULL;
}

static int ccb_emit_hosted_entrypoint_shim(CcbModule *mod, const Node *fn, CcbEntrypointShimKind kind,
                                           const char *public_name, const char *hidden_name)
{
    if (!mod || !fn || kind == CCB_ENTRY_SHIM_NONE || !public_name || !hidden_name)
        return 0;

    CCValueType ret_ty = map_type_to_cc(fn->ret_type);
    if (ret_ty != CC_TYPE_VOID && ret_ty != CC_TYPE_I32 && ret_ty != CC_TYPE_U32 && !ccb_value_type_is_integer(ret_ty))
    {
        diag_error_at(fn->src, fn->line, fn->col,
                      "hosted entrypoint shim only supports integer or void return types");
        return 1;
    }

    if (!ccb_module_appendf(mod, ".func %s ret=i32 params=2 locals=0", public_name))
        return 1;
    if (!ccb_module_append_line(mod, ".params i32 ptr"))
        return 1;

    if (kind == CCB_ENTRY_SHIM_STRING_ARRAY)
    {
        if (!ccb_module_append_line(mod, "  load_param 1"))
            return 1;
        if (!ccb_module_append_line(mod, "  load_param 0"))
            return 1;
        if (!ccb_module_append_line(mod, "  convert zext i32 u64"))
            return 1;
        if (!ccb_module_appendf(mod, "  call %s %s (ptr,u64)", hidden_name, cc_type_name(ret_ty)))
            return 1;
    }
    else
    {
        if (!ccb_module_appendf(mod, "  call %s %s ()", hidden_name, cc_type_name(ret_ty)))
            return 1;
    }

    if (!ccb_append_entrypoint_result_as_i32(&mod->lines, ret_ty))
    {
        diag_error_at(fn->src, fn->line, fn->col,
                      "failed to normalize hosted entrypoint return type");
        return 1;
    }
    if (!ccb_module_append_line(mod, "  ret"))
        return 1;
    if (!ccb_module_append_line(mod, ".endfunc"))
        return 1;
    if (!ccb_module_appendf(mod, ".preserve %s", public_name))
        return 1;
    return 0;
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
            if (ccb_symbol_param_has_managed_length(sym, i))
                params_len += 1 + strlen(cc_type_name(CC_TYPE_U64));
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
            if (ccb_symbol_param_has_managed_length(sym, i))
            {
                params[pos++] = ',';
                ty_name = cc_type_name(CC_TYPE_U64);
                len = strlen(ty_name);
                memcpy(params + pos, ty_name, len);
                pos += len;
            }
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
        if (!sym || !sym->is_extern || sym->kind != SYM_FUNC)
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

static bool ccb_parse_symbol_after_prefix(const char *line, const char *prefix,
                                          char *out, size_t outsz)
{
    if (!line || !prefix || !out || outsz == 0)
        return false;
    out[0] = '\0';

    const char *cursor = line;
    while (*cursor == ' ' || *cursor == '\t')
        ++cursor;

    size_t prelen = strlen(prefix);
    if (strncmp(cursor, prefix, prelen) != 0)
        return false;

    cursor += prelen;
    while (*cursor == ' ' || *cursor == '\t')
        ++cursor;
    if (*cursor == '\0')
        return false;

    size_t n = 0;
    while (*cursor && *cursor != ' ' && *cursor != '\t' && *cursor != '\r' && *cursor != '\n')
    {
        if (n + 1 >= outsz)
            return false;
        out[n++] = *cursor++;
    }

    if (n == 0)
        return false;
    out[n] = '\0';
    return true;
}

static bool ccb_parse_symbol_reference(const char *line, char *out, size_t outsz)
{
    if (!line || !out || outsz == 0)
        return false;
    out[0] = '\0';

    if (ccb_parse_symbol_after_prefix(line, "call ", out, outsz))
    {
        if (strcmp(out, "call_indirect") != 0)
            return true;
        out[0] = '\0';
    }

    return ccb_parse_symbol_after_prefix(line, "addr_global ", out, outsz) ||
           ccb_parse_symbol_after_prefix(line, "load_global ", out, outsz) ||
           ccb_parse_symbol_after_prefix(line, "store_global ", out, outsz);
}

static bool ccb_parse_extern_symbol(const char *line, char *out, size_t outsz)
{
    return ccb_parse_symbol_after_prefix(line, ".extern ", out, outsz);
}

static bool ccb_parse_no_return_symbol(const char *line, char *out, size_t outsz)
{
    return ccb_parse_symbol_after_prefix(line, ".no-return ", out, outsz);
}

static void ccb_module_optimize(CcbModule *mod, const CodegenOptions *opts)
{
    if (!mod || !opts || opts->opt_level < 2 || mod->lines.count == 0)
        return;

    StringList used_symbols;
    string_list_init(&used_symbols);

    for (size_t i = 0; i < mod->lines.count; ++i)
    {
        const char *line = mod->lines.items[i];
        char symbol[256] = {0};
        if (!ccb_parse_symbol_reference(line, symbol, sizeof(symbol)))
            continue;
        if (!string_list_contains(&used_symbols, symbol))
            string_list_append(&used_symbols, symbol);
    }

    size_t removed_externs = 0;
    size_t removed_no_return = 0;
    for (size_t i = 0; i < mod->lines.count;)
    {
        const char *line = mod->lines.items[i];
        char symbol[256] = {0};

        if (ccb_parse_extern_symbol(line, symbol, sizeof(symbol)))
        {
            if (!string_list_contains(&used_symbols, symbol))
            {
                string_list_remove_range(&mod->lines, i, 1);
                ++removed_externs;
                continue;
            }
            ++i;
            continue;
        }

        if (ccb_parse_no_return_symbol(line, symbol, sizeof(symbol)))
        {
            bool symbol_is_defined = string_list_contains(&mod->defined_funcs, symbol);
            bool symbol_is_used = string_list_contains(&used_symbols, symbol);
            if (!symbol_is_defined && !symbol_is_used)
            {
                string_list_remove_range(&mod->lines, i, 1);
                ++removed_no_return;
                continue;
            }
        }

        ++i;
    }

    if (compiler_verbose_enabled() && (removed_externs || removed_no_return))
    {
        compiler_verbose_logf("optimizer", "module pass: pruned %zu externs, %zu no-return tags",
                              removed_externs, removed_no_return);
    }

    string_list_free(&used_symbols);
}

static bool ccb_node_uses_tracked_alloc(const Node *node)
{
    if (!node)
        return false;

    if (node->kind == ND_NEW)
        return true;

    if (ccb_node_uses_tracked_alloc(node->lhs) ||
        ccb_node_uses_tracked_alloc(node->rhs) ||
        ccb_node_uses_tracked_alloc(node->body) ||
        ccb_node_uses_tracked_alloc(node->type_expr))
    {
        return true;
    }

    for (int i = 0; i < node->arg_count; ++i)
    {
        if (ccb_node_uses_tracked_alloc(node->args[i]))
            return true;
    }

    for (int i = 0; i < node->stmt_count; ++i)
    {
        if (ccb_node_uses_tracked_alloc(node->stmts[i]))
            return true;
    }

    for (int i = 0; i < node->init.count; ++i)
    {
        if (ccb_node_uses_tracked_alloc(node->init.elems[i]))
            return true;
    }

    if (ccb_node_uses_tracked_alloc(node->switch_stmt.expr))
        return true;
    for (int i = 0; i < node->switch_stmt.case_count; ++i)
    {
        if (ccb_node_uses_tracked_alloc(node->switch_stmt.cases[i].value) ||
            ccb_node_uses_tracked_alloc(node->switch_stmt.cases[i].body))
        {
            return true;
        }
    }

    if (ccb_node_uses_tracked_alloc(node->match_stmt.expr))
        return true;
    for (int i = 0; i < node->match_stmt.arm_count; ++i)
    {
        if (ccb_node_uses_tracked_alloc(node->match_stmt.arms[i].pattern) ||
            ccb_node_uses_tracked_alloc(node->match_stmt.arms[i].guard) ||
            ccb_node_uses_tracked_alloc(node->match_stmt.arms[i].body))
        {
            return true;
        }
    }

    return false;
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
    fb->needs_gc_prep = false;
    fb->loop_stack = NULL;
    fb->loop_depth = 0;
    fb->loop_capacity = 0;
    fb->active_try_error_label = NULL;
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
    fb->needs_gc_prep = false;
    free(fb->loop_stack);
    fb->loop_stack = NULL;
    fb->loop_depth = 0;
    fb->loop_capacity = 0;
    fb->active_try_error_label = NULL;
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

static int ccb_emit_active_try_pending_branch(CcbFunctionBuilder *fb)
{
    if (!fb || !fb->active_try_error_label || !fb->active_try_error_label[0])
        return 0;

    if (!string_list_appendf(&fb->body, "  call __cert__exception_has_pending i32 ()"))
        return 1;
    if (!ccb_emit_const_zero(&fb->body, CC_TYPE_I32))
        return 1;
    if (!string_list_appendf(&fb->body, "  compare ne i32"))
        return 1;

    char continue_label[32];
    ccb_make_label(fb, continue_label, sizeof(continue_label), "try_expr_cont");
    if (!string_list_appendf(&fb->body, "  branch %s %s", fb->active_try_error_label, continue_label))
        return 1;
    if (!string_list_appendf(&fb->body, "label %s", continue_label))
        return 1;
    return 0;
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

        if (ccb_function_param_has_managed_length(target_fn, i))
        {
            char *length_name = ccb_make_managed_length_name(param_name);
            CcbLocal *length_local = ccb_local_add_u64(fb, length_name, false);
            if (!length_local)
            {
                diag_error_at(target_fn->src, target_fn->line, target_fn->col,
                              "failed to allocate local for inline managed array length %d", i);
                if (mutable_target)
                    mutable_target->inline_needs_body = 1;
                goto cleanup;
            }
            if (ccb_emit_managed_array_length_value(fb, arg))
            {
                if (mutable_target)
                    mutable_target->inline_needs_body = 1;
                goto cleanup;
            }
            if (!ccb_emit_store_local(fb, length_local))
            {
                diag_error_at(arg->src, arg->line, arg->col,
                              "failed to store inline managed array length");
                if (mutable_target)
                    mutable_target->inline_needs_body = 1;
                goto cleanup;
            }
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

static bool ccb_type_is_ptr_name(const char *type_name)
{
    return type_name && strcmp(type_name, "ptr") == 0;
}

static bool ccb_type_is_signed_name(const char *type_name)
{
    return type_name && type_name[0] == 'i';
}

static bool ccb_instruction_is_drop(const char *line)
{
    if (!line)
        return false;
    while (*line == ' ' || *line == '\t')
        ++line;
    return strncmp(line, "drop ", 5) == 0;
}

static bool ccb_parse_local_index(const char *line, const char *prefix, int *out_index)
{
    if (!line || !prefix || !out_index)
        return false;
    line = ccb_trim_leading_ws(line);
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

static bool ccb_is_const_str_line(const char *line)
{
    line = ccb_trim_leading_ws(line);
    return line && strncmp(line, "const_str ", 10) == 0;
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
    if (strncmp(line, "test_null", 9) == 0)
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
    bool is_ptr = ccb_type_is_ptr_name(type_name);
    if (!ccb_type_is_integer_name(type_name) && !is_ptr)
        return false;

    bool is_signed = ccb_type_is_signed_name(type_name);
    char *endptr = NULL;
    errno = 0;
    unsigned long long uval = 0;
    long long sval = 0;
    if (is_ptr && strcmp(value_text, "null") == 0)
    {
        uval = 0;
        sval = 0;
    }
    else if (is_signed)
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

static bool ccb_parse_unop_info(const char *line, char *op_buf, size_t op_sz,
                                char *type_buf, size_t type_sz)
{
    if (!line || !op_buf || !type_buf)
        return false;

    while (*line == ' ' || *line == '\t')
        ++line;
    if (strncmp(line, "unop ", 5) != 0)
        return false;
    line += 5;

    char op[16];
    char type_name[16];
    if (sscanf(line, "%15s %15s", op, type_name) != 2)
        return false;

    strncpy(op_buf, op, op_sz - 1);
    op_buf[op_sz - 1] = '\0';
    strncpy(type_buf, type_name, type_sz - 1);
    type_buf[type_sz - 1] = '\0';
    return true;
}

static bool ccb_parse_compare_info(const char *line, char *op_buf, size_t op_sz,
                                   char *type_buf, size_t type_sz, bool *is_unsigned)
{
    if (!line || !op_buf || !type_buf || !is_unsigned)
        return false;

    while (*line == ' ' || *line == '\t')
        ++line;
    if (strncmp(line, "compare ", 8) != 0)
        return false;
    line += 8;

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

static bool ccb_parse_convert_info(const char *line, char *kind_buf, size_t kind_sz,
                                   char *from_buf, size_t from_sz, char *to_buf, size_t to_sz)
{
    if (!line || !kind_buf || !from_buf || !to_buf)
        return false;

    while (*line == ' ' || *line == '\t')
        ++line;
    if (strncmp(line, "convert ", 8) != 0)
        return false;
    line += 8;

    char kind[16];
    char from_name[16];
    char to_name[16];
    if (sscanf(line, "%15s %15s %15s", kind, from_name, to_name) != 3)
        return false;

    strncpy(kind_buf, kind, kind_sz - 1);
    kind_buf[kind_sz - 1] = '\0';
    strncpy(from_buf, from_name, from_sz - 1);
    from_buf[from_sz - 1] = '\0';
    strncpy(to_buf, to_name, to_sz - 1);
    to_buf[to_sz - 1] = '\0';
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

static void ccb_format_const_line(char *out, size_t outsz, const char *type_name,
                                  unsigned long long uval, long long sval)
{
    if (!out || outsz == 0 || !type_name)
        return;
    if (ccb_type_is_signed_name(type_name))
        snprintf(out, outsz, "  const %s %lld", type_name, sval);
    else
        snprintf(out, outsz, "  const %s %llu", type_name, uval);
}

static const char *ccb_trim_leading_ws(const char *line)
{
    if (!line)
        return line;
    while (*line == ' ' || *line == '\t')
        ++line;
    return line;
}

static int ccb_module_has_extern(const CcbModule *mod, const char *name)
{
    if (!mod || !name)
        return 0;
    for (size_t i = 0; i < mod->lines.count; ++i)
    {
        const char *line = mod->lines.items[i];
        line = ccb_trim_leading_ws(line);
        if (!line || strncmp(line, ".extern ", 8) != 0)
            continue;
        const char *cursor = line + 8;
        while (*cursor == ' ' || *cursor == '\t')
            ++cursor;
        if (*cursor == '\0')
            continue;
        char symbol[256];
        size_t pos = 0;
        while (*cursor && !isspace((unsigned char)*cursor) && pos + 1 < sizeof(symbol))
            symbol[pos++] = *cursor++;
        symbol[pos] = '\0';
        if (symbol[0] != '\0' && strcmp(symbol, name) == 0)
            return 1;
    }
    return 0;
}

static int ccb_module_has_function(const CcbModule *mod, const char *name)
{
    if (!mod || !name)
        return 0;
    if (string_list_contains(&mod->defined_funcs, name))
        return 1;
    for (size_t i = 0; i < mod->lines.count; ++i)
    {
        const char *line = mod->lines.items[i];
        line = ccb_trim_leading_ws(line);
        if (!line || strncmp(line, ".func ", 6) != 0)
            continue;
        const char *cursor = line + 6;
        while (*cursor == ' ' || *cursor == '\t')
            ++cursor;
        if (*cursor == '\0')
            continue;
        char symbol[256];
        size_t pos = 0;
        while (*cursor && !isspace((unsigned char)*cursor) && pos + 1 < sizeof(symbol))
            symbol[pos++] = *cursor++;
        symbol[pos] = '\0';
        if (symbol[0] != '\0' && strcmp(symbol, name) == 0)
            return 1;
    }
    return 0;
}

static bool ccb_type_is_object_ast(const Type *ty)
{
    const Type *t = ty;
    while (t && t->kind == TY_REF)
        t = t->pointee;
    return t && t->is_object;
}

static void ccb_type_metadata_name(const Type *ty, char *buffer, size_t bufsz)
{
    if (!buffer || bufsz == 0)
        return;
    const Type *t = ty;
    while (t && t->kind == TY_REF)
        t = t->pointee;
    if (!t)
    {
        snprintf(buffer, bufsz, "unknown");
        return;
    }
    if (t->is_object)
    {
        snprintf(buffer, bufsz, "object");
        return;
    }
    switch (t->kind)
    {
    case TY_I8:
        snprintf(buffer, bufsz, "i8");
        return;
    case TY_U8:
        snprintf(buffer, bufsz, "u8");
        return;
    case TY_I16:
        snprintf(buffer, bufsz, "i16");
        return;
    case TY_U16:
        snprintf(buffer, bufsz, "u16");
        return;
    case TY_I32:
        snprintf(buffer, bufsz, "i32");
        return;
    case TY_U32:
        snprintf(buffer, bufsz, "u32");
        return;
    case TY_I64:
        snprintf(buffer, bufsz, "i64");
        return;
    case TY_U64:
        snprintf(buffer, bufsz, "u64");
        return;
    case TY_F32:
        snprintf(buffer, bufsz, "f32");
        return;
    case TY_F64:
        snprintf(buffer, bufsz, "f64");
        return;
    case TY_CHAR:
        snprintf(buffer, bufsz, "char");
        return;
    case TY_BOOL:
        snprintf(buffer, bufsz, "bool");
        return;
    case TY_PTR:
        if (t->pointee && t->pointee->kind == TY_STRUCT && t->pointee->struct_name)
        {
            snprintf(buffer, bufsz, "ptr(struct:%s)", t->pointee->struct_name);
            return;
        }
        if (t->pointee && t->pointee->kind == TY_CHAR)
        {
            snprintf(buffer, bufsz, "string");
            return;
        }
        snprintf(buffer, bufsz, "ptr");
        return;
    case TY_STRUCT:
        snprintf(buffer, bufsz, "struct:%s", t->struct_name ? t->struct_name : "anon");
        return;
    case TY_ARRAY:
        snprintf(buffer, bufsz, "array");
        return;
    default:
        snprintf(buffer, bufsz, "%s", cc_type_name(map_type_to_cc(t)));
        return;
    }
}

static int ccb_emit_const_str_lit(CcbFunctionBuilder *fb, const char *text)
{
    if (!fb)
        return 1;
    const char *safe = text ? text : "";
    char *escaped = ccb_escape_string_literal(safe, (int)strlen(safe));
    if (!escaped)
        return 1;
    bool ok = string_list_appendf(&fb->body, "  const_str \"%s\"", escaped);
    free(escaped);
    return ok ? 0 : 1;
}

static int ccb_ensure_runtime_extern(CcbFunctionBuilder *fb, const char *name, const char *ret_ty, const char *params)
{
    if (!fb || !fb->module || !name || !ret_ty || !params)
        return 1;
    if (ccb_module_has_function(fb->module, name) || ccb_module_has_extern(fb->module, name))
        return 0;
    if (!ccb_module_appendf(fb->module, ".extern %s params=%s returns=%s", name, params, ret_ty))
        return 1;
    return 0;
}

static bool ccb_parse_load_ref(const char *line, const char **op, int *index)
{
    if (!line || !op || !index)
        return false;
    line = ccb_trim_leading_ws(line);
    if (strncmp(line, "load_local ", 11) == 0)
    {
        line += 11;
        *op = "load_local";
    }
    else if (strncmp(line, "load_param ", 11) == 0)
    {
        line += 11;
        *op = "load_param";
    }
    else
    {
        return false;
    }
    char *endptr = NULL;
    long val = strtol(line, &endptr, 10);
    if (endptr == line || (*endptr && !isspace((unsigned char)*endptr)))
        return false;
    *index = (int)val;
    return true;
}

static bool ccb_parse_const_i32(const char *line, int *value)
{
    if (!line || !value)
        return false;
    line = ccb_trim_leading_ws(line);
    if (strncmp(line, "const i32 ", 10) != 0)
        return false;
    line += 10;
    char *endptr = NULL;
    long val = strtol(line, &endptr, 10);
    if (endptr == line || (*endptr && !isspace((unsigned char)*endptr)))
        return false;
    *value = (int)val;
    return true;
}

static bool ccb_parse_const_i64(const char *line, long long *value)
{
    if (!line || !value)
        return false;
    line = ccb_trim_leading_ws(line);
    if (strncmp(line, "const i64 ", 10) != 0)
        return false;
    line += 10;
    char *endptr = NULL;
    long long val = strtoll(line, &endptr, 10);
    if (endptr == line || (*endptr && !isspace((unsigned char)*endptr)))
        return false;
    *value = val;
    return true;
}

static bool ccb_is_exact_line(const char *line, const char *expected)
{
    line = ccb_trim_leading_ws(line);
    return line && expected && strcmp(line, expected) == 0;
}

static bool ccb_is_prefix_line(const char *line, const char *prefix)
{
    line = ccb_trim_leading_ws(line);
    if (!line || !prefix)
        return false;
    size_t len = strlen(prefix);
    return strncmp(line, prefix, len) == 0;
}

static bool ccb_parse_label_name(const char *line, char *out, size_t outsz)
{
    if (!line || !out || outsz == 0)
        return false;
    line = ccb_trim_leading_ws(line);
    if (strncmp(line, "label ", 6) != 0)
        return false;
    const char *cursor = line + 6;
    while (*cursor == ' ' || *cursor == '\t')
        ++cursor;
    if (*cursor == '\0')
        return false;
    size_t pos = 0;
    while (*cursor && !isspace((unsigned char)*cursor) && pos + 1 < outsz)
        out[pos++] = *cursor++;
    out[pos] = '\0';
    return pos > 0;
}

static bool ccb_parse_branch_targets(const char *line, char *true_label,
                                     size_t true_sz, char *false_label,
                                     size_t false_sz)
{
    if (!line || !true_label || !false_label || true_sz == 0 || false_sz == 0)
        return false;
    line = ccb_trim_leading_ws(line);
    if (strncmp(line, "branch ", 7) != 0)
        return false;
    const char *cursor = line + 7;
    while (*cursor == ' ' || *cursor == '\t')
        ++cursor;
    size_t pos = 0;
    while (*cursor && !isspace((unsigned char)*cursor) && pos + 1 < true_sz)
        true_label[pos++] = *cursor++;
    true_label[pos] = '\0';
    while (*cursor == ' ' || *cursor == '\t')
        ++cursor;
    pos = 0;
    while (*cursor && !isspace((unsigned char)*cursor) && pos + 1 < false_sz)
        false_label[pos++] = *cursor++;
    false_label[pos] = '\0';
    return true_label[0] != '\0' && false_label[0] != '\0';
}

static bool ccb_parse_jump_target(const char *line, char *out, size_t outsz)
{
    if (!line || !out || outsz == 0)
        return false;
    line = ccb_trim_leading_ws(line);
    if (strncmp(line, "jump ", 5) != 0)
        return false;
    const char *cursor = line + 5;
    while (*cursor == ' ' || *cursor == '\t')
        ++cursor;
    if (*cursor == '\0')
        return false;
    size_t pos = 0;
    while (*cursor && !isspace((unsigned char)*cursor) && pos + 1 < outsz)
        out[pos++] = *cursor++;
    out[pos] = '\0';
    return pos > 0;
}

static bool ccb_parse_zero_store_at_offset(const StringList *body, size_t index,
                                           const char **load_op, int *load_index,
                                           int *offset, size_t *stride)
{
    if (!body || !load_op || !load_index || !offset || !stride)
        return false;
    if (index + 6 >= body->count)
        return false;

    const char *line0 = body->items[index + 0];
    const char *line1 = body->items[index + 1];
    const char *line2 = body->items[index + 2];
    const char *line3 = body->items[index + 3];
    const char *line4 = body->items[index + 4];
    const char *line5 = body->items[index + 5];
    const char *line6 = body->items[index + 6];
    const char *line7 = (index + 7 < body->count) ? body->items[index + 7] : NULL;

    if (!ccb_parse_load_ref(line0, load_op, load_index))
        return false;
    if (!ccb_is_exact_line(line1, "convert bitcast ptr i64"))
        return false;

    if (ccb_parse_const_i32(line2, offset))
    {
        if (!ccb_is_exact_line(line3, "convert sext i32 i64") &&
            !ccb_is_exact_line(line3, "convert zext i32 i64"))
            return false;
        if (!ccb_is_exact_line(line4, "binop add i64"))
            return false;
        if (!ccb_is_exact_line(line5, "convert bitcast i64 ptr"))
            return false;
        if (!ccb_is_exact_line(line6, "const i8 0"))
            return false;
        if (!line7 || !ccb_is_exact_line(line7, "store_indirect i8"))
            return false;
        *stride = 8;
        return true;
    }

    long long offset64 = 0;
    if (!ccb_parse_const_i64(line2, &offset64))
        return false;
    if (offset64 < INT_MIN || offset64 > INT_MAX)
        return false;
    *offset = (int)offset64;
    if (!ccb_is_exact_line(line3, "binop add i64"))
        return false;
    if (!ccb_is_exact_line(line4, "convert bitcast i64 ptr"))
        return false;
    if (!ccb_is_exact_line(line5, "const i8 0"))
        return false;
    if (!ccb_is_exact_line(line6, "store_indirect i8"))
        return false;
    *stride = 7;
    return true;
}

typedef struct
{
    const char *load_op;
    int load_index;
    int offset;
    unsigned byte_value;
    size_t start;
    size_t end;
    size_t value_line;
    size_t store_line;
} CcbByteStorePattern;

typedef struct
{
    const char *load_op;
    int load_index;
    int offset;
    int width_bytes;
    size_t start;
    size_t end;
} CcbIndirectStorePattern;

typedef struct
{
    size_t start;
    size_t addr_start;
    size_t addr_end;
    size_t const_line;
    size_t store_line;
    size_t end;
    CcbConstInfo value;
} CcbConstStoreBlock;

typedef struct
{
    size_t start;
    size_t end;
    unsigned long long or_value;
} CcbOrStoreBlock;

static bool ccb_is_loc_directive(const char *line)
{
    if (!line)
        return false;
    line = ccb_trim_leading_ws(line);
    return strncmp(line, ".loc ", 5) == 0;
}

static size_t ccb_skip_loc_directives(const StringList *body, size_t index)
{
    if (!body)
        return index;
    while (index < body->count && ccb_is_loc_directive(body->items[index]))
        ++index;
    return index;
}

static bool ccb_is_byte_store_line(const char *line)
{
    if (!line)
        return false;
    line = ccb_trim_leading_ws(line);
    return strcmp(line, "store_indirect i8") == 0 || strcmp(line, "store_indirect u8") == 0;
}

static bool ccb_parse_store_indirect_width(const char *line, int *out_width_bytes)
{
    if (!line || !out_width_bytes)
        return false;
    line = ccb_trim_leading_ws(line);
    if (strncmp(line, "store_indirect ", 15) != 0)
        return false;
    line += 15;
    while (*line == ' ' || *line == '\t')
        ++line;
    if (*line == '\0')
        return false;
    char type_name[16] = {0};
    size_t pos = 0;
    while (*line && !isspace((unsigned char)*line) && pos + 1 < sizeof(type_name))
        type_name[pos++] = *line++;
    type_name[pos] = '\0';
    unsigned bits = ccb_type_bit_width_from_name(type_name);
    if (bits == 0 || bits % 8 != 0)
        return false;
    *out_width_bytes = (int)(bits / 8);
    return *out_width_bytes > 0;
}

static bool ccb_parse_indirect_store_pattern(const StringList *body, size_t start,
                                             CcbIndirectStorePattern *out)
{
    if (!body || !out || start >= body->count)
        return false;

    memset(out, 0, sizeof(*out));
    out->start = start;

    size_t p = ccb_skip_loc_directives(body, start);
    if (p >= body->count)
        return false;
    if (!ccb_parse_load_ref(body->items[p], &out->load_op, &out->load_index))
        return false;

    int offset = 0;
    size_t p_next = ccb_skip_loc_directives(body, p + 1);
    if (p_next >= body->count)
        return false;

    if (ccb_is_exact_line(body->items[p_next], "convert bitcast ptr i64"))
    {
        p_next = ccb_skip_loc_directives(body, p_next + 1);
        long long offset_i64 = 0;
        if (!ccb_parse_const_i64(body->items[p_next], &offset_i64))
            return false;
        if (offset_i64 < INT_MIN || offset_i64 > INT_MAX)
            return false;
        offset = (int)offset_i64;

        p_next = ccb_skip_loc_directives(body, p_next + 1);
        if (p_next >= body->count || !ccb_is_exact_line(body->items[p_next], "binop add i64"))
            return false;

        p_next = ccb_skip_loc_directives(body, p_next + 1);
        if (p_next >= body->count || !ccb_is_exact_line(body->items[p_next], "convert bitcast i64 ptr"))
            return false;

        p_next = ccb_skip_loc_directives(body, p_next + 1);
    }
    else
    {
        offset = 0;
    }

    if (p_next >= body->count)
        return false;
    CcbConstInfo cinfo;
    if (!ccb_parse_const_info(body->items[p_next], &cinfo))
        return false;

    p_next = ccb_skip_loc_directives(body, p_next + 1);
    if (p_next >= body->count)
        return false;
    int width_bytes = 0;
    if (!ccb_parse_store_indirect_width(body->items[p_next], &width_bytes))
        return false;

    out->offset = offset;
    out->width_bytes = width_bytes;
    out->end = p_next + 1;
    return true;
}

static bool ccb_parse_byte_store_pattern(const StringList *body, size_t start, CcbByteStorePattern *out)
{
    if (!body || !out || start >= body->count)
        return false;

    memset(out, 0, sizeof(*out));
    out->start = start;

    size_t p = ccb_skip_loc_directives(body, start);
    if (p >= body->count)
        return false;
    if (!ccb_parse_load_ref(body->items[p], &out->load_op, &out->load_index))
        return false;

    p = ccb_skip_loc_directives(body, p + 1);
    if (p >= body->count || !ccb_is_exact_line(body->items[p], "convert bitcast ptr i64"))
        return false;

    p = ccb_skip_loc_directives(body, p + 1);
    if (p >= body->count)
        return false;

    int offset_i32 = 0;
    long long offset_i64 = 0;
    if (ccb_parse_const_i32(body->items[p], &offset_i32))
    {
        out->offset = offset_i32;

        p = ccb_skip_loc_directives(body, p + 1);
        if (p >= body->count)
            return false;
        if (!ccb_is_exact_line(body->items[p], "convert sext i32 i64") &&
            !ccb_is_exact_line(body->items[p], "convert zext i32 i64"))
            return false;

        p = ccb_skip_loc_directives(body, p + 1);
        if (p >= body->count || !ccb_is_exact_line(body->items[p], "binop add i64"))
            return false;

        p = ccb_skip_loc_directives(body, p + 1);
        if (p >= body->count || !ccb_is_exact_line(body->items[p], "convert bitcast i64 ptr"))
            return false;
    }
    else
    {
        if (!ccb_parse_const_i64(body->items[p], &offset_i64))
            return false;
        if (offset_i64 < INT_MIN || offset_i64 > INT_MAX)
            return false;
        out->offset = (int)offset_i64;

        p = ccb_skip_loc_directives(body, p + 1);
        if (p >= body->count || !ccb_is_exact_line(body->items[p], "binop add i64"))
            return false;

        p = ccb_skip_loc_directives(body, p + 1);
        if (p >= body->count || !ccb_is_exact_line(body->items[p], "convert bitcast i64 ptr"))
            return false;
    }

    p = ccb_skip_loc_directives(body, p + 1);
    if (p >= body->count)
        return false;

    CcbConstInfo cinfo;
    if (!ccb_parse_const_info(body->items[p], &cinfo))
        return false;
    if (!(strcmp(cinfo.type, "i8") == 0 || strcmp(cinfo.type, "u8") == 0))
        return false;
    out->byte_value = (unsigned)(cinfo.u & 0xFFULL);
    out->value_line = p;

    p = ccb_skip_loc_directives(body, p + 1);
    if (p >= body->count || !ccb_is_byte_store_line(body->items[p]))
        return false;
    out->store_line = p;
    out->end = p + 1;
    return true;
}

static bool ccb_trimmed_lines_equal(const char *lhs, const char *rhs)
{
    lhs = ccb_trim_leading_ws(lhs);
    rhs = ccb_trim_leading_ws(rhs);
    if (!lhs || !rhs)
        return false;
    return strcmp(lhs, rhs) == 0;
}

static bool ccb_match_addr_sequence(const StringList *body, size_t start,
                                    size_t addr_ref_start, size_t addr_ref_end,
                                    size_t *out_next)
{
    if (!body || !out_next || addr_ref_end <= addr_ref_start)
        return false;

    size_t cursor = ccb_skip_loc_directives(body, start);
    size_t ref = ccb_skip_loc_directives(body, addr_ref_start);
    while (ref < addr_ref_end)
    {
        if (cursor >= body->count)
            return false;
        if (!ccb_trimmed_lines_equal(body->items[cursor], body->items[ref]))
            return false;
        cursor = ccb_skip_loc_directives(body, cursor + 1);
        ref = ccb_skip_loc_directives(body, ref + 1);
    }

    *out_next = cursor;
    return true;
}

static bool ccb_parse_const_store_block(const StringList *body, size_t start,
                                        CcbConstStoreBlock *out)
{
    if (!body || !out || start >= body->count)
        return false;

    memset(out, 0, sizeof(*out));
    out->start = start;

    size_t p = ccb_skip_loc_directives(body, start);
    if (p >= body->count)
        return false;

    const size_t addr_start = p;
    size_t addr_steps = 0;
    while (p < body->count)
    {
        CcbConstInfo cinfo;
        if (ccb_parse_const_info(body->items[p], &cinfo))
        {
            size_t store_line = ccb_skip_loc_directives(body, p + 1);
            if (store_line >= body->count)
                return false;
            char expected_store[64] = {0};
            snprintf(expected_store, sizeof(expected_store), "store_indirect %s", cinfo.type);
            if (!ccb_is_exact_line(body->items[store_line], expected_store))
                return false;
            if (addr_steps == 0)
                return false;

            size_t prev = p;
            bool found_prev = false;
            while (prev > 0)
            {
                --prev;
                if (ccb_is_loc_directive(body->items[prev]))
                    continue;
                found_prev = true;
                break;
            }
            if (!found_prev || !ccb_is_exact_line(body->items[prev], "convert bitcast i64 ptr"))
                return false;

            out->addr_start = addr_start;
            out->addr_end = p;
            out->const_line = p;
            out->store_line = store_line;
            out->end = store_line + 1;
            out->value = cinfo;
            return true;
        }

        const char *line = ccb_trim_leading_ws(body->items[p]);
        if (!line)
            return false;

        bool addr_step = strncmp(line, "addr_global ", 12) == 0 ||
                         strncmp(line, "addr_local ", 11) == 0 ||
                         strncmp(line, "addr_param ", 11) == 0;
        if (!addr_step && !ccb_instruction_is_pure(line))
            return false;

        ++addr_steps;
        if (addr_steps > 16)
            return false;

        ++p;
        p = ccb_skip_loc_directives(body, p);
    }

    return false;
}

static bool ccb_parse_or_store_block(const StringList *body, size_t start,
                                     size_t addr_ref_start, size_t addr_ref_end,
                                     const char *type_name,
                                     CcbOrStoreBlock *out)
{
    if (!body || !type_name || !out || start >= body->count || addr_ref_end <= addr_ref_start)
        return false;

    memset(out, 0, sizeof(*out));
    out->start = start;

    size_t p = 0;
    if (!ccb_match_addr_sequence(body, start, addr_ref_start, addr_ref_end, &p))
        return false;

    char expected_line[64] = {0};
    snprintf(expected_line, sizeof(expected_line), "load_indirect %s", type_name);

    // Form A (direct stack):
    //   [addr-expr]
    //   [dup]
    //   load_indirect T
    //   const T C
    //   binop or T
    //   store_indirect T
    size_t q = p;
    if (q < body->count && ccb_is_prefix_line(body->items[q], "dup"))
        q = ccb_skip_loc_directives(body, q + 1);
    if (q < body->count && ccb_is_exact_line(body->items[q], expected_line))
    {
        q = ccb_skip_loc_directives(body, q + 1);

        CcbConstInfo direct_const;
        if (q < body->count && ccb_parse_const_info(body->items[q], &direct_const) &&
            strcmp(direct_const.type, type_name) == 0)
        {
            q = ccb_skip_loc_directives(body, q + 1);

            char op_name[16] = {0};
            char op_type[16] = {0};
            bool unsigned_hint = false;
            if (q < body->count &&
                ccb_parse_binop_info(body->items[q], op_name, sizeof(op_name), op_type, sizeof(op_type), &unsigned_hint) &&
                strcmp(op_name, "or") == 0 && strcmp(op_type, type_name) == 0)
            {
                (void)unsigned_hint;
                q = ccb_skip_loc_directives(body, q + 1);

                snprintf(expected_line, sizeof(expected_line), "store_indirect %s", type_name);
                if (q < body->count && ccb_is_exact_line(body->items[q], expected_line))
                {
                    out->or_value = direct_const.u;
                    out->end = q + 1;
                    return true;
                }
            }
        }
    }

    int ptr_idx = -1;
    int val_idx = -1;
    if (p >= body->count || !ccb_parse_local_index(body->items[p], "store_local", &ptr_idx))
        return false;
    p = ccb_skip_loc_directives(body, p + 1);

    int idx = -1;
    if (p >= body->count || !ccb_parse_local_index(body->items[p], "load_local", &idx) || idx != ptr_idx)
        return false;
    p = ccb_skip_loc_directives(body, p + 1);

    snprintf(expected_line, sizeof(expected_line), "load_indirect %s", type_name);
    if (p >= body->count || !ccb_is_exact_line(body->items[p], expected_line))
        return false;
    p = ccb_skip_loc_directives(body, p + 1);

    if (p >= body->count || !ccb_parse_local_index(body->items[p], "store_local", &val_idx))
        return false;
    p = ccb_skip_loc_directives(body, p + 1);

    if (p >= body->count || !ccb_parse_local_index(body->items[p], "load_local", &idx) || idx != val_idx)
        return false;
    p = ccb_skip_loc_directives(body, p + 1);

    CcbConstInfo cinfo;
    if (p >= body->count || !ccb_parse_const_info(body->items[p], &cinfo) || strcmp(cinfo.type, type_name) != 0)
        return false;
    out->or_value = cinfo.u;
    p = ccb_skip_loc_directives(body, p + 1);

    char op_name[16] = {0};
    char op_type[16] = {0};
    bool unsigned_hint = false;
    if (p >= body->count ||
        !ccb_parse_binop_info(body->items[p], op_name, sizeof(op_name), op_type, sizeof(op_type), &unsigned_hint) ||
        strcmp(op_name, "or") != 0 || strcmp(op_type, type_name) != 0)
        return false;
    (void)unsigned_hint;
    p = ccb_skip_loc_directives(body, p + 1);

    if (p >= body->count || !ccb_parse_local_index(body->items[p], "store_local", &idx) || idx != val_idx)
        return false;
    p = ccb_skip_loc_directives(body, p + 1);

    if (p >= body->count || !ccb_parse_local_index(body->items[p], "load_local", &idx) || idx != ptr_idx)
        return false;
    p = ccb_skip_loc_directives(body, p + 1);

    if (p >= body->count || !ccb_parse_local_index(body->items[p], "load_local", &idx) || idx != val_idx)
        return false;
    p = ccb_skip_loc_directives(body, p + 1);

    snprintf(expected_line, sizeof(expected_line), "store_indirect %s", type_name);
    if (p >= body->count || !ccb_is_exact_line(body->items[p], expected_line))
        return false;

    out->end = p + 1;
    return true;
}

static bool ccb_replace_linef(StringList *body, size_t index, const char *fmt, ...)
{
    if (!body || !fmt || index >= body->count)
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
    free(body->items[index]);
    body->items[index] = buffer;
    return true;
}

static bool ccb_replace_line_copy(StringList *body, size_t index, const char *replacement)
{
    if (!body || !replacement || index >= body->count)
        return false;
    char *copy = xstrdup(replacement);
    if (!copy)
        return false;
    free(body->items[index]);
    body->items[index] = copy;
    return true;
}

static const char *ccb_local_type_name_by_index(const CcbFunctionBuilder *fb, int local_index)
{
    if (!fb || local_index < 0)
        return NULL;
    for (size_t i = 0; i < fb->locals_count; ++i)
    {
        const CcbLocal *local = &fb->locals[i];
        if (!local->is_param && local->index == local_index)
            return cc_type_name(local->value_type);
    }
    return NULL;
}

static bool ccb_line_is_clonable_local_value(const char *line)
{
    line = ccb_trim_leading_ws(line);
    if (!line)
        return false;
    return strncmp(line, "const ", 6) == 0 ||
           strncmp(line, "const_str ", 10) == 0 ||
           strncmp(line, "load_param ", 11) == 0 ||
           strncmp(line, "addr_param ", 11) == 0 ||
           strncmp(line, "addr_local ", 11) == 0 ||
           strncmp(line, "addr_global ", 12) == 0;
}

static bool ccb_const_is_integer_value(const CcbConstInfo *info, unsigned long long expected)
{
    if (!info)
        return false;
    unsigned long long mask = ccb_mask_for_width(info->width);
    return (info->u & mask) == (expected & mask);
}

static bool ccb_const_is_all_ones(const CcbConstInfo *info)
{
    if (!info || info->width == 0)
        return false;
    return ccb_const_is_integer_value(info, ccb_mask_for_width(info->width));
}

static bool ccb_const_is_power_of_two(const CcbConstInfo *info, unsigned *shift_out)
{
    if (!info || info->width == 0)
        return false;
    unsigned long long value = info->u & ccb_mask_for_width(info->width);
    if (value == 0ULL || (value & (value - 1ULL)) != 0ULL)
        return false;
    unsigned shift = 0;
    while ((value >> shift) != 1ULL)
        ++shift;
    if (shift_out)
        *shift_out = shift;
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

static void ccb_opt_fold_const_unops(CcbFunctionBuilder *fb)
{
    if (!fb)
        return;

    StringList *body = &fb->body;
    size_t i = 0;
    while (i + 1 < body->count)
    {
        const char *line0 = body->items[i];
        const char *line1 = body->items[i + 1];

        CcbConstInfo operand;
        if (!ccb_parse_const_info(line0, &operand))
        {
            ++i;
            continue;
        }

        char op[16];
        char type_name[16];
        if (!ccb_parse_unop_info(line1, op, sizeof(op), type_name,
                                 sizeof(type_name)))
        {
            ++i;
            continue;
        }

        if (strcmp(type_name, operand.type) != 0)
        {
            ++i;
            continue;
        }

        unsigned width = operand.width;
        if (width == 0)
        {
            ++i;
            continue;
        }

        unsigned long long result_u = 0;
        long long result_s = 0;
        const char *result_type = type_name;
        bool handled = true;

        if (strcmp(op, "not") == 0)
        {
            bool truthy = (operand.u & ccb_mask_for_width(width)) != 0ULL;
            result_u = truthy ? 0ULL : 1ULL;
            result_s = (long long)result_u;
            result_type = "i1";
        }
        else if (strcmp(op, "neg") == 0)
        {
            unsigned long long mask = ccb_mask_for_width(width);
            if (operand.is_signed)
            {
                long long neg = -operand.s;
                result_u = (unsigned long long)neg & mask;
                result_s = ccb_sign_extend(result_u, width);
            }
            else
            {
                result_u = (unsigned long long)(0ULL - operand.u) & mask;
                result_s = (long long)result_u;
            }
        }
        else if (strcmp(op, "bitnot") == 0)
        {
            unsigned long long mask = ccb_mask_for_width(width);
            result_u = (~operand.u) & mask;
            result_s = operand.is_signed ? ccb_sign_extend(result_u, width)
                                         : (long long)result_u;
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

        char new_line[128];
        ccb_format_const_line(new_line, sizeof(new_line), result_type, result_u,
                              result_s);

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
        string_list_remove_range(body, i + 1, 1);
        if (i > 0)
            --i;
    }
}

static void ccb_opt_fold_const_compares(CcbFunctionBuilder *fb)
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
        if (!ccb_parse_compare_info(line2, op, sizeof(op), type_name,
                                    sizeof(type_name), &unsigned_hint))
        {
            ++i;
            continue;
        }
        if (strcmp(type_name, lhs.type) != 0)
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

        unsigned long long mask = ccb_mask_for_width(width);
        unsigned long long lhs_u = lhs.u & mask;
        unsigned long long rhs_u = rhs.u & mask;
        long long lhs_s = ccb_sign_extend(lhs_u, width);
        long long rhs_s = ccb_sign_extend(rhs_u, width);
        bool use_unsigned = unsigned_hint || !lhs.is_signed || ccb_type_is_ptr_name(type_name);

        bool result = false;
        bool handled = true;
        if (strcmp(op, "eq") == 0)
            result = use_unsigned ? (lhs_u == rhs_u) : (lhs_s == rhs_s);
        else if (strcmp(op, "ne") == 0)
            result = use_unsigned ? (lhs_u != rhs_u) : (lhs_s != rhs_s);
        else if (strcmp(op, "lt") == 0)
            result = use_unsigned ? (lhs_u < rhs_u) : (lhs_s < rhs_s);
        else if (strcmp(op, "le") == 0)
            result = use_unsigned ? (lhs_u <= rhs_u) : (lhs_s <= rhs_s);
        else if (strcmp(op, "gt") == 0)
            result = use_unsigned ? (lhs_u > rhs_u) : (lhs_s > rhs_s);
        else if (strcmp(op, "ge") == 0)
            result = use_unsigned ? (lhs_u >= rhs_u) : (lhs_s >= rhs_s);
        else
            handled = false;

        if (!handled)
        {
            ++i;
            continue;
        }

        char new_line[64];
        ccb_format_const_line(new_line, sizeof(new_line), "i1",
                              result ? 1ULL : 0ULL,
                              result ? 1LL : 0LL);

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

static void ccb_opt_fold_const_test_null(CcbFunctionBuilder *fb)
{
    if (!fb)
        return;

    StringList *body = &fb->body;
    size_t i = 0;
    while (i + 1 < body->count)
    {
        CcbConstInfo value;
        if (!ccb_parse_const_info(body->items[i], &value))
        {
            ++i;
            continue;
        }

        const char *line1 = ccb_trim_leading_ws(body->items[i + 1]);
        if (!line1 || strcmp(line1, "test_null") != 0)
        {
            ++i;
            continue;
        }

        bool is_null = (value.u & ccb_mask_for_width(value.width)) == 0ULL;

        char new_line[64];
        ccb_format_const_line(new_line, sizeof(new_line), "i1",
                              is_null ? 1ULL : 0ULL,
                              is_null ? 1LL : 0LL);

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
        string_list_remove_range(body, i + 1, 1);
        if (i > 0)
            --i;
    }
}

static void ccb_opt_fold_const_converts(CcbFunctionBuilder *fb)
{
    if (!fb)
        return;

    StringList *body = &fb->body;
    size_t i = 0;
    while (i + 1 < body->count)
    {
        const char *line0 = body->items[i];
        const char *line1 = body->items[i + 1];

        CcbConstInfo source;
        if (!ccb_parse_const_info(line0, &source))
        {
            ++i;
            continue;
        }

        char kind[16];
        char from_name[16];
        char to_name[16];
        if (!ccb_parse_convert_info(line1, kind, sizeof(kind), from_name,
                                    sizeof(from_name), to_name, sizeof(to_name)))
        {
            ++i;
            continue;
        }
        if (strcmp(from_name, source.type) != 0)
        {
            ++i;
            continue;
        }

        unsigned from_width = source.width;
        unsigned to_width = ccb_type_bit_width_from_name(to_name);
        if (from_width == 0 || to_width == 0)
        {
            ++i;
            continue;
        }

        if ((!ccb_type_is_integer_name(from_name) && !ccb_type_is_ptr_name(from_name)) ||
            (!ccb_type_is_integer_name(to_name) && !ccb_type_is_ptr_name(to_name)))
        {
            ++i;
            continue;
        }

        unsigned long long mask_from = ccb_mask_for_width(from_width);
        unsigned long long mask_to = ccb_mask_for_width(to_width);
        unsigned long long value_u = source.u & mask_from;
        unsigned long long result_u = 0;
        long long result_s = 0;
        bool handled = true;

        if (strcmp(kind, "trunc") == 0)
        {
            result_u = value_u & mask_to;
        }
        else if (strcmp(kind, "sext") == 0)
        {
            long long signed_val = ccb_sign_extend(value_u, from_width);
            result_u = (unsigned long long)signed_val & mask_to;
        }
        else if (strcmp(kind, "zext") == 0)
        {
            result_u = value_u & mask_to;
        }
        else if (strcmp(kind, "bitcast") == 0)
        {
            result_u = value_u & mask_to;
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

        if (ccb_type_is_signed_name(to_name))
            result_s = ccb_sign_extend(result_u, to_width);
        else
            result_s = (long long)result_u;

        char new_line[128];
        ccb_format_const_line(new_line, sizeof(new_line), to_name, result_u,
                              result_s);

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
        string_list_remove_range(body, i + 1, 1);
        if (i > 0)
            --i;
    }
}

static void ccb_opt_strength_reduce_binops(CcbFunctionBuilder *fb)
{
    if (!fb)
        return;

    StringList *body = &fb->body;
    for (size_t i = 0; i + 2 < body->count; ++i)
    {
        CcbConstInfo rhs;
        char op[16];
        char type_name[16];
        bool unsigned_hint = false;
        if (!ccb_parse_const_info(body->items[i + 1], &rhs) ||
            !ccb_parse_binop_info(body->items[i + 2], op, sizeof(op), type_name, sizeof(type_name), &unsigned_hint) ||
            strcmp(rhs.type, type_name) != 0 ||
            !ccb_type_is_integer_name(type_name))
        {
            continue;
        }

        unsigned shift = 0;
        if (strcmp(op, "mul") == 0 && ccb_const_is_power_of_two(&rhs, &shift) && shift > 0)
        {
            ccb_replace_linef(body, i + 1, "  const %s %u", type_name, shift);
            ccb_replace_linef(body, i + 2, "  binop shl %s%s", type_name,
                              unsigned_hint ? " unsigned" : "");
            continue;
        }

        if (unsigned_hint && strcmp(op, "div") == 0 && ccb_const_is_power_of_two(&rhs, &shift) && shift > 0)
        {
            ccb_replace_linef(body, i + 1, "  const %s %u", type_name, shift);
            ccb_replace_linef(body, i + 2, "  binop shr %s unsigned", type_name);
            continue;
        }

        if (unsigned_hint && strcmp(op, "mod") == 0 && ccb_const_is_power_of_two(&rhs, &shift) && shift > 0)
        {
            unsigned long long mask = (shift >= rhs.width) ? ccb_mask_for_width(rhs.width) : ((1ULL << shift) - 1ULL);
            ccb_replace_linef(body, i + 1, "  const %s %llu", type_name, (unsigned long long)mask);
            ccb_replace_linef(body, i + 2, "  binop and %s unsigned", type_name);
        }
    }
}

static void ccb_opt_simplify_noop_arith_and_bitcasts(CcbFunctionBuilder *fb)
{
    if (!fb)
        return;

    StringList *body = &fb->body;
    size_t i = 0;
    while (i + 2 < body->count)
    {
        CcbConstInfo rhs_const;
        char op_name[16];
        char type_name[16];
        bool unsigned_hint = false;
        if (ccb_parse_const_info(body->items[i + 1], &rhs_const) &&
            ccb_parse_binop_info(body->items[i + 2], op_name, sizeof(op_name),
                                 type_name, sizeof(type_name), &unsigned_hint) &&
            strcmp(rhs_const.type, type_name) == 0 &&
            (ccb_type_is_integer_name(type_name) || ccb_type_is_ptr_name(type_name)) &&
                        ((ccb_const_is_integer_value(&rhs_const, 0ULL) &&
                            (strcmp(op_name, "add") == 0 || strcmp(op_name, "sub") == 0 ||
                             strcmp(op_name, "or") == 0 || strcmp(op_name, "xor") == 0 ||
                             strcmp(op_name, "shl") == 0 || strcmp(op_name, "shr") == 0)) ||
                         (ccb_const_is_integer_value(&rhs_const, 1ULL) &&
                            (strcmp(op_name, "mul") == 0 || strcmp(op_name, "div") == 0)) ||
                         (strcmp(op_name, "and") == 0 && ccb_const_is_all_ones(&rhs_const))))
        {
            (void)unsigned_hint;
            string_list_remove_range(body, i + 1, 2);
            if (i > 0)
                --i;
            continue;
        }

        CcbConstInfo lhs_const;
        if (ccb_parse_const_info(body->items[i], &lhs_const) &&
            ccb_parse_binop_info(body->items[i + 2], op_name, sizeof(op_name),
                                 type_name, sizeof(type_name), &unsigned_hint) &&
            strcmp(lhs_const.type, type_name) == 0 &&
            (ccb_type_is_integer_name(type_name) || ccb_type_is_ptr_name(type_name)) &&
                        ((ccb_const_is_integer_value(&lhs_const, 0ULL) &&
                            (strcmp(op_name, "add") == 0 || strcmp(op_name, "or") == 0 ||
                             strcmp(op_name, "xor") == 0 || strcmp(op_name, "mul") == 0)) ||
                         (strcmp(op_name, "and") == 0 && ccb_const_is_all_ones(&lhs_const))))
        {
            (void)unsigned_hint;
            string_list_remove_range(body, i + 2, 1);
            string_list_remove_range(body, i, 1);
            if (i > 0)
                --i;
            continue;
        }

        ++i;
    }

    i = 0;
    while (i + 1 < body->count)
    {
        char kind1[16], from1[16], to1[16];
        char kind2[16], from2[16], to2[16];
        if (!ccb_parse_convert_info(body->items[i], kind1, sizeof(kind1), from1, sizeof(from1), to1, sizeof(to1)) ||
            !ccb_parse_convert_info(body->items[i + 1], kind2, sizeof(kind2), from2, sizeof(from2), to2, sizeof(to2)))
        {
            if (ccb_parse_convert_info(body->items[i], kind1, sizeof(kind1), from1, sizeof(from1), to1, sizeof(to1)) &&
                strcmp(from1, to1) == 0)
            {
                string_list_remove_range(body, i, 1);
                if (i > 0)
                    --i;
                continue;
            }
            ++i;
            continue;
        }

        if (strcmp(from1, to1) == 0)
        {
            string_list_remove_range(body, i, 1);
            if (i > 0)
                --i;
            continue;
        }

        if (strcmp(kind1, "bitcast") == 0 && strcmp(kind2, "bitcast") == 0 &&
            strcmp(from1, to2) == 0 && strcmp(to1, from2) == 0)
        {
            string_list_remove_range(body, i, 2);
            if (i > 0)
                --i;
            continue;
        }

        ++i;
    }
}

static void ccb_opt_promote_local_values(CcbFunctionBuilder *fb)
{
    if (!fb || fb->local_count == 0)
        return;

    StringList *body = &fb->body;
    bool *promotable = (bool *)calloc(fb->local_count, sizeof(bool));
    char **known_lines = (char **)calloc(fb->local_count, sizeof(char *));
    if (!promotable || !known_lines)
    {
        free(promotable);
        free(known_lines);
        return;
    }

    for (size_t i = 0; i < fb->local_count; ++i)
        promotable[i] = true;

    for (size_t i = 0; i < body->count; ++i)
    {
        int local_idx = -1;
        if (ccb_parse_local_index(body->items[i], "addr_local", &local_idx) &&
            local_idx >= 0 && (size_t)local_idx < fb->local_count)
            promotable[local_idx] = false;
    }

    for (size_t i = 0; i + 1 < body->count; ++i)
    {
        int store_idx = -1;
        int load_idx = -1;
        if (!ccb_parse_local_index(body->items[i], "store_local", &store_idx) ||
            !ccb_parse_local_index(body->items[i + 1], "load_local", &load_idx) ||
            store_idx != load_idx || store_idx < 0 || (size_t)store_idx >= fb->local_count ||
            !promotable[store_idx])
        {
            continue;
        }

        const char *type_name = ccb_local_type_name_by_index(fb, store_idx);
        if (!type_name)
            continue;
        if (!ccb_replace_linef(body, i, "  dup %s", type_name))
            continue;
        ccb_replace_linef(body, i + 1, "  store_local %d", store_idx);
    }

    for (size_t i = 0; i < body->count; ++i)
    {
        int local_idx = -1;
        if (ccb_parse_local_index(body->items[i], "load_local", &local_idx) &&
            local_idx >= 0 && (size_t)local_idx < fb->local_count && promotable[local_idx] && known_lines[local_idx])
        {
            ccb_replace_line_copy(body, i, known_lines[local_idx]);
            continue;
        }

        if (ccb_parse_local_index(body->items[i], "store_local", &local_idx) &&
            local_idx >= 0 && (size_t)local_idx < fb->local_count)
        {
            free(known_lines[local_idx]);
            known_lines[local_idx] = NULL;
            if (promotable[local_idx] && i > 0 && ccb_line_is_clonable_local_value(body->items[i - 1]))
                known_lines[local_idx] = xstrdup(body->items[i - 1]);
            continue;
        }

        if (ccb_instruction_is_control_barrier(body->items[i]))
        {
            for (size_t j = 0; j < fb->local_count; ++j)
            {
                free(known_lines[j]);
                known_lines[j] = NULL;
            }
        }
    }

    for (size_t i = 0; i < fb->local_count; ++i)
        free(known_lines[i]);
    free(known_lines);
    free(promotable);
}

static void ccb_opt_fold_const_or_store_chains(CcbFunctionBuilder *fb)
{
    if (!fb)
        return;

    StringList *body = &fb->body;
    size_t i = 0;
    while (i < body->count)
    {
        CcbConstStoreBlock base;
        if (!ccb_parse_const_store_block(body, i, &base))
        {
            ++i;
            continue;
        }

        unsigned long long combined = base.value.u;
        size_t cursor = base.end;
        size_t folded_blocks = 0;

        while (cursor < body->count)
        {
            CcbOrStoreBlock rmw;
            if (!ccb_parse_or_store_block(body, cursor,
                                          base.addr_start, base.addr_end,
                                          base.value.type, &rmw))
                break;

            combined |= rmw.or_value;
            cursor = rmw.end;
            ++folded_blocks;
        }

        if (folded_blocks == 0)
        {
            i = base.end;
            continue;
        }

        unsigned long long masked = combined & ccb_mask_for_width(base.value.width);
        long long signed_value = ccb_type_is_signed_name(base.value.type)
                                     ? ccb_sign_extend(masked, base.value.width)
                                     : (long long)masked;
        char const_line[128] = {0};
        ccb_format_const_line(const_line, sizeof(const_line), base.value.type, masked, signed_value);
        if (!ccb_replace_linef(body, base.const_line, "%s", const_line))
        {
            i = cursor;
            continue;
        }

        string_list_remove_range(body, base.end, cursor - base.end);
        i = base.end;
    }
}

static bool ccb_parse_addr_expr_block(const StringList *body, size_t start,
                                      size_t *out_begin, size_t *out_end)
{
    if (!body || !out_begin || !out_end || start >= body->count)
        return false;

    size_t p = ccb_skip_loc_directives(body, start);
    if (p >= body->count)
        return false;

    size_t begin = p;
    size_t steps = 0;
    while (p < body->count)
    {
        const char *line = ccb_trim_leading_ws(body->items[p]);
        if (!line)
            return false;

        bool addr_step = strncmp(line, "addr_global ", 12) == 0 ||
                         strncmp(line, "addr_local ", 11) == 0 ||
                         strncmp(line, "addr_param ", 11) == 0 ||
                         strncmp(line, "load_local ", 11) == 0 ||
                         strncmp(line, "load_param ", 11) == 0 ||
                         strncmp(line, "const i64 ", 10) == 0 ||
                         strncmp(line, "const i32 ", 10) == 0 ||
                         strcmp(line, "convert bitcast ptr i64") == 0 ||
                         strcmp(line, "convert sext i32 i64") == 0 ||
                         strcmp(line, "convert zext i32 i64") == 0 ||
                         strcmp(line, "binop add i64") == 0 ||
                         strcmp(line, "convert bitcast i64 ptr") == 0;
        if (!addr_step)
            return false;

        ++steps;
        if (steps > 24)
            return false;

        if (strcmp(line, "convert bitcast i64 ptr") == 0)
        {
            *out_begin = begin;
            *out_end = ccb_skip_loc_directives(body, p + 1);
            return true;
        }

        p = ccb_skip_loc_directives(body, p + 1);
    }

    return false;
}

static bool ccb_same_addr_expr(const StringList *body,
                               size_t a_begin, size_t a_end,
                               size_t b_begin, size_t b_end)
{
    if (!body)
        return false;

    size_t a = ccb_skip_loc_directives(body, a_begin);
    size_t b = ccb_skip_loc_directives(body, b_begin);
    while (a < a_end && b < b_end)
    {
        if (!ccb_trimmed_lines_equal(body->items[a], body->items[b]))
            return false;
        a = ccb_skip_loc_directives(body, a + 1);
        b = ccb_skip_loc_directives(body, b + 1);
    }
    return a >= a_end && b >= b_end;
}

static bool ccb_parse_const_store_after_addr(const StringList *body, size_t start,
                                             CcbConstInfo *out_const, size_t *out_end)
{
    if (!body || !out_const || !out_end)
        return false;

    size_t p = ccb_skip_loc_directives(body, start);
    if (p >= body->count || !ccb_parse_const_info(body->items[p], out_const))
        return false;

    p = ccb_skip_loc_directives(body, p + 1);
    if (p >= body->count)
        return false;

    char expected[64] = {0};
    snprintf(expected, sizeof(expected), "store_indirect %s", out_const->type);
    if (!ccb_is_exact_line(body->items[p], expected))
        return false;

    *out_end = p + 1;
    return true;
}

static bool ccb_parse_dup_or_store_after_addr(const StringList *body, size_t start,
                                              const char *type_name,
                                              unsigned long long *out_or_value,
                                              size_t *out_end)
{
    if (!body || !type_name || !out_or_value || !out_end)
        return false;

    size_t p = ccb_skip_loc_directives(body, start);
    if (p >= body->count || !ccb_is_prefix_line(body->items[p], "dup"))
        return false;

    p = ccb_skip_loc_directives(body, p + 1);
    char expected[64] = {0};
    snprintf(expected, sizeof(expected), "load_indirect %s", type_name);
    if (p >= body->count || !ccb_is_exact_line(body->items[p], expected))
        return false;

    p = ccb_skip_loc_directives(body, p + 1);
    CcbConstInfo cinfo;
    if (p >= body->count || !ccb_parse_const_info(body->items[p], &cinfo) || strcmp(cinfo.type, type_name) != 0)
        return false;

    p = ccb_skip_loc_directives(body, p + 1);
    char op_name[16] = {0};
    char op_type[16] = {0};
    bool unsigned_hint = false;
    if (p >= body->count ||
        !ccb_parse_binop_info(body->items[p], op_name, sizeof(op_name), op_type, sizeof(op_type), &unsigned_hint) ||
        strcmp(op_name, "or") != 0 || strcmp(op_type, type_name) != 0)
        return false;
    (void)unsigned_hint;

    p = ccb_skip_loc_directives(body, p + 1);
    snprintf(expected, sizeof(expected), "store_indirect %s", type_name);
    if (p >= body->count || !ccb_is_exact_line(body->items[p], expected))
        return false;

    *out_or_value = cinfo.u;
    *out_end = p + 1;
    return true;
}

static void ccb_opt_fold_dup_rmw_or_chains(CcbFunctionBuilder *fb)
{
    if (!fb)
        return;

    StringList *body = &fb->body;
    size_t i = 0;
    while (i < body->count)
    {
        size_t addr_begin = 0;
        size_t addr_end = 0;
        if (!ccb_parse_addr_expr_block(body, i, &addr_begin, &addr_end))
        {
            ++i;
            continue;
        }

        CcbConstInfo base_const;
        size_t base_end = 0;
        if (!ccb_parse_const_store_after_addr(body, addr_end, &base_const, &base_end))
        {
            i = addr_end;
            continue;
        }

        unsigned long long combined = base_const.u;
        size_t cursor = base_end;
        size_t folds = 0;

        while (cursor < body->count)
        {
            size_t next_addr_begin = 0;
            size_t next_addr_end = 0;
            if (!ccb_parse_addr_expr_block(body, cursor, &next_addr_begin, &next_addr_end))
                break;
            if (!ccb_same_addr_expr(body, addr_begin, addr_end, next_addr_begin, next_addr_end))
                break;

            unsigned long long or_value = 0;
            size_t rmw_end = 0;
            if (!ccb_parse_dup_or_store_after_addr(body, next_addr_end, base_const.type, &or_value, &rmw_end))
                break;

            combined |= or_value;
            cursor = rmw_end;
            ++folds;
        }

        if (folds == 0)
        {
            i = base_end;
            continue;
        }

        unsigned long long masked = combined & ccb_mask_for_width(base_const.width);
        long long signed_value = ccb_type_is_signed_name(base_const.type)
                                     ? ccb_sign_extend(masked, base_const.width)
                                     : (long long)masked;
        char const_line[128] = {0};
        ccb_format_const_line(const_line, sizeof(const_line), base_const.type, masked, signed_value);
        ccb_replace_linef(body, addr_end, "%s", const_line);

        string_list_remove_range(body, base_end, cursor - base_end);
        i = base_end;
    }
}

static void ccb_opt_pack_byte_store_runs(CcbFunctionBuilder *fb)
{
    if (!fb)
        return;

    StringList *body = &fb->body;
    size_t i = 0;
    while (i < body->count)
    {
        CcbByteStorePattern p0;
        if (!ccb_parse_byte_store_pattern(body, i, &p0))
        {
            ++i;
            continue;
        }

        CcbByteStorePattern p1;
        if (!ccb_parse_byte_store_pattern(body, p0.end, &p1) ||
            strcmp(p0.load_op, p1.load_op) != 0 ||
            p0.load_index != p1.load_index ||
            p1.offset != p0.offset + 1)
        {
            i = p0.end;
            continue;
        }

        CcbByteStorePattern p2;
        CcbByteStorePattern p3;
        bool have_four = ccb_parse_byte_store_pattern(body, p1.end, &p2) &&
                         ccb_parse_byte_store_pattern(body, p2.end, &p3) &&
                         strcmp(p0.load_op, p2.load_op) == 0 &&
                         strcmp(p0.load_op, p3.load_op) == 0 &&
                         p0.load_index == p2.load_index &&
                         p0.load_index == p3.load_index &&
                         p2.offset == p0.offset + 2 &&
                         p3.offset == p0.offset + 3;

        if (have_four)
        {
            uint32_t packed = ((uint32_t)p0.byte_value) |
                              ((uint32_t)p1.byte_value << 8) |
                              ((uint32_t)p2.byte_value << 16) |
                              ((uint32_t)p3.byte_value << 24);
            int32_t packed_i32 = (int32_t)packed;
            ccb_replace_linef(body, p0.value_line, "  const i32 %d", (int)packed_i32);
            ccb_replace_linef(body, p0.store_line, "  store_indirect i32");

            string_list_remove_range(body, p3.start, p3.end - p3.start);
            string_list_remove_range(body, p2.start, p2.end - p2.start);
            string_list_remove_range(body, p1.start, p1.end - p1.start);
            i = p0.end;
            continue;
        }

        uint16_t packed16 = (uint16_t)(((uint16_t)p0.byte_value) |
                                       ((uint16_t)p1.byte_value << 8));
        int16_t packed_i16 = (int16_t)packed16;
        ccb_replace_linef(body, p0.value_line, "  const i16 %d", (int)packed_i16);
        ccb_replace_linef(body, p0.store_line, "  store_indirect i16");
        string_list_remove_range(body, p1.start, p1.end - p1.start);
        i = p0.end;
    }
}

static void ccb_opt_remove_overwritten_indirect_stores(CcbFunctionBuilder *fb)
{
    if (!fb)
        return;

    StringList *body = &fb->body;
    size_t i = 0;
    while (i < body->count)
    {
        CcbIndirectStorePattern src;
        if (!ccb_parse_indirect_store_pattern(body, i, &src))
        {
            ++i;
            continue;
        }

        bool removed = false;
        size_t j = src.end;
        while (j < body->count)
        {
            size_t probe = ccb_skip_loc_directives(body, j);
            if (probe >= body->count)
                break;

            const char *line = ccb_trim_leading_ws(body->items[probe]);
            if (!line)
                break;

            if (strncmp(line, "label ", 6) == 0 ||
                strncmp(line, "jump ", 5) == 0 ||
                strncmp(line, "branch ", 7) == 0 ||
                strncmp(line, "ret", 3) == 0 ||
                strncmp(line, "call ", 5) == 0 ||
                strncmp(line, "call_indirect ", 14) == 0 ||
                strncmp(line, "load_indirect ", 14) == 0)
            {
                break;
            }

            if (strcmp(src.load_op, "load_local") == 0)
            {
                int idx = -1;
                if (ccb_parse_local_index(line, "store_local", &idx) && idx == src.load_index)
                    break;
                if (ccb_parse_local_index(line, "addr_local", &idx) && idx == src.load_index)
                    break;
            }

            CcbIndirectStorePattern dst;
            if (ccb_parse_indirect_store_pattern(body, j, &dst))
            {
                if (strcmp(src.load_op, dst.load_op) == 0 &&
                    src.load_index == dst.load_index)
                {
                    int src_begin = src.offset;
                    int src_end = src.offset + src.width_bytes;
                    int dst_begin = dst.offset;
                    int dst_end = dst.offset + dst.width_bytes;
                    if (dst_begin <= src_begin && dst_end >= src_end)
                    {
                        string_list_remove_range(body, src.start, src.end - src.start);
                        removed = true;
                        if (src.start > 0)
                            i = src.start - 1;
                        else
                            i = 0;
                        break;
                    }
                }
                j = dst.end;
                continue;
            }

            ++j;
        }

        if (!removed)
            i = src.end;
    }
}

static void ccb_opt_fold_zero_init_memset(CcbFunctionBuilder *fb)
{
    if (!fb || !fb->module)
        return;

    const char *memset_sym = NULL;
    bool use_cert_memset = false;
    if (ccb_module_has_function(fb->module, "__cert__memset"))
    {
        memset_sym = "__cert__memset";
        use_cert_memset = true;
    }
    else if (ccb_module_has_extern(fb->module, "__cert__memset"))
    {
        memset_sym = "__cert__memset";
        use_cert_memset = true;
    }
    else if (ccb_module_has_extern(fb->module, "Std_Memory_memset_ptr_to_void_u8_i32"))
    {
        memset_sym = "Std_Memory_memset_ptr_to_void_u8_i32";
    }
    else
    {
        if (!ccb_module_appendf(fb->module,
                                ".extern __cert__memset params=(ptr,i32,u64) returns=ptr"))
            return;
        memset_sym = "__cert__memset";
        use_cert_memset = true;
    }

    StringList *body = &fb->body;
    size_t i = 0;
    while (i + 7 < body->count)
    {
        const char *load_op = NULL;
        int load_index = -1;
        int offset = 0;
        size_t stride = 0;
        if (!ccb_parse_zero_store_at_offset(body, i, &load_op, &load_index, &offset, &stride))
        {
            ++i;
            continue;
        }
        if (offset != 0)
        {
            ++i;
            continue;
        }

        size_t run = 1;
        size_t cursor = i + stride;
        while (cursor + 6 < body->count)
        {
            const char *next_load_op = NULL;
            int next_index = -1;
            int next_offset = 0;
            size_t next_stride = 0;
            if (!ccb_parse_zero_store_at_offset(body, cursor, &next_load_op,
                                                &next_index, &next_offset, &next_stride))
                break;
            if (next_stride != stride)
                break;
            if (next_index != load_index || strcmp(next_load_op, load_op) != 0)
                break;
            if (next_offset != (int)run)
                break;
            run++;
            cursor += stride;
        }

        if (run < 8)
        {
            ++i;
            continue;
        }

        if (!ccb_replace_linef(body, i + 0, "  %s %d", load_op, load_index))
        {
            ++i;
            continue;
        }
        if (!ccb_replace_linef(body, i + 1, "  const %s 0",
                               use_cert_memset ? "i32" : "i8"))
        {
            ++i;
            continue;
        }
        if (!ccb_replace_linef(body, i + 2, "  const %s %zu",
                               use_cert_memset ? "u64" : "i32", run))
        {
            ++i;
            continue;
        }
        if (!ccb_replace_linef(body, i + 3,
                               "  call %s ptr (ptr,%s,%s)", memset_sym,
                               use_cert_memset ? "i32" : "u8",
                               use_cert_memset ? "u64" : "i32"))
        {
            ++i;
            continue;
        }
        if (!ccb_replace_linef(body, i + 4, "  drop ptr"))
        {
            ++i;
            continue;
        }

        size_t total = cursor - i;
        if (total > 5)
            string_list_remove_range(body, i + 5, total - 5);
        if (i > 0)
            --i;
    }
}

static void ccb_opt_fold_string_copy_loop(CcbFunctionBuilder *fb)
{
    if (!fb || !fb->module)
        return;

    StringList *body = &fb->body;
    if (body->count < 53)
        return;

    size_t cursor = 0;
    if (!ccb_is_exact_line(body->items[cursor++], "const i32 0"))
        return;
    if (!ccb_is_exact_line(body->items[cursor++], "store_local 0"))
        return;

    char cond_label[64] = {0};
    char body_label[64] = {0};
    char end_label[64] = {0};
    if (!ccb_parse_label_name(body->items[cursor++], cond_label, sizeof(cond_label)))
        return;

    if (!ccb_is_exact_line(body->items[cursor++], "load_param 0") ||
        !ccb_is_exact_line(body->items[cursor++], "convert bitcast ptr i64") ||
        !ccb_is_exact_line(body->items[cursor++], "load_local 0") ||
        !ccb_is_exact_line(body->items[cursor++], "convert sext i32 i64") ||
        !ccb_is_exact_line(body->items[cursor++], "binop add i64") ||
        !ccb_is_exact_line(body->items[cursor++], "convert bitcast i64 ptr") ||
        !ccb_is_exact_line(body->items[cursor++], "load_indirect i8") ||
        !ccb_is_exact_line(body->items[cursor++], "convert sext i8 i32") ||
        !ccb_is_exact_line(body->items[cursor++], "const i32 0") ||
        !ccb_is_exact_line(body->items[cursor++], "compare ne i32") ||
        !ccb_is_exact_line(body->items[cursor++], "const i1 0") ||
        !ccb_is_exact_line(body->items[cursor++], "compare ne i1"))
        return;

    char branch_true[64] = {0};
    char branch_false[64] = {0};
    if (!ccb_parse_branch_targets(body->items[cursor++], branch_true,
                                  sizeof(branch_true), branch_false,
                                  sizeof(branch_false)))
        return;

    if (!ccb_parse_label_name(body->items[cursor++], body_label, sizeof(body_label)))
        return;
    if (!ccb_is_exact_line(body->items[cursor++], "load_param 1") ||
        !ccb_is_exact_line(body->items[cursor++], "convert bitcast ptr i64") ||
        !ccb_is_exact_line(body->items[cursor++], "load_local 0") ||
        !ccb_is_exact_line(body->items[cursor++], "convert sext i32 i64") ||
        !ccb_is_exact_line(body->items[cursor++], "binop add i64") ||
        !ccb_is_exact_line(body->items[cursor++], "convert bitcast i64 ptr") ||
        !ccb_is_exact_line(body->items[cursor++], "load_param 0") ||
        !ccb_is_exact_line(body->items[cursor++], "convert bitcast ptr i64") ||
        !ccb_is_exact_line(body->items[cursor++], "load_local 0") ||
        !ccb_is_exact_line(body->items[cursor++], "convert sext i32 i64") ||
        !ccb_is_exact_line(body->items[cursor++], "binop add i64") ||
        !ccb_is_exact_line(body->items[cursor++], "convert bitcast i64 ptr") ||
        !ccb_is_exact_line(body->items[cursor++], "load_indirect i8") ||
        !ccb_is_exact_line(body->items[cursor++], "store_local 1") ||
        !ccb_is_exact_line(body->items[cursor++], "load_local 1") ||
        !ccb_is_exact_line(body->items[cursor++], "store_indirect i8"))
        return;

    if (cursor + 1 < body->count &&
        ccb_is_exact_line(body->items[cursor], "load_local 1") &&
        ccb_is_exact_line(body->items[cursor + 1], "drop i8"))
        cursor += 2;

    if (!ccb_is_exact_line(body->items[cursor++], "load_local 0") ||
        !ccb_is_exact_line(body->items[cursor++], "store_local 2") ||
        !ccb_is_exact_line(body->items[cursor++], "load_local 0") ||
        !ccb_is_exact_line(body->items[cursor++], "const i32 1") ||
        !ccb_is_exact_line(body->items[cursor++], "binop add i32") ||
        !ccb_is_exact_line(body->items[cursor++], "store_local 0"))
        return;

    if (cursor + 1 < body->count &&
        ccb_is_exact_line(body->items[cursor], "load_local 2") &&
        ccb_is_exact_line(body->items[cursor + 1], "drop i32"))
        cursor += 2;

    if (!ccb_is_prefix_line(body->items[cursor++], "jump "))
        return;

    if (!ccb_parse_label_name(body->items[cursor++], end_label, sizeof(end_label)))
        return;
    if (!ccb_is_exact_line(body->items[cursor++], "load_param 1") ||
        !ccb_is_exact_line(body->items[cursor++], "convert bitcast ptr i64") ||
        !ccb_is_exact_line(body->items[cursor++], "load_local 0") ||
        !ccb_is_exact_line(body->items[cursor++], "convert sext i32 i64") ||
        !ccb_is_exact_line(body->items[cursor++], "binop add i64") ||
        !ccb_is_exact_line(body->items[cursor++], "convert bitcast i64 ptr") ||
        !ccb_is_exact_line(body->items[cursor++], "const i8 0") ||
        !ccb_is_exact_line(body->items[cursor++], "store_local 3") ||
        !ccb_is_exact_line(body->items[cursor++], "load_local 3") ||
        !ccb_is_exact_line(body->items[cursor++], "store_indirect i8") ||
        !ccb_is_exact_line(body->items[cursor++], "load_local 0") ||
        !ccb_is_exact_line(body->items[cursor++], "ret"))
        return;

    if (strcmp(branch_true, body_label) != 0 || strcmp(branch_false, end_label) != 0)
        return;

    if (!ccb_module_has_function(fb->module, "__cert__strlen") &&
        !ccb_module_has_extern(fb->module, "__cert__strlen"))
    {
        if (!ccb_module_appendf(fb->module,
                                ".extern __cert__strlen params=(ptr,ptr,u64,ptr) returns=u64"))
            return;
    }
    if (!ccb_module_has_function(fb->module, "__cert__memcpy") &&
        !ccb_module_has_extern(fb->module, "__cert__memcpy"))
    {
        if (!ccb_module_appendf(fb->module,
                                ".extern __cert__memcpy params=(ptr,ptr,u64) returns=ptr"))
            return;
    }

    string_list_remove_range(body, 0, body->count);
    string_list_append(body, "  load_param 0");
    string_list_append(body, "  const ptr null");
    string_list_append(body, "  const u64 0");
    string_list_append(body, "  const ptr null");
    string_list_append(body, "  call __cert__strlen u64 (ptr,ptr,u64,ptr)");
    string_list_append(body, "  convert trunc u64 i32");
    string_list_append(body, "  store_local 0");
    string_list_append(body, "  load_param 1");
    string_list_append(body, "  load_param 0");
    string_list_append(body, "  load_local 0");
    string_list_append(body, "  convert zext i32 u64");
    string_list_append(body, "  const u64 1");
    string_list_append(body, "  binop add u64");
    string_list_append(body, "  call __cert__memcpy ptr (ptr,ptr,u64)");
    string_list_append(body, "  drop ptr");
    string_list_append(body, "  load_local 0");
    string_list_append(body, "  ret");
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

static void ccb_opt_simplify_store_load_store(CcbFunctionBuilder *fb)
{
    if (!fb)
        return;

    StringList *body = &fb->body;
    size_t i = 0;
    while (i + 2 < body->count)
    {
        int store_idx = -1;
        int load_idx = -1;
        if (!ccb_parse_local_index(body->items[i], "store_local", &store_idx) ||
            !ccb_parse_local_index(body->items[i + 1], "load_local", &load_idx) ||
            store_idx != load_idx)
        {
            ++i;
            continue;
        }

        const char *line2 = body->items[i + 2];
        if (!ccb_is_prefix_line(line2, "store_indirect "))
        {
            ++i;
            continue;
        }

        int used_later = 0;
        for (size_t j = i + 2; j < body->count; ++j)
        {
            int idx = -1;
            if (ccb_parse_local_index(body->items[j], "load_local", &idx) && idx == store_idx)
            {
                used_later = 1;
                break;
            }
        }
        if (used_later)
        {
            ++i;
            continue;
        }

        string_list_remove_range(body, i, 2);
        if (i > 0)
            --i;
    }
}

static void ccb_opt_inline_const_str_locals(CcbFunctionBuilder *fb)
{
    if (!fb)
        return;

    StringList *body = &fb->body;
    size_t i = 0;
    while (i + 1 < body->count)
    {
        if (!ccb_is_const_str_line(body->items[i]))
        {
            ++i;
            continue;
        }

        int store_idx = -1;
        if (!ccb_parse_local_index(body->items[i + 1], "store_local", &store_idx))
        {
            ++i;
            continue;
        }

        size_t use_index = (size_t)-1;
        int blocked = 0;
        for (size_t j = i + 2; j < body->count; ++j)
        {
            int idx = -1;
            if (ccb_parse_local_index(body->items[j], "store_local", &idx) && idx == store_idx)
            {
                blocked = 1;
                break;
            }
            if (ccb_parse_local_index(body->items[j], "load_local", &idx) && idx == store_idx)
            {
                use_index = j;
                break;
            }
        }

        if (blocked || use_index == (size_t)-1)
        {
            ++i;
            continue;
        }

        int extra_use = 0;
        for (size_t j = use_index + 1; j < body->count; ++j)
        {
            int idx = -1;
            if (ccb_parse_local_index(body->items[j], "load_local", &idx) && idx == store_idx)
            {
                extra_use = 1;
                break;
            }
        }
        if (extra_use)
        {
            ++i;
            continue;
        }

        free(body->items[use_index]);
        body->items[use_index] = xstrdup(body->items[i]);

        string_list_remove_range(body, i, 2);
        if (use_index > i)
            use_index -= 2;
        if (i > 0)
            --i;
    }
}

static bool ccb_instruction_is_control_barrier(const char *line)
{
    line = ccb_trim_leading_ws(line);
    if (!line || *line == '\0')
        return true;
    return strncmp(line, "label ", 6) == 0 ||
           strncmp(line, "jump ", 5) == 0 ||
           strncmp(line, "jump_indirect", 13) == 0 ||
           strncmp(line, "branch ", 7) == 0 ||
           strncmp(line, "ret", 3) == 0;
}

static bool ccb_instruction_is_terminator(const char *line)
{
    line = ccb_trim_leading_ws(line);
    if (!line || *line == '\0')
        return false;
    return strncmp(line, "jump ", 5) == 0 ||
           strncmp(line, "jump_indirect", 13) == 0 ||
           strncmp(line, "branch ", 7) == 0 ||
           strncmp(line, "ret", 3) == 0;
}

static void ccb_opt_propagate_local_values(CcbFunctionBuilder *fb)
{
    if (!fb)
        return;

    StringList *body = &fb->body;
    if (body->count < 2)
        return;

    for (size_t i = 0; i + 1 < body->count; ++i)
    {
        CcbConstInfo cinfo;
        int const_store_idx = -1;
        if (ccb_parse_const_info(body->items[i], &cinfo) &&
            ccb_parse_local_index(body->items[i + 1], "store_local", &const_store_idx))
        {
            char const_line[128];
            ccb_format_const_line(const_line, sizeof(const_line), cinfo.type, cinfo.u, cinfo.s);
            for (size_t j = i + 2; j < body->count; ++j)
            {
                const char *line = body->items[j];
                if (ccb_instruction_is_control_barrier(line))
                    break;

                int idx = -1;
                if (ccb_parse_local_index(line, "store_local", &idx) && idx == const_store_idx)
                    break;
                if (ccb_parse_local_index(line, "addr_local", &idx) && idx == const_store_idx)
                    break;
                if (ccb_parse_local_index(line, "load_local", &idx) && idx == const_store_idx)
                    ccb_replace_linef(body, j, "%s", const_line);
            }
        }

        int src_idx = -1;
        int dst_idx = -1;
        if (!ccb_parse_local_index(body->items[i], "load_local", &src_idx) ||
            !ccb_parse_local_index(body->items[i + 1], "store_local", &dst_idx) ||
            src_idx == dst_idx)
        {
            continue;
        }

        for (size_t j = i + 2; j < body->count; ++j)
        {
            const char *line = body->items[j];
            if (ccb_instruction_is_control_barrier(line))
                break;

            int idx = -1;
            if (ccb_parse_local_index(line, "store_local", &idx) && (idx == src_idx || idx == dst_idx))
                break;
            if (ccb_parse_local_index(line, "addr_local", &idx) && (idx == src_idx || idx == dst_idx))
                break;
            if (ccb_parse_local_index(line, "load_local", &idx) && idx == dst_idx)
                ccb_replace_linef(body, j, "  load_local %d", src_idx);
        }
    }
}

static void ccb_opt_remove_dead_local_stores(CcbFunctionBuilder *fb)
{
    if (!fb)
        return;

    StringList *body = &fb->body;
    size_t i = 0;
    while (i < body->count)
    {
        int store_idx = -1;
        if (!ccb_parse_local_index(body->items[i], "store_local", &store_idx))
        {
            ++i;
            continue;
        }

        bool used = false;
        bool overwritten = false;
        bool hit_barrier = false;

        for (size_t j = i + 1; j < body->count; ++j)
        {
            const char *line = body->items[j];
            const char *trimmed = ccb_trim_leading_ws(line);
            if (trimmed && (strncmp(trimmed, "label ", 6) == 0 ||
                            strncmp(trimmed, "jump ", 5) == 0 ||
                            strncmp(trimmed, "branch ", 7) == 0))
            {
                hit_barrier = true;
                break;
            }
            if (trimmed && strncmp(trimmed, "ret", 3) == 0)
                break;

            int idx = -1;
            if ((ccb_parse_local_index(line, "load_local", &idx) ||
                 ccb_parse_local_index(line, "addr_local", &idx)) &&
                idx == store_idx)
            {
                used = true;
                break;
            }

            if (ccb_parse_local_index(line, "store_local", &idx) && idx == store_idx)
            {
                overwritten = true;
                break;
            }
        }

        if (used)
        {
            ++i;
            continue;
        }

        if (hit_barrier && !overwritten)
        {
            ++i;
            continue;
        }

        size_t remove_index = i;
        size_t remove_count = 1;
        if (i > 0 && ccb_instruction_is_pure(body->items[i - 1]))
        {
            remove_index = i - 1;
            remove_count = 2;
        }
        string_list_remove_range(body, remove_index, remove_count);
        if (remove_index > 0)
            --i;
    }
}

static void ccb_remap_local_indices(StringList *list, const size_t *map, size_t old_count)
{
    if (!list || !map)
        return;

    for (size_t i = 0; i < list->count; ++i)
    {
        int idx = -1;
        if (ccb_parse_local_index(list->items[i], "load_local", &idx))
        {
            if (idx >= 0 && (size_t)idx < old_count && map[idx] != SIZE_MAX)
                ccb_replace_linef(list, i, "  load_local %zu", map[idx]);
            continue;
        }
        if (ccb_parse_local_index(list->items[i], "store_local", &idx))
        {
            if (idx >= 0 && (size_t)idx < old_count && map[idx] != SIZE_MAX)
                ccb_replace_linef(list, i, "  store_local %zu", map[idx]);
            continue;
        }
        if (ccb_parse_local_index(list->items[i], "addr_local", &idx))
        {
            if (idx >= 0 && (size_t)idx < old_count && map[idx] != SIZE_MAX)
                ccb_replace_linef(list, i, "  addr_local %zu", map[idx]);
        }
    }
}

static void ccb_opt_remove_unused_local_slots(CcbFunctionBuilder *fb)
{
    if (!fb || fb->local_count == 0)
        return;

    size_t old_count = fb->local_count;
    bool *used = (bool *)calloc(old_count, sizeof(bool));
    if (!used)
        return;

    StringList *lists[2] = {&fb->prologue, &fb->body};
    for (size_t list_index = 0; list_index < 2; ++list_index)
    {
        StringList *list = lists[list_index];
        for (size_t i = 0; i < list->count; ++i)
        {
            int idx = -1;
            if ((ccb_parse_local_index(list->items[i], "load_local", &idx) ||
                 ccb_parse_local_index(list->items[i], "store_local", &idx) ||
                 ccb_parse_local_index(list->items[i], "addr_local", &idx)) &&
                idx >= 0 && (size_t)idx < old_count)
            {
                used[idx] = true;
            }
        }
    }

    size_t new_count = 0;
    for (size_t i = 0; i < old_count; ++i)
    {
        if (used[i])
            ++new_count;
    }

    if (new_count == old_count)
    {
        free(used);
        return;
    }

    size_t *map = (size_t *)malloc(old_count * sizeof(size_t));
    if (!map)
    {
        free(used);
        return;
    }

    size_t next_index = 0;
    for (size_t i = 0; i < old_count; ++i)
    {
        if (used[i])
            map[i] = next_index++;
        else
            map[i] = SIZE_MAX;
    }

    ccb_remap_local_indices(&fb->prologue, map, old_count);
    ccb_remap_local_indices(&fb->body, map, old_count);

    size_t write_index = 0;
    for (size_t i = 0; i < fb->locals_count; ++i)
    {
        CcbLocal local = fb->locals[i];
        if (!local.is_param)
        {
            if (local.index < 0 || (size_t)local.index >= old_count || map[local.index] == SIZE_MAX)
                continue;
            local.index = (int)map[local.index];
        }
        fb->locals[write_index++] = local;
    }

    fb->locals_count = write_index;
    fb->local_count = new_count;

    free(map);
    free(used);
}

static void ccb_opt_simplify_const_branches(CcbFunctionBuilder *fb)
{
    if (!fb)
        return;

    StringList *body = &fb->body;
    size_t i = 0;
    while (i + 1 < body->count)
    {
        CcbConstInfo cond;
        if (!ccb_parse_const_info(body->items[i], &cond) || strcmp(cond.type, "i1") != 0)
        {
            ++i;
            continue;
        }

        char true_label[64] = {0};
        char false_label[64] = {0};
        if (!ccb_parse_branch_targets(body->items[i + 1], true_label, sizeof(true_label),
                                      false_label, sizeof(false_label)))
        {
            ++i;
            continue;
        }

        const char *dest = cond.u ? true_label : false_label;
        if (!ccb_replace_linef(body, i + 1, "  jump %s", dest))
        {
            ++i;
            continue;
        }

        string_list_remove_range(body, i, 1);
        if (i > 0)
            --i;
    }
}

static void ccb_opt_simplify_bool_normalization(CcbFunctionBuilder *fb)
{
    if (!fb)
        return;

    StringList *body = &fb->body;
    size_t i = 0;
    while (i + 1 < body->count)
    {
        if (i == 0)
        {
            ++i;
            continue;
        }

        CcbConstInfo cinfo;
        if (!ccb_parse_const_info(body->items[i], &cinfo) || strcmp(cinfo.type, "i1") != 0 || cinfo.u != 0)
        {
            ++i;
            continue;
        }

        char op[16] = {0};
        char type_name[16] = {0};
        bool is_unsigned = false;
        if (!ccb_parse_compare_info(body->items[i + 1], op, sizeof(op), type_name,
                                    sizeof(type_name), &is_unsigned) ||
            is_unsigned || strcmp(op, "ne") != 0 || strcmp(type_name, "i1") != 0)
        {
            ++i;
            continue;
        }

        string_list_remove_range(body, i, 2);
        if (i > 0)
            --i;
    }
}

static void ccb_opt_remove_unreachable_fallthrough(CcbFunctionBuilder *fb)
{
    if (!fb)
        return;

    StringList *body = &fb->body;
    if (body->count == 0)
        return;

    StringList referenced;
    string_list_init(&referenced);

    for (size_t i = 0; i < body->count; ++i)
    {
        char jump_label[64] = {0};
        if (ccb_parse_jump_target(body->items[i], jump_label, sizeof(jump_label)))
        {
            if (!string_list_contains(&referenced, jump_label))
                string_list_append(&referenced, jump_label);
            continue;
        }

        char true_label[64] = {0};
        char false_label[64] = {0};
        if (!ccb_parse_branch_targets(body->items[i], true_label, sizeof(true_label),
                                      false_label, sizeof(false_label)))
            continue;

        if (!string_list_contains(&referenced, true_label))
            string_list_append(&referenced, true_label);
        if (!string_list_contains(&referenced, false_label))
            string_list_append(&referenced, false_label);
    }

    bool reachable = true;
    for (size_t i = 0; i < body->count;)
    {
        char label[64] = {0};
        if (ccb_parse_label_name(body->items[i], label, sizeof(label)))
        {
            bool has_inbound = string_list_contains(&referenced, label) != 0;
            if (!reachable && !has_inbound)
            {
                string_list_remove_range(body, i, 1);
                continue;
            }
            reachable = reachable || has_inbound;
            ++i;
            continue;
        }

        if (!reachable)
        {
            string_list_remove_range(body, i, 1);
            continue;
        }

        if (ccb_instruction_is_terminator(body->items[i]))
            reachable = false;
        ++i;
    }

    string_list_free(&referenced);
}

static bool ccb_rewrite_label_target(StringList *body, const char *from_label, const char *to_label)
{
    if (!body || !from_label || !to_label || strcmp(from_label, to_label) == 0)
        return false;

    bool changed = false;
    for (size_t i = 0; i < body->count; ++i)
    {
        char jump_label[64] = {0};
        if (ccb_parse_jump_target(body->items[i], jump_label, sizeof(jump_label)))
        {
            if (strcmp(jump_label, from_label) == 0)
            {
                if (ccb_replace_linef(body, i, "  jump %s", to_label))
                    changed = true;
            }
            continue;
        }

        char true_label[64] = {0};
        char false_label[64] = {0};
        if (!ccb_parse_branch_targets(body->items[i], true_label, sizeof(true_label),
                                      false_label, sizeof(false_label)))
            continue;

        bool rewrite_true = strcmp(true_label, from_label) == 0;
        bool rewrite_false = strcmp(false_label, from_label) == 0;
        if (!rewrite_true && !rewrite_false)
            continue;

        const char *new_true = rewrite_true ? to_label : true_label;
        const char *new_false = rewrite_false ? to_label : false_label;
        if (ccb_replace_linef(body, i, "  branch %s %s", new_true, new_false))
            changed = true;
    }

    return changed;
}

static void ccb_opt_merge_consecutive_labels(CcbFunctionBuilder *fb)
{
    if (!fb)
        return;

    StringList *body = &fb->body;
    if (body->count < 2)
        return;

    size_t i = 0;
    while (i + 1 < body->count)
    {
        char keep_label[64] = {0};
        char merge_label[64] = {0};
        if (!ccb_parse_label_name(body->items[i], keep_label, sizeof(keep_label)) ||
            !ccb_parse_label_name(body->items[i + 1], merge_label, sizeof(merge_label)))
        {
            ++i;
            continue;
        }

        ccb_rewrite_label_target(body, merge_label, keep_label);
        string_list_remove_range(body, i + 1, 1);
    }
}

static void ccb_opt_remove_dead_local_copies(CcbFunctionBuilder *fb)
{
    if (!fb)
        return;

    StringList *body = &fb->body;
    size_t i = 0;
    while (i + 1 < body->count)
    {
        int src_idx = -1;
        int dst_idx = -1;
        if (!ccb_parse_local_index(body->items[i], "load_local", &src_idx) ||
            !ccb_parse_local_index(body->items[i + 1], "store_local", &dst_idx) ||
            src_idx == dst_idx)
        {
            ++i;
            continue;
        }

        bool may_escape_block = false;
        bool used = false;
        bool killed = false;

        for (size_t j = i + 2; j < body->count; ++j)
        {
            const char *line = body->items[j];
            if (ccb_instruction_is_control_barrier(line))
            {
                const char *trimmed = ccb_trim_leading_ws(line);
                if (!trimmed || strncmp(trimmed, "ret", 3) != 0)
                    may_escape_block = true;
                break;
            }

            int idx = -1;
            if (ccb_parse_local_index(line, "load_local", &idx) && idx == dst_idx)
            {
                used = true;
                break;
            }
            if ((ccb_parse_local_index(line, "store_local", &idx) ||
                 ccb_parse_local_index(line, "addr_local", &idx)) &&
                idx == dst_idx)
            {
                killed = true;
                break;
            }
        }

        if (used || may_escape_block)
        {
            ++i;
            continue;
        }

        (void)killed;
        string_list_remove_range(body, i, 2);
        if (i > 0)
            --i;
    }
}

static void ccb_opt_simplify_addr_local_temp(CcbFunctionBuilder *fb)
{
    if (!fb)
        return;

    StringList *body = &fb->body;
    size_t i = 0;
    while (i + 2 < body->count)
    {
        int base_idx = -1;
        int temp_idx = -1;
        int load_idx = -1;
        if (!ccb_parse_local_index(body->items[i], "addr_local", &base_idx) ||
            !ccb_parse_local_index(body->items[i + 1], "store_local", &temp_idx) ||
            !ccb_parse_local_index(body->items[i + 2], "load_local", &load_idx) ||
            temp_idx != load_idx)
        {
            ++i;
            continue;
        }

        bool used_later = false;
        for (size_t j = i + 3; j < body->count; ++j)
        {
            const char *line = body->items[j];
            if (ccb_instruction_is_control_barrier(line))
                break;

            int idx = -1;
            if (ccb_parse_local_index(line, "load_local", &idx) && idx == temp_idx)
            {
                used_later = true;
                break;
            }
            if ((ccb_parse_local_index(line, "store_local", &idx) ||
                 ccb_parse_local_index(line, "addr_local", &idx)) &&
                idx == temp_idx)
                break;
        }

        if (used_later)
        {
            ++i;
            continue;
        }

        (void)base_idx;
        string_list_remove_range(body, i + 1, 2);
        if (i > 0)
            --i;
    }
}

static void ccb_opt_remove_redundant_jumps(CcbFunctionBuilder *fb)
{
    if (!fb)
        return;

    StringList *body = &fb->body;
    size_t i = 0;
    while (i + 1 < body->count)
    {
        char jump_label[64] = {0};
        char next_label[64] = {0};
        if (!ccb_parse_jump_target(body->items[i], jump_label, sizeof(jump_label)) ||
            !ccb_parse_label_name(body->items[i + 1], next_label, sizeof(next_label)) ||
            strcmp(jump_label, next_label) != 0)
        {
            ++i;
            continue;
        }

        string_list_remove_range(body, i, 1);
        if (i > 0)
            --i;
    }
}

static void ccb_opt_remove_unused_labels(CcbFunctionBuilder *fb)
{
    if (!fb)
        return;

    StringList *body = &fb->body;
    if (body->count == 0)
        return;

    StringList referenced;
    string_list_init(&referenced);

    for (size_t i = 0; i < body->count; ++i)
    {
        char jump_label[64] = {0};
        if (ccb_parse_jump_target(body->items[i], jump_label, sizeof(jump_label)))
        {
            if (!string_list_contains(&referenced, jump_label))
                string_list_append(&referenced, jump_label);
            continue;
        }

        char true_label[64] = {0};
        char false_label[64] = {0};
        if (!ccb_parse_branch_targets(body->items[i], true_label, sizeof(true_label),
                                      false_label, sizeof(false_label)))
            continue;

        if (!string_list_contains(&referenced, true_label))
            string_list_append(&referenced, true_label);
        if (!string_list_contains(&referenced, false_label))
            string_list_append(&referenced, false_label);
    }

    for (size_t i = 0; i < body->count;)
    {
        char label[64] = {0};
        if (!ccb_parse_label_name(body->items[i], label, sizeof(label)) ||
            string_list_contains(&referenced, label))
        {
            ++i;
            continue;
        }

        bool unreachable_from_fallthrough = (i > 0) && ccb_instruction_is_terminator(body->items[i - 1]);
        if (!unreachable_from_fallthrough)
        {
            string_list_remove_range(body, i, 1);
            continue;
        }

        size_t end = i + 1;
        while (end < body->count)
        {
            char next_label[64] = {0};
            if (ccb_parse_label_name(body->items[end], next_label, sizeof(next_label)))
                break;
            ++end;
        }
        string_list_remove_range(body, i, end - i);
    }

    string_list_free(&referenced);
}

static void ccb_opt_remove_nops(CcbFunctionBuilder *fb)
{
    if (!fb)
        return;
    StringList *body = &fb->body;
    for (size_t i = 0; i < body->count;)
    {
        const char *line = ccb_trim_leading_ws(body->items[i]);
        if (line && strcmp(line, "nop") == 0)
        {
            string_list_remove_range(body, i, 1);
            continue;
        }
        ++i;
    }
}

static void ccb_function_optimize(CcbFunctionBuilder *fb, const CodegenOptions *opts)
{
    if (!fb || !opts || opts->opt_level <= 0)
        return;

    const char *fn_name = (fb->fn && fb->fn->name) ? fb->fn->name : "<anon>";
    if (compiler_verbose_enabled())
        compiler_verbose_logf("optimizer", "optimizing '%s' (O%d)", fn_name, opts->opt_level);
    if (opts->opt_level >= 3)
    {
        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass fold string copy loop");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass fold string copy loop");
        ccb_opt_fold_string_copy_loop(fb);
    }
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

        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass fold constant unops");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass fold constant unops");
        ccb_opt_fold_const_unops(fb);

        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass fold constant compares");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass fold constant compares");
        ccb_opt_fold_const_compares(fb);

        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass fold test_null constants");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass fold test_null constants");
        ccb_opt_fold_const_test_null(fb);

        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass fold constant converts");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass fold constant converts");
        ccb_opt_fold_const_converts(fb);

        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass strength reduce binops");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass strength reduce binops");
        ccb_opt_strength_reduce_binops(fb);

        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass simplify no-op arith/bitcasts");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass simplify no-op arith/bitcasts");
        ccb_opt_simplify_noop_arith_and_bitcasts(fb);

        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass fold const OR store chains");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass fold const OR store chains");
        ccb_opt_fold_const_or_store_chains(fb);

        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass pack byte store runs");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass pack byte store runs");
        ccb_opt_pack_byte_store_runs(fb);

        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass remove overwritten indirect stores");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass remove overwritten indirect stores");
        ccb_opt_remove_overwritten_indirect_stores(fb);
    }

    if (opts->opt_level >= 3)
    {
        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass simplify store/load/store");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass simplify store/load/store");
        ccb_opt_simplify_store_load_store(fb);
        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass promote local values");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass promote local values");
        ccb_opt_promote_local_values(fb);
        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass propagate local values");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass propagate local values");
        ccb_opt_propagate_local_values(fb);
        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass remove dead local stores");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass remove dead local stores");
        ccb_opt_remove_dead_local_stores(fb);
        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass remove unused local slots");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass remove unused local slots");
        ccb_opt_remove_unused_local_slots(fb);
        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass fold constant compares");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass fold constant compares");
        ccb_opt_fold_const_compares(fb);
        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass fold test_null constants");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass fold test_null constants");
        ccb_opt_fold_const_test_null(fb);
        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass simplify bool normalization");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass simplify bool normalization");
        ccb_opt_simplify_bool_normalization(fb);
        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass simplify const branches");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass simplify const branches");
        ccb_opt_simplify_const_branches(fb);
        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass remove unreachable fallthrough");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass remove unreachable fallthrough");
        ccb_opt_remove_unreachable_fallthrough(fb);
        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass merge consecutive labels");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass merge consecutive labels");
        ccb_opt_merge_consecutive_labels(fb);
        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass remove redundant jumps");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass remove redundant jumps");
        ccb_opt_remove_redundant_jumps(fb);
        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass remove unused labels");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass remove unused labels");
        ccb_opt_remove_unused_labels(fb);
        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass remove redundant jumps");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass remove redundant jumps");
        ccb_opt_remove_redundant_jumps(fb);
        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass remove unused labels");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass remove unused labels");
        ccb_opt_remove_unused_labels(fb);
        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass propagate local values");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass propagate local values");
        ccb_opt_propagate_local_values(fb);
        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass remove dead local stores");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass remove dead local stores");
        ccb_opt_remove_dead_local_stores(fb);
        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass remove unused local slots");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass remove unused local slots");
        ccb_opt_remove_unused_local_slots(fb);
        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass remove dead local copies");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass remove dead local copies");
        ccb_opt_remove_dead_local_copies(fb);
        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass simplify addr_local temps");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass simplify addr_local temps");
        ccb_opt_simplify_addr_local_temp(fb);

        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass fold const OR store chains");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass fold const OR store chains");
        ccb_opt_fold_const_or_store_chains(fb);

        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass fold dup RMW OR chains");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass fold dup RMW OR chains");
        ccb_opt_fold_dup_rmw_or_chains(fb);

        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass prune dropped values");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass prune dropped values");
        ccb_opt_prune_dropped_values(fb);
        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass inline const_str locals");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass inline const_str locals");
        ccb_opt_inline_const_str_locals(fb);
        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass fold zero-init memset");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass fold zero-init memset");
        ccb_opt_fold_zero_init_memset(fb);

        if (compiler_verbose_deep_enabled())
            fprintf(stderr, "\x1b[31m& CCSim hardcore simulation\x1b[0m\n");
        if (compiler_verbose_deep_enabled())
            compiler_verbose_treef("optimizer", "+-", "pass ccsim (final)");
        if (compiler_verbose_enabled())
            compiler_verbose_logf("optimizer", "pass ccsim (final)");
        CcsimOptions ccsim_options;
        ccsim_options.opt_level = opts->opt_level;
        ccsim_options.aggressive = (fb->fn && !fb->fn->is_exposed && !fb->fn->export_name) ? 1 : 0;
        CcsimStats ccsim_stats;
        memset(&ccsim_stats, 0, sizeof(ccsim_stats));
        if (fb->module)
        {
            const char *vm_fn_name = (fb->fn && fb->fn->metadata.backend_name) ? fb->fn->metadata.backend_name :
                                     ((fb->fn && fb->fn->name) ? fb->fn->name : NULL);
            if (fb->fn && fb->fn->export_name && fb->fn->name)
                vm_fn_name = fb->fn->name;
            if (vm_fn_name)
            {
                int vm_collapsed = ccsim_vm_collapse_hidden_function(fb->body.items, fb->body.count,
                                                                      vm_fn_name,
                                                                      fb->module->lines.items, fb->module->lines.count,
                                                                      &ccsim_options, &ccsim_stats);
                if (vm_collapsed && compiler_verbose_deep_enabled())
                    compiler_verbose_treef("ccsim-vm", "|-", "collapsed '%s'", vm_fn_name);
                else if (vm_collapsed && compiler_verbose_enabled())
                    compiler_verbose_logf("ccsim-vm", "collapsed '%s'", vm_fn_name);
            }
            ccsim_collapse_hidden_calls(fb->body.items, fb->body.count,
                                        fb->module->lines.items, fb->module->lines.count,
                                        &ccsim_options, &ccsim_stats);
        }
        ccsim_optimize_lines(fb->body.items, fb->body.count, &ccsim_options, &ccsim_stats);
        ccb_opt_remove_nops(fb);
        ccb_opt_remove_unreachable_fallthrough(fb);
        ccb_opt_remove_unused_labels(fb);
        ccb_opt_remove_redundant_jumps(fb);
        if (compiler_verbose_enabled() && (ccsim_stats.vm_collapsed_functions || ccsim_stats.rewritten_load_locals || ccsim_stats.const_folds || ccsim_stats.collapsed_hidden_calls))
            compiler_verbose_logf("optimizer", "ccsim: passes=%zu vm-collapses=%zu call-collapses=%zu rewrites=%zu folds=%zu barriers=%zu",
                                  ccsim_stats.passes,
                                  ccsim_stats.vm_collapsed_functions,
                                  ccsim_stats.collapsed_hidden_calls,
                                  ccsim_stats.rewritten_load_locals,
                                  ccsim_stats.const_folds,
                                  ccsim_stats.simulation_barriers);
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

        if (ccb_function_param_has_managed_length(fb->fn, i))
        {
            char *length_name = ccb_make_managed_length_name(param_name);
            if (!ccb_local_add_u64(fb, length_name, true))
            {
                diag_error_at(fb->fn->src, fb->fn->line, fb->fn->col,
                              "failed to allocate managed array length slot for parameter %d", i);
                return;
            }
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

static CcbLocal *ccb_local_add_u64(CcbFunctionBuilder *fb, const char *name, bool is_param)
{
    CcbLocal *slot = ccb_local_add(fb, name, NULL, false, is_param);
    if (slot)
        slot->value_type = CC_TYPE_U64;
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

static bool ccb_is_unsized_array_type(const Type *ty)
{
    return ty && ty->kind == TY_ARRAY && ty->array.is_unsized;
}

static const Type *ccb_node_array_source_type(const Node *node)
{
    if (!node)
        return NULL;
    if (node->var_type && node->var_type->kind == TY_ARRAY)
        return node->var_type;
    if (node->type && node->type->kind == TY_ARRAY)
        return node->type;
    return NULL;
}

static bool ccb_function_param_has_managed_length(const Node *fn, int index)
{
    if (!fn || fn->kind != ND_FUNC || !fn->is_managed || index < 0 || index >= fn->param_count)
        return false;
    return ccb_is_unsized_array_type(fn->param_types ? fn->param_types[index] : NULL);
}

static int ccb_function_hidden_managed_param_count(const Node *fn)
{
    if (!fn || fn->kind != ND_FUNC)
        return 0;
    int count = 0;
    for (int i = 0; i < fn->param_count; ++i)
    {
        if (ccb_function_param_has_managed_length(fn, i))
            count++;
    }
    return count;
}

static bool ccb_symbol_param_has_managed_length(const Symbol *sym, int index)
{
    return sym && sym->ast_node && ccb_function_param_has_managed_length(sym->ast_node, index);
}

static char *ccb_make_managed_length_name(const char *name)
{
    const char *base = (name && *name) ? name : "array";
    size_t need = strlen(base) + strlen("__managed_len_") + 1;
    char *buffer = (char *)xmalloc(need);
    snprintf(buffer, need, "__managed_len_%s", base);
    return buffer;
}

static int ccb_emit_managed_array_length_value(CcbFunctionBuilder *fb, const Node *expr)
{
    if (!fb || !expr)
        return 1;

    if (expr->kind == ND_MANAGED_ARRAY_ADAPT)
    {
        if (!expr->rhs)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "managed array adapter missing length expression");
            return 1;
        }
        if (ccb_emit_expr_basic(fb, expr->rhs))
            return 1;
        CCValueType len_ty = ccb_type_for_expr(expr->rhs);
        if (len_ty != CC_TYPE_U64)
        {
            if (ccb_emit_convert_between(fb, len_ty, CC_TYPE_U64, expr->rhs))
                return 1;
        }
        return 0;
    }

    if (expr->managed_length_name && expr->managed_length_name[0] != '\0')
    {
        CcbLocal *local = ccb_local_lookup(fb, expr->managed_length_name);
        if (local)
        {
            if (!ccb_emit_load_local(fb, local))
                return 1;
            if (local->value_type != CC_TYPE_U64)
            {
                if (ccb_emit_convert_between(fb, local->value_type, CC_TYPE_U64, expr))
                    return 1;
            }
            return 0;
        }
        if (!ccb_emit_load_global(&fb->body, expr->managed_length_name))
            return 1;
        return 0;
    }

    const Type *array_ty = ccb_node_array_source_type(expr);
    if (array_ty && !array_ty->array.is_unsized)
    {
        if (!ccb_emit_const_u64(&fb->body, CC_TYPE_U64, (uint64_t)array_ty->array.length))
            return 1;
        return 0;
    }

    diag_error_at(expr->src, expr->line, expr->col,
                  "managed array value is missing runtime length metadata");
    return 1;
}

static int ccb_emit_store_managed_array_length(CcbFunctionBuilder *fb, const Node *site, const char *name, const Node *value_expr)
{
    if (!fb || !name || name[0] == '\0' || !value_expr)
        return 0;

    CcbLocal *local = ccb_local_lookup(fb, name);
    if (local && local->is_param)
    {
        if (ccb_promote_param_to_local(fb, site ? site : value_expr, &local))
            return 1;
    }

    if (ccb_emit_managed_array_length_value(fb, value_expr))
        return 1;

    if (local)
    {
        if (!ccb_emit_store_local(fb, local))
            return 1;
        return 0;
    }

    if (!ccb_emit_store_global(&fb->body, name))
        return 1;
    return 0;
}

static int ccb_emit_ref_arg_value(CcbFunctionBuilder *fb, const Node *expr)
{
    if (!fb || !expr)
        return 1;

    if (expr->kind == ND_VAR && expr->var_ref)
    {
        CcbLocal *local = ccb_local_lookup(fb, expr->var_ref);
        if (local)
        {
            if (!ccb_emit_load_local(fb, local))
                return 1;
            return 0;
        }
        if (expr->var_is_global)
        {
            if (!ccb_emit_load_global(&fb->body, expr->var_ref))
                return 1;
            return 0;
        }
    }

    return ccb_emit_expr_basic(fb, expr);
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
    {
        if (expr->kind == ND_VAR && expr->type->kind == TY_REF && expr->type->pointee)
            return map_type_to_cc(expr->type->pointee);
        return map_type_to_cc(expr->type);
    }

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

static bool ccb_emit_const_u64(StringList *body, CCValueType ty, uint64_t value)
{
    return string_list_appendf(body, "  const %s %llu", cc_type_name(ty),
                               (unsigned long long)value);
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

static int ccb_emit_string_equality_compare(CcbFunctionBuilder *fb, const Node *expr)
{
    if (!fb || !expr || !expr->lhs || !expr->rhs)
        return 1;

    Type *lhs_type = expr->lhs->type ? expr->lhs->type : type_ptr(type_char());
    Type *rhs_type = expr->rhs->type ? expr->rhs->type : type_ptr(type_char());

    CcbLocal *lhs_local = ccb_local_add(fb, NULL, lhs_type, false, false);
    if (!lhs_local)
        return 1;
    int lhs_local_index = lhs_local->index;
    bool lhs_local_is_param = lhs_local->is_param;
    if (ccb_emit_expr_basic(fb, expr->lhs))
        return 1;
    if (ccb_emit_convert_between(fb, ccb_type_for_expr(expr->lhs), CC_TYPE_PTR, expr->lhs))
        return 1;
    if (!ccb_emit_store_local(fb, lhs_local))
        return 1;

    CcbLocal *rhs_local = ccb_local_add(fb, NULL, rhs_type, false, false);
    if (!rhs_local)
        return 1;
    int rhs_local_index = rhs_local->index;
    bool rhs_local_is_param = rhs_local->is_param;
    if (ccb_emit_expr_basic(fb, expr->rhs))
        return 1;
    if (ccb_emit_convert_between(fb, ccb_type_for_expr(expr->rhs), CC_TYPE_PTR, expr->rhs))
        return 1;
    if (!ccb_emit_store_local(fb, rhs_local))
        return 1;

    if (!ccb_module_has_function(fb->module, "__cert__strcmp") && !ccb_module_has_extern(fb->module, "__cert__strcmp"))
    {
        if (!ccb_module_appendf(fb->module, ".extern __cert__strcmp params=(ptr,ptr) returns=i32"))
            return 1;
    }

    if (!string_list_appendf(&fb->body, lhs_local_is_param ? "  load_param %d" : "  load_local %d", lhs_local_index))
        return 1;
    if (!string_list_appendf(&fb->body, rhs_local_is_param ? "  load_param %d" : "  load_local %d", rhs_local_index))
        return 1;
    if (!string_list_appendf(&fb->body, "  call __cert__strcmp i32 (ptr,ptr)"))
        return 1;
    if (!ccb_emit_const_zero(&fb->body, CC_TYPE_I32))
        return 1;
    if (!string_list_appendf(&fb->body, "  compare %s %s", expr->kind == ND_EQ ? "eq" : "ne", cc_type_name(CC_TYPE_I32)))
        return 1;

    return 0;
}

static int ccb_emit_string_length_expr(CcbFunctionBuilder *fb, const Node *expr)
{
    if (!fb || !expr || expr->kind != ND_MEMBER || !expr->lhs)
        return 1;

    if (!expr->field_name || strcmp(expr->field_name, "length") != 0)
        return 1;
    if (!ccb_is_string_ptr_type(expr->lhs->type))
        return 1;

    if (!ccb_module_has_function(fb->module, "__cert__strlen") && !ccb_module_has_extern(fb->module, "__cert__strlen"))
    {
        if (!ccb_module_appendf(fb->module, ".extern __cert__strlen params=(ptr,ptr,u64,ptr) returns=u64"))
            return 1;
    }

    if (ccb_emit_expr_basic(fb, expr->lhs))
        return 1;
    if (ccb_emit_convert_between(fb, ccb_type_for_expr(expr->lhs), CC_TYPE_PTR, expr->lhs))
        return 1;

    const char *file_name = (expr->src && expr->src->filename) ? expr->src->filename : "";
    const char *symbol_name = "<string>";
    if (expr->lhs->kind == ND_VAR && expr->lhs->var_ref && *expr->lhs->var_ref)
        symbol_name = expr->lhs->var_ref;

    size_t file_len = strlen(file_name);
    uint8_t *file_bytes = (uint8_t *)malloc(file_len + 1);
    if (!file_bytes)
        return 1;
    memcpy(file_bytes, file_name, file_len);
    file_bytes[file_len] = 0;
    const char *file_sym = ccb_module_intern_hidden_byte_string(fb->module, "file", file_bytes, file_len + 1);
    free(file_bytes);
    if (!file_sym)
        return 1;

    size_t symbol_len = strlen(symbol_name);
    uint8_t *name_bytes = (uint8_t *)malloc(symbol_len + 1);
    if (!name_bytes)
        return 1;
    memcpy(name_bytes, symbol_name, symbol_len);
    name_bytes[symbol_len] = 0;
    const char *name_sym = ccb_module_intern_hidden_byte_string(fb->module, "name", name_bytes, symbol_len + 1);
    free(name_bytes);
    if (!name_sym)
        return 1;

    if (!ccb_emit_addr_global(&fb->body, file_sym))
        return 1;
    if (!ccb_emit_const_u64(&fb->body, CC_TYPE_U64, (uint64_t)expr->line))
        return 1;
    if (!ccb_emit_addr_global(&fb->body, name_sym))
        return 1;

    if (!string_list_appendf(&fb->body, "  call __cert__strlen u64 (ptr,ptr,u64,ptr)"))
        return 1;

    return 0;
}

static int ccb_emit_array_length_expr(CcbFunctionBuilder *fb, const Node *expr)
{
    if (!fb || !expr || expr->kind != ND_MEMBER || !expr->lhs)
        return 1;
    if (!expr->field_name || strcmp(expr->field_name, "length") != 0)
        return 1;

    const Type *array_ty = ccb_node_array_source_type(expr->lhs);
    if (!array_ty || array_ty->kind != TY_ARRAY)
        return 1;

    if (!array_ty->array.is_unsized)
    {
        if (!ccb_emit_const_u64(&fb->body, CC_TYPE_U64, (uint64_t)array_ty->array.length))
            return 1;
        return 0;
    }

    return ccb_emit_managed_array_length_value(fb, expr->lhs);
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

    if (base->kind == ND_VAR && base->type && base->type->kind == TY_REF)
    {
        if (!base->var_ref)
        {
            diag_error_at(base->src, base->line, base->col,
                          "variable reference missing name");
            return 1;
        }

        CcbLocal *base_local = ccb_local_lookup(fb, base->var_ref);
        if (!base_local)
        {
            if (!base->var_is_global)
            {
                diag_error_at(base->src, base->line, base->col,
                              "unknown local '%s'", base->var_ref);
                return 1;
            }
            if (!ccb_emit_load_global(&fb->body, base->var_ref))
                return 1;
        }
        else if (!ccb_emit_load_local(fb, base_local))
        {
            return 1;
        }
    }
    else if (ccb_emit_expr_basic(fb, base))
        return 1;

    const Type *base_type = base->type;
    CCValueType base_ty = ccb_type_for_expr(base);
    if (base_type && base_type->kind == TY_PTR)
    {
        base_ty = CC_TYPE_PTR;
    }
    else if (base_type && base_type->kind == TY_REF)
    {
        // treat refs like pointers for codegen; may need runtime null-check
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

    const Type *elem_type = NULL;
    if (base_type && base_type->kind == TY_PTR)
        elem_type = base_type->pointee;
    else if (base_type && base_type->kind == TY_REF)
        elem_type = base_type->pointee;
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
    // If base is a checked nullable ref, insert runtime null-check before any subsequent load.
    if (base_type && base_type->kind == TY_REF && base_type->ref_nullability == 1)
    {
        // Keep pointer on stack for continuation and test a duplicate for null.
        if (!string_list_appendf(&fb->body, "  dup ptr"))
            return 1;
        if (!string_list_appendf(&fb->body, "  test_null"))
            return 1;

        // create labels
        char call_label[64];
        char cont_label[64];
        snprintf(call_label, sizeof(call_label), "Lcc_nullchk_call_%d", fb->next_label_id++);
        snprintf(cont_label, sizeof(cont_label), "Lcc_nullchk_cont_%d", fb->next_label_id++);

        if (!string_list_appendf(&fb->body, "  branch %s %s", call_label, cont_label))
            return 1;

        // call path
        if (!string_list_appendf(&fb->body, "label %s", call_label))
            return 1;

        if (fb->active_try_error_label && fb->active_try_error_label[0])
        {
            if (!ccb_module_has_function(fb->module, "__cert__null_deref") && !ccb_module_has_extern(fb->module, "__cert__null_deref"))
            {
                if (!ccb_module_appendf(fb->module, ".extern __cert__null_deref params=(ptr,u64,ptr) returns=void"))
                    return 1;
            }
        }
        else if (!ccb_module_has_function(fb->module, "__cert__null_deref_fallback") && !ccb_module_has_extern(fb->module, "__cert__null_deref_fallback"))
        {
            if (!ccb_module_appendf(fb->module, ".extern __cert__null_deref_fallback params=(ptr,u64,ptr) returns=ptr"))
                return 1;
        }

        // file literal global
        const char *fname = expr->src && expr->src->filename ? expr->src->filename : "";
        size_t flen = strlen(fname);
        uint8_t *fbytes = (uint8_t *)malloc(flen + 1);
        if (!fbytes)
            return 1;
        memcpy(fbytes, fname, flen);
        fbytes[flen] = 0;
        const char *file_sym = ccb_module_intern_hidden_byte_string(fb->module, "file", fbytes, flen + 1);
        free(fbytes);
        if (!file_sym)
            return 1;

        // name literal (var name if available)
        const char *vname = "";
        if (base->kind == ND_VAR && base->var_ref)
            vname = base->var_ref;
        size_t nlen = strlen(vname);
        uint8_t *nbytes = (uint8_t *)malloc(nlen + 1);
        if (!nbytes)
            return 1;
        memcpy(nbytes, vname, nlen);
        nbytes[nlen] = 0;
        const char *name_sym = ccb_module_intern_hidden_byte_string(fb->module, "name", nbytes, nlen + 1);
        free(nbytes);
        if (!name_sym)
            return 1;

        if (!string_list_appendf(&fb->body, "  drop ptr"))
            return 1;

        if (!ccb_emit_addr_global(&fb->body, file_sym))
            return 1;
        if (!ccb_emit_const_u64(&fb->body, CC_TYPE_U64, (uint64_t)expr->line))
            return 1;
        if (!ccb_emit_addr_global(&fb->body, name_sym))
            return 1;
        if (fb->active_try_error_label && fb->active_try_error_label[0])
        {
            if (!string_list_appendf(&fb->body, "  call __cert__null_deref void (ptr,u64,ptr)"))
                return 1;
            if (!string_list_appendf(&fb->body, "  jump %s", fb->active_try_error_label))
                return 1;
        }
        else if (!string_list_appendf(&fb->body, "  call __cert__null_deref_fallback ptr (ptr,u64,ptr)"))
            return 1;

        // continuation label
        if (!string_list_appendf(&fb->body, "label %s", cont_label))
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

    if (base->kind == ND_VAR && base->type && base->type->kind == TY_REF)
    {
        if (!base->var_ref)
        {
            diag_error_at(base->src, base->line, base->col,
                          "variable reference missing name");
            return 1;
        }

        CcbLocal *base_local = ccb_local_lookup(fb, base->var_ref);
        if (!base_local)
        {
            if (!base->var_is_global)
            {
                diag_error_at(base->src, base->line, base->col,
                              "unknown local '%s'", base->var_ref);
                return 1;
            }
            if (!ccb_emit_load_global(&fb->body, base->var_ref))
                return 1;
        }
        else if (!ccb_emit_load_local(fb, base_local))
        {
            return 1;
        }
    }
    else if (ccb_emit_expr_basic(fb, base))
    {
        return 1;
    }

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
    else if (base->type && base->type->kind == TY_REF && base->type->pointee)
    {
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

    if (!expr->is_pointer_deref && base->type && base->type->kind == TY_REF && base->type->ref_nullability == 1)
    {
        if (!string_list_appendf(&fb->body, "  dup ptr"))
            return 1;
        if (!string_list_appendf(&fb->body, "  test_null"))
            return 1;

        char call_label[64];
        char cont_label[64];
        snprintf(call_label, sizeof(call_label), "Lcc_nullchk_call_%d", fb->next_label_id++);
        snprintf(cont_label, sizeof(cont_label), "Lcc_nullchk_cont_%d", fb->next_label_id++);

        if (!string_list_appendf(&fb->body, "  branch %s %s", call_label, cont_label))
            return 1;
        if (!string_list_appendf(&fb->body, "label %s", call_label))
            return 1;

        if (fb->active_try_error_label && fb->active_try_error_label[0])
        {
            if (!ccb_module_has_function(fb->module, "__cert__null_deref") && !ccb_module_has_extern(fb->module, "__cert__null_deref"))
            {
                if (!ccb_module_appendf(fb->module, ".extern __cert__null_deref params=(ptr,u64,ptr) returns=void"))
                    return 1;
            }
        }
        else if (!ccb_module_has_function(fb->module, "__cert__null_deref_fallback") && !ccb_module_has_extern(fb->module, "__cert__null_deref_fallback"))
        {
            if (!ccb_module_appendf(fb->module, ".extern __cert__null_deref_fallback params=(ptr,u64,ptr) returns=ptr"))
                return 1;
        }

        const char *fname = expr->src && expr->src->filename ? expr->src->filename : "";
        size_t flen = strlen(fname);
        uint8_t *fbytes = (uint8_t *)malloc(flen + 1);
        if (!fbytes)
            return 1;
        memcpy(fbytes, fname, flen);
        fbytes[flen] = 0;
        const char *file_sym = ccb_module_intern_hidden_byte_string(fb->module, "file", fbytes, flen + 1);
        free(fbytes);
        if (!file_sym)
            return 1;

        const char *vname = "";
        if (base->kind == ND_VAR && base->var_ref)
            vname = base->var_ref;
        size_t nlen = strlen(vname);
        uint8_t *nbytes = (uint8_t *)malloc(nlen + 1);
        if (!nbytes)
            return 1;
        memcpy(nbytes, vname, nlen);
        nbytes[nlen] = 0;
        const char *name_sym = ccb_module_intern_hidden_byte_string(fb->module, "name", nbytes, nlen + 1);
        free(nbytes);
        if (!name_sym)
            return 1;

        if (!string_list_appendf(&fb->body, "  drop ptr"))
            return 1;
        if (!ccb_emit_addr_global(&fb->body, file_sym))
            return 1;
        if (!ccb_emit_const_u64(&fb->body, CC_TYPE_U64, (uint64_t)expr->line))
            return 1;
        if (!ccb_emit_addr_global(&fb->body, name_sym))
            return 1;
        if (fb->active_try_error_label && fb->active_try_error_label[0])
        {
            if (!string_list_appendf(&fb->body, "  call __cert__null_deref void (ptr,u64,ptr)"))
                return 1;
            if (!string_list_appendf(&fb->body, "  jump %s", fb->active_try_error_label))
                return 1;
        }
        else if (!string_list_appendf(&fb->body, "  call __cert__null_deref_fallback ptr (ptr,u64,ptr)"))
            return 1;
        if (!string_list_appendf(&fb->body, "label %s", cont_label))
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

    CcbLocal *result_tmp = ccb_local_add(fb, NULL, value_type, false, false);
    if (!result_tmp)
        return 1;
    ptrdiff_t result_slot = result_tmp ? (ptrdiff_t)(result_tmp - fb->locals) : -1;

    if (addr_slot >= 0)
    {
        addr_local = ccb_local_from_slot(fb, addr_slot);
        if (!addr_local)
            return 1;
    }

    if (!ccb_emit_load_local(fb, addr_local))
        return 1;
    if (!ccb_emit_load_indirect(&fb->body, value_ty))
        return 1;

    if (ccb_emit_expr_basic(fb, expr->rhs))
        return 1;
    if (ccb_compound_convert_rhs(fb, expr->rhs, value_ty))
        return 1;

    if (ccb_emit_compound_binop_instr(fb, expr, value_ty))
        return 1;
    if (!ccb_emit_store_local(fb, result_tmp))
        return 1;

    if (addr_slot >= 0)
    {
        addr_local = ccb_local_from_slot(fb, addr_slot);
        if (!addr_local)
            return 1;
    }
    if (result_slot >= 0)
    {
        result_tmp = ccb_local_from_slot(fb, result_slot);
        if (!result_tmp)
            return 1;
    }

    if (!ccb_emit_load_local(fb, addr_local))
        return 1;
    if (!ccb_emit_load_local(fb, result_tmp))
        return 1;
    if (!ccb_emit_store_indirect(&fb->body, value_ty))
        return 1;
    if (!ccb_emit_load_local(fb, result_tmp))
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

static bool ccb_is_compound_assign_kind(NodeKind kind)
{
    return kind == ND_ADD_ASSIGN || kind == ND_SUB_ASSIGN ||
           kind == ND_MUL_ASSIGN || kind == ND_DIV_ASSIGN ||
           kind == ND_MOD_ASSIGN || kind == ND_BITAND_ASSIGN ||
           kind == ND_BITOR_ASSIGN || kind == ND_BITXOR_ASSIGN ||
           kind == ND_SHL_ASSIGN || kind == ND_SHR_ASSIGN;
}

static int ccb_emit_compound_assign_stmt_noresult(CcbFunctionBuilder *fb, const Node *expr)
{
    if (!fb || !expr || !expr->lhs || !expr->rhs)
        return 1;

    const Node *target = expr->lhs;
    CCValueType value_ty = CC_TYPE_INVALID;
    const Type *value_type = NULL;

    switch (target->kind)
    {
    case ND_INDEX:
        if (ccb_emit_index_address(fb, target, &value_ty, &value_type))
            return 1;
        break;
    case ND_DEREF:
        if (ccb_emit_deref_address(fb, target, &value_ty, &value_type))
            return 1;
        break;
    case ND_MEMBER:
        if (ccb_emit_member_address(fb, target, &value_ty, &value_type))
            return 1;
        break;
    default:
        if (ccb_emit_compound_assign(fb, expr))
            return 1;
        if (!ccb_emit_drop_for_type(&fb->body, ccb_type_for_expr(expr)))
            return 1;
        return 0;
    }

    if (type_is_address_only(value_type))
    {
        diag_error_at(target->src, target->line, target->col,
                      "compound assignment not supported on this type");
        return 1;
    }

    if (!string_list_appendf(&fb->body, "  dup %s", cc_type_name(CC_TYPE_PTR)))
        return 1;
    if (!ccb_emit_load_indirect(&fb->body, value_ty))
        return 1;
    if (ccb_emit_expr_basic(fb, expr->rhs))
        return 1;
    if (ccb_compound_convert_rhs(fb, expr->rhs, value_ty))
        return 1;
    if (ccb_emit_compound_binop_instr(fb, expr, value_ty))
        return 1;
    if (!ccb_emit_store_indirect(&fb->body, value_ty))
        return 1;

    return 0;
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

    Node var_ref = {0};
    var_ref.kind = ND_VAR;
    var_ref.var_ref = var_name;
    var_ref.type = (Type *)struct_type;
    var_ref.src = var_decl ? var_decl->src : NULL;
    var_ref.line = var_decl ? var_decl->line : 0;
    var_ref.col = var_decl ? var_decl->col : 0;

    if (!init || init->kind == ND_INIT_LIST)
    {
        if (ccb_emit_struct_zero(fb, var_decl, var_name, struct_type))
            return 1;
        if (ccb_emit_struct_default_fields(fb, var_decl, var_name, struct_type, false))
            return 1;
        if (!init || init->init.is_zero)
            return 0;
    }

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

        const Node *value = init->init.elems ? init->init.elems[i] : NULL;
        if (type_is_address_only(member.type))
        {
            if (!value)
                continue;
            if (value->kind == ND_INIT_LIST && (value->init.is_zero || value->init.count == 0))
                continue;

            size_t field_size = ccb_type_size_bytes(member.type);
            if (field_size == 0)
            {
                diag_error_at(value->src, value->line, value->col,
                              "struct field size is unknown for initializer");
                return 1;
            }

            uint8_t *field_bytes = (uint8_t *)calloc(field_size, 1);
            if (!field_bytes)
                return 1;
            if (!ccb_store_constant_value(member.type, value, field_bytes, field_size))
            {
                free(field_bytes);
                diag_error_at(value->src, value->line, value->col,
                              "initializer for aggregate struct field must be constant");
                return 1;
            }

            if (ccb_emit_member_address(fb, &member, NULL, NULL))
            {
                free(field_bytes);
                return 1;
            }

            Type *field_ptr_ty = type_ptr((Type *)member.type);
            CcbLocal *field_ptr = ccb_local_add(fb, NULL, field_ptr_ty, false, false);
            if (!field_ptr)
            {
                free(field_bytes);
                return 1;
            }
            if (!ccb_emit_store_local(fb, field_ptr))
            {
                free(field_bytes);
                return 1;
            }

            for (size_t b = 0; b < field_size; ++b)
            {
                if (!ccb_emit_load_local(fb, field_ptr))
                {
                    free(field_bytes);
                    return 1;
                }
                if (ccb_emit_pointer_offset(fb, (int)b, value))
                {
                    free(field_bytes);
                    return 1;
                }
                if (!ccb_emit_const(&fb->body, CC_TYPE_U8, (int64_t)field_bytes[b]))
                {
                    free(field_bytes);
                    return 1;
                }
                if (!ccb_emit_store_indirect(&fb->body, CC_TYPE_U8))
                {
                    free(field_bytes);
                    return 1;
                }
            }

            free(field_bytes);
            continue;
        }

        CCValueType field_ty = CC_TYPE_I32;
        if (ccb_emit_member_address(fb, &member, &field_ty, NULL))
            return 1;

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

static int ccb_emit_struct_default_fields(CcbFunctionBuilder *fb, const Node *var_decl, const char *var_name, const Type *struct_type, bool pointer_base)
{
    if (!fb || !var_name || !struct_type || struct_type->kind != TY_STRUCT || !ccb_struct_has_field_defaults(struct_type))
        return 0;

    Node base_ref = {0};
    base_ref.kind = ND_VAR;
    base_ref.var_ref = var_name;
    base_ref.type = pointer_base ? type_ptr((Type *)struct_type) : (Type *)struct_type;
    base_ref.src = var_decl ? var_decl->src : NULL;
    base_ref.line = var_decl ? var_decl->line : 0;
    base_ref.col = var_decl ? var_decl->col : 0;

    for (int i = 0; i < struct_type->strct.field_count; ++i)
    {
        const char *spec = struct_type->strct.field_default_values ? struct_type->strct.field_default_values[i] : NULL;
        const Type *field_type = struct_type->strct.field_types ? struct_type->strct.field_types[i] : NULL;
        Node value = {0};
        Node member = {0};

        if (!spec || !field_type)
            continue;
        if (!ccb_make_default_literal_expr(field_type, spec, &value))
            return 1;

        member.kind = ND_MEMBER;
        member.lhs = &base_ref;
        member.field_index = i;
        member.field_offset = struct_type->strct.field_offsets ? struct_type->strct.field_offsets[i] : 0;
        member.type = (Type *)field_type;
        member.is_pointer_deref = pointer_base ? 1 : 0;
        member.src = base_ref.src;
        member.line = base_ref.line;
        member.col = base_ref.col;

        if (ccb_emit_member_address(fb, &member, NULL, NULL))
            return 1;
        if (ccb_emit_expr_basic(fb, &value))
            return 1;
        if (!ccb_emit_store_indirect(&fb->body, map_type_to_cc(field_type)))
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
        size_t elem_size = ccb_type_size_bytes(elem_type);
        if (elem_size == 0)
        {
            diag_error_at(var_decl ? var_decl->src : NULL, var_decl ? var_decl->line : 0, var_decl ? var_decl->col : 0,
                          "array element size is unknown for zero-initialization");
            return 1;
        }
        size_t total_size = elem_size * (size_t)length;
        if (total_size == 0)
            return 0;

        Type byte_type = {0};
        byte_type.kind = TY_U8;

        Node base_ref = {0};
        base_ref.kind = ND_VAR;
        base_ref.var_ref = var_name;
        base_ref.type = type_ptr(&byte_type);
        base_ref.var_type = (Type *)array_type;
        base_ref.var_is_array = 1;
        base_ref.var_is_const = var_decl ? var_decl->var_is_const : 0;
        base_ref.var_is_global = var_decl ? var_decl->var_is_global : 0;
        base_ref.src = var_decl ? var_decl->src : NULL;
        base_ref.line = var_decl ? var_decl->line : 0;
        base_ref.col = var_decl ? var_decl->col : 0;

        for (size_t i = 0; i < total_size; ++i)
        {
            Node idx_lit = {0};
            idx_lit.kind = ND_INT;
            idx_lit.int_val = (int64_t)i;
            idx_lit.type = type_i32();
            idx_lit.src = base_ref.src;
            idx_lit.line = base_ref.line;
            idx_lit.col = base_ref.col;

            Node idx_expr = {0};
            idx_expr.kind = ND_INDEX;
            idx_expr.lhs = &base_ref;
            idx_expr.rhs = &idx_lit;
            idx_expr.type = &byte_type;
            idx_expr.src = base_ref.src;
            idx_expr.line = base_ref.line;
            idx_expr.col = base_ref.col;

            CCValueType elem_ty = CC_TYPE_U8;
            if (ccb_emit_index_address(fb, &idx_expr, &elem_ty, NULL))
                return 1;
            if (!ccb_emit_const_zero(&fb->body, elem_ty))
                return 1;
            if (!ccb_emit_store_indirect(&fb->body, elem_ty))
                return 1;
        }
        return 0;
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
        size_t elem_size = ccb_type_size_bytes(elem_type);
        if (elem_size == 0)
        {
            diag_error_at(init->src, init->line, init->col,
                          "array element size is unknown for initializer");
            return 1;
        }

        if (ccb_emit_array_zero(fb, var_decl, var_name, array_type))
            return 1;

        Type byte_type = {0};
        byte_type.kind = TY_U8;

        Node base_ref = {0};
        base_ref.kind = ND_VAR;
        base_ref.var_ref = var_name;
        base_ref.type = type_ptr(&byte_type);
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

            uint8_t *elem_bytes = (uint8_t *)calloc(elem_size, 1);
            if (!elem_bytes)
                return 1;
            if (!ccb_store_constant_value(elem_type, value, elem_bytes, elem_size))
            {
                free(elem_bytes);
                diag_error_at(value->src, value->line, value->col,
                              "initializer for aggregate array element must be constant");
                return 1;
            }

            for (size_t b = 0; b < elem_size; ++b)
            {
                size_t offset = (size_t)i * elem_size + b;
                Node idx_lit = {0};
                idx_lit.kind = ND_INT;
                idx_lit.int_val = (int64_t)offset;
                idx_lit.type = type_i32();
                idx_lit.src = value->src;
                idx_lit.line = value->line;
                idx_lit.col = value->col;

                Node idx_expr = {0};
                idx_expr.kind = ND_INDEX;
                idx_expr.lhs = &base_ref;
                idx_expr.rhs = &idx_lit;
                idx_expr.type = &byte_type;
                idx_expr.src = value->src;
                idx_expr.line = value->line;
                idx_expr.col = value->col;

                CCValueType elem_ty = CC_TYPE_U8;
                if (ccb_emit_index_address(fb, &idx_expr, &elem_ty, NULL))
                {
                    free(elem_bytes);
                    return 1;
                }
                if (!ccb_emit_const(&fb->body, CC_TYPE_U8, (int64_t)elem_bytes[b]))
                {
                    free(elem_bytes);
                    return 1;
                }
                if (!ccb_emit_store_indirect(&fb->body, elem_ty))
                {
                    free(elem_bytes);
                    return 1;
                }
            }

            free(elem_bytes);
        }

        return 0;
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
    const bool stack_safe_try = fb->active_try_error_label && fb->active_try_error_label[0];

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

    int hidden_arg_count = 0;
    if (!is_indirect && expr->call_target)
    {
        int visible_params = expr->call_target->param_count;
        if (visible_params > expr->arg_count)
            visible_params = expr->arg_count;
        for (int i = 0; i < visible_params; ++i)
        {
            if (ccb_function_param_has_managed_length(expr->call_target, i))
                hidden_arg_count++;
        }
    }

    int total_arg_count = expr->arg_count + hidden_arg_count;
    CCValueType *arg_types = NULL;
    CcbLocal **arg_locals = NULL;
    if (total_arg_count > 0)
    {
        arg_types = (CCValueType *)xcalloc((size_t)total_arg_count, sizeof(CCValueType));
        if (stack_safe_try)
            arg_locals = (CcbLocal **)xcalloc((size_t)total_arg_count, sizeof(CcbLocal *));
    }

    const int fixed_param_count = expr->call_target ? expr->call_target->param_count : -1;
    const bool call_is_varargs = expr->call_is_varargs || (expr->call_target && expr->call_target->is_varargs);

    int rc = 0;
    int arg_slot = 0;
    for (int i = 0; i < expr->arg_count; ++i)
    {
        const Node *arg = expr->args ? expr->args[i] : NULL;
        Type *expected_param_type = NULL;
        bool pass_ref_raw = false;
        if (!is_indirect && expr->call_target && expr->call_target->param_types && i < expr->call_target->param_count)
        {
            expected_param_type = expr->call_target->param_types[i];
            if (expected_param_type && expected_param_type->kind == TY_REF && arg && arg->type && arg->type->kind == TY_REF)
                pass_ref_raw = true;
        }

        if (pass_ref_raw ? ccb_emit_ref_arg_value(fb, arg) : ccb_emit_expr_basic(fb, arg))
        {
            rc = 1;
            break;
        }

        CCValueType arg_ty = pass_ref_raw ? CC_TYPE_PTR : ccb_type_for_expr(arg);
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
                    break;
                }
                arg_ty = CC_TYPE_I32;
            }
        }

        if (arg_types)
            arg_types[arg_slot] = arg_ty;

        if (stack_safe_try)
        {
            bool address_only = type_is_address_only(arg ? arg->type : NULL);
            CcbLocal *arg_local = ccb_local_add(fb, NULL, arg ? arg->type : NULL, address_only, false);
            if (!arg_local)
            {
                diag_error_at(expr->src, expr->line, expr->col,
                              "failed to allocate temporary for call argument");
                rc = 1;
                break;
            }
            if (!ccb_emit_store_local(fb, arg_local))
            {
                rc = 1;
                break;
            }
            arg_locals[arg_slot] = arg_local;
        }

        arg_slot++;

        if (!is_indirect && expr->call_target && i < expr->call_target->param_count &&
            ccb_function_param_has_managed_length(expr->call_target, i))
        {
            if (ccb_emit_managed_array_length_value(fb, arg))
            {
                rc = 1;
                break;
            }

            if (arg_types)
                arg_types[arg_slot] = CC_TYPE_U64;

            if (stack_safe_try)
            {
                CcbLocal *length_local = ccb_local_add_u64(fb, NULL, false);
                if (!length_local)
                {
                    diag_error_at(expr->src, expr->line, expr->col,
                                  "failed to allocate temporary for managed array length argument");
                    rc = 1;
                    break;
                }
                if (!ccb_emit_store_local(fb, length_local))
                {
                    rc = 1;
                    break;
                }
                arg_locals[arg_slot] = length_local;
            }

            arg_slot++;
        }
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
        if (stack_safe_try)
        {
            for (int i = 0; i < total_arg_count; ++i)
            {
                if (arg_locals && arg_locals[i] && !ccb_emit_load_local(fb, arg_locals[i]))
                {
                    rc = 1;
                    break;
                }
            }
        }

        CCValueType ret_ty = ccb_type_for_expr(expr);
        const char *ret_name = cc_type_name(ret_ty);
        char *arg_text = NULL;
        if (!ccb_format_type_list(arg_types, (size_t)total_arg_count, &arg_text))
        {
            rc = 1;
        }
        else if (is_indirect)
        {
            const char *suffix = call_is_varargs ? " varargs" : "";
            if (!string_list_appendf(&fb->body, "  call_indirect %s %s%s", ret_name, arg_text, suffix))
                rc = 1;
        }
        else
        {
            const char *direct_name = expr->call_name;
            if (expr->call_target)
            {
                const char *effective = ccb_effective_function_name(expr->call_target);
                if (effective && *effective)
                    direct_name = effective;
            }
            if (!direct_name || !*direct_name)
            {
                diag_error_at(expr->src, expr->line, expr->col,
                              "function call missing symbol name");
                rc = 1;
            }
            else
            {
                const char *suffix = call_is_varargs ? " varargs" : "";
                if (!string_list_appendf(&fb->body, "  call %s %s %s%s", direct_name, ret_name, arg_text, suffix))
                    rc = 1;
            }
        }
        free(arg_text);
    }

    if (!rc && fb->active_try_error_label && fb->active_try_error_label[0])
    {
        CCValueType ret_ty = ccb_type_for_expr(expr);
        CcbLocal *ret_local = NULL;
        if (ret_ty != CC_TYPE_VOID && ret_ty != CC_TYPE_INVALID)
        {
            ret_local = ccb_local_add(fb, NULL, expr->type, false, false);
            if (!ret_local)
            {
                diag_error_at(expr->src, expr->line, expr->col,
                              "failed to allocate temporary for call result");
                rc = 1;
            }
            else if (!ccb_emit_store_local(fb, ret_local))
            {
                rc = 1;
            }
        }

        if (!rc && ccb_emit_active_try_pending_branch(fb))
            rc = 1;

        if (!rc && ret_local && !ccb_emit_load_local(fb, ret_local))
            rc = 1;
    }

    free(arg_locals);
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
    case ND_MANAGED_ARRAY_ADAPT:
    {
        if (!expr->lhs)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "managed array adapter missing source expression");
            return 1;
        }
        return ccb_emit_expr_basic(fb, expr->lhs);
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
    case ND_NEW:
    {
        if (!expr->type || expr->type->kind != TY_PTR || !expr->type->pointee)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "new: target type must be a pointer to an element type");
            return 1;
        }
        const Type *elem = expr->type->pointee;
        size_t elem_size = ccb_type_size_bytes(elem);
        if (elem_size == 0)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "new: cannot allocate incomplete or void type");
            return 1;
        }

        // Compute allocation size (u64)
        if (expr->lhs)
        {
            if (ccb_emit_expr_basic(fb, expr->lhs))
                return 1;
            if (ccb_emit_convert_between(fb, ccb_type_for_expr(expr->lhs), CC_TYPE_I64, expr->lhs))
                return 1;
            if (!ccb_emit_const(&fb->body, CC_TYPE_I64, (int64_t)elem_size))
                return 1;
            if (!string_list_appendf(&fb->body, "  binop mul %s", cc_type_name(CC_TYPE_I64)))
                return 1;
        }
        else
        {
            if (!ccb_emit_const(&fb->body, CC_TYPE_I64, (int64_t)elem_size))
                return 1;
        }

        // Ensure extern for malloc wrapper
        if (!ccb_module_has_function(fb->module, "__cert__new") && !ccb_module_has_extern(fb->module, "__cert__new"))
        {
            if (!ccb_module_appendf(fb->module, ".extern __cert__new params=(u64) returns=ptr"))
                return 1;
        }

        if (!string_list_appendf(&fb->body, "  call __cert__new ptr (u64)"))
            return 1;

        if (elem->kind == TY_STRUCT && ccb_struct_has_field_defaults(elem))
        {
            char temp_name_buf[32];
            ccb_make_label(fb, temp_name_buf, sizeof(temp_name_buf), "new");
            char *temp_name = (char *)xmalloc(strlen(temp_name_buf) + 1);
            strcpy(temp_name, temp_name_buf);
            CcbLocal *temp_local = ccb_local_add(fb, temp_name, (Type *)expr->type, false, false);
            if (!temp_local)
                return 1;
            if (!ccb_emit_store_local(fb, temp_local))
                return 1;
            if (ccb_emit_struct_default_fields(fb, expr, temp_name, elem, true))
                return 1;
            if (!ccb_emit_load_local(fb, temp_local))
                return 1;
        }
        return 0;
    }
    case ND_INT:
    {
        CCValueType ty = map_type_to_cc(expr->type);
        bool is_unsigned = ccb_value_type_is_integer(ty) && !ccb_value_type_is_signed(ty);
        if (is_unsigned)
        {
            if (!ccb_emit_const_u64(&fb->body, ty, expr->int_uval))
                return 1;
            return 0;
        }
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
        if (!target)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "operand of %s must be an lvalue",
                          expr->kind == ND_PREINC ? "++" : "--");
            return 1;
        }

        if (target->kind == ND_DEREF)
        {
            CCValueType elem_ty = CC_TYPE_I32;
            const Type *elem_type = NULL;
            if (ccb_emit_deref_address(fb, target, &elem_ty, &elem_type))
                return 1;

            Type *value_type = (Type *)(elem_type ? elem_type : target->type);
            Type *addr_type = type_ptr(value_type);
            CcbLocal *addr_local = ccb_local_add(fb, NULL, addr_type, false, false);
            if (!addr_local)
                return 1;
            if (!ccb_emit_store_local(fb, addr_local))
                return 1;

            bool is_ptr = (elem_ty == CC_TYPE_PTR);
            if (!ccb_value_type_is_integer(elem_ty) && !is_ptr)
            {
                diag_error_at(expr->src, expr->line, expr->col,
                              "%s only supported on integer or pointer lvalues",
                              expr->kind == ND_PREINC ? "pre-increment" : "pre-decrement");
                return 1;
            }

            size_t elem_size = is_ptr ? ccb_pointer_elem_size(value_type) : 1;
            const char *op = expr->kind == ND_PREINC ? "add" : "sub";

            if (!ccb_emit_load_local(fb, addr_local))
                return 1;
            if (!ccb_emit_load_indirect(&fb->body, elem_ty))
                return 1;

            if (is_ptr)
            {
                if (ccb_emit_convert_between(fb, elem_ty, CC_TYPE_I64, expr))
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
                if (!ccb_emit_const(&fb->body, elem_ty, 1))
                    return 1;
                if (!string_list_appendf(&fb->body, "  binop %s %s", op, cc_type_name(elem_ty)))
                    return 1;
            }

            CcbLocal *new_value = ccb_local_add(fb, NULL, value_type, false, false);
            if (!new_value)
                return 1;
            if (!ccb_emit_store_local(fb, new_value))
                return 1;

            if (!ccb_emit_load_local(fb, addr_local))
                return 1;
            if (!ccb_emit_load_local(fb, new_value))
                return 1;
            if (!ccb_emit_store_indirect(&fb->body, elem_ty))
                return 1;

            if (!ccb_emit_load_local(fb, new_value))
                return 1;
            return 0;
        }

        if (target->kind != ND_VAR || !target->var_ref)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "operand of %s must be a variable or dereference",
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
        if (!target)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "operand of %s must be an lvalue",
                          expr->kind == ND_POSTINC ? "post-increment" : "post-decrement");
            return 1;
        }

        if (target->kind == ND_DEREF)
        {
            CCValueType elem_ty = CC_TYPE_I32;
            const Type *elem_type = NULL;
            if (ccb_emit_deref_address(fb, target, &elem_ty, &elem_type))
                return 1;

            Type *value_type = (Type *)(elem_type ? elem_type : target->type);
            Type *addr_type = type_ptr(value_type);
            CcbLocal *addr_local = ccb_local_add(fb, NULL, addr_type, false, false);
            if (!addr_local)
                return 1;
            if (!ccb_emit_store_local(fb, addr_local))
                return 1;

            bool is_ptr = (elem_ty == CC_TYPE_PTR);
            if (!ccb_value_type_is_integer(elem_ty) && !is_ptr)
            {
                diag_error_at(expr->src, expr->line, expr->col,
                              "%s only supported on integer or pointer lvalues",
                              expr->kind == ND_POSTINC ? "post-increment" : "post-decrement");
                return 1;
            }

            size_t elem_size = is_ptr ? ccb_pointer_elem_size(value_type) : 1;
            const char *op = expr->kind == ND_POSTINC ? "add" : "sub";

            if (!ccb_emit_load_local(fb, addr_local))
                return 1;
            if (!ccb_emit_load_indirect(&fb->body, elem_ty))
                return 1;

            CcbLocal *old_value = ccb_local_add(fb, NULL, value_type, false, false);
            if (!old_value)
                return 1;
            if (!ccb_emit_store_local(fb, old_value))
                return 1;

            if (!ccb_emit_load_local(fb, old_value))
                return 1;
            if (is_ptr)
            {
                if (ccb_emit_convert_between(fb, elem_ty, CC_TYPE_I64, expr))
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
                if (!ccb_emit_const(&fb->body, elem_ty, 1))
                    return 1;
                if (!string_list_appendf(&fb->body, "  binop %s %s", op, cc_type_name(elem_ty)))
                    return 1;
            }

            CcbLocal *new_value = ccb_local_add(fb, NULL, value_type, false, false);
            if (!new_value)
                return 1;
            if (!ccb_emit_store_local(fb, new_value))
                return 1;

            if (!ccb_emit_load_local(fb, addr_local))
                return 1;
            if (!ccb_emit_load_local(fb, new_value))
                return 1;
            if (!ccb_emit_store_indirect(&fb->body, elem_ty))
                return 1;

            if (!ccb_emit_load_local(fb, old_value))
                return 1;
            return 0;
        }

        if (target->kind != ND_VAR || !target->var_ref)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "operand of %s must be a variable or dereference",
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
    case ND_STRICT_EQ:
    case ND_NE:
    {
        if (!expr->lhs || !expr->rhs)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "comparison missing operand");
            return 1;
        }

        if ((expr->kind == ND_EQ || expr->kind == ND_NE) &&
            ccb_is_string_ptr_type(expr->lhs->type) && ccb_is_string_ptr_type(expr->rhs->type))
            return ccb_emit_string_equality_compare(fb, expr);

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
        case ND_STRICT_EQ:
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
    case ND_IS:
    {
        if (!expr->lhs || !expr->is_type)
        {
            diag_error_at(expr->src, expr->line, expr->col,
                          "invalid 'is' expression");
            return 1;
        }

        if (ccb_emit_expr_basic(fb, expr->lhs))
            return 1;

        CCValueType lhs_ty = ccb_type_for_expr(expr->lhs);
        if (lhs_ty != CC_TYPE_PTR)
        {
            if (ccb_emit_convert_between(fb, lhs_ty, CC_TYPE_PTR, expr->lhs))
                return 1;
        }

        char target_meta[128];
        ccb_type_metadata_name(expr->is_type, target_meta, sizeof(target_meta));
        if (ccb_emit_const_str_lit(fb, target_meta))
            return 1;

        if (ccb_ensure_runtime_extern(fb, "__cert__object_is_type", "i32", "(ptr,ptr)"))
            return 1;
        if (!string_list_appendf(&fb->body, "  call __cert__object_is_type i32 (ptr,ptr)"))
            return 1;

        CCValueType result_ty = map_type_to_cc(expr->type);
        if (result_ty == CC_TYPE_I1)
        {
            if (ccb_emit_convert_between(fb, CC_TYPE_I32, CC_TYPE_I1, expr))
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
        if (!string_list_appendf(&fb->body, "  jump %s", end_label))
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
        if (!string_list_appendf(&fb->body, "  jump %s", end_label))
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

        CCValueType result_cc = map_type_to_cc(result_type);
        if (result_cc == CC_TYPE_VOID || result_cc == CC_TYPE_INVALID)
            result_cc = ccb_type_for_expr(expr->rhs);
        if (result_cc == CC_TYPE_VOID || result_cc == CC_TYPE_INVALID)
            result_cc = ccb_type_for_expr(expr->body);
        if (result_cc == CC_TYPE_VOID || result_cc == CC_TYPE_INVALID)
            result_cc = CC_TYPE_I32;

        if (!string_list_appendf(&fb->body, "label %s", true_label))
            return 1;
        if (ccb_emit_expr_basic(fb, expr->rhs))
            return 1;
        if (ccb_emit_convert_between(fb, ccb_type_for_expr(expr->rhs), result_cc, expr->rhs))
            return 1;
        if (!string_list_appendf(&fb->body, "  jump %s", end_label))
            return 1;

        if (!string_list_appendf(&fb->body, "label %s", false_label))
            return 1;
        if (ccb_emit_expr_basic(fb, expr->body))
            return 1;
        if (ccb_emit_convert_between(fb, ccb_type_for_expr(expr->body), result_cc, expr->body))
            return 1;
        if (!string_list_appendf(&fb->body, "  jump %s", end_label))
            return 1;

        if (!string_list_appendf(&fb->body, "label %s", end_label))
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
        if (expr->field_name && strcmp(expr->field_name, "length") == 0 &&
            !expr->is_pointer_deref && ccb_node_array_source_type(expr->lhs ? expr->lhs : NULL))
            return ccb_emit_array_length_expr(fb, expr);

        if (expr->field_name && strcmp(expr->field_name, "length") == 0 &&
            !expr->is_pointer_deref && ccb_is_string_ptr_type(expr->lhs ? expr->lhs->type : NULL))
            return ccb_emit_string_length_expr(fb, expr);

        CCValueType field_ty = CC_TYPE_I32;
        const Type *field_type = NULL;
        if (ccb_emit_member_address(fb, expr, &field_ty, &field_type))
            return 1;

        if (field_type && (field_type->kind == TY_STRUCT || field_type->kind == TY_ARRAY))
        {
            // Struct/array fields are handled by address; caller will copy or index as needed
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
                    const char *global_name = operand->var_ref;
                    if (operand->var_is_function && operand->referenced_function)
                    {
                        const char *effective = ccb_effective_function_name(operand->referenced_function);
                        if (effective && *effective)
                            global_name = effective;
                    }
                    if (!ccb_emit_addr_global(&fb->body, global_name))
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

                if (expr->type && expr->type->kind == TY_REF && expr->type->pointee)
                {
                    const Type *pointee = expr->type->pointee;
                    if (!type_is_address_only(pointee))
                    {
                        CCValueType pointee_ty = map_type_to_cc(pointee);
                        if (pointee_ty != CC_TYPE_INVALID && pointee_ty != CC_TYPE_VOID)
                        {
                            if (!ccb_emit_load_indirect(&fb->body, pointee_ty))
                                return 1;
                        }
                    }
                }
                return 0;
            }
            diag_error_at(expr->src, expr->line, expr->col,
                          "unknown local '%s'", expr->var_ref);
            return 1;
        }
        if (!ccb_emit_load_local(fb, local))
            return 1;

        if (expr->type && expr->type->kind == TY_REF && expr->type->pointee)
        {
            const Type *pointee = expr->type->pointee;
            if (!type_is_address_only(pointee))
            {
                CCValueType pointee_ty = map_type_to_cc(pointee);
                if (pointee_ty != CC_TYPE_INVALID && pointee_ty != CC_TYPE_VOID)
                {
                    if (!ccb_emit_load_indirect(&fb->body, pointee_ty))
                        return 1;
                }
            }
        }
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
                    const char *global_name = target->var_ref;
                    if (target->var_is_function && target->referenced_function)
                    {
                        const char *effective = ccb_effective_function_name(target->referenced_function);
                        if (effective && *effective)
                            global_name = effective;
                    }
                    if (type_is_address_only(target->type))
                    {
                        Type *ptr_ty = type_ptr((Type *)target->type);
                        CcbLocal *dst_ptr = ccb_local_add(fb, NULL, ptr_ty, false, false);
                        if (!dst_ptr)
                            return 1;
                        if (!ccb_emit_addr_global(&fb->body, global_name))
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
                            if (!ccb_emit_addr_global(&fb->body, global_name))
                                return 1;
                        }
                        return 0;
                    }

                    if (ccb_emit_expr_basic(fb, expr->rhs))
                        return 1;

                    if (!ccb_emit_store_global(&fb->body, global_name))
                        return 1;

                    if (expr->managed_length_name && expr->managed_length_name[0] != '\0')
                    {
                        if (ccb_emit_store_managed_array_length(fb, expr, expr->managed_length_name, expr->rhs))
                            return 1;
                    }

                    CCValueType result_ty = ccb_type_for_expr(expr);
                    if (result_ty != CC_TYPE_VOID)
                    {
                        if (!ccb_emit_load_global(&fb->body, global_name))
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

            if (expr->managed_length_name && expr->managed_length_name[0] != '\0')
            {
                if (ccb_emit_store_managed_array_length(fb, expr, expr->managed_length_name, expr->rhs))
                    return 1;
            }

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

        const Type *from_ast = expr->lhs ? expr->lhs->type : NULL;
        const Type *to_ast = expr->type;
        const int from_is_object = ccb_type_is_object_ast(from_ast);
        const int to_is_object = ccb_type_is_object_ast(to_ast);
        char from_meta[128];
        char to_meta[128];
        ccb_type_metadata_name(from_ast, from_meta, sizeof(from_meta));
        ccb_type_metadata_name(to_ast, to_meta, sizeof(to_meta));

        if (to_is_object && !from_is_object)
        {
            if (ccb_value_type_is_integer(from_ty))
            {
                if (from_ty != CC_TYPE_I64)
                {
                    if (ccb_emit_convert_between(fb, from_ty, CC_TYPE_I64, expr->lhs))
                        return 1;
                }
                if (ccb_emit_const_str_lit(fb, from_meta))
                    return 1;
                if (ccb_ensure_runtime_extern(fb, "__cert__box_i64", "ptr", "(i64,ptr)"))
                    return 1;
                if (!string_list_appendf(&fb->body, "  call __cert__box_i64 ptr (i64,ptr)"))
                    return 1;
                return 0;
            }
            if (ccb_value_type_is_float(from_ty))
            {
                if (from_ty != CC_TYPE_F64)
                {
                    if (ccb_emit_convert_between(fb, from_ty, CC_TYPE_F64, expr->lhs))
                        return 1;
                }
                if (ccb_emit_const_str_lit(fb, from_meta))
                    return 1;
                if (ccb_ensure_runtime_extern(fb, "__cert__box_f64", "ptr", "(f64,ptr)"))
                    return 1;
                if (!string_list_appendf(&fb->body, "  call __cert__box_f64 ptr (f64,ptr)"))
                    return 1;
                return 0;
            }
            if (from_ty == CC_TYPE_PTR)
            {
                if (ccb_emit_const_str_lit(fb, from_meta))
                    return 1;
                if (ccb_ensure_runtime_extern(fb, "__cert__box_ptr", "ptr", "(ptr,ptr)"))
                    return 1;
                if (!string_list_appendf(&fb->body, "  call __cert__box_ptr ptr (ptr,ptr)"))
                    return 1;
                return 0;
            }
            diag_error_at(expr->src, expr->line, expr->col,
                          "unsupported runtime boxing source type");
            return 1;
        }

        if (from_is_object && !to_is_object)
        {
            if (from_ty != CC_TYPE_PTR)
            {
                if (ccb_emit_convert_between(fb, from_ty, CC_TYPE_PTR, expr->lhs))
                    return 1;
            }

            const char *cast_file = (expr->src && expr->src->filename) ? expr->src->filename : "<unknown>";
            int64_t cast_line = (expr->line > 0) ? (int64_t)expr->line : 0;
            const char *cast_symbol = to_meta;

            if (ccb_value_type_is_integer(to_ty))
            {
                if (ccb_emit_const_str_lit(fb, to_meta))
                    return 1;
                if (ccb_emit_const_str_lit(fb, cast_file))
                    return 1;
                if (!ccb_emit_const(&fb->body, CC_TYPE_U64, cast_line))
                    return 1;
                if (ccb_emit_const_str_lit(fb, cast_symbol))
                    return 1;
                if (ccb_ensure_runtime_extern(fb, "__cert__unbox_i64", "i64", "(ptr,ptr,ptr,u64,ptr)"))
                    return 1;
                if (!string_list_appendf(&fb->body, "  call __cert__unbox_i64 i64 (ptr,ptr,ptr,u64,ptr)"))
                    return 1;
                if (to_ty != CC_TYPE_I64)
                {
                    if (ccb_emit_convert_between(fb, CC_TYPE_I64, to_ty, expr))
                        return 1;
                }
                return 0;
            }
            if (ccb_value_type_is_float(to_ty))
            {
                if (ccb_emit_const_str_lit(fb, to_meta))
                    return 1;
                if (ccb_emit_const_str_lit(fb, cast_file))
                    return 1;
                if (!ccb_emit_const(&fb->body, CC_TYPE_U64, cast_line))
                    return 1;
                if (ccb_emit_const_str_lit(fb, cast_symbol))
                    return 1;
                if (ccb_ensure_runtime_extern(fb, "__cert__unbox_f64", "f64", "(ptr,ptr,ptr,u64,ptr)"))
                    return 1;
                if (!string_list_appendf(&fb->body, "  call __cert__unbox_f64 f64 (ptr,ptr,ptr,u64,ptr)"))
                    return 1;
                if (to_ty != CC_TYPE_F64)
                {
                    if (ccb_emit_convert_between(fb, CC_TYPE_F64, to_ty, expr))
                        return 1;
                }
                return 0;
            }
            if (to_ty == CC_TYPE_PTR)
            {
                if (ccb_emit_const_str_lit(fb, to_meta))
                    return 1;
                if (ccb_emit_const_str_lit(fb, cast_file))
                    return 1;
                if (!ccb_emit_const(&fb->body, CC_TYPE_U64, cast_line))
                    return 1;
                if (ccb_emit_const_str_lit(fb, cast_symbol))
                    return 1;
                if (ccb_ensure_runtime_extern(fb, "__cert__unbox_ptr", "ptr", "(ptr,ptr,ptr,u64,ptr)"))
                    return 1;
                if (!string_list_appendf(&fb->body, "  call __cert__unbox_ptr ptr (ptr,ptr,ptr,u64,ptr)"))
                    return 1;
                return 0;
            }

            diag_error_at(expr->src, expr->line, expr->col,
                          "unsupported runtime unboxing target type");
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
            // if this is the entrypoint, call GC prep exit before returning
            if (fb->fn && fb->fn->is_entrypoint && fb->needs_gc_prep)
            {
                if (!ccb_module_has_function(fb->module, "__cert__GC__prep_exit") && !ccb_module_has_extern(fb->module, "__cert__GC__prep_exit"))
                {
                    if (!ccb_module_appendf(fb->module, ".extern __cert__GC__prep_exit params=() returns=void"))
                        return 1;
                }
                if (!string_list_appendf(&fb->body, "  call __cert__GC__prep_exit void ()"))
                    return 1;
            }
            if (ccb_emit_expr_basic(fb, stmt->lhs))
                return 1;
            if (!string_list_append(&fb->body, "  ret"))
                return 1;
        }
        else
        {
            // if this is the entrypoint, call GC prep exit before returning
            if (fb->fn && fb->fn->is_entrypoint && fb->needs_gc_prep)
            {
                if (!ccb_module_has_function(fb->module, "__cert__GC__prep_exit") && !ccb_module_has_extern(fb->module, "__cert__GC__prep_exit"))
                {
                    if (!ccb_module_appendf(fb->module, ".extern __cert__GC__prep_exit params=() returns=void"))
                        return 1;
                }
                if (!string_list_appendf(&fb->body, "  call __cert__GC__prep_exit void ()"))
                    return 1;
            }
            if (!string_list_append(&fb->body, "  ret void"))
                return 1;
        }
        return 0;
    case ND_BLOCK:
        return ccb_emit_block(fb, stmt, true);
    case ND_EXPR_STMT:
        if (stmt->lhs)
        {
            if (stmt->lhs->kind == ND_CALL && stmt->lhs->call_is_jump)
            {
                const Node *jump_call = stmt->lhs;

                if (jump_call->call_is_indirect)
                {
                    if (!jump_call->lhs)
                    {
                        diag_error_at(jump_call->src, jump_call->line, jump_call->col,
                                      "jump call missing target expression");
                        return 1;
                    }
                    if (ccb_emit_expr_basic(fb, jump_call->lhs))
                        return 1;
                    CCValueType target_ty = ccb_type_for_expr(jump_call->lhs);
                    if (target_ty != CC_TYPE_PTR)
                    {
                        if (ccb_emit_convert_between(fb, target_ty, CC_TYPE_PTR, jump_call->lhs))
                            return 1;
                    }
                }
                else
                {
                    if (!jump_call->call_name || !*jump_call->call_name)
                    {
                        diag_error_at(jump_call->src, jump_call->line, jump_call->col,
                                      "jump call missing target symbol");
                        return 1;
                    }
                    if (!ccb_emit_addr_global(&fb->body, jump_call->call_name))
                        return 1;
                }

                if (!string_list_append(&fb->body, "  jump_indirect"))
                    return 1;
                return 0;
            }

            if (ccb_is_compound_assign_kind(stmt->lhs->kind))
                return ccb_emit_compound_assign_stmt_noresult(fb, stmt->lhs);
            if (ccb_emit_expr_basic(fb, stmt->lhs))
                return 1;
            CCValueType expr_ty = ccb_type_for_expr(stmt->lhs);
            if (!ccb_emit_drop_for_type(&fb->body, expr_ty))
                return 1;
        }
        return 0;
    case ND_DELETE:
    {
        if (!stmt->lhs)
        {
            diag_error_at(stmt->src, stmt->line, stmt->col,
                          "delete missing operand");
            return 1;
        }

        // Ensure extern for free wrapper
        if (!ccb_module_has_function(fb->module, "__cert__delete") && !ccb_module_has_extern(fb->module, "__cert__delete"))
        {
            if (!ccb_module_appendf(fb->module, ".extern __cert__delete params=(ptr) returns=void"))
                return 1;
        }

        // Handle different lvalue kinds so we can null the pointer after free.
        const Node *target = stmt->lhs;
        if (target->kind == ND_VAR && target->var_ref)
        {
            // Load variable value, call free, then store null into variable
            CcbLocal *local = ccb_local_lookup(fb, target->var_ref);
            if (local)
            {
                if (!ccb_emit_load_local(fb, local))
                    return 1;
                if (!string_list_appendf(&fb->body, "  call __cert__delete void (ptr)"))
                    return 1;
                if (!ccb_emit_const_zero(&fb->body, CC_TYPE_PTR))
                    return 1;
                if (!ccb_emit_store_local(fb, local))
                    return 1;
                return 0;
            }
            else
            {
                // global
                if (!ccb_emit_load_global(&fb->body, target->var_ref))
                    return 1;
                if (!string_list_appendf(&fb->body, "  call __cert__delete void (ptr)"))
                    return 1;
                if (!ccb_emit_const_zero(&fb->body, CC_TYPE_PTR))
                    return 1;
                if (!ccb_emit_store_global(&fb->body, target->var_ref))
                    return 1;
                return 0;
            }
        }

        if (target->kind == ND_DEREF || target->kind == ND_INDEX || target->kind == ND_MEMBER)
        {
            CCValueType elem_ty = CC_TYPE_PTR;
            const Type *elem_type = NULL;
            // Compute address and stash it in a temp local
            CcbLocal *addr_local = ccb_local_add(fb, NULL, type_ptr(type_void()), false, false);
            if (!addr_local)
                return 1;

            if (target->kind == ND_DEREF)
            {
                if (ccb_emit_deref_address(fb, target, &elem_ty, &elem_type))
                    return 1;
            }
            else if (target->kind == ND_INDEX)
            {
                if (ccb_emit_index_address(fb, target, &elem_ty, &elem_type))
                    return 1;
            }
            else // ND_MEMBER
            {
                if (ccb_emit_member_address(fb, target, &elem_ty, &elem_type))
                    return 1;
            }

            if (!ccb_emit_store_local(fb, addr_local))
                return 1;

            // load the pointer from address, call free
            if (!ccb_emit_load_local(fb, addr_local))
                return 1;
            if (!ccb_emit_load_indirect(&fb->body, CC_TYPE_PTR))
                return 1;
            if (!string_list_appendf(&fb->body, "  call __cert__delete void (ptr)"))
                return 1;

            // store null back through the address
            if (!ccb_emit_load_local(fb, addr_local))
                return 1;
            if (!ccb_emit_const_zero(&fb->body, CC_TYPE_PTR))
                return 1;
            if (!ccb_emit_store_indirect(&fb->body, CC_TYPE_PTR))
                return 1;
            return 0;
        }

        // Fallback: evaluate expression, call free
        if (ccb_emit_expr_basic(fb, stmt->lhs))
            return 1;
        if (!string_list_appendf(&fb->body, "  call __cert__delete void (ptr)"))
            return 1;
        return 0;
    }
    case ND_SEQ:
    {
        if (ccb_emit_expr_basic(fb, stmt))
            return 1;
        CCValueType seq_ty = ccb_type_for_expr(stmt);
        if (!ccb_emit_drop_for_type(&fb->body, seq_ty))
            return 1;
        return 0;
    }
    case ND_TRY:
    {
        if (!stmt->lhs)
        {
            diag_error_at(stmt->src, stmt->line, stmt->col,
                          "try statement missing try block");
            return 1;
        }

        if (ccb_ensure_runtime_extern(fb, "__cert__exception_enter_try", "void", "()"))
            return 1;
        if (ccb_ensure_runtime_extern(fb, "__cert__exception_leave_try", "void", "()"))
            return 1;
        if (ccb_ensure_runtime_extern(fb, "__cert__exception_has_pending", "i32", "()"))
            return 1;
        if (ccb_ensure_runtime_extern(fb, "__cert__exception_clear", "void", "()"))
            return 1;
        if (ccb_ensure_runtime_extern(fb, "__cert__exception_matches_type", "i32", "(ptr)"))
            return 1;
        if (ccb_ensure_runtime_extern(fb, "__cert__exception_propagate", "void", "()"))
            return 1;

        char try_error_label[32];
        char try_after_label[32];
        char catch_match_label[32];
        char catch_label[32];
        char catch_clear_label[32];
        char catch_rethrow_label[32];
        char uncaught_label[32];
        char no_pending_label[32];
        char finally_label[32];
        char done_label[32];
        ccb_make_label(fb, try_error_label, sizeof(try_error_label), "try_err");
        ccb_make_label(fb, try_after_label, sizeof(try_after_label), "try_after");
        ccb_make_label(fb, catch_match_label, sizeof(catch_match_label), "try_catch_match");
        ccb_make_label(fb, catch_label, sizeof(catch_label), "try_catch");
        ccb_make_label(fb, catch_clear_label, sizeof(catch_clear_label), "try_catch_clear");
        ccb_make_label(fb, catch_rethrow_label, sizeof(catch_rethrow_label), "try_catch_rethrow");
        ccb_make_label(fb, uncaught_label, sizeof(uncaught_label), "try_uncaught");
        ccb_make_label(fb, no_pending_label, sizeof(no_pending_label), "try_nopending");
        ccb_make_label(fb, finally_label, sizeof(finally_label), "try_finally");
        ccb_make_label(fb, done_label, sizeof(done_label), "try_done");

        if (!string_list_appendf(&fb->body, "  call __cert__exception_enter_try void ()"))
            return 1;

        const char *prev_try_error_label = fb->active_try_error_label;
        fb->active_try_error_label = try_error_label;

        if (stmt->lhs->kind == ND_BLOCK)
        {
            for (int i = 0; i < stmt->lhs->stmt_count; ++i)
            {
                char step_continue_label[32];
                ccb_make_label(fb, step_continue_label, sizeof(step_continue_label), "try_cont");
                if (ccb_emit_stmt_basic(fb, stmt->lhs->stmts[i]))
                    return 1;
                if (!string_list_appendf(&fb->body, "  call __cert__exception_has_pending i32 ()"))
                    return 1;
                if (!ccb_emit_const_zero(&fb->body, CC_TYPE_I32))
                    return 1;
                if (!string_list_appendf(&fb->body, "  compare ne i32"))
                    return 1;
                if (!string_list_appendf(&fb->body, "  branch %s %s", try_error_label, step_continue_label))
                    return 1;
                if (!string_list_appendf(&fb->body, "label %s", step_continue_label))
                    return 1;
            }
        }
        else
        {
            if (ccb_emit_stmt_basic(fb, stmt->lhs))
            {
                fb->active_try_error_label = prev_try_error_label;
                return 1;
            }
        }

        fb->active_try_error_label = prev_try_error_label;

        if (!string_list_appendf(&fb->body, "  jump %s", try_after_label))
            return 1;
        if (!string_list_appendf(&fb->body, "label %s", try_error_label))
            return 1;
        if (!string_list_appendf(&fb->body, "label %s", try_after_label))
            return 1;

        if (!string_list_appendf(&fb->body, "  call __cert__exception_leave_try void ()"))
            return 1;

        if (!string_list_appendf(&fb->body, "  call __cert__exception_has_pending i32 ()"))
            return 1;
        if (!ccb_emit_const_zero(&fb->body, CC_TYPE_I32))
            return 1;
        if (!string_list_appendf(&fb->body, "  compare ne i32"))
            return 1;
        if (!string_list_appendf(&fb->body, "  branch %s %s", catch_label, no_pending_label))
            return 1;

        if (!string_list_appendf(&fb->body, "label %s", catch_label))
            return 1;

        if (stmt->rhs)
        {
            if (stmt->var_type)
            {
                char catch_meta[128];
                ccb_type_metadata_name(stmt->var_type, catch_meta, sizeof(catch_meta));
                if (ccb_emit_const_str_lit(fb, catch_meta))
                    return 1;
                if (!string_list_appendf(&fb->body, "  call __cert__exception_matches_type i32 (ptr)"))
                    return 1;
                if (!ccb_emit_const_zero(&fb->body, CC_TYPE_I32))
                    return 1;
                if (!string_list_appendf(&fb->body, "  compare ne i32"))
                    return 1;
                if (!string_list_appendf(&fb->body, "  branch %s %s", catch_match_label, uncaught_label))
                    return 1;
                if (!string_list_appendf(&fb->body, "label %s", catch_match_label))
                    return 1;
            }

            if (ccb_emit_stmt_basic(fb, stmt->rhs))
                return 1;

            if (!string_list_appendf(&fb->body, "  call __cert__exception_has_pending i32 ()"))
                return 1;
            if (!ccb_emit_const_zero(&fb->body, CC_TYPE_I32))
                return 1;
            if (!string_list_appendf(&fb->body, "  compare ne i32"))
                return 1;
            if (!string_list_appendf(&fb->body, "  branch %s %s", catch_rethrow_label, catch_clear_label))
                return 1;

            if (!string_list_appendf(&fb->body, "label %s", catch_clear_label))
                return 1;
            if (!string_list_appendf(&fb->body, "  call __cert__exception_clear void ()"))
                return 1;
            if (!string_list_appendf(&fb->body, "  jump %s", finally_label))
                return 1;

            if (!string_list_appendf(&fb->body, "label %s", catch_rethrow_label))
                return 1;
            if (!string_list_appendf(&fb->body, "  jump %s", finally_label))
                return 1;
        }
        else
        {
            if (!string_list_appendf(&fb->body, "  jump %s", uncaught_label))
                return 1;
        }

        if (!string_list_appendf(&fb->body, "label %s", uncaught_label))
            return 1;
        if (!string_list_appendf(&fb->body, "  call __cert__exception_propagate void ()"))
            return 1;
        if (!string_list_appendf(&fb->body, "  jump %s", finally_label))
            return 1;

        if (!string_list_appendf(&fb->body, "label %s", no_pending_label))
            return 1;
        if (!string_list_appendf(&fb->body, "  jump %s", finally_label))
            return 1;

        if (!string_list_appendf(&fb->body, "label %s", finally_label))
            return 1;
        if (stmt->body)
        {
            if (ccb_emit_stmt_basic(fb, stmt->body))
                return 1;
        }
        if (!string_list_appendf(&fb->body, "label %s", done_label))
            return 1;
        return 0;
    }
    case ND_THROW:
    {
        if (!stmt->lhs)
        {
            if (ccb_ensure_runtime_extern(fb, "__cert__exception_has_pending", "i32", "()"))
                return 1;
            if (ccb_ensure_runtime_extern(fb, "__cert__runtime_error_ex", "void", "(i32,ptr,ptr,ptr,i64,ptr)"))
                return 1;

            char has_exception_label[32];
            char no_exception_label[32];
            char done_label[32];
            ccb_make_label(fb, has_exception_label, sizeof(has_exception_label), "throw_hasexc");
            ccb_make_label(fb, no_exception_label, sizeof(no_exception_label), "throw_noexc");
            ccb_make_label(fb, done_label, sizeof(done_label), "throw_done");

            if (!string_list_appendf(&fb->body, "  call __cert__exception_has_pending i32 ()"))
                return 1;
            if (!ccb_emit_const_zero(&fb->body, CC_TYPE_I32))
                return 1;
            if (!string_list_appendf(&fb->body, "  compare ne i32"))
                return 1;
            if (!string_list_appendf(&fb->body, "  branch %s %s", has_exception_label, no_exception_label))
                return 1;

            if (!string_list_appendf(&fb->body, "label %s", has_exception_label))
                return 1;
            if (!string_list_appendf(&fb->body, "  jump %s", done_label))
                return 1;

            if (!string_list_appendf(&fb->body, "label %s", no_exception_label))
                return 1;
            if (!string_list_appendf(&fb->body, "  const i32 1"))
                return 1;
            if (ccb_emit_const_str_lit(fb, "RuntimeError"))
                return 1;
            if (ccb_emit_const_str_lit(fb, "throw; requires an active exception to rethrow"))
                return 1;
            if (ccb_emit_const_str_lit(fb, (stmt->src && stmt->src->filename) ? stmt->src->filename : ""))
                return 1;
            if (!string_list_appendf(&fb->body, "  const i64 %lld", (long long)stmt->line))
                return 1;
            if (ccb_emit_const_str_lit(fb, "throw"))
                return 1;
            if (!string_list_appendf(&fb->body, "  call __cert__runtime_error_ex void (i32,ptr,ptr,ptr,i64,ptr)"))
                return 1;
            if (!string_list_appendf(&fb->body, "label %s", done_label))
                return 1;
            return 0;
        }

        if (ccb_ensure_runtime_extern(fb, "__cert__runtime_error_ex", "void", "(i32,ptr,ptr,ptr,i64,ptr)"))
            return 1;

        const char *category = "RuntimeError";
        if (stmt->var_type && stmt->var_type->kind == TY_STRUCT && stmt->var_type->struct_name)
            category = stmt->var_type->struct_name;

        if (!string_list_appendf(&fb->body, "  const i32 1"))
            return 1;
        if (ccb_emit_const_str_lit(fb, category))
            return 1;

        if (stmt->rhs)
        {
            if (ccb_emit_expr_basic(fb, stmt->rhs))
                return 1;
        }
        else if (stmt->var_type)
        {
            if (ccb_emit_const_str_lit(fb, "exception thrown"))
                return 1;
        }
        else
        {
            if (ccb_emit_expr_basic(fb, stmt->lhs))
                return 1;
        }

        if (ccb_emit_const_str_lit(fb, (stmt->src && stmt->src->filename) ? stmt->src->filename : ""))
            return 1;
        if (!string_list_appendf(&fb->body, "  const i64 %lld", (long long)stmt->line))
            return 1;
        if (ccb_emit_const_str_lit(fb, "throw"))
            return 1;
        if (!string_list_appendf(&fb->body, "  call __cert__runtime_error_ex void (i32,ptr,ptr,ptr,i64,ptr)"))
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

        if (stmt->managed_length_name && stmt->managed_length_name[0] != '\0')
        {
            if (!ccb_local_add_u64(fb, stmt->managed_length_name, false))
            {
                diag_error_at(stmt->src, stmt->line, stmt->col,
                              "failed to allocate managed array length storage for local '%s'", stmt->var_name);
                return 1;
            }
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

            if (stmt->managed_length_name && stmt->managed_length_name[0] != '\0')
            {
                if (ccb_emit_store_managed_array_length(fb, stmt, stmt->managed_length_name, init))
                    return 1;
            }
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
        if (ccb_function_param_has_managed_length(fn, i))
        {
            if (!append_token_dynamic(&line, &cap, &len, cc_type_name(CC_TYPE_U64)))
            {
                free(line);
                return false;
            }
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
        if (ty && ty->kind == TY_PTR && expr->int_uval == 0)
        {
            snprintf(buffer, bufsz, "null");
            return true;
        }
        if ((ty && (ty->kind == TY_U8 || ty->kind == TY_U16 || ty->kind == TY_U32 || ty->kind == TY_U64)) ||
            (!ty && expr->int_is_unsigned))
            snprintf(buffer, bufsz, "%llu", (unsigned long long)expr->int_uval);
        else
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
            if (ccb_emit_string_ptr_array_global(mod, name, decl->var_type, init, section_literal, const_attr, hidden_attr))
            {
                status = 0;
                goto cleanup;
            }
        }
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
        else if (decl->var_type->kind == TY_STRUCT && (init || ccb_struct_has_field_defaults(decl->var_type)))
        {
            uint8_t *bytes = NULL;
            size_t byte_len = 0;
            if (!ccb_flatten_struct_initializer(decl->var_type, init, &bytes, &byte_len) || !bytes)
            {
                diag_error_at(init ? init->src : decl->src, init ? init->line : decl->line, init ? init->col : decl->col,
                              "failed to encode initializer for global '%s'", decl->var_name);
                free(bytes);
                goto cleanup;
            }
            if ((size_t)size_bytes != byte_len)
            {
                diag_error_at(init ? init->src : decl->src, init ? init->line : decl->line, init ? init->col : decl->col,
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
    if (fn->export_name && !fn->raw_export_name)
        backend_name = fn->name;
    const char *ret_name = cc_type_name(map_type_to_cc(fn->ret_type));
    int declared_params = (fn->metadata.declared_param_count >= 0)
                              ? fn->metadata.declared_param_count
                              : fn->param_count;
    if (declared_params < 0)
        declared_params = 0;
    size_t param_count = (size_t)declared_params;
    if (declared_params == fn->param_count)
        param_count += (size_t)ccb_function_hidden_managed_param_count(fn);
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
        const char *jump_target_suffix = fn->is_jump_target ? " jump-target" : "";
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
                                           varargs_suffix, jump_target_suffix, force_inline_suffix, hidden_suffix)
                      : ccb_module_appendf(mod, ".func %s ret=%s params=%zu locals=%zu%s%s%s%s",
                                           backend_name, ret_name, param_count, local_count,
                                           varargs_suffix, jump_target_suffix, force_inline_suffix, hidden_suffix);
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
    if (fn->export_name && !fn->raw_export_name)
        backend_name = fn->name;
    const char *ret_name = cc_type_name(map_type_to_cc(fn->ret_type));
    int declared_params = (fn->metadata.declared_param_count >= 0)
                              ? fn->metadata.declared_param_count
                              : fn->param_count;
    if (declared_params < 0)
        declared_params = 0;
    size_t param_count = (size_t)declared_params;
    if (declared_params == fn->param_count)
        param_count += (size_t)ccb_function_hidden_managed_param_count(fn);
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
        const char *jump_target_suffix = fn->is_jump_target ? " jump-target" : "";
        const char *force_inline_suffix = fn->force_inline_literal ? " force-inline-literal" : "";
        const char *hidden_suffix = fn->is_exposed ? "" : " hidden";
        if (!ccb_module_appendf(mod, ".func %s ret=%s params=%zu locals=%zu%s%s%s%s",
                    backend_name, ret_name, param_count, local_count, varargs_suffix, jump_target_suffix, force_inline_suffix, hidden_suffix))
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
    fb.needs_gc_prep = ccb_node_uses_tracked_alloc(fn->body);
    int rc = 0;

    // If this function is the entry point, arrange for GC prep calls.
    if (fn->is_entrypoint && fb.needs_gc_prep)
    {
        if (!ccb_module_has_function(mod, "__cert__GC__prep_enter") && !ccb_module_has_extern(mod, "__cert__GC__prep_enter"))
        {
            if (!ccb_module_appendf(mod, ".extern __cert__GC__prep_enter params=() returns=void"))
                rc = 1;
        }
        if (!ccb_module_has_function(mod, "__cert__GC__prep_exit") && !ccb_module_has_extern(mod, "__cert__GC__prep_exit"))
        {
            if (!ccb_module_appendf(mod, ".extern __cert__GC__prep_exit params=() returns=void"))
                rc = 1;
        }

        if (rc == 0)
        {
            if (!string_list_appendf(&fb.prologue, "  call __cert__GC__prep_enter void ()"))
                rc = 1;
        }
    }
    if (fn->body && fn->body->kind == ND_BLOCK)
        rc = ccb_emit_block(&fb, fn->body, false);
    else
        rc = ccb_emit_stmt_basic(&fb, fn->body);
    if (!rc)
        ccb_function_optimize(&fb, opts);
    if (!rc)
    {
        const char *backend_name = fn->metadata.backend_name ? fn->metadata.backend_name : fn->name;
        if (fn->export_name && !fn->raw_export_name)
            backend_name = fn->name;

        if (fn->metadata.func_line)
        {
            if (!ccb_module_append_line(mod, fn->metadata.func_line))
                rc = 1;
        }
        else
        {
            const char *varargs_suffix = fn->is_varargs ? " varargs" : "";
            const char *jump_target_suffix = fn->is_jump_target ? " jump-target" : "";
            const char *hidden_suffix = fn->is_exposed ? "" : " hidden";
            if (!ccb_module_appendf(mod, ".func %s ret=%s params=%zu locals=%zu%s%s%s", backend_name,
                                    cc_type_name(fb.ret_type), fb.param_count, fb.local_count, varargs_suffix, jump_target_suffix, hidden_suffix))
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

    if (rc != 0 && fn && fn->name)
    {
        diag_error_at(fn->src, fn->line, fn->col,
                      "codegen failed while emitting function '%s'", fn->name);
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
    case TY_REF:
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
        if (expr->type && (expr->type->kind == TY_U8 || expr->type->kind == TY_U16 ||
                           expr->type->kind == TY_U32 || expr->type->kind == TY_U64))
            *out_value = (double)expr->int_uval;
        else
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

static bool ccb_make_default_literal_expr(const Type *field_type, const char *spec, Node *out_expr)
{
    if (!spec || !out_expr)
        return false;

    memset(out_expr, 0, sizeof(*out_expr));
    out_expr->type = (Type *)field_type;

    switch (spec[0])
    {
    case 'N':
        out_expr->kind = ND_NULL;
        return true;
    case 'S':
    {
        const char *payload = spec + 1;
        char *endptr = NULL;
        unsigned long long len_value = strtoull(payload, &endptr, 10);
        out_expr->kind = ND_STRING;
        if (endptr && *endptr == ':')
        {
            size_t len = (size_t)len_value;
            char *copy = (char *)xmalloc(len + 1);
            if (len > 0)
                memcpy(copy, endptr + 1, len);
            copy[len] = '\0';
            out_expr->str_data = copy;
            out_expr->str_len = (int)len;
        }
        else
        {
            out_expr->str_data = spec + 1;
            out_expr->str_len = (int)strlen(spec + 1);
        }
        return true;
    }
    case 'I':
        out_expr->kind = ND_INT;
        out_expr->int_val = (int64_t)strtoll(spec + 1, NULL, 10);
        out_expr->int_uval = (uint64_t)out_expr->int_val;
        out_expr->int_is_unsigned = 0;
        return true;
    case 'U':
        out_expr->kind = ND_INT;
        out_expr->int_uval = (uint64_t)strtoull(spec + 1, NULL, 10);
        out_expr->int_val = (int64_t)out_expr->int_uval;
        out_expr->int_is_unsigned = 1;
        return true;
    case 'F':
        out_expr->kind = ND_FLOAT;
        out_expr->float_val = strtod(spec + 1, NULL);
        return true;
    default:
        return false;
    }
}

static bool ccb_struct_has_field_defaults(const Type *struct_type)
{
    if (!struct_type || struct_type->kind != TY_STRUCT || !struct_type->strct.field_default_values)
        return false;
    for (int i = 0; i < struct_type->strct.field_count; ++i)
    {
        if (struct_type->strct.field_default_values[i])
            return true;
    }
    return false;
}

static bool ccb_store_struct_default_bytes(const Type *struct_type, uint8_t *dst, size_t dst_size)
{
    if (!struct_type || struct_type->kind != TY_STRUCT || !dst)
        return false;
    if (!ccb_struct_has_field_defaults(struct_type))
        return true;

    for (int i = 0; i < struct_type->strct.field_count; ++i)
    {
        const char *spec = struct_type->strct.field_default_values ? struct_type->strct.field_default_values[i] : NULL;
        const Type *field_type = struct_type->strct.field_types ? struct_type->strct.field_types[i] : NULL;
        size_t field_size = ccb_type_size_bytes(field_type);
        size_t field_offset = (size_t)(struct_type->strct.field_offsets ? struct_type->strct.field_offsets[i] : 0);
        Node expr = {0};

        if (!spec || !field_type)
            continue;
        if (field_offset + field_size > dst_size)
            return false;
        if (!ccb_make_default_literal_expr(field_type, spec, &expr))
            return false;
        if (!ccb_store_constant_value(field_type, &expr, dst + field_offset, field_size))
            return false;
    }

    return true;
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
    {
        if (type->kind == TY_STRUCT)
            return ccb_store_struct_bytes(type, expr, dst, type_size);
        if (type->kind == TY_ARRAY)
            return ccb_store_array_bytes(type, expr, dst, type_size);
        return true;
    }
    if (expr->kind == ND_INIT_LIST && (expr->init.is_zero || expr->init.count == 0))
    {
        if (type->kind == TY_STRUCT)
            return ccb_store_struct_bytes(type, expr, dst, type_size);
        if (type->kind == TY_ARRAY)
            return ccb_store_array_bytes(type, expr, dst, type_size);
        return true;
    }

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
        return ccb_store_struct_default_bytes(struct_type, dst, dst_size);

    if (!ccb_store_struct_default_bytes(struct_type, dst, dst_size))
        return false;

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
            if (!elem || !ccb_store_constant_value(array_type->array.elem, elem, buffer + (size_t)i * elem_size, elem_size))
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

static bool ccb_is_string_ptr_type(const Type *ty)
{
    while (ty && ty->kind == TY_REF)
        ty = ty->pointee;
    if (!ty || ty->kind != TY_PTR || !ty->pointee)
        return false;
    TypeKind pointee = ty->pointee->kind;
    return (pointee == TY_CHAR || pointee == TY_I8 || pointee == TY_U8);
}

static char *ccb_make_string_symbol(const char *base, int index)
{
    const char *safe = (base && *base) ? base : "str";
    int needed = snprintf(NULL, 0, "__ccb_str_%s_%d", safe, index);
    if (needed < 0)
        return NULL;
    size_t len = (size_t)needed;
    char *name = (char *)malloc(len + 1);
    if (!name)
        return NULL;
    snprintf(name, len + 1, "__ccb_str_%s_%d", safe, index);
    return name;
}

static const char *ccb_module_intern_hidden_byte_string(CcbModule *mod, const char *base, const uint8_t *data, size_t len)
{
    if (!mod)
        return NULL;

    const char *safe = (base && *base) ? base : "str";
    char *literal = ccb_encode_bytes_literal(data, len);
    if (!literal)
        return NULL;

    int key_len = snprintf(NULL, 0, "%s|%s", safe, literal);
    if (key_len < 0)
    {
        free(literal);
        return NULL;
    }

    char *key = (char *)malloc((size_t)key_len + 1);
    if (!key)
    {
        free(literal);
        return NULL;
    }
    snprintf(key, (size_t)key_len + 1, "%s|%s", safe, literal);

    for (size_t i = 0; i < mod->interned_string_keys.count; ++i)
    {
        const char *existing_key = mod->interned_string_keys.items[i];
        if (existing_key && strcmp(existing_key, key) == 0)
        {
            const char *existing_sym = (i < mod->interned_string_symbols.count) ? mod->interned_string_symbols.items[i] : NULL;
            free(key);
            free(literal);
            return existing_sym;
        }
    }

    char *sym = ccb_make_string_symbol(safe, mod->next_interned_string_id++);
    if (!sym)
    {
        free(key);
        free(literal);
        return NULL;
    }

    if (!ccb_module_appendf(mod, ".global %s type=%s size=%zu align=1 data=%s const hidden",
                            sym, cc_type_name(CC_TYPE_U8), len, literal))
    {
        free(sym);
        free(key);
        free(literal);
        return NULL;
    }

    bool ok = string_list_append(&mod->interned_string_keys, key) &&
              string_list_append(&mod->interned_string_symbols, sym);
    free(key);
    free(literal);
    free(sym);
    if (!ok)
        return NULL;

    return mod->interned_string_symbols.items[mod->interned_string_symbols.count - 1];
}

static bool ccb_emit_string_ptr_array_global(CcbModule *mod, const char *name, const Type *array_type, const Node *init,
                                             const char *section_literal, const char *const_attr, const char *hidden_attr)
{
    if (!mod || !name || !array_type || array_type->kind != TY_ARRAY || !init || init->kind != ND_INIT_LIST)
        return false;

    int length = array_type->array.length;
    if (length <= 0)
        return false;

    const Type *elem_type = array_type->array.elem;
    if (!ccb_is_string_ptr_type(elem_type))
        return false;

    for (int i = 0; i < length; ++i)
    {
        const Node *elem = (init->init.elems && i < init->init.count) ? init->init.elems[i] : NULL;
        if (!elem)
            continue;
        if (elem->kind == ND_NULL)
            continue;
        if (elem->kind != ND_STRING)
            return false;
    }

    char **symbols = (char **)calloc((size_t)length, sizeof(char *));
    if (!symbols)
        return false;

    bool ok = true;
    for (int i = 0; i < length && ok; ++i)
    {
        const Node *elem = (init->init.elems && i < init->init.count) ? init->init.elems[i] : NULL;
        if (!elem || elem->kind == ND_NULL)
            continue;
        if (elem->kind != ND_STRING)
        {
            ok = false;
            break;
        }

        char *sym = ccb_make_string_symbol(name, i);
        if (!sym)
        {
            ok = false;
            break;
        }

        size_t str_len = (elem->str_len >= 0) ? (size_t)elem->str_len : 0;
        size_t total_len = str_len + 1;
        uint8_t *bytes = (uint8_t *)malloc(total_len);
        if (!bytes)
        {
            free(sym);
            ok = false;
            break;
        }
        if (str_len > 0 && elem->str_data)
            memcpy(bytes, elem->str_data, str_len);
        bytes[str_len] = 0;

        char *literal = ccb_encode_bytes_literal(bytes, total_len);
        free(bytes);
        if (!literal)
        {
            free(sym);
            ok = false;
            break;
        }

        if (!ccb_module_appendf(mod, ".global %s type=%s size=%zu align=1 data=%s const hidden",
                                sym, cc_type_name(CC_TYPE_U8), total_len, literal))
        {
            free(literal);
            free(sym);
            ok = false;
            break;
        }

        free(literal);
        symbols[i] = sym;
    }

    if (!ok)
    {
        for (int i = 0; i < length; ++i)
            free(symbols[i]);
        free(symbols);
        return false;
    }

    size_t list_len = 0;
    for (int i = 0; i < length; ++i)
    {
        const char *entry = symbols[i] ? symbols[i] : "null";
        list_len += strlen(entry) + 1;
    }

    char *ptr_list = (char *)malloc(list_len + 1);
    if (!ptr_list)
    {
        for (int i = 0; i < length; ++i)
            free(symbols[i]);
        free(symbols);
        return false;
    }

    char *cursor = ptr_list;
    for (int i = 0; i < length; ++i)
    {
        const char *entry = symbols[i] ? symbols[i] : "null";
        size_t entry_len = strlen(entry);
        if (i > 0)
            *cursor++ = ',';
        memcpy(cursor, entry, entry_len);
        cursor += entry_len;
    }
    *cursor = '\0';

    size_t size_bytes = (size_t)length * 8;
    bool emitted = section_literal
                       ? ccb_module_appendf(mod, ".global %s type=%s size=%zu align=8 ptrs=%s section=%s%s%s",
                                            name, cc_type_name(CC_TYPE_PTR), size_bytes, ptr_list, section_literal,
                                            const_attr, hidden_attr)
                       : ccb_module_appendf(mod, ".global %s type=%s size=%zu align=8 ptrs=%s%s%s",
                                            name, cc_type_name(CC_TYPE_PTR), size_bytes, ptr_list, const_attr, hidden_attr);

    free(ptr_list);
    for (int i = 0; i < length; ++i)
        free(symbols[i]);
    free(symbols);

    return emitted;
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
    CcbEntrypointShimKind hosted_entry_kind = CCB_ENTRY_SHIM_NONE;
    const char *hosted_entry_public_name = NULL;
    const char *hosted_entry_hidden_name = NULL;
    const Node *hosted_entry_fn = ccb_prepare_hosted_entrypoint(&mod, unit, opts,
                                                                &hosted_entry_kind,
                                                                &hosted_entry_public_name,
                                                                &hosted_entry_hidden_name);
    ccb_module_collect_defined_functions(&mod, unit);

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
            if (!rc && hosted_entry_fn && hosted_entry_kind != CCB_ENTRY_SHIM_NONE)
            {
                if (ccb_emit_hosted_entrypoint_shim(&mod, hosted_entry_fn, hosted_entry_kind,
                                                    hosted_entry_public_name, hosted_entry_hidden_name))
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
        {
            ccb_module_optimize(&mod, opts);
            write_rc = write_module_to_file(out_path, &mod);
        }
    }

    ccb_module_free(&mod);
    return rc || write_rc;
}
