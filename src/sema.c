#include "ast.h"
#include "module_registry.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <stdint.h>

enum
{
    INLINE_PARAM_LIMIT = 4,
    INLINE_COST_LIMIT = 40,
    INLINE_EVAL_MAX_DEPTH = 16
};

typedef struct
{
    Symbol symbol;
    const char *module_full;
} ImportedFunctionCandidate;

typedef struct ImportedFunctionSet
{
    char *name;
    ImportedFunctionCandidate *candidates;
    int count;
    int cap;
} ImportedFunctionSet;

static void analyze_inline_candidates(Node *root);
static int inline_try_fold_call(Node *call_expr);

static Type *canonicalize_type_deep(Type *ty)
{
    ty = module_registry_canonical_type(ty);
    if (ty && ty->kind == TY_PTR && ty->pointee)
    {
        Type *resolved = canonicalize_type_deep(ty->pointee);
        if (resolved && resolved != ty->pointee)
            ty->pointee = resolved;
    }
    else if (ty && ty->kind == TY_ARRAY && ty->array.elem)
    {
        Type *resolved_elem = canonicalize_type_deep(ty->array.elem);
        if (resolved_elem && resolved_elem != ty->array.elem)
            ty->array.elem = resolved_elem;
    }
    else if (ty && ty->kind == TY_FUNC && ty->func.params)
    {
        for (int i = 0; i < ty->func.param_count; ++i)
        {
            Type *resolved_param = canonicalize_type_deep(ty->func.params[i]);
            if (resolved_param && resolved_param != ty->func.params[i])
                ty->func.params[i] = resolved_param;
        }
        if (ty->func.ret)
        {
            Type *resolved_ret = canonicalize_type_deep(ty->func.ret);
            if (resolved_ret && resolved_ret != ty->func.ret)
                ty->func.ret = resolved_ret;
        }
    }
    return ty;
}

static Type *make_function_type_from_sig(const FuncSig *sig)
{
    if (!sig)
        return NULL;
    Type *fn_ty = type_func();
    fn_ty->func.param_count = sig->param_count;
    if (sig->param_count > 0)
    {
        fn_ty->func.params = (Type **)xcalloc((size_t)sig->param_count, sizeof(Type *));
        for (int i = 0; i < sig->param_count; ++i)
        {
            Type *param = canonicalize_type_deep(sig->params ? sig->params[i] : NULL);
            fn_ty->func.params[i] = param;
        }
    }
    fn_ty->func.ret = canonicalize_type_deep(sig->ret);
    fn_ty->func.is_varargs = sig->is_varargs;
    fn_ty->func.has_signature = 1;
    return fn_ty;
}

struct SymTable
{
    Symbol *items;
    int count;
    int cap;
};

SymTable *symtab_create(void)
{
    SymTable *s = (SymTable *)xcalloc(1, sizeof(SymTable));
    return s;
}
void symtab_destroy(SymTable *st)
{
    if (!st)
        return;
    free(st->items);
    free(st);
}
static int symtab_grow(SymTable *st)
{
    int ncap = st->cap ? st->cap * 2 : 8;
    Symbol *ni = (Symbol *)realloc(st->items, ncap * sizeof(Symbol));
    if (!ni)
        return 0;
    st->items = ni;
    st->cap = ncap;
    return 1;
}
int symtab_add(SymTable *st, Symbol sym)
{
    if (st->count == st->cap && !symtab_grow(st))
        return 0;
    st->items[st->count++] = sym;
    return 1;
}
const Symbol *symtab_get(SymTable *st, const char *name)
{
    for (int i = 0; i < st->count; i++)
    {
        if (strcmp(st->items[i].name, name) == 0)
            return &st->items[i];
    }
    return NULL;
}

static int symtab_has_symbol_with_prefix(SymTable *st, const char *prefix)
{
    if (!st || !prefix || !*prefix)
        return 0;
    size_t prefix_len = strlen(prefix);
    for (int i = 0; i < st->count; ++i)
    {
        const char *candidate = st->items[i].name;
        if (!candidate)
            continue;
        if (strncmp(candidate, prefix, prefix_len) == 0 && candidate[prefix_len] == '.')
            return 1;
    }
    return 0;
}

static ImportedFunctionSet *sema_find_imported_function_set(SemaContext *sc, const char *name)
{
    if (!sc || !name || !sc->imported_funcs)
        return NULL;
    for (int i = 0; i < sc->imported_func_count; ++i)
    {
        if (sc->imported_funcs[i].name && strcmp(sc->imported_funcs[i].name, name) == 0)
            return &sc->imported_funcs[i];
    }
    return NULL;
}

static ImportedFunctionSet *sema_ensure_imported_function_set(SemaContext *sc, const char *name)
{
    if (!sc || !name)
        return NULL;
    ImportedFunctionSet *set = sema_find_imported_function_set(sc, name);
    if (set)
        return set;
    if (sc->imported_func_count == sc->imported_func_cap)
    {
        int new_cap = sc->imported_func_cap ? sc->imported_func_cap * 2 : 4;
        ImportedFunctionSet *grown = (ImportedFunctionSet *)realloc(sc->imported_funcs, (size_t)new_cap * sizeof(ImportedFunctionSet));
        if (!grown)
        {
            diag_error("out of memory while tracking imported functions");
            exit(1);
        }
        for (int i = sc->imported_func_cap; i < new_cap; ++i)
        {
            grown[i].name = NULL;
            grown[i].candidates = NULL;
            grown[i].count = 0;
            grown[i].cap = 0;
        }
        sc->imported_funcs = grown;
        sc->imported_func_cap = new_cap;
    }
    set = &sc->imported_funcs[sc->imported_func_count++];
    set->name = xstrdup(name);
    set->candidates = NULL;
    set->count = 0;
    set->cap = 0;
    return set;
}

static int funcsig_equal(const FuncSig *a, const FuncSig *b);

static void sema_imported_function_insert(SemaContext *sc, const char *name, const char *module_full, const Symbol *symbol)
{
    if (!sc || !name || !symbol)
        return;
    ImportedFunctionSet *set = sema_ensure_imported_function_set(sc, name);
    for (int i = 0; i < set->count; ++i)
    {
        if (funcsig_equal(&set->candidates[i].symbol.sig, &symbol->sig))
        {
            const char *existing_mod = set->candidates[i].module_full ? set->candidates[i].module_full : "<unknown>";
            const char *incoming_mod = module_full ? module_full : "<unknown>";
            diag_error("ambiguous import: function '%s' from module '%s' conflicts with module '%s'", name, existing_mod, incoming_mod);
            exit(1);
        }
    }
    if (set->count == set->cap)
    {
        int new_cap = set->cap ? set->cap * 2 : 4;
        ImportedFunctionCandidate *grown = (ImportedFunctionCandidate *)realloc(set->candidates, (size_t)new_cap * sizeof(ImportedFunctionCandidate));
        if (!grown)
        {
            diag_error("out of memory while tracking imported function overloads");
            exit(1);
        }
        set->candidates = grown;
        set->cap = new_cap;
    }
    ImportedFunctionCandidate *slot = &set->candidates[set->count++];
    slot->symbol = *symbol;
    slot->module_full = module_full;
}

void sema_track_imported_function(SemaContext *sc, const char *name, const char *module_full, const Symbol *symbol)
{
    sema_imported_function_insert(sc, name, module_full, symbol);
}

static const ImportedFunctionCandidate *sema_get_unique_imported_candidate(SemaContext *sc, const char *name, int emit_error)
{
    ImportedFunctionSet *set = sema_find_imported_function_set(sc, name);
    if (!set)
        return NULL;
    if (set->count == 1)
        return &set->candidates[0];
    if (emit_error)
    {
        if (set->count >= 2)
        {
            const char *mod_a = set->candidates[0].module_full ? set->candidates[0].module_full : "<unknown>";
            const char *mod_b = set->candidates[1].module_full ? set->candidates[1].module_full : "<unknown>";
            diag_error("ambiguous reference to function '%s'; candidates exist in modules '%s' and '%s'", name, mod_a, mod_b);
            exit(1);
        }
    }
    return NULL;
}

static ImportedFunctionCandidate *sema_resolve_imported_call(SemaContext *sc, const char *name, Node *call_expr, int *args_checked);
static int call_arguments_match_signature(Node *call_expr, const FuncSig *sig);
static void check_expr(SemaContext *sc, Node *e);
static int can_assign(Type *target, Node *rhs);

struct VarBind
{
    const char *name;
    Type *type;
    int is_const;
};
struct Scope
{
    struct VarBind locals[128];
    int local_count;
    struct Scope *parent;
};
struct SemaContext
{
    SymTable *syms;
    struct Scope *scope;
    const struct Node *unit;
    ImportedFunctionSet *imported_funcs;
    int imported_func_count;
    int imported_func_cap;
};
SemaContext *sema_create(void)
{
    SemaContext *sc = (SemaContext *)xcalloc(1, sizeof(SemaContext));
    sc->syms = symtab_create();
    sc->unit = NULL;
    sc->imported_funcs = NULL;
    sc->imported_func_count = 0;
    sc->imported_func_cap = 0;
    return sc;
}
void sema_destroy(SemaContext *sc)
{
    if (!sc)
        return;
    if (sc->imported_funcs)
    {
        for (int i = 0; i < sc->imported_func_count; ++i)
        {
            free(sc->imported_funcs[i].name);
            free(sc->imported_funcs[i].candidates);
        }
        free(sc->imported_funcs);
    }
    symtab_destroy(sc->syms);
    free(sc);
}

static Type ty_i8 = {.kind = TY_I8};
static Type ty_u8 = {.kind = TY_U8};
static Type ty_i16 = {.kind = TY_I16};
static Type ty_u16 = {.kind = TY_U16};
static Type ty_i32 = {.kind = TY_I32};
static Type ty_u32 = {.kind = TY_U32};
static Type ty_i64 = {.kind = TY_I64};
static Type ty_u64 = {.kind = TY_U64};
static Type ty_f32 = {.kind = TY_F32};
static Type ty_f64 = {.kind = TY_F64};
static Type ty_f128 = {.kind = TY_F128};
static Type ty_void = {.kind = TY_VOID};
static Type ty_char = {.kind = TY_CHAR};
static Type ty_bool = {.kind = TY_BOOL};
static Type ty_va_list = {.kind = TY_VA_LIST};
static Type ty_module_placeholder = {.kind = TY_IMPORT};

static Type *metadata_token_to_type(const char *token)
{
    if (!token)
        return NULL;
    if (strcmp(token, "i1") == 0 || strcmp(token, "bool") == 0)
        return &ty_bool;
    if (strcmp(token, "i8") == 0)
        return &ty_i8;
    if (strcmp(token, "u8") == 0)
        return &ty_u8;
    if (strcmp(token, "i16") == 0)
        return &ty_i16;
    if (strcmp(token, "u16") == 0)
        return &ty_u16;
    if (strcmp(token, "i32") == 0)
        return &ty_i32;
    if (strcmp(token, "u32") == 0)
        return &ty_u32;
    if (strcmp(token, "i64") == 0)
        return &ty_i64;
    if (strcmp(token, "u64") == 0)
        return &ty_u64;
    if (strcmp(token, "f32") == 0)
        return &ty_f32;
    if (strcmp(token, "f64") == 0)
        return &ty_f64;
    if (strcmp(token, "f128") == 0)
        return &ty_f128;
    if (strcmp(token, "void") == 0)
        return &ty_void;
    if (strcmp(token, "char") == 0)
        return &ty_char;
    return NULL;
}

static char *make_qualified_name(const ModulePath *mod, const char *name)
{
    if (!mod || mod->part_count == 0 || !mod->full_name || !name || !*name)
        return NULL;
    size_t base_len = strlen(mod->full_name);
    size_t name_len = strlen(name);
    char *res = (char *)xmalloc(base_len + 1 + name_len + 1);
    memcpy(res, mod->full_name, base_len);
    res[base_len] = '.';
    memcpy(res + base_len + 1, name, name_len);
    res[base_len + 1 + name_len] = '\0';
    return res;
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

static char *module_backend_name(const char *module_full, const char *fn_name)
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
    return res;
}

static char *backend_from_qualified(const char *qualified_name)
{
    if (!qualified_name)
        return NULL;
    const char *dot = strrchr(qualified_name, '.');
    if (!dot || dot == qualified_name || *(dot + 1) == '\0')
        return xstrdup(qualified_name);
    size_t module_len = (size_t)(dot - qualified_name);
    char *module = (char *)xmalloc(module_len + 1);
    memcpy(module, qualified_name, module_len);
    module[module_len] = '\0';
    const char *fn = dot + 1;
    char *backend = module_backend_name(module, fn);
    free(module);
    if (!backend)
        return xstrdup(qualified_name);
    return backend;
}

static char *resolve_import_alias(const Node *unit, const char *call_name)
{
    if (!unit || unit->kind != ND_UNIT || !call_name)
        return NULL;
    const char *dot = strchr(call_name, '.');
    if (!dot || dot == call_name)
        return NULL;
    size_t alias_len = (size_t)(dot - call_name);
    const char *rest = dot + 1;
    if (!rest || *rest == '\0')
        return NULL;
    if (unit->imports && unit->import_count > 0)
    {
        for (int i = 0; i < unit->import_count; ++i)
        {
            const ModulePath *imp = &unit->imports[i];
            if (!imp)
                continue;
            if (imp->alias)
            {
                size_t alias_name_len = strlen(imp->alias);
                if (alias_name_len == alias_len && strncmp(imp->alias, call_name, alias_len) == 0)
                {
                    size_t full_len = strlen(imp->full_name);
                    size_t rest_len = strlen(rest);
                    char *combined = (char *)xmalloc(full_len + 1 + rest_len + 1);
                    memcpy(combined, imp->full_name, full_len);
                    combined[full_len] = '.';
                    memcpy(combined + full_len + 1, rest, rest_len + 1);
                    return combined;
                }
            }
        }
    }
    return NULL;
}

static int module_name_matches(const char *full, const char *candidate, size_t len)
{
    if (!full || !candidate)
        return 0;
    return strlen(full) == len && strncmp(full, candidate, len) == 0;
}

static int unit_allows_module_call(const Node *unit, const char *qualified_name)
{
    if (!unit || unit->kind != ND_UNIT || !qualified_name)
        return 0;
    const char *dot = strrchr(qualified_name, '.');
    if (!dot || dot == qualified_name)
        return 0;
    size_t module_len = (size_t)(dot - qualified_name);
    if (module_name_matches(unit->module_path.full_name, qualified_name, module_len))
        return 1;
    if (unit->imports && unit->import_count > 0)
    {
        for (int i = 0; i < unit->import_count; ++i)
        {
            const ModulePath *imp = &unit->imports[i];
            if (imp->alias)
            {
                size_t alias_len = strlen(imp->alias);
                if (alias_len == module_len && strncmp(imp->alias, qualified_name, module_len) == 0)
                    return 1;
            }
            if (module_name_matches(imp->full_name, qualified_name, module_len))
                return 1;
        }
    }
    return 0;
}

typedef enum
{
    IMPORT_STYLE_NONE = 0,
    IMPORT_STYLE_ALIAS = 1,
    IMPORT_STYLE_AUTO = 2
} ImportStyle;

static ImportStyle unit_import_style(const Node *unit, const char *module_full)
{
    if (!unit || unit->kind != ND_UNIT || !module_full || !*module_full)
        return IMPORT_STYLE_NONE;
    if (!unit->imports || unit->import_count <= 0)
        return IMPORT_STYLE_NONE;
    for (int i = 0; i < unit->import_count; ++i)
    {
        const ModulePath *imp = &unit->imports[i];
        if (!imp || !imp->full_name)
            continue;
        if (strcmp(imp->full_name, module_full) == 0)
        {
            if (imp->alias && imp->alias[0])
                return IMPORT_STYLE_ALIAS;
            return IMPORT_STYLE_AUTO;
        }
    }
    return IMPORT_STYLE_NONE;
}

static const ModulePath *unit_find_import_for_ident(const Node *unit, const char *name, int *parts_consumed)
{
    if (parts_consumed)
        *parts_consumed = 0;
    if (!unit || unit->kind != ND_UNIT || !name)
        return NULL;
    if (!unit->imports || unit->import_count <= 0)
        return NULL;
    for (int i = 0; i < unit->import_count; ++i)
    {
        const ModulePath *imp = &unit->imports[i];
        if (!imp)
            continue;
        if (imp->alias && strcmp(imp->alias, name) == 0)
        {
            if (parts_consumed)
                *parts_consumed = imp->part_count;
            return imp;
        }
        if (!imp->alias && imp->part_count > 0 && imp->parts && imp->parts[0] && strcmp(imp->parts[0], name) == 0)
        {
            if (parts_consumed)
                *parts_consumed = 1;
            return imp;
        }
    }
    return NULL;
}

static int type_is_int(Type *t)
{
    t = canonicalize_type_deep(t);
    if (!t)
        return 0;
    switch (t->kind)
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
        return 1;
    default:
        return 0;
    }
}

static int type_is_builtin(const Type *t)
{
    if (!t)
        return 1;
    switch (t->kind)
    {
    case TY_I8:
    case TY_U8:
    case TY_I16:
    case TY_U16:
    case TY_I32:
    case TY_U32:
    case TY_I64:
    case TY_U64:
    case TY_F32:
    case TY_F64:
    case TY_F128:
    case TY_VOID:
    case TY_CHAR:
    case TY_BOOL:
    case TY_VA_LIST:
        return 1;
    default:
        return 0;
    }
}

static ImportedFunctionCandidate *sema_resolve_imported_call(SemaContext *sc, const char *name, Node *call_expr, int *args_checked)
{
    if (!sc || !name || !call_expr)
        return NULL;
    if (strchr(name, '.'))
        return NULL;
    ImportedFunctionSet *set = sema_find_imported_function_set(sc, name);
    if (!set)
        return NULL;

    if (!args_checked || !*args_checked)
    {
        for (int i = 0; i < call_expr->arg_count; ++i)
        {
            if (call_expr->args && call_expr->args[i])
                check_expr(sc, call_expr->args[i]);
        }
        if (args_checked)
            *args_checked = 1;
    }

    ImportedFunctionCandidate *match = NULL;
    for (int ci = 0; ci < set->count; ++ci)
    {
        ImportedFunctionCandidate *cand = &set->candidates[ci];
        const FuncSig *sig = &cand->symbol.sig;
        int provided = call_expr->arg_count;
        int expected = sig->param_count;
        if (!sig->is_varargs)
        {
            if (provided != expected)
                continue;
        }
        else if (provided < expected)
        {
            continue;
        }

        int ok = 1;
        for (int pi = 0; pi < expected && ok; ++pi)
        {
            if (!sig->params || pi >= sig->param_count)
                continue;
            Type *expected_ty = sig->params[pi];
            if (!expected_ty)
                continue;
            if (!call_expr->args || !call_expr->args[pi])
            {
                ok = 0;
                break;
            }
            Node *arg_node = call_expr->args[pi];
            Type *saved_type = arg_node->type;
            int64_t saved_int = arg_node->int_val;
            if (!can_assign(expected_ty, arg_node))
                ok = 0;
            arg_node->type = saved_type;
            arg_node->int_val = saved_int;
        }
        if (!ok)
            continue;

        if (!match)
        {
            match = cand;
            continue;
        }

        if (funcsig_equal(&match->symbol.sig, &cand->symbol.sig))
        {
            const char *mod_a = match->module_full ? match->module_full : "<unknown>";
            const char *mod_b = cand->module_full ? cand->module_full : "<unknown>";
            diag_error_at(call_expr->src, call_expr->line, call_expr->col,
                          "ambiguous call to '%s'; both modules '%s' and '%s' provide identical overloads",
                          name, mod_a, mod_b);
            exit(1);
        }
        else
        {
            const char *mod_a = match->module_full ? match->module_full : "<unknown>";
            const char *mod_b = cand->module_full ? cand->module_full : "<unknown>";
            diag_error_at(call_expr->src, call_expr->line, call_expr->col,
                          "ambiguous call to '%s'; matches found in modules '%s' and '%s'",
                          name, mod_a, mod_b);
            exit(1);
        }
    }

    if (!match)
    {
        diag_error_at(call_expr->src, call_expr->line, call_expr->col,
                      "no overload of '%s' matches provided arguments", name);
        if (set->count > 0)
        {
            for (int i = 0; i < set->count; ++i)
            {
                const char *mod = set->candidates[i].module_full ? set->candidates[i].module_full : "<unknown>";
                diag_note_at(call_expr->src, call_expr->line, call_expr->col,
                             "candidate: %s.%s", mod, set->candidates[i].symbol.name);
            }
        }
        exit(1);
    }

    return match;
}

static void describe_type(const Type *t, char *buf, size_t bufsz)
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
    default:
        snprintf(buf, bufsz, "type kind %d", t->kind);
        return;
    }
}

static int type_is_exportable(const Type *t)
{
    if (!t)
        return 1;
    if (type_is_builtin(t))
        return 1;
    if (t->kind == TY_PTR)
        return 1;
    if (t->kind == TY_ARRAY)
        return t->array.elem ? type_is_exportable(t->array.elem) : 0;
    if (t->kind == TY_STRUCT)
        return t->is_exposed != 0;
    return 1;
}

static int check_exposed_function_signature(const Node *fn)
{
    if (!fn || fn->kind != ND_FUNC || !fn->is_exposed)
        return 0;
    Type *ret_type = fn->ret_type ? fn->ret_type : &ty_i32;
    if (!type_is_exportable(ret_type))
    {
        char buf[128];
        describe_type(ret_type, buf, sizeof(buf));
        diag_error_at(fn->src, fn->line, fn->col,
                      "exposed function '%s' cannot return hidden type '%s'",
                      fn->name ? fn->name : "<unnamed>", buf);
        return 1;
    }
    for (int i = 0; i < fn->param_count; i++)
    {
        Type *param_type = fn->param_types ? fn->param_types[i] : NULL;
        if (!type_is_exportable(param_type))
        {
            char buf[128];
            describe_type(param_type, buf, sizeof(buf));
            const char *param_name = (fn->param_names && fn->param_names[i]) ? fn->param_names[i] : "<param>";
            diag_error_at(fn->src, fn->line, fn->col,
                          "exposed function '%s' parameter '%s' uses hidden type '%s'",
                          fn->name ? fn->name : "<unnamed>", param_name, buf);
            return 1;
        }
    }
    return 0;
}
static int type_equal(Type *a, Type *b)
{
    a = canonicalize_type_deep(a);
    b = canonicalize_type_deep(b);
    if (a == b)
        return 1;
    if (!a || !b)
        return 0;
    if (a->kind != b->kind)
        return 0;
    if (a->kind == TY_PTR)
        return type_equal(a->pointee, b->pointee);
    if (a->kind == TY_ARRAY)
    {
        if (a->array.is_unsized != b->array.is_unsized)
            return 0;
        if (!a->array.is_unsized && a->array.length != b->array.length)
            return 0;
        return type_equal(a->array.elem, b->array.elem);
    }
    if (a->kind == TY_FUNC)
    {
        if (a->func.param_count != b->func.param_count)
            return 0;
        if (a->func.is_varargs != b->func.is_varargs)
            return 0;
        if (!!a->func.ret != !!b->func.ret)
            return 0;
        if (a->func.ret && !type_equal(a->func.ret, b->func.ret))
            return 0;
        for (int i = 0; i < a->func.param_count; ++i)
        {
            Type *ap = a->func.params ? a->func.params[i] : NULL;
            Type *bp = b->func.params ? b->func.params[i] : NULL;
            if (!type_equal(ap, bp))
                return 0;
        }
        return 1;
    }
    if (a->kind == TY_STRUCT)
    {
        if (a->struct_name && b->struct_name)
            return strcmp(a->struct_name, b->struct_name) == 0;
        return a == b;
    }
    return 1;
}

static int funcsig_equal(const FuncSig *a, const FuncSig *b)
{
    if (a == b)
        return 1;
    if (!a || !b)
        return 0;
    if (a->param_count != b->param_count)
        return 0;
    if (a->is_varargs != b->is_varargs)
        return 0;
    if (!!a->ret != !!b->ret)
        return 0;
    if (a->ret && !type_equal(a->ret, b->ret))
        return 0;
    for (int i = 0; i < a->param_count; ++i)
    {
        Type *ap = (a->params && i < a->param_count) ? a->params[i] : NULL;
        Type *bp = (b->params && i < b->param_count) ? b->params[i] : NULL;
        if (!type_equal(ap, bp))
            return 0;
    }
    return 1;
}

static int sizeof_type_bytes(Type *ty)
{
    ty = canonicalize_type_deep(ty);
    if (!ty)
        return 8;
    switch (ty->kind)
    {
    case TY_I8:
    case TY_U8:
    case TY_CHAR:
        return 1;
    case TY_BOOL:
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
    case TY_ARRAY:
        if (ty->array.is_unsized)
            return 8;
        if (ty->array.length <= 0)
            return 0;
        return ty->array.length * sizeof_type_bytes(ty->array.elem);
    case TY_VOID:
        return 0;
    default:
        return 8;
    }
}

static void populate_symbol_from_function(Symbol *s, Node *fn)
{
    if (!s || !fn || fn->kind != ND_FUNC)
        return;

    s->kind = SYM_FUNC;
    s->name = fn->name;
    s->backend_name = fn->metadata.backend_name ? fn->metadata.backend_name : fn->name;
    s->is_extern = 0;
    s->abi = "C";
    s->sig.is_varargs = fn->is_varargs ? 1 : 0;
    s->ast_node = fn;

    Type *decl_ret = fn->ret_type ? fn->ret_type : &ty_i32;
    if (fn->metadata.ret_token)
    {
        Type *meta_ret = metadata_token_to_type(fn->metadata.ret_token);
        if (!meta_ret)
        {
            meta_ret = decl_ret;
            diag_warning_at(fn->src, fn->line, fn->col,
                            "unsupported metadata return type '%s'; using declared return type",
                            fn->metadata.ret_token);
        }
        if (!type_equal(decl_ret, meta_ret))
        {
            char decl_buf[64];
            char meta_buf[64];
            describe_type(decl_ret, decl_buf, sizeof(decl_buf));
            describe_type(meta_ret, meta_buf, sizeof(meta_buf));
            diag_error_at(fn->src, fn->line, fn->col,
                          "return type mismatch between declaration ('%s') and metadata ('%s')",
                          decl_buf, meta_buf);
            exit(1);
        }
        s->sig.ret = meta_ret;
    }
    else
    {
        s->sig.ret = decl_ret;
    }

    if (fn->metadata.params_line)
    {
        int count = fn->metadata.param_type_count;
        s->sig.param_count = count;
        if (count > 0)
        {
            Type **meta_params = (Type **)xcalloc((size_t)count, sizeof(Type *));
            for (int i = 0; i < count; ++i)
            {
                const char *tok = (fn->metadata.param_type_names && i < count) ? fn->metadata.param_type_names[i] : NULL;
                Type *meta_ty = metadata_token_to_type(tok);
                if (!meta_ty)
                {
                    if (fn->param_types && i < fn->param_count)
                        meta_ty = fn->param_types[i];
                    diag_warning_at(fn->src, fn->line, fn->col,
                                    "unsupported metadata parameter type '%s'; using declared type for parameter %d",
                                    tok ? tok : "<null>", i + 1);
                }
                if (!meta_ty)
                {
                    diag_error_at(fn->src, fn->line, fn->col,
                                  "unable to determine type for parameter %d", i + 1);
                    exit(1);
                }
                meta_params[i] = meta_ty;
            }
            s->sig.params = meta_params;
        }
        else
        {
            s->sig.params = NULL;
        }
    }
    else
    {
        s->sig.param_count = fn->param_count;
        s->sig.params = fn->param_types;
    }
}

static void populate_symbol_from_global(Symbol *s, Node *decl)
{
    if (!s || !decl || decl->kind != ND_VAR_DECL)
        return;

    s->kind = SYM_GLOBAL;
    s->name = decl->var_name;
    s->backend_name = (decl->metadata.backend_name && decl->metadata.backend_name[0])
                          ? decl->metadata.backend_name
                          : decl->var_name;
    s->is_extern = 0;
    s->abi = "C";
    s->sig.ret = NULL;
    s->sig.params = NULL;
    s->sig.param_count = 0;
    s->sig.is_varargs = 0;
    s->is_noreturn = 0;
    s->var_type = decl->var_type;
    s->is_const = decl->var_is_const;
    s->ast_node = decl;
}

static void sema_register_function_local(SemaContext *sc, Node *unit_node, Node *fn)
{
    if (!sc || !sc->syms || !fn || fn->kind != ND_FUNC)
        return;

    const char *module_full = NULL;
    if (unit_node && unit_node->kind == ND_UNIT)
        module_full = unit_node->module_path.full_name;

    if (module_full && !fn->metadata.backend_name && !fn->is_entrypoint)
    {
        char *backend = module_backend_name(module_full, fn->name);
        if (backend)
            fn->metadata.backend_name = backend;
    }

    Symbol s = {0};
    populate_symbol_from_function(&s, fn);
    symtab_add(sc->syms, s);

    if (fn->metadata.backend_name && strcmp(fn->metadata.backend_name, fn->name) != 0)
    {
        Symbol backend_alias = s;
        backend_alias.name = fn->metadata.backend_name;
        backend_alias.backend_name = s.backend_name;
        symtab_add(sc->syms, backend_alias);
    }

    if (fn->is_exposed && module_full)
    {
        char *qualified = make_qualified_name(&unit_node->module_path, fn->name);
        if (qualified)
        {
            Symbol alias = s;
            alias.name = qualified;
            alias.backend_name = s.backend_name;
            symtab_add(sc->syms, alias);
        }
    }
}

static void sema_register_global_local(SemaContext *sc, Node *unit_node, Node *decl)
{
    if (!sc || !sc->syms || !decl || decl->kind != ND_VAR_DECL || !decl->var_is_global)
        return;

    const char *module_full = NULL;
    if (unit_node && unit_node->kind == ND_UNIT)
        module_full = unit_node->module_path.full_name;

    if (module_full && !decl->metadata.backend_name)
    {
        char *backend = module_backend_name(module_full, decl->var_name);
        if (backend)
            decl->metadata.backend_name = backend;
    }

    decl->var_type = canonicalize_type_deep(decl->var_type);

    Symbol s = {0};
    populate_symbol_from_global(&s, decl);

    if (!s.name)
    {
        diag_error_at(decl->src, decl->line, decl->col,
                      "global variable missing name");
        exit(1);
    }

    const Symbol *existing = symtab_get(sc->syms, s.name);
    if (existing)
    {
        diag_error_at(decl->src, decl->line, decl->col,
                      "duplicate symbol '%s'", s.name);
        exit(1);
    }

    symtab_add(sc->syms, s);

    if (s.backend_name && strcmp(s.backend_name, s.name) != 0)
    {
        Symbol alias = s;
        alias.name = s.backend_name;
        symtab_add(sc->syms, alias);
    }

    if (decl->is_exposed && module_full)
    {
        char *qualified = make_qualified_name(&unit_node->module_path, decl->var_name);
        if (qualified)
        {
            Symbol alias = s;
            alias.name = qualified;
            symtab_add(sc->syms, alias);
        }
    }
}

static void sema_register_function_foreign(SemaContext *sc, const Node *unit_node, Node *fn, int auto_import)
{
    if (!sc || !sc->syms || !unit_node || unit_node->kind != ND_UNIT || !fn || fn->kind != ND_FUNC)
        return;
    if (!fn->is_exposed)
        return;

    const char *module_full = unit_node->module_path.full_name;
    if (!module_full || !*module_full)
        return;

    if (!fn->metadata.backend_name && !fn->is_entrypoint)
    {
        char *backend = module_backend_name(module_full, fn->name);
        if (backend)
            fn->metadata.backend_name = backend;
    }

    Symbol s = {0};
    populate_symbol_from_function(&s, fn);
    s.is_extern = 1;

    if (auto_import)
        sema_imported_function_insert(sc, fn->name, module_full, &s);

    if (fn->metadata.backend_name && strcmp(fn->metadata.backend_name, fn->name) != 0)
    {
        Symbol backend_alias = s;
        backend_alias.name = fn->metadata.backend_name;
        backend_alias.backend_name = s.backend_name;
        symtab_add(sc->syms, backend_alias);
    }

    char *qualified = make_qualified_name(&unit_node->module_path, fn->name);
    if (qualified)
    {
        Symbol alias = s;
        alias.name = qualified;
        alias.backend_name = s.backend_name;
        symtab_add(sc->syms, alias);
    }
}

static void sema_register_global_foreign(SemaContext *sc, const Node *unit_node, Node *decl)
{
    if (!sc || !sc->syms || !unit_node || unit_node->kind != ND_UNIT || !decl || decl->kind != ND_VAR_DECL || !decl->var_is_global)
        return;
    if (!decl->is_exposed)
        return;

    const char *module_full = unit_node->module_path.full_name;
    if (!module_full || !*module_full)
        return;

    if (!decl->metadata.backend_name)
    {
        char *backend = module_backend_name(module_full, decl->var_name);
        if (backend)
            decl->metadata.backend_name = backend;
    }

    decl->var_type = canonicalize_type_deep(decl->var_type);

    Symbol s = {0};
    populate_symbol_from_global(&s, decl);
    s.is_extern = 1;

    symtab_add(sc->syms, s);

    if (s.backend_name && strcmp(s.backend_name, s.name) != 0)
    {
        Symbol alias = s;
        alias.name = s.backend_name;
        symtab_add(sc->syms, alias);
    }

    if (decl->is_exposed)
    {
        char *qualified = make_qualified_name(&unit_node->module_path, decl->var_name);
        if (qualified)
        {
            Symbol alias = s;
            alias.name = qualified;
            symtab_add(sc->syms, alias);
        }
    }
}

static int struct_find_field(Type *st, const char *name)
{
    if (!st || st->kind != TY_STRUCT || !name)
        return -1;
    for (int i = 0; i < st->strct.field_count; i++)
    {
        if (st->strct.field_names &&
            st->strct.field_names[i] &&
            strcmp(st->strct.field_names[i], name) == 0)
            return i;
    }
    return -1;
}

static void check_expr(SemaContext *sc, Node *e);

static int type_is_signed_int(Type *t)
{
    t = canonicalize_type_deep(t);
    if (!t)
        return 0;
    switch (t->kind)
    {
    case TY_I8:
    case TY_I16:
    case TY_I32:
    case TY_I64:
    case TY_CHAR:
        return 1;
    default:
        return 0;
    }
}

static int type_is_unsigned_int(Type *t)
{
    t = canonicalize_type_deep(t);
    if (!t)
        return 0;
    switch (t->kind)
    {
    case TY_U8:
    case TY_U16:
    case TY_U32:
    case TY_U64:
    case TY_BOOL:
        return 1;
    default:
        return 0;
    }
}

static int type_is_float(Type *t)
{
    t = canonicalize_type_deep(t);
    if (!t)
        return 0;
    switch (t->kind)
    {
    case TY_F32:
    case TY_F64:
    case TY_F128:
        return 1;
    default:
        return 0;
    }
}

static int type_is_pointer(Type *t)
{
    t = canonicalize_type_deep(t);
    if (!t)
        return 0;
    if (t->kind == TY_PTR)
        return 1;
    if (t->kind == TY_ARRAY && t->array.is_unsized)
        return 1;
    return 0;
}

static int64_t type_int_min(Type *t)
{
    if (!t)
        return 0;
    switch (t->kind)
    {
    case TY_I8:
        return INT8_MIN;
    case TY_I16:
        return INT16_MIN;
    case TY_I32:
        return INT32_MIN;
    case TY_I64:
        return INT64_MIN;
    case TY_CHAR:
        return SCHAR_MIN;
    case TY_BOOL:
        return 0;
    default:
        return 0;
    }
}

static int64_t type_int_max(Type *t)
{
    if (!t)
        return 0;
    switch (t->kind)
    {
    case TY_I8:
        return INT8_MAX;
    case TY_I16:
        return INT16_MAX;
    case TY_I32:
        return INT32_MAX;
    case TY_I64:
        return INT64_MAX;
    case TY_U8:
        return UINT8_MAX;
    case TY_BOOL:
        return 1;
    case TY_U16:
        return UINT16_MAX;
    case TY_U32:
        return UINT32_MAX;
    case TY_U64:
        return INT64_MAX;
    case TY_CHAR:
        return SCHAR_MAX;
    default:
        return 0;
    }
}

static int type_bit_width(Type *t)
{
    t = canonicalize_type_deep(t);
    if (!t)
        return 0;
    switch (t->kind)
    {
    case TY_BOOL:
        return 1;
    case TY_I8:
    case TY_U8:
    case TY_CHAR:
        return 8;
    case TY_I16:
    case TY_U16:
        return 16;
    case TY_I32:
    case TY_U32:
        return 32;
    case TY_I64:
    case TY_U64:
        return 64;
    default:
        return 0;
    }
}

static int coerce_int_literal_to_type(Node *literal, Type *target, const char *context)
{
    if (!literal || !target)
        return 0;
    if (literal->kind != ND_INT)
        return 0;

    Type *canon_target = canonicalize_type_deep(target);
    if (!type_is_int(canon_target))
        return 0;

    int64_t original = literal->int_val;
    int64_t coerced = original;
    int warn = 0;

    int64_t min = type_is_unsigned_int(canon_target) ? 0 : type_int_min(canon_target);
    int64_t max = type_int_max(canon_target);

    if (type_is_unsigned_int(canon_target))
    {
        if (original < 0 || (uint64_t)original > (uint64_t)max)
            warn = 1;
    }
    else if (type_is_signed_int(canon_target))
    {
        if (original < min || original > max)
            warn = 1;
    }

    int bits = type_bit_width(canon_target);
    if ((warn || literal->type != canon_target) && bits > 0 && bits < 64)
    {
        uint64_t mask = (1ULL << bits) - 1ULL;
        uint64_t truncated = ((uint64_t)original) & mask;
        if (type_is_unsigned_int(canon_target))
        {
            coerced = (int64_t)truncated;
        }
        else
        {
            uint64_t sign_bit = 1ULL << (bits - 1);
            if (truncated & sign_bit)
                truncated |= ~mask;
            coerced = (int64_t)truncated;
        }
    }
    else if (warn)
    {
        if (type_is_unsigned_int(canon_target))
        {
            if (original < 0)
                coerced = 0;
            else if (original > max)
                coerced = max;
        }
        else
        {
            if (original < min)
                coerced = min;
            else if (original > max)
                coerced = max;
        }
    }

    literal->int_val = coerced;
    literal->type = canon_target;

    if (warn)
    {
        char ty_name[64];
        describe_type(canon_target, ty_name, sizeof(ty_name));
        if (context && *context)
        {
            diag_warning_at(literal->src, literal->line, literal->col,
                            "integer literal %lld does not fit in %s; value converted for %s",
                            (long long)original, ty_name, context);
        }
        else
        {
            diag_warning_at(literal->src, literal->line, literal->col,
                            "integer literal %lld does not fit in %s; value converted",
                            (long long)original, ty_name);
        }
    }

    return 1;
}

static int can_assign(Type *target, Node *rhs)
{
    if (!target || !rhs)
        return 0;
    Type *canon_target = canonicalize_type_deep(target);
    if (!canon_target)
        return 0;
    if (canon_target->kind == TY_PTR && rhs->kind == ND_NULL)
    {
        rhs->type = canon_target;
        return 1;
    }
    if (rhs->type && type_equal(canon_target, rhs->type))
        return 1;
    if (rhs->kind == ND_COND)
    {
        if (!rhs->rhs || !rhs->body)
            return 0;
        if (!can_assign(canon_target, rhs->rhs))
            return 0;
        if (!can_assign(canon_target, rhs->body))
            return 0;
        rhs->type = canon_target;
        return 1;
    }
    if (coerce_int_literal_to_type(rhs, canon_target, "assignment"))
        return 1;
    return 0;
}

static int call_arguments_match_signature(Node *call_expr, const FuncSig *sig)
{
    if (!call_expr || !sig)
        return 0;
    int provided = call_expr->arg_count;
    int expected = sig->param_count;
    if (!sig->is_varargs)
    {
        if (provided != expected)
            return 0;
    }
    else if (provided < expected)
    {
        return 0;
    }
    for (int i = 0; i < expected; ++i)
    {
        if (!sig->params || i >= sig->param_count)
            continue;
        Type *expected_ty = sig->params[i];
        if (!expected_ty)
            continue;
        if (!call_expr->args || !call_expr->args[i])
            return 0;
        if (!can_assign(expected_ty, call_expr->args[i]))
            return 0;
    }
    return 1;
}

static void apply_default_vararg_promotion(Node **slot)
{
    if (!slot || !*slot)
        return;

    Node *arg = *slot;
    if (!arg)
        return;

    Type *arg_result_ty = canonicalize_type_deep(arg->type);
    if (arg_result_ty && arg_result_ty->kind == TY_F64)
        return;

    if (!arg_result_ty && arg->kind == ND_CAST && arg->type)
        arg_result_ty = canonicalize_type_deep(arg->type);

    if (!arg_result_ty && arg->lhs)
        arg_result_ty = canonicalize_type_deep(arg->lhs->type);

    if (!arg_result_ty)
        return;

    if (arg_result_ty->kind == TY_F32)
    {
        Type *target = canonicalize_type_deep(type_f64());
        Node *cast = (Node *)xcalloc(1, sizeof(Node));
        cast->kind = ND_CAST;
        cast->lhs = arg;
        cast->type = target ? target : type_f64();
        cast->line = arg->line;
        cast->col = arg->col;
        cast->src = arg->src;
        *slot = cast;
    }
}

static void check_initializer_for_type(SemaContext *sc, Node *init, Type *target)
{
    if (!init || !target)
        return;
    target = canonicalize_type_deep(target);
    if (target && target->kind == TY_ARRAY)
    {
        Type *elem = canonicalize_type_deep(target->array.elem);
        if (target->array.is_unsized)
        {
            if (init->kind == ND_INIT_LIST && !init->init.is_zero)
            {
                diag_error_at(init->src, init->line, init->col,
                              "unsized arrays do not support initializer lists");
                exit(1);
            }
            check_expr(sc, init);
            Type *ptr_ty = type_ptr(elem ? elem : &ty_i32);
            if (!can_assign(ptr_ty, init))
            {
                diag_error_at(init->src, init->line, init->col,
                              "initializer expression is not compatible with dynamic array type");
                exit(1);
            }
            init->type = ptr_ty;
            return;
        }
        if (init->kind == ND_INIT_LIST)
        {
            if (init->init.is_zero || init->init.count == 0)
            {
                init->type = target;
                return;
            }

            int expected_len = target->array.length;
            if (expected_len >= 0 && init->init.count > expected_len)
            {
                diag_error_at(init->src, init->line, init->col,
                              "initializer has %d elements but array length is %d",
                              init->init.count, expected_len);
                exit(1);
            }

            int elem_is_aggregate = elem &&
                                    (elem->kind == TY_STRUCT ||
                                     (elem->kind == TY_ARRAY && !elem->array.is_unsized));
            if (elem_is_aggregate)
            {
                diag_error_at(init->src, init->line, init->col,
                              "array initializer lists for aggregate element types are not supported yet");
                exit(1);
            }

            for (int i = 0; i < init->init.count; ++i)
            {
                Node *elem_init = (init->init.elems && i < init->init.count) ? init->init.elems[i] : NULL;
                if (!elem_init)
                {
                    diag_error_at(init->src, init->line, init->col,
                                  "missing initializer expression for array element %d", i);
                    exit(1);
                }
                if (elem_init->kind == ND_INIT_LIST)
                {
                    diag_error_at(elem_init->src, elem_init->line, elem_init->col,
                                  "nested initializer lists are not supported for array elements yet");
                    exit(1);
                }
                check_expr(sc, elem_init);
                Type *expected_elem = elem ? elem : &ty_i32;
                if (expected_elem && !can_assign(expected_elem, elem_init))
                {
                    diag_error_at(elem_init->src, elem_init->line, elem_init->col,
                                  "initializer element type mismatch");
                    exit(1);
                }
            }
            init->type = target;
            return;
        }
        check_expr(sc, init);
        if (!can_assign(target, init))
        {
            diag_error_at(init->src, init->line, init->col,
                          "initializer expression type mismatch");
            exit(1);
        }
        init->type = target;
        return;
    }
    if (init->kind != ND_INIT_LIST)
    {
        check_expr(sc, init);
        if (target && !can_assign(target, init))
        {
            diag_error_at(init->src, init->line, init->col,
                          "initializer expression type mismatch");
            exit(1);
        }
        init->type = target;
        return;
    }
    if (target->kind == TY_STRUCT)
    {
        int count = init->init.count;
        int field_count = target->strct.field_count;
        int *indices = NULL;
        if (count > 0)
            indices = (int *)xcalloc((size_t)count, sizeof(int));
        unsigned char *used = (field_count > 0) ? (unsigned char *)xcalloc((size_t)field_count, sizeof(unsigned char)) : NULL;
        int next_field = 0;
        if (init->init.is_zero)
        {
            init->init.field_indices = NULL;
            init->type = target;
            if (indices)
                free(indices);
            if (used)
                free(used);
            return;
        }
        for (int i = 0; i < count; i++)
        {
            const char *desig = init->init.designators ? init->init.designators[i] : NULL;
            int field_index = -1;
            if (desig)
            {
                field_index = struct_find_field(target, desig);
                if (field_index < 0)
                {
                    diag_error_at(init->src, init->line, init->col,
                                  "unknown field '%s' in initializer", desig);
                    if (indices)
                        free(indices);
                    if (used)
                        free(used);
                    exit(1);
                }
            }
            else
            {
                if (next_field >= field_count)
                {
                    diag_error_at(init->src, init->line, init->col,
                                  "too many initializer elements for struct");
                    if (indices)
                        free(indices);
                    if (used)
                        free(used);
                    exit(1);
                }
                field_index = next_field++;
            }
            if (used && used[field_index])
            {
                diag_error_at(init->src, init->line, init->col,
                              "duplicate initializer for field '%s'",
                              target->strct.field_names[field_index]);
                if (indices)
                    free(indices);
                free(used);
                exit(1);
            }
            if (used)
                used[field_index] = 1;
            Node *elem = (init->init.elems && i < init->init.count) ? init->init.elems[i] : NULL;
            if (!elem)
            {
                diag_error_at(init->src, init->line, init->col,
                              "missing initializer expression");
                if (indices)
                    free(indices);
                if (used)
                    free(used);
                exit(1);
            }
            Type *ft = (field_index >= 0 && field_index < field_count)
                           ? target->strct.field_types[field_index]
                           : NULL;
            if (elem->kind == ND_INIT_LIST)
            {
                check_initializer_for_type(sc, elem, ft);
            }
            else
            {
                check_expr(sc, elem);
                if (ft && !can_assign(ft, elem))
                {
                    diag_error_at(elem->src, elem->line, elem->col,
                                  "initializer type mismatch for field '%s'",
                                  target->strct.field_names[field_index]);
                    if (indices)
                        free(indices);
                    if (used)
                        free(used);
                    exit(1);
                }
            }
            if (indices)
                indices[i] = field_index;
        }
        init->init.field_indices = indices;
        init->type = target;
        if (used)
            free(used);
        return;
    }
    if (!init->init.is_zero)
    {
        diag_error_at(init->src, init->line, init->col,
                      "brace initializer not supported for this type");
        exit(1);
    }
    init->type = target;
}

static void scope_push(SemaContext *sc)
{
    struct Scope *s = (struct Scope *)xcalloc(1, sizeof(struct Scope));
    s->parent = sc->scope;
    sc->scope = s;
}
static void scope_pop(SemaContext *sc)
{
    if (!sc || !sc->scope)
        return;
    struct Scope *p = sc->scope->parent;
    free(sc->scope);
    sc->scope = p;
}
static int scope_find(SemaContext *sc, const char *name)
{
    if (!sc || !sc->scope)
        return 0;
    struct Scope *s = sc->scope;
    for (int i = 0; i < s->local_count; i++)
    {
        if (strcmp(s->locals[i].name, name) == 0)
            return 1;
    }
    return 0;
}
static Type *scope_get_type(SemaContext *sc, const char *name)
{
    for (struct Scope *s = sc->scope; s; s = s->parent)
    {
        for (int i = 0; i < s->local_count; i++)
        {
            if (strcmp(s->locals[i].name, name) == 0)
                return s->locals[i].type;
        }
    }
    return NULL;
}
static void scope_add(SemaContext *sc, const char *name, Type *ty,
                      int is_const)
{
    if (!sc->scope)
        scope_push(sc);
    ty = canonicalize_type_deep(ty);
    struct Scope *s = sc->scope;
    if (s->local_count < 128)
    {
        s->locals[s->local_count].name = name;
        s->locals[s->local_count].type = ty;
        s->locals[s->local_count].is_const = is_const;
        s->local_count++;
    }
}
static int scope_is_const(SemaContext *sc, const char *name)
{
    for (struct Scope *s = sc->scope; s; s = s->parent)
    {
        for (int i = 0; i < s->local_count; i++)
        {
            if (strcmp(s->locals[i].name, name) == 0)
                return s->locals[i].is_const;
        }
    }
    return 0;
}

static Type *resolve_variable(SemaContext *sc, const char *name, int *is_global, int *is_const, int *is_function, const Symbol **out_sym)
{
    if (is_global)
        *is_global = 0;
    if (is_const)
        *is_const = 0;
    if (is_function)
        *is_function = 0;
    if (out_sym)
        *out_sym = NULL;
    if (!sc || !name)
        return NULL;

    Type *local_ty = scope_get_type(sc, name);
    if (local_ty)
    {
        if (is_const)
            *is_const = scope_is_const(sc, name);
        return canonicalize_type_deep(local_ty);
    }

    if (!sc->syms)
        return NULL;

    const Symbol *sym = symtab_get(sc->syms, name);
    if (sym && sym->kind == SYM_GLOBAL)
    {
        if (is_global)
            *is_global = 1;
        if (is_const)
            *is_const = sym->is_const;
        if (out_sym)
            *out_sym = sym;
        return canonicalize_type_deep(sym->var_type);
    }

    if (sym && sym->kind == SYM_FUNC)
    {
        if (is_function)
            *is_function = 1;
        if (is_const)
            *is_const = 1;
        if (out_sym)
            *out_sym = sym;
        Type *fn_ty = make_function_type_from_sig(&sym->sig);
        return fn_ty;
    }

    return NULL;
}

static const char *nodekind_name(NodeKind k)
{
    switch (k)
    {
    case ND_INT:
        return "ND_INT";
    case ND_FLOAT:
        return "ND_FLOAT";
    case ND_ADD:
        return "ND_ADD";
    case ND_RET:
        return "ND_RET";
    case ND_FUNC:
        return "ND_FUNC";
    case ND_STRING:
        return "ND_STRING";
    case ND_CALL:
        return "ND_CALL";
    case ND_VA_START:
        return "ND_VA_START";
    case ND_VA_ARG:
        return "ND_VA_ARG";
    case ND_VA_END:
        return "ND_VA_END";
    case ND_BLOCK:
        return "ND_BLOCK";
    case ND_VAR_DECL:
        return "ND_VAR_DECL";
    case ND_ASSIGN:
        return "ND_ASSIGN";
    case ND_IF:
        return "ND_IF";
    case ND_INDEX:
        return "ND_INDEX";
    case ND_DEREF:
        return "ND_DEREF";
    case ND_CAST:
        return "ND_CAST";
    case ND_GT_EXPR:
        return "ND_GT_EXPR";
    case ND_LT:
        return "ND_LT";
    case ND_LE:
        return "ND_LE";
    case ND_GE:
        return "ND_GE";
    case ND_SUB:
        return "ND_SUB";
    case ND_WHILE:
        return "ND_WHILE";
    case ND_EXPR_STMT:
        return "ND_EXPR_STMT";
    case ND_VAR:
        return "ND_VAR";
    case ND_UNIT:
        return "ND_UNIT";
    case ND_PREINC:
        return "ND_PREINC";
    case ND_PREDEC:
        return "ND_PREDEC";
    case ND_POSTINC:
        return "ND_POSTINC";
    case ND_POSTDEC:
        return "ND_POSTDEC";
    case ND_ADDR:
        return "ND_ADDR";
    case ND_LOR:
        return "ND_LOR";
    default:
        return "<unknown-node>";
    }
}

static void check_expr(SemaContext *sc, Node *e)
{
    if (e->kind == ND_INT)
    {
        if (!e->type)
            e->type = &ty_i32;
        return;
    }
    if (e->kind == ND_FLOAT)
    {
        if (!e->type)
            e->type = &ty_f64;
        return;
    }
    if (e->kind == ND_STRING)
    {
        static Type char_ptr = {.kind = TY_PTR, .pointee = &ty_char};
        e->type = &char_ptr;
        return;
    }
    if (e->kind == ND_NULL)
    {
        static Type null_ptr = {.kind = TY_PTR, .pointee = &ty_void};
        e->type = &null_ptr;
        return;
    }
    if (e->kind == ND_VAR)
    {
        const char *orig_name = e->var_ref;
        int is_global = 0;
        int is_const = 0;
        int is_function = 0;
        const Symbol *resolved_sym = NULL;
        Type *t = resolve_variable(sc, orig_name, &is_global, &is_const, &is_function, &resolved_sym);
        if (!t)
        {
            ImportedFunctionSet *auto_set = sema_find_imported_function_set(sc, orig_name);
            if (auto_set)
            {
                if (auto_set->count == 1)
                {
                    const Symbol *sym = symtab_get(sc->syms, orig_name);
                    if (!sym)
                        sym = &auto_set->candidates[0].symbol;
                    Type *fn_ty = make_function_type_from_sig(&sym->sig);
                    fn_ty = canonicalize_type_deep(fn_ty);
                    e->type = fn_ty;
                    e->var_type = fn_ty;
                    e->var_is_array = 0;
                    e->var_is_global = 0;
                    e->var_is_const = 1;
                    e->var_is_function = 1;
                    if (sym && sym->kind == SYM_FUNC)
                        e->referenced_function = sym->ast_node;
                    const char *backend = sym->backend_name ? sym->backend_name : sym->name;
                    if (backend)
                        e->var_ref = backend;
                    return;
                }
                else if (auto_set->count > 1)
                {
                    const char *mod_a = auto_set->candidates[0].module_full ? auto_set->candidates[0].module_full : "<unknown>";
                    const char *mod_b = auto_set->candidates[1].module_full ? auto_set->candidates[1].module_full : "<unknown>";
                    diag_error_at(e->src, e->line, e->col,
                                  "ambiguous reference to '%s'; modules '%s' and '%s' both provide candidates",
                                  orig_name ? orig_name : "<unnamed>", mod_a, mod_b);
                    exit(1);
                }
            }
            int import_parts = 0;
            const ModulePath *imp = unit_find_import_for_ident(sc ? sc->unit : NULL, orig_name, &import_parts);
            if (imp)
            {
                e->module_ref = imp;
                e->module_ref_parts = import_parts;
                e->module_type_name = NULL;
                e->module_type_is_enum = 0;
                e->type = &ty_module_placeholder;
                e->var_type = &ty_module_placeholder;
                e->var_is_const = 1;
                e->var_is_global = 0;
                e->var_is_function = 0;
                return;
            }
            diag_error_at(e->src, e->line, e->col, "unknown variable '%s'",
                          orig_name ? orig_name : "<null>");
            exit(1);
        }
        Type *canon = canonicalize_type_deep(t);
        e->var_type = canon ? canon : t;
        if (canon && canon->kind == TY_ARRAY)
        {
            e->var_is_array = canon->array.is_unsized ? 0 : 1;
            Type *elem = canon->array.elem ? canon->array.elem : &ty_i32;
            e->type = type_ptr(elem);
        }
        else
        {
            e->var_is_array = 0;
            e->type = canon ? canon : t;
        }
        e->var_is_global = is_global;
        e->var_is_const = is_const;
        e->var_is_function = is_function;
        if (resolved_sym && resolved_sym->kind == SYM_FUNC)
            e->referenced_function = resolved_sym->ast_node;

        if (is_function)
        {
            // Function names evaluate to function type; treat as non-addressable value.
            e->type = canon ? canon : t;
        }

        if (is_global && sc && sc->syms)
        {
            const Symbol *sym = symtab_get(sc->syms, orig_name);
            if (!sym)
                sym = symtab_get(sc->syms, e->var_ref);
            if (sym && sym->kind == SYM_GLOBAL && sym->backend_name)
                e->var_ref = sym->backend_name;
        }
        if (is_function && sc && sc->syms)
        {
            const Symbol *sym = symtab_get(sc->syms, orig_name);
            if (!sym)
                sym = symtab_get(sc->syms, e->var_ref);
            if (sym && sym->kind == SYM_FUNC)
            {
                const char *backend = sym->backend_name ? sym->backend_name : sym->name;
                if (backend)
                    e->var_ref = backend;
            }
        }
        return;
    }
    if (e->kind == ND_SIZEOF)
    {
        Type *ty = NULL;
        if (e->var_type)
        {
            ty = e->var_type;
        }
        else if (e->lhs)
        {
            check_expr(sc, e->lhs);
            if (e->lhs->kind == ND_VAR && e->lhs->var_type)
                ty = e->lhs->var_type;
            else if (e->lhs->type)
                ty = e->lhs->type;
        }
        ty = ty ? canonicalize_type_deep(ty) : &ty_i32;
        if (ty && ty->kind == TY_IMPORT)
            ty = canonicalize_type_deep(ty);
        int sz = sizeof_type_bytes(ty);
        e->int_val = sz;
        e->type = &ty_i32;
        return;
    }
    if (e->kind == ND_TYPEOF)
    {
        // Determine type either from explicit type or from expression
        Type *target = NULL;
        if (e->var_type)
            target = e->var_type;
        else if (e->lhs)
        {
            check_expr(sc, e->lhs);
            target = e->lhs->type;
        }
        target = canonicalize_type_deep(target);
        // Build a string literal node carrying the formatted type name
        char buf[256];
        buf[0] = '\0';
        if (!target)
        {
            snprintf(buf, sizeof(buf), "<unknown>::?");
        }
        else
        {
            switch (target->kind)
            {
            case TY_STRUCT:
            {
                const char *mod = module_registry_find_struct_module(target);
                const char *name = target->struct_name ? target->struct_name : "<struct>";
                if (mod && *mod)
                    snprintf(buf, sizeof(buf), "%s.%s::struct", mod, name);
                else
                    snprintf(buf, sizeof(buf), "%s::struct", name);
                break;
            }
            case TY_I8:
                snprintf(buf, sizeof(buf), "<built-in>::i8");
                break;
            case TY_U8:
                snprintf(buf, sizeof(buf), "<built-in>::u8");
                break;
            case TY_I16:
                snprintf(buf, sizeof(buf), "<built-in>::i16");
                break;
            case TY_U16:
                snprintf(buf, sizeof(buf), "<built-in>::u16");
                break;
            case TY_I32:
                snprintf(buf, sizeof(buf), "<built-in>::i32");
                break;
            case TY_U32:
                snprintf(buf, sizeof(buf), "<built-in>::u32");
                break;
            case TY_I64:
                snprintf(buf, sizeof(buf), "<built-in>::i64");
                break;
            case TY_U64:
                snprintf(buf, sizeof(buf), "<built-in>::u64");
                break;
            case TY_F32:
                snprintf(buf, sizeof(buf), "<built-in>::f32");
                break;
            case TY_F64:
                snprintf(buf, sizeof(buf), "<built-in>::f64");
                break;
            case TY_F128:
                snprintf(buf, sizeof(buf), "<built-in>::f128");
                break;
            case TY_VOID:
                snprintf(buf, sizeof(buf), "<built-in>::void");
                break;
            case TY_CHAR:
                snprintf(buf, sizeof(buf), "<built-in>::char");
                break;
            case TY_BOOL:
                snprintf(buf, sizeof(buf), "<built-in>::bool");
                break;
            case TY_PTR:
                snprintf(buf, sizeof(buf), "<built-in>::ptr");
                break;
            default:
                snprintf(buf, sizeof(buf), "<built-in>::?");
                break;
            }
            if (e->var_ref && target->kind != TY_STRUCT)
            {
                snprintf(buf, sizeof(buf), "<char*/alias>::%s", e->var_ref);
            }
        }
        // Convert to string literal
        Node *s = (Node *)xcalloc(1, sizeof(Node));
        s->kind = ND_STRING;
        s->src = e->src;
        s->line = e->line;
        s->col = e->col;
        s->str_len = (int)strlen(buf);
        char *heap = (char *)xmalloc((size_t)s->str_len + 1);
        memcpy(heap, buf, (size_t)s->str_len + 1);
        s->str_data = heap;
        // Replace e with string node semantics: set type to char*
        e->kind = ND_STRING;
        e->str_data = s->str_data;
        e->str_len = s->str_len;
        static Type char_ptr = {.kind = TY_PTR, .pointee = &ty_char};
        e->type = &char_ptr;
        return;
    }
    if (e->kind == ND_MEMBER)
    {
        if (!e->lhs)
        {
            diag_error_at(e->src, e->line, e->col, "member access missing base expression");
            exit(1);
        }
        check_expr(sc, e->lhs);
        Node *base_node = e->lhs;

        if (base_node && base_node->module_type_is_enum)
        {
            const ModulePath *imp = base_node->module_ref;
            const char *enum_name = base_node->module_type_name;
            const char *value_name = e->field_name;
            const char *module_full = imp ? imp->full_name : NULL;
            if (!module_full && sc && sc->unit && sc->unit->kind == ND_UNIT)
                module_full = sc->unit->module_path.full_name;
            if (!module_full || !enum_name || !value_name)
            {
                diag_error_at(e->src, e->line, e->col,
                              "incomplete enum reference for '%s'",
                              value_name ? value_name : "<value>");
                exit(1);
            }
            int enum_value = 0;
            if (!module_registry_lookup_enum_value(module_full, enum_name, value_name, &enum_value))
            {
                diag_error_at(e->src, e->line, e->col,
                              "unknown enum value '%s' on '%s.%s'",
                              value_name, module_full, enum_name);
                exit(1);
            }
            Type *enum_ty = canonicalize_type_deep(base_node->type);
            e->kind = ND_INT;
            e->lhs = NULL;
            e->rhs = NULL;
            e->int_val = enum_value;
            e->type = enum_ty ? enum_ty : &ty_i32;
            e->module_ref = NULL;
            e->module_ref_parts = 0;
            e->module_type_name = NULL;
            e->module_type_is_enum = 0;
            return;
        }

        if (base_node && base_node->module_ref)
        {
            const ModulePath *imp = base_node->module_ref;
            int consumed = base_node->module_ref_parts;
            const char *field = e->field_name;
            if (!imp || !field)
            {
                diag_error_at(e->src, e->line, e->col,
                              "invalid module-qualified reference");
                exit(1);
            }

            if (consumed < imp->part_count)
            {
                if (!imp->parts || !imp->parts[consumed] || strcmp(imp->parts[consumed], field) != 0)
                {
                    diag_error_at(e->src, e->line, e->col,
                                  "unknown module path segment '%s' in '%s'",
                                  field, imp->full_name ? imp->full_name : "<module>");
                    exit(1);
                }
                e->module_ref = imp;
                e->module_ref_parts = consumed + 1;
                e->module_type_name = NULL;
                e->module_type_is_enum = 0;
                e->type = &ty_module_placeholder;
                return;
            }

            const char *module_full = NULL;
            if (base_node->var_type && base_node->var_type->kind == TY_IMPORT && base_node->var_ref && *base_node->var_ref)
                module_full = base_node->var_ref;
            if (imp && imp->alias && imp->alias[0] && base_node->var_ref && strcmp(base_node->var_ref, imp->alias) == 0)
                module_full = imp->full_name;
            if (!module_full)
                module_full = imp->full_name;
            if (!module_full)
            {
                diag_error_at(e->src, e->line, e->col,
                              "module path missing for qualified reference");
                exit(1);
            }

            Type *struct_ty = module_registry_lookup_struct(module_full, field);
            if (struct_ty)
            {
                e->module_ref = imp;
                e->module_ref_parts = imp->part_count;
                e->module_type_name = NULL;
                e->module_type_is_enum = 0;
                e->type = struct_ty;
                return;
            }

            Type *enum_ty = module_registry_lookup_enum(module_full, field);
            if (enum_ty)
            {
                e->module_ref = imp;
                e->module_ref_parts = imp->part_count;
                e->module_type_name = field;
                e->module_type_is_enum = 1;
                e->type = enum_ty;
                return;
            }

            size_t module_len = module_full ? strlen(module_full) : 0;
            size_t field_len = strlen(field);
            char *qualified = NULL;
            if (module_full && *module_full)
            {
                qualified = (char *)xmalloc(module_len + 1 + field_len + 1);
                memcpy(qualified, module_full, module_len);
                qualified[module_len] = '.';
                memcpy(qualified + module_len + 1, field, field_len + 1);
            }
            else
            {
                qualified = make_qualified_name(imp, field);
            }

            const Symbol *sym = symtab_get(sc->syms, qualified);
            if (sym)
            {
                e->module_ref = NULL;
                e->module_ref_parts = 0;
                e->module_type_name = NULL;
                e->module_type_is_enum = 0;
                e->field_name = NULL;
                e->is_pointer_deref = 0;
                if (sym->kind == SYM_FUNC)
                {
                    Type *fn_ty = make_function_type_from_sig(&sym->sig);
                    e->kind = ND_VAR;
                    e->lhs = NULL;
                    e->rhs = NULL;
                    e->var_ref = sym->backend_name ? sym->backend_name : sym->name;
                    e->var_is_function = 1;
                    e->var_is_const = 1;
                    e->var_is_global = 0;
                    e->var_type = fn_ty;
                    e->type = fn_ty;
                    free(qualified);
                    return;
                }
                if (sym->kind == SYM_GLOBAL)
                {
                    Type *var_ty = canonicalize_type_deep(sym->var_type);
                    e->kind = ND_VAR;
                    e->lhs = NULL;
                    e->rhs = NULL;
                    e->var_ref = sym->backend_name ? sym->backend_name : sym->name;
                    e->var_is_function = 0;
                    e->var_is_const = sym->is_const;
                    e->var_is_global = 1;
                    e->var_type = var_ty;
                    e->type = var_ty;
                    free(qualified);
                    return;
                }
            }

            if (symtab_has_symbol_with_prefix(sc->syms, qualified))
            {
                // Treat this segment as another module placeholder so chained lookups can continue.
                e->kind = ND_VAR;
                e->lhs = NULL;
                e->rhs = NULL;
                e->var_ref = qualified;
                e->var_is_function = 0;
                e->var_is_const = 1;
                e->var_is_global = 0;
                e->var_type = &ty_module_placeholder;
                e->type = &ty_module_placeholder;
                e->module_ref = imp;
                e->module_ref_parts = imp->part_count;
                e->module_type_name = NULL;
                e->module_type_is_enum = 0;
                ast_free(base_node);
                return;
            }

            free(qualified);

            diag_error_at(e->src, e->line, e->col,
                          "unknown member '%s' on module '%s'",
                          field, module_full);
            exit(1);
        }

        Type *base = canonicalize_type_deep(e->lhs->type);
        if (e->is_pointer_deref)
        {
            if (!base || base->kind != TY_PTR || !base->pointee)
            {
                diag_error_at(e->src, e->line, e->col,
                              "'->' requires pointer to struct");
                exit(1);
            }
            base = canonicalize_type_deep(base->pointee);
        }
        else
        {
            if (!base || base->kind != TY_STRUCT)
            {
                diag_error_at(e->src, e->line, e->col,
                              "'.' requires struct value");
                exit(1);
            }
        }
        if (!base || base->kind != TY_STRUCT)
        {
            diag_error_at(e->src, e->line, e->col,
                          "member access requires struct type");
            exit(1);
        }
        int idx = struct_find_field(base, e->field_name);
        if (idx < 0)
        {
            diag_error_at(e->src, e->line, e->col,
                          "unknown field '%s' on struct '%s'",
                          e->field_name ? e->field_name : "<anon>",
                          base->struct_name ? base->struct_name : "<anon>");
            exit(1);
        }
        e->field_index = idx;
        e->field_offset = base->strct.field_offsets ? base->strct.field_offsets[idx] : 0;
        e->type = base->strct.field_types ? base->strct.field_types[idx] : NULL;
        if (!e->type)
        {
            diag_error_at(e->src, e->line, e->col,
                          "incomplete type for field '%s'",
                          base->strct.field_names[idx]);
            exit(1);
        }
        return;
    }
    if (e->kind == ND_ADDR)
    {
        if (!e->lhs)
        {
            diag_error_at(e->src, e->line, e->col,
                          "address-of operator requires an operand");
            exit(1);
        }
        Node *target = e->lhs;
        if (target->kind != ND_VAR && target->kind != ND_MEMBER && target->kind != ND_INDEX && target->kind != ND_DEREF)
        {
            diag_error_at(target->src, target->line, target->col,
                          "operand of '&' must be an lvalue");
            exit(1);
        }
        check_expr(sc, target);
        if (target->kind == ND_VAR && target->var_is_global && !target->var_is_function)
        {
            diag_error_at(target->src, target->line, target->col,
                          "address-of operator is not supported for global variables");
            exit(1);
        }
        if (!target->type && target->kind == ND_VAR)
            target->type = resolve_variable(sc, target->var_ref, NULL, NULL, NULL, NULL);
        if (!target->type)
        {
            diag_error_at(target->src, target->line, target->col,
                          "cannot determine operand type for '&'");
            exit(1);
        }
        Type *addr_type = target->type;
        if (target->kind == ND_VAR && target->var_type && target->var_type->kind == TY_ARRAY && !target->var_type->array.is_unsized)
            addr_type = target->var_type;
        e->type = type_ptr(addr_type);
        return;
    }
    if (e->kind == ND_ADD)
    {
        check_expr(sc, e->lhs);
        check_expr(sc, e->rhs);

        if (e->lhs && e->rhs && e->lhs->kind == ND_STRING && e->rhs->kind == ND_STRING)
        {
            size_t lhs_len = (size_t)(e->lhs->str_len >= 0 ? e->lhs->str_len : 0);
            size_t rhs_len = (size_t)(e->rhs->str_len >= 0 ? e->rhs->str_len : 0);
            size_t total = lhs_len + rhs_len;
            char *merged = (char *)xmalloc(total + 1);
            if (lhs_len > 0 && e->lhs->str_data)
                memcpy(merged, e->lhs->str_data, lhs_len);
            if (rhs_len > 0 && e->rhs->str_data)
                memcpy(merged + lhs_len, e->rhs->str_data, rhs_len);
            merged[total] = '\0';

            Node *lhs_old = e->lhs;
            Node *rhs_old = e->rhs;

            e->kind = ND_STRING;
            e->lhs = NULL;
            e->rhs = NULL;
            e->str_data = merged;
            e->str_len = (int)total;
            static Type char_ptr = {.kind = TY_PTR, .pointee = &ty_char};
            e->type = &char_ptr;

            ast_free(lhs_old);
            ast_free(rhs_old);
            return;
        }

        Type *lhs_type = canonicalize_type_deep(e->lhs->type);
        Type *rhs_type = canonicalize_type_deep(e->rhs->type);
        e->lhs->type = lhs_type;
        e->rhs->type = rhs_type;
        int lhs_is_ptr = type_is_pointer(lhs_type);
        int rhs_is_ptr = type_is_pointer(rhs_type);
        if (lhs_is_ptr || rhs_is_ptr)
        {
            if (lhs_is_ptr && rhs_is_ptr)
            {
                diag_error_at(e->src, e->line, e->col,
                              "pointer addition requires an integer offset, not another pointer");
                exit(1);
            }
            if (lhs_is_ptr && type_is_int(rhs_type))
            {
                e->type = lhs_type;
                return;
            }
            if (rhs_is_ptr && type_is_int(lhs_type))
            {
                e->type = rhs_type;
                return;
            }
            diag_error_at(e->src, e->line, e->col,
                          "pointer addition requires exactly one pointer and one integer operand");
            exit(1);
        }
        if (!type_equal(lhs_type, rhs_type))
        {
            if (type_is_int(rhs_type) && coerce_int_literal_to_type(e->lhs, rhs_type, "+"))
                lhs_type = canonicalize_type_deep(e->lhs->type);
            if (!type_equal(lhs_type, rhs_type) && type_is_int(lhs_type) && coerce_int_literal_to_type(e->rhs, lhs_type, "+"))
                rhs_type = canonicalize_type_deep(e->rhs->type);
            if (!type_equal(lhs_type, rhs_type))
            {
                diag_error_at(e->src, e->line, e->col,
                              "'+' requires both operands to have the same type");
                exit(1);
            }
        }
        e->type = lhs_type;
        return;
    }
    if (e->kind == ND_SUB)
    {
        check_expr(sc, e->lhs);
        check_expr(sc, e->rhs);
        Type *lhs_type = canonicalize_type_deep(e->lhs->type);
        Type *rhs_type = canonicalize_type_deep(e->rhs->type);
        e->lhs->type = lhs_type;
        e->rhs->type = rhs_type;
        int lhs_is_ptr = type_is_pointer(lhs_type);
        int rhs_is_ptr = type_is_pointer(rhs_type);
        if (lhs_is_ptr || rhs_is_ptr)
        {
            if (lhs_is_ptr && rhs_is_ptr)
            {
                if (!type_equal(lhs_type, rhs_type))
                {
                    diag_error_at(e->src, e->line, e->col,
                                  "pointer subtraction requires both operands to point to the same type");
                    exit(1);
                }
                e->type = &ty_i64;
                return;
            }
            if (lhs_is_ptr && type_is_int(rhs_type))
            {
                e->type = lhs_type;
                return;
            }
            diag_error_at(e->src, e->line, e->col,
                          "pointer subtraction requires a pointer minus an integer or pointer minus pointer of the same type");
            exit(1);
        }
        if (!type_equal(lhs_type, rhs_type))
        {
            if (type_is_int(rhs_type) && coerce_int_literal_to_type(e->lhs, rhs_type, "-"))
                lhs_type = canonicalize_type_deep(e->lhs->type);
            if (!type_equal(lhs_type, rhs_type) && type_is_int(lhs_type) && coerce_int_literal_to_type(e->rhs, lhs_type, "-"))
                rhs_type = canonicalize_type_deep(e->rhs->type);
            if (!type_equal(lhs_type, rhs_type))
            {
                diag_error_at(e->src, e->line, e->col,
                              "'-' requires both operands to have the same type");
                exit(1);
            }
        }
        e->type = lhs_type;
        return;
    }
    if (e->kind == ND_NEG)
    {
        if (!e->lhs)
        {
            diag_error_at(e->src, e->line, e->col, "negation missing operand");
            exit(1);
        }
        check_expr(sc, e->lhs);
        if (!(type_is_int(e->lhs->type) || type_is_float(e->lhs->type)))
        {
            diag_error_at(e->src, e->line, e->col, "unary '-' requires integer or floating-point operand");
            exit(1);
        }
        e->type = e->lhs->type;
        return;
    }
    if (e->kind == ND_MUL || e->kind == ND_DIV)
    {
        check_expr(sc, e->lhs);
        check_expr(sc, e->rhs);
        Type *lhs_type = canonicalize_type_deep(e->lhs->type);
        Type *rhs_type = canonicalize_type_deep(e->rhs->type);
        e->lhs->type = lhs_type;
        e->rhs->type = rhs_type;
        if (!type_equal(lhs_type, rhs_type))
        {
            const char *op = (e->kind == ND_MUL) ? "*" : "/";
            if (type_is_int(rhs_type) && coerce_int_literal_to_type(e->lhs, rhs_type, op))
                lhs_type = canonicalize_type_deep(e->lhs->type);
            if (!type_equal(lhs_type, rhs_type) && type_is_int(lhs_type) && coerce_int_literal_to_type(e->rhs, lhs_type, op))
                rhs_type = canonicalize_type_deep(e->rhs->type);
            if (!type_equal(lhs_type, rhs_type))
            {
                diag_error_at(e->src, e->line, e->col,
                              "'%s' requires both operands to have the same type", op);
                exit(1);
            }
        }
        int lhs_is_int = type_is_int(e->lhs->type);
        int lhs_is_float = type_is_float(e->lhs->type);
        if (!(lhs_is_int || lhs_is_float))
        {
            diag_error_at(e->src, e->line, e->col,
                          "numeric type required for '%s'",
                          e->kind == ND_MUL ? "*" : "/");
            exit(1);
        }
        e->type = e->lhs->type;
        return;
    }
    if (e->kind == ND_SHL || e->kind == ND_SHR)
    {
        check_expr(sc, e->lhs);
        check_expr(sc, e->rhs);
        if (!(type_is_int(e->lhs->type) && type_is_int(e->rhs->type)))
        {
            diag_error_at(e->src, e->line, e->col,
                          "shift operands must be integers");
            exit(1);
        }
        // Result type is the type of the left operand
        e->type = e->lhs->type;
        return;
    }
    if (e->kind == ND_BITAND || e->kind == ND_BITOR || e->kind == ND_BITXOR)
    {
        check_expr(sc, e->lhs);
        check_expr(sc, e->rhs);
        Type *lhs_type = canonicalize_type_deep(e->lhs->type);
        Type *rhs_type = canonicalize_type_deep(e->rhs->type);
        e->lhs->type = lhs_type;
        e->rhs->type = rhs_type;
        const char *op_symbol = (e->kind == ND_BITAND) ? "&" : (e->kind == ND_BITOR) ? "|"
                                                                                     : "^";
        if (!type_is_int(lhs_type) || !type_is_int(rhs_type))
        {
            diag_error_at(e->src, e->line, e->col,
                          "'%s' requires integer operands", op_symbol);
            exit(1);
        }
        if (!type_equal(lhs_type, rhs_type))
        {
            if (type_is_int(rhs_type) && coerce_int_literal_to_type(e->lhs, rhs_type, op_symbol))
                lhs_type = canonicalize_type_deep(e->lhs->type);
            if (!type_equal(lhs_type, rhs_type) && type_is_int(lhs_type) && coerce_int_literal_to_type(e->rhs, lhs_type, op_symbol))
                rhs_type = canonicalize_type_deep(e->rhs->type);
        }
        if (!type_equal(lhs_type, rhs_type))
        {
            diag_error_at(e->src, e->line, e->col,
                          "'%s' requires both operands to have the same type", op_symbol);
            exit(1);
        }
        e->type = lhs_type;
        return;
    }
    if (e->kind == ND_BITNOT)
    {
        if (!e->lhs)
        {
            diag_error_at(e->src, e->line, e->col,
                          "bitwise '~' requires an operand");
            exit(1);
        }
        check_expr(sc, e->lhs);
        Type *operand_type = canonicalize_type_deep(e->lhs->type);
        e->lhs->type = operand_type;
        if (!type_is_int(operand_type))
        {
            diag_error_at(e->src, e->line, e->col,
                          "bitwise '~' requires integer operand");
            exit(1);
        }
        e->type = operand_type;
        return;
    }
    if (e->kind == ND_GT_EXPR || e->kind == ND_LT || e->kind == ND_LE || e->kind == ND_GE)
    {
        check_expr(sc, e->lhs);
        check_expr(sc, e->rhs);
        // Allow integer, floating-point, or pointer relational comparisons when categories match.
        int lhs_is_int = type_is_int(e->lhs->type);
        int rhs_is_int = type_is_int(e->rhs->type);
        int lhs_is_float = type_is_float(e->lhs->type);
        int rhs_is_float = type_is_float(e->rhs->type);
        int lhs_is_ptr = (e->lhs->type && e->lhs->type->kind == TY_PTR);
        int rhs_is_ptr = (e->rhs->type && e->rhs->type->kind == TY_PTR);
        if (!((lhs_is_int && rhs_is_int) || (lhs_is_float && rhs_is_float) || (lhs_is_ptr && rhs_is_ptr)))
        {
            diag_error_at(e->src, e->line, e->col, "relational operator requires integer, floating-point, or pointer operands of the same category");
            exit(1);
        }
        e->type = type_bool();
        return;
    }
    if (e->kind == ND_LAND || e->kind == ND_LOR)
    {
        check_expr(sc, e->lhs);
        check_expr(sc, e->rhs);
        if (!(type_is_int(e->lhs->type) && type_is_int(e->rhs->type)))
        {
            diag_error_at(e->src, e->line, e->col, "%s requires integer operands",
                          e->kind == ND_LAND ? "&&" : "||");
            exit(1);
        }
        e->type = type_bool();
        return;
    }
    if (e->kind == ND_EQ || e->kind == ND_NE)
    {
        check_expr(sc, e->lhs);
        check_expr(sc, e->rhs);
        // Allow integer, floating-point, or pointer equality when categories match.
        int lhs_is_int = type_is_int(e->lhs->type);
        int rhs_is_int = type_is_int(e->rhs->type);
        int lhs_is_float = type_is_float(e->lhs->type);
        int rhs_is_float = type_is_float(e->rhs->type);
        int lhs_is_ptr = (e->lhs->type && e->lhs->type->kind == TY_PTR);
        int rhs_is_ptr = (e->rhs->type && e->rhs->type->kind == TY_PTR);
        if (!((lhs_is_int && rhs_is_int) || (lhs_is_float && rhs_is_float) || (lhs_is_ptr && rhs_is_ptr)))
        {
            diag_error_at(e->src, e->line, e->col,
                          "equality requires both operands to be integers, floats, or pointers");
            exit(1);
        }
        e->type = type_bool();
        return;
    }
    if (e->kind == ND_CAST)
    {
        check_expr(sc, e->lhs); /* trust parser's target type on node */
        if (!e->type)
            e->type = e->lhs->type;
        return;
    }
    if (e->kind == ND_DEREF)
    {
        if (!e->lhs)
        {
            diag_error_at(e->src, e->line, e->col, "dereference missing operand");
            exit(1);
        }
        check_expr(sc, e->lhs);
        Type *ptr_type = canonicalize_type_deep(e->lhs->type);
        if (!ptr_type || ptr_type->kind != TY_PTR || !ptr_type->pointee)
        {
            diag_error_at(e->src, e->line, e->col,
                          "'*' requires pointer operand");
            exit(1);
        }
        Type *elem_type = canonicalize_type_deep(ptr_type->pointee);
        if (elem_type && elem_type->kind == TY_STRUCT)
        {
            e->type = elem_type;
        }
        else if (elem_type && elem_type->kind == TY_CHAR)
        {
            e->type = &ty_i32;
        }
        else
        {
            e->type = elem_type ? elem_type : &ty_i32;
        }
        return;
    }
    if (e->kind == ND_INDEX)
    {
        // lhs must be a pointer; rhs must be integer; result is element type
        // (promoted to int for now)
        if (!e->lhs || !e->rhs)
        {
            diag_error_at(e->src, e->line, e->col, "invalid index expression");
            exit(1);
        }
        check_expr(sc, e->lhs);
        check_expr(sc, e->rhs);
        if (!type_is_int(e->rhs->type))
        {
            diag_error_at(e->src, e->line, e->col, "array index is not an integer");
            exit(1);
        }
        if (!e->lhs->type || e->lhs->type->kind != TY_PTR ||
            !e->lhs->type->pointee)
        {
            diag_error_at(e->src, e->line, e->col,
                          "subscripted value is not an array or pointer");
            exit(1);
        }
        // If the pointer points to a struct, result type is the struct type
        Type *elem_type = canonicalize_type_deep(e->lhs->type->pointee);
        if (elem_type && elem_type->kind == TY_STRUCT)
        {
            e->type = elem_type;
        }
        else if (elem_type && elem_type->kind == TY_CHAR)
        {
            // char* promotes to int for indexing
            e->type = &ty_i32;
        }
        else
        {
            // Default: use pointee type
            e->type = elem_type ? elem_type : &ty_i32;
        }
        return;
    }
    if (e->kind == ND_ASSIGN)
    {
        if (!e->lhs)
        {
            diag_error_at(e->src, e->line, e->col,
                          "assignment missing left-hand side");
            exit(1);
        }
        check_expr(sc, e->lhs);
        Node *lhs_expr = e->lhs;
        Node *lhs_base = lhs_expr;
        if (lhs_base->kind == ND_CAST && lhs_base->lhs)
            lhs_base = lhs_base->lhs;
        if (lhs_base->kind != ND_VAR && lhs_base->kind != ND_INDEX && lhs_base->kind != ND_MEMBER && lhs_base->kind != ND_DEREF)
        {
            diag_error_at(e->src, e->line, e->col,
                          "lvalue required as left operand of assignment");
            exit(1);
        }
        if (lhs_base->kind == ND_VAR)
        {
            if (lhs_base->var_is_const)
            {
                diag_error_at(lhs_base->src, lhs_base->line, lhs_base->col,
                              "cannot assign to constant variable '%s'",
                              lhs_base->var_ref ? lhs_base->var_ref : "<unnamed>");
                exit(1);
            }
        }
        check_expr(sc, e->rhs);
        Type *lhs_type = lhs_expr->type;
        if (!lhs_type)
        {
            if (lhs_base->kind == ND_VAR)
                lhs_type = resolve_variable(sc, lhs_base->var_ref, NULL, NULL, NULL, NULL);
            else if (lhs_base->kind == ND_MEMBER)
                lhs_type = lhs_base->type;
            else if ((lhs_base->kind == ND_INDEX || lhs_base->kind == ND_DEREF) && lhs_base->lhs && lhs_base->lhs->type && lhs_base->lhs->type->kind == TY_PTR)
                lhs_type = lhs_base->lhs->type->pointee;
        }
        if (lhs_base->kind == ND_VAR)
        {
            if (!lhs_type)
            {
                diag_error_at(lhs_base->src, lhs_base->line, lhs_base->col,
                              "unknown variable '%s' on left-hand side of assignment",
                              lhs_base->var_ref ? lhs_base->var_ref : "<unnamed>");
                exit(1);
            }
            if (lhs_base->var_type && lhs_base->var_type->kind == TY_ARRAY && !lhs_base->var_type->array.is_unsized)
            {
                diag_error_at(lhs_base->src, lhs_base->line, lhs_base->col,
                              "cannot assign to array variable '%s'",
                              lhs_base->var_ref ? lhs_base->var_ref : "<unnamed>");
                exit(1);
            }
        }
        if (lhs_type)
        {
            if (!can_assign(lhs_type, e->rhs))
            {
                diag_error_at(e->src, e->line, e->col,
                              "cannot assign '%d' to '%d' without cast",
                              e->rhs->type ? e->rhs->type->kind : -1,
                              lhs_type ? lhs_type->kind : -1);
                exit(1);
            }
            e->rhs->type = lhs_type;
        }
        e->type = lhs_type ? lhs_type : (e->rhs->type ? e->rhs->type : &ty_i32);
        return;
    }
    if (e->kind == ND_PREINC || e->kind == ND_PREDEC || e->kind == ND_POSTINC ||
        e->kind == ND_POSTDEC)
    {
        // operand must be an lvalue variable of integer type (simplified)
        if (!e->lhs)
        {
            diag_error_at(e->src, e->line, e->col,
                          "operand of ++/-- must be a variable");
            exit(1);
        }
        check_expr(sc, e->lhs);
        if (e->lhs->kind != ND_VAR)
        {
            diag_error_at(e->src, e->line, e->col,
                          "operand of ++/-- must be a variable");
            exit(1);
        }
        if (e->lhs->var_is_const)
        {
            diag_error_at(e->lhs->src, e->lhs->line, e->lhs->col,
                          "cannot modify constant variable '%s'",
                          e->lhs->var_ref ? e->lhs->var_ref : "<unnamed>");
            exit(1);
        }
        Type *t = e->lhs->type;
        if (!t)
            t = resolve_variable(sc, e->lhs->var_ref, NULL, NULL, NULL, NULL);
        t = canonicalize_type_deep(t);
        e->lhs->type = t;
        if (!t || (!type_is_int(t) && !type_is_pointer(t)))
        {
            diag_error_at(e->src, e->line, e->col, "++/-- requires integer or pointer variable");
            exit(1);
        }
        e->type = t;
        return;
    }
    if (e->kind == ND_CALL)
    {
        Node *target = e->lhs;
        const char *original_name = e->call_name;
        const char *resolved_name = original_name;

        if (sc->unit && resolved_name)
        {
            char *alias_resolved = resolve_import_alias(sc->unit, resolved_name);
            if (alias_resolved)
            {
                resolved_name = alias_resolved;
                e->call_name = alias_resolved;
            }
        }

        const Symbol *sym_lookup = resolved_name ? symtab_get(sc->syms, resolved_name) : NULL;
        const Symbol *direct_sym = (sym_lookup && sym_lookup->kind == SYM_FUNC) ? sym_lookup : NULL;
        int args_checked = 0;

        Type *func_sig = NULL;
        int call_is_indirect = 0;

        Type *target_type = NULL;
        int target_is_function_symbol = 0;

        if (resolved_name)
        {
            ImportedFunctionCandidate *resolved_import = sema_resolve_imported_call(sc, resolved_name, e, &args_checked);
            if (resolved_import)
            {
                direct_sym = &resolved_import->symbol;
                func_sig = make_function_type_from_sig(&resolved_import->symbol.sig);
                call_is_indirect = 0;
            }
        }

        if (!func_sig && direct_sym)
        {
            func_sig = make_function_type_from_sig(&direct_sym->sig);
            call_is_indirect = 0;
        }

        if (target)
        {
            check_expr(sc, target);
            target_type = canonicalize_type_deep(target->type);
            target_is_function_symbol = (target->kind == ND_VAR && target->var_is_function);
        }

        if (!func_sig && target_type)
        {
            if (target_type->kind == TY_PTR && target_type->pointee && target_type->pointee->kind == TY_FUNC)
            {
                func_sig = canonicalize_type_deep(target_type->pointee);
                call_is_indirect = 1;
            }
            else if (target_type->kind == TY_FUNC)
            {
                func_sig = target_type;
                call_is_indirect = target_is_function_symbol ? 0 : 1;
            }
        }

        if (!func_sig)
        {
            if (resolved_name && !sym_lookup && unit_allows_module_call(sc->unit, resolved_name))
            {
                diag_error_at(e->src, e->line, e->col,
                              "missing metadata for function '%s'",
                              resolved_name);
                exit(1);
            }
            if (sym_lookup && sym_lookup->kind != SYM_FUNC)
            {
                diag_error_at(e->src, e->line, e->col,
                              "symbol '%s' is not callable",
                              resolved_name ? resolved_name : (original_name ? original_name : "<unnamed>"));
                exit(1);
            }
            if (resolved_name && !direct_sym)
            {
                diag_error_at(e->src, e->line, e->col,
                              "unknown function '%s'",
                              resolved_name);
                exit(1);
            }
            diag_error_at(e->src, e->line, e->col,
                          "call target is not callable");
            exit(1);
        }

        func_sig = canonicalize_type_deep(func_sig);
        if (!func_sig || func_sig->kind != TY_FUNC)
        {
            diag_error_at(e->src, e->line, e->col,
                          "call target is not a function type");
            exit(1);
        }

        if (!call_is_indirect && direct_sym && direct_sym->kind == SYM_FUNC)
        {
            e->call_target = direct_sym->ast_node;
            fprintf(stderr, "call_target set for %s -> %p inline=%d\n",
                    resolved_name ? resolved_name : "<null>",
                    (void *)e->call_target,
                    e->call_target ? e->call_target->inline_candidate : -1);
        }
        else if (!call_is_indirect && target && target->referenced_function)
        {
            e->call_target = target->referenced_function;
        }
        else
        {
            e->call_target = NULL;
        }
        if (!func_sig->func.has_signature)
        {
            diag_error_at(e->src, e->line, e->col,
                          "cannot call function pointer without a signature");
            exit(1);
        }

        if (!args_checked)
        {
            for (int i = 0; i < e->arg_count; ++i)
                check_expr(sc, e->args[i]);
        }

        const char *diag_name = resolved_name ? resolved_name : (original_name ? original_name : "<call>");

        int expected = func_sig->func.param_count;
        if (!func_sig->func.is_varargs)
        {
            if (e->arg_count != expected)
            {
                diag_error_at(e->src, e->line, e->col,
                              "function call to '%s' expects %d argument(s) but %d provided",
                              diag_name, expected, e->arg_count);
                exit(1);
            }
        }
        else if (e->arg_count < expected)
        {
            diag_error_at(e->src, e->line, e->col,
                          "function call to '%s' expects at least %d argument(s) before varargs",
                          diag_name, expected);
            exit(1);
        }

        int check_count = expected;
        if (func_sig->func.is_varargs && e->arg_count > expected)
            check_count = expected;
        if (!func_sig->func.is_varargs && e->arg_count < check_count)
            check_count = e->arg_count;

        for (int i = 0; i < check_count; ++i)
        {
            Type *expected_ty = (func_sig->func.params && i < expected) ? func_sig->func.params[i] : NULL;
            if (!expected_ty)
                continue;
            if (!can_assign(expected_ty, e->args[i]))
            {
                char want[64];
                char got[64];
                describe_type(expected_ty, want, sizeof(want));
                describe_type(e->args[i]->type, got, sizeof(got));
                diag_error_at(e->args[i]->src, e->args[i]->line, e->args[i]->col,
                              "argument %d type mismatch: expected %s, got %s",
                              i + 1, want, got);
                exit(1);
            }
        }

        if (func_sig->func.is_varargs && e->arg_count > expected)
        {
            for (int i = expected; i < e->arg_count; ++i)
                apply_default_vararg_promotion(&e->args[i]);
        }

        if (direct_sym)
        {
            const char *backend = direct_sym->backend_name ? direct_sym->backend_name : direct_sym->name;
            if (backend)
                e->call_name = backend;
            call_is_indirect = 0;
        }

        Type *ret_type = func_sig->func.ret ? func_sig->func.ret : &ty_i32;
        e->type = ret_type;
        e->call_func_type = func_sig;
        e->call_is_indirect = call_is_indirect;
        e->call_is_varargs = func_sig->func.is_varargs;

        if (!call_is_indirect)
            inline_try_fold_call(e);
        return;
    }
    if (e->kind == ND_VA_START)
    {
        // va_start() -> returns a va_list
        e->type = type_va_list();
        return;
    }
    if (e->kind == ND_VA_ARG)
    {
        // va_arg(list, T) -> yields value of type T
        if (!e->lhs)
        {
            diag_error_at(e->src, e->line, e->col, "va_arg requires a va_list expression");
            exit(1);
        }
        check_expr(sc, e->lhs);
        // ensure lhs is a va_list (best-effort)
        if (e->lhs->type && canonicalize_type_deep(e->lhs->type) && canonicalize_type_deep(e->lhs->type)->kind != TY_VA_LIST)
        {
            diag_error_at(e->lhs->src, e->lhs->line, e->lhs->col, "first argument to va_arg must be a va_list");
            exit(1);
        }
        // target type is recorded in parser as e->var_type
        if (!e->var_type)
        {
            diag_error_at(e->src, e->line, e->col, "va_arg missing target type");
            exit(1);
        }
        e->type = canonicalize_type_deep(e->var_type);
        return;
    }
    if (e->kind == ND_VA_END)
    {
        // va_end(list) -> void
        if (!e->lhs)
        {
            diag_error_at(e->src, e->line, e->col, "va_end requires a va_list expression");
            exit(1);
        }
        check_expr(sc, e->lhs);
        e->type = &ty_void;
        return;
    }
    if (e->kind == ND_COND)
    {
        // lhs ? rhs : body
        if (!e->lhs || !e->rhs || !e->body)
        {
            diag_error_at(e->src, e->line, e->col, "malformed ternary expression");
            exit(1);
        }
        check_expr(sc, e->lhs);
        check_expr(sc, e->rhs);
        check_expr(sc, e->body);
        // Condition: allow integers or pointers (non-zero truthy)
        int cond_ok = type_is_int(e->lhs->type) || (e->lhs->type && e->lhs->type->kind == TY_PTR);
        if (!cond_ok)
        {
            diag_error_at(e->lhs->src, e->lhs->line, e->lhs->col, "ternary condition must be integer or pointer");
            exit(1);
        }
        Type *then_type = canonicalize_type_deep(e->rhs->type);
        Type *else_type = canonicalize_type_deep(e->body->type);
        e->rhs->type = then_type;
        e->body->type = else_type;

        if (type_equal(then_type, else_type))
        {
            e->type = then_type;
            return;
        }

        if (then_type && then_type->kind == TY_PTR && e->body->kind == ND_NULL)
        {
            e->body->type = then_type;
            e->type = then_type;
            return;
        }
        if (else_type && else_type->kind == TY_PTR && e->rhs->kind == ND_NULL)
        {
            e->rhs->type = else_type;
            e->type = else_type;
            return;
        }

        if (else_type && coerce_int_literal_to_type(e->rhs, else_type, "conditional branch"))
        {
            then_type = canonicalize_type_deep(e->rhs->type);
        }
        if (then_type && coerce_int_literal_to_type(e->body, then_type, "conditional branch"))
        {
            else_type = canonicalize_type_deep(e->body->type);
        }

        if (type_equal(then_type, else_type))
        {
            e->type = then_type;
            return;
        }

        diag_error_at(e->src, e->line, e->col, "ternary branches must have compatible types");
        exit(1);
    }
    diag_error_at(e->src, e->line, e->col, "unsupported expression: %s",
                  nodekind_name(e->kind));
    exit(1);
}

static int sema_check_statement(SemaContext *sc, Node *stmt, Node *fn, int *found_ret);

static int sema_check_block(SemaContext *sc, Node *block, Node *fn, int *found_ret, int push_scope)
{
    if (!block)
        return 0;
    if (block->kind != ND_BLOCK)
        return sema_check_statement(sc, block, fn, found_ret);
    if (push_scope)
        scope_push(sc);
    for (int i = 0; i < block->stmt_count; ++i)
    {
        Node *stmt = block->stmts[i];
        if (sema_check_statement(sc, stmt, fn, found_ret))
        {
            if (push_scope)
                scope_pop(sc);
            return 1;
        }
    }
    if (push_scope)
        scope_pop(sc);
    return 0;
}

static int sema_check_statement(SemaContext *sc, Node *stmt, Node *fn, int *found_ret)
{
    if (!stmt)
        return 0;
    switch (stmt->kind)
    {
    case ND_BLOCK:
        return sema_check_block(sc, stmt, fn, found_ret, 1);
    case ND_VAR_DECL:
    {
        if (scope_find(sc, stmt->var_name))
        {
            diag_error_at(stmt->src, stmt->line, stmt->col, "redeclaration of '%s'",
                          stmt->var_name);
            return 1;
        }
        stmt->var_type = canonicalize_type_deep(stmt->var_type);
        scope_add(sc, stmt->var_name, stmt->var_type, stmt->var_is_const);
        if (stmt->rhs)
        {
            if (stmt->rhs->kind == ND_INIT_LIST)
            {
                check_initializer_for_type(sc, stmt->rhs, stmt->var_type);
            }
            else
            {
                check_expr(sc, stmt->rhs);
                if (stmt->var_type && !can_assign(stmt->var_type, stmt->rhs))
                {
                    diag_error_at(stmt->rhs->src, stmt->rhs->line, stmt->rhs->col,
                                  "cannot initialize '%s' with incompatible type",
                                  stmt->var_name);
                    return 1;
                }
                if (stmt->var_type)
                    stmt->rhs->type = stmt->var_type;
            }
        }
        return 0;
    }
    case ND_EXPR_STMT:
        if (stmt->lhs)
            check_expr(sc, stmt->lhs);
        return 0;
    case ND_IF:
        if (stmt->lhs)
            check_expr(sc, stmt->lhs);
        if (stmt->rhs && sema_check_statement(sc, stmt->rhs, fn, found_ret))
            return 1;
        if (stmt->body && sema_check_statement(sc, stmt->body, fn, found_ret))
            return 1;
        return 0;
    case ND_WHILE:
        if (stmt->lhs)
        {
            check_expr(sc, stmt->lhs);
            if (!type_is_int(stmt->lhs->type))
            {
                diag_error_at(stmt->lhs->src, stmt->lhs->line, stmt->lhs->col,
                              "while condition must be integer");
                return 1;
            }
        }
        if (stmt->rhs && sema_check_statement(sc, stmt->rhs, fn, found_ret))
            return 1;
        return 0;
    case ND_RET:
        if (fn->ret_type && fn->ret_type->kind == TY_VOID)
        {
            if (stmt->lhs)
            {
                diag_error_at(stmt->src, stmt->line, stmt->col,
                              "cannot return a value from a void function");
                return 1;
            }
        }
        else
        {
            if (!stmt->lhs)
            {
                diag_error_at(stmt->src, stmt->line, stmt->col,
                              "non-void function must return a value");
                return 1;
            }
            check_expr(sc, stmt->lhs);
            stmt->type = stmt->lhs->type;
            Type *decl = fn->ret_type ? fn->ret_type : &ty_i32;
            if (!type_equal(stmt->type, decl))
            {
                diag_error_at(stmt->src, stmt->line, stmt->col,
                              "return type mismatch: returning %d but function returns %d",
                              stmt->type ? stmt->type->kind : -1,
                              decl ? decl->kind : -1);
                return 1;
            }
        }
        if (found_ret)
            *found_ret = 1;
        return 0;
    default:
        return 0;
    }
}

static int sema_global_initializer_is_const(const Node *expr)
{
    if (!expr)
        return 1;

    switch (expr->kind)
    {
    case ND_INT:
    case ND_FLOAT:
    case ND_NULL:
        return 1;
    case ND_NEG:
    case ND_CAST:
        return sema_global_initializer_is_const(expr->lhs);
    case ND_INIT_LIST:
        return expr->init.is_zero;
    default:
        return 0;
    }
}

static int sema_check_global_decl(SemaContext *sc, Node *decl)
{
    if (!decl || decl->kind != ND_VAR_DECL || !decl->var_is_global)
        return 0;

    if (!decl->var_name)
    {
        diag_error_at(decl->src, decl->line, decl->col,
                      "global variable requires a name");
        return 1;
    }

    decl->var_type = canonicalize_type_deep(decl->var_type);
    Type *ty = decl->var_type;
    if (!ty)
    {
        diag_error_at(decl->src, decl->line, decl->col,
                      "unable to determine type for global '%s'",
                      decl->var_name);
        return 1;
    }
    if (ty->kind == TY_VOID)
    {
        diag_error_at(decl->src, decl->line, decl->col,
                      "global '%s' cannot have type void",
                      decl->var_name);
        return 1;
    }
    if (ty->kind == TY_STRUCT)
    {
        if (ty->strct.size_bytes <= 0)
        {
            diag_error_at(decl->src, decl->line, decl->col,
                          "struct global '%s' has incomplete size",
                          decl->var_name);
            return 1;
        }

        if (!decl->rhs)
            return 0;

        if (decl->rhs->kind != ND_INIT_LIST || !decl->rhs->init.is_zero)
        {
            diag_error_at(decl->rhs->src, decl->rhs->line, decl->rhs->col,
                          "struct global '%s' requires an all-zero initializer",
                          decl->var_name);
            return 1;
        }

        decl->rhs->type = ty;
        return 0;
    }

    if (!decl->rhs)
        return 0;

    if (decl->rhs->kind == ND_INIT_LIST)
    {
        if (!decl->rhs->init.is_zero)
        {
            diag_error_at(decl->rhs->src, decl->rhs->line, decl->rhs->col,
                          "non-zero initializer lists are not supported for global variables");
            return 1;
        }
        decl->rhs->type = ty;
        return 0;
    }

    check_expr(sc, decl->rhs);
    if (!can_assign(ty, decl->rhs))
    {
        diag_error_at(decl->rhs->src, decl->rhs->line, decl->rhs->col,
                      "cannot initialize global '%s' with incompatible type",
                      decl->var_name);
        return 1;
    }
    decl->rhs->type = ty;

    if (!sema_global_initializer_is_const(decl->rhs))
    {
        diag_error_at(decl->rhs->src, decl->rhs->line, decl->rhs->col,
                      "global initializer for '%s' must be a constant expression",
                      decl->var_name);
        return 1;
    }

    return 0;
}

static int sema_check_function(SemaContext *sc, Node *fn)
{
    if (!fn->ret_type)
        fn->ret_type = &ty_i32;
    if (fn->is_chancecode)
        return 0;
    Node *body = fn->body;
    if (!body)
    {
        diag_error_at(fn->src, fn->line, fn->col, "missing function body");
        return 1;
    }
    scope_push(sc);
    for (int i = 0; i < fn->param_count; i++)
    {
        fn->param_types[i] = canonicalize_type_deep(fn->param_types[i]);
        scope_add(sc, fn->param_names[i], fn->param_types[i], 0);
    }
    int has_return = 0;
    int rc;
    if (body->kind == ND_BLOCK)
        rc = sema_check_block(sc, body, fn, &has_return, 0);
    else
        rc = sema_check_statement(sc, body, fn, &has_return);
    scope_pop(sc);
    if (rc)
        return rc;
    if ((!fn->ret_type || fn->ret_type->kind != TY_VOID) && !has_return)
    {
        diag_error_at(fn->src, fn->line, fn->col,
                      "function body must contain a return statement");
        return 1;
    }
    return 0;
}

int sema_check_unit(SemaContext *sc, Node *unit)
{
    if (!unit)
    {
        diag_error("null unit");
        return 1;
    }
    const Node *previous_unit = sc->unit;
    sc->unit = unit;
    if (unit->kind == ND_FUNC)
    {
        // single function case
        // add symbol so calls can resolve
        sema_register_function_local(sc, NULL, unit);
        if (check_exposed_function_signature(unit))
            return 1;
        int rc = sema_check_function(sc, unit);
        if (!rc)
            analyze_inline_candidates(unit);
        return rc;
    }
    if (unit->kind != ND_UNIT)
    {
        fprintf(stderr, "sema: expected unit\n");
        return 1;
    }
    // First pass: register functions and globals
    for (int i = 0; i < unit->stmt_count; i++)
    {
        Node *decl = unit->stmts[i];
        if (!decl)
            continue;
        if (decl->kind == ND_FUNC)
        {
            sema_register_function_local(sc, unit, decl);
        }
        else if (decl->kind == ND_VAR_DECL && decl->var_is_global)
        {
            sema_register_global_local(sc, unit, decl);
        }
        else
        {
            diag_error_at(decl ? decl->src : unit->src,
                          decl ? decl->line : unit->line,
                          decl ? decl->col : unit->col,
                          "unsupported top-level declaration");
            return 1;
        }
    }

    // Second pass: validate globals and type-check function bodies
    for (int i = 0; i < unit->stmt_count; i++)
    {
        Node *decl = unit->stmts[i];
        if (!decl)
            continue;
        if (decl->kind == ND_FUNC)
        {
            if (check_exposed_function_signature(decl))
                return 1;
            if (sema_check_function(sc, decl))
                return 1;
        }
        else if (decl->kind == ND_VAR_DECL && decl->var_is_global)
        {
            if (sema_check_global_decl(sc, decl))
                return 1;
        }
    }

    analyze_inline_candidates(unit);
    sc->unit = previous_unit;
    return 0;
}

static int inline_type_supported(Type *ty)
{
    if (!ty)
        return 1;
    ty = canonicalize_type_deep(ty);
    if (!ty)
        return 1;
    if (ty->kind == TY_STRUCT || ty->kind == TY_ARRAY)
        return 0;
    return 1;
}

static int inline_type_is_unsigned(Type *ty)
{
    ty = canonicalize_type_deep(ty);
    if (!ty)
        return 0;
    switch (ty->kind)
    {
    case TY_U8:
    case TY_U16:
    case TY_U32:
    case TY_U64:
        return 1;
    default:
        return 0;
    }
}

static int inline_type_bit_width(Type *ty)
{
    ty = canonicalize_type_deep(ty);
    if (!ty)
        return 0;
    switch (ty->kind)
    {
    case TY_BOOL:
        return 1;
    case TY_CHAR:
    case TY_I8:
    case TY_U8:
        return 8;
    case TY_I16:
    case TY_U16:
        return 16;
    case TY_I32:
    case TY_U32:
        return 32;
    case TY_I64:
    case TY_U64:
        return 64;
    default:
        return 0;
    }
}

static int64_t inline_normalize_value(int64_t value, Type *ty)
{
    if (!ty)
        return value;
    ty = canonicalize_type_deep(ty);
    if (!ty)
        return value;
    int width = inline_type_bit_width(ty);
    if (width <= 0)
        return value;
    if (width == 1 || ty->kind == TY_BOOL)
        return value ? 1 : 0;
    if (width >= 64)
        return value;
    uint64_t mask = (1ULL << width) - 1ULL;
    uint64_t uv = ((uint64_t)value) & mask;
    if (inline_type_is_unsigned(ty))
        return (int64_t)uv;
    uint64_t sign_bit = 1ULL << (width - 1);
    if (uv & sign_bit)
        uv |= (~mask);
    return (int64_t)uv;
}

static Node *inline_make_conditional(Node *cond, Node *then_expr, Node *else_expr, Type *ret_type)
{
    if (!cond || !then_expr || !else_expr)
        return NULL;
    Node *n = (Node *)xcalloc(1, sizeof(Node));
    n->kind = ND_COND;
    n->lhs = cond;
    n->rhs = then_expr;
    n->body = else_expr;
    n->type = canonicalize_type_deep(ret_type);
    if (cond->src)
    {
        n->src = cond->src;
        n->line = cond->line;
        n->col = cond->col;
    }
    else if (then_expr->src)
    {
        n->src = then_expr->src;
        n->line = then_expr->line;
        n->col = then_expr->col;
    }
    else if (else_expr->src)
    {
        n->src = else_expr->src;
        n->line = else_expr->line;
        n->col = else_expr->col;
    }
    return n;
}

static Node *inline_stmt_to_expr(const Node *stmt, Node *tail_expr, Type *ret_type)
{
    if (!stmt)
        return NULL;

    switch (stmt->kind)
    {
    case ND_BLOCK:
    {
        Node *expr = tail_expr;
        for (int i = stmt->stmt_count - 1; i >= 0; --i)
        {
            expr = inline_stmt_to_expr(stmt->stmts[i], expr, ret_type);
            if (!expr)
                return NULL;
        }
        return expr;
    }
    case ND_RET:
        if (!stmt->lhs)
            return NULL;
        if (tail_expr)
            return NULL;
        return stmt->lhs;
    case ND_IF:
    {
        Node *cond = stmt->lhs ? (Node *)stmt->lhs : NULL;
        if (!cond)
            return NULL;
        Node *then_expr = inline_stmt_to_expr(stmt->rhs, NULL, ret_type);
        if (!then_expr)
            return NULL;
        Node *else_expr = NULL;
        if (stmt->body)
        {
            if (tail_expr)
                return NULL;
            else_expr = inline_stmt_to_expr(stmt->body, NULL, ret_type);
            if (!else_expr)
                return NULL;
        }
        else
        {
            if (!tail_expr)
                return NULL;
            else_expr = tail_expr;
        }
        return inline_make_conditional(cond, then_expr, else_expr, ret_type);
    }
    default:
        return NULL;
    }
}

static const Node *inline_extract_return_expr(const Node *fn)
{
    if (!fn || fn->kind != ND_FUNC || !fn->body)
        return NULL;
    const Node *body = fn->body;
    if (body->kind != ND_BLOCK)
        return NULL;
    Node *expr = NULL;
    for (int i = body->stmt_count - 1; i >= 0; --i)
    {
        const Node *stmt = body->stmts ? body->stmts[i] : NULL;
        expr = inline_stmt_to_expr(stmt, expr, fn->ret_type);
        if (!expr)
            return NULL;
    }
    return expr;
}

typedef struct InlineBinding
{
    const char *name;
    int64_t value;
    Type *type;
} InlineBinding;

static int inline_lookup_binding(const InlineBinding *bindings, int count, const char *name, int64_t *out_value, Type **out_type)
{
    if (!bindings || count <= 0 || !name)
        return 0;
    for (int i = 0; i < count; ++i)
    {
        if (bindings[i].name && strcmp(bindings[i].name, name) == 0)
        {
            if (out_value)
                *out_value = bindings[i].value;
            if (out_type)
                *out_type = bindings[i].type;
            return 1;
        }
    }
    return 0;
}

static int inline_eval_const_expr(const Node *expr,
                                  const InlineBinding *bindings,
                                  int binding_count,
                                  int depth,
                                  int64_t *out_value);

static int inline_eval_inline_call(const Node *call_expr,
                                   const InlineBinding *bindings,
                                   int binding_count,
                                   int depth,
                                   int64_t *out_value)
{
    if (!call_expr || !out_value)
        return 0;
    if (!call_expr->call_target || call_expr->call_is_indirect)
        return 0;
    const Node *fn = call_expr->call_target;
    if (!fn->inline_candidate || !fn->inline_expr)
        return 0;
    if (!fn->ret_type || !type_is_int(fn->ret_type))
        return 0;
    if (fn->param_count != call_expr->arg_count)
        return 0;
    if (fn->param_count > INLINE_PARAM_LIMIT)
        return 0;

    InlineBinding params[INLINE_PARAM_LIMIT];
    for (int i = 0; i < fn->param_count; ++i)
    {
        const Node *arg = (call_expr->args && i < call_expr->arg_count) ? call_expr->args[i] : NULL;
        if (!arg)
            return 0;
        const char *param_name = (fn->param_names && i < fn->param_count) ? fn->param_names[i] : NULL;
        if (!param_name)
            return 0;
        int64_t arg_value = 0;
        if (!inline_eval_const_expr(arg, bindings, binding_count, depth + 1, &arg_value))
            return 0;
        Type *param_type = (fn->param_types && i < fn->param_count) ? fn->param_types[i] : NULL;
        params[i].name = param_name;
        params[i].value = inline_normalize_value(arg_value, param_type);
        params[i].type = param_type;
    }

    int64_t result = 0;
    if (!inline_eval_const_expr(fn->inline_expr, params, fn->param_count, depth + 1, &result))
        return 0;
    *out_value = inline_normalize_value(result, fn->ret_type);
    return 1;
}

static int inline_eval_const_expr(const Node *expr,
                                  const InlineBinding *bindings,
                                  int binding_count,
                                  int depth,
                                  int64_t *out_value)
{
    if (!expr || !out_value)
        return 0;
    if (depth > INLINE_EVAL_MAX_DEPTH)
        return 0;

    switch (expr->kind)
    {
    case ND_INT:
        *out_value = inline_normalize_value(expr->int_val, expr->type);
        return 1;
    case ND_VAR:
    {
        int64_t value = 0;
        Type *ty = expr->type;
        if (expr->var_ref && inline_lookup_binding(bindings, binding_count, expr->var_ref, &value, &ty))
        {
            *out_value = inline_normalize_value(value, ty ? ty : expr->type);
            return 1;
        }
        return 0;
    }
    case ND_NEG:
    {
        int64_t lhs = 0;
        if (!inline_eval_const_expr(expr->lhs, bindings, binding_count, depth + 1, &lhs))
            return 0;
        *out_value = inline_normalize_value(-lhs, expr->type);
        return 1;
    }
    case ND_BITNOT:
    {
        int64_t lhs = 0;
        if (!inline_eval_const_expr(expr->lhs, bindings, binding_count, depth + 1, &lhs))
            return 0;
        *out_value = inline_normalize_value(~lhs, expr->type);
        return 1;
    }
    case ND_ADD:
    case ND_SUB:
    case ND_MUL:
    case ND_DIV:
    case ND_SHL:
    case ND_SHR:
    case ND_BITAND:
    case ND_BITOR:
    case ND_BITXOR:
    {
        int64_t lhs = 0;
        int64_t rhs = 0;
        if (!inline_eval_const_expr(expr->lhs, bindings, binding_count, depth + 1, &lhs))
            return 0;
        if (!inline_eval_const_expr(expr->rhs, bindings, binding_count, depth + 1, &rhs))
            return 0;
        int64_t result = 0;
        switch (expr->kind)
        {
        case ND_ADD:
            result = lhs + rhs;
            break;
        case ND_SUB:
            result = lhs - rhs;
            break;
        case ND_MUL:
            result = lhs * rhs;
            break;
        case ND_DIV:
            if (rhs == 0)
                return 0;
            result = lhs / rhs;
            break;
        case ND_SHL:
            result = lhs << (int)(rhs & 63);
            break;
        case ND_SHR:
            if (inline_type_is_unsigned(expr->lhs ? expr->lhs->type : NULL))
                result = ((uint64_t)lhs) >> (int)(rhs & 63);
            else
                result = lhs >> (int)(rhs & 63);
            break;
        case ND_BITAND:
            result = lhs & rhs;
            break;
        case ND_BITOR:
            result = lhs | rhs;
            break;
        case ND_BITXOR:
            result = lhs ^ rhs;
            break;
        default:
            return 0;
        }
        *out_value = inline_normalize_value(result, expr->type);
        return 1;
    }
    case ND_LAND:
    {
        int64_t lhs = 0;
        if (!inline_eval_const_expr(expr->lhs, bindings, binding_count, depth + 1, &lhs))
            return 0;
        if (!lhs)
        {
            *out_value = inline_normalize_value(0, expr->type);
            return 1;
        }
        int64_t rhs = 0;
        if (!inline_eval_const_expr(expr->rhs, bindings, binding_count, depth + 1, &rhs))
            return 0;
        *out_value = inline_normalize_value(rhs ? 1 : 0, expr->type);
        return 1;
    }
    case ND_LOR:
    {
        int64_t lhs = 0;
        if (!inline_eval_const_expr(expr->lhs, bindings, binding_count, depth + 1, &lhs))
            return 0;
        if (lhs)
        {
            *out_value = inline_normalize_value(1, expr->type);
            return 1;
        }
        int64_t rhs = 0;
        if (!inline_eval_const_expr(expr->rhs, bindings, binding_count, depth + 1, &rhs))
            return 0;
        *out_value = inline_normalize_value(rhs ? 1 : 0, expr->type);
        return 1;
    }
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
    case ND_GE:
    case ND_GT_EXPR:
    {
        int64_t lhs = 0;
        int64_t rhs = 0;
        if (!inline_eval_const_expr(expr->lhs, bindings, binding_count, depth + 1, &lhs))
            return 0;
        if (!inline_eval_const_expr(expr->rhs, bindings, binding_count, depth + 1, &rhs))
            return 0;
        int64_t result = 0;
        switch (expr->kind)
        {
        case ND_EQ:
            result = (lhs == rhs);
            break;
        case ND_NE:
            result = (lhs != rhs);
            break;
        case ND_LT:
            result = lhs < rhs;
            break;
        case ND_LE:
            result = lhs <= rhs;
            break;
        case ND_GT_EXPR:
            result = lhs > rhs;
            break;
        case ND_GE:
            result = lhs >= rhs;
            break;
        default:
            return 0;
        }
        *out_value = inline_normalize_value(result ? 1 : 0, expr->type);
        return 1;
    }
    case ND_COND:
    {
        int64_t cond = 0;
        if (!inline_eval_const_expr(expr->lhs, bindings, binding_count, depth + 1, &cond))
            return 0;
        if (cond)
        {
            if (!inline_eval_const_expr(expr->rhs, bindings, binding_count, depth + 1, out_value))
                return 0;
            *out_value = inline_normalize_value(*out_value, expr->type);
            return 1;
        }
        if (!inline_eval_const_expr(expr->body, bindings, binding_count, depth + 1, out_value))
            return 0;
        *out_value = inline_normalize_value(*out_value, expr->type);
        return 1;
    }
    case ND_CAST:
    {
        int64_t val = 0;
        if (!inline_eval_const_expr(expr->lhs, bindings, binding_count, depth + 1, &val))
            return 0;
        *out_value = inline_normalize_value(val, expr->type);
        return 1;
    }
    case ND_CALL:
        return inline_eval_inline_call(expr, bindings, binding_count, depth + 1, out_value);
    default:
        return 0;
    }
}

static int inline_try_fold_call(Node *call_expr)
{
    if (!call_expr || call_expr->kind != ND_CALL)
        return 0;
    if (call_expr->call_is_indirect)
        return 0;
    if (!call_expr->call_target)
        return 0;
    const Node *fn = call_expr->call_target;
    if (!fn->inline_candidate || !fn->inline_expr)
        return 0;
    if (!fn->ret_type || !type_is_int(fn->ret_type))
        return 0;
    if (fn->param_count != call_expr->arg_count)
        return 0;
    if (fn->param_count > INLINE_PARAM_LIMIT)
        return 0;

    InlineBinding bindings[INLINE_PARAM_LIMIT];
    for (int i = 0; i < fn->param_count; ++i)
    {
        const Node *arg = (call_expr->args && i < call_expr->arg_count) ? call_expr->args[i] : NULL;
        if (!arg)
            return 0;
        const char *param_name = (fn->param_names && i < fn->param_count) ? fn->param_names[i] : NULL;
        if (!param_name)
            return 0;
        int64_t value = 0;
        if (!inline_eval_const_expr(arg, NULL, 0, 0, &value))
            return 0;
        Type *param_type = (fn->param_types && i < fn->param_count) ? fn->param_types[i] : NULL;
        bindings[i].name = param_name;
        bindings[i].value = inline_normalize_value(value, param_type);
        bindings[i].type = param_type;
    }

    int64_t result = 0;
    if (!inline_eval_const_expr(fn->inline_expr, bindings, fn->param_count, 0, &result))
        return 0;

    Type *ret_type = canonicalize_type_deep(fn->ret_type);
    result = inline_normalize_value(result, ret_type);

    call_expr->kind = ND_INT;
    call_expr->int_val = result;
    call_expr->lhs = NULL;
    call_expr->rhs = NULL;
    call_expr->body = NULL;
    call_expr->args = NULL;
    call_expr->arg_count = 0;
    call_expr->call_target = NULL;
    call_expr->call_name = NULL;
    call_expr->call_func_type = NULL;
    call_expr->call_is_indirect = 0;
    call_expr->call_is_varargs = 0;
    call_expr->type = ret_type;
    return 1;
}

static int inline_expr_cost(const Node *expr)
{
    if (!expr)
        return 0;
    int cost = 1;
    cost += inline_expr_cost(expr->lhs);
    cost += inline_expr_cost(expr->rhs);
    cost += inline_expr_cost(expr->body);
    if (expr->kind == ND_BLOCK && expr->stmts)
    {
        for (int i = 0; i < expr->stmt_count; ++i)
            cost += inline_expr_cost(expr->stmts[i]);
    }
    if (expr->kind == ND_CALL && expr->args)
    {
        for (int i = 0; i < expr->arg_count; ++i)
            cost += inline_expr_cost(expr->args[i]);
    }
    if (expr->kind == ND_INIT_LIST && expr->init.elems)
    {
        for (int i = 0; i < expr->init.count; ++i)
            cost += inline_expr_cost(expr->init.elems[i]);
    }
    return cost;
}

static void inline_walk_usage(Node *current_fn, const Node *node, const Node *parent)
{
    if (!node)
        return;

    if (node->kind == ND_CALL)
    {
        if (node->call_target && node->call_target == current_fn)
            current_fn->inline_recursive = 1;
    }

    if (node->kind == ND_VAR && node->referenced_function)
    {
        int is_call_target = parent && parent->kind == ND_CALL && parent->lhs == node;
        if (!is_call_target)
            node->referenced_function->inline_address_taken = 1;
    }

    inline_walk_usage(current_fn, node->lhs, node);
    inline_walk_usage(current_fn, node->rhs, node);
    inline_walk_usage(current_fn, node->body, node);

    if (node->kind == ND_BLOCK && node->stmts)
    {
        for (int i = 0; i < node->stmt_count; ++i)
            inline_walk_usage(current_fn, node->stmts[i], node);
    }

    if (node->kind == ND_CALL && node->args)
    {
        for (int i = 0; i < node->arg_count; ++i)
            inline_walk_usage(current_fn, node->args[i], node);
    }

    if (node->kind == ND_INIT_LIST && node->init.elems)
    {
        for (int i = 0; i < node->init.count; ++i)
            inline_walk_usage(current_fn, node->init.elems[i], node);
    }
}

static void analyze_inline_candidates(Node *root)
{
    if (!root)
        return;

    Node **functions = NULL;
    int fn_count = 0;
    int fn_cap = 0;

    if (root->kind == ND_FUNC)
    {
        fn_cap = 1;
        functions = (Node **)xcalloc((size_t)fn_cap, sizeof(Node *));
        functions[fn_count++] = root;
    }
    else if (root->kind == ND_UNIT)
    {
        for (int i = 0; i < root->stmt_count; ++i)
        {
            Node *decl = root->stmts[i];
            if (!decl || decl->kind != ND_FUNC)
                continue;
            if (fn_count == fn_cap)
            {
                int new_cap = fn_cap ? fn_cap * 2 : 8;
                Node **grown = (Node **)realloc(functions, (size_t)new_cap * sizeof(Node *));
                if (!grown)
                {
                    free(functions);
                    return;
                }
                functions = grown;
                if (new_cap > fn_cap)
                    memset(functions + fn_cap, 0, (size_t)(new_cap - fn_cap) * sizeof(Node *));
                fn_cap = new_cap;
            }
            functions[fn_count++] = decl;
        }
    }

    if (fn_count == 0)
    {
        free(functions);
        return;
    }

    for (int i = 0; i < fn_count; ++i)
    {
        Node *fn = functions[i];
        fn->inline_candidate = 0;
        fn->inline_cost = 0;
        fn->inline_address_taken = 0;
        fn->inline_recursive = 0;
        fn->inline_expr = NULL;
        fn->inline_needs_body = 1;
    }

    for (int i = 0; i < fn_count; ++i)
    {
        Node *fn = functions[i];
        if (fn->body)
            inline_walk_usage(fn, fn->body, NULL);
    }

    for (int i = 0; i < fn_count; ++i)
    {
        Node *fn = functions[i];
        if (!fn->wants_inline)
            continue;
        if (fn->is_chancecode)
            continue;
        if (fn->inline_address_taken)
            continue;
        if (fn->inline_recursive)
            continue;
        if (fn->is_varargs)
            continue;
        if (fn->param_count > INLINE_PARAM_LIMIT)
            continue;
        int params_ok = 1;
        for (int p = 0; p < fn->param_count; ++p)
        {
            Type *pty = (fn->param_types && p < fn->param_count) ? fn->param_types[p] : NULL;
            if (!inline_type_supported(pty))
            {
                params_ok = 0;
                break;
            }
        }
        if (!params_ok)
            continue;
        if (fn->ret_type && fn->ret_type->kind == TY_VOID)
            continue;
        if (!inline_type_supported(fn->ret_type))
            continue;
        const Node *ret_expr = inline_extract_return_expr(fn);
        if (!ret_expr)
            continue;
        int cost = inline_expr_cost(ret_expr);
        if (cost > INLINE_COST_LIMIT)
            continue;
        fn->inline_candidate = 1;
        fn->inline_cost = cost;
        fn->inline_expr = ret_expr;
        if (!fn->is_exposed && !fn->is_entrypoint)
            fn->inline_needs_body = 0;
        fprintf(stderr, "inline candidate: %s cost=%d\n", fn->name ? fn->name : "<anon>", cost);
    }

    free(functions);
}

void sema_register_foreign_unit_symbols(SemaContext *sc, Node *target_unit, Node *foreign_unit)
{
    if (!sc || !target_unit || target_unit->kind != ND_UNIT || !foreign_unit)
        return;

    if (foreign_unit->kind == ND_UNIT)
    {
        const char *module_full = foreign_unit->module_path.full_name;
        ImportStyle style = unit_import_style(target_unit, module_full);
        if (style == IMPORT_STYLE_NONE)
            return;
        int auto_import = (style == IMPORT_STYLE_AUTO);

        for (int i = 0; i < foreign_unit->stmt_count; ++i)
        {
            Node *decl = foreign_unit->stmts[i];
            if (!decl)
                continue;
            if (decl->kind == ND_FUNC)
                sema_register_function_foreign(sc, foreign_unit, decl, auto_import);
            else if (decl->kind == ND_VAR_DECL && decl->var_is_global)
                sema_register_global_foreign(sc, foreign_unit, decl);
        }
        return;
    }

    if (foreign_unit->kind == ND_FUNC)
    {
        sema_register_function_foreign(sc, NULL, foreign_unit, 0);
    }
}

// Simple constant evaluation for current subset
int sema_eval_const_i32(Node *expr)
{
    if (expr->kind == ND_INT)
        return (int)expr->int_val;
    if (expr->kind == ND_ADD)
        return sema_eval_const_i32(expr->lhs) + sema_eval_const_i32(expr->rhs);
    fprintf(stderr, "const eval: unsupported expression kind %d\n", expr->kind);
    return 0;
}
