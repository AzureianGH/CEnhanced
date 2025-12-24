#include "ast.h"
#include "mangle.h"
#include "module_registry.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <stdint.h>
#include <ctype.h>

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
static int template_param_allows(Type *t, TemplateConstraintKind want);
static int type_is_float(Type *t);
static int type_is_pointer(Type *t);
static int type_equal(Type *a, Type *b);
static int bind_template_type_pattern(Type *pattern, Type *actual,
                                      Type **bindings, int binding_count);
static int check_exposed_function_signature(const Node *fn);
static void sema_register_function_local(SemaContext *sc, Node *unit_node, Node *fn);
static int sema_check_function(SemaContext *sc, Node *fn);
static void sema_ensure_call_args_checked(SemaContext *sc, Node *call_expr, int *args_checked);
static const Symbol *sema_resolve_local_overload(SemaContext *sc, const char *name, Node *call_expr, int *args_checked);

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

static int type_contains_template_param(const Type *t)
{
    if (!t)
        return 0;
    t = canonicalize_type_deep((Type *)t);
    if (!t)
        return 0;
    if (t->kind == TY_TEMPLATE_PARAM)
        return 1;
    switch (t->kind)
    {
    case TY_PTR:
        return type_contains_template_param(t->pointee);
    case TY_ARRAY:
        return type_contains_template_param(t->array.elem);
    case TY_FUNC:
        if (t->func.ret && type_contains_template_param(t->func.ret))
            return 1;
        for (int i = 0; i < t->func.param_count; ++i)
        {
            Type *param = t->func.params ? t->func.params[i] : NULL;
            if (param && type_contains_template_param(param))
                return 1;
        }
        return 0;
    default:
        return 0;
    }
}

static int symbol_is_template_function(const Symbol *sym)
{
    if (!sym)
        return 0;
    if (sym->ast_node)
    {
        const Node *fn = sym->ast_node;
        if (fn->kind == ND_FUNC && fn->generic_param_count > 0)
            return 1;
    }
    if (sym->sig.param_count > 0)
    {
        for (int i = 0; i < sym->sig.param_count; ++i)
        {
            Type *param = sym->sig.params ? sym->sig.params[i] : NULL;
            if (param && type_contains_template_param(param))
                return 1;
        }
    }
    if (sym->sig.ret && type_contains_template_param(sym->sig.ret))
        return 1;
    return 0;
}

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

static int symbols_share_body(const Symbol *a, const Symbol *b)
{
    if (!a || !b)
        return 0;
    if (a->ast_node && b->ast_node && a->ast_node == b->ast_node)
        return 1;
    if (a->backend_name && b->backend_name && strcmp(a->backend_name, b->backend_name) == 0)
        return 1;
    return 0;
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

Symbol *sema_copy_imported_function_symbols(const SemaContext *sc, int *out_count)
{
    if (out_count)
        *out_count = 0;
    if (!sc || sc->imported_func_count == 0)
        return NULL;

    int total = 0;
    for (int i = 0; i < sc->imported_func_count; ++i)
    {
        ImportedFunctionSet *set = &sc->imported_funcs[i];
        if (!set)
            continue;
        total += set->count;
    }

    if (total <= 0)
        return NULL;

    Symbol *list = (Symbol *)xcalloc((size_t)total, sizeof(Symbol));
    int written = 0;
    for (int i = 0; i < sc->imported_func_count; ++i)
    {
        ImportedFunctionSet *set = &sc->imported_funcs[i];
        if (!set)
            continue;
        for (int c = 0; c < set->count; ++c)
        {
            const Symbol *sym = &set->candidates[c].symbol;
            if (!sym)
                continue;
            const char *name = sym->name;
            int duplicate = 0;
            if (name)
            {
                for (int j = 0; j < written; ++j)
                {
                    if (list[j].name && strcmp(list[j].name, name) == 0)
                    {
                        duplicate = 1;
                        break;
                    }
                }
            }
            if (duplicate)
                continue;
            list[written++] = *sym;
        }
    }

    if (written == 0)
    {
        free(list);
        return NULL;
    }

    if (out_count)
        *out_count = written;

    if (written < total)
    {
        Symbol *shrunk = (Symbol *)realloc(list, (size_t)written * sizeof(Symbol));
        if (shrunk)
            list = shrunk;
    }

    return list;
}

static const char *symbol_effective_name(const Symbol *sym)
{
    if (!sym)
        return NULL;
    if (sym->backend_name && sym->backend_name[0])
        return sym->backend_name;
    return sym->name;
}

static void sema_track_imported_global_usage(SemaContext *sc, const Symbol *sym)
{
    if (!sc || !sym || sym->kind != SYM_GLOBAL || !sym->is_extern)
        return;
    const char *name = symbol_effective_name(sym);
    if (!name || !*name)
        return;
    for (int i = 0; i < sc->imported_global_count; ++i)
    {
        const Symbol *existing = &sc->imported_globals[i];
        const char *existing_name = symbol_effective_name(existing);
        if (existing_name && strcmp(existing_name, name) == 0)
            return;
    }
    if (sc->imported_global_count == sc->imported_global_cap)
    {
        int new_cap = sc->imported_global_cap ? sc->imported_global_cap * 2 : 4;
        Symbol *grown = (Symbol *)realloc(sc->imported_globals, (size_t)new_cap * sizeof(Symbol));
        if (!grown)
        {
            diag_error("out of memory while tracking imported globals");
            exit(1);
        }
        sc->imported_globals = grown;
        sc->imported_global_cap = new_cap;
    }
    sc->imported_globals[sc->imported_global_count++] = *sym;
}

Symbol *sema_copy_imported_global_symbols(const SemaContext *sc, int *out_count)
{
    if (out_count)
        *out_count = 0;
    if (!sc || sc->imported_global_count <= 0)
        return NULL;

    Symbol *list = (Symbol *)xcalloc((size_t)sc->imported_global_count, sizeof(Symbol));
    if (!list)
        return NULL;
    memcpy(list, sc->imported_globals, (size_t)sc->imported_global_count * sizeof(Symbol));
    if (out_count)
        *out_count = sc->imported_global_count;
    return list;
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
    int is_static;
    const char *backend_name;
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
    Node *unit;
    ImportedFunctionSet *imported_funcs;
    int imported_func_count;
    int imported_func_cap;
    Symbol *imported_globals;
    int imported_global_count;
    int imported_global_cap;
    int loop_depth;
    int switch_depth;
    int lambda_counter;
};
SemaContext *sema_create(void)
{
    SemaContext *sc = (SemaContext *)xcalloc(1, sizeof(SemaContext));
    sc->syms = symtab_create();
    sc->unit = NULL;
    sc->imported_funcs = NULL;
    sc->imported_func_count = 0;
    sc->imported_func_cap = 0;
    sc->imported_globals = NULL;
    sc->imported_global_count = 0;
    sc->imported_global_cap = 0;
    sc->loop_depth = 0;
    sc->switch_depth = 0;
    sc->lambda_counter = 0;
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
    free(sc->imported_globals);
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
    char *backend = module_backend_name(module, fn, NULL);
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
    if (t->kind == TY_TEMPLATE_PARAM)
    {
        if (template_param_allows(t, TEMPLATE_CONSTRAINT_INTEGRAL))
            return 1;
        if (t->template_default_type && t->template_default_type != t)
            return type_is_int(t->template_default_type);
        return 0;
    }
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

static int template_param_allows(Type *t, TemplateConstraintKind want)
{
    t = canonicalize_type_deep(t);
    if (!t || t->kind != TY_TEMPLATE_PARAM)
        return 0;
    TemplateConstraintKind have = t->template_constraint_kind;
    switch (want)
    {
    case TEMPLATE_CONSTRAINT_INTEGRAL:
        return (have == TEMPLATE_CONSTRAINT_INTEGRAL || have == TEMPLATE_CONSTRAINT_NUMERIC);
    case TEMPLATE_CONSTRAINT_FLOATING:
        return (have == TEMPLATE_CONSTRAINT_FLOATING || have == TEMPLATE_CONSTRAINT_NUMERIC);
    case TEMPLATE_CONSTRAINT_NUMERIC:
        return (have == TEMPLATE_CONSTRAINT_NUMERIC || have == TEMPLATE_CONSTRAINT_INTEGRAL || have == TEMPLATE_CONSTRAINT_FLOATING);
    case TEMPLATE_CONSTRAINT_POINTER:
        return have == TEMPLATE_CONSTRAINT_POINTER;
    default:
        return 0;
    }
}

static int type_matches_constraint(Type *ty, TemplateConstraintKind constraint)
{
    if (constraint == TEMPLATE_CONSTRAINT_NONE)
        return 1;
    ty = canonicalize_type_deep(ty);
    if (!ty)
        return 0;
    switch (constraint)
    {
    case TEMPLATE_CONSTRAINT_INTEGRAL:
        return type_is_int(ty);
    case TEMPLATE_CONSTRAINT_FLOATING:
        return type_is_float(ty);
    case TEMPLATE_CONSTRAINT_NUMERIC:
        return type_is_int(ty) || type_is_float(ty);
    case TEMPLATE_CONSTRAINT_POINTER:
        return type_is_pointer(ty);
    default:
        return 1;
    }
}

typedef struct
{
    const Type *orig;
    Type *inst;
} TypeSubstCacheEntry;

static Type *instantiate_type_with_bindings(const Type *orig,
                                            Type **bindings,
                                            int binding_count,
                                            TypeSubstCacheEntry **cache,
                                            int *cache_count,
                                            int *cache_cap);

static Type *instantiate_type_with_bindings(const Type *orig,
                                            Type **bindings,
                                            int binding_count,
                                            TypeSubstCacheEntry **cache,
                                            int *cache_count,
                                            int *cache_cap)
{
    if (!orig)
        return NULL;
    if (orig->kind == TY_TEMPLATE_PARAM)
    {
        int idx = orig->template_param_index;
        if (idx >= 0 && idx < binding_count && bindings && bindings[idx])
            return bindings[idx];
        return (Type *)orig;
    }
    if (!bindings || binding_count <= 0)
        return (Type *)orig;
    if (orig->kind != TY_PTR && orig->kind != TY_ARRAY && orig->kind != TY_FUNC)
        return (Type *)orig;

    if (cache && cache_count && cache_cap)
    {
        for (int i = 0; i < *cache_count; ++i)
        {
            if ((*cache)[i].orig == orig)
                return (*cache)[i].inst;
        }
    }

    Type *result = NULL;
    switch (orig->kind)
    {
    case TY_PTR:
    {
        Type *pointee = instantiate_type_with_bindings(orig->pointee, bindings, binding_count, cache, cache_count, cache_cap);
        if (pointee == orig->pointee)
            result = (Type *)orig;
        else
        {
            Type *clone = type_ptr(pointee);
            result = clone;
        }
        break;
    }
    case TY_ARRAY:
    {
        Type *elem = instantiate_type_with_bindings(orig->array.elem, bindings, binding_count, cache, cache_count, cache_cap);
        if (elem == orig->array.elem)
            result = (Type *)orig;
        else
        {
            Type *clone = type_array(elem, orig->array.length);
            clone->array.is_unsized = orig->array.is_unsized;
            result = clone;
        }
        break;
    }
    case TY_FUNC:
    {
        Type *ret = instantiate_type_with_bindings(orig->func.ret, bindings, binding_count, cache, cache_count, cache_cap);
        int changed = (ret != orig->func.ret);
        Type **params = NULL;
        if (orig->func.param_count > 0)
        {
            params = (Type **)xcalloc((size_t)orig->func.param_count, sizeof(Type *));
            for (int i = 0; i < orig->func.param_count; ++i)
            {
                Type *sub = instantiate_type_with_bindings(orig->func.params ? orig->func.params[i] : NULL,
                                                           bindings, binding_count, cache, cache_count, cache_cap);
                params[i] = sub;
                if (sub != (orig->func.params ? orig->func.params[i] : NULL))
                    changed = 1;
            }
        }
        if (!changed)
        {
            free(params);
            result = (Type *)orig;
        }
        else
        {
            Type *clone = type_func();
            clone->func.param_count = orig->func.param_count;
            clone->func.params = params;
            clone->func.ret = ret;
            clone->func.is_varargs = orig->func.is_varargs;
            clone->func.has_signature = orig->func.has_signature;
            result = clone;
            params = NULL;
        }
        if (params)
            free(params);
        break;
    }
    default:
        result = (Type *)orig;
        break;
    }

    if (result && result != orig && cache && cache_count && cache_cap)
    {
        if (*cache_count == *cache_cap)
        {
            int new_cap = *cache_cap ? (*cache_cap * 2) : 8;
            TypeSubstCacheEntry *grown = (TypeSubstCacheEntry *)realloc(*cache, (size_t)new_cap * sizeof(TypeSubstCacheEntry));
            if (!grown)
            {
                diag_error("out of memory while caching type instantiations");
                exit(1);
            }
            *cache = grown;
            *cache_cap = new_cap;
        }
        (*cache)[*cache_count].orig = orig;
        (*cache)[*cache_count].inst = result;
        (*cache_count)++;
    }

    return result;
}

static Node *clone_node_tree_with_bindings(const Node *src,
                                           Type **bindings,
                                           int binding_count,
                                           TypeSubstCacheEntry **cache,
                                           int *cache_count,
                                           int *cache_cap)
{
    if (!src)
        return NULL;
    Node *dst = (Node *)xcalloc(1, sizeof(Node));
    memcpy(dst, src, sizeof(Node));

    dst->lhs = clone_node_tree_with_bindings(src->lhs, bindings, binding_count, cache, cache_count, cache_cap);
    dst->rhs = clone_node_tree_with_bindings(src->rhs, bindings, binding_count, cache, cache_count, cache_cap);
    dst->body = clone_node_tree_with_bindings(src->body, bindings, binding_count, cache, cache_count, cache_cap);

    if (src->arg_count > 0 && src->args)
    {
        dst->args = (Node **)xcalloc((size_t)src->arg_count, sizeof(Node *));
        for (int i = 0; i < src->arg_count; ++i)
            dst->args[i] = clone_node_tree_with_bindings(src->args[i], bindings, binding_count, cache, cache_count, cache_cap);
    }
    else
    {
        dst->args = NULL;
        dst->arg_count = src->arg_count;
    }

    if (src->call_type_arg_count > 0 && src->call_type_args)
    {
        dst->call_type_args = (Type **)xcalloc((size_t)src->call_type_arg_count, sizeof(Type *));
        for (int i = 0; i < src->call_type_arg_count; ++i)
            dst->call_type_args[i] = instantiate_type_with_bindings(src->call_type_args[i], bindings, binding_count, cache, cache_count, cache_cap);
    }
    else
    {
        dst->call_type_args = NULL;
        dst->call_type_arg_count = 0;
    }

    if (src->stmt_count > 0 && src->stmts)
    {
        dst->stmts = (Node **)xcalloc((size_t)src->stmt_count, sizeof(Node *));
        for (int i = 0; i < src->stmt_count; ++i)
            dst->stmts[i] = clone_node_tree_with_bindings(src->stmts[i], bindings, binding_count, cache, cache_count, cache_cap);
    }
    else
    {
        dst->stmts = NULL;
        dst->stmt_count = src->stmt_count;
    }

    if (src->init.count > 0)
    {
        if (src->init.elems)
        {
            dst->init.elems = (Node **)xcalloc((size_t)src->init.count, sizeof(Node *));
            for (int i = 0; i < src->init.count; ++i)
                dst->init.elems[i] = clone_node_tree_with_bindings(src->init.elems[i], bindings, binding_count, cache, cache_count, cache_cap);
        }
        if (src->init.designators)
        {
            dst->init.designators = (const char **)xcalloc((size_t)src->init.count, sizeof(const char *));
            for (int i = 0; i < src->init.count; ++i)
                dst->init.designators[i] = src->init.designators[i];
        }
        if (src->init.field_indices)
        {
            dst->init.field_indices = (int *)xcalloc((size_t)src->init.count, sizeof(int));
            memcpy(dst->init.field_indices, src->init.field_indices, (size_t)src->init.count * sizeof(int));
        }
    }

    dst->type = instantiate_type_with_bindings(src->type, bindings, binding_count, cache, cache_count, cache_cap);
    dst->var_type = instantiate_type_with_bindings(src->var_type, bindings, binding_count, cache, cache_count, cache_cap);
    dst->ret_type = instantiate_type_with_bindings(src->ret_type, bindings, binding_count, cache, cache_count, cache_cap);
    dst->call_func_type = NULL;
    dst->call_target = NULL;
    dst->referenced_function = NULL;
    dst->inline_expr = NULL;

    return dst;
}

static Node *instantiate_function_template(const Node *fn,
                                           char *inst_name,
                                           Type **bindings,
                                           int binding_count)
{
    if (!fn || fn->kind != ND_FUNC || !inst_name)
        return NULL;
    TypeSubstCacheEntry *cache = NULL;
    int cache_count = 0;
    int cache_cap = 0;

    Node *clone = (Node *)xcalloc(1, sizeof(Node));
    memcpy(clone, fn, sizeof(Node));
    clone->name = inst_name;
    clone->metadata = fn->metadata;
    clone->metadata.backend_name = inst_name;
    clone->generic_param_count = 0;
    clone->generic_param_names = NULL;
    clone->generic_param_types = NULL;
    clone->body = clone_node_tree_with_bindings(fn->body, bindings, binding_count, &cache, &cache_count, &cache_cap);
    clone->ret_type = instantiate_type_with_bindings(fn->ret_type, bindings, binding_count, &cache, &cache_count, &cache_cap);

    if (fn->param_count > 0 && fn->param_types)
    {
        clone->param_types = (Type **)xcalloc((size_t)fn->param_count, sizeof(Type *));
        for (int i = 0; i < fn->param_count; ++i)
            clone->param_types[i] = instantiate_type_with_bindings(fn->param_types[i], bindings, binding_count, &cache, &cache_count, &cache_cap);
    }
    else
    {
        clone->param_types = NULL;
        clone->param_count = fn->param_count;
    }

    if (fn->param_names && fn->param_count > 0)
    {
        clone->param_names = (const char **)xcalloc((size_t)fn->param_count, sizeof(char *));
        for (int i = 0; i < fn->param_count; ++i)
            clone->param_names[i] = fn->param_names[i];
    }
    else
    {
        clone->param_names = NULL;
    }

    if (fn->param_const_flags && fn->param_count > 0)
    {
        clone->param_const_flags = (unsigned char *)xmalloc((size_t)fn->param_count);
        memcpy(clone->param_const_flags, fn->param_const_flags, (size_t)fn->param_count);
    }
    else
    {
        clone->param_const_flags = NULL;
    }

    clone->call_target = NULL;
    clone->call_func_type = NULL;
    clone->referenced_function = NULL;
    clone->inline_expr = NULL;
    clone->is_entrypoint = 0;

    free(cache);
    return clone;
}

static char *constraint_name(TemplateConstraintKind kind)
{
    switch (kind)
    {
    case TEMPLATE_CONSTRAINT_INTEGRAL:
        return "integral";
    case TEMPLATE_CONSTRAINT_FLOATING:
        return "floating-point";
    case TEMPLATE_CONSTRAINT_NUMERIC:
        return "numeric";
    case TEMPLATE_CONSTRAINT_POINTER:
        return "pointer";
    default:
        return "unspecified";
    }
}

static char *make_template_instance_name(const Symbol *sym, Type **bindings, int binding_count)
{
    if (!sym || !sym->name)
        return NULL;
    size_t base_len = strlen(sym->name);
    size_t total = base_len + 8;
    char **chunks = NULL;
    if (binding_count > 0)
    {
        chunks = (char **)xcalloc((size_t)binding_count, sizeof(char *));
        for (int i = 0; i < binding_count; ++i)
        {
            char raw[128];
            describe_type(bindings[i], raw, sizeof(raw));
            size_t raw_len = strlen(raw);
            char *chunk = (char *)xmalloc(raw_len + 1);
            for (size_t j = 0; j < raw_len; ++j)
            {
                char c = raw[j];
                if (isalnum((unsigned char)c))
                    chunk[j] = c;
                else
                    chunk[j] = '_';
            }
            chunk[raw_len] = '\0';
            chunks[i] = chunk;
            total += raw_len + 8;
        }
    }
    char *name = (char *)xmalloc(total + 1);
    name[0] = '\0';
    strncat(name, sym->name, total);
    strncat(name, "__inst", total - strlen(name));
    for (int i = 0; i < binding_count; ++i)
    {
        char idxbuf[32];
        snprintf(idxbuf, sizeof(idxbuf), "__T%d_", i);
        strncat(name, idxbuf, total - strlen(name));
        if (chunks && chunks[i])
            strncat(name, chunks[i], total - strlen(name));
    }
    if (chunks)
    {
        for (int i = 0; i < binding_count; ++i)
            free(chunks[i]);
        free(chunks);
    }
    return name;
}

static void unit_append_function(Node *unit, Node *fn)
{
    if (!unit || unit->kind != ND_UNIT || !fn)
        return;
    int new_count = unit->stmt_count + 1;
    Node **grown = (Node **)realloc(unit->stmts, (size_t)new_count * sizeof(Node *));
    if (!grown)
    {
        diag_error("out of memory while registering instantiated function");
        exit(1);
    }
    unit->stmts = grown;
    unit->stmts[unit->stmt_count] = fn;
    unit->stmt_count = new_count;
}

static int sema_check_global_decl(SemaContext *sc, Node *decl);

static void unit_append_decl(Node *unit, Node *decl)
{
    if (!unit || unit->kind != ND_UNIT || !decl)
        return;
    int new_count = unit->stmt_count + 1;
    Node **grown = (Node **)realloc(unit->stmts, (size_t)new_count * sizeof(Node *));
    if (!grown)
    {
        diag_error("out of memory while appending declaration");
        exit(1);
    }
    unit->stmts = grown;
    unit->stmts[unit->stmt_count] = decl;
    unit->stmt_count = new_count;
}

static char *make_static_local_backend_name(const Node *fn, const char *var_name)
{
    const char *base = NULL;
    if (fn && fn->metadata.backend_name && fn->metadata.backend_name[0])
        base = fn->metadata.backend_name;
    else if (fn && fn->name)
        base = fn->name;
    else
        base = "__static";
    const char *suffix = (var_name && var_name[0]) ? var_name : "unnamed";
    size_t need = strlen(base) + strlen(suffix) + 12;
    char *buf = (char *)xmalloc(need);
    snprintf(buf, need, "%s__static_%s", base, suffix);
    return buf;
}

static char *sema_make_lambda_name(SemaContext *sc)
{
    int index = sc ? sc->lambda_counter++ : 0;
    char buf[64];
    snprintf(buf, sizeof(buf), "__lambda_%d", index);
    return xstrdup(buf);
}

static Type *sema_make_func_type_from_node(const Node *fn)
{
    if (!fn)
        return NULL;
    FuncSig sig = {0};
    sig.ret = fn->ret_type;
    sig.params = fn->param_types;
    sig.param_count = fn->param_count;
    sig.is_varargs = fn->is_varargs;
    return make_function_type_from_sig(&sig);
}

static void sema_lower_lambda(SemaContext *sc, Node *lambda)
{
    if (!sc || !lambda || lambda->kind != ND_LAMBDA)
        return;
    if (!sc->unit || sc->unit->kind != ND_UNIT)
    {
        diag_error_at(lambda->src, lambda->line, lambda->col,
                      "lambda expressions require a translation unit context");
        exit(1);
    }

    Node *fn = (Node *)xcalloc(1, sizeof(Node));
    fn->kind = ND_FUNC;
    fn->name = sema_make_lambda_name(sc);
    fn->line = lambda->line;
    fn->col = lambda->col;
    fn->src = lambda->src;
    fn->ret_type = lambda->ret_type;
    lambda->ret_type = NULL;
    fn->param_types = lambda->param_types;
    lambda->param_types = NULL;
    fn->param_names = lambda->param_names;
    lambda->param_names = NULL;
    fn->param_const_flags = lambda->param_const_flags;
    lambda->param_const_flags = NULL;
    fn->param_count = lambda->param_count;
    lambda->param_count = 0;
    fn->is_varargs = lambda->is_varargs;
    lambda->is_varargs = 0;
    fn->body = lambda->body;
    lambda->body = NULL;
    fn->is_exposed = 0;
    fn->is_noreturn = 0;
    fn->metadata.declared_param_count = -1;
    fn->metadata.declared_local_count = -1;
    fn->metadata.backend_name = fn->name ? xstrdup(fn->name) : NULL;

    unit_append_function(sc->unit, fn);
    sema_register_function_local(sc, sc->unit, fn);

    Type *func_ty = sema_make_func_type_from_node(fn);
    Type *func_ptr_ty = func_ty ? type_ptr(func_ty) : NULL;

    Node *fn_ref = (Node *)xcalloc(1, sizeof(Node));
    fn_ref->kind = ND_VAR;
    fn_ref->var_ref = fn->name ? xstrdup(fn->name) : NULL;
    fn_ref->var_is_const = 1;
    fn_ref->var_is_function = 1;
    fn_ref->referenced_function = fn;
    fn_ref->type = func_ty;
    fn_ref->var_type = func_ty;
    fn_ref->line = lambda->line;
    fn_ref->col = lambda->col;
    fn_ref->src = lambda->src;

    lambda->kind = ND_ADDR;
    lambda->lhs = fn_ref;
    lambda->rhs = NULL;
    lambda->type = func_ptr_ty;
}

static Node *clone_function_var_ref(const Node *var)
{
    if (!var || var->kind != ND_VAR)
        return NULL;
    Node *clone = (Node *)xcalloc(1, sizeof(Node));
    clone->kind = ND_VAR;
    clone->var_ref = var->var_ref ? xstrdup(var->var_ref) : NULL;
    clone->var_type = var->var_type;
    clone->type = var->type;
    clone->var_is_const = var->var_is_const;
    clone->var_is_static = var->var_is_static;
    clone->var_is_global = var->var_is_global;
    clone->var_is_array = var->var_is_array;
    clone->var_is_function = var->var_is_function;
    clone->referenced_function = var->referenced_function;
    clone->module_ref = var->module_ref;
    clone->module_ref_parts = var->module_ref_parts;
    clone->module_type_name = var->module_type_name;
    clone->module_type_is_enum = var->module_type_is_enum;
    clone->line = var->line;
    clone->col = var->col;
    clone->src = var->src;
    return clone;
}

static Node *clone_function_pointer_expr(const Node *expr)
{
    if (!expr)
        return NULL;
    if (expr->kind == ND_ADDR && expr->lhs && expr->lhs->kind == ND_VAR)
    {
        Node *clone = (Node *)xcalloc(1, sizeof(Node));
        clone->kind = ND_ADDR;
        clone->lhs = clone_function_var_ref(expr->lhs);
        clone->type = expr->type;
        clone->line = expr->line;
        clone->col = expr->col;
        clone->src = expr->src;
        return clone;
    }
    if (expr->kind == ND_VAR && expr->var_is_function)
    {
        Node *clone = clone_function_var_ref(expr);
        clone->type = expr->type;
        return clone;
    }
    return NULL;
}

static const Symbol *sema_instantiate_generic_call(SemaContext *sc,
                                                   const Symbol *template_sym,
                                                   Node *call_expr,
                                                   int *args_checked)
{
    if (!sc || !template_sym || !call_expr || !template_sym->ast_node)
        return NULL;
    const Node *template_fn = template_sym->ast_node;
    if (!template_fn || template_fn->kind != ND_FUNC || template_fn->generic_param_count <= 0)
        return NULL;

    int template_arg_count = template_fn->generic_param_count;
    Type **bindings = (Type **)xcalloc((size_t)template_arg_count, sizeof(Type *));

    if (call_expr->call_type_arg_count > template_arg_count)
    {
        diag_error_at(call_expr->src, call_expr->line, call_expr->col,
                      "function '%s' expects %d template argument(s) but %d provided",
                      template_sym->name, template_arg_count, call_expr->call_type_arg_count);
        exit(1);
    }

    for (int i = 0; i < call_expr->call_type_arg_count && i < template_arg_count; ++i)
    {
        Type *explicit_ty = canonicalize_type_deep(call_expr->call_type_args ? call_expr->call_type_args[i] : NULL);
        if (!explicit_ty)
        {
            diag_error_at(call_expr->src, call_expr->line, call_expr->col,
                          "invalid explicit template argument for parameter %d",
                          i + 1);
            exit(1);
        }
        bindings[i] = explicit_ty;
    }

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

    for (int i = 0; i < template_fn->param_count && i < call_expr->arg_count; ++i)
    {
        Type *param_pattern = template_fn->param_types ? template_fn->param_types[i] : NULL;
        Node *arg_node = (call_expr->args && i < call_expr->arg_count) ? call_expr->args[i] : NULL;
        if (!param_pattern || !arg_node || !arg_node->type)
            continue;
        if (!bind_template_type_pattern(param_pattern, arg_node->type, bindings, template_arg_count))
        {
            const char *param_name = (template_fn->param_names && i < template_fn->param_count) ? template_fn->param_names[i] : NULL;
            char want[64];
            describe_type(param_pattern, want, sizeof(want));
            char got[64];
            describe_type(arg_node->type, got, sizeof(got));
            diag_error_at(arg_node->src, arg_node->line, arg_node->col,
                          "cannot match argument type %s to template parameter %s of '%s'",
                          got, param_name ? param_name : "", template_sym->name);
            exit(1);
        }
    }

    for (int i = 0; i < template_arg_count; ++i)
    {
        Type *placeholder = (template_fn->generic_param_types && i < template_fn->generic_param_count)
                                ? template_fn->generic_param_types[i]
                                : NULL;
        if (!bindings[i] && placeholder && placeholder->template_default_type)
            bindings[i] = canonicalize_type_deep(placeholder->template_default_type);
        if (!bindings[i])
        {
            const char *pname = (template_fn->generic_param_names && i < template_fn->generic_param_count)
                                    ? template_fn->generic_param_names[i]
                                    : NULL;
            diag_error_at(call_expr->src, call_expr->line, call_expr->col,
                          "unable to deduce template parameter '%s' for call to '%s'",
                          pname ? pname : "T", template_sym->name);
            exit(1);
        }
        TemplateConstraintKind constraint = placeholder ? placeholder->template_constraint_kind : TEMPLATE_CONSTRAINT_NONE;
        if (constraint != TEMPLATE_CONSTRAINT_NONE && !type_matches_constraint(bindings[i], constraint))
        {
            char tybuf[64];
            describe_type(bindings[i], tybuf, sizeof(tybuf));
            const char *pname = (template_fn->generic_param_names && i < template_fn->generic_param_count)
                                    ? template_fn->generic_param_names[i]
                                    : NULL;
            diag_error_at(call_expr->src, call_expr->line, call_expr->col,
                          "template parameter '%s' of '%s' requires %s type but argument is %s",
                          pname ? pname : "T", template_sym->name,
                          constraint_name(constraint), tybuf);
            exit(1);
        }
    }

    char *inst_name = make_template_instance_name(template_sym, bindings, template_arg_count);
    if (!inst_name)
    {
        diag_error("failed to mangle template instance name for '%s'", template_sym->name);
        exit(1);
    }

    const Symbol *existing = symtab_get(sc->syms, inst_name);
    if (existing && existing->kind == SYM_FUNC)
    {
        free(bindings);
        call_expr->call_name = existing->name;
        call_expr->call_target = existing->ast_node;
        call_expr->call_is_indirect = 0;
        free(inst_name);
        return existing;
    }

    Node *inst_fn = instantiate_function_template(template_fn, inst_name, bindings, template_arg_count);
    unit_append_function(sc->unit, inst_fn);
    sema_register_function_local(sc, sc->unit, inst_fn);
    if (check_exposed_function_signature(inst_fn))
    {
        diag_error_at(call_expr->src, call_expr->line, call_expr->col,
                      "instantiated template '%s' violates exposure rules", template_sym->name);
        exit(1);
    }
    if (sema_check_function(sc, inst_fn))
    {
        diag_error_at(call_expr->src, call_expr->line, call_expr->col,
                      "failed to type-check instantiated template '%s'", template_sym->name);
        exit(1);
    }

    const Symbol *inst_sym = symtab_get(sc->syms, inst_fn->name);
    if (!inst_sym)
    {
        diag_error("internal error: missing symbol for instantiated template '%s'", inst_fn->name);
        exit(1);
    }

    call_expr->call_name = inst_fn->name;
    call_expr->call_target = inst_fn;
    call_expr->call_is_indirect = 0;

    free(bindings);
    return inst_sym;
}

static int bind_template_type_pattern(Type *pattern, Type *actual,
                                      Type **bindings, int binding_count)
{
    if (!pattern || !actual || !bindings || binding_count <= 0)
        return 1;
    pattern = canonicalize_type_deep(pattern);
    actual = canonicalize_type_deep(actual);
    if (!pattern || !actual)
        return 1;
    if (pattern->kind == TY_TEMPLATE_PARAM)
    {
        int idx = pattern->template_param_index;
        if (idx < 0 || idx >= binding_count)
            return 0;
        if (!bindings[idx])
        {
            bindings[idx] = actual;
            return 1;
        }
        return type_equal(bindings[idx], actual);
    }
    if (pattern->kind != actual->kind)
        return 0;
    switch (pattern->kind)
    {
    case TY_PTR:
        if (!pattern->pointee || !actual->pointee)
            return 0;
        return bind_template_type_pattern(pattern->pointee, actual->pointee, bindings, binding_count);
    case TY_ARRAY:
        if (pattern->array.length != actual->array.length ||
            pattern->array.is_unsized != actual->array.is_unsized)
            return 0;
        return bind_template_type_pattern(pattern->array.elem, actual->array.elem, bindings, binding_count);
    case TY_FUNC:
    {
        if (!!pattern->func.ret != !!actual->func.ret)
            return 0;
        if (pattern->func.param_count != actual->func.param_count)
            return 0;
        if (pattern->func.is_varargs != actual->func.is_varargs)
            return 0;
        if (pattern->func.ret &&
            !bind_template_type_pattern(pattern->func.ret, actual->func.ret, bindings, binding_count))
            return 0;
        for (int i = 0; i < pattern->func.param_count; ++i)
        {
            Type *pp = pattern->func.params ? pattern->func.params[i] : NULL;
            Type *ap = actual->func.params ? actual->func.params[i] : NULL;
            if (!bind_template_type_pattern(pp, ap, bindings, binding_count))
                return 0;
        }
        return 1;
    }
    default:
        return type_equal(pattern, actual);
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
    ImportedFunctionCandidate *template_match = NULL;
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
        int is_template = symbol_is_template_function(&cand->symbol);
        if (is_template)
        {
            if (!template_match)
            {
                template_match = cand;
                continue;
            }

            if (funcsig_equal(&template_match->symbol.sig, &cand->symbol.sig))
            {
                const char *mod_a = template_match->module_full ? template_match->module_full : "<unknown>";
                const char *mod_b = cand->module_full ? cand->module_full : "<unknown>";
                diag_error_at(call_expr->src, call_expr->line, call_expr->col,
                              "ambiguous call to '%s'; both modules '%s' and '%s' provide identical overloads",
                              name, mod_a, mod_b);
                exit(1);
            }
            else
            {
                const char *mod_a = template_match->module_full ? template_match->module_full : "<unknown>";
                const char *mod_b = cand->module_full ? cand->module_full : "<unknown>";
                diag_error_at(call_expr->src, call_expr->line, call_expr->col,
                              "ambiguous call to '%s'; matches found in modules '%s' and '%s'",
                              name, mod_a, mod_b);
                exit(1);
            }
        }

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

    if (!match && template_match)
        return template_match;

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

static void sema_ensure_call_args_checked(SemaContext *sc, Node *call_expr, int *args_checked)
{
    if (!sc || !call_expr)
        return;
    if (args_checked && *args_checked)
        return;
    for (int i = 0; i < call_expr->arg_count; ++i)
    {
        if (call_expr->args && call_expr->args[i])
            check_expr(sc, call_expr->args[i]);
    }
    if (args_checked)
        *args_checked = 1;
}

static const Symbol *sema_resolve_local_overload(SemaContext *sc,
                                                 const char *name,
                                                 Node *call_expr,
                                                 int *args_checked)
{
    if (!sc || !sc->syms || !name || !*name || !call_expr)
        return NULL;

    sema_ensure_call_args_checked(sc, call_expr, args_checked);

    const Symbol *match = NULL;
    const Symbol *template_candidate = NULL;
    int candidate_count = 0;

    for (int i = 0; i < sc->syms->count; ++i)
    {
        const Symbol *sym = &sc->syms->items[i];
        if (!sym || sym->kind != SYM_FUNC || !sym->name)
            continue;
        if (strcmp(sym->name, name) != 0)
            continue;

        if (symbol_is_template_function(sym))
        {
            if (!template_candidate)
            {
                template_candidate = sym;
            }
            else if (!symbols_share_body(template_candidate, sym))
            {
                diag_error_at(call_expr->src, call_expr->line, call_expr->col,
                              "ambiguous call to '%s'; multiple template overloads available",
                              name);
                exit(1);
            }
            continue;
        }

        candidate_count++;

        if (!call_arguments_match_signature(call_expr, &sym->sig))
            continue;

        if (match && match != sym && !symbols_share_body(match, sym))
        {
            diag_error_at(call_expr->src, call_expr->line, call_expr->col,
                          "ambiguous call to '%s'; multiple overloads match provided arguments",
                          name);
            exit(1);
        }

        match = sym;
    }

    if (match)
        return match;

    if (template_candidate)
        return template_candidate;

    if (!candidate_count)
        return NULL;

    diag_error_at(call_expr->src, call_expr->line, call_expr->col,
                  "no overload of '%s' matches provided arguments", name);
    exit(1);

    return NULL;
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
    if (t->kind == TY_TEMPLATE_PARAM)
    {
        if (t->template_default_type)
            return type_is_exportable(t->template_default_type);
        return 1;
    }
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

static int alignof_type(Type *ty)
{
    ty = canonicalize_type_deep(ty);
    if (!ty)
        return 1;
    switch (ty->kind)
    {
    case TY_BOOL:
    case TY_CHAR:
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
    case TY_VA_LIST:
        return 8;
    case TY_F128:
        return 16;
    case TY_ARRAY:
        if (ty->array.elem)
            return alignof_type(ty->array.elem);
        return 8;
    case TY_STRUCT:
    {
        int max_align = 1;
        if (!ty->strct.field_types || ty->strct.field_count <= 0)
            return max_align;
        for (int i = 0; i < ty->strct.field_count; ++i)
        {
            Type *field_ty = ty->strct.field_types[i];
            int field_align = alignof_type(field_ty);
            if (field_align > max_align)
                max_align = field_align;
        }
        return max_align > 0 ? max_align : 1;
    }
    case TY_VOID:
        return 1;
    default:
        return 1;
    }
}

static void populate_symbol_from_function(Symbol *s, Node *fn)
{
    if (!s || !fn || fn->kind != ND_FUNC)
        return;

    s->kind = SYM_FUNC;
    s->name = fn->name;
    if (!fn->metadata.backend_name && !fn->is_entrypoint)
    {
        char *generated = append_param_signature(fn->name, fn);
        if (generated)
            fn->metadata.backend_name = generated;
    }
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

    if (module_full && !fn->metadata.backend_name && !fn->is_entrypoint && !fn->export_name)
    {
        char *backend = module_backend_name(module_full, fn->name, fn);
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

    if (module_full && !decl->metadata.backend_name && !decl->export_name)
    {
        char *backend = module_backend_name(module_full, decl->var_name, NULL);
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

    if (!fn->metadata.backend_name && !fn->is_entrypoint && !fn->export_name)
    {
        char *backend = module_backend_name(module_full, fn->name, fn);
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

    if (!decl->metadata.backend_name && !decl->export_name)
    {
        char *backend = module_backend_name(module_full, decl->var_name, NULL);
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
    if (t->kind == TY_TEMPLATE_PARAM)
    {
        if (template_param_allows(t, TEMPLATE_CONSTRAINT_FLOATING))
            return 1;
        if (t->template_default_type && t->template_default_type != t)
            return type_is_float(t->template_default_type);
        return 0;
    }
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
    if (t->kind == TY_TEMPLATE_PARAM)
    {
        if (template_param_allows(t, TEMPLATE_CONSTRAINT_POINTER))
            return 1;
        if (t->template_default_type && t->template_default_type != t)
            return type_is_pointer(t->template_default_type);
        return 0;
    }
    if (t->kind == TY_PTR)
        return 1;
    if (t->kind == TY_ARRAY && t->array.is_unsized)
        return 1;
    return 0;
}

static int type_is_function_pointer(const Type *t)
{
    t = canonicalize_type_deep((Type *)t);
    if (!t || t->kind != TY_PTR || !t->pointee)
        return 0;
    Type *pointee = canonicalize_type_deep(t->pointee);
    return pointee && pointee->kind == TY_FUNC;
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

static int node_force_int_literal(Node *n)
{
    if (!n)
        return 0;
    if (n->kind == ND_INT)
        return 1;
    if (n->kind == ND_NEG && n->lhs)
    {
        if (!node_force_int_literal(n->lhs))
            return 0;
        if (n->lhs->kind != ND_INT)
            return 0;
        int64_t val = n->lhs->int_val;
        int was_unsigned = n->lhs->int_is_unsigned;
        ast_free(n->lhs);
        n->lhs = NULL;
        if (n->rhs)
        {
            ast_free(n->rhs);
            n->rhs = NULL;
        }
        n->kind = ND_INT;
        n->int_val = -val;
        n->int_is_unsigned = was_unsigned;
        return 1;
    }
    if (n->kind == ND_CAST && n->lhs && type_is_int(n->type))
    {
        if (!node_force_int_literal(n->lhs))
            return 0;
        if (n->lhs->kind != ND_INT)
            return 0;
        int64_t val = n->lhs->int_val;
        int was_unsigned = n->lhs->int_is_unsigned;
        ast_free(n->lhs);
        n->lhs = NULL;
        if (n->rhs)
        {
            ast_free(n->rhs);
            n->rhs = NULL;
        }
        n->kind = ND_INT;
        n->int_val = val;
        n->int_is_unsigned = was_unsigned;
        return 1;
    }
    return 0;
}

static int coerce_int_literal_to_type(Node *literal, Type *target, const char *context)
{
    if (!literal || !target)
        return 0;
    if (!node_force_int_literal(literal) && literal->kind != ND_INT)
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
    literal->int_is_unsigned = type_is_unsigned_int(canon_target) ? 1 : 0;

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

static void copy_function_signature(Type *dst, const Type *src)
{
    if (!dst || !src || dst == src)
        return;
    if (dst->kind != TY_FUNC || src->kind != TY_FUNC)
        return;
    if (dst->func.params)
        free(dst->func.params);
    dst->func.param_count = src->func.param_count;
    if (dst->func.param_count > 0)
    {
        dst->func.params = (Type **)xcalloc((size_t)dst->func.param_count, sizeof(Type *));
        for (int i = 0; i < dst->func.param_count; ++i)
            dst->func.params[i] = src->func.params ? src->func.params[i] : NULL;
    }
    else
    {
        dst->func.params = NULL;
    }
    dst->func.ret = src->func.ret;
    dst->func.is_varargs = src->func.is_varargs;
    dst->func.has_signature = src->func.has_signature;
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
    if (canon_target->kind == TY_PTR && canon_target->pointee && canon_target->pointee->kind == TY_FUNC)
    {
        Type *rhs_ty = canonicalize_type_deep(rhs->type);
        if (rhs_ty && rhs_ty->kind == TY_PTR && rhs_ty->pointee && rhs_ty->pointee->kind == TY_FUNC)
        {
            Type *dst_func = canonicalize_type_deep(canon_target->pointee);
            Type *src_func = canonicalize_type_deep(rhs_ty->pointee);
            if (dst_func && src_func && src_func->func.has_signature)
            {
                if (!dst_func->func.has_signature)
                    copy_function_signature(dst_func, src_func);
                if (type_equals(dst_func, src_func))
                {
                    rhs->type = canon_target;
                    return 1;
                }
            }
        }
    }
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
        Node *arg = call_expr->args[i];
        Type *saved_type = arg->type;
        int64_t saved_int = arg->int_val;
        if (!can_assign(expected_ty, arg))
        {
            arg->type = saved_type;
            arg->int_val = saved_int;
            return 0;
        }
        arg->type = saved_type;
        arg->int_val = saved_int;
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
        int count = init->init.count;
        if (count != 1)
        {
            diag_error_at(init->src, init->line, init->col,
                          "brace initializer for this type requires exactly one element");
            exit(1);
        }
        const char *designator = init->init.designators ? init->init.designators[0] : NULL;
        if (designator)
        {
            diag_error_at(init->src, init->line, init->col,
                          "designators are not supported for this initializer");
            exit(1);
        }
        Node *elem = (init->init.elems && init->init.count > 0) ? init->init.elems[0] : NULL;
        if (!elem)
        {
            diag_error_at(init->src, init->line, init->col,
                          "missing initializer expression");
            exit(1);
        }
        if (elem->kind == ND_INIT_LIST)
        {
            diag_error_at(elem->src, elem->line, elem->col,
                          "nested initializer lists are not supported for this type");
            exit(1);
        }
        check_expr(sc, elem);
        if (target && !can_assign(target, elem))
        {
            diag_error_at(elem->src, elem->line, elem->col,
                          "initializer expression type mismatch");
            exit(1);
        }
        init->type = target;
        return;
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
static const struct VarBind *scope_get_binding(SemaContext *sc, const char *name)
{
    for (struct Scope *s = sc ? sc->scope : NULL; s; s = s->parent)
    {
        for (int i = 0; i < s->local_count; i++)
        {
            if (strcmp(s->locals[i].name, name) == 0)
                return &s->locals[i];
        }
    }
    return NULL;
}
static void scope_add(SemaContext *sc, const char *name, Type *ty,
                      int is_const, int is_static, const char *backend_name)
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
        s->locals[s->local_count].is_static = is_static;
        s->locals[s->local_count].backend_name = backend_name;
        s->local_count++;
    }
}
static int scope_is_const(SemaContext *sc, const char *name)
{
    const struct VarBind *b = scope_get_binding(sc, name);
    return b ? b->is_const : 0;
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

    const struct VarBind *binding = scope_get_binding(sc, name);
    if (binding)
    {
        if (is_const)
            *is_const = binding->is_const;
        if (is_global)
            *is_global = binding->is_static;
        if (is_function)
            *is_function = 0;
        if (out_sym)
            *out_sym = NULL;
        return canonicalize_type_deep(binding->type);
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
    case ND_ALIGNOF:
        return "ND_ALIGNOF";
    case ND_OFFSETOF:
        return "ND_OFFSETOF";
    default:
        return "<unknown-node>";
    }
}

static Node *strip_casts(Node *n)
{
    while (n && n->kind == ND_CAST && n->lhs)
        n = n->lhs;
    return n;
}

static int expr_has_pointer_override(const Node *expr)
{
    const Node *n = expr;
    while (n && n->kind == ND_CAST)
    {
        Type *cast_ty = canonicalize_type_deep(n->type);
        if (cast_ty && cast_ty->kind == TY_PTR)
            return 1;
        n = n->lhs;
    }
    return 0;
}

static int expr_has_any_cast(const Node *expr)
{
    const Node *n = expr;
    while (n && n->kind == ND_CAST)
        return 1;
    return 0;
}

static const Node *find_const_storage_origin(Node *expr);
static const Node *find_const_pointer_origin(Node *expr);
static const Node *find_pointer_to_const_origin(Node *expr);
static Type *sema_prepare_assignment_lhs(SemaContext *sc, Node *assign_expr, Node **out_lhs_base);

static const Node *find_const_pointer_origin(Node *expr)
{
    expr = strip_casts(expr);
    if (!expr)
        return NULL;
    switch (expr->kind)
    {
    case ND_VAR:
        if (expr->var_is_function)
            return NULL;
        return expr->var_is_const ? expr : NULL;
    case ND_MEMBER:
        return find_const_storage_origin(expr);
    case ND_INDEX:
    case ND_DEREF:
        if (!expr->lhs)
            return NULL;
        return find_const_pointer_origin(expr->lhs);
    case ND_ADDR:
        if (!expr->lhs)
            return NULL;
        return find_const_storage_origin(expr->lhs);
    case ND_CAST:
        return find_const_pointer_origin(expr->lhs);
    case ND_ADD:
    case ND_SUB:
    {
        const Node *lhs_origin = NULL;
        if (expr->lhs)
        {
            Type *lhs_type = canonicalize_type_deep(expr->lhs->type);
            if (lhs_type && lhs_type->kind == TY_PTR)
                lhs_origin = find_const_pointer_origin(expr->lhs);
        }
        if (lhs_origin)
            return lhs_origin;
        if (expr->rhs)
        {
            Type *rhs_type = canonicalize_type_deep(expr->rhs->type);
            if (rhs_type && rhs_type->kind == TY_PTR)
                return find_const_pointer_origin(expr->rhs);
        }
        return NULL;
    }
    default:
        return NULL;
    }
}

static const Node *find_const_storage_origin(Node *expr)
{
    expr = strip_casts(expr);
    if (!expr)
        return NULL;
    switch (expr->kind)
    {
    case ND_VAR:
        if (expr->var_is_function)
            return NULL;
        return expr->var_is_const ? expr : NULL;
    case ND_MEMBER:
        if (!expr->lhs)
            return NULL;
        if (expr->is_pointer_deref)
            return find_const_pointer_origin(expr->lhs);
        return find_const_storage_origin(expr->lhs);
    case ND_INDEX:
    case ND_DEREF:
        if (!expr->lhs)
            return NULL;
        return find_const_pointer_origin(expr->lhs);
    default:
        return NULL;
    }
}

static const Node *find_pointer_to_const_origin(Node *expr)
{
    expr = strip_casts(expr);
    if (!expr)
        return NULL;
    Type *ty = canonicalize_type_deep(expr->type);
    if (ty && ty->kind == TY_PTR && ty->pointee)
    {
        Type *pointee = canonicalize_type_deep(ty->pointee);
        if (pointee && pointee->kind == TY_PTR)
        {
            const Node *inner = find_const_storage_origin(expr);
            if (inner)
                return inner;
        }
    }
    switch (expr->kind)
    {
    case ND_VAR:
        if (expr->var_is_function)
            return NULL;
        return expr->var_is_const ? expr : NULL;
    case ND_MEMBER:
        if (!expr->lhs)
            return NULL;
        if (expr->is_pointer_deref)
            return find_pointer_to_const_origin(expr->lhs);
        return find_pointer_to_const_origin(expr->lhs);
    case ND_INDEX:
    case ND_DEREF:
        if (!expr->lhs)
            return NULL;
        return find_pointer_to_const_origin(expr->lhs);
    case ND_ADDR:
        if (!expr->lhs)
            return NULL;
        return find_pointer_to_const_origin(expr->lhs);
    default:
        return NULL;
    }
}

static Type *sema_prepare_assignment_lhs(SemaContext *sc, Node *assign_expr, Node **out_lhs_base)
{
    if (!assign_expr || !assign_expr->lhs)
    {
        diag_error_at(assign_expr ? assign_expr->src : NULL, assign_expr ? assign_expr->line : 0, assign_expr ? assign_expr->col : 0,
                      "assignment missing left-hand side");
        exit(1);
    }

    Node *lhs_expr = assign_expr->lhs;
    check_expr(sc, lhs_expr);

    Node *lhs_base = lhs_expr;
    if (lhs_base->kind == ND_CAST && lhs_base->lhs)
        lhs_base = lhs_base->lhs;

    if (lhs_base->kind != ND_VAR && lhs_base->kind != ND_INDEX && lhs_base->kind != ND_MEMBER && lhs_base->kind != ND_DEREF)
    {
        diag_error_at(assign_expr->src, assign_expr->line, assign_expr->col,
                      "lvalue required as left operand of assignment");
        exit(1);
    }

    const Node *const_origin = NULL;
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
    else
    {
        const_origin = find_const_storage_origin(lhs_base);
    }
    if (!const_origin && (lhs_base->kind == ND_INDEX || lhs_base->kind == ND_DEREF))
        const_origin = find_pointer_to_const_origin(lhs_base->lhs);
    if (const_origin)
    {
        const char *const_name = const_origin->var_ref ? const_origin->var_ref : "<unnamed>";
        diag_warning_at(assign_expr->src, assign_expr->line, assign_expr->col,
                        "assignment modifies data derived from constant '%s' (treating operands as writable)",
                        const_name);
    }

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

    if (out_lhs_base)
        *out_lhs_base = lhs_base;
    return lhs_type;
}

static void check_expr(SemaContext *sc, Node *e)
{
    if (!e)
        return;
    if (e->kind == ND_LAMBDA)
    {
        sema_lower_lambda(sc, e);
        if (e->kind != ND_LAMBDA)
        {
            check_expr(sc, e);
            return;
        }
    }
    if (e->kind == ND_LAMBDA_CALL)
    {
        Node saved = *e;
        const SourceBuffer *src = e->src;
        int line = e->line;
        int col = e->col;

        Node *call_node = (Node *)xcalloc(1, sizeof(Node));
        *call_node = saved;
        call_node->kind = ND_CALL;

        memset(e, 0, sizeof(Node));
        e->kind = ND_SEQ;
        e->lhs = call_node;
        e->line = line;
        e->col = col;
        e->src = src;

        check_expr(sc, call_node);

        Node *lambda_value = clone_function_pointer_expr(call_node->lhs);
        if (!lambda_value || !lambda_value->type)
        {
            diag_error_at(src, line, col,
                          "lambda immediate invocation requires an addressable target");
            exit(1);
        }

        e->rhs = lambda_value;
        e->type = lambda_value->type;
        return;
    }
    if (e->kind == ND_INT)
    {
        if (!e->type)
            e->type = e->int_is_unsigned ? &ty_u32 : &ty_i32;
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
    if (e->kind == ND_INIT_LIST)
    {
        Type *target = e->type ? e->type : e->var_type;
        if (!target)
        {
            diag_error_at(e->src, e->line, e->col,
                          "initializer list requires a target type");
            exit(1);
        }
        target = canonicalize_type_deep(target);
        check_initializer_for_type(sc, e, target);
        e->type = target;
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
        if (resolved_sym && resolved_sym->kind == SYM_GLOBAL)
            sema_track_imported_global_usage(sc, resolved_sym);

        const struct VarBind *binding = scope_get_binding(sc, orig_name);
        if (binding && binding->is_static && binding->backend_name)
            e->var_ref = binding->backend_name;

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
    if (e->kind == ND_ALIGNOF)
    {
        Type *ty = e->var_type;
        if (!ty && e->lhs)
        {
            check_expr(sc, e->lhs);
            if (e->lhs->kind == ND_VAR && e->lhs->var_type)
                ty = e->lhs->var_type;
            else if (e->lhs->type)
                ty = e->lhs->type;
        }
        ty = canonicalize_type_deep(ty);
        if (!ty || ty->kind == TY_IMPORT)
        {
            diag_error_at(e->src, e->line, e->col,
                          "alignof operand must resolve to a concrete type");
            exit(1);
        }
        int align = alignof_type(ty);
        if (align <= 0)
            align = 1;
        e->int_val = align;
        e->type = &ty_i32;
        return;
    }
    if (e->kind == ND_OFFSETOF)
    {
        Type *st = canonicalize_type_deep(e->var_type);
        if (!st || st->kind != TY_STRUCT)
        {
            diag_error_at(e->src, e->line, e->col,
                          "offsetof requires a struct type operand");
            exit(1);
        }
        if (!e->field_name || !*e->field_name)
        {
            diag_error_at(e->src, e->line, e->col,
                          "offsetof requires a field designator");
            exit(1);
        }
        if (st->kind == TY_STRUCT && (!st->strct.field_types || st->strct.field_count <= 0))
        {
            diag_error_at(e->src, e->line, e->col,
                          "struct '%s' is incomplete",
                          st->struct_name ? st->struct_name : "<anonymous>");
            exit(1);
        }
        int idx = struct_find_field(st, e->field_name);
        if (idx < 0)
        {
            diag_error_at(e->src, e->line, e->col,
                          "unknown field '%s' on struct '%s'",
                          e->field_name,
                          st->struct_name ? st->struct_name : "<anonymous>");
            exit(1);
        }
        if (!st->strct.field_offsets)
        {
            diag_error_at(e->src, e->line, e->col,
                          "struct '%s' is missing offset metadata",
                          st->struct_name ? st->struct_name : "<anonymous>");
            exit(1);
        }
        e->int_val = st->strct.field_offsets[idx];
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
                    sema_track_imported_global_usage(sc, sym);
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
    if (e->kind == ND_MUL || e->kind == ND_DIV || e->kind == ND_MOD)
    {
        check_expr(sc, e->lhs);
        check_expr(sc, e->rhs);
        Type *lhs_type = canonicalize_type_deep(e->lhs->type);
        Type *rhs_type = canonicalize_type_deep(e->rhs->type);
        e->lhs->type = lhs_type;
        e->rhs->type = rhs_type;
        if (!type_equal(lhs_type, rhs_type))
        {
            const char *op = (e->kind == ND_MUL) ? "*" : (e->kind == ND_DIV ? "/" : "%");
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
        if (e->kind == ND_MOD)
        {
            if (!lhs_is_int)
            {
                diag_error_at(e->src, e->line, e->col,
                              "integer type required for '%%'");
                exit(1);
            }
        }
        else if (!(lhs_is_int || lhs_is_float))
        {
            const char *op = e->kind == ND_MUL ? "*" : "/";
            diag_error_at(e->src, e->line, e->col,
                          "numeric type required for '%s'",
                          op);
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
    if (e->kind == ND_LNOT)
    {
        if (!e->lhs)
        {
            diag_error_at(e->src, e->line, e->col,
                          "logical '!' requires an operand");
            exit(1);
        }
        check_expr(sc, e->lhs);
        if (!type_is_int(e->lhs->type))
        {
            diag_error_at(e->src, e->line, e->col,
                          "logical '!' requires integer operand");
            exit(1);
        }
        e->type = type_bool();
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
        else
        {
            // Default: use pointee type
            e->type = elem_type ? elem_type : &ty_i32;
        }
        return;
    }
    if (e->kind == ND_ASSIGN)
    {
        Type *lhs_type = sema_prepare_assignment_lhs(sc, e, NULL);
        check_expr(sc, e->rhs);
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
    if (e->kind == ND_ADD_ASSIGN || e->kind == ND_SUB_ASSIGN || e->kind == ND_MUL_ASSIGN ||
        e->kind == ND_DIV_ASSIGN || e->kind == ND_MOD_ASSIGN || e->kind == ND_BITAND_ASSIGN ||
        e->kind == ND_BITOR_ASSIGN || e->kind == ND_BITXOR_ASSIGN || e->kind == ND_SHL_ASSIGN ||
        e->kind == ND_SHR_ASSIGN)
    {
        Type *lhs_type = sema_prepare_assignment_lhs(sc, e, NULL);
        check_expr(sc, e->rhs);
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

        Type *canon_lhs = canonicalize_type_deep(lhs_type);
        int lhs_is_int = canon_lhs ? type_is_int(canon_lhs) : 0;
        int lhs_is_float = canon_lhs ? type_is_float(canon_lhs) : 0;

        const char *op_text = "+=";
        int allow_int = 1;
        int allow_float = 0;
        switch (e->kind)
        {
        case ND_ADD_ASSIGN:
            op_text = "+=";
            allow_float = 1;
            break;
        case ND_SUB_ASSIGN:
            op_text = "-=";
            allow_float = 1;
            break;
        case ND_MUL_ASSIGN:
            op_text = "*=";
            allow_float = 1;
            break;
        case ND_DIV_ASSIGN:
            op_text = "/=";
            allow_float = 1;
            break;
        case ND_MOD_ASSIGN:
            op_text = "%=";
            allow_float = 0;
            break;
        case ND_BITAND_ASSIGN:
            op_text = "&=";
            allow_float = 0;
            break;
        case ND_BITOR_ASSIGN:
            op_text = "|=";
            allow_float = 0;
            break;
        case ND_BITXOR_ASSIGN:
            op_text = "^=";
            allow_float = 0;
            break;
        case ND_SHL_ASSIGN:
            op_text = "<<=";
            allow_float = 0;
            break;
        case ND_SHR_ASSIGN:
            op_text = ">>=";
            allow_float = 0;
            break;
        default:
            break;
        }

        const char *type_desc = (allow_int && allow_float) ? "integer or floating-point"
                                                           : (allow_float ? "floating-point"
                                                                          : "integer");
        int lhs_acceptable = (allow_int && lhs_is_int) || (allow_float && lhs_is_float);
        if (!lhs_acceptable)
        {
            diag_error_at(e->src, e->line, e->col,
                          "%s requires %s operands", op_text, type_desc);
            exit(1);
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
                if (!symbol_is_template_function(direct_sym))
                    func_sig = make_function_type_from_sig(&resolved_import->symbol.sig);
                call_is_indirect = 0;
            }
        }

        if (!func_sig && direct_sym && !symbol_is_template_function(direct_sym))
        {
            sema_ensure_call_args_checked(sc, e, &args_checked);
            if (!call_arguments_match_signature(e, &direct_sym->sig))
                direct_sym = NULL;
        }

        if (!func_sig && !direct_sym && resolved_name)
        {
            const Symbol *overload = sema_resolve_local_overload(sc, resolved_name, e, &args_checked);
            if (overload)
                direct_sym = overload;
        }

        if (!func_sig && direct_sym && direct_sym->ast_node &&
            direct_sym->ast_node->kind == ND_FUNC && direct_sym->ast_node->generic_param_count > 0)
        {
            const Symbol *inst_sym = sema_instantiate_generic_call(sc, direct_sym, e, &args_checked);
            if (inst_sym)
                direct_sym = inst_sym;
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

        const char *call_display_name =
            resolved_name ? resolved_name : (original_name ? original_name : "<call>");

        if (!call_is_indirect && direct_sym && direct_sym->kind == SYM_FUNC)
        {
            e->call_target = direct_sym->ast_node;
            if (compiler_verbose_enabled())
            {
                const char *target_name = direct_sym->name ? direct_sym->name : call_display_name;
                const char *inline_status =
                    (e->call_target && e->call_target->inline_candidate)
                        ? "eligible for inlining"
                        : "requires emitted body";
                compiler_verbose_logf("sema", "resolved call '%s' to '%s' (%s)",
                                      call_display_name, target_name, inline_status);
            }
        }
        else if (!call_is_indirect && target && target->referenced_function)
        {
            e->call_target = target->referenced_function;
            if (compiler_verbose_enabled())
            {
                const char *target_name =
                    target->referenced_function->name ? target->referenced_function->name : call_display_name;
                compiler_verbose_logf("sema", "resolved call '%s' via referenced function '%s'",
                                      call_display_name, target_name);
            }
        }
        else
        {
            e->call_target = NULL;
            if (!call_is_indirect && compiler_verbose_enabled())
                compiler_verbose_logf("sema", "call '%s' has no inline metadata (treating as external)",
                                      call_display_name);
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

            int param_is_const = 0;
            if (e->call_target && e->call_target->param_const_flags && i < e->call_target->param_count)
                param_is_const = e->call_target->param_const_flags[i];
            Type *canon_expected = canonicalize_type_deep(expected_ty);
            if (param_is_const)
                continue;

            const Node *const_origin = NULL;
            if (canon_expected && canon_expected->kind == TY_PTR)
                const_origin = find_pointer_to_const_origin(e->args[i]);
            else
                const_origin = find_const_storage_origin(e->args[i]);

            if (!const_origin)
                continue;

            const char *const_name = const_origin->var_ref ? const_origin->var_ref : "<unnamed>";
            if (canon_expected && canon_expected->kind == TY_PTR)
            {
                if (!expr_has_pointer_override(e->args[i]))
                {
                    diag_error_at(e->args[i]->src, e->args[i]->line, e->args[i]->col,
                                  "argument %d to '%s' passes pointer derived from constant '%s'; cast to a mutable pointer to override",
                                  i + 1, diag_name, const_name);
                    exit(1);
                }
            }
            else
            {
                if (!expr_has_any_cast(e->args[i]))
                {
                    diag_warning_at(e->args[i]->src, e->args[i]->line, e->args[i]->col,
                                    "argument %d to '%s' derives from constant '%s'",
                                    i + 1, diag_name, const_name);
                }
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
    if (e->kind == ND_MATCH)
    {
        if (!e->match_stmt.expr)
        {
            diag_error_at(e->src, e->line, e->col,
                          "match expression missing scrutinee");
            exit(1);
        }
        if (e->match_stmt.arm_count <= 0 || !e->match_stmt.arms)
        {
            diag_error_at(e->src, e->line, e->col,
                          "match expression requires at least one arm");
            exit(1);
        }

        check_expr(sc, e->match_stmt.expr);
        Type *scrut_type = canonicalize_type_deep(e->match_stmt.expr->type);
        if (!type_is_int(scrut_type))
        {
            diag_error_at(e->match_stmt.expr->src, e->match_stmt.expr->line, e->match_stmt.expr->col,
                          "match expression scrutinee must be integral");
            exit(1);
        }

        int arm_count = e->match_stmt.arm_count;
        MatchArm *arms = e->match_stmt.arms;
        int wildcard_index = -1;
        int value_count = 0;
        int64_t *pattern_values = (int64_t *)xcalloc((size_t)arm_count, sizeof(int64_t));
        Type *result_type = NULL;

        for (int i = 0; i < arm_count; ++i)
        {
            MatchArm *arm = &arms[i];
            if (!arm)
            {
                diag_error_at(e->src, e->line, e->col,
                              "match arm is null");
                if (pattern_values)
                    free(pattern_values);
                exit(1);
            }

            if (arm->pattern)
            {
                check_expr(sc, arm->pattern);
                if (!type_is_int(arm->pattern->type))
                {
                    if (!coerce_int_literal_to_type(arm->pattern, scrut_type, "match arm"))
                    {
                        diag_error_at(arm->pattern->src, arm->pattern->line, arm->pattern->col,
                                      "match patterns must be integer literals compatible with scrutinee");
                        if (pattern_values)
                            free(pattern_values);
                        exit(1);
                    }
                }
                if (!node_force_int_literal(arm->pattern))
                {
                    diag_error_at(arm->pattern->src, arm->pattern->line, arm->pattern->col,
                                  "match patterns must be integer constants");
                    if (pattern_values)
                        free(pattern_values);
                    exit(1);
                }
                if (arm->pattern->kind != ND_INT)
                {
                    diag_error_at(arm->pattern->src, arm->pattern->line, arm->pattern->col,
                                  "match patterns must be integer constants");
                    if (pattern_values)
                        free(pattern_values);
                    exit(1);
                }
                int64_t val = arm->pattern->int_val;
                for (int j = 0; j < value_count; ++j)
                {
                    if (pattern_values[j] == val)
                    {
                        diag_error_at(arm->pattern->src, arm->pattern->line, arm->pattern->col,
                                      "duplicate match pattern value '%lld'", (long long)val);
                        free(pattern_values);
                        exit(1);
                    }
                }
                pattern_values[value_count++] = val;
            }
            else
            {
                if (wildcard_index >= 0)
                {
                    diag_error_at(e->src, e->line, e->col,
                                  "match expression may contain only one '_' arm");
                    if (pattern_values)
                        free(pattern_values);
                    exit(1);
                }
                wildcard_index = i;
                if (i != arm_count - 1)
                {
                    diag_error_at(e->src, e->line, e->col,
                                  "wildcard '_' arm must be the last arm in a match expression");
                    if (pattern_values)
                        free(pattern_values);
                    exit(1);
                }
            }

            if (arm->guard)
            {
                diag_error_at(arm->guard->src, arm->guard->line, arm->guard->col,
                              "match guards are not supported yet");
                if (pattern_values)
                    free(pattern_values);
                exit(1);
            }

            if (!arm->body)
            {
                diag_error_at(e->src, e->line, e->col,
                              "match arm missing result expression");
                if (pattern_values)
                    free(pattern_values);
                exit(1);
            }

            check_expr(sc, arm->body);
            Type *body_type = canonicalize_type_deep(arm->body->type);
            if (!result_type)
            {
                result_type = body_type;
            }
            else
            {
                if (!type_equal(result_type, body_type))
                {
                    if (coerce_int_literal_to_type(arm->body, result_type, "match arm result"))
                        body_type = canonicalize_type_deep(arm->body->type);
                }
                if (!type_equal(result_type, body_type))
                {
                    diag_error_at(arm->body->src, arm->body->line, arm->body->col,
                                  "all match arms must yield the same type");
                    if (pattern_values)
                        free(pattern_values);
                    exit(1);
                }
            }
        }

        if (wildcard_index < 0)
        {
            diag_error_at(e->src, e->line, e->col,
                          "match expression must include a trailing '_' arm");
            if (pattern_values)
                free(pattern_values);
            exit(1);
        }

        if (!result_type)
            result_type = &ty_i32;
        e->type = result_type;
        if (pattern_values)
            free(pattern_values);
        return;
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
        int rhs_checked = 0;
        if (stmt->var_is_inferred)
        {
            if (!stmt->rhs)
            {
                diag_error_at(stmt->src, stmt->line, stmt->col,
                              "'var' declarations require an initializer to infer the type");
                return 1;
            }
            if (stmt->rhs->kind == ND_INIT_LIST)
            {
                diag_error_at(stmt->rhs->src, stmt->rhs->line, stmt->rhs->col,
                              "cannot use brace initializer with 'var' declaration; specify the type explicitly");
                return 1;
            }
            check_expr(sc, stmt->rhs);
            rhs_checked = 1;
            if (!stmt->rhs->type)
            {
                diag_error_at(stmt->rhs->src, stmt->rhs->line, stmt->rhs->col,
                              "unable to infer type for '%s'",
                              stmt->var_name ? stmt->var_name : "<unnamed>");
                return 1;
            }
            stmt->var_type = canonicalize_type_deep(stmt->rhs->type);
            stmt->rhs->type = stmt->var_type;
        }
        stmt->var_type = canonicalize_type_deep(stmt->var_type);
        // Support multi-element brace initializers on pointer targets by
        // treating the storage as a fixed-size array of the pointee type.
        if (stmt->var_type && stmt->var_type->kind == TY_PTR && stmt->rhs && stmt->rhs->kind == ND_INIT_LIST && !stmt->rhs->init.is_zero)
        {
            int elem_count = stmt->rhs->init.count;
            if (elem_count > 0)
            {
                Type *elem_ty = stmt->var_type->pointee ? canonicalize_type_deep(stmt->var_type->pointee) : &ty_i32;
                stmt->var_type = canonicalize_type_deep(type_array(elem_ty, elem_count));
            }
        }
        if (stmt->var_type && stmt->var_type->kind == TY_ARRAY && stmt->var_type->array.is_unsized && stmt->rhs && stmt->rhs->kind == ND_INIT_LIST)
        {
            int elem_count = stmt->rhs->init.count;
            if (elem_count <= 0)
            {
                diag_error_at(stmt->rhs->src, stmt->rhs->line, stmt->rhs->col,
                              "unsized arrays require at least one initializer element to determine their length");
                return 1;
            }
            Type *elem_ty = stmt->var_type->array.elem ? stmt->var_type->array.elem : type_i32();
            stmt->var_type = canonicalize_type_deep(type_array(elem_ty, elem_count));
        }
        if (stmt->var_type && stmt->var_type->kind == TY_ARRAY)
            stmt->var_is_array = stmt->var_type->array.is_unsized ? 0 : 1;
        else
            stmt->var_is_array = 0;
        stmt->var_is_function = type_is_function_pointer(stmt->var_type);
        if (stmt->var_is_static)
        {
            if (!sc || !sc->unit || sc->unit->kind != ND_UNIT)
            {
                diag_error_at(stmt->src, stmt->line, stmt->col,
                              "static local variables require a translation unit context");
                return 1;
            }
            char *backend = make_static_local_backend_name(fn, stmt->var_name);
            Node *hoisted = (Node *)xcalloc(1, sizeof(Node));
            hoisted->kind = ND_VAR_DECL;
            hoisted->var_name = backend;
            hoisted->var_type = stmt->var_type;
            hoisted->var_is_const = stmt->var_is_const;
            hoisted->var_is_static = 1;
            hoisted->var_is_global = 1;
            hoisted->var_is_array = stmt->var_is_array;
            hoisted->var_is_function = stmt->var_is_function;
            hoisted->is_exposed = 0;
            hoisted->export_name = 0;
            hoisted->src = stmt->src;
            hoisted->line = stmt->line;
            hoisted->col = stmt->col;
            hoisted->metadata.backend_name = backend;
            hoisted->rhs = stmt->rhs;

            if (sema_check_global_decl(sc, hoisted))
                return 1;

            sema_register_global_local(sc, sc->unit, hoisted);
            unit_append_decl(sc->unit, hoisted);
            scope_add(sc, stmt->var_name, stmt->var_type, stmt->var_is_const, 1, backend);

            stmt->rhs = NULL;
            stmt->var_is_global = 1;
            return 0;
        }
        scope_add(sc, stmt->var_name, stmt->var_type, stmt->var_is_const, 0, NULL);
        if (stmt->rhs)
        {
            if (stmt->rhs->kind == ND_INIT_LIST)
            {
                check_initializer_for_type(sc, stmt->rhs, stmt->var_type);
            }
            else
            {
                if (!rhs_checked)
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
        sc->loop_depth++;
        if (stmt->rhs && sema_check_statement(sc, stmt->rhs, fn, found_ret))
        {
            sc->loop_depth--;
            return 1;
        }
        if (stmt->body && sema_check_statement(sc, stmt->body, fn, found_ret))
        {
            sc->loop_depth--;
            return 1;
        }
        sc->loop_depth--;
        return 0;
    case ND_SWITCH:
    {
        if (!stmt->switch_stmt.expr)
        {
            diag_error_at(stmt->src, stmt->line, stmt->col,
                          "switch statement missing selector expression");
            return 1;
        }
        check_expr(sc, stmt->switch_stmt.expr);
        Type *cond_type = canonicalize_type_deep(stmt->switch_stmt.expr->type);
        if (!type_is_int(cond_type))
        {
            diag_error_at(stmt->switch_stmt.expr->src, stmt->switch_stmt.expr->line, stmt->switch_stmt.expr->col,
                          "switch selector must be an integral type");
            return 1;
        }

        int case_count = stmt->switch_stmt.case_count;
        SwitchCase *cases = stmt->switch_stmt.cases;
        int has_default = 0;
        int value_used = 0;
        int64_t *case_values = (case_count > 0) ? (int64_t *)xcalloc((size_t)case_count, sizeof(int64_t)) : NULL;

        for (int i = 0; i < case_count; ++i)
        {
            SwitchCase *entry = cases ? &cases[i] : NULL;
            if (!entry)
                continue;
            if (entry->is_default)
            {
                if (has_default)
                {
                    diag_error_at(stmt->src, stmt->line, stmt->col,
                                  "switch statement cannot contain multiple 'default' labels");
                    if (case_values)
                        free(case_values);
                    return 1;
                }
                has_default = 1;
                continue;
            }

            if (!entry->value)
            {
                diag_error_at(stmt->src, stmt->line, stmt->col,
                              "switch case label missing expression");
                if (case_values)
                    free(case_values);
                return 1;
            }

            check_expr(sc, entry->value);
            if (!type_is_int(entry->value->type))
            {
                if (!coerce_int_literal_to_type(entry->value, cond_type, "switch case"))
                {
                    diag_error_at(entry->value->src, entry->value->line, entry->value->col,
                                  "switch case labels must be integer constants");
                    if (case_values)
                        free(case_values);
                    return 1;
                }
            }
            if (!node_force_int_literal(entry->value))
            {
                diag_error_at(entry->value->src, entry->value->line, entry->value->col,
                              "switch case labels must be integral literals");
                if (case_values)
                    free(case_values);
                return 1;
            }
            if (entry->value->kind != ND_INT)
            {
                diag_error_at(entry->value->src, entry->value->line, entry->value->col,
                              "switch case labels must be integral literals");
                if (case_values)
                    free(case_values);
                return 1;
            }

            int64_t val = entry->value->int_val;
            for (int j = 0; j < value_used; ++j)
            {
                if (case_values && case_values[j] == val)
                {
                    diag_error_at(entry->value->src, entry->value->line, entry->value->col,
                                  "duplicate switch case label '%lld'", (long long)val);
                    free(case_values);
                    return 1;
                }
            }
            if (case_values)
                case_values[value_used++] = val;
        }

        sc->switch_depth++;
        int rc = 0;
        for (int i = 0; i < case_count; ++i)
        {
            SwitchCase *entry = cases ? &cases[i] : NULL;
            if (!entry || !entry->body)
                continue;
            if (sema_check_block(sc, entry->body, fn, found_ret, 1))
            {
                rc = 1;
                break;
            }
        }
        sc->switch_depth--;
        if (case_values)
            free(case_values);
        return rc;
    }
    case ND_BREAK:
        if (sc->loop_depth <= 0 && sc->switch_depth <= 0)
        {
            diag_error_at(stmt->src, stmt->line, stmt->col,
                          "'break' may only appear inside a loop or switch");
            return 1;
        }
        return 0;
    case ND_CONTINUE:
        if (sc->loop_depth <= 0)
        {
            diag_error_at(stmt->src, stmt->line, stmt->col,
                          "'continue' may only appear inside a loop");
            return 1;
        }
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
    case ND_STRING:
        return 1;
    case ND_NEG:
    case ND_CAST:
        return sema_global_initializer_is_const(expr->lhs);
    case ND_ADDR:
        if (!expr->lhs)
            return 0;
        if (expr->lhs->kind == ND_VAR && (expr->lhs->var_is_global || expr->lhs->var_is_function))
            return 1;
        return 0;
    case ND_INIT_LIST:
        if (expr->init.is_zero)
            return 1;
        if (!expr->init.elems || expr->init.count <= 0)
            return 0;
        for (int i = 0; i < expr->init.count; ++i)
        {
            const Node *elem = expr->init.elems ? expr->init.elems[i] : NULL;
            if (!elem || !sema_global_initializer_is_const(elem))
                return 0;
        }
        return 1;
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
    // Allow brace initializers with multiple elements to target pointer types by
    // materializing a fixed-size array of the pointee type. This supports
    // patterns like `char** tok = {null, null};`.
    if (ty->kind == TY_PTR && decl->rhs && decl->rhs->kind == ND_INIT_LIST && !decl->rhs->init.is_zero)
    {
        int elem_count = decl->rhs->init.count;
        if (elem_count > 0)
        {
            Type *elem_ty = ty->pointee ? canonicalize_type_deep(ty->pointee) : &ty_i32;
            decl->var_type = canonicalize_type_deep(type_array(elem_ty, elem_count));
            ty = decl->var_type;
        }
    }
    if (ty->kind == TY_ARRAY && ty->array.is_unsized && decl->rhs && decl->rhs->kind == ND_INIT_LIST)
    {
        int elem_count = decl->rhs->init.count;
        if (elem_count <= 0)
        {
            diag_error_at(decl->rhs->src, decl->rhs->line, decl->rhs->col,
                          "unsized arrays require at least one initializer element to determine their length");
            return 1;
        }
        Type *elem_ty = ty->array.elem ? ty->array.elem : type_i32();
        decl->var_type = canonicalize_type_deep(type_array(elem_ty, elem_count));
        ty = decl->var_type;
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

        if (decl->rhs->kind != ND_INIT_LIST)
        {
            diag_error_at(decl->rhs->src, decl->rhs->line, decl->rhs->col,
                          "struct global '%s' must use an initializer list",
                          decl->var_name);
            return 1;
        }

        check_initializer_for_type(sc, decl->rhs, ty);
        if (!sema_global_initializer_is_const(decl->rhs))
        {
            diag_error_at(decl->rhs->src, decl->rhs->line, decl->rhs->col,
                          "global initializer for '%s' must be a constant expression",
                          decl->var_name);
            return 1;
        }
        return 0;
    }

    if (!decl->rhs)
        return 0;

    if (ty->kind == TY_ARRAY && decl->rhs->kind == ND_INIT_LIST)
    {
        check_initializer_for_type(sc, decl->rhs, ty);
        if (!sema_global_initializer_is_const(decl->rhs))
        {
            diag_error_at(decl->rhs->src, decl->rhs->line, decl->rhs->col,
                          "global initializer for '%s' must be a constant expression",
                          decl->var_name);
            return 1;
        }
        return 0;
    }

    if (decl->rhs->kind == ND_INIT_LIST)
    {
        check_initializer_for_type(sc, decl->rhs, ty);
        if (!sema_global_initializer_is_const(decl->rhs))
        {
            diag_error_at(decl->rhs->src, decl->rhs->line, decl->rhs->col,
                          "global initializer for '%s' must be a constant expression",
                          decl->var_name);
            return 1;
        }
        if (!decl->rhs->type)
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
    if (fn->is_chancecode || fn->is_literal)
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
        int param_is_const = (fn->param_const_flags && i < fn->param_count) ? fn->param_const_flags[i] : 0;
        scope_add(sc, fn->param_names[i], fn->param_types[i], param_is_const, 0, NULL);
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
    Node *previous_unit = sc->unit;
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
    case ND_LNOT:
    {
        int64_t lhs = 0;
        if (!inline_eval_const_expr(expr->lhs, bindings, binding_count, depth + 1, &lhs))
            return 0;
        *out_value = inline_normalize_value(!lhs, expr->type);
        return 1;
    }
    case ND_ADD:
    case ND_SUB:
    case ND_MUL:
    case ND_DIV:
    case ND_MOD:
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
        case ND_MOD:
            if (rhs == 0)
                return 0;
            result = lhs % rhs;
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
        const char *fn_name = fn->name ? fn->name : "<anon>";

        if (!fn->wants_inline)
        {
            compiler_verbose_logf("inline", "skip %s: function not marked inline", fn_name);
            continue;
        }
        if (fn->is_chancecode || fn->is_literal)
        {
            compiler_verbose_logf("inline", "skip %s: ChanceCode/Literal shim cannot inline", fn_name);
            continue;
        }
        if (fn->inline_address_taken)
        {
            compiler_verbose_logf("inline", "skip %s: address is taken", fn_name);
            continue;
        }
        if (fn->inline_recursive)
        {
            compiler_verbose_logf("inline", "skip %s: recursion detected", fn_name);
            continue;
        }
        if (fn->is_varargs)
        {
            compiler_verbose_logf("inline", "skip %s: varargs not supported", fn_name);
            continue;
        }
        if (fn->param_count > INLINE_PARAM_LIMIT)
        {
            compiler_verbose_logf("inline", "skip %s: %d parameters exceeds inline limit %d", fn_name,
                                  fn->param_count, INLINE_PARAM_LIMIT);
            continue;
        }
        int params_ok = 1;
        for (int p = 0; p < fn->param_count; ++p)
        {
            Type *pty = (fn->param_types && p < fn->param_count) ? fn->param_types[p] : NULL;
            if (!inline_type_supported(pty))
            {
                params_ok = 0;
                compiler_verbose_logf("inline", "skip %s: parameter %d has unsupported type", fn_name, p);
                break;
            }
        }
        if (!params_ok)
            continue;
        if (fn->ret_type && fn->ret_type->kind == TY_VOID)
        {
            compiler_verbose_logf("inline", "skip %s: returns void", fn_name);
            continue;
        }
        if (!inline_type_supported(fn->ret_type))
        {
            compiler_verbose_logf("inline", "skip %s: return type unsupported for inlining", fn_name);
            continue;
        }
        const Node *ret_expr = inline_extract_return_expr(fn);
        if (!ret_expr)
        {
            compiler_verbose_logf("inline", "skip %s: body is not a single return expression", fn_name);
            continue;
        }
        int cost = inline_expr_cost(ret_expr);
        if (cost > INLINE_COST_LIMIT)
        {
            compiler_verbose_logf("inline", "skip %s: inline cost %d exceeds limit %d", fn_name, cost,
                                  INLINE_COST_LIMIT);
            continue;
        }
        fn->inline_candidate = 1;
        fn->inline_cost = cost;
        fn->inline_expr = ret_expr;
        if (!fn->is_exposed && !fn->is_entrypoint)
            fn->inline_needs_body = 0;
        compiler_verbose_logf("inline", "select %s as inline candidate (cost=%d)", fn_name, cost);
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
