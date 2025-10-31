#include "ast.h"
#include "module_registry.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
static Type *canonicalize_type_deep(Type *ty)
{
    ty = module_registry_canonical_type(ty);
    if (ty && ty->kind == TY_PTR && ty->pointee)
    {
        Type *resolved = canonicalize_type_deep(ty->pointee);
        if (resolved && resolved != ty->pointee)
            ty->pointee = resolved;
    }
    return ty;
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
};
SemaContext *sema_create(void)
{
    SemaContext *sc = (SemaContext *)xcalloc(1, sizeof(SemaContext));
    sc->syms = symtab_create();
    sc->unit = NULL;
    return sc;
}
void sema_destroy(SemaContext *sc)
{
    if (!sc)
        return;
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

static Type *metadata_token_to_type(const char *token)
{
    if (!token)
        return NULL;
    if (strcmp(token, "i1") == 0)
        return &ty_i8;
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

static int type_is_int(Type *t)
{
    return t && (t->kind == TY_I8 || t->kind == TY_U8 || t->kind == TY_I16 ||
                 t->kind == TY_U16 || t->kind == TY_I32 || t->kind == TY_U32 ||
                 t->kind == TY_I64 || t->kind == TY_U64 || t->kind == TY_CHAR);
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
        return 1;
    default:
        return 0;
    }
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
    if (a->kind == TY_STRUCT)
    {
        if (a->struct_name && b->struct_name)
            return strcmp(a->struct_name, b->struct_name) == 0;
        return a == b;
    }
    return 1;
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
    s->sig.is_varargs = 0;

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

static void sema_register_function_foreign(SemaContext *sc, const Node *unit_node, Node *fn)
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
    if (!t)
        return 0;
    switch (t->kind)
    {
    case TY_I8:
    case TY_I16:
    case TY_I32:
    case TY_I64:
        return 1;
    default:
        return 0;
    }
}

static int type_is_unsigned_int(Type *t)
{
    if (!t)
        return 0;
    switch (t->kind)
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

static int type_is_pointer(Type *t)
{
    t = canonicalize_type_deep(t);
    return t && t->kind == TY_PTR;
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
    case TY_U16:
        return UINT16_MAX;
    case TY_U32:
        return UINT32_MAX;
    case TY_U64:
        return INT64_MAX;
    default:
        return 0;
    }
}

static int literal_fits_type(Type *target, const Node *rhs)
{
    if (!target || !rhs)
        return 0;
    if (rhs->kind != ND_INT)
        return 0;
    if (!type_is_int(target))
        return 0;
    int64_t v = rhs->int_val;
    if (type_is_unsigned_int(target))
    {
        if (v < 0)
            return 0;
        return (uint64_t)v <= (uint64_t)type_int_max(target);
    }
    if (type_is_signed_int(target))
        return v >= type_int_min(target) && v <= type_int_max(target);
    return 0;
}

static int can_assign(Type *target, Node *rhs)
{
    if (!target || !rhs)
        return 0;
    if (rhs->type && type_equal(target, rhs->type))
        return 1;
    if (literal_fits_type(target, rhs))
    {
        rhs->type = target;
        return 1;
    }
    return 0;
}

static void check_initializer_for_type(SemaContext *sc, Node *init, Type *target)
{
    if (!init || !target)
        return;
    target = canonicalize_type_deep(target);
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
    for (struct Scope *s = sc->scope; s; s = s->parent)
    {
        for (int i = 0; i < s->local_count; i++)
        {
            if (strcmp(s->locals[i].name, name) == 0)
                return 1;
        }
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
    if (e->kind == ND_VAR)
    {
        Type *t = scope_get_type(sc, e->var_ref);
        if (!t)
        {
            diag_error_at(e->src, e->line, e->col, "unknown variable '%s'",
                          e->var_ref);
            exit(1);
        }
        e->type = canonicalize_type_deep(t);
        return;
    }
    if (e->kind == ND_SIZEOF)
    {
        // size of a type known at parse-time; stored in e->var_type
        Type *ty = e->var_type ? canonicalize_type_deep(e->var_type) : &ty_i32;
        if (ty && ty->kind == TY_IMPORT)
            ty = canonicalize_type_deep(ty);
        int sz = 0;
        switch (ty->kind)
        {
        case TY_I8: case TY_U8: case TY_CHAR: sz = 1; break;
        case TY_I16: case TY_U16: sz = 2; break;
        case TY_I32: case TY_U32: case TY_F32: sz = 4; break;
        case TY_I64: case TY_U64: case TY_F64: case TY_PTR: sz = 8; break;
        case TY_F128: sz = 16; break;
        case TY_STRUCT: sz = ty->strct.size_bytes; break;
        case TY_VOID: sz = 0; break;
        default: sz = 8; break;
        }
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
            snprintf(buf, sizeof(buf), "<unknown>::?" );
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
            case TY_I8: snprintf(buf, sizeof(buf), "<built-in>::i8"); break;
            case TY_U8: snprintf(buf, sizeof(buf), "<built-in>::u8"); break;
            case TY_I16: snprintf(buf, sizeof(buf), "<built-in>::i16"); break;
            case TY_U16: snprintf(buf, sizeof(buf), "<built-in>::u16"); break;
            case TY_I32: snprintf(buf, sizeof(buf), "<built-in>::i32"); break;
            case TY_U32: snprintf(buf, sizeof(buf), "<built-in>::u32"); break;
            case TY_I64: snprintf(buf, sizeof(buf), "<built-in>::i64"); break;
            case TY_U64: snprintf(buf, sizeof(buf), "<built-in>::u64"); break;
            case TY_F32: snprintf(buf, sizeof(buf), "<built-in>::f32"); break;
            case TY_F64: snprintf(buf, sizeof(buf), "<built-in>::f64"); break;
            case TY_F128: snprintf(buf, sizeof(buf), "<built-in>::f128"); break;
            case TY_VOID: snprintf(buf, sizeof(buf), "<built-in>::void"); break;
            case TY_CHAR: snprintf(buf, sizeof(buf), "<built-in>::char"); break;
            case TY_PTR: snprintf(buf, sizeof(buf), "<built-in>::ptr"); break;
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
        s->src = e->src; s->line = e->line; s->col = e->col;
        s->str_len = (int)strlen(buf);
        char *heap = (char*)xmalloc((size_t)s->str_len + 1);
        memcpy(heap, buf, (size_t)s->str_len + 1);
        s->str_data = heap;
        // Replace e with string node semantics: set type to char*
        e->kind = ND_STRING;
        e->str_data = s->str_data;
        e->str_len = s->str_len;
        static Type char_ptr = { .kind = TY_PTR, .pointee = &ty_char};
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
        if (target->kind != ND_VAR && target->kind != ND_MEMBER && target->kind != ND_INDEX)
        {
            diag_error_at(target->src, target->line, target->col,
                          "operand of '&' must be an lvalue");
            exit(1);
        }
        check_expr(sc, target);
        if (!target->type)
        {
            diag_error_at(target->src, target->line, target->col,
                          "cannot determine operand type for '&'");
            exit(1);
        }
        e->type = type_ptr(target->type);
        return;
    }
    if (e->kind == ND_ADD)
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
            diag_error_at(e->src, e->line, e->col,
                          "'+' requires both operands to have the same type");
            exit(1);
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
            diag_error_at(e->src, e->line, e->col,
                          "'-' requires both operands to have the same type");
            exit(1);
        }
        e->type = lhs_type;
        return;
    }
    if (e->kind == ND_MUL || e->kind == ND_DIV)
    {
        check_expr(sc, e->lhs);
        check_expr(sc, e->rhs);
        if (!type_equal(e->lhs->type, e->rhs->type))
        {
            diag_error_at(e->src, e->line, e->col,
                          "'%s' requires both operands to have the same type",
                          e->kind == ND_MUL ? "*" : "/");
            exit(1);
        }
        if (!type_is_int(e->lhs->type))
        {
            diag_error_at(e->src, e->line, e->col,
                          "integer type required for '%s'",
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
    if (e->kind == ND_GT_EXPR || e->kind == ND_LT || e->kind == ND_LE || e->kind == ND_GE)
    {
        check_expr(sc, e->lhs);
        check_expr(sc, e->rhs);
        // Allow integer-vs-integer or pointer-vs-pointer relational comparisons.
        int lhs_is_int = type_is_int(e->lhs->type);
        int rhs_is_int = type_is_int(e->rhs->type);
        int lhs_is_ptr = (e->lhs->type && e->lhs->type->kind == TY_PTR);
        int rhs_is_ptr = (e->rhs->type && e->rhs->type->kind == TY_PTR);
        if (!((lhs_is_int && rhs_is_int) || (lhs_is_ptr && rhs_is_ptr)))
        {
            diag_error_at(e->src, e->line, e->col, "relational operator requires integer or pointer operands of the same category");
            exit(1);
        }
        e->type = &ty_i32;
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
        e->type = &ty_i32;
        return;
    }
    if (e->kind == ND_EQ || e->kind == ND_NE)
    {
        check_expr(sc, e->lhs);
        check_expr(sc, e->rhs);
        // Allow integer==integer or pointer==pointer comparisons.
        int lhs_is_int = type_is_int(e->lhs->type);
        int rhs_is_int = type_is_int(e->rhs->type);
        int lhs_is_ptr = (e->lhs->type && e->lhs->type->kind == TY_PTR);
        int rhs_is_ptr = (e->rhs->type && e->rhs->type->kind == TY_PTR);
        if (!((lhs_is_int && rhs_is_int) || (lhs_is_ptr && rhs_is_ptr)))
        {
            diag_error_at(e->src, e->line, e->col,
                          "equality requires both operands to be integers or pointers");
            exit(1);
        }
        e->type = &ty_i32;
        return;
    }
    if (e->kind == ND_CAST)
    {
        check_expr(sc, e->lhs); /* trust parser's target type on node */
        if (!e->type)
            e->type = e->lhs->type;
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
        if (lhs_base->kind != ND_VAR && lhs_base->kind != ND_INDEX && lhs_base->kind != ND_MEMBER)
        {
            diag_error_at(e->src, e->line, e->col,
                          "lvalue required as left operand of assignment");
            exit(1);
        }
        if (lhs_base->kind == ND_VAR)
        {
            if (scope_is_const(sc, lhs_base->var_ref))
            {
                diag_error_at(lhs_base->src, lhs_base->line, lhs_base->col,
                              "cannot assign to constant variable '%s'",
                              lhs_base->var_ref);
                exit(1);
            }
        }
        check_expr(sc, e->rhs);
        Type *lhs_type = lhs_expr->type;
        if (!lhs_type)
        {
            if (lhs_base->kind == ND_VAR)
                lhs_type = scope_get_type(sc, lhs_base->var_ref);
            else if (lhs_base->kind == ND_MEMBER)
                lhs_type = lhs_base->type;
            else if (lhs_base->kind == ND_INDEX && lhs_base->lhs && lhs_base->lhs->type && lhs_base->lhs->type->kind == TY_PTR)
                lhs_type = lhs_base->lhs->type->pointee;
        }
        if (lhs_base->kind == ND_VAR)
        {
            if (!lhs_type)
            {
                diag_error_at(lhs_base->src, lhs_base->line, lhs_base->col,
                              "unknown variable '%s' on left-hand side of assignment",
                              lhs_base->var_ref);
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
        if (!e->lhs || e->lhs->kind != ND_VAR)
        {
            diag_error_at(e->src, e->line, e->col,
                          "operand of ++/-- must be a variable");
            exit(1);
        }
        if (scope_is_const(sc, e->lhs->var_ref))
        {
            diag_error_at(e->lhs->src, e->lhs->line, e->lhs->col,
                          "cannot modify constant variable '%s'", e->lhs->var_ref);
            exit(1);
        }
        Type *t = scope_get_type(sc, e->lhs->var_ref);
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
        // lookup symbol
        if (sc->unit)
        {
            char *alias_resolved = resolve_import_alias(sc->unit, e->call_name);
            if (alias_resolved)
                e->call_name = alias_resolved;
        }
        const Symbol *sym = symtab_get(sc->syms, e->call_name);
        if (!sym)
        {
            if (unit_allows_module_call(sc->unit, e->call_name))
            {
                diag_error_at(e->src, e->line, e->col,
                              "missing metadata for function '%s'",
                              e->call_name);
            }
            else
            {
                diag_error_at(e->src, e->line, e->col,
                              "unknown function '%s'",
                              e->call_name);
            }
            exit(1);
        }
        // check args types minimally
        for (int i = 0; i < e->arg_count; i++)
        {
            check_expr(sc, e->args[i]);
        }

        int expected = sym->sig.param_count;
        if (!sym->sig.is_varargs)
        {
            if (e->arg_count != expected)
            {
                diag_error_at(e->src, e->line, e->col,
                              "function '%s' expects %d argument(s) but %d provided",
                              sym->name ? sym->name : e->call_name,
                              expected, e->arg_count);
                exit(1);
            }
        }
        else if (e->arg_count < expected)
        {
            diag_error_at(e->src, e->line, e->col,
                          "function '%s' expects at least %d argument(s) before varargs",
                          sym->name ? sym->name : e->call_name,
                          expected);
            exit(1);
        }

        int check_count = expected;
        if (sym->sig.is_varargs && e->arg_count > expected)
            check_count = expected;
        if (!sym->sig.is_varargs && e->arg_count < check_count)
            check_count = e->arg_count;

        for (int i = 0; i < check_count; ++i)
        {
            Type *expected_ty = (sym->sig.params && i < expected) ? sym->sig.params[i] : NULL;
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

        Type *ret_type = sym->sig.ret ? sym->sig.ret : &ty_i32;
        e->type = ret_type;
        const char *backend = sym->backend_name ? sym->backend_name : sym->name;
        if (backend)
            e->call_name = backend;
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
        // Branch types must match exactly for now
        if (!type_equal(e->rhs->type, e->body->type))
        {
            diag_error_at(e->src, e->line, e->col, "ternary branches must have the same type");
            exit(1);
        }
        e->type = e->rhs->type;
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
        return sema_check_function(sc, unit);
    }
    if (unit->kind != ND_UNIT)
    {
        fprintf(stderr, "sema: expected unit\n");
        return 1;
    }
    // First pass: add all function symbols
    for (int i = 0; i < unit->stmt_count; i++)
    {
        Node *fn = unit->stmts[i];
        if (!fn || fn->kind != ND_FUNC)
        {
            diag_error("non-function in unit");
            return 1;
        }
        sema_register_function_local(sc, unit, fn);
    }
    // Second pass: type-check bodies
    for (int i = 0; i < unit->stmt_count; i++)
    {
        Node *fn = unit->stmts[i];
        if (check_exposed_function_signature(fn))
            return 1;
        if (sema_check_function(sc, fn))
            return 1;
    }
    sc->unit = previous_unit;
    return 0;
}

void sema_register_foreign_unit_symbols(SemaContext *sc, Node *unit)
{
    if (!sc || !unit)
        return;

    if (unit->kind == ND_UNIT)
    {
        for (int i = 0; i < unit->stmt_count; ++i)
        {
            Node *fn = unit->stmts[i];
            if (fn && fn->kind == ND_FUNC)
                sema_register_function_foreign(sc, unit, fn);
        }
        return;
    }

    if (unit->kind == ND_FUNC)
    {
        sema_register_function_foreign(sc, NULL, unit);
    }
}

// Simple constant evaluation for current subset
int sema_eval_const_i32(Node *expr)
{
    if (expr->kind == ND_INT)
        return (int)expr->int_val;
    if (expr->kind == ND_ADD)
        return sema_eval_const_i32(expr->lhs) + sema_eval_const_i32(expr->rhs);
    fprintf(stderr, "const eval: unsupported expr kind %d\n", expr->kind);
    return 0;
}
