
#include "ast.h"
#include "mangle.h"
#include "module_registry.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>

static int parser_disable_formatting_notes = 0;
static ChanceLanguageStandard parser_language_standard = CHANCE_STD_H28;

#define RAW_EXPORT_PREFIX "__cc_raw_export__"

void parser_set_disable_formatting_notes(int disable)
{
    parser_disable_formatting_notes = disable ? 1 : 0;
}

void parser_set_language_standard(ChanceLanguageStandard standard)
{
    if (standard != CHANCE_STD_H26 && standard != CHANCE_STD_H27 && standard != CHANCE_STD_H28)
        standard = CHANCE_STD_H28;
    parser_language_standard = standard;
}

ChanceLanguageStandard parser_get_language_standard(void)
{
    return parser_language_standard;
}

static int parser_is_h27_enabled(void)
{
    return parser_language_standard >= CHANCE_STD_H27;
}

static int parser_is_h28_enabled(void)
{
    return parser_language_standard >= CHANCE_STD_H28;
}

static void parser_require_h27(Parser *ps, Token tok, const char *feature_name)
{
    (void)ps;
    if (parser_is_h27_enabled())
        return;
    diag_error_at(NULL, tok.line, tok.col,
                  "'%s' requires H27 mode (use -H27)",
                  feature_name ? feature_name : "feature");
    exit(1);
}

static void parser_require_h28(Parser *ps, Token tok, const char *feature_name)
{
    (void)ps;
    if (parser_is_h28_enabled())
        return;
    diag_error_at(NULL, tok.line, tok.col,
                  "'%s' requires H28 mode (use -H28)",
                  feature_name ? feature_name : "feature");
    exit(1);
}

struct ModuleImport
{
    char **parts;
    int part_count;
    char *full_name;
    char *alias;
};

struct BundleStaticMember
{
    char *bundle_name;
    int bundle_name_len;
    char *member_name;
    int member_name_len;
    char *symbol_name;
    int is_function;
    int is_const;
};

struct BundleTemplateInstance
{
    Type **type_args;
    int type_arg_count;
    Type *inst_type;
    char *inst_name;
};

struct BundleTemplate
{
    char *name;
    int name_len;
    int is_exposed;
    Type *bundle_type;
    const char **generic_param_names;
    Type **generic_param_types;
    int generic_param_count;
    const char **field_names;
    Type **field_types;
    const char **field_default_values;
    unsigned char *field_exposed_flags;
    int field_count;
    Node **method_templates;
    int method_template_count;
    int method_template_cap;
    struct BundleTemplateInstance *instances;
    int instance_count;
    int instance_cap;
};

struct Parser
{
    Lexer *lx;
    
    Symbol *externs;
    int ext_count;
    int ext_cap;
    const char *current_function_name;
    
    struct Alias
    {
        char *name;
        int name_len;
        int is_generic;
        char *param;
        int param_len;
        TypeKind base_kind;
        int ptr_depth;
        int gen_ptr_depth;
        Type *resolved_type;
        int is_exposed;
    } *aliases;
    int alias_count;
    int alias_cap;
    struct GenericParam
    {
        char *name;
        int name_len;
        int index;
        Type *placeholder;
        TemplateConstraintKind constraint_kind;
        Type *default_type;
    } *generic_params;
    int generic_param_count;
    int generic_param_cap;
    
    struct NamedType
    {
        char *name;
        int name_len;
        Type *type;
        int is_exposed;
    } *named_types;
    int nt_count;
    int nt_cap;
    
    struct EnumConst
    {
        char *name;
        int name_len;
        int value;
    } *enum_consts;
    int ec_count;
    int ec_cap;
    struct EnumType
    {
        char *name;
        int name_len;
        int is_exposed;
    } *enum_types;
    int et_count;
    int et_cap;
    
    struct ConstInt
    {
        char *name;
        int name_len;
        int value;
    } *const_ints;
    int const_int_count;
    int const_int_cap;
    int *const_scope_marks;
    int const_scope_count;
    int const_scope_cap;
    
    char **module_parts;
    int module_part_count;
    int module_part_cap;
    char *module_full_name;
    struct ModuleImport *imports;
    int import_count;
    int import_cap;
    int suppress_access_for_match;
    int suppress_arrow_member_for_throw;
    int module_is_managed;
    int managed_scope_depth;
    int unmanaged_scope_depth;

    struct BundleStaticMember *bundle_static_members;
    int bundle_static_member_count;
    int bundle_static_member_cap;

    struct BundleTemplate *bundle_templates;
    int bundle_template_count;
    int bundle_template_cap;

    Node **pending_instantiated_decls;
    int pending_instantiated_decl_count;
    int pending_instantiated_decl_cap;
    int foreach_temp_counter;
};

static void parser_push_access_suppression(Parser *ps)
{
    if (ps)
        ps->suppress_access_for_match++;
}

static void parser_pop_access_suppression(Parser *ps)
{
    if (ps && ps->suppress_access_for_match > 0)
        ps->suppress_access_for_match--;
}

static int import_alias_matches(const struct ModuleImport *imp, const char *alias, int alias_len)
{
    if (!imp || !alias || alias_len <= 0 || !imp->alias)
        return 0;
    return ((int)strlen(imp->alias) == alias_len) && strncmp(imp->alias, alias, (size_t)alias_len) == 0;
}

static int import_prefix_matches(const struct ModuleImport *imp, const Token *parts, int part_count)
{
    if (!imp || part_count <= 0 || !parts)
        return 0;
    if (imp->part_count < part_count)
        return 0;
    for (int i = 0; i < part_count; ++i)
    {
        if (!imp->parts[i])
            return 0;
        if (strncmp(imp->parts[i], parts[i].lexeme, (size_t)parts[i].length) != 0 ||
            (int)strlen(imp->parts[i]) != parts[i].length)
            return 0;
    }
    return 1;
}

static const struct ModuleImport *parser_find_import_by_alias(Parser *ps, const char *alias, int alias_len)
{
    if (!ps || !alias)
        return NULL;
    for (int i = 0; i < ps->import_count; ++i)
    {
        if (import_alias_matches(&ps->imports[i], alias, alias_len))
            return &ps->imports[i];
    }
    return NULL;
}

static const struct ModuleImport *parser_find_import_by_parts(Parser *ps, const Token *parts, int part_count)
{
    if (!ps || !parts || part_count <= 0)
        return NULL;
    for (int i = 0; i < ps->import_count; ++i)
    {
        if (import_prefix_matches(&ps->imports[i], parts, part_count))
            return &ps->imports[i];
    }
    return NULL;
}

static int parser_module_tokens_match_current(Parser *ps, const Token *parts, int part_count)
{
    if (!ps || part_count <= 0)
        return 0;
    if (!ps->module_parts || ps->module_part_count != part_count)
        return 0;
    for (int i = 0; i < part_count; ++i)
    {
        if (!ps->module_parts[i])
            return 0;
        if (strncmp(ps->module_parts[i], parts[i].lexeme, (size_t)parts[i].length) != 0 ||
            (int)strlen(ps->module_parts[i]) != parts[i].length)
            return 0;
    }
    return 1;
}

static void parser_unit_append_decl(Node ***decls, int *decl_count, int *decl_cap, Node *decl, int *fn_count)
{
    if (!decls || !decl_count || !decl_cap || !decl)
        return;
    if (*decl_count == *decl_cap)
    {
        *decl_cap = *decl_cap ? (*decl_cap * 2) : 4;
        *decls = (Node **)realloc(*decls, sizeof(Node *) * (size_t)(*decl_cap));
        if (!*decls)
        {
            diag_error("out of memory while appending declaration");
            exit(1);
        }
    }
    (*decls)[(*decl_count)++] = decl;
    if (fn_count && decl->kind == ND_FUNC)
        (*fn_count)++;
}

static void parser_queue_instantiated_decl(Parser *ps, Node *decl)
{
    if (!ps || !decl)
        return;
    if (ps->pending_instantiated_decl_count == ps->pending_instantiated_decl_cap)
    {
        int new_cap = ps->pending_instantiated_decl_cap ? ps->pending_instantiated_decl_cap * 2 : 8;
        Node **grown = (Node **)realloc(ps->pending_instantiated_decls,
                                        (size_t)new_cap * sizeof(Node *));
        if (!grown)
        {
            diag_error("out of memory while queuing instantiated bundle methods");
            exit(1);
        }
        ps->pending_instantiated_decls = grown;
        ps->pending_instantiated_decl_cap = new_cap;
    }
    ps->pending_instantiated_decls[ps->pending_instantiated_decl_count++] = decl;
}

static int parser_token_is_index_overrider_name(const char *name)
{
    return name && name[0] == '[' && name[1] == ']' && name[2] == '\0';
}

static void parser_validate_bundle_index_overrider_signature(Parser *ps,
                                                             const Node *method,
                                                             int is_static)
{
    if (!method || method->kind != ND_FUNC || !parser_token_is_index_overrider_name(method->name))
        return;

    if (is_static)
    {
        diag_error_at(lexer_source(ps->lx), method->line, method->col,
                      "bundle index overrider '[]' cannot be declared static");
        exit(1);
    }

    Type *ret_ty = method->ret_type;
    int is_void_ret = (ret_ty && ret_ty->kind == TY_VOID) ? 1 : 0;
    int user_param_count = method->param_count;

    if (is_void_ret)
    {
        if (user_param_count != 2)
        {
            diag_error_at(lexer_source(ps->lx), method->line, method->col,
                          "bundle index setter '[]' must declare exactly 2 parameters: key and value");
            exit(1);
        }
    }
    else
    {
        if (user_param_count != 1)
        {
            diag_error_at(lexer_source(ps->lx), method->line, method->col,
                          "bundle index getter '[]' must declare exactly 1 key parameter");
            exit(1);
        }
    }
}

static char *parser_make_bundle_symbol_name(const char *prefix, const char *bundle_name, const char *member_name)
{
    if (!prefix || !bundle_name || !member_name)
        return NULL;
    const char *member_segment = parser_token_is_index_overrider_name(member_name) ? "__index" : member_name;
    size_t need = strlen(prefix) + strlen(bundle_name) + strlen(member_segment) + 3;
    char *out = (char *)xmalloc(need);
    snprintf(out, need, "%s_%s_%s", prefix, bundle_name, member_segment);
    return out;
}

static char *parser_make_bundle_instance_method_symbol(const char *bundle_name, const char *method_name)
{
    return parser_make_bundle_symbol_name("__bundle_inst", bundle_name, method_name);
}

static char *parser_make_bundle_static_method_symbol(const char *bundle_name, const char *method_name)
{
    return parser_make_bundle_symbol_name("__bundle_static", bundle_name, method_name);
}

static char *parser_make_bundle_static_field_symbol(const char *bundle_name, const char *field_name)
{
    return parser_make_bundle_symbol_name("__bundle_static_field", bundle_name, field_name);
}

static void parser_record_bundle_static_member(Parser *ps,
                                               const char *bundle_name,
                                               const char *member_name,
                                               const char *symbol_name,
                                               int is_function,
                                               int is_const)
{
    if (!ps || !bundle_name || !member_name || !symbol_name)
        return;
    if (ps->bundle_static_member_count == ps->bundle_static_member_cap)
    {
        int new_cap = ps->bundle_static_member_cap ? ps->bundle_static_member_cap * 2 : 8;
        struct BundleStaticMember *grown = (struct BundleStaticMember *)realloc(
            ps->bundle_static_members, (size_t)new_cap * sizeof(struct BundleStaticMember));
        if (!grown)
        {
            diag_error("out of memory while recording bundle static member");
            exit(1);
        }
        ps->bundle_static_members = grown;
        ps->bundle_static_member_cap = new_cap;
    }

    struct BundleStaticMember *entry = &ps->bundle_static_members[ps->bundle_static_member_count++];
    entry->bundle_name = xstrdup(bundle_name);
    entry->bundle_name_len = (int)strlen(bundle_name);
    entry->member_name = xstrdup(member_name);
    entry->member_name_len = (int)strlen(member_name);
    entry->symbol_name = xstrdup(symbol_name);
    entry->is_function = is_function ? 1 : 0;
    entry->is_const = is_const ? 1 : 0;
}

static const struct BundleStaticMember *parser_find_bundle_static_member(Parser *ps,
                                                                          const char *bundle_name,
                                                                          int bundle_name_len,
                                                                          const char *member_name,
                                                                          int member_name_len)
{
    if (!ps || !bundle_name || !member_name || bundle_name_len <= 0 || member_name_len <= 0)
        return NULL;
    for (int i = 0; i < ps->bundle_static_member_count; ++i)
    {
        const struct BundleStaticMember *entry = &ps->bundle_static_members[i];
        if (!entry->bundle_name || !entry->member_name)
            continue;
        if (entry->bundle_name_len != bundle_name_len || entry->member_name_len != member_name_len)
            continue;
        if (strncmp(entry->bundle_name, bundle_name, (size_t)bundle_name_len) != 0)
            continue;
        if (strncmp(entry->member_name, member_name, (size_t)member_name_len) != 0)
            continue;
        return entry;
    }
    return NULL;
}

static Type *parse_type_spec(Parser *ps);
static Type *named_type_get(Parser *ps, const char *name, int len);
static void named_type_add(Parser *ps, const char *name, int len, Type *ty, int is_exposed);
static int type_sizeof_simple(Type *ty);
static int type_align_simple(Type *ty);
static int align_up(int value, int alignment);

static struct BundleTemplate *parser_find_bundle_template(Parser *ps, const char *name, int name_len)
{
    if (!ps || !name || name_len <= 0)
        return NULL;
    for (int i = 0; i < ps->bundle_template_count; ++i)
    {
        struct BundleTemplate *templ = &ps->bundle_templates[i];
        if (templ->name_len != name_len)
            continue;
        if (strncmp(templ->name, name, (size_t)name_len) == 0)
            return templ;
    }
    return NULL;
}

static int parser_struct_name_has_template_instance_prefix(const char *struct_name,
                                                           const char *base_name,
                                                           int base_name_len)
{
    if (!struct_name || !base_name || base_name_len <= 0)
        return 0;
    if (strncmp(struct_name, base_name, (size_t)base_name_len) != 0)
        return 0;
    return strncmp(struct_name + base_name_len, "__inst", 6) == 0;
}

static int parser_module_is_current_or_imported(Parser *ps, const char *module_full)
{
    if (!ps || !module_full || !*module_full)
        return 0;
    if (ps->module_full_name && strcmp(ps->module_full_name, module_full) == 0)
        return 1;
    for (int i = 0; i < ps->import_count; ++i)
    {
        const struct ModuleImport *imp = &ps->imports[i];
        if (!imp || !imp->full_name)
            continue;
        if (strcmp(imp->full_name, module_full) == 0)
            return 1;
    }
    return 0;
}

static int parser_type_contains_template_param_recur(Type *ty, Type **seen, int seen_count)
{
    ty = module_registry_canonical_type(ty);
    if (!ty)
        return 0;

    if (ty->kind == TY_TEMPLATE_PARAM)
        return 1;

    for (int i = 0; i < seen_count; ++i)
    {
        if (seen[i] == ty)
            return 0;
    }

    if (seen_count >= 64)
        return 0;
    seen[seen_count++] = ty;

    switch (ty->kind)
    {
    case TY_PTR:
    case TY_REF:
        return parser_type_contains_template_param_recur(ty->pointee, seen, seen_count);
    case TY_ARRAY:
        return parser_type_contains_template_param_recur(ty->array.elem, seen, seen_count);
    case TY_FUNC:
        if (parser_type_contains_template_param_recur(ty->func.ret, seen, seen_count))
            return 1;
        for (int i = 0; i < ty->func.param_count; ++i)
        {
            Type *param = ty->func.params ? ty->func.params[i] : NULL;
            if (parser_type_contains_template_param_recur(param, seen, seen_count))
                return 1;
        }
        return 0;
    case TY_STRUCT:
        for (int i = 0; i < ty->strct.field_count; ++i)
        {
            Type *field_ty = ty->strct.field_types ? ty->strct.field_types[i] : NULL;
            if (parser_type_contains_template_param_recur(field_ty, seen, seen_count))
                return 1;
        }
        return 0;
    default:
        return 0;
    }
}

static int parser_type_contains_template_param(Type *ty)
{
    Type *seen[64];
    memset(seen, 0, sizeof(seen));
    return parser_type_contains_template_param_recur(ty, seen, 0);
}

static int parser_type_tree_match_score_recur(Type *haystack,
                                              Type *needle,
                                              Type **seen,
                                              int seen_count,
                                              int through_container)
{
    haystack = module_registry_canonical_type(haystack);
    needle = module_registry_canonical_type(needle);
    if (!haystack || !needle)
        return 0;

    int best = 0;
    if (type_equals(haystack, needle))
        best = through_container ? 2 : 1;

    for (int i = 0; i < seen_count; ++i)
    {
        if (seen[i] == haystack)
            return best;
    }
    if (seen_count >= 64)
        return best;
    seen[seen_count++] = haystack;

    int child_score = 0;
    switch (haystack->kind)
    {
    case TY_PTR:
    case TY_REF:
        child_score = parser_type_tree_match_score_recur(haystack->pointee, needle, seen, seen_count, 1);
        break;
    case TY_ARRAY:
        child_score = parser_type_tree_match_score_recur(haystack->array.elem, needle, seen, seen_count, 1);
        break;
    case TY_FUNC:
        child_score = parser_type_tree_match_score_recur(haystack->func.ret, needle, seen, seen_count, 1);
        for (int i = 0; i < haystack->func.param_count; ++i)
        {
            Type *param = haystack->func.params ? haystack->func.params[i] : NULL;
            int param_score = parser_type_tree_match_score_recur(param, needle, seen, seen_count, 1);
            if (param_score > child_score)
                child_score = param_score;
        }
        break;
    case TY_STRUCT:
        for (int i = 0; i < haystack->strct.field_count; ++i)
        {
            Type *field_ty = haystack->strct.field_types ? haystack->strct.field_types[i] : NULL;
            int field_score = parser_type_tree_match_score_recur(field_ty, needle, seen, seen_count, through_container);
            if (field_score > child_score)
                child_score = field_score;
        }
        break;
    default:
        break;
    }

    return (child_score > best) ? child_score : best;
}

static int parser_type_tree_match_score(Type *haystack, Type *needle)
{
    Type *seen[64];
    memset(seen, 0, sizeof(seen));
    return parser_type_tree_match_score_recur(haystack, needle, seen, 0, 0);
}

static Type *parser_lookup_preinstantiated_template_instance(Parser *ps,
                                                             const char *module_full,
                                                             const char *base_name,
                                                             int base_name_len,
                                                             Type **template_type_args,
                                                             int template_type_arg_count,
                                                             int *out_match_count)
{
    int matches = 0;
    Type *resolved = NULL;
    int best_score = INT_MIN;
    int best_count = 0;
    if (!base_name || base_name_len <= 0)
    {
        if (out_match_count)
            *out_match_count = 0;
        return NULL;
    }

    int total = module_registry_struct_entry_count();
    for (int i = 0; i < total; ++i)
    {
        const char *entry_module = module_registry_struct_entry_module(i);
        const char *entry_name = module_registry_struct_entry_name(i);
        Type *entry_type = module_registry_struct_entry_type(i);
        if (!entry_name || !entry_type || !entry_type->is_bundle)
            continue;
        if (!parser_struct_name_has_template_instance_prefix(entry_name, base_name, base_name_len))
            continue;

        if (module_full && *module_full)
        {
            if (!entry_module || strcmp(entry_module, module_full) != 0)
                continue;
        }
        else
        {
            if (!parser_module_is_current_or_imported(ps, entry_module))
                continue;
        }

        if (parser_type_contains_template_param(entry_type))
            continue;

        if (template_type_arg_count > 0)
        {
            int candidate_score = 0;
            int arg_match = 1;
            for (int ai = 0; ai < template_type_arg_count; ++ai)
            {
                Type *want = template_type_args ? template_type_args[ai] : NULL;
                want = module_registry_canonical_type(want);
                if (!want)
                    continue;

                int score = parser_type_tree_match_score(entry_type, want);
                if (score <= 0)
                {
                    arg_match = 0;
                    break;
                }
                candidate_score += score;
            }
            if (!arg_match)
                continue;

            if (candidate_score > best_score)
            {
                best_score = candidate_score;
                best_count = 1;
                resolved = entry_type;
            }
            else if (candidate_score == best_score)
            {
                best_count++;
            }
            continue;
        }

        resolved = entry_type;
        matches++;
    }

    if (template_type_arg_count > 0)
    {
        if (out_match_count)
            *out_match_count = best_count;
        return best_count == 1 ? resolved : NULL;
    }

    if (out_match_count)
        *out_match_count = matches;
    return matches == 1 ? resolved : NULL;
}

static Type **parser_parse_type_arg_list(Parser *ps, int *out_count)
{
    if (!ps)
        return NULL;
    Token lt = lexer_next(ps->lx);
    if (lt.kind != TK_LT)
    {
        diag_error_at(lexer_source(ps->lx), lt.line, lt.col,
                      "internal parser error: expected '<' before type arguments");
        exit(1);
    }
    Token next = lexer_peek(ps->lx);
    if (next.kind == TK_GT)
    {
        diag_error_at(lexer_source(ps->lx), next.line, next.col,
                      "type argument list cannot be empty");
        exit(1);
    }
    Type **args = NULL;
    int count = 0;
    int cap = 0;
    while (1)
    {
        Type *arg_ty = parse_type_spec(ps);
        if (count == cap)
        {
            cap = cap ? cap * 2 : 4;
            Type **grown = (Type **)realloc(args, (size_t)cap * sizeof(Type *));
            if (!grown)
            {
                diag_error("out of memory while parsing type arguments");
                exit(1);
            }
            args = grown;
        }
        args[count++] = arg_ty;
        Token sep = lexer_peek(ps->lx);
        if (sep.kind == TK_COMMA)
        {
            lexer_next(ps->lx);
            continue;
        }
        if (sep.kind == TK_GT)
        {
            lexer_next(ps->lx);
            break;
        }
        diag_error_at(lexer_source(ps->lx), sep.line, sep.col,
                      "expected ',' or '>' in type argument list");
        exit(1);
    }
    if (out_count)
        *out_count = count;
    return args;
}

static Type *parser_substitute_template_type(Type *orig, Type **bindings, int binding_count)
{
    if (!orig)
        return NULL;
    orig = module_registry_canonical_type(orig);
    if (!orig)
        return NULL;

    if (orig->kind == TY_TEMPLATE_PARAM)
    {
        int idx = orig->template_param_index;
        if (idx >= 0 && idx < binding_count && bindings && bindings[idx])
            return bindings[idx];
        if (orig->template_default_type)
            return parser_substitute_template_type(orig->template_default_type, bindings, binding_count);
        return orig;
    }

    if (!bindings || binding_count <= 0)
        return orig;

    switch (orig->kind)
    {
    case TY_PTR:
    {
        Type *pointee = parser_substitute_template_type(orig->pointee, bindings, binding_count);
        if (pointee == orig->pointee)
            return orig;
        return type_ptr(pointee);
    }
    case TY_REF:
    {
        Type *pointee = parser_substitute_template_type(orig->pointee, bindings, binding_count);
        if (pointee == orig->pointee)
            return orig;
        return type_ref(pointee, orig->ref_nullability);
    }
    case TY_ARRAY:
    {
        Type *elem = parser_substitute_template_type(orig->array.elem, bindings, binding_count);
        if (elem == orig->array.elem)
            return orig;
        Type *arr = type_array(elem, orig->array.length);
        arr->array.is_unsized = orig->array.is_unsized;
        return arr;
    }
    case TY_FUNC:
    {
        Type *ret = parser_substitute_template_type(orig->func.ret, bindings, binding_count);
        int changed = (ret != orig->func.ret);
        Type **params = NULL;
        if (orig->func.param_count > 0)
        {
            params = (Type **)xcalloc((size_t)orig->func.param_count, sizeof(Type *));
            for (int i = 0; i < orig->func.param_count; ++i)
            {
                Type *sub = parser_substitute_template_type(orig->func.params ? orig->func.params[i] : NULL,
                                                            bindings, binding_count);
                params[i] = sub;
                if (sub != (orig->func.params ? orig->func.params[i] : NULL))
                    changed = 1;
            }
        }
        if (!changed)
        {
            free(params);
            return orig;
        }
        Type *fn = type_func();
        fn->func.param_count = orig->func.param_count;
        fn->func.params = params;
        fn->func.ret = ret;
        fn->func.is_varargs = orig->func.is_varargs;
        fn->func.has_signature = orig->func.has_signature;
        return fn;
    }
    default:
        return orig;
    }
}

static Type *parser_substitute_bundle_method_type(Type *orig,
                                                  Type **bindings,
                                                  int binding_count,
                                                  Type *template_bundle_type,
                                                  Type *inst_bundle_type)
{
    if (!orig)
        return NULL;
    orig = module_registry_canonical_type(orig);
    if (!orig)
        return NULL;

    Type *canonical_template = module_registry_canonical_type(template_bundle_type);
    if (canonical_template && inst_bundle_type && orig == canonical_template)
        return inst_bundle_type;

    if (orig->kind == TY_TEMPLATE_PARAM)
    {
        int idx = orig->template_param_index;
        if (idx >= 0 && idx < binding_count && bindings && bindings[idx])
            return bindings[idx];
        if (orig->template_default_type)
            return parser_substitute_bundle_method_type(orig->template_default_type,
                                                        bindings,
                                                        binding_count,
                                                        template_bundle_type,
                                                        inst_bundle_type);
        return orig;
    }

    if (!bindings || binding_count <= 0)
        return orig;

    switch (orig->kind)
    {
    case TY_PTR:
    {
        Type *pointee = parser_substitute_bundle_method_type(orig->pointee,
                                                             bindings,
                                                             binding_count,
                                                             template_bundle_type,
                                                             inst_bundle_type);
        if (pointee == orig->pointee)
            return orig;
        return type_ptr(pointee);
    }
    case TY_REF:
    {
        Type *pointee = parser_substitute_bundle_method_type(orig->pointee,
                                                             bindings,
                                                             binding_count,
                                                             template_bundle_type,
                                                             inst_bundle_type);
        if (pointee == orig->pointee)
            return orig;
        return type_ref(pointee, orig->ref_nullability);
    }
    case TY_ARRAY:
    {
        Type *elem = parser_substitute_bundle_method_type(orig->array.elem,
                                                          bindings,
                                                          binding_count,
                                                          template_bundle_type,
                                                          inst_bundle_type);
        if (elem == orig->array.elem)
            return orig;
        Type *arr = type_array(elem, orig->array.length);
        arr->array.is_unsized = orig->array.is_unsized;
        return arr;
    }
    case TY_FUNC:
    {
        Type *ret = parser_substitute_bundle_method_type(orig->func.ret,
                                                         bindings,
                                                         binding_count,
                                                         template_bundle_type,
                                                         inst_bundle_type);
        int changed = (ret != orig->func.ret);
        Type **params = NULL;
        if (orig->func.param_count > 0)
        {
            params = (Type **)xcalloc((size_t)orig->func.param_count, sizeof(Type *));
            for (int i = 0; i < orig->func.param_count; ++i)
            {
                Type *sub = parser_substitute_bundle_method_type(orig->func.params ? orig->func.params[i] : NULL,
                                                                 bindings,
                                                                 binding_count,
                                                                 template_bundle_type,
                                                                 inst_bundle_type);
                params[i] = sub;
                if (sub != (orig->func.params ? orig->func.params[i] : NULL))
                    changed = 1;
            }
        }
        if (!changed)
        {
            free(params);
            return orig;
        }
        Type *fn = type_func();
        fn->func.param_count = orig->func.param_count;
        fn->func.params = params;
        fn->func.ret = ret;
        fn->func.is_varargs = orig->func.is_varargs;
        fn->func.has_signature = orig->func.has_signature;
        return fn;
    }
    default:
        return orig;
    }
}

static Node *parser_clone_node_tree_for_bundle_method(const Node *src,
                                                      Type **bindings,
                                                      int binding_count,
                                                      Type *template_bundle_type,
                                                      Type *inst_bundle_type)
{
    if (!src)
        return NULL;
    Node *dst = (Node *)xcalloc(1, sizeof(Node));
    memcpy(dst, src, sizeof(Node));

    dst->lhs = parser_clone_node_tree_for_bundle_method(src->lhs,
                                                        bindings,
                                                        binding_count,
                                                        template_bundle_type,
                                                        inst_bundle_type);
    dst->rhs = parser_clone_node_tree_for_bundle_method(src->rhs,
                                                        bindings,
                                                        binding_count,
                                                        template_bundle_type,
                                                        inst_bundle_type);
    dst->body = parser_clone_node_tree_for_bundle_method(src->body,
                                                         bindings,
                                                         binding_count,
                                                         template_bundle_type,
                                                         inst_bundle_type);

    if (src->arg_count > 0 && src->args)
    {
        dst->args = (Node **)xcalloc((size_t)src->arg_count, sizeof(Node *));
        for (int i = 0; i < src->arg_count; ++i)
            dst->args[i] = parser_clone_node_tree_for_bundle_method(src->args[i],
                                                                    bindings,
                                                                    binding_count,
                                                                    template_bundle_type,
                                                                    inst_bundle_type);
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
            dst->call_type_args[i] = parser_substitute_bundle_method_type(src->call_type_args[i],
                                                                           bindings,
                                                                           binding_count,
                                                                           template_bundle_type,
                                                                           inst_bundle_type);
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
            dst->stmts[i] = parser_clone_node_tree_for_bundle_method(src->stmts[i],
                                                                     bindings,
                                                                     binding_count,
                                                                     template_bundle_type,
                                                                     inst_bundle_type);
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
                dst->init.elems[i] = parser_clone_node_tree_for_bundle_method(src->init.elems[i],
                                                                               bindings,
                                                                               binding_count,
                                                                               template_bundle_type,
                                                                               inst_bundle_type);
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
            memcpy(dst->init.field_indices,
                   src->init.field_indices,
                   (size_t)src->init.count * sizeof(int));
        }
    }

    dst->managed_length_expr = parser_clone_node_tree_for_bundle_method(src->managed_length_expr,
                                                                         bindings,
                                                                         binding_count,
                                                                         template_bundle_type,
                                                                         inst_bundle_type);

    dst->type = parser_substitute_bundle_method_type(src->type,
                                                     bindings,
                                                     binding_count,
                                                     template_bundle_type,
                                                     inst_bundle_type);
    dst->var_type = parser_substitute_bundle_method_type(src->var_type,
                                                         bindings,
                                                         binding_count,
                                                         template_bundle_type,
                                                         inst_bundle_type);
    dst->ret_type = parser_substitute_bundle_method_type(src->ret_type,
                                                         bindings,
                                                         binding_count,
                                                         template_bundle_type,
                                                         inst_bundle_type);

    dst->call_func_type = NULL;
    dst->call_target = NULL;
    dst->referenced_function = NULL;
    dst->inline_expr = NULL;

    return dst;
}

static Node *parser_instantiate_bundle_method_template(const Node *fn,
                                                       char *inst_name,
                                                       Type **bindings,
                                                       int binding_count,
                                                       Type *template_bundle_type,
                                                       Type *inst_bundle_type)
{
    if (!fn || fn->kind != ND_FUNC || !inst_name)
        return NULL;

    Node *clone = (Node *)xcalloc(1, sizeof(Node));
    memcpy(clone, fn, sizeof(Node));
    clone->name = inst_name;
    clone->metadata = fn->metadata;
    clone->metadata.backend_name = NULL;
    clone->generic_param_count = 0;
    clone->generic_param_names = NULL;
    clone->generic_param_types = NULL;

    clone->body = parser_clone_node_tree_for_bundle_method(fn->body,
                                                           bindings,
                                                           binding_count,
                                                           template_bundle_type,
                                                           inst_bundle_type);
    clone->ret_type = parser_substitute_bundle_method_type(fn->ret_type,
                                                           bindings,
                                                           binding_count,
                                                           template_bundle_type,
                                                           inst_bundle_type);

    if (fn->param_count > 0 && fn->param_types)
    {
        clone->param_types = (Type **)xcalloc((size_t)fn->param_count, sizeof(Type *));
        for (int i = 0; i < fn->param_count; ++i)
        {
            clone->param_types[i] = parser_substitute_bundle_method_type(fn->param_types[i],
                                                                          bindings,
                                                                          binding_count,
                                                                          template_bundle_type,
                                                                          inst_bundle_type);
        }
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

    return clone;
}

static Type *parser_instantiate_bundle_template(Parser *ps,
                                                struct BundleTemplate *templ,
                                                Type **type_args,
                                                int type_arg_count,
                                                int line,
                                                int col);
static struct BundleTemplateInstance *parser_find_bundle_template_instance_by_type(Parser *ps,
                                                                                   Type *inst_type,
                                                                                   struct BundleTemplate **out_template);

static void parser_specialize_instantiated_method_new_types(Parser *ps,
                                                            Node *node,
                                                            Type **bindings,
                                                            int binding_count,
                                                            int line,
                                                            int col)
{
    if (!ps || !node)
        return;

    if (node->kind == ND_NEW && node->type)
    {
        Type *new_ty = module_registry_canonical_type(node->type);
        if (new_ty && new_ty->kind == TY_PTR && new_ty->pointee)
        {
            Type *pointee = module_registry_canonical_type(new_ty->pointee);
            if (pointee && pointee->kind == TY_STRUCT)
            {
                struct BundleTemplate *nested_template = NULL;
                struct BundleTemplateInstance *nested_inst =
                    parser_find_bundle_template_instance_by_type(ps, pointee, &nested_template);
                if (nested_inst && nested_template && nested_inst->type_arg_count > 0)
                {
                    int nested_count = nested_inst->type_arg_count;
                    Type **nested_args = (Type **)xcalloc((size_t)nested_count, sizeof(Type *));
                    int nested_changed = 0;
                    for (int ai = 0; ai < nested_count; ++ai)
                    {
                        Type *orig_arg = nested_inst->type_args ? nested_inst->type_args[ai] : NULL;
                        Type *sub_arg = parser_substitute_template_type(orig_arg, bindings, binding_count);
                        nested_args[ai] = sub_arg;
                        if (sub_arg != orig_arg)
                            nested_changed = 1;
                    }

                    if (nested_changed)
                    {
                        Type *specialized = parser_instantiate_bundle_template(ps,
                                                                               nested_template,
                                                                               nested_args,
                                                                               nested_count,
                                                                               line,
                                                                               col);
                        if (specialized)
                            node->type = type_ptr(specialized);
                    }
                    free(nested_args);
                }
            }
        }
    }

    parser_specialize_instantiated_method_new_types(ps, node->lhs, bindings, binding_count, line, col);
    parser_specialize_instantiated_method_new_types(ps, node->rhs, bindings, binding_count, line, col);
    parser_specialize_instantiated_method_new_types(ps, node->body, bindings, binding_count, line, col);

    for (int i = 0; i < node->arg_count; ++i)
        parser_specialize_instantiated_method_new_types(ps, node->args ? node->args[i] : NULL, bindings, binding_count, line, col);
    for (int i = 0; i < node->stmt_count; ++i)
        parser_specialize_instantiated_method_new_types(ps, node->stmts ? node->stmts[i] : NULL, bindings, binding_count, line, col);

    if (node->init.count > 0 && node->init.elems)
    {
        for (int i = 0; i < node->init.count; ++i)
            parser_specialize_instantiated_method_new_types(ps, node->init.elems[i], bindings, binding_count, line, col);
    }

    parser_specialize_instantiated_method_new_types(ps, node->managed_length_expr, bindings, binding_count, line, col);
}

static void parser_emit_template_bundle_methods(Parser *ps,
                                                struct BundleTemplate *templ,
                                                Type *inst_type,
                                                Type **bindings,
                                                int binding_count,
                                                int line,
                                                int col)
{
    if (!ps || !templ || !inst_type || !inst_type->struct_name)
        return;

    for (int i = 0; i < templ->method_template_count; ++i)
    {
        Node *method_template = templ->method_templates ? templ->method_templates[i] : NULL;
        if (!method_template || method_template->kind != ND_FUNC || !method_template->name)
            continue;

        int is_static_method = method_template->var_is_static ? 1 : 0;

        char *inst_symbol = is_static_method
                                ? parser_make_bundle_static_method_symbol(inst_type->struct_name,
                                                                          method_template->name)
                                : parser_make_bundle_instance_method_symbol(inst_type->struct_name,
                                                                            method_template->name);
        if (!inst_symbol)
        {
            diag_error_at(lexer_source(ps->lx), line, col,
                          "failed to build instantiated template bundle method symbol");
            exit(1);
        }

        Node *inst_method = parser_instantiate_bundle_method_template(method_template,
                                                                       inst_symbol,
                                                                       bindings,
                                                                       binding_count,
                                                                       templ->bundle_type,
                                                                       inst_type);
        if (!inst_method)
        {
            diag_error_at(lexer_source(ps->lx), line, col,
                          "failed to instantiate template bundle method '%s'",
                          method_template->name ? method_template->name : "<unnamed>");
            exit(1);
        }

        parser_specialize_instantiated_method_new_types(ps,
                                                        inst_method->body,
                                                        bindings,
                                                        binding_count,
                                                        line,
                                                        col);

        inst_method->var_is_static = is_static_method;
        inst_method->is_bundle_global_init = 0;
        if (is_static_method)
            parser_record_bundle_static_member(ps, inst_type->struct_name, method_template->name, inst_symbol, 1, 1);
        parser_queue_instantiated_decl(ps, inst_method);
    }
}

static Type *parser_instantiate_bundle_template(Parser *ps,
                                                struct BundleTemplate *templ,
                                                Type **type_args,
                                                int type_arg_count,
                                                int line,
                                                int col);

static Type *parser_default_type_for_template_param(Type *placeholder)
{
    if (!placeholder)
        return NULL;

    if (placeholder->template_default_type)
        return module_registry_canonical_type(placeholder->template_default_type);

    switch (placeholder->template_constraint_kind)
    {
    case TEMPLATE_CONSTRAINT_FLOATING:
        return type_f64();
    case TEMPLATE_CONSTRAINT_POINTER:
        return type_ptr(type_void());
    case TEMPLATE_CONSTRAINT_INTEGRAL:
    case TEMPLATE_CONSTRAINT_NUMERIC:
    case TEMPLATE_CONSTRAINT_NONE:
    default:
        return type_i32();
    }
}

static void parser_auto_seed_template_bundle(Parser *ps,
                                             struct BundleTemplate *templ,
                                             int line,
                                             int col)
{
    if (!ps || !templ)
        return;
    if (!templ->is_exposed)
        return;
    if (templ->instance_count > 0)
        return;
    if (templ->generic_param_count <= 0)
        return;

    int arg_count = templ->generic_param_count;
    Type **seed_args = (Type **)xcalloc((size_t)arg_count, sizeof(Type *));
    if (!seed_args)
    {
        diag_error("out of memory while seeding template bundle instantiation");
        exit(1);
    }

    for (int i = 0; i < arg_count; ++i)
    {
        Type *placeholder = templ->generic_param_types ? templ->generic_param_types[i] : NULL;
        seed_args[i] = parser_default_type_for_template_param(placeholder);
        if (!seed_args[i])
        {
            free(seed_args);
            return;
        }
    }

    (void)parser_instantiate_bundle_template(ps, templ, seed_args, arg_count, line, col);
    free(seed_args);
}

static struct BundleTemplateInstance *parser_find_bundle_template_instance_by_type(Parser *ps,
                                                                                   Type *inst_type,
                                                                                   struct BundleTemplate **out_template)
{
    if (out_template)
        *out_template = NULL;
    if (!ps || !inst_type)
        return NULL;

    for (int ti = 0; ti < ps->bundle_template_count; ++ti)
    {
        struct BundleTemplate *templ = &ps->bundle_templates[ti];
        if (!templ || !templ->instances)
            continue;
        for (int ii = 0; ii < templ->instance_count; ++ii)
        {
            struct BundleTemplateInstance *inst = &templ->instances[ii];
            if (inst->inst_type != inst_type)
                continue;
            if (out_template)
                *out_template = templ;
            return inst;
        }
    }
    return NULL;
}

static Type *parser_instantiate_bundle_template(Parser *ps,
                                                struct BundleTemplate *templ,
                                                Type **type_args,
                                                int type_arg_count,
                                                int line,
                                                int col)
{
    if (!ps || !templ)
        return NULL;

    if (type_arg_count > templ->generic_param_count)
    {
        diag_error_at(lexer_source(ps->lx), line, col,
                      "bundle '%s' expects %d type argument(s) but %d provided",
                      templ->name, templ->generic_param_count, type_arg_count);
        exit(1);
    }

    int arg_count = templ->generic_param_count;
    Type **resolved_args = (Type **)xcalloc((size_t)arg_count, sizeof(Type *));
    for (int i = 0; i < type_arg_count; ++i)
        resolved_args[i] = module_registry_canonical_type(type_args ? type_args[i] : NULL);

    for (int i = type_arg_count; i < arg_count; ++i)
    {
        Type *placeholder = templ->generic_param_types ? templ->generic_param_types[i] : NULL;
        Type *default_type = placeholder ? placeholder->template_default_type : NULL;
        if (!default_type)
        {
            const char *pname = (templ->generic_param_names && templ->generic_param_names[i])
                                    ? templ->generic_param_names[i]
                                    : "T";
            diag_error_at(lexer_source(ps->lx), line, col,
                          "missing type argument for template parameter '%s' of bundle '%s'",
                          pname, templ->name);
            exit(1);
        }
        resolved_args[i] = module_registry_canonical_type(default_type);
    }

    for (int i = 0; i < templ->instance_count; ++i)
    {
        struct BundleTemplateInstance *inst = &templ->instances[i];
        if (inst->type_arg_count != arg_count)
            continue;
        int match = 1;
        for (int j = 0; j < arg_count; ++j)
        {
            if (!type_equals(inst->type_args[j], resolved_args[j]))
            {
                match = 0;
                break;
            }
        }
        if (match)
        {
            free(resolved_args);
            return inst->inst_type;
        }
    }

    int inst_index = templ->instance_count;
    char name_buf[256];
    for (;;)
    {
        snprintf(name_buf, sizeof(name_buf), "%s__inst%d", templ->name, inst_index);
        if (!named_type_get(ps, name_buf, (int)strlen(name_buf)))
            break;
        inst_index++;
    }
    char *inst_name = xstrdup(name_buf);

    Type *inst_type = (Type *)xcalloc(1, sizeof(Type));
    inst_type->kind = TY_STRUCT;
    inst_type->is_bundle = 1;
    inst_type->is_exposed = templ->is_exposed ? 1 : 0;
    inst_type->struct_name = inst_name;
    inst_type->bundle_ctor_declared = (templ->bundle_type && templ->bundle_type->bundle_ctor_declared) ? 1 : 0;
    inst_type->bundle_ctor_declared_exposed = (templ->bundle_type && templ->bundle_type->bundle_ctor_declared_exposed) ? 1 : 0;
    inst_type->bundle_ctor_has_zero_arity = (templ->bundle_type && templ->bundle_type->bundle_ctor_has_zero_arity) ? 1 : 0;
    inst_type->bundle_ctor_has_exposed_zero_arity = (templ->bundle_type && templ->bundle_type->bundle_ctor_has_exposed_zero_arity) ? 1 : 0;

    int field_count = templ->field_count;
    const char **field_names = (const char **)xcalloc((size_t)field_count, sizeof(const char *));
    Type **field_types = (Type **)xcalloc((size_t)field_count, sizeof(Type *));
    const char **field_defaults = (const char **)xcalloc((size_t)field_count, sizeof(const char *));
    unsigned char *field_exposed = (unsigned char *)xcalloc((size_t)field_count, sizeof(unsigned char));
    int *field_offsets = (int *)xcalloc((size_t)field_count, sizeof(int));

    int offset = 0;
    int max_align = 1;
    for (int i = 0; i < field_count; ++i)
    {
        const char *src_name = templ->field_names ? templ->field_names[i] : NULL;
        Type *src_type = templ->field_types ? templ->field_types[i] : NULL;
        Type *inst_field_type = parser_substitute_template_type(src_type, resolved_args, arg_count);

        if (inst_field_type && inst_field_type->kind == TY_STRUCT)
        {
            struct BundleTemplate *nested_template = NULL;
            struct BundleTemplateInstance *nested_inst =
                parser_find_bundle_template_instance_by_type(ps, inst_field_type, &nested_template);
            if (nested_inst && nested_template && nested_inst->type_arg_count > 0)
            {
                int nested_count = nested_inst->type_arg_count;
                Type **nested_args = (Type **)xcalloc((size_t)nested_count, sizeof(Type *));
                int nested_changed = 0;
                for (int ai = 0; ai < nested_count; ++ai)
                {
                    Type *orig_arg = nested_inst->type_args ? nested_inst->type_args[ai] : NULL;
                    Type *sub_arg = parser_substitute_template_type(orig_arg, resolved_args, arg_count);
                    nested_args[ai] = sub_arg;
                    if (sub_arg != orig_arg)
                        nested_changed = 1;
                }

                if (nested_changed)
                {
                    Type *specialized = parser_instantiate_bundle_template(ps,
                                                                           nested_template,
                                                                           nested_args,
                                                                           nested_count,
                                                                           line,
                                                                           col);
                    if (specialized)
                        inst_field_type = specialized;
                }
                free(nested_args);
            }
        }

        if (!inst_field_type)
        {
            diag_error_at(lexer_source(ps->lx), line, col,
                          "failed to instantiate field type for bundle '%s'",
                          templ->name);
            exit(1);
        }

        field_names[i] = src_name ? xstrdup(src_name) : xstrdup("<unnamed>");
        field_types[i] = inst_field_type;
        field_defaults[i] = (templ->field_default_values && templ->field_default_values[i])
                                ? xstrdup(templ->field_default_values[i])
                                : NULL;
        field_exposed[i] = (templ->field_exposed_flags && templ->field_exposed_flags[i]) ? 1 : 0;

        int field_align = type_align_simple(inst_field_type);
        if (field_align <= 0)
            field_align = 1;
        int field_size = type_sizeof_simple(inst_field_type);
        if (inst_field_type->kind == TY_STRUCT && field_size == 0)
        {
            diag_error_at(lexer_source(ps->lx), line, col,
                          "template bundle '%s' field '%s' has incomplete instantiated type",
                          templ->name, field_names[i]);
            exit(1);
        }

        offset = align_up(offset, field_align);
        field_offsets[i] = offset;
        offset += field_size;
        if (field_align > max_align)
            max_align = field_align;
    }

    inst_type->strct.field_names = field_names;
    inst_type->strct.field_types = field_types;
    inst_type->strct.field_default_values = field_defaults;
    inst_type->strct.field_exposed_flags = field_exposed;
    inst_type->strct.field_offsets = field_offsets;
    inst_type->strct.field_count = field_count;
    inst_type->strct.size_bytes = align_up(offset, max_align > 0 ? max_align : 1);

    named_type_add(ps, inst_name, (int)strlen(inst_name), inst_type, inst_type->is_exposed);
    if (inst_type->is_exposed && ps->module_full_name)
        module_registry_register_struct(ps->module_full_name, inst_type);

    if (templ->instance_count == templ->instance_cap)
    {
        int new_cap = templ->instance_cap ? templ->instance_cap * 2 : 4;
        struct BundleTemplateInstance *grown =
            (struct BundleTemplateInstance *)realloc(templ->instances,
                                                     (size_t)new_cap * sizeof(struct BundleTemplateInstance));
        if (!grown)
        {
            diag_error("out of memory while storing bundle template instantiation");
            exit(1);
        }
        templ->instances = grown;
        templ->instance_cap = new_cap;
    }

    struct BundleTemplateInstance *slot = &templ->instances[templ->instance_count++];
    memset(slot, 0, sizeof(*slot));
    slot->type_arg_count = arg_count;
    slot->type_args = resolved_args;
    slot->inst_type = inst_type;
    slot->inst_name = inst_name;

    parser_emit_template_bundle_methods(ps,
                                        templ,
                                        inst_type,
                                        resolved_args,
                                        arg_count,
                                        line,
                                        col);

    return inst_type;
}

static void parser_prepend_bundle_this_param(Parser *ps, Node *fn, Type *bundle_type)
{
    (void)ps;
    if (!fn || fn->kind != ND_FUNC || !bundle_type)
        return;

    int old_count = fn->param_count;
    int new_count = old_count + 1;

    Type **new_param_types = (Type **)xcalloc((size_t)new_count, sizeof(Type *));
    const char **new_param_names = (const char **)xcalloc((size_t)new_count, sizeof(char *));
    unsigned char *new_param_const = (unsigned char *)xcalloc((size_t)new_count, sizeof(unsigned char));
    if (!new_param_types || !new_param_names || !new_param_const)
    {
        diag_error("out of memory while adding implicit 'this' parameter");
        exit(1);
    }

    new_param_types[0] = type_ptr(bundle_type);
    new_param_names[0] = xstrdup("this");
    new_param_const[0] = 0;

    for (int i = 0; i < old_count; ++i)
    {
        new_param_types[i + 1] = fn->param_types ? fn->param_types[i] : NULL;
        new_param_names[i + 1] = fn->param_names ? fn->param_names[i] : NULL;
        new_param_const[i + 1] = fn->param_const_flags ? fn->param_const_flags[i] : 0;
    }

    free(fn->param_types);
    free(fn->param_names);
    free(fn->param_const_flags);

    fn->param_types = new_param_types;
    fn->param_names = new_param_names;
    fn->param_const_flags = new_param_const;
    fn->param_count = new_count;
}

struct PendingAttr
{
    char *name;
    char *value;
    int line;
    int col;
};

typedef enum
{
    FN_BODY_NORMAL = 0,
    FN_BODY_CHANCECODE,
    FN_BODY_LITERAL,
} FunctionBodyKind;

static Token expect(Parser *ps, TokenKind k, const char *what);
static Node *new_node(NodeKind k);
static Node *parse_expr(Parser *ps);
static Node *parse_postfix_suffixes(Parser *ps, Node *expr);
static Node *parse_lambda_expr(Parser *ps, Token fun_tok);
static Type *parse_type_spec(Parser *ps);
static Type **parser_parse_type_arg_list(Parser *ps, int *out_count);
static struct BundleTemplate *parser_find_bundle_template(Parser *ps, const char *name, int name_len);
static Type *parser_substitute_template_type(Type *orig, Type **bindings, int binding_count);
static Type *parser_substitute_bundle_method_type(Type *orig,
                                                  Type **bindings,
                                                  int binding_count,
                                                  Type *template_bundle_type,
                                                  Type *inst_bundle_type);
static Node *parser_clone_node_tree_for_bundle_method(const Node *src,
                                                      Type **bindings,
                                                      int binding_count,
                                                      Type *template_bundle_type,
                                                      Type *inst_bundle_type);
static Node *parser_instantiate_bundle_method_template(const Node *fn,
                                                       char *inst_name,
                                                       Type **bindings,
                                                       int binding_count,
                                                       Type *template_bundle_type,
                                                       Type *inst_bundle_type);
static void parser_emit_template_bundle_methods(Parser *ps,
                                                struct BundleTemplate *templ,
                                                Type *inst_type,
                                                Type **bindings,
                                                int binding_count,
                                                int line,
                                                int col);
static Type *parser_instantiate_bundle_template(Parser *ps,
                                                struct BundleTemplate *templ,
                                                Type **type_args,
                                                int type_arg_count,
                                                int line,
                                                int col);
static struct BundleTemplateInstance *parser_find_bundle_template_instance_by_type(Parser *ps,
                                                                                   Type *inst_type,
                                                                                   struct BundleTemplate **out_template);
static Type *named_type_get(Parser *ps, const char *name, int len);
static void named_type_add(Parser *ps, const char *name, int len, Type *ty, int is_exposed);
static int type_sizeof_simple(Type *ty);
static int type_align_simple(Type *ty);
static int align_up(int value, int alignment);
static Node *parse_inferred_var_decl(Parser *ps, int expect_semicolon);
static Node *parse_bundle_this_method(Parser *ps, Token sign_tok, int is_noreturn, int is_exposed, int is_managed);
static Node *parse_bundle_on_method(Parser *ps, Token on_tok, int is_noreturn, int is_exposed, int is_managed);
static char *dup_token_text(Token t);
static int parser_call_type_args_ahead(Parser *ps);
static Type **parser_parse_call_type_args(Parser *ps, int *out_count);
static Type *parser_lookup_generic_param(Parser *ps, const Token *tok);
static TemplateConstraintKind parser_constraint_from_token(const Token *tok);
static void parser_parse_generic_param_options(Parser *ps, TemplateConstraintKind *constraint_kind_out, Type **default_type_out);
static struct GenericParam *parser_push_generic_param(Parser *ps, Token name_tok, int index_within_owner, TemplateConstraintKind constraint_kind, Type *default_type);
static void parser_pop_generic_params(Parser *ps, int count);
static int is_type_start(Parser *ps, Token t);
static int attrs_contains(const struct PendingAttr *attrs, int count, const char *name)
{
    if (!attrs || count <= 0 || !name)
        return 0;
    for (int i = 0; i < count; ++i)
    {
        if (attrs[i].name && strcmp(attrs[i].name, name) == 0)
            return 1;
    }
    return 0;
}
static void chancecode_buffer_append(char **buffer, size_t *capacity, size_t *length, const char *text, size_t text_len);
static void append_string(char ***arr, int *count, int *cap, char *value);
static void parse_chancecode_body(Parser *ps, Node *fn);
static void parse_literal_body(Parser *ps, Node *fn);
static int line_is_blank(const char *text);

static int token_is_varargs(Token tok)
{
    if (tok.kind == TK_ELLIPSIS)
        return 1;
    if (tok.kind == TK_IDENT && tok.length == 8 && strncmp(tok.lexeme, "_vaargs_", 8) == 0)
        return 1;
    return 0;
}

static int token_is_ident_text(Token tok, const char *text)
{
    if (tok.kind != TK_IDENT || !text)
        return 0;
    size_t len = strlen(text);
    if ((size_t)tok.length != len)
        return 0;
    return strncmp(tok.lexeme, text, len) == 0;
}

static char *dup_trimmed(const char *text)
{
    if (!text)
        return NULL;
    const char *start = text;
    while (*start && isspace((unsigned char)*start))
        start++;
    const char *end = start + strlen(start);
    while (end > start && isspace((unsigned char)*(end - 1)))
        --end;
    size_t len = (size_t)(end - start);
    char *copy = (char *)xmalloc(len + 1);
    if (len > 0)
        memcpy(copy, start, len);
    copy[len] = '\0';
    return copy;
}

static int parse_nonnegative_int(const char *text, int *out_value)
{
    if (!text || !*text)
        return 0;
    int value = 0;
    const char *p = text;
    while (*p)
    {
        if (!isdigit((unsigned char)*p))
            return 0;
        value = value * 10 + (*p - '0');
        p++;
    }
    if (out_value)
        *out_value = value;
    return 1;
}

static void pending_attr_cleanup(struct PendingAttr *attr)
{
    if (!attr)
        return;
    free(attr->name);
    free(attr->value);
    attr->name = NULL;
    attr->value = NULL;
    attr->line = 0;
    attr->col = 0;
}

static void clear_pending_attrs(struct PendingAttr *attrs, int count)
{
    if (!attrs || count <= 0)
        return;
    for (int i = 0; i < count; ++i)
        pending_attr_cleanup(&attrs[i]);
}

static const char *attribute_name_from_keyword(TokenKind kind)
{
    switch (kind)
    {
    case TK_KW_OVERRIDEMETADATA:
        return "OverrideMetadata";
    case TK_KW_CHANCECODE:
        return "ChanceCode";
    case TK_KW_LITERAL:
        return "Literal";
    case TK_KW_ENTRYPOINT:
        return "EntryPoint";
    case TK_KW_JUMPTARGET:
        return "JumpTarget";
    case TK_KW_EXPORT:
        return "Export";
    case TK_KW_RAW:
        return "Raw";
    case TK_KW_PRESERVE:
        return "Preserve";
    case TK_KW_SECTION:
        return "Section";
    case TK_KW_FORCEINLINE:
        return "ForceInline";
    case TK_KW_INLINE:
        return "Inline";
    case TK_KW_HINT:
        return "Hint";
    case TK_KW_NOHINT:
        return "NoHint";
    default:
        return NULL;
    }
}

static int token_is_attribute_keyword(TokenKind kind)
{
    return attribute_name_from_keyword(kind) != NULL;
}

static struct PendingAttr parse_attribute(Parser *ps)
{
    struct PendingAttr attr = {0};
    Token open = expect(ps, TK_LBRACKET, "[");
    attr.line = open.line;
    attr.col = open.col;

    Token name_tok = expect(ps, TK_IDENT, "attribute name");
    attr.name = (char *)xmalloc((size_t)name_tok.length + 1);
    memcpy(attr.name, name_tok.lexeme, (size_t)name_tok.length);
    attr.name[name_tok.length] = '\0';

    Token maybe_paren = lexer_peek(ps->lx);
    if (maybe_paren.kind == TK_LPAREN)
    {
        lexer_next(ps->lx);
        Token arg = expect(ps, TK_STRING, "string literal");
        if (arg.length < 2)
        {
            diag_error_at(lexer_source(ps->lx), arg.line, arg.col,
                          "attribute requires string literal argument");
            exit(1);
        }
        int val_len = arg.length - 2;
        attr.value = (char *)xmalloc((size_t)val_len + 1);
        if (val_len > 0)
            memcpy(attr.value, arg.lexeme + 1, (size_t)val_len);
        attr.value[val_len] = '\0';
        expect(ps, TK_RPAREN, ")");
    }
    expect(ps, TK_RBRACKET, "]");
    return attr;
}

static struct PendingAttr parse_attribute_keyword(Parser *ps)
{
    struct PendingAttr attr = {0};
    Token keyword = lexer_next(ps->lx);
    const char *name = attribute_name_from_keyword(keyword.kind);
    if (!name)
    {
        diag_error_at(lexer_source(ps->lx), keyword.line, keyword.col,
                      "internal parser error: token is not an attribute keyword");
        exit(1);
    }

    attr.line = keyword.line;
    attr.col = keyword.col;
    attr.name = xstrdup(name);

    Token maybe_paren = lexer_peek(ps->lx);
    if (maybe_paren.kind == TK_LPAREN)
    {
        lexer_next(ps->lx);
        Token arg = expect(ps, TK_STRING, "string literal");
        if (arg.length < 2)
        {
            diag_error_at(lexer_source(ps->lx), arg.line, arg.col,
                          "attribute requires string literal argument");
            exit(1);
        }
        int val_len = arg.length - 2;
        attr.value = (char *)xmalloc((size_t)val_len + 1);
        if (val_len > 0)
            memcpy(attr.value, arg.lexeme + 1, (size_t)val_len);
        attr.value[val_len] = '\0';
        expect(ps, TK_RPAREN, ")");
    }

    return attr;
}

static void chancecode_buffer_append(char **buffer, size_t *capacity, size_t *length, const char *text, size_t text_len)
{
    if (!buffer || !capacity || !length)
        return;
    size_t needed = *length + text_len + 1;
    if (needed > *capacity)
    {
        size_t new_cap = *capacity ? (*capacity * 2) : 64;
        while (needed > new_cap)
            new_cap *= 2;
        char *resized = (char *)realloc(*buffer, new_cap);
        if (!resized)
        {
            diag_error("out of memory while recording ChanceCode body");
            exit(1);
        }
        *buffer = resized;
        *capacity = new_cap;
    }
    if (text_len > 0 && text)
    {
        memcpy(*buffer + *length, text, text_len);
        *length += text_len;
    }
    (*buffer)[*length] = '\0';
}

static void parse_chancecode_body(Parser *ps, Node *fn)
{
    if (!ps || !fn)
        return;

    expect(ps, TK_LBRACE, "{");
    char **lines = NULL;
    int count = 0;
    int cap = 0;

    for (;;)
    {
        Token peek = lexer_peek(ps->lx);
        if (peek.kind == TK_RBRACE)
        {
            lexer_next(ps->lx);
            break;
        }
        if (peek.kind == TK_EOF)
        {
            diag_error_at(lexer_source(ps->lx), peek.line, peek.col,
                          "unterminated ChanceCode block");
            exit(1);
        }

        char *line_buf = NULL;
        size_t buf_cap = 0;
        size_t buf_len = 0;
        int saw_token = 0;
        const char *prev_end = NULL;

        while (1)
        {
            Token tok = lexer_peek(ps->lx);
            if (tok.kind == TK_SEMI)
            {
                lexer_next(ps->lx);
                break;
            }
            if (tok.kind == TK_RBRACE || tok.kind == TK_EOF)
            {
                diag_error_at(lexer_source(ps->lx), tok.line, tok.col,
                              "ChanceCode statements must end with ';'");
                free(line_buf);
                exit(1);
            }

            tok = lexer_next(ps->lx);
            if (prev_end)
            {
                size_t gap = (size_t)(tok.lexeme - prev_end);
                if (gap > 0 && buf_len > 0)
                {
                    const char space = ' ';
                    chancecode_buffer_append(&line_buf, &buf_cap, &buf_len, &space, 1);
                }
            }
            chancecode_buffer_append(&line_buf, &buf_cap, &buf_len, tok.lexeme, (size_t)tok.length);
            prev_end = tok.lexeme + tok.length;
            saw_token = 1;
        }

        if (!saw_token)
        {
            free(line_buf);
            continue;
        }

        char *trimmed = dup_trimmed(line_buf);
        free(line_buf);
        if (trimmed && *trimmed == '\0')
        {
            free(trimmed);
            continue;
        }
        append_string(&lines, &count, &cap, trimmed);
    }

    fn->chancecode.lines = lines;
    fn->chancecode.count = count;
    fn->is_chancecode = 1;
    fn->body = NULL;
}

static int line_is_blank(const char *text)
{
    if (!text)
        return 1;
    while (*text)
    {
        if (!isspace((unsigned char)*text))
            return 0;
        ++text;
    }
    return 1;
}

static void parse_literal_body(Parser *ps, Node *fn)
{
    if (!ps || !fn)
        return;

    Token open = expect(ps, TK_LBRACE, "{");
    char *raw = NULL;
    if (!lexer_collect_literal_block(ps->lx, &raw))
    {
        diag_error_at(lexer_source(ps->lx), open.line, open.col,
                      "failed to parse literal body");
        exit(1);
    }

    char **lines = NULL;
    int count = 0;
    int cap = 0;
    char *cursor = raw;
    while (cursor && *cursor)
    {
        char *line_start = cursor;
        while (*cursor && *cursor != '\n')
            ++cursor;
        size_t len = (size_t)(cursor - line_start);
        while (len > 0 && line_start[len - 1] == '\r')
            --len;
        char *line = (char *)xmalloc(len + 1);
        if (len > 0)
            memcpy(line, line_start, len);
        line[len] = '\0';
        append_string(&lines, &count, &cap, line);
        if (*cursor == '\n')
            ++cursor;
    }
    free(raw);

    int start = 0;
    while (start < count && line_is_blank(lines[start]))
    {
        free(lines[start]);
        ++start;
    }
    int end = count;
    while (end > start && line_is_blank(lines[end - 1]))
    {
        free(lines[end - 1]);
        --end;
    }
    int new_count = end - start;
    if (new_count <= 0)
    {
        free(lines);
        diag_error_at(lexer_source(ps->lx), open.line, open.col,
                      "Literal body must contain at least one line of code");
        exit(1);
    }
    if (start > 0)
    {
        for (int i = start; i < end; ++i)
            lines[i - start] = lines[i];
    }
    fn->literal.lines = lines;
    fn->literal.count = new_count;
    fn->is_literal = 1;
    fn->body = NULL;
}

static void apply_override_metadata(Parser *ps, Node *fn, const struct PendingAttr *attr)
{
    if (!fn || fn->kind != ND_FUNC || !attr || !attr->value)
        return;

    char *line = dup_trimmed(attr->value);
    if (!line)
        return;
    const char *cursor = line;
    if (strncmp(cursor, ".func", 5) == 0 && (cursor[5] == '\0' || isspace((unsigned char)cursor[5])))
    {
        if (fn->metadata.func_line)
        {
            diag_error_at(lexer_source(ps->lx), attr->line, attr->col,
                          "duplicate '.func' override metadata");
            exit(1);
        }
        cursor += 5;
        while (isspace((unsigned char)*cursor))
            cursor++;
        if (!*cursor)
        {
            diag_error_at(lexer_source(ps->lx), attr->line, attr->col,
                          "'.func' metadata requires function name");
            exit(1);
        }
        const char *name_start = cursor;
        while (*cursor && !isspace((unsigned char)*cursor))
            cursor++;
        size_t name_len = (size_t)(cursor - name_start);
        if (name_len == 0)
        {
            diag_error_at(lexer_source(ps->lx), attr->line, attr->col,
                          "'.func' metadata requires function name");
            exit(1);
        }
        char *backend = (char *)xmalloc(name_len + 1);
        memcpy(backend, name_start, name_len);
        backend[name_len] = '\0';
        fn->metadata.func_line = line;
        fn->metadata.backend_name = backend;

        fn->metadata.declared_param_count = -1;
        fn->metadata.declared_local_count = -1;
        free(fn->metadata.ret_token);
        fn->metadata.ret_token = NULL;

        while (*cursor)
        {
            while (isspace((unsigned char)*cursor))
                cursor++;
            if (!*cursor)
                break;

            const char *token_start = cursor;
            while (*cursor && !isspace((unsigned char)*cursor))
                cursor++;
            size_t token_len = (size_t)(cursor - token_start);
            if (token_len == 0)
                continue;

            const char *eq_pos = (const char *)memchr(token_start, '=', token_len);
            if (!eq_pos)
            {
                if (token_len == 7 && strncmp(token_start, "varargs", 7) == 0)
                    fn->is_varargs = 1;
                else if ((token_len == 9 && strncmp(token_start, "no-return", 9) == 0) ||
                         (token_len == 8 && strncmp(token_start, "noreturn", 8) == 0))
                    fn->is_noreturn = 1;
                continue;
            }

            size_t key_len = (size_t)(eq_pos - token_start);
            const char *value_start = eq_pos + 1;
            size_t value_len = token_len - key_len - 1;
            if (value_len == 0)
                continue;

            if (key_len == 3 && strncmp(token_start, "ret", 3) == 0)
            {
                char *ret = (char *)xmalloc(value_len + 1);
                memcpy(ret, value_start, value_len);
                ret[value_len] = '\0';
                free(fn->metadata.ret_token);
                fn->metadata.ret_token = ret;
            }
            else if (key_len == 6 && strncmp(token_start, "params", 6) == 0)
            {
                char buffer[32];
                if (value_len >= sizeof(buffer))
                {
                    diag_error_at(lexer_source(ps->lx), attr->line, attr->col,
                                  "'.func' params value too large");
                    exit(1);
                }
                memcpy(buffer, value_start, value_len);
                buffer[value_len] = '\0';
                int parsed = 0;
                if (!parse_nonnegative_int(buffer, &parsed))
                {
                    diag_error_at(lexer_source(ps->lx), attr->line, attr->col,
                                  "'.func' params value must be non-negative integer");
                    exit(1);
                }
                fn->metadata.declared_param_count = parsed;
                if (parsed != fn->param_count)
                {
                    diag_error_at(lexer_source(ps->lx), attr->line, attr->col,
                                  "'.func' params value (%d) does not match function parameter count (%d)",
                                  parsed, fn->param_count);
                    exit(1);
                }
            }
            else if (key_len == 6 && strncmp(token_start, "locals", 6) == 0)
            {
                char buffer[32];
                if (value_len >= sizeof(buffer))
                {
                    diag_error_at(lexer_source(ps->lx), attr->line, attr->col,
                                  "'.func' locals value too large");
                    exit(1);
                }
                memcpy(buffer, value_start, value_len);
                buffer[value_len] = '\0';
                int parsed = 0;
                if (!parse_nonnegative_int(buffer, &parsed))
                {
                    diag_error_at(lexer_source(ps->lx), attr->line, attr->col,
                                  "'.func' locals value must be non-negative integer");
                    exit(1);
                }
                fn->metadata.declared_local_count = parsed;
            }
        }
        if (fn->metadata.declared_param_count == -1)
            fn->metadata.declared_param_count = fn->param_count;
        if (fn->metadata.declared_local_count == -1)
            fn->metadata.declared_local_count = 0;
        return;
    }

    if (strncmp(cursor, ".params", 7) == 0 && (cursor[7] == '\0' || isspace((unsigned char)cursor[7])))
    {
        if (fn->metadata.params_line)
        {
            diag_error_at(lexer_source(ps->lx), attr->line, attr->col,
                          "duplicate '.params' override metadata");
            exit(1);
        }
        cursor += 7;
        while (isspace((unsigned char)*cursor))
            cursor++;
        char **tokens = NULL;
        int count = 0;
        int cap = 0;
        while (*cursor)
        {
            const char *start = cursor;
            while (*cursor && !isspace((unsigned char)*cursor))
                cursor++;
            size_t len = (size_t)(cursor - start);
            if (len > 0)
            {
                char *tok = (char *)xmalloc(len + 1);
                memcpy(tok, start, len);
                tok[len] = '\0';
                if (count == cap)
                {
                    int new_cap = cap ? cap * 2 : 4;
                    char **new_tokens = (char **)realloc(tokens, (size_t)new_cap * sizeof(char *));
                    if (!new_tokens)
                    {
                        diag_error("out of memory while parsing .params metadata");
                        exit(1);
                    }
                    tokens = new_tokens;
                    cap = new_cap;
                }
                tokens[count++] = tok;
            }
            while (isspace((unsigned char)*cursor))
                cursor++;
        }
        fn->metadata.params_line = line;
        int varargs_tokens = 0;
        int varargs_index = -1;
        for (int i = 0; i < count; ++i)
        {
            if (tokens[i] && (strcmp(tokens[i], "...") == 0 || strcmp(tokens[i], "_vaargs_") == 0))
            {
                varargs_tokens++;
                varargs_index = i;
            }
        }
        if (varargs_tokens > 0)
        {
            if (varargs_tokens > 1 || varargs_index != count - 1)
            {
                diag_error_at(lexer_source(ps->lx), attr->line, attr->col,
                              "'.params' varargs ('...') must appear once at the end");
                exit(1);
            }
            if (count == 1)
            {
                diag_error_at(lexer_source(ps->lx), attr->line, attr->col,
                              "variadic function must have at least one explicit parameter before '...'");
                exit(1);
            }
            fn->is_varargs = 1;
            free(tokens[count - 1]);
            tokens[count - 1] = NULL;
            count--;
        }

        fn->metadata.param_type_names = tokens;
        fn->metadata.param_type_count = count;
        if (fn->param_count != count)
        {
            diag_error_at(lexer_source(ps->lx), attr->line, attr->col,
                          "'.params' metadata count (%d) does not match function parameter count (%d)",
                          count, fn->param_count);
            exit(1);
        }
        return;
    }

    if (strncmp(cursor, ".locals", 7) == 0 && (cursor[7] == '\0' || isspace((unsigned char)cursor[7])))
    {
        if (fn->metadata.locals_line)
        {
            diag_error_at(lexer_source(ps->lx), attr->line, attr->col,
                          "duplicate '.locals' override metadata");
            exit(1);
        }
        fn->metadata.locals_line = line;
        return;
    }

    diag_error_at(lexer_source(ps->lx), attr->line, attr->col,
                  "unknown override metadata directive '%s'", attr->value);
    exit(1);
}

static char *make_raw_export_backend_name(const char *base_name)
{
    if (!base_name || !*base_name)
        return NULL;
    size_t prefix_len = strlen(RAW_EXPORT_PREFIX);
    size_t base_len = strlen(base_name);
    char *out = (char *)xmalloc(prefix_len + base_len + 1);
    memcpy(out, RAW_EXPORT_PREFIX, prefix_len);
    memcpy(out + prefix_len, base_name, base_len);
    out[prefix_len + base_len] = '\0';
    return out;
}

static void apply_function_attributes(Parser *ps, Node *fn, struct PendingAttr *attrs, int attr_count)
{
    if (!attrs || attr_count <= 0)
        return;
    for (int i = 0; i < attr_count; ++i)
    {
        struct PendingAttr *attr = &attrs[i];
        if (!attr->name)
            continue;
        if (strcmp(attr->name, "OverrideMetadata") == 0)
        {
            apply_override_metadata(ps, fn, attr);
        }
        else if (strcmp(attr->name, "ChanceCode") == 0)
        {
            fn->is_chancecode = 1;
        }
        else if (strcmp(attr->name, "Literal") == 0)
        {
            if (attr->value && *attr->value)
            {
                diag_error_at(lexer_source(ps->lx), attr->line, attr->col,
                              "'Literal' attribute does not take arguments");
                exit(1);
            }
            if (!fn->is_literal)
            {
                diag_error_at(lexer_source(ps->lx), attr->line, attr->col,
                              "'Literal' attribute requires a literal function body");
                exit(1);
            }
            fn->is_literal = 1;
        }
        else if (strcmp(attr->name, "EntryPoint") == 0)
        {
            fn->is_entrypoint = 1;
            fn->is_preserve = 1;
        }
        else if (strcmp(attr->name, "JumpTarget") == 0)
        {
            fn->is_jump_target = 1;
        }
        else if (strcmp(attr->name, "Export") == 0)
        {
            fn->export_name = 1;
        }
        else if (strcmp(attr->name, "Raw") == 0)
        {
            if (attr->value && *attr->value)
            {
                diag_error_at(lexer_source(ps->lx), attr->line, attr->col,
                              "'Raw' attribute does not take arguments");
                exit(1);
            }
            fn->raw_export_name = 1;
        }
        else if (strcmp(attr->name, "Preserve") == 0)
        {
            fn->is_preserve = 1;
        }
        else if (strcmp(attr->name, "Section") == 0)
        {
            if (!attr->value || !*attr->value)
            {
                diag_error_at(lexer_source(ps->lx), attr->line, attr->col,
                              "'Section' attribute requires a string literal argument");
                exit(1);
            }
            if (fn->section_name)
            {
                diag_error_at(lexer_source(ps->lx), attr->line, attr->col,
                              "duplicate 'Section' attribute");
                exit(1);
            }
            fn->section_name = xstrdup(attr->value);
        }
        else if (strcmp(attr->name, "ForceInline") == 0)
        {
            if (!fn->is_literal)
            {
                diag_error_at(lexer_source(ps->lx), attr->line, attr->col,
                              "'ForceInline' attribute requires a literal function body");
                exit(1);
            }
            fn->force_inline_literal = 1;
        }
        else if (strcmp(attr->name, "Inline") == 0)
        {
            fn->wants_inline = 1;
        }
        else if (strcmp(attr->name, "Hint") == 0)
        {
            

            continue;
        }
        else if (strcmp(attr->name, "NoHint") == 0)
        {
            
            continue;
        }
        else
        {
            diag_error_at(lexer_source(ps->lx), attr->line, attr->col,
                          "unknown attribute '%s'", attr->name);
            exit(1);
        }
    }

    if (fn->raw_export_name && !fn->export_name)
    {
        diag_error_at(lexer_source(ps->lx), fn->line, fn->col,
                      "'Raw' is only valid together with 'Export'");
        exit(1);
    }

    if (fn->raw_export_name)
    {
        const char *raw_base = (fn->metadata.backend_name && fn->metadata.backend_name[0])
                                   ? fn->metadata.backend_name
                                   : fn->name;
        if (!raw_base || !*raw_base)
        {
            diag_error_at(lexer_source(ps->lx), fn->line, fn->col,
                          "raw export function is missing a symbol name");
            exit(1);
        }
        if (strncmp(raw_base, RAW_EXPORT_PREFIX, strlen(RAW_EXPORT_PREFIX)) != 0)
        {
            char *raw_backend = make_raw_export_backend_name(raw_base);
            free(fn->metadata.backend_name);
            fn->metadata.backend_name = raw_backend;
        }
    }
}

static void apply_global_attributes(Parser *ps, Node *decl, struct PendingAttr *attrs, int attr_count)
{
    if (!decl || !attrs || attr_count <= 0)
        return;
    for (int i = 0; i < attr_count; ++i)
    {
        struct PendingAttr *attr = &attrs[i];
        if (!attr->name)
            continue;
        if (strcmp(attr->name, "Section") == 0)
        {
            if (!attr->value || !*attr->value)
            {
                diag_error_at(lexer_source(ps->lx), attr->line, attr->col,
                              "'Section' attribute requires a string literal argument");
                exit(1);
            }
            if (decl->section_name)
            {
                diag_error_at(lexer_source(ps->lx), attr->line, attr->col,
                              "duplicate 'Section' attribute");
                exit(1);
            }
            decl->section_name = xstrdup(attr->value);
            continue;
        }

        if (strcmp(attr->name, "Export") == 0)
        {
            decl->export_name = 1;
            continue;
        }

        if (strcmp(attr->name, "Raw") == 0)
        {
            if (attr->value && *attr->value)
            {
                diag_error_at(lexer_source(ps->lx), attr->line, attr->col,
                              "'Raw' attribute does not take arguments");
                exit(1);
            }
            decl->raw_export_name = 1;
            continue;
        }

        diag_error_at(lexer_source(ps->lx), attr->line, attr->col,
                      "attribute '%s' is not supported on global variables", attr->name);
        exit(1);
    }

    if (decl->raw_export_name && !decl->export_name)
    {
        diag_error_at(lexer_source(ps->lx), decl->line, decl->col,
                      "'Raw' is only valid together with 'Export'");
        exit(1);
    }

    if (decl->raw_export_name)
    {
        const char *raw_base = (decl->metadata.backend_name && decl->metadata.backend_name[0])
                                   ? decl->metadata.backend_name
                                   : decl->var_name;
        if (!raw_base || !*raw_base)
        {
            diag_error_at(lexer_source(ps->lx), decl->line, decl->col,
                          "raw export global is missing a symbol name");
            exit(1);
        }
        if (strncmp(raw_base, RAW_EXPORT_PREFIX, strlen(RAW_EXPORT_PREFIX)) != 0)
        {
            char *raw_backend = make_raw_export_backend_name(raw_base);
            free(decl->metadata.backend_name);
            decl->metadata.backend_name = raw_backend;
        }
    }
}

static Node *parse_metadata_call(Parser *ps, Token open)
{
    Token name_tok = expect(ps, TK_IDENT, "metadata call directive");
    if (!(name_tok.length == 12 && strncmp(name_tok.lexeme, "MetadataCall", 12) == 0))
    {
        diag_error_at(lexer_source(ps->lx), name_tok.line, name_tok.col,
                      "unknown metadata expression '%.*s'", name_tok.length, name_tok.lexeme);
        exit(1);
    }

    expect(ps, TK_LPAREN, "(");
    Token target_tok = expect(ps, TK_STRING, "metadata call target");
    if (target_tok.length < 2)
    {
        diag_error_at(lexer_source(ps->lx), target_tok.line, target_tok.col,
                      "metadata call target must be a string literal");
        exit(1);
    }
    int name_len = target_tok.length - 2;
    char *target = (char *)xmalloc((size_t)name_len + 1);
    if (name_len > 0)
        memcpy(target, target_tok.lexeme + 1, (size_t)name_len);
    target[name_len] = '\0';
    expect(ps, TK_RPAREN, ")");
    expect(ps, TK_RBRACKET, "]");

    expect(ps, TK_LPAREN, "(");
    Node **args = NULL;
    int argc = 0;
    int cap = 0;
    Token peek = lexer_peek(ps->lx);
    if (peek.kind != TK_RPAREN)
    {
        for (;;)
        {
            Node *arg = parse_expr(ps);
            if (argc == cap)
            {
                int new_cap = cap ? cap * 2 : 4;
                Node **resized = (Node **)realloc(args, (size_t)new_cap * sizeof(Node *));
                if (!resized)
                {
                    diag_error("out of memory while parsing metadata call arguments");
                    exit(1);
                }
                args = resized;
                cap = new_cap;
            }
            args[argc++] = arg;
            Token comma = lexer_peek(ps->lx);
            if (comma.kind == TK_COMMA)
            {
                lexer_next(ps->lx);
                continue;
            }
            break;
        }
    }
    expect(ps, TK_RPAREN, ")");

    Node *call = new_node(ND_CALL);
    call->call_name = target;
    call->args = args;
    call->arg_count = argc;
    call->line = open.line;
    call->col = open.col;
    call->src = lexer_source(ps->lx);
    return call;
}

static int parse_extend_decl(Parser *ps, int leading_noreturn);

static Node *new_node(NodeKind k)
{
    Node *n = (Node *)xcalloc(1, sizeof(Node));
    n->kind = k;
    n->line = 0;
    n->col = 0;
    n->src = NULL;
    return n;
}

static Node *make_string_node_from_token(Parser *ps, Token t)
{
    Node *n = new_node(ND_STRING);
    if (t.length >= 2)
    {
        n->str_data = t.lexeme + 1;
        n->str_len = t.length - 2;
    }
    else
    {
        n->str_data = "";
        n->str_len = 0;
    }
    n->line = t.line;
    n->col = t.col;
    n->src = lexer_source(ps->lx);
    return n;
}

static char *dup_token_text(Token t)
{
    char *nm = (char *)xmalloc((size_t)t.length + 1);
    memcpy(nm, t.lexeme, (size_t)t.length);
    nm[t.length] = '\0';
    return nm;
}

static char *call_name_from_expr(const Node *expr)
{
    if (!expr)
        return NULL;
    if (expr->kind == ND_VAR)
    {
        if (!expr->var_ref)
            return NULL;
        return xstrdup(expr->var_ref);
    }
    if (expr->kind == ND_MEMBER)
    {
        if (expr->is_pointer_deref)
            return NULL;
        if (!expr->field_name)
            return NULL;
        char *base = call_name_from_expr(expr->lhs);
        if (!base)
            return NULL;
        size_t base_len = strlen(base);
        size_t field_len = strlen(expr->field_name);
        char *res = (char *)xmalloc(base_len + 1 + field_len + 1);
        memcpy(res, base, base_len);
        res[base_len] = '.';
        memcpy(res + base_len + 1, expr->field_name, field_len);
        res[base_len + 1 + field_len] = '\0';
        free(base);
        return res;
    }
    return NULL;
}

static void append_string(char ***arr, int *count, int *cap, char *value)
{
    if (!arr || !count || !cap)
        return;
    if (*count == *cap)
    {
        int new_cap = *cap ? (*cap * 2) : 4;
        char **resized = (char **)realloc(*arr, (size_t)new_cap * sizeof(char *));
        if (!resized)
        {
            diag_error("out of memory while growing string array");
            exit(1);
        }
        *arr = resized;
        *cap = new_cap;
    }
    (*arr)[(*count)++] = value;
}

static char *join_parts_with_dot(char **parts, int count)
{
    if (!parts || count <= 0)
        return NULL;
    size_t total = 0;
    for (int i = 0; i < count; ++i)
    {
        if (parts[i])
            total += strlen(parts[i]);
        if (i + 1 < count)
            total += 1; 
    }
    char *res = (char *)xmalloc(total + 1);
    size_t pos = 0;
    for (int i = 0; i < count; ++i)
    {
        if (parts[i])
        {
            size_t len = strlen(parts[i]);
            memcpy(res + pos, parts[i], len);
            pos += len;
        }
        if (i + 1 < count)
            res[pos++] = '.';
    }
    res[pos] = '\0';
    return res;
}

static void parse_module_path(Parser *ps, char ***out_parts, int *out_count, char **out_full_name)
{
    if (!ps)
        return;
    int cap = 0;
    int count = 0;
    char **parts = NULL;

    Token part = expect(ps, TK_IDENT, "identifier");
    append_string(&parts, &count, &cap, dup_token_text(part));

    while (1)
    {
        Token dot = lexer_peek(ps->lx);
        if (dot.kind != TK_DOT)
            break;
        lexer_next(ps->lx);
        Token next = expect(ps, TK_IDENT, "identifier");
        append_string(&parts, &count, &cap, dup_token_text(next));
    }

    if (out_parts)
        *out_parts = parts;
    else
        free(parts);
    if (out_count)
        *out_count = count;
    if (out_full_name)
        *out_full_name = join_parts_with_dot(parts, count);
}

static void parse_module_decl(Parser *ps, int managed_mode)
{
    Token module_tok = expect(ps, TK_KW_MODULE, "module");
    if (ps->module_full_name)
    {
        diag_error_at(lexer_source(ps->lx), module_tok.line, module_tok.col, "module already declared as '%s'", ps->module_full_name);
        exit(1);
    }
    char **parts = NULL;
    int count = 0;
    char *full = NULL;
    parse_module_path(ps, &parts, &count, &full);
    expect(ps, TK_SEMI, ";");
    ps->module_parts = parts;
    ps->module_part_count = count;
    ps->module_part_cap = count;
    ps->module_full_name = full;
    ps->module_is_managed = managed_mode ? 1 : 0;
}

static int parse_bring_decl(Parser *ps)
{
    Token bring_tok = expect(ps, TK_KW_BRING, "bring");
    char **parts = NULL;
    int count = 0;
    char *full = NULL;
    parse_module_path(ps, &parts, &count, &full);
    char *alias = NULL;
    Token maybe_as = lexer_peek(ps->lx);
    if (maybe_as.kind == TK_KW_AS)
    {
        lexer_next(ps->lx);
        Token alias_tok = expect(ps, TK_IDENT, "module alias");
        alias = (char *)xmalloc((size_t)alias_tok.length + 1);
        memcpy(alias, alias_tok.lexeme, (size_t)alias_tok.length);
        alias[alias_tok.length] = '\0';
    }
    expect(ps, TK_SEMI, ";");

    if (ps->import_count >= ps->import_cap)
    {
        ps->import_cap = ps->import_cap ? ps->import_cap * 2 : 4;
        ps->imports = (struct ModuleImport *)realloc(ps->imports, (size_t)ps->import_cap * sizeof(struct ModuleImport));
        if (!ps->imports)
        {
            diag_error("out of memory while recording module import");
            exit(1);
        }
    }
    ps->imports[ps->import_count].parts = parts;
    ps->imports[ps->import_count].part_count = count;
    ps->imports[ps->import_count].full_name = full;
    ps->imports[ps->import_count].alias = alias;
    ps->import_count++;
    return bring_tok.line;
}

static Token expect(Parser *ps, TokenKind k, const char *what)
{
    Token t = lexer_next(ps->lx);
    if (t.kind != k)
    {
        diag_error_at(lexer_source(ps->lx), t.line, t.col,
                      "expected %s, got token kind=%d", what, t.kind);
        exit(1);
    }
    return t;
}


static Node *parse_expr(Parser *ps);
static Node *parse_stmt(Parser *ps);
static Node *parse_block(Parser *ps);
static Node *parse_try_stmt(Parser *ps);
static Node *parse_while(Parser *ps);
static Node *parse_for(Parser *ps);
static Node *parse_foreach(Parser *ps);
static Node *parse_switch(Parser *ps);
static Node *parse_match_expr(Parser *ps, Token match_tok);
static void parse_alias_decl(Parser *ps, int is_exposed);
static Node *parse_unary(Parser *ps);
static Node *parse_rel(Parser *ps);
static Node *parse_and(Parser *ps);
static Node *parse_eq(Parser *ps);
static Node *parse_or(Parser *ps);
static Node *parse_shift(Parser *ps);
static Node *parse_brace_initializer(Parser *ps);
static int parser_peek_struct_literal(Parser *ps, Token look);

static Node *parse_initializer(Parser *ps);
static void parse_struct_decl(Parser *ps, int is_exposed, int is_union, int is_packed);
static void parse_bundle_decl(Parser *ps, int is_exposed, Node ***decls, int *decl_count, int *decl_cap, int *fn_count);
static void parse_enum_decl(Parser *ps, int is_exposed);
static void parse_module_decl(Parser *ps, int managed_mode);
static int parse_bring_decl(Parser *ps);
static int parse_extend_decl(Parser *ps, int leading_noreturn);
static Type *parse_inline_union_type(Parser *ps);
static int struct_packed_from_attributes(Parser *ps, struct PendingAttr *attrs, int attr_count, int is_union);
static const struct BundleStaticMember *parser_find_bundle_static_member(Parser *ps, const char *bundle_name, int bundle_name_len, const char *member_name, int member_name_len);
static char *parser_serialize_struct_field_default(Parser *ps, const Type *field_type, Node *expr);

static int struct_packed_from_attributes(Parser *ps, struct PendingAttr *attrs, int attr_count, int is_union)
{
    int packed = 0;
    if (!attrs || attr_count <= 0)
        return 0;
    for (int i = 0; i < attr_count; ++i)
    {
        struct PendingAttr *attr = &attrs[i];
        if (!attr->name)
            continue;
        if (strcmp(attr->name, "Packed") == 0)
        {
            if (attr->value && *attr->value)
            {
                diag_error_at(lexer_source(ps->lx), attr->line, attr->col,
                              "'Packed' attribute does not take arguments");
                exit(1);
            }
            packed = 1;
            continue;
        }
        diag_error_at(lexer_source(ps->lx), attr->line, attr->col,
                      "attribute '%s' is not supported on %s declarations",
                      attr->name, is_union ? "union" : "struct");
        exit(1);
    }
    return packed;
}

static Node *parser_make_var_ref(Parser *ps, const char *name, int line, int col)
{
    Node *n = new_node(ND_VAR);
    n->var_ref = name;
    n->line = line;
    n->col = col;
    n->src = lexer_source(ps->lx);
    return n;
}

static Node *parser_make_call0(Parser *ps, const char *name, int line, int col)
{
    Node *call = new_node(ND_CALL);
    call->call_name = name;
    call->args = NULL;
    call->arg_count = 0;
    call->line = line;
    call->col = col;
    call->src = lexer_source(ps->lx);
    return call;
}

static Node *parser_make_member(Parser *ps, Node *base, const char *field, int line, int col)
{
    Node *m = new_node(ND_MEMBER);
    m->lhs = base;
    m->field_name = field;
    m->line = line;
    m->col = col;
    m->src = lexer_source(ps->lx);
    return m;
}

static Node *parser_make_assign_stmt(Parser *ps, Node *lhs, Node *rhs, int line, int col)
{
    Node *as = new_node(ND_ASSIGN);
    as->lhs = lhs;
    as->rhs = rhs;
    as->line = line;
    as->col = col;
    as->src = lexer_source(ps->lx);

    Node *es = new_node(ND_EXPR_STMT);
    es->lhs = as;
    es->line = line;
    es->col = col;
    es->src = lexer_source(ps->lx);
    return es;
}

static void parser_try_prepend_stmt(Node *block, Node *stmt)
{
    if (!block || !stmt || block->kind != ND_BLOCK)
        return;
    int old = block->stmt_count;
    Node **grown = (Node **)realloc(block->stmts, (size_t)(old + 1) * sizeof(Node *));
    if (!grown)
    {
        diag_error("out of memory while preparing catch block");
        exit(1);
    }
    block->stmts = grown;
    for (int i = old; i > 0; --i)
        block->stmts[i] = block->stmts[i - 1];
    block->stmts[0] = stmt;
    block->stmt_count = old + 1;
}

static void parser_try_append_stmt(Node *block, Node *stmt)
{
    if (!block || !stmt || block->kind != ND_BLOCK)
        return;
    int old = block->stmt_count;
    Node **grown = (Node **)realloc(block->stmts, (size_t)(old + 1) * sizeof(Node *));
    if (!grown)
    {
        diag_error("out of memory while preparing catch block");
        exit(1);
    }
    block->stmts = grown;
    block->stmts[old] = stmt;
    block->stmt_count = old + 1;
}

static Node *parser_make_empty_block(Parser *ps, int line, int col)
{
    Node *b = new_node(ND_BLOCK);
    b->line = line;
    b->col = col;
    b->src = lexer_source(ps->lx);
    return b;
}

static void parser_try_append_block_stmts(Node *dst, Node *src)
{
    if (!dst || !src || dst->kind != ND_BLOCK || src->kind != ND_BLOCK)
        return;
    for (int i = 0; i < src->stmt_count; ++i)
        parser_try_append_stmt(dst, src->stmts[i]);
}

static Node *parser_make_bool_lit(Parser *ps, int value, int line, int col)
{
    Node *n = new_node(ND_INT);
    n->int_val = value ? 1 : 0;
    n->int_uval = (uint64_t)(value ? 1 : 0);
    n->int_is_unsigned = 0;
    n->int_width = 0;
    n->line = line;
    n->col = col;
    n->src = lexer_source(ps->lx);
    return n;
}

static char *parser_make_foreach_temp_name(Parser *ps, const char *role)
{
    const char *safe_role = (role && *role) ? role : "tmp";
    int next_id = 1;
    if (ps)
        next_id = ++ps->foreach_temp_counter;
    char buf[96];
    snprintf(buf, sizeof(buf), "__chance_foreach_%s_%d", safe_role, next_id);
    return xstrdup(buf);
}

static Node *parser_make_string_lit(Parser *ps, const char *text, int line, int col)
{
    Node *s = new_node(ND_STRING);
    const char *safe = text ? text : "";
    int len = (int)strlen(safe);
    char *heap = (char *)xmalloc((size_t)len + 1);
    memcpy(heap, safe, (size_t)len + 1);
    s->str_data = heap;
    s->str_len = len;
    s->line = line;
    s->col = col;
    s->src = lexer_source(ps->lx);
    return s;
}

static Node *parser_make_call1(Parser *ps, const char *name, Node *arg0, int line, int col)
{
    Node *call = new_node(ND_CALL);
    call->call_name = name;
    call->args = (Node **)xcalloc(1, sizeof(Node *));
    call->args[0] = arg0;
    call->arg_count = 1;
    call->line = line;
    call->col = col;
    call->src = lexer_source(ps->lx);
    return call;
}

static Node *parser_make_not(Parser *ps, Node *expr, int line, int col)
{
    Node *n = new_node(ND_LNOT);
    n->lhs = expr;
    n->line = line;
    n->col = col;
    n->src = lexer_source(ps->lx);
    return n;
}

static Node *parser_make_land(Parser *ps, Node *lhs, Node *rhs, int line, int col)
{
    Node *n = new_node(ND_LAND);
    n->lhs = lhs;
    n->rhs = rhs;
    n->line = line;
    n->col = col;
    n->src = lexer_source(ps->lx);
    return n;
}

static Node *parser_make_lor(Parser *ps, Node *lhs, Node *rhs, int line, int col)
{
    Node *n = new_node(ND_LOR);
    n->lhs = lhs;
    n->rhs = rhs;
    n->line = line;
    n->col = col;
    n->src = lexer_source(ps->lx);
    return n;
}

static char *parser_catch_type_meta(Type *ty)
{
    if (!ty)
        return xstrdup("RuntimeError");
    if (ty->kind == TY_STRUCT && ty->struct_name)
    {
        size_t len = strlen(ty->struct_name);
        char *s = (char *)xmalloc(len + strlen("struct:") + 1);
        memcpy(s, "struct:", strlen("struct:"));
        memcpy(s + strlen("struct:"), ty->struct_name, len + 1);
        return s;
    }
    if (ty->kind == TY_IMPORT && ty->import_type_name)
    {
        size_t len = strlen(ty->import_type_name);
        char *s = (char *)xmalloc(len + strlen("struct:") + 1);
        memcpy(s, "struct:", strlen("struct:"));
        memcpy(s + strlen("struct:"), ty->import_type_name, len + 1);
        return s;
    }
    return xstrdup("RuntimeError");
}

static Node *parse_try_stmt(Parser *ps)
{
    Token try_tok = expect(ps, TK_KW_TRY, "try");
    Node *try_block = parse_block(ps);

    Node *catch_block = NULL;
    Node *finally_block = NULL;
    Token next = lexer_peek(ps->lx);
    if (next.kind == TK_KW_CATCH)
    {
        Node *dispatch = new_node(ND_BLOCK);
        dispatch->line = try_tok.line;
        dispatch->col = try_tok.col;
        dispatch->src = lexer_source(ps->lx);

        char matched_name_buf[64];
        snprintf(matched_name_buf, sizeof(matched_name_buf), "__catch_matched_%d_%d", try_tok.line, try_tok.col);
        char *matched_name = xstrdup(matched_name_buf);

        Node *matched_decl = new_node(ND_VAR_DECL);
        matched_decl->var_name = matched_name;
        static Type tbool = {.kind = TY_BOOL};
        matched_decl->var_type = &tbool;
        matched_decl->line = try_tok.line;
        matched_decl->col = try_tok.col;
        matched_decl->src = lexer_source(ps->lx);
        matched_decl->rhs = parser_make_bool_lit(ps, 0, try_tok.line, try_tok.col);
        parser_try_append_stmt(dispatch, matched_decl);

        while (next.kind == TK_KW_CATCH)
        {
            lexer_next(ps->lx);
            expect(ps, TK_LPAREN, "(");
            Type *clause_type = parse_type_spec(ps);
            const char *clause_name = NULL;
            Node *clause_guard = NULL;

            Token maybe_ident = lexer_peek(ps->lx);
            if (maybe_ident.kind == TK_IDENT)
            {
                Token nm = lexer_next(ps->lx);
                char *heap = (char *)xmalloc((size_t)nm.length + 1);
                memcpy(heap, nm.lexeme, (size_t)nm.length);
                heap[nm.length] = '\0';
                clause_name = heap;
            }

            Token guard_tok = lexer_peek(ps->lx);
            if (guard_tok.kind == TK_QUESTION)
            {
                Token guard_gt = lexer_peek_n(ps->lx, 1);
                if (guard_gt.kind != TK_GT)
                {
                    diag_error_at(lexer_source(ps->lx), guard_tok.line, guard_tok.col,
                                  "expected '>' after '?' in catch guard; use '?> <expr>'");
                    exit(1);
                }
                lexer_next(ps->lx);
                lexer_next(ps->lx);
                if (!clause_name)
                {
                    diag_error_at(lexer_source(ps->lx), guard_tok.line, guard_tok.col,
                                  "catch guard requires a catch variable name (e.g. catch (T ex ?> ...))");
                    exit(1);
                }
                clause_guard = parse_expr(ps);
            }
            else if (guard_tok.kind == TK_KW_WHERE)
            {
                lexer_next(ps->lx);
                if (!clause_name)
                {
                    diag_error_at(lexer_source(ps->lx), guard_tok.line, guard_tok.col,
                                  "catch guard requires a catch variable name (e.g. catch (T ex where ...))");
                    exit(1);
                }
                clause_guard = parse_expr(ps);
            }

            expect(ps, TK_RPAREN, ")");
            Node *user_block = parse_block(ps);
            Node *clause_block = parser_make_empty_block(ps, try_tok.line, try_tok.col);

            if (clause_name && clause_block)
            {
                Node *decl = new_node(ND_VAR_DECL);
                decl->var_name = clause_name;
                decl->var_type = clause_type;
                decl->line = try_tok.line;
                decl->col = try_tok.col;
                decl->src = lexer_source(ps->lx);
                Node *init = new_node(ND_INIT_LIST);
                init->init.is_zero = 1;
                init->line = try_tok.line;
                init->col = try_tok.col;
                init->src = lexer_source(ps->lx);
                decl->rhs = init;
                parser_try_append_stmt(clause_block, decl);

                parser_try_append_stmt(clause_block, parser_make_assign_stmt(ps,
                    parser_make_member(ps, parser_make_var_ref(ps, clause_name, try_tok.line, try_tok.col), "message", try_tok.line, try_tok.col),
                    parser_make_call0(ps, "__cert__exception_get_message", try_tok.line, try_tok.col), try_tok.line, try_tok.col));
            }

            Node *mark_matched = parser_make_assign_stmt(
                ps,
                parser_make_var_ref(ps, matched_name, try_tok.line, try_tok.col),
                parser_make_bool_lit(ps, 1, try_tok.line, try_tok.col),
                try_tok.line,
                try_tok.col);

            if (clause_guard)
            {
                Node *guard_body = parser_make_empty_block(ps, try_tok.line, try_tok.col);
                parser_try_append_stmt(guard_body, mark_matched);
                parser_try_append_block_stmts(guard_body, user_block);

                Node *guard_if = new_node(ND_IF);
                guard_if->lhs = clause_guard;
                guard_if->rhs = guard_body;
                guard_if->body = NULL;
                guard_if->line = try_tok.line;
                guard_if->col = try_tok.col;
                guard_if->src = lexer_source(ps->lx);
                parser_try_append_stmt(clause_block, guard_if);
            }
            else
            {
                parser_try_append_stmt(clause_block, mark_matched);
                parser_try_append_block_stmts(clause_block, user_block);
            }

            char *meta = parser_catch_type_meta(clause_type);
            Node *match_call = parser_make_call1(ps, "__cert__exception_matches_type", parser_make_string_lit(ps, meta, try_tok.line, try_tok.col), try_tok.line, try_tok.col);
            free(meta);

            const char *catch_type_name = NULL;
            if (clause_type)
            {
                if (clause_type->kind == TY_STRUCT && clause_type->struct_name && clause_type->struct_name[0] != '\0')
                    catch_type_name = clause_type->struct_name;
                else if (clause_type->kind == TY_IMPORT && clause_type->import_type_name && clause_type->import_type_name[0] != '\0')
                    catch_type_name = clause_type->import_type_name;
            }

            if (catch_type_name)
            {
                char alias_exception[256];
                char alias_std_exception[256];
                snprintf(alias_exception, sizeof(alias_exception), "Exception.%s", catch_type_name);
                snprintf(alias_std_exception, sizeof(alias_std_exception), "Std.Exception.%s", catch_type_name);

                Node *alias_match_exception = parser_make_call1(
                    ps,
                    "__cert__exception_matches_type",
                    parser_make_string_lit(ps, alias_exception, try_tok.line, try_tok.col),
                    try_tok.line,
                    try_tok.col);
                Node *alias_match_std_exception = parser_make_call1(
                    ps,
                    "__cert__exception_matches_type",
                    parser_make_string_lit(ps, alias_std_exception, try_tok.line, try_tok.col),
                    try_tok.line,
                    try_tok.col);

                match_call = parser_make_lor(ps, match_call, alias_match_exception, try_tok.line, try_tok.col);
                match_call = parser_make_lor(ps, match_call, alias_match_std_exception, try_tok.line, try_tok.col);
            }

            Node *cond = match_call;
            cond = parser_make_land(ps,
                                    parser_make_not(ps, parser_make_var_ref(ps, matched_name, try_tok.line, try_tok.col), try_tok.line, try_tok.col),
                                    cond,
                                    try_tok.line,
                                    try_tok.col);

            Node *if_stmt = new_node(ND_IF);
            if_stmt->lhs = cond;
            if_stmt->rhs = clause_block;
            if_stmt->body = NULL;
            if_stmt->line = try_tok.line;
            if_stmt->col = try_tok.col;
            if_stmt->src = lexer_source(ps->lx);
            parser_try_append_stmt(dispatch, if_stmt);

            next = lexer_peek(ps->lx);
        }

        Node *throw_unmatched = new_node(ND_THROW);
        throw_unmatched->line = try_tok.line;
        throw_unmatched->col = try_tok.col;
        throw_unmatched->src = lexer_source(ps->lx);

        Node *unmatched_if = new_node(ND_IF);
        unmatched_if->lhs = parser_make_not(ps, parser_make_var_ref(ps, matched_name, try_tok.line, try_tok.col), try_tok.line, try_tok.col);
        unmatched_if->rhs = throw_unmatched;
        unmatched_if->body = NULL;
        unmatched_if->line = try_tok.line;
        unmatched_if->col = try_tok.col;
        unmatched_if->src = lexer_source(ps->lx);
        parser_try_append_stmt(dispatch, unmatched_if);

        catch_block = dispatch;
    }

    next = lexer_peek(ps->lx);
    if (next.kind == TK_KW_FINALLY)
    {
        lexer_next(ps->lx);
        finally_block = parse_block(ps);
    }

    if (!catch_block && !finally_block)
    {
        diag_error_at(lexer_source(ps->lx), try_tok.line, try_tok.col,
                      "try statement requires a catch and/or finally block");
        exit(1);
    }

    Node *n = new_node(ND_TRY);
    n->lhs = try_block;
    n->rhs = catch_block;
    n->body = finally_block;
    n->var_type = NULL;
    n->var_name = NULL;
    n->type_expr = NULL;
    n->line = try_tok.line;
    n->col = try_tok.col;
    n->src = lexer_source(ps->lx);
    return n;
}

static int source_only_preprocessor_between(const SourceBuffer *src, int start_line, int end_line)
{
    if (!src || !src->src || start_line <= 0 || end_line <= 0)
        return 0;
    if (end_line <= start_line + 1)
        return 1;
    const char *data = src->src;
    const char *end = data + src->length;
    int current_line = 1;
    const char *line_start = data;
    for (const char *p = data; p <= end; ++p)
    {
        if (p == end || *p == '\n')
        {
            if (current_line > start_line && current_line < end_line)
            {
                const char *trim = line_start;
                while (trim < p && (*trim == ' ' || *trim == '\t' || *trim == '\r' || *trim == '\f' || *trim == '\v'))
                    ++trim;
                if (trim < p && *trim != '#')
                    return 0;
            }
            if (p == end)
                break;
            line_start = p + 1;
            current_line++;
        }
    }
    return 1;
}

static int alias_find(Parser *ps, const char *name, int len)
{
    for (int i = 0; i < ps->alias_count; i++)
    {
        if (ps->aliases[i].name_len == len &&
            strncmp(ps->aliases[i].name, name, len) == 0)
            return i;
    }
    return -1;
}

static int named_type_find(Parser *ps, const char *name, int len)
{
    for (int i = 0; i < ps->nt_count; i++)
    {
        if (ps->named_types[i].name_len == len && strncmp(ps->named_types[i].name, name, len) == 0)
            return i;
    }
    return -1;
}
static Type *named_type_get(Parser *ps, const char *name, int len)
{
    int i = named_type_find(ps, name, len);
    return i >= 0 ? ps->named_types[i].type : NULL;
}
static void named_type_add(Parser *ps, const char *name, int len, Type *ty, int is_exposed)
{
    if (ps->nt_count == ps->nt_cap)
    {
        ps->nt_cap = ps->nt_cap ? ps->nt_cap * 2 : 8;
        ps->named_types = (struct NamedType *)realloc(ps->named_types, ps->nt_cap * sizeof(*ps->named_types));
    }
    ps->named_types[ps->nt_count].name = (char *)xmalloc((size_t)len + 1);
    memcpy(ps->named_types[ps->nt_count].name, name, (size_t)len);
    ps->named_types[ps->nt_count].name[len] = '\0';
    ps->named_types[ps->nt_count].name_len = len;
    ps->named_types[ps->nt_count].type = ty;
    ps->named_types[ps->nt_count].is_exposed = is_exposed;
    ps->nt_count++;
}
static int enum_const_find(Parser *ps, const char *name, int len)
{
    for (int i = 0; i < ps->ec_count; i++)
        if (ps->enum_consts[i].name_len == len && strncmp(ps->enum_consts[i].name, name, len) == 0)
            return i;
    return -1;
}
static int enum_const_get(Parser *ps, const char *name, int len, int *out)
{
    int i = enum_const_find(ps, name, len);
    if (i >= 0)
    {
        *out = ps->enum_consts[i].value;
        return 1;
    }
    return 0;
}
static void enum_const_add(Parser *ps, const char *name, int len, int value)
{
    if (ps->ec_count == ps->ec_cap)
    {
        ps->ec_cap = ps->ec_cap ? ps->ec_cap * 2 : 8;
        ps->enum_consts = (struct EnumConst *)realloc(ps->enum_consts, ps->ec_cap * sizeof(*ps->enum_consts));
    }
    ps->enum_consts[ps->ec_count].name = (char *)xmalloc((size_t)len + 1);
    memcpy(ps->enum_consts[ps->ec_count].name, name, (size_t)len);
    ps->enum_consts[ps->ec_count].name[len] = '\0';
    ps->enum_consts[ps->ec_count].name_len = len;
    ps->enum_consts[ps->ec_count].value = value;
    ps->ec_count++;
}
static int const_int_get(Parser *ps, const char *name, int len, int *out)
{
    if (!ps || !name || len <= 0)
        return 0;
    for (int i = ps->const_int_count - 1; i >= 0; --i)
    {
        if (ps->const_ints[i].name_len == len &&
            strncmp(ps->const_ints[i].name, name, (size_t)len) == 0)
        {
            if (out)
                *out = ps->const_ints[i].value;
            return 1;
        }
    }
    return 0;
}
static void const_int_add(Parser *ps, const char *name, int len, int value)
{
    if (!ps || !name || len <= 0)
        return;
    if (ps->const_int_count == ps->const_int_cap)
    {
        ps->const_int_cap = ps->const_int_cap ? ps->const_int_cap * 2 : 8;
        ps->const_ints = (struct ConstInt *)realloc(ps->const_ints, ps->const_int_cap * sizeof(struct ConstInt));
        if (!ps->const_ints)
        {
            diag_error("out of memory while tracking constant integers");
            exit(1);
        }
    }
    ps->const_ints[ps->const_int_count].name = (char *)xmalloc((size_t)len + 1);
    memcpy(ps->const_ints[ps->const_int_count].name, name, (size_t)len);
    ps->const_ints[ps->const_int_count].name[len] = '\0';
    ps->const_ints[ps->const_int_count].name_len = len;
    ps->const_ints[ps->const_int_count].value = value;
    ps->const_int_count++;
}
static void const_scope_push(Parser *ps)
{
    if (!ps)
        return;
    if (ps->const_scope_count == ps->const_scope_cap)
    {
        ps->const_scope_cap = ps->const_scope_cap ? ps->const_scope_cap * 2 : 4;
        ps->const_scope_marks = (int *)realloc(ps->const_scope_marks, ps->const_scope_cap * sizeof(int));
        if (!ps->const_scope_marks)
        {
            diag_error("out of memory while tracking constant scopes");
            exit(1);
        }
    }
    ps->const_scope_marks[ps->const_scope_count++] = ps->const_int_count;
}
static void const_scope_pop(Parser *ps)
{
    if (!ps || ps->const_scope_count <= 0)
        return;
    int mark = ps->const_scope_marks[--ps->const_scope_count];
    for (int i = ps->const_int_count - 1; i >= mark; --i)
    {
        free(ps->const_ints[i].name);
        ps->const_ints[i].name = NULL;
        ps->const_ints[i].name_len = 0;
        ps->const_ints[i].value = 0;
    }
    ps->const_int_count = mark;
}
static int parser_eval_const_int(Node *expr, int *out)
{
    if (!expr || !out)
        return 0;
    switch (expr->kind)
    {
    case ND_INT:
        *out = (int)expr->int_val;
        return 1;
    case ND_NEG:
    {
        int v = 0;
        if (!parser_eval_const_int(expr->lhs, &v))
            return 0;
        *out = -v;
        return 1;
    }
    default:
        break;
    }
    return 0;
}
static void parser_maybe_register_const_int(Parser *ps, int is_const, Token name, Node *init)
{
    if (!ps || !is_const || !init)
        return;
    int value = 0;
    if (!parser_eval_const_int(init, &value))
        return;
    const_int_add(ps, name.lexeme, name.length, value);
}
static int enum_type_find(Parser *ps, const char *name, int len)
{
    for (int i = 0; i < ps->et_count; i++)
        if (ps->enum_types[i].name_len == len && strncmp(ps->enum_types[i].name, name, len) == 0)
            return i;
    return -1;
}
static void enum_type_add(Parser *ps, const char *name, int len, int is_exposed)
{
    int existing = enum_type_find(ps, name, len);
    if (existing >= 0)
    {
        if (is_exposed && !ps->enum_types[existing].is_exposed)
            ps->enum_types[existing].is_exposed = 1;
        return;
    }
    if (ps->et_count == ps->et_cap)
    {
        ps->et_cap = ps->et_cap ? ps->et_cap * 2 : 4;
        ps->enum_types = (struct EnumType *)realloc(ps->enum_types, ps->et_cap * sizeof(*ps->enum_types));
    }
    ps->enum_types[ps->et_count].name = (char *)xmalloc((size_t)len + 1);
    memcpy(ps->enum_types[ps->et_count].name, name, (size_t)len);
    ps->enum_types[ps->et_count].name[len] = '\0';
    ps->enum_types[ps->et_count].name_len = len;
    ps->enum_types[ps->et_count].is_exposed = is_exposed;
    ps->et_count++;
}

static Type *make_ptr_chain_dyn(Type *base, int depth)
{
    Type *t = base;
    for (int i = 0; i < depth; i++)
    {
        Type *p = (Type *)xcalloc(1, sizeof(Type));
        p->kind = TY_PTR;
        p->pointee = t;
        t = p;
    }
    return t;
}

static int type_sizeof_simple(Type *ty)
{
    ty = module_registry_canonical_type(ty);
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
    case TY_CHAR:
        return 1;
    case TY_VOID:
        return 0;
    case TY_STRUCT:
        return ty->strct.size_bytes;
    case TY_ARRAY:
        if (!ty->array.elem)
            return 8;
        if (ty->array.is_unsized)
            return 8;
        if (ty->array.length <= 0)
            return 0;
        return ty->array.length * type_sizeof_simple(ty->array.elem);
    default:
        return 8;
    }
}

static int type_align_simple(Type *ty)
{
    ty = module_registry_canonical_type(ty);
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
            return type_align_simple(ty->array.elem);
        return 8;
    case TY_STRUCT:
    {
        if (ty->strct.is_packed)
            return 1;
        int max_align = 1;
        if (!ty->strct.field_types || ty->strct.field_count <= 0)
            return max_align;
        for (int i = 0; i < ty->strct.field_count; ++i)
        {
            int field_align = type_align_simple(ty->strct.field_types[i]);
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

static int align_up(int value, int alignment)
{
    if (alignment <= 0)
        return value;
    int remainder = value % alignment;
    if (remainder == 0)
        return value;
    return value + (alignment - remainder);
}

static int module_path_followed_by_call(Parser *ps)
{
    if (!ps || !ps->lx)
        return 0;
    Token next = lexer_peek_n(ps->lx, 1);
    if (next.kind != TK_DOT)
        return 0;
    int offset = 1;
    for (;;)
    {
        offset++;
        Token ident = lexer_peek_n(ps->lx, offset);
        if (ident.kind != TK_IDENT)
            return 0;
        offset++;
        Token after = lexer_peek_n(ps->lx, offset);
        if (after.kind == TK_DOT)
            continue;
        if (after.kind == TK_LPAREN)
            return 1;
        return 0;
    }
}

static Type *parser_lookup_generic_param(Parser *ps, const Token *tok)
{
    if (!ps || !tok || tok->kind != TK_IDENT || ps->generic_param_count <= 0)
        return NULL;
    for (int i = ps->generic_param_count - 1; i >= 0; --i)
    {
        struct GenericParam *gp = &ps->generic_params[i];
        if (!gp || !gp->name)
            continue;
        if (gp->name_len != tok->length)
            continue;
        if (strncmp(gp->name, tok->lexeme, (size_t)tok->length) != 0)
            continue;
        if (!gp->placeholder)
            gp->placeholder = type_template_param(gp->name, gp->index);
        return gp->placeholder;
    }
    return NULL;
}

static TemplateConstraintKind parser_constraint_from_token(const Token *tok)
{
    if (!tok || tok->kind != TK_IDENT || !tok->lexeme)
        return TEMPLATE_CONSTRAINT_NONE;
    if (tok->length == 8 && strncmp(tok->lexeme, "integral", 8) == 0)
        return TEMPLATE_CONSTRAINT_INTEGRAL;
    if (tok->length == 7 && strncmp(tok->lexeme, "numeric", 7) == 0)
        return TEMPLATE_CONSTRAINT_NUMERIC;
    if (tok->length == 8 && strncmp(tok->lexeme, "floating", 8) == 0)
        return TEMPLATE_CONSTRAINT_FLOATING;
    if (tok->length == 6 && strncmp(tok->lexeme, "double", 6) == 0)
        return TEMPLATE_CONSTRAINT_FLOATING;
    if (tok->length == 7 && strncmp(tok->lexeme, "pointer", 7) == 0)
        return TEMPLATE_CONSTRAINT_POINTER;
    if (tok->length == 3 && strncmp(tok->lexeme, "ptr", 3) == 0)
        return TEMPLATE_CONSTRAINT_POINTER;
    return TEMPLATE_CONSTRAINT_NONE;
}

static void parser_parse_generic_param_options(Parser *ps, TemplateConstraintKind *constraint_kind_out, Type **default_type_out)
{
    if (!ps)
        return;
    if (constraint_kind_out)
        *constraint_kind_out = TEMPLATE_CONSTRAINT_NONE;
    if (default_type_out)
        *default_type_out = NULL;

    Token next = lexer_peek(ps->lx);
    if (next.kind == TK_COLON)
    {
        lexer_next(ps->lx);
        Token after = lexer_peek(ps->lx);
        if (after.kind == TK_IDENT)
        {
            TemplateConstraintKind parsed_kind = parser_constraint_from_token(&after);
            if (parsed_kind != TEMPLATE_CONSTRAINT_NONE)
            {
                lexer_next(ps->lx);
                if (constraint_kind_out)
                    *constraint_kind_out = parsed_kind;
                Token maybe_assign = lexer_peek(ps->lx);
                if (maybe_assign.kind == TK_ASSIGN)
                {
                    lexer_next(ps->lx);
                    if (default_type_out)
                        *default_type_out = parse_type_spec(ps);
                }
                return;
            }
        }
        if (after.kind == TK_KW_CONSTANT || is_type_start(ps, after))
        {
            if (default_type_out)
                *default_type_out = parse_type_spec(ps);
            return;
        }
        if (after.kind == TK_IDENT)
        {
            diag_error_at(lexer_source(ps->lx), after.line, after.col,
                          "unknown generic constraint '%.*s'", after.length, after.lexeme);
            exit(1);
        }
        diag_error_at(lexer_source(ps->lx), after.line, after.col,
                      "expected constraint keyword or type after ':' in generic parameter");
        exit(1);
    }
    if (next.kind == TK_ASSIGN)
    {
        lexer_next(ps->lx);
        if (default_type_out)
            *default_type_out = parse_type_spec(ps);
    }
}

static struct GenericParam *parser_push_generic_param(Parser *ps, Token name_tok, int index_within_owner, TemplateConstraintKind constraint_kind, Type *default_type)
{
    if (!ps)
        return NULL;
    if (ps->generic_param_count == ps->generic_param_cap)
    {
        int new_cap = ps->generic_param_cap ? ps->generic_param_cap * 2 : 4;
        struct GenericParam *grown = (struct GenericParam *)realloc(ps->generic_params, (size_t)new_cap * sizeof(struct GenericParam));
        if (!grown)
        {
            diag_error("out of memory while tracking generic parameters");
            exit(1);
        }
        ps->generic_params = grown;
        ps->generic_param_cap = new_cap;
    }
    struct GenericParam *gp = &ps->generic_params[ps->generic_param_count++];
    memset(gp, 0, sizeof(*gp));
    gp->name = dup_token_text(name_tok);
    gp->name_len = name_tok.length;
    gp->index = index_within_owner;
    gp->constraint_kind = constraint_kind;
    gp->default_type = default_type;
    gp->placeholder = type_template_param(gp->name, gp->index);
    if (gp->placeholder)
    {
        gp->placeholder->template_constraint_kind = constraint_kind;
        gp->placeholder->template_default_type = default_type;
    }
    return gp;
}

static void parser_pop_generic_params(Parser *ps, int count)
{
    if (!ps || count <= 0)
        return;
    while (count-- > 0 && ps->generic_param_count > 0)
    {
        struct GenericParam *gp = &ps->generic_params[ps->generic_param_count - 1];
        free(gp->name);
        gp->name = NULL;
        gp->placeholder = NULL;
        gp->constraint_kind = TEMPLATE_CONSTRAINT_NONE;
        gp->default_type = NULL;
        gp->name_len = 0;
        gp->index = 0;
        ps->generic_param_count--;
    }
}

static Type *parser_make_deferred_import_type(Parser *ps, const Token *tok)
{
    if (!tok || tok->kind != TK_IDENT || tok->length <= 0)
        return NULL;

    char *type_name = (char *)xmalloc((size_t)tok->length + 1);
    memcpy(type_name, tok->lexeme, (size_t)tok->length);
    type_name[tok->length] = '\0';

    Type *imp_type = (Type *)xcalloc(1, sizeof(Type));
    imp_type->kind = TY_IMPORT;
    imp_type->struct_name = type_name;
    imp_type->import_type_name = type_name;

    if (ps && ps->module_full_name && ps->module_full_name[0] != '\0')
        imp_type->import_module = xstrdup(ps->module_full_name);
    else
        imp_type->import_module = xstrdup("");

    return imp_type;
}

static int is_type_start(Parser *ps, Token t)
{
    switch (t.kind)
    {
    case TK_KW_CONSTANT:
        return 1;
    case TK_KW_I8:
    case TK_KW_U8:
    case TK_KW_I16:
    case TK_KW_U16:
    case TK_KW_I32:
    case TK_KW_U32:
    case TK_KW_I64:
    case TK_KW_U64:
    case TK_KW_F32:
    case TK_KW_F64:
    case TK_KW_F128:
    case TK_KW_INT:
    case TK_KW_UINT:
    case TK_KW_SHORT:
    case TK_KW_USHORT:
    case TK_KW_LONG:
    case TK_KW_ULONG:
    case TK_KW_BYTE:
    case TK_KW_UBYTE:
    case TK_KW_FLOAT:
    case TK_KW_DOUBLE:
    case TK_KW_VOID:
    case TK_KW_CHAR:
    case TK_KW_STRING:
    case TK_KW_BOOL:
    case TK_KW_STACK:
    case TK_KW_ACTION:
        return 1;
    case TK_KW_OBJECT:
        return parser_is_h27_enabled();
    case TK_KW_REF:
        return parser_is_h27_enabled();
    case TK_KW_FUN:
    {
        Token next = lexer_peek_n(ps->lx, 1);
        return next.kind == TK_STAR;
    }
    case TK_KW_STRUCT:
        return 1;
    case TK_KW_BUNDLE:
        return parser_is_h28_enabled();
    case TK_KW_UNION:
        return parser_is_h27_enabled();
    default:
        break;
    }
    if (t.kind == TK_IDENT)
    {
        if (t.length == 7 && strncmp(t.lexeme, "va_list", 7) == 0)
            return 1;
        if (parser_lookup_generic_param(ps, &t))
            return 1;
        if (alias_find(ps, t.lexeme, t.length) >= 0 || named_type_find(ps, t.lexeme, t.length) >= 0)
            return 1;
        if (parser_find_import_by_alias(ps, t.lexeme, t.length))
        {
            if (module_path_followed_by_call(ps))
                return 0;
            return 1;
        }
        if (parser_find_import_by_parts(ps, &t, 1))
        {
            if (module_path_followed_by_call(ps))
                return 0;
            return 1;
        }
    }
    return 0;
}

static int parser_is_implicit_ident_decl_start(Parser *ps, Token t)
{
    if (!ps || t.kind != TK_IDENT)
        return 0;
    if (lexer_peek_n(ps->lx, 1).kind == TK_IDENT)
        return 1;

    if (lexer_peek_n(ps->lx, 1).kind != TK_LT)
        return 0;

    int depth = 0;
    int paren_depth = 0;
    int bracket_depth = 0;
    int offset = 1;
    while (1)
    {
        Token tok = lexer_peek_n(ps->lx, offset);
        if (offset == 1)
        {
            depth = 1;
            offset++;
            continue;
        }
        switch (tok.kind)
        {
        case TK_LT:
            depth++;
            break;
        case TK_GT:
            depth--;
            break;
        case TK_SHR:
            depth -= 2;
            break;
        case TK_LPAREN:
            paren_depth++;
            break;
        case TK_RPAREN:
            if (paren_depth > 0)
                paren_depth--;
            else if (depth > 0 && bracket_depth == 0)
                return 0;
            break;
        case TK_LBRACKET:
            bracket_depth++;
            break;
        case TK_RBRACKET:
            if (bracket_depth > 0)
                bracket_depth--;
            else if (depth > 0 && paren_depth == 0)
                return 0;
            break;
        case TK_EOF:
        case TK_SEMI:
        case TK_ASSIGN:
            return 0;
        case TK_LBRACE:
        case TK_RBRACE:
            if (depth > 0 && paren_depth == 0 && bracket_depth == 0)
                return 0;
            break;
        default:
            break;
        }

        if (depth <= 0)
        {
            if (depth < 0)
                return 0;
            Token maybe_name = lexer_peek_n(ps->lx, offset + 1);
            if (maybe_name.kind != TK_IDENT)
                return 0;
            Token after_name = lexer_peek_n(ps->lx, offset + 2);
            return after_name.kind == TK_ASSIGN || after_name.kind == TK_SEMI;
        }
        offset++;
    }
}

static Type *parser_decl_canonicalize_bundle_reference(Type *ty)
{
    if (!ty)
        return ty;
    Type *resolved = module_registry_canonical_type(ty);
    if (resolved)
        ty = resolved;
    if (ty->kind == TY_STRUCT && ty->is_bundle)
        return type_ptr(ty);
    return ty;
}

static int stmt_starts_with_qualified_member_expr(Parser *ps)
{
    if (!ps)
        return 0;

    Token first = lexer_peek(ps->lx);
    if (first.kind != TK_IDENT)
        return 0;

    if (lexer_peek_n(ps->lx, 1).kind != TK_DOT)
        return 0;

    int idx = 1;
    while (lexer_peek_n(ps->lx, idx).kind == TK_DOT)
    {
        if (lexer_peek_n(ps->lx, idx + 1).kind != TK_IDENT)
            return 0;
        idx += 2;
    }

    int decl_idx = idx;
    Token look = lexer_peek_n(ps->lx, decl_idx);
    while (look.kind == TK_STAR)
    {
        decl_idx++;
        look = lexer_peek_n(ps->lx, decl_idx);
    }

    /*
     * A declaration must provide a declarator name after the type path.
     * If no identifier follows, treat it as an expression (e.g. Type.member = ...).
     */
    return look.kind != TK_IDENT;
}

static int type_is_function_pointer(const Type *ty)
{
    return ty && ty->kind == TY_PTR && ty->pointee && ty->pointee->kind == TY_FUNC;
}

static void fun_type_reset_signature(Type *func_ty)
{
    if (!func_ty || func_ty->kind != TY_FUNC)
        return;
    free(func_ty->func.params);
    func_ty->func.params = NULL;
    func_ty->func.param_count = 0;
    func_ty->func.is_varargs = 0;
    func_ty->func.ret = NULL;
    func_ty->func.has_signature = 0;
}

static int parse_funptr_signature_parens(Parser *ps, Type *func_ty, int allow_arrow_inside)
{
    if (!ps || !func_ty || func_ty->kind != TY_FUNC)
        return 0;

    Token lparen = lexer_next(ps->lx);
    (void)lparen;

    Type **params = NULL;
    int param_count = 0;
    int is_varargs = 0;
    int ret_parsed = 0;

    for (;;)
    {
        Token tok = lexer_peek(ps->lx);
        if (is_varargs)
        {
            if (!(tok.kind == TK_RPAREN || tok.kind == TK_COMMA || (allow_arrow_inside && tok.kind == TK_ARROW)))
            {
                diag_error_at(lexer_source(ps->lx), tok.line, tok.col,
                              "varargs must be the final entry in a function pointer signature");
                exit(1);
            }
        }
        if (tok.kind == TK_RPAREN)
        {
            lexer_next(ps->lx);
            break;
        }
        if (tok.kind == TK_EOF)
        {
            diag_error_at(lexer_source(ps->lx), tok.line, tok.col,
                          "unexpected end of input in function pointer signature");
            exit(1);
        }
        if (tok.kind == TK_COMMA)
        {
            lexer_next(ps->lx);
            continue;
        }
        if (allow_arrow_inside && tok.kind == TK_ARROW)
        {
            if (ret_parsed)
            {
                diag_error_at(lexer_source(ps->lx), tok.line, tok.col,
                              "duplicate return type in function pointer signature");
                exit(1);
            }
            lexer_next(ps->lx);
            Type *ret_ty = parse_type_spec(ps);
            func_ty->func.ret = ret_ty;
            ret_parsed = 1;
            continue;
        }
        if (token_is_varargs(tok))
        {
            if (is_varargs)
            {
                diag_error_at(lexer_source(ps->lx), tok.line, tok.col,
                              "varargs may only appear once in a function pointer signature");
                exit(1);
            }
            lexer_next(ps->lx);
            is_varargs = 1;
            continue;
        }

        Type *param_ty = parse_type_spec(ps);
        Token maybe_name = lexer_peek(ps->lx);
        if (maybe_name.kind == TK_IDENT && !is_type_start(ps, maybe_name) &&
            !token_is_varargs(maybe_name) && maybe_name.kind != TK_ARROW)
        {
            lexer_next(ps->lx);
        }

        Type **grown = (Type **)realloc(params, (size_t)(param_count + 1) * sizeof(Type *));
        if (!grown)
        {
            diag_error("out of memory while parsing function pointer parameters");
            exit(1);
        }
        params = grown;
        params[param_count++] = param_ty;
    }

    free(func_ty->func.params);
    func_ty->func.params = params;
    func_ty->func.param_count = param_count;
    func_ty->func.is_varargs = is_varargs;
    return ret_parsed;
}

static void finalize_funptr_signature(Parser *ps, Type *func_ty, const Token *where)
{
    if (!func_ty || func_ty->kind != TY_FUNC)
        return;
    if (!func_ty->func.ret)
    {
        const SourceBuffer *src = ps ? lexer_source(ps->lx) : NULL;
        diag_error_at(src, where ? where->line : 0, where ? where->col : 0,
                      "function pointer signature missing return type");
        exit(1);
    }
    func_ty->func.has_signature = 1;
}

static void parse_trailing_funptr_signature(Parser *ps, Type *ty)
{
    if (!type_is_function_pointer(ty))
        return;
    Type *func_ty = ty->pointee;
    if (func_ty->func.has_signature)
        return;
    Token next = lexer_peek(ps->lx);
    if (next.kind != TK_LPAREN)
        return;

    int ret_parsed_inside = parse_funptr_signature_parens(ps, func_ty, 1);
    if (!ret_parsed_inside)
    {
        Token arrow = lexer_peek(ps->lx);
        if (arrow.kind != TK_ARROW)
        {
            diag_error_at(lexer_source(ps->lx), arrow.line, arrow.col,
                          "function pointer declaration requires '->' return type");
            exit(1);
        }
        lexer_next(ps->lx);
        func_ty->func.ret = parse_type_spec(ps);
    }
    finalize_funptr_signature(ps, func_ty, &next);
}

static Type *parse_type_base(Parser *ps)
{
    if (!ps)
        return NULL;
    
    int consumed_stack = 0;
    while (1)
    {
        Token t = lexer_peek(ps->lx);
        if (t.kind == TK_KW_CONSTANT)
        {
            lexer_next(ps->lx);
            continue;
        }
        if (!consumed_stack && t.kind == TK_KW_STACK)
        {
            lexer_next(ps->lx);
            consumed_stack = 1;
            continue;
        }
        break;
    }
    Type *base = NULL;
    Token p;
    Token b = lexer_next(ps->lx);
    if (b.kind == TK_KW_REF)
    {
        parser_require_h27(ps, b, "ref type");
        int nullability = 0; 
        Token look = lexer_peek(ps->lx);
        if (look.kind == TK_QUESTION)
        {
            lexer_next(ps->lx);
            nullability = 1;
        }
        else if (look.kind == TK_BANG)
        {
            lexer_next(ps->lx);
            nullability = 2;
        }
        Type *pointee = parse_type_spec(ps);
        return type_ref(pointee, nullability);
    }
    if (b.kind == TK_KW_REF)
    {
        parser_require_h27(ps, b, "ref type");
        int nullability = 0; 
        Token look = lexer_peek(ps->lx);
        if (look.kind == TK_QUESTION)
        {
            lexer_next(ps->lx);
            nullability = 1;
        }
        else if (look.kind == TK_BANG)
        {
            lexer_next(ps->lx);
            nullability = 2;
        }
        Type *pointee = parse_type_spec(ps);
        return type_ref(pointee, nullability);
    }
    
    static Type ti8 = {.kind = TY_I8}, tu8 = {.kind = TY_U8},
                ti16 = {.kind = TY_I16}, tu16 = {.kind = TY_U16},
                ti32 = {.kind = TY_I32}, tu32 = {.kind = TY_U32},
                ti64 = {.kind = TY_I64}, tu64 = {.kind = TY_U64};
    static Type tf32 = {.kind = TY_F32}, tf64 = {.kind = TY_F64},
                tf128 = {.kind = TY_F128};
    static Type tv = {.kind = TY_VOID}, tch = {.kind = TY_CHAR}, tbool = {.kind = TY_BOOL};
    static Type tobject = {.kind = TY_PTR, .pointee = &tv, .is_object = 1};
    if (b.kind == TK_KW_I8)
        base = &ti8;
    else if (b.kind == TK_KW_U8)
        base = &tu8;
    else if (b.kind == TK_KW_I16 || b.kind == TK_KW_SHORT)
        base = &ti16;
    else if (b.kind == TK_KW_U16 || b.kind == TK_KW_USHORT)
        base = &tu16;
    else if (b.kind == TK_KW_I32 || b.kind == TK_KW_INT)
        base = &ti32;
    else if (b.kind == TK_KW_U32 || b.kind == TK_KW_UINT)
        base = &tu32;
    else if (b.kind == TK_KW_BYTE)
        base = &ti8;
    else if (b.kind == TK_KW_UBYTE)
        base = &tu8;
    else if (b.kind == TK_KW_I64 || b.kind == TK_KW_LONG)
        base = &ti64;
    else if (b.kind == TK_KW_U64 || b.kind == TK_KW_ULONG)
        base = &tu64;
    else if (b.kind == TK_KW_F32 || b.kind == TK_KW_FLOAT)
        base = &tf32;
    else if (b.kind == TK_KW_DOUBLE)
    {
        
        
        base = &tf64;
    }
    else if (b.kind == TK_KW_F64)
        base = &tf64;
    else if (b.kind == TK_KW_F128)
        base = &tf128;
    else if (b.kind == TK_KW_VOID)
        base = &tv;
    else if (b.kind == TK_KW_CHAR)
        base = &tch;
    else if (b.kind == TK_KW_STRING)
        base = type_ptr(&tch);
    else if (b.kind == TK_KW_BOOL)
        base = &tbool;
    else if (b.kind == TK_KW_OBJECT)
    {
        parser_require_h27(ps, b, "object type");
        base = &tobject;
    }
    else if (b.kind == TK_KW_LONG)
    {
        
        Token p = lexer_peek(ps->lx);
        if (p.kind == TK_KW_DOUBLE)
        {
            lexer_next(ps->lx);
            base = &tf128;
        }
        else
            base = &ti64;
    }
    else if (b.kind == TK_KW_FUN)
    {
        Token star = lexer_peek(ps->lx);
        if (star.kind != TK_STAR)
        {
            diag_error_at(lexer_source(ps->lx), star.line, star.col,
                          "expected '*' after 'fun' in function pointer type");
            exit(1);
        }
        lexer_next(ps->lx); 
        Type *func_ty = type_func();
        Token after_star = lexer_peek(ps->lx);
        if (after_star.kind == TK_LPAREN)
        {
            int ret_inside = parse_funptr_signature_parens(ps, func_ty, 1);
            if (!ret_inside)
            {
                Token arrow = lexer_peek(ps->lx);
                if (arrow.kind != TK_ARROW)
                {
                    diag_error_at(lexer_source(ps->lx), arrow.line, arrow.col,
                                  "function pointer type requires '->' return type");
                    exit(1);
                }
                lexer_next(ps->lx);
                func_ty->func.ret = parse_type_spec(ps);
            }
            finalize_funptr_signature(ps, func_ty, &after_star);
        }
        base = type_ptr(func_ty);
    }
    else if (b.kind == TK_KW_ACTION)
    {
        Type *func_ty = type_func();
        Token maybe_lparen = lexer_peek(ps->lx);
        if (maybe_lparen.kind == TK_LPAREN)
        {
            int ret_inside = parse_funptr_signature_parens(ps, func_ty, 1);
            if (!ret_inside)
            {
                Token arrow = lexer_peek(ps->lx);
                if (arrow.kind != TK_ARROW)
                {
                    diag_error_at(lexer_source(ps->lx), arrow.line, arrow.col,
                                  "action type requires '->' return type when signature is present");
                    exit(1);
                }
                lexer_next(ps->lx);
                func_ty->func.ret = parse_type_spec(ps);
            }
            finalize_funptr_signature(ps, func_ty, &maybe_lparen);
        }
        base = type_ptr(func_ty);
    }
    else if (b.kind == TK_IDENT)
    {
        Token local_buf[8];
        Token *tokens = local_buf;
        int token_count = 0;
        int token_cap = (int)(sizeof(local_buf) / sizeof(local_buf[0]));
        tokens[token_count++] = b;
        Token dot = lexer_peek(ps->lx);
        while (dot.kind == TK_DOT)
        {
            lexer_next(ps->lx);
            Token next_ident = expect(ps, TK_IDENT, "identifier");
            if (token_count == token_cap)
            {
                token_cap *= 2;
                Token *grown = (Token *)xmalloc((size_t)token_cap * sizeof(Token));
                memcpy(grown, tokens, (size_t)token_count * sizeof(Token));
                if (tokens != local_buf)
                    free(tokens);
                tokens = grown;
            }
            tokens[token_count++] = next_ident;
            dot = lexer_peek(ps->lx);
        }

        if (token_count >= 2)
        {
            int module_parts = token_count - 1;
            Token *module_tokens = tokens;
            Token type_tok = tokens[token_count - 1];
            const struct ModuleImport *imp = NULL;
            if (module_parts == 1)
                imp = parser_find_import_by_alias(ps, module_tokens[0].lexeme, module_tokens[0].length);
            if (!imp)
                imp = parser_find_import_by_parts(ps, module_tokens, module_parts);
            const char *module_full = imp ? imp->full_name : NULL;
            if (!module_full && parser_module_tokens_match_current(ps, module_tokens, module_parts))
                module_full = ps->module_full_name;
            if (!module_full)
            {
                size_t total_len = 0;
                for (int i = 0; i < module_parts; ++i)
                    total_len += (size_t)module_tokens[i].length + 1;
                char *module_name = (char *)xmalloc(total_len + 1);
                size_t pos = 0;
                for (int i = 0; i < module_parts; ++i)
                {
                    memcpy(module_name + pos, module_tokens[i].lexeme, (size_t)module_tokens[i].length);
                    pos += (size_t)module_tokens[i].length;
                    if (i + 1 < module_parts)
                        module_name[pos++] = '.';
                }
                module_name[pos] = '\0';
                diag_error_at(lexer_source(ps->lx), b.line, b.col,
                              "unknown module '%s' for qualified type", module_name);
                free(module_name);
                if (tokens != local_buf)
                    free(tokens);
                exit(1);
            }

            char *type_name = (char *)xmalloc((size_t)type_tok.length + 1);
            memcpy(type_name, type_tok.lexeme, (size_t)type_tok.length);
            type_name[type_tok.length] = '\0';

            Type **template_type_args = NULL;
            int template_type_arg_count = 0;
            Token maybe_lt = lexer_peek(ps->lx);
            if (maybe_lt.kind == TK_LT)
                template_type_args = parser_parse_type_arg_list(ps, &template_type_arg_count);

            if (template_type_args)
            {
                int inst_matches = 0;
                Type *inst = parser_lookup_preinstantiated_template_instance(ps,
                                                                             module_full,
                                                                             type_name,
                                                                             (int)strlen(type_name),
                                                                             template_type_args,
                                                                             template_type_arg_count,
                                                                             &inst_matches);
                if (inst_matches > 1)
                {
                    diag_error_at(lexer_source(ps->lx), type_tok.line, type_tok.col,
                                  "ambiguous template bundle '%s' in module '%s'; multiple pre-instantiated variants are available",
                                  type_name, module_full);
                    free(type_name);
                    if (tokens != local_buf)
                        free(tokens);
                    exit(1);
                }
                if (!inst)
                {
                    diag_error_at(lexer_source(ps->lx), type_tok.line, type_tok.col,
                                  "type '%s.%s' does not accept type arguments",
                                  module_full, type_name);
                    free(type_name);
                    if (tokens != local_buf)
                        free(tokens);
                    exit(1);
                }

                base = inst;
                free(type_name);
                if (tokens != local_buf)
                    free(tokens);
                return base;
            }

            Type *resolved = module_registry_lookup_struct(module_full, type_name);
            if (!resolved)
                resolved = module_registry_lookup_enum(module_full, type_name);
            if (resolved)
            {
                base = resolved;
                free(type_name);
            }
            else
            {
                Type *imp_type = (Type *)xcalloc(1, sizeof(Type));
                imp_type->kind = TY_IMPORT;
                imp_type->struct_name = type_name;
                imp_type->import_module = xstrdup(module_full);
                imp_type->import_type_name = type_name;
                base = imp_type;
            }

            if (tokens != local_buf)
                free(tokens);
        }
        else
        {
            if (b.length == 7 && strncmp(b.lexeme, "va_list", 7) == 0)
            {
                base = type_va_list();
            }
            else
            {
                Type *generic_param = parser_lookup_generic_param(ps, &b);
                if (generic_param)
                {
                    base = generic_param;
                }
                else
                {
                    int ai = alias_find(ps, b.lexeme, b.length);
                    struct BundleTemplate *templ = parser_find_bundle_template(ps, b.lexeme, b.length);
                    Type **template_type_args = NULL;
                    int template_type_arg_count = 0;

                    Token maybe_lt = lexer_peek(ps->lx);
                    if (maybe_lt.kind == TK_LT)
                    {
                        if (ai >= 0 && ps->aliases[ai].is_generic)
                        {
                            // Parsed below in generic alias path.
                        }
                        else
                        {
                            template_type_args = parser_parse_type_arg_list(ps, &template_type_arg_count);
                        }
                    }

                    if (template_type_args)
                    {
                        if (!templ)
                        {
                            int inst_matches = 0;
                            Type *inst = parser_lookup_preinstantiated_template_instance(ps,
                                                                                         NULL,
                                                                                         b.lexeme,
                                                                                         b.length,
                                                                                         template_type_args,
                                                                                         template_type_arg_count,
                                                                                         &inst_matches);
                            if (inst_matches > 1)
                            {
                                diag_error_at(lexer_source(ps->lx), b.line, b.col,
                                              "ambiguous template bundle '%.*s'; multiple imported pre-instantiated variants are available",
                                              b.length, b.lexeme);
                                exit(1);
                            }
                            if (!inst)
                            {
                                diag_error_at(lexer_source(ps->lx), b.line, b.col,
                                              "type '%.*s' does not accept type arguments (found %d pre-instantiated variant(s))",
                                              b.length, b.lexeme, inst_matches);
                                exit(1);
                            }
                            base = inst;
                        }
                        else
                        {
                            base = parser_instantiate_bundle_template(ps,
                                                                       templ,
                                                                       template_type_args,
                                                                       template_type_arg_count,
                                                                       b.line,
                                                                       b.col);
                        }
                    }
                    else if (ai < 0)
                    {
                        Type *nt = named_type_get(ps, b.lexeme, b.length);
                        if (nt)
                        {
                            if (templ)
                            {
                                diag_error_at(lexer_source(ps->lx), b.line, b.col,
                                              "template bundle '%.*s' requires explicit type arguments",
                                              b.length, b.lexeme);
                                exit(1);
                            }
                            base = nt;
                        }
                        else
                        {
                            Type *deferred = parser_make_deferred_import_type(ps, &b);
                            if (!deferred)
                            {
                                diag_error_at(lexer_source(ps->lx), b.line, b.col, "unknown type '%.*s'",
                                              b.length, b.lexeme);
                                exit(1);
                            }
                            base = deferred;
                        }
                    }
                    if (ai >= 0)
                    {
                        struct Alias *A = &ps->aliases[ai];
                        if (A->is_generic)
                        {
                            Token lt = lexer_next(ps->lx);
                            if (lt.kind != TK_LT)
                            {
                                diag_error_at(lexer_source(ps->lx), lt.line, lt.col,
                                              "expected '<' after generic alias '%.*s'", b.length,
                                              b.lexeme);
                                exit(1);
                            }
                            Type *arg = parse_type_spec(ps);
                            Token gt = lexer_next(ps->lx);
                            if (gt.kind != TK_GT)
                            {
                                diag_error_at(lexer_source(ps->lx), gt.line, gt.col,
                                              "expected '>' after generic argument");
                                exit(1);
                            }
                            base = make_ptr_chain_dyn(arg, A->gen_ptr_depth);
                        }
                        else
                        {
                            if (A->resolved_type)
                            {
                                base = A->resolved_type;
                            }
                            else
                            {
                                Type *bk = NULL;
                                switch (A->base_kind)
                                {
                                case TY_I8:
                                    bk = &ti8;
                                    break;
                                case TY_U8:
                                    bk = &tu8;
                                    break;
                                case TY_I16:
                                    bk = &ti16;
                                    break;
                                case TY_U16:
                                    bk = &tu16;
                                    break;
                                case TY_I32:
                                    bk = &ti32;
                                    break;
                                case TY_U32:
                                    bk = &tu32;
                                    break;
                                case TY_I64:
                                    bk = &ti64;
                                    break;
                                case TY_U64:
                                    bk = &tu64;
                                    break;
                                case TY_F32:
                                    bk = &tf32;
                                    break;
                                case TY_F64:
                                    bk = &tf64;
                                    break;
                                case TY_F128:
                                    bk = &tf128;
                                    break;
                                case TY_VOID:
                                    bk = &tv;
                                    break;
                                case TY_CHAR:
                                    bk = &tch;
                                    break;
                                default:
                                    diag_error("unsupported alias base kind");
                                    exit(1);
                                }
                                base = make_ptr_chain_dyn(bk, A->ptr_depth);
                            }
                        }
                    }
                }
            }
        }
    }
    else if (b.kind == TK_KW_STRUCT || b.kind == TK_KW_BUNDLE || b.kind == TK_KW_UNION)
    {
        if (b.kind == TK_KW_BUNDLE)
            parser_require_h28(ps, b, "bundle type");
        if (b.kind == TK_KW_UNION)
            parser_require_h27(ps, b, "union type");
        int want_union = (b.kind == TK_KW_UNION);
        Token look = lexer_peek(ps->lx);
        if (want_union && look.kind == TK_LBRACE)
        {
            base = parse_inline_union_type(ps);
            return base;
        }
        
        Token nm = expect(ps, TK_IDENT, want_union ? "union name" : "struct name");
        if (!want_union)
        {
            Token maybe_lt = lexer_peek(ps->lx);
            if (maybe_lt.kind == TK_LT)
            {
                struct BundleTemplate *templ = parser_find_bundle_template(ps, nm.lexeme, nm.length);
                if (!templ)
                {
                    diag_error_at(lexer_source(ps->lx), maybe_lt.line, maybe_lt.col,
                                  "type '%.*s' does not accept type arguments",
                                  nm.length, nm.lexeme);
                    exit(1);
                }
                Type **type_args = NULL;
                int type_arg_count = 0;
                type_args = parser_parse_type_arg_list(ps, &type_arg_count);
                base = parser_instantiate_bundle_template(ps, templ, type_args, type_arg_count,
                                                          nm.line, nm.col);
                return base;
            }
        }
        Type *nt = named_type_get(ps, nm.lexeme, nm.length);
        if (!nt)
        {
            diag_error_at(lexer_source(ps->lx), nm.line, nm.col,
                          "unknown %s '%.*s'", want_union ? "union" : "struct",
                          nm.length, nm.lexeme);
            exit(1);
        }
        if (!want_union && parser_find_bundle_template(ps, nm.lexeme, nm.length))
        {
            diag_error_at(lexer_source(ps->lx), nm.line, nm.col,
                          "template bundle '%.*s' requires explicit type arguments",
                          nm.length, nm.lexeme);
            exit(1);
        }
        if (!!nt->is_union != want_union)
        {
            diag_error_at(lexer_source(ps->lx), nm.line, nm.col,
                          "type '%.*s' is declared as %s",
                          nm.length, nm.lexeme,
                          nt->is_union ? "union" : "struct");
            exit(1);
        }
        base = nt;
    }
    else
    {
        diag_error_at(lexer_source(ps->lx), b.line, b.col,
                      "expected type specifier");
        exit(1);
    }
    return base;
}

static Type *parse_type_spec(Parser *ps)
{
    if (!ps)
        return NULL;
    
    int consumed_stack = 0;
    while (1)
    {
        Token t = lexer_peek(ps->lx);
        if (t.kind == TK_KW_CONSTANT)
        {
            lexer_next(ps->lx);
            continue;
        }
        if (!consumed_stack && t.kind == TK_KW_STACK)
        {
            lexer_next(ps->lx);
            consumed_stack = 1;
            continue;
        }
        break;
    }
    Type *base = NULL;
    Token p;
    Token b = lexer_next(ps->lx);
    if (b.kind == TK_KW_REF)
    {
        parser_require_h27(ps, b, "ref type");
        int nullability = 0; 
        Token look = lexer_peek(ps->lx);
        if (look.kind == TK_QUESTION)
        {
            lexer_next(ps->lx);
            nullability = 1;
        }
        else if (look.kind == TK_BANG)
        {
            lexer_next(ps->lx);
            nullability = 2;
        }
        Type *pointee = parse_type_spec(ps);
        return type_ref(pointee, nullability);
    }
    static Type ti8 = {.kind = TY_I8}, tu8 = {.kind = TY_U8},
                ti16 = {.kind = TY_I16}, tu16 = {.kind = TY_U16},
                ti32 = {.kind = TY_I32}, tu32 = {.kind = TY_U32},
                ti64 = {.kind = TY_I64}, tu64 = {.kind = TY_U64};
    static Type tf32 = {.kind = TY_F32}, tf64 = {.kind = TY_F64},
                tf128 = {.kind = TY_F128};
    static Type tv = {.kind = TY_VOID}, tch = {.kind = TY_CHAR}, tbool = {.kind = TY_BOOL};
    static Type tobject = {.kind = TY_PTR, .pointee = &tv, .is_object = 1};
    if (b.kind == TK_KW_I8)
        base = &ti8;
    else if (b.kind == TK_KW_U8)
        base = &tu8;
    else if (b.kind == TK_KW_I16 || b.kind == TK_KW_SHORT)
        base = &ti16;
    else if (b.kind == TK_KW_U16 || b.kind == TK_KW_USHORT)
        base = &tu16;
    else if (b.kind == TK_KW_I32 || b.kind == TK_KW_INT)
        base = &ti32;
    else if (b.kind == TK_KW_U32 || b.kind == TK_KW_UINT)
        base = &tu32;
    else if (b.kind == TK_KW_BYTE)
        base = &ti8;
    else if (b.kind == TK_KW_UBYTE)
        base = &tu8;
    else if (b.kind == TK_KW_I64 || b.kind == TK_KW_LONG)
        base = &ti64;
    else if (b.kind == TK_KW_U64 || b.kind == TK_KW_ULONG)
        base = &tu64;
    else if (b.kind == TK_KW_F32 || b.kind == TK_KW_FLOAT)
        base = &tf32;
    else if (b.kind == TK_KW_DOUBLE)
    {
        
        
        base = &tf64;
    }
    else if (b.kind == TK_KW_F64)
        base = &tf64;
    else if (b.kind == TK_KW_F128)
        base = &tf128;
    else if (b.kind == TK_KW_VOID)
        base = &tv;
    else if (b.kind == TK_KW_CHAR)
        base = &tch;
    else if (b.kind == TK_KW_STRING)
        base = type_ptr(&tch);
    else if (b.kind == TK_KW_BOOL)
        base = &tbool;
    else if (b.kind == TK_KW_OBJECT)
    {
        parser_require_h27(ps, b, "object type");
        base = &tobject;
    }
    else if (b.kind == TK_KW_LONG)
    {
        
        Token p = lexer_peek(ps->lx);
        if (p.kind == TK_KW_DOUBLE)
        {
            lexer_next(ps->lx);
            base = &tf128;
        }
        else
            base = &ti64;
    }
    else if (b.kind == TK_KW_FUN)
    {
        Token star = lexer_peek(ps->lx);
        if (star.kind != TK_STAR)
        {
            diag_error_at(lexer_source(ps->lx), star.line, star.col,
                          "expected '*' after 'fun' in function pointer type");
            exit(1);
        }
        lexer_next(ps->lx); 
        Type *func_ty = type_func();
        Token after_star = lexer_peek(ps->lx);
        if (after_star.kind == TK_LPAREN)
        {
            int ret_inside = parse_funptr_signature_parens(ps, func_ty, 1);
            if (!ret_inside)
            {
                Token arrow = lexer_peek(ps->lx);
                if (arrow.kind != TK_ARROW)
                {
                    diag_error_at(lexer_source(ps->lx), arrow.line, arrow.col,
                                  "function pointer type requires '->' return type");
                    exit(1);
                }
                lexer_next(ps->lx);
                func_ty->func.ret = parse_type_spec(ps);
            }
            finalize_funptr_signature(ps, func_ty, &after_star);
        }
        base = type_ptr(func_ty);
    }
    else if (b.kind == TK_KW_ACTION)
    {
        Type *func_ty = type_func();
        Token maybe_lparen = lexer_peek(ps->lx);
        if (maybe_lparen.kind == TK_LPAREN)
        {
            int ret_inside = parse_funptr_signature_parens(ps, func_ty, 1);
            if (!ret_inside)
            {
                Token arrow = lexer_peek(ps->lx);
                if (arrow.kind != TK_ARROW)
                {
                    diag_error_at(lexer_source(ps->lx), arrow.line, arrow.col,
                                  "action type requires '->' return type when signature is present");
                    exit(1);
                }
                lexer_next(ps->lx);
                func_ty->func.ret = parse_type_spec(ps);
            }
            finalize_funptr_signature(ps, func_ty, &maybe_lparen);
        }
        base = type_ptr(func_ty);
    }
    else if (b.kind == TK_IDENT)
    {
        Token local_buf[8];
        Token *tokens = local_buf;
        int token_count = 0;
        int token_cap = (int)(sizeof(local_buf) / sizeof(local_buf[0]));
        tokens[token_count++] = b;
        Token dot = lexer_peek(ps->lx);
        while (dot.kind == TK_DOT)
        {
            lexer_next(ps->lx);
            Token next_ident = expect(ps, TK_IDENT, "identifier");
            if (token_count == token_cap)
            {
                token_cap *= 2;
                Token *grown = (Token *)xmalloc((size_t)token_cap * sizeof(Token));
                memcpy(grown, tokens, (size_t)token_count * sizeof(Token));
                if (tokens != local_buf)
                    free(tokens);
                tokens = grown;
            }
            tokens[token_count++] = next_ident;
            dot = lexer_peek(ps->lx);
        }

        if (token_count >= 2)
        {
            int module_parts = token_count - 1;
            Token *module_tokens = tokens;
            Token type_tok = tokens[token_count - 1];
            const struct ModuleImport *imp = NULL;
            if (module_parts == 1)
                imp = parser_find_import_by_alias(ps, module_tokens[0].lexeme, module_tokens[0].length);
            if (!imp)
                imp = parser_find_import_by_parts(ps, module_tokens, module_parts);
            const char *module_full = imp ? imp->full_name : NULL;
            if (!module_full && parser_module_tokens_match_current(ps, module_tokens, module_parts))
                module_full = ps->module_full_name;
            if (!module_full)
            {
                size_t total_len = 0;
                for (int i = 0; i < module_parts; ++i)
                    total_len += (size_t)module_tokens[i].length + 1;
                char *module_name = (char *)xmalloc(total_len + 1);
                size_t pos = 0;
                for (int i = 0; i < module_parts; ++i)
                {
                    memcpy(module_name + pos, module_tokens[i].lexeme, (size_t)module_tokens[i].length);
                    pos += (size_t)module_tokens[i].length;
                    if (i + 1 < module_parts)
                        module_name[pos++] = '.';
                }
                module_name[pos] = '\0';
                diag_error_at(lexer_source(ps->lx), b.line, b.col,
                              "unknown module '%s' for qualified type", module_name);
                free(module_name);
                if (tokens != local_buf)
                    free(tokens);
                exit(1);
            }

            char *type_name = (char *)xmalloc((size_t)type_tok.length + 1);
            memcpy(type_name, type_tok.lexeme, (size_t)type_tok.length);
            type_name[type_tok.length] = '\0';

            Type **template_type_args = NULL;
            int template_type_arg_count = 0;
            Token maybe_lt = lexer_peek(ps->lx);
            if (maybe_lt.kind == TK_LT)
                template_type_args = parser_parse_type_arg_list(ps, &template_type_arg_count);

            if (template_type_args)
            {
                int inst_matches = 0;
                Type *inst = parser_lookup_preinstantiated_template_instance(ps,
                                                                             module_full,
                                                                             type_name,
                                                                             (int)strlen(type_name),
                                                                             template_type_args,
                                                                             template_type_arg_count,
                                                                             &inst_matches);
                if (inst_matches > 1)
                {
                    diag_error_at(lexer_source(ps->lx), type_tok.line, type_tok.col,
                                  "ambiguous template bundle '%s' in module '%s'; multiple pre-instantiated variants are available",
                                  type_name, module_full);
                    free(type_name);
                    if (tokens != local_buf)
                        free(tokens);
                    exit(1);
                }
                if (!inst)
                {
                    diag_error_at(lexer_source(ps->lx), type_tok.line, type_tok.col,
                                  "type '%s.%s' does not accept type arguments",
                                  module_full, type_name);
                    free(type_name);
                    if (tokens != local_buf)
                        free(tokens);
                    exit(1);
                }

                base = inst;
                free(type_name);
                if (tokens != local_buf)
                    free(tokens);
                goto parse_type_spec_post_base;
            }

            Type *resolved = module_registry_lookup_struct(module_full, type_name);
            if (!resolved)
                resolved = module_registry_lookup_enum(module_full, type_name);
            if (resolved)
            {
                base = resolved;
                free(type_name);
            }
            else
            {
                Type *imp_type = (Type *)xcalloc(1, sizeof(Type));
                imp_type->kind = TY_IMPORT;
                imp_type->struct_name = type_name;
                imp_type->import_module = xstrdup(module_full);
                imp_type->import_type_name = type_name;
                base = imp_type;
            }

            if (tokens != local_buf)
                free(tokens);
        }
        else
        {
            if (b.length == 7 && strncmp(b.lexeme, "va_list", 7) == 0)
            {
                base = type_va_list();
            }
            else
            {
                Type *generic_param = parser_lookup_generic_param(ps, &b);
                if (generic_param)
                {
                    base = generic_param;
                }
                else
                {
                    int ai = alias_find(ps, b.lexeme, b.length);
                    struct BundleTemplate *templ = parser_find_bundle_template(ps, b.lexeme, b.length);
                    Type **template_type_args = NULL;
                    int template_type_arg_count = 0;

                    Token maybe_lt = lexer_peek(ps->lx);
                    if (maybe_lt.kind == TK_LT)
                    {
                        if (ai >= 0 && ps->aliases[ai].is_generic)
                        {
                            // Parsed below in generic alias path.
                        }
                        else
                        {
                            template_type_args = parser_parse_type_arg_list(ps, &template_type_arg_count);
                        }
                    }

                    if (template_type_args)
                    {
                        if (!templ)
                        {
                            int inst_matches = 0;
                            Type *inst = parser_lookup_preinstantiated_template_instance(ps,
                                                                                         NULL,
                                                                                         b.lexeme,
                                                                                         b.length,
                                                                                         template_type_args,
                                                                                         template_type_arg_count,
                                                                                         &inst_matches);
                            if (inst_matches > 1)
                            {
                                diag_error_at(lexer_source(ps->lx), b.line, b.col,
                                              "ambiguous template bundle '%.*s'; multiple imported pre-instantiated variants are available",
                                              b.length, b.lexeme);
                                exit(1);
                            }
                            if (!inst)
                            {
                                diag_error_at(lexer_source(ps->lx), b.line, b.col,
                                              "type '%.*s' does not accept type arguments (found %d pre-instantiated variant(s))",
                                              b.length, b.lexeme, inst_matches);
                                exit(1);
                            }
                            base = inst;
                        }
                        else
                        {
                            base = parser_instantiate_bundle_template(ps,
                                                                       templ,
                                                                       template_type_args,
                                                                       template_type_arg_count,
                                                                       b.line,
                                                                       b.col);
                        }
                    }
                    else if (ai < 0)
                    {
                        Type *nt = named_type_get(ps, b.lexeme, b.length);
                        if (nt)
                        {
                            if (templ)
                            {
                                diag_error_at(lexer_source(ps->lx), b.line, b.col,
                                              "template bundle '%.*s' requires explicit type arguments",
                                              b.length, b.lexeme);
                                exit(1);
                            }
                            base = nt;
                        }
                        else
                        {
                            Type *deferred = parser_make_deferred_import_type(ps, &b);
                            if (!deferred)
                            {
                                diag_error_at(lexer_source(ps->lx), b.line, b.col, "unknown type '%.*s'",
                                              b.length, b.lexeme);
                                exit(1);
                            }
                            base = deferred;
                        }
                    }
                    if (ai >= 0)
                    {
                        struct Alias *A = &ps->aliases[ai];
                        if (A->is_generic)
                        {
                            Token lt = lexer_next(ps->lx);
                            if (lt.kind != TK_LT)
                            {
                                diag_error_at(lexer_source(ps->lx), lt.line, lt.col,
                                              "expected '<' after generic alias '%.*s'", b.length,
                                              b.lexeme);
                                exit(1);
                            }
                            Type *arg = parse_type_spec(ps);
                            Token gt = lexer_next(ps->lx);
                            if (gt.kind != TK_GT)
                            {
                                diag_error_at(lexer_source(ps->lx), gt.line, gt.col,
                                              "expected '>' after generic argument");
                                exit(1);
                            }
                            base = make_ptr_chain_dyn(arg, A->gen_ptr_depth);
                        }
                        else
                        {
                            if (A->resolved_type)
                            {
                                base = A->resolved_type;
                            }
                            else
                            {
                                Type *bk = NULL;
                                switch (A->base_kind)
                                {
                                case TY_I8:
                                    bk = &ti8;
                                    break;
                                case TY_U8:
                                    bk = &tu8;
                                    break;
                                case TY_I16:
                                    bk = &ti16;
                                    break;
                                case TY_U16:
                                    bk = &tu16;
                                    break;
                                case TY_I32:
                                    bk = &ti32;
                                    break;
                                case TY_U32:
                                    bk = &tu32;
                                    break;
                                case TY_I64:
                                    bk = &ti64;
                                    break;
                                case TY_U64:
                                    bk = &tu64;
                                    break;
                                case TY_F32:
                                    bk = &tf32;
                                    break;
                                case TY_F64:
                                    bk = &tf64;
                                    break;
                                case TY_F128:
                                    bk = &tf128;
                                    break;
                                case TY_VOID:
                                    bk = &tv;
                                    break;
                                case TY_CHAR:
                                    bk = &tch;
                                    break;
                                default:
                                    diag_error("unsupported alias base kind");
                                    exit(1);
                                }
                                base = make_ptr_chain_dyn(bk, A->ptr_depth);
                            }
                        }
                    }
                }
            }
        }
    }
    else if (b.kind == TK_KW_STRUCT || b.kind == TK_KW_BUNDLE || b.kind == TK_KW_UNION)
    {
        if (b.kind == TK_KW_BUNDLE)
            parser_require_h28(ps, b, "bundle type");
        if (b.kind == TK_KW_UNION)
            parser_require_h27(ps, b, "union type");
        int want_union = (b.kind == TK_KW_UNION);
        Token look = lexer_peek(ps->lx);
        if (want_union && look.kind == TK_LBRACE)
        {
            base = parse_inline_union_type(ps);
        }
        else
        {
            
            Token nm = expect(ps, TK_IDENT, want_union ? "union name" : "struct name");
            if (!want_union)
            {
                Token maybe_lt = lexer_peek(ps->lx);
                if (maybe_lt.kind == TK_LT)
                {
                    struct BundleTemplate *templ = parser_find_bundle_template(ps, nm.lexeme, nm.length);
                    if (!templ)
                    {
                        diag_error_at(lexer_source(ps->lx), maybe_lt.line, maybe_lt.col,
                                      "type '%.*s' does not accept type arguments",
                                      nm.length, nm.lexeme);
                        exit(1);
                    }
                    Type **type_args = NULL;
                    int type_arg_count = 0;
                    type_args = parser_parse_type_arg_list(ps, &type_arg_count);
                    base = parser_instantiate_bundle_template(ps, templ, type_args, type_arg_count,
                                                              nm.line, nm.col);
                    goto parse_type_spec_post_base;
                }
            }
            Type *nt = named_type_get(ps, nm.lexeme, nm.length);
            if (!nt)
            {
                diag_error_at(lexer_source(ps->lx), nm.line, nm.col,
                              "unknown %s '%.*s'", want_union ? "union" : "struct",
                              nm.length, nm.lexeme);
                exit(1);
            }
            if (!want_union && parser_find_bundle_template(ps, nm.lexeme, nm.length))
            {
                diag_error_at(lexer_source(ps->lx), nm.line, nm.col,
                              "template bundle '%.*s' requires explicit type arguments",
                              nm.length, nm.lexeme);
                exit(1);
            }
            if (!!nt->is_union != want_union)
            {
                diag_error_at(lexer_source(ps->lx), nm.line, nm.col,
                              "type '%.*s' is declared as %s",
                              nm.length, nm.lexeme,
                              nt->is_union ? "union" : "struct");
                exit(1);
            }
            base = nt;
        }
    }
    else
    {
        diag_error_at(lexer_source(ps->lx), b.line, b.col,
                      "expected type specifier");
        exit(1);
    }
parse_type_spec_post_base:
    p = lexer_peek(ps->lx);
    while (p.kind == TK_LBRACKET)
    {
        lexer_next(ps->lx);
        Token len_tok = lexer_peek(ps->lx);
        int length = -1;
        if (len_tok.kind == TK_RBRACKET)
        {
            length = -1;
        }
        else
        {
            len_tok = lexer_next(ps->lx);
            if (len_tok.kind == TK_INT)
            {
                if (len_tok.int_val < 0)
                {
                    diag_error_at(lexer_source(ps->lx), len_tok.line, len_tok.col,
                                  "array length must be non-negative");
                    exit(1);
                }
                if (len_tok.int_val > INT_MAX)
                {
                    diag_error_at(lexer_source(ps->lx), len_tok.line, len_tok.col,
                                  "array length is too large");
                    exit(1);
                }
                length = (int)len_tok.int_val;
            }
            else if (len_tok.kind == TK_IDENT)
            {
                int ev = 0;
                if (enum_const_get(ps, len_tok.lexeme, len_tok.length, &ev) ||
                    const_int_get(ps, len_tok.lexeme, len_tok.length, &ev))
                {
                    if (ev < 0)
                    {
                        diag_error_at(lexer_source(ps->lx), len_tok.line, len_tok.col,
                                      "array length must be non-negative");
                        exit(1);
                    }
                    length = ev;
                }
                else
                {
                    diag_error_at(lexer_source(ps->lx), len_tok.line, len_tok.col,
                                  "array length must be an integer literal or constant");
                    exit(1);
                }
            }
            else
            {
                diag_error_at(lexer_source(ps->lx), len_tok.line, len_tok.col,
                              "array length must be an integer literal or constant");
                exit(1);
            }
        }
        expect(ps, TK_RBRACKET, "]");
        base = type_array(base, length);
        p = lexer_peek(ps->lx);
    }
    
    int depth = 0;
    while (p.kind == TK_STAR)
    {
        lexer_next(ps->lx);
        depth++;
        p = lexer_peek(ps->lx);
    }
    Type *ty = (depth == 0) ? base : make_ptr_chain_dyn(base, depth);

    
    while (p.kind == TK_LBRACKET)
    {
        lexer_next(ps->lx);
        Token len_tok = lexer_peek(ps->lx);
        int length = -1;
        if (len_tok.kind == TK_RBRACKET)
        {
            length = -1;
        }
        else
        {
            len_tok = lexer_next(ps->lx);
            if (len_tok.kind == TK_INT)
            {
                if (len_tok.int_val < 0)
                {
                    diag_error_at(lexer_source(ps->lx), len_tok.line, len_tok.col,
                                  "array length must be non-negative");
                    exit(1);
                }
                if (len_tok.int_val > INT_MAX)
                {
                    diag_error_at(lexer_source(ps->lx), len_tok.line, len_tok.col,
                                  "array length is too large");
                    exit(1);
                }
                length = (int)len_tok.int_val;
            }
            else if (len_tok.kind == TK_IDENT)
            {
                int ev = 0;
                if (enum_const_get(ps, len_tok.lexeme, len_tok.length, &ev) ||
                    const_int_get(ps, len_tok.lexeme, len_tok.length, &ev))
                {
                    if (ev < 0)
                    {
                        diag_error_at(lexer_source(ps->lx), len_tok.line, len_tok.col,
                                      "array length must be non-negative");
                        exit(1);
                    }
                    length = ev;
                }
                else
                {
                    diag_error_at(lexer_source(ps->lx), len_tok.line, len_tok.col,
                                  "array length must be an integer literal or constant");
                    exit(1);
                }
            }
            else
            {
                diag_error_at(lexer_source(ps->lx), len_tok.line, len_tok.col,
                              "array length must be an integer literal or constant");
                exit(1);
            }
        }
        expect(ps, TK_RBRACKET, "]");
        ty = type_array(ty, length);
        p = lexer_peek(ps->lx);
    }

    return ty;
}

static int parser_peek_struct_literal(Parser *ps, Token look)
{
    if (!ps)
        return 0;
    if (look.kind != TK_IDENT)
        return 0;
    if (!is_type_start(ps, look))
        return 0;

    int offset = 1;
    for (;;)
    {
        Token tok = lexer_peek_n(ps->lx, offset);
        if (tok.kind == TK_LBRACE)
            return 1;
        if (tok.kind == TK_DOT)
        {
            Token ident = lexer_peek_n(ps->lx, offset + 1);
            if (ident.kind != TK_IDENT)
                return 0;
            offset += 2;
            continue;
        }
        return 0;
    }
}

static Node *parse_brace_initializer(Parser *ps)
{
    if (!ps)
        return NULL;

    Node *list = new_node(ND_INIT_LIST);
    Token open = expect(ps, TK_LBRACE, "{");
    list->line = open.line;
    list->col = open.col;
    list->src = lexer_source(ps->lx);

    Node **elems = NULL;
    const char **designators = NULL;
    int count = 0;
    int cap = 0;

    Token peek = lexer_peek(ps->lx);
    if (peek.kind == TK_RBRACE)
    {
        lexer_next(ps->lx);
        list->init.is_zero = 1;
        list->init.count = 0;
        list->init.elems = NULL;
        list->init.designators = NULL;
        return list;
    }

    while (1)
    {
        char *designator = NULL;
        Token maybe_dot = lexer_peek(ps->lx);
        if (maybe_dot.kind == TK_DOT)
        {
            lexer_next(ps->lx);
            Token field = expect(ps, TK_IDENT, "field name");
            designator = dup_token_text(field);
            expect(ps, TK_ASSIGN, "=");
        }

        Node *value = parse_expr(ps);

        if (count == cap)
        {
            int new_cap = cap ? cap * 2 : 4;
            Node **elem_grown = (Node **)realloc(elems, (size_t)new_cap * sizeof(Node *));
            const char **desig_grown = (const char **)realloc(designators, (size_t)new_cap * sizeof(const char *));
            if (!elem_grown || !desig_grown)
            {
                diag_error("out of memory while parsing initializer list");
                exit(1);
            }
            elems = elem_grown;
            designators = desig_grown;
            cap = new_cap;
        }
        elems[count] = value;
        designators[count] = designator;
        count++;

        Token next = lexer_peek(ps->lx);
        if (next.kind == TK_COMMA)
        {
            lexer_next(ps->lx);
            Token after_comma = lexer_peek(ps->lx);
            if (after_comma.kind == TK_RBRACE)
            {
                lexer_next(ps->lx);
                break;
            }
            continue;
        }
        if (next.kind == TK_RBRACE)
        {
            lexer_next(ps->lx);
            break;
        }
        diag_error_at(lexer_source(ps->lx), next.line, next.col,
                      "expected ',' or '}' in initializer list");
        exit(1);
    }

    list->init.count = count;
    list->init.elems = elems;
    list->init.designators = designators;
    list->init.is_zero = (count == 0);
    return list;
}

static Node *parse_lambda_expr(Parser *ps, Token fun_tok)
{
    if (!ps)
        return NULL;

    Node *lambda = new_node(ND_LAMBDA);
    lambda->line = fun_tok.line;
    lambda->col = fun_tok.col;
    lambda->src = lexer_source(ps->lx);

    expect(ps, TK_LPAREN, "(");

    Type **param_types = NULL;
    const char **param_names = NULL;
    unsigned char *param_const_flags = NULL;
    int param_count = 0;
    int param_cap = 0;
    int saw_varargs = 0;

    while (1)
    {
        Token next = lexer_peek(ps->lx);
        if (next.kind == TK_RPAREN)
            break;

        if (token_is_varargs(next))
        {
            if (saw_varargs)
            {
                diag_error_at(lexer_source(ps->lx), next.line, next.col,
                              "varargs ('...') may only appear once in a parameter list");
                exit(1);
            }
            lexer_next(ps->lx);
            saw_varargs = 1;
            break;
        }

        int param_is_const = 0;
        Token maybe_const = lexer_peek(ps->lx);
        if (maybe_const.kind == TK_KW_CONSTANT)
        {
            lexer_next(ps->lx);
            param_is_const = 1;
        }

        Type *pty = parse_type_spec(ps);
        pty = parser_decl_canonicalize_bundle_reference(pty);
        Token pn = expect(ps, TK_IDENT, "parameter name");

        if (param_count == param_cap)
        {
            param_cap = param_cap ? param_cap * 2 : 4;
            param_types = (Type **)realloc(param_types, sizeof(Type *) * param_cap);
            param_names = (const char **)realloc(param_names, sizeof(char *) * param_cap);
            param_const_flags = (unsigned char *)realloc(param_const_flags, sizeof(unsigned char) * param_cap);
        }

        param_types[param_count] = pty;
        char *nm = (char *)xmalloc((size_t)pn.length + 1);
        memcpy(nm, pn.lexeme, (size_t)pn.length);
        nm[pn.length] = '\0';
        param_names[param_count] = nm;
        if (param_const_flags)
            param_const_flags[param_count] = (unsigned char)param_is_const;
        parse_trailing_funptr_signature(ps, pty);
        param_count++;

        Token delim = lexer_peek(ps->lx);
        if (delim.kind == TK_COMMA)
        {
            lexer_next(ps->lx);
            continue;
        }
        break;
    }

    if (saw_varargs)
    {
        Token after = lexer_peek(ps->lx);
        if (after.kind != TK_RPAREN)
        {
            diag_error_at(lexer_source(ps->lx), after.line, after.col,
                          "varargs ('...') must be the final parameter");
            exit(1);
        }
        if (param_count == 0)
        {
            diag_error_at(lexer_source(ps->lx), fun_tok.line, fun_tok.col,
                          "variadic lambdas must have at least one explicit parameter before '...'");
            exit(1);
        }
    }

    expect(ps, TK_RPAREN, ")");
    expect(ps, TK_ARROW, "->");

    Type *ret_type = parse_type_spec(ps);

    const char *prev_fn = ps->current_function_name;
    ps->current_function_name = "<lambda>";
    Node *body = parse_block(ps);
    ps->current_function_name = prev_fn;

    lambda->param_types = param_types;
    lambda->param_names = param_names;
    lambda->param_const_flags = param_const_flags;
    lambda->param_count = param_count;
    lambda->is_varargs = saw_varargs;
    lambda->ret_type = ret_type;
    lambda->body = body;

    return lambda;
}

static Node *parse_primary(Parser *ps)
{
    Token pre = lexer_peek(ps->lx);
    if (parser_peek_struct_literal(ps, pre))
    {
        Type *struct_ty = parse_type_spec(ps);
        Node *literal = parse_brace_initializer(ps);
        literal->type = struct_ty;
        literal->var_type = struct_ty;
        return literal;
    }

    Token t = lexer_next(ps->lx);
    if (t.kind == TK_KW_FUN)
        return parse_lambda_expr(ps, t);
    if (t.kind == TK_KW_IF)
    {
        expect(ps, TK_LPAREN, "(");
        Node *cond = parse_expr(ps);
        expect(ps, TK_RPAREN, ")");
        Node *then_expr = parse_expr(ps);
        Token else_tok = lexer_peek(ps->lx);
        if (else_tok.kind != TK_KW_ELSE)
        {
            diag_error_at(lexer_source(ps->lx), else_tok.line, else_tok.col,
                          "conditional expression requires 'else' branch");
            exit(1);
        }
        lexer_next(ps->lx); 
        Node *else_expr = parse_expr(ps);
        Node *n = new_node(ND_COND);
        n->lhs = cond;
        n->rhs = then_expr;
        n->body = else_expr;
        n->line = t.line;
        n->col = t.col;
        n->src = lexer_source(ps->lx);
        return n;
    }
    if (t.kind == TK_KW_MATCH)
    {
        return parse_match_expr(ps, t);
    }
    if (t.kind == TK_KW_SIZEOF)
    {
        expect(ps, TK_LPAREN, "(");
        Token peek = lexer_peek(ps->lx);
        Node *operand_expr = NULL;
        Type *operand_type = NULL;
        if (is_type_start(ps, peek))
        {
            operand_type = parse_type_spec(ps);
        }
        else
        {
            operand_expr = parse_expr(ps);
        }
        expect(ps, TK_RPAREN, ")");
        Node *n = new_node(ND_SIZEOF);
        n->type = type_i32();
        
        n->lhs = operand_expr;
        n->rhs = NULL;
        n->line = t.line;
        n->col = t.col;
        n->src = lexer_source(ps->lx);
        n->var_type = operand_type;
        return n;
    }
    if (t.kind == TK_KW_ALIGNOF)
    {
        expect(ps, TK_LPAREN, "(");
        Token peek = lexer_peek(ps->lx);
        Node *operand_expr = NULL;
        Type *operand_type = NULL;
        if (is_type_start(ps, peek))
            operand_type = parse_type_spec(ps);
        else
            operand_expr = parse_expr(ps);
        expect(ps, TK_RPAREN, ")");
        Node *n = new_node(ND_ALIGNOF);
        n->lhs = operand_expr;
        n->var_type = operand_type;
        n->type = type_i32();
        n->line = t.line;
        n->col = t.col;
        n->src = lexer_source(ps->lx);
        return n;
    }
    if (t.kind == TK_KW_OFFSETOF)
    {
        expect(ps, TK_LPAREN, "(");
        Type *struct_ty = parse_type_spec(ps);
        expect(ps, TK_COMMA, ",");
        Token maybe_dot = lexer_peek(ps->lx);
        if (maybe_dot.kind == TK_DOT)
            lexer_next(ps->lx);
        Token field_tok = expect(ps, TK_IDENT, "field name");
        expect(ps, TK_RPAREN, ")");
        Node *n = new_node(ND_OFFSETOF);
        n->var_type = struct_ty;
        n->type = type_i32();
        n->line = t.line;
        n->col = t.col;
        n->src = lexer_source(ps->lx);
        n->field_name = dup_token_text(field_tok);
        return n;
    }
    if (t.kind == TK_KW_TYPEOF)
    {
        expect(ps, TK_LPAREN, "(");
        
        Token p = lexer_peek(ps->lx);
        Node *arg_node = NULL;
        Type *arg_type = NULL;
        char *alias_name = NULL;
        if (is_type_start(ps, p))
        {
            if (p.kind == TK_IDENT)
            {
                
                if (alias_find(ps, p.lexeme, p.length) >= 0)
                {
                    alias_name = (char *)xmalloc((size_t)p.length + 1);
                    memcpy(alias_name, p.lexeme, (size_t)p.length);
                    alias_name[p.length] = '\0';
                }
            }
            arg_type = parse_type_spec(ps);
        }
        else
        {
            arg_node = parse_expr(ps);
        }
        expect(ps, TK_RPAREN, ")");
        Node *n = new_node(ND_TYPEOF);
        n->lhs = arg_node;
        n->var_type = arg_type; 
        if (alias_name)
            n->var_ref = alias_name; 
        n->line = t.line;
        n->col = t.col;
        n->src = lexer_source(ps->lx);
        return n;
    }
    if (t.kind == TK_INT)
    {
        Node *n = new_node(ND_INT);
        n->int_val = t.int_val;
        n->int_uval = t.int_uval;
        n->int_is_unsigned = t.int_is_unsigned;
        n->int_width = t.int_width;
        n->line = t.line;
        n->col = t.col;
        n->src = lexer_source(ps->lx);
        return n;
    }
    if (t.kind == TK_FLOAT)
    {
        Node *n = new_node(ND_FLOAT);
        n->float_val = t.float_val;
        n->type = t.float_is_f32 ? type_f32() : type_f64();
        n->line = t.line;
        n->col = t.col;
        n->src = lexer_source(ps->lx);
        return n;
    }
    if (t.kind == TK_STRING)
    {
        Node *result = make_string_node_from_token(ps, t);
        for (;;)
        {
            Token look = lexer_peek(ps->lx);
            if (look.kind != TK_STRING)
                break;
            Token next_tok = lexer_next(ps->lx);
            Node *rhs = make_string_node_from_token(ps, next_tok);
            Node *concat = new_node(ND_ADD);
            concat->lhs = result;
            concat->rhs = rhs;
            concat->line = result->line;
            concat->col = result->col;
            concat->src = result->src;
            result = concat;
        }
        return result;
    }
    if (t.kind == TK_CHAR_LIT)
    {
        Node *n = new_node(ND_INT);
        n->int_val = t.int_val;
        n->int_uval = (uint64_t)t.int_val;
        n->int_width = 0;
        n->type = type_char();
        n->line = t.line;
        n->col = t.col;
        n->src = lexer_source(ps->lx);
        return n;
    }
    if (t.kind == TK_KW_NULL)
    {
        Node *n = new_node(ND_NULL);
        static Type *null_ty = NULL;
        if (!null_ty)
            null_ty = type_ptr(type_void());
        n->type = null_ty;
        n->line = t.line;
        n->col = t.col;
        n->src = lexer_source(ps->lx);
        return n;
    }
    if (t.kind == TK_LPAREN)
    {
        Node *inner = parse_expr(ps);
        expect(ps, TK_RPAREN, ")");
        return inner;
    }
    if (t.kind == TK_LBRACKET)
    {
        Token next = lexer_peek(ps->lx);
        if (next.kind == TK_RBRACKET)
        {
            lexer_next(ps->lx);
            Node *init = new_node(ND_INIT_LIST);
            init->line = t.line;
            init->col = t.col;
            init->src = lexer_source(ps->lx);
            init->init.is_zero = 1;
            init->init.is_array_literal = 1;
            init->init.count = 0;
            init->init.elems = NULL;
            init->init.designators = NULL;
            return init;
        }
        return parse_metadata_call(ps, t);
    }
    if (t.kind == TK_IDENT)
    {
        if (t.length == 4 && strncmp(t.lexeme, "true", 4) == 0)
        {
            Node *n = new_node(ND_INT);
            n->int_val = 1;
            n->int_uval = 1;
            n->int_width = 0;
            n->type = type_bool();
            n->line = t.line;
            n->col = t.col;
            n->src = lexer_source(ps->lx);
            return n;
        }
        if (t.length == 5 && strncmp(t.lexeme, "false", 5) == 0)
        {
            Node *n = new_node(ND_INT);
            n->int_val = 0;
            n->int_uval = 0;
            n->int_width = 0;
            n->type = type_bool();
            n->line = t.line;
            n->col = t.col;
            n->src = lexer_source(ps->lx);
            return n;
        }
        if (t.length == 8 && strncmp(t.lexeme, "va_start", 8) == 0)
        {
            expect(ps, TK_LPAREN, "(");
            expect(ps, TK_RPAREN, ")");
            Node *n = new_node(ND_VA_START);
            n->line = t.line;
            n->col = t.col;
            n->src = lexer_source(ps->lx);
            n->type = type_va_list();
            return n;
        }
        if (t.length == 6 && strncmp(t.lexeme, "va_end", 6) == 0)
        {
            expect(ps, TK_LPAREN, "(");
            Node *list_expr = parse_expr(ps);
            expect(ps, TK_RPAREN, ")");
            Node *n = new_node(ND_VA_END);
            n->lhs = list_expr;
            n->line = t.line;
            n->col = t.col;
            n->src = lexer_source(ps->lx);
            n->type = type_void();
            return n;
        }
        if (t.length == 6 && strncmp(t.lexeme, "va_arg", 6) == 0)
        {
            expect(ps, TK_LPAREN, "(");
            Node *list_expr = parse_expr(ps);
            expect(ps, TK_COMMA, ",");
            Type *target_type = parse_type_spec(ps);
            expect(ps, TK_RPAREN, ")");
            Node *n = new_node(ND_VA_ARG);
            n->lhs = list_expr;
            n->var_type = target_type;
            n->line = t.line;
            n->col = t.col;
            n->src = lexer_source(ps->lx);
            return n;
        }

        if (t.length == 12 && strncmp(t.lexeme, "__FUNCTION__", 12) == 0)
        {
            if (!ps->current_function_name)
            {
                diag_error_at(lexer_source(ps->lx), t.line, t.col,
                              "__FUNCTION__ is only valid within a function body");
                exit(1);
            }
            Node *n = new_node(ND_STRING);
            size_t len = strlen(ps->current_function_name);
            char *copy = (char *)xmalloc(len + 1);
            memcpy(copy, ps->current_function_name, len + 1);
            n->str_data = copy;
            n->str_len = (int)len;
            n->line = t.line;
            n->col = t.col;
            n->src = lexer_source(ps->lx);
            return n;
        }
        
        int ev = 0;
        if (enum_const_get(ps, t.lexeme, t.length, &ev))
        {
            Node *n = new_node(ND_INT);
            n->int_val = ev;
            n->int_uval = (uint64_t)ev;
            n->int_width = 0;
            n->line = t.line;
            n->col = t.col;
            n->src = lexer_source(ps->lx);
            return n;
        }
        
        Token p = lexer_peek(ps->lx);
        Type **call_type_args = NULL;
        int call_type_arg_count = 0;
        int type_arg_line = 0;
        int type_arg_col = 0;
        if (p.kind == TK_LT && parser_call_type_args_ahead(ps))
        {
            type_arg_line = p.line;
            type_arg_col = p.col;
            call_type_args = parser_parse_call_type_args(ps, &call_type_arg_count);
            p = lexer_peek(ps->lx);
        }
        if (p.kind == TK_LPAREN)
        {
            lexer_next(ps->lx); 
            
            Node **args = NULL;
            int argc = 0, cap = 0;
            Token nxt = lexer_peek(ps->lx);
            if (nxt.kind != TK_RPAREN)
            {
                for (;;)
                {
                    Node *e = parse_expr(ps);
                    if (argc == cap)
                    {
                        cap = cap ? cap * 2 : 4;
                        args = (Node **)realloc(args, sizeof(Node *) * cap);
                    }
                    args[argc++] = e;
                    Token c = lexer_peek(ps->lx);
                    if (c.kind == TK_COMMA)
                    {
                        lexer_next(ps->lx);
                        continue;
                    }
                    break;
                }
            }
            expect(ps, TK_RPAREN, ")");
            Node *call = new_node(ND_CALL);
            
            char *nm = (char *)xmalloc((size_t)t.length + 1);
            memcpy(nm, t.lexeme, (size_t)t.length);
            nm[t.length] = '\0';
            call->call_name = nm;
            call->args = args;
            call->arg_count = argc;
            call->call_type_args = call_type_args;
            call->call_type_arg_count = call_type_arg_count;
            call->line = t.line;
            call->col = t.col;
            call->src = lexer_source(ps->lx);
            Node *target = new_node(ND_VAR);
            char *target_name = (char *)xmalloc((size_t)t.length + 1);
            memcpy(target_name, t.lexeme, (size_t)t.length);
            target_name[t.length] = '\0';
            target->var_ref = target_name;
            target->line = t.line;
            target->col = t.col;
            target->src = lexer_source(ps->lx);
            call->lhs = target;
            return call;
        }
        if (call_type_args)
        {
            diag_error_at(lexer_source(ps->lx), type_arg_line, type_arg_col,
                          "explicit type arguments must be followed by '(' in a call expression");
            exit(1);
        }
        
        Node *v = new_node(ND_VAR);
        char *nm = (char *)xmalloc((size_t)t.length + 1);
        memcpy(nm, t.lexeme, (size_t)t.length);
        nm[t.length] = '\0';
        v->var_ref = nm;
        v->line = t.line;
        v->col = t.col;
        v->src = lexer_source(ps->lx);
        return v;
    }
    diag_error_at(lexer_source(ps->lx), t.line, t.col,
                  "expected expression; got token kind=%d", t.kind);
    exit(1);
}


static void parse_enum_decl(Parser *ps, int is_exposed);
static void parse_struct_decl(Parser *ps, int is_exposed, int is_union, int is_packed);
static int parser_call_type_args_ahead(Parser *ps)
{
    if (!ps)
        return 0;
    Token first = lexer_peek(ps->lx);
    if (first.kind != TK_LT)
        return 0;
    int depth = 0;
    int paren_depth = 0;
    int bracket_depth = 0;
    int offset = 0;
    while (1)
    {
        Token tok = lexer_peek_n(ps->lx, offset);
        if (offset == 0)
        {
            if (tok.kind != TK_LT)
                return 0;
            depth = 1;
            offset++;
            continue;
        }
        switch (tok.kind)
        {
        case TK_LT:
            depth++;
            break;
        case TK_GT:
            depth--;
            break;
        case TK_SHR:
            depth -= 2;
            break;
        case TK_LPAREN:
            paren_depth++;
            break;
        case TK_RPAREN:
            if (paren_depth > 0)
                paren_depth--;
            else if (depth > 0 && bracket_depth == 0)
                return 0;
            break;
        case TK_LBRACKET:
            bracket_depth++;
            break;
        case TK_RBRACKET:
            if (bracket_depth > 0)
                bracket_depth--;
            else if (depth > 0 && paren_depth == 0)
                return 0;
            break;
        case TK_EOF:
        case TK_SEMI:
            return 0;
        case TK_LBRACE:
        case TK_RBRACE:
            if (depth > 0 && paren_depth == 0 && bracket_depth == 0)
                return 0;
            break;
        default:
            break;
        }
        if (depth <= 0)
        {
            if (depth < 0)
                return 0;
            Token after = lexer_peek_n(ps->lx, offset + 1);
            return after.kind == TK_LPAREN;
        }
        offset++;
    }
}

static Type **parser_parse_call_type_args(Parser *ps, int *out_count)
{
    if (!ps)
        return NULL;
    Token lt = lexer_next(ps->lx);
    if (lt.kind != TK_LT)
    {
        diag_error_at(lexer_source(ps->lx), lt.line, lt.col,
                      "internal parser error: expected '<' before type arguments");
        exit(1);
    }
    Token next = lexer_peek(ps->lx);
    if (next.kind == TK_GT)
    {
        diag_error_at(lexer_source(ps->lx), next.line, next.col,
                      "type argument list cannot be empty");
        exit(1);
    }
    Type **args = NULL;
    int count = 0;
    int cap = 0;
    while (1)
    {
        Type *arg_ty = parse_type_spec(ps);
        if (count == cap)
        {
            cap = cap ? cap * 2 : 4;
            Type **grown = (Type **)realloc(args, (size_t)cap * sizeof(Type *));
            if (!grown)
            {
                diag_error("out of memory while parsing type arguments");
                exit(1);
            }
            args = grown;
        }
        args[count++] = arg_ty;
        Token sep = lexer_peek(ps->lx);
        if (sep.kind == TK_COMMA)
        {
            lexer_next(ps->lx);
            continue;
        }
        if (sep.kind == TK_GT)
        {
            lexer_next(ps->lx);
            break;
        }
        diag_error_at(lexer_source(ps->lx), sep.line, sep.col,
                      "expected ',' or '>' in type argument list");
        exit(1);
    }
    if (out_count)
        *out_count = count;
    return args;
}
static Node *parse_postfix_suffixes(Parser *ps, Node *expr)
{
    Node *e = expr;
    Type **pending_type_args = NULL;
    int pending_type_arg_count = 0;
    int pending_type_arg_line = 0;
    int pending_type_arg_col = 0;
    for (;;)
    {
        Token p = lexer_peek(ps->lx);
        if (pending_type_args && p.kind != TK_LPAREN)
        {
            diag_error_at(lexer_source(ps->lx), pending_type_arg_line, pending_type_arg_col,
                          "explicit type arguments must be immediately followed by '(' in a call expression");
            exit(1);
        }
        if (p.kind == TK_LT && parser_call_type_args_ahead(ps))
        {
            if (pending_type_args)
            {
                diag_error_at(lexer_source(ps->lx), p.line, p.col,
                              "duplicate explicit type argument lists before call");
                exit(1);
            }
            pending_type_arg_line = p.line;
            pending_type_arg_col = p.col;
            pending_type_args = parser_parse_call_type_args(ps, &pending_type_arg_count);
            continue;
        }
        if (p.kind == TK_LT && e && e->kind == ND_VAR && e->var_ref)
        {
            int name_len = (int)strlen(e->var_ref);
            struct BundleTemplate *templ = parser_find_bundle_template(ps, e->var_ref, name_len);
            if (templ)
            {
                int type_arg_count = 0;
                Type **type_args = parser_parse_type_arg_list(ps, &type_arg_count);
                Type *inst_type = parser_instantiate_bundle_template(ps,
                                                                     templ,
                                                                     type_args,
                                                                     type_arg_count,
                                                                     p.line,
                                                                     p.col);
                if (!inst_type || !inst_type->struct_name)
                {
                    diag_error_at(lexer_source(ps->lx), p.line, p.col,
                                  "failed to resolve template bundle type arguments");
                    exit(1);
                }
                free((void *)e->var_ref);
                e->var_ref = xstrdup(inst_type->struct_name);
                continue;
            }
        }
        if (p.kind == TK_PLUSPLUS)
        {
            lexer_next(ps->lx);
            Node *n = new_node(ND_POSTINC);
            n->lhs = e;
            n->line = p.line;
            n->col = p.col;
            n->src = lexer_source(ps->lx);
            e = n;
            continue;
        }
        if (p.kind == TK_MINUSMINUS)
        {
            lexer_next(ps->lx);
            Node *n = new_node(ND_POSTDEC);
            n->lhs = e;
            n->line = p.line;
            n->col = p.col;
            n->src = lexer_source(ps->lx);
            e = n;
            continue;
        }
        if (p.kind == TK_LBRACKET)
        {
            lexer_next(ps->lx);
            Node *idx = parse_expr(ps);
            expect(ps, TK_RBRACKET, "]");
            Node *ix = new_node(ND_INDEX);
            ix->lhs = e;
            ix->rhs = idx;
            ix->line = p.line;
            ix->col = p.col;
            ix->src = lexer_source(ps->lx);
            e = ix;
            continue;
        }
        if (p.kind == TK_KW_AS)
        {
            lexer_next(ps->lx);
            Token next = lexer_peek(ps->lx);
            if (next.kind == TK_KW_TYPEOF)
            {
                Node *type_expr = parse_expr(ps);
                Node *cs = new_node(ND_CAST);
                cs->lhs = e;
                cs->type = NULL;
                cs->type_expr = type_expr;
                cs->line = p.line;
                cs->col = p.col;
                cs->src = lexer_source(ps->lx);
                e = cs;
                continue;
            }
            else
            {
                Type *ty = parse_type_spec(ps);
                ty = parser_decl_canonicalize_bundle_reference(ty);
                Node *cs = new_node(ND_CAST);
                cs->lhs = e;
                cs->type = ty;
                cs->line = p.line;
                cs->col = p.col;
                cs->src = lexer_source(ps->lx);
                e = cs;
                continue;
            }
        }
        if (p.kind == TK_ACCESS || p.kind == TK_DOT || p.kind == TK_ARROW)
        {
            if (p.kind == TK_ACCESS && ps->suppress_access_for_match > 0)
                break;
            if (p.kind == TK_ARROW && ps->suppress_arrow_member_for_throw > 0)
                break;
            Token op = lexer_next(ps->lx);
            Token field = expect(ps, TK_IDENT, "member name");

            if (op.kind == TK_DOT && e && e->kind == ND_VAR && e->var_ref)
            {
                int bundle_name_len = (int)strlen(e->var_ref);
                const struct BundleStaticMember *static_member =
                    parser_find_bundle_static_member(ps, e->var_ref, bundle_name_len,
                                                     field.lexeme, field.length);
                if (static_member && static_member->symbol_name)
                {
                    Node *sv = new_node(ND_VAR);
                    sv->var_ref = xstrdup(static_member->symbol_name);
                    sv->var_is_global = 1;
                    sv->var_is_function = static_member->is_function ? 1 : 0;
                    sv->var_is_const = static_member->is_function ? 1 : (static_member->is_const ? 1 : 0);
                    sv->line = field.line;
                    sv->col = field.col;
                    sv->src = lexer_source(ps->lx);
                    e = sv;
                    continue;
                }
            }
            
            if (op.kind == TK_ACCESS && e->kind == ND_VAR)
            {
                const char *base_name = e->var_ref;
                int base_len = (int)strlen(base_name);
                if (enum_type_find(ps, base_name, base_len) >= 0)
                {
                    int combo_len = base_len + 2 + field.length;
                    char *combo = (char *)xmalloc((size_t)combo_len + 1);
                    memcpy(combo, base_name, (size_t)base_len);
                    combo[base_len] = '=';
                    combo[base_len + 1] = '>';
                    memcpy(combo + base_len + 2, field.lexeme, (size_t)field.length);
                    combo[combo_len] = '\0';
                    int ev = 0;
                    if (enum_const_get(ps, combo, combo_len, &ev))
                    {
                        Node *n = new_node(ND_INT);
                        n->int_val = ev;
                        n->int_uval = (uint64_t)ev;
                        n->int_width = 0;
                        n->line = field.line;
                        n->col = field.col;
                        n->src = lexer_source(ps->lx);
                        free(combo);
                        e = n;
                        continue;
                    }
                    free(combo);
                }
            }
            Node *m = new_node(ND_MEMBER);
            m->lhs = e;
            char *nm = (char *)xmalloc((size_t)field.length + 1);
            memcpy(nm, field.lexeme, (size_t)field.length);
            nm[field.length] = '\0';
            m->field_name = nm;
            m->is_pointer_deref = (op.kind == TK_ACCESS || op.kind == TK_ARROW);
            m->line = field.line;
            m->col = field.col;
            m->src = lexer_source(ps->lx);
            e = m;
            continue;
        }
        if (p.kind == TK_LPAREN)
        {
            lexer_next(ps->lx); 
            Node **args = NULL;
            int argc = 0, cap = 0;
            Token nxt = lexer_peek(ps->lx);
            if (nxt.kind != TK_RPAREN)
            {
                for (;;)
                {
                    Node *arg = parse_expr(ps);
                    if (argc == cap)
                    {
                        cap = cap ? cap * 2 : 4;
                        args = (Node **)realloc(args, sizeof(Node *) * cap);
                        if (!args)
                        {
                            diag_error("out of memory while parsing call arguments");
                            exit(1);
                        }
                    }
                    args[argc++] = arg;
                    Token comma = lexer_peek(ps->lx);
                    if (comma.kind == TK_COMMA)
                    {
                        lexer_next(ps->lx);
                        continue;
                    }
                    break;
                }
            }
            expect(ps, TK_RPAREN, ")");
            char *call_name = call_name_from_expr(e);
            Node *call = new_node(ND_CALL);
            call->lhs = e;
            call->args = args;
            call->arg_count = argc;
            call->call_name = call_name;
            call->call_type_args = pending_type_args;
            call->call_type_arg_count = pending_type_arg_count;
            call->line = e ? e->line : p.line;
            call->col = e ? e->col : p.col;
            call->src = lexer_source(ps->lx);
            if (e && e->kind == ND_LAMBDA)
                call->kind = ND_LAMBDA_CALL;
            e = call;
            pending_type_args = NULL;
            pending_type_arg_count = 0;
            continue;
        }
        break;
    }
    if (pending_type_args)
    {
        diag_error_at(lexer_source(ps->lx), pending_type_arg_line, pending_type_arg_col,
                      "explicit type arguments must be followed by '(' in a call expression");
        exit(1);
    }
    return e;
}

static Node *parse_postfix(Parser *ps)
{
    Node *e = parse_primary(ps);
    return parse_postfix_suffixes(ps, e);
}

static Node *parse_managed_array_adapter(Parser *ps, Token open_paren)
{
    expect(ps, TK_KW_MANAGED, "managed");
    expect(ps, TK_LBRACKET, "[");
    expect(ps, TK_RBRACKET, "]");
    expect(ps, TK_COLON, ":");
    expect(ps, TK_DOT, ".");

    Token field = expect(ps, TK_IDENT, "managed array property");
    if (!(field.length == 6 && strncmp(field.lexeme, "length", 6) == 0))
    {
        diag_error_at(lexer_source(ps->lx), field.line, field.col,
                      "only '.length' is supported in managed array adapters");
        exit(1);
    }

    expect(ps, TK_ASSIGN, "=");
    Node *length_expr = parse_expr(ps);
    expect(ps, TK_RPAREN, ")");

    Node *operand = parse_unary(ps);
    Node *adapt = new_node(ND_MANAGED_ARRAY_ADAPT);
    adapt->lhs = operand;
    adapt->rhs = length_expr;
    adapt->line = open_paren.line;
    adapt->col = open_paren.col;
    adapt->src = lexer_source(ps->lx);
    return adapt;
}

static Node *parse_unary(Parser *ps)
{
    Token p = lexer_peek(ps->lx);
    if (p.kind == TK_LPAREN)
    {
        Token next = lexer_peek_n(ps->lx, 1);
        if (next.kind == TK_KW_MANAGED)
        {
            lexer_next(ps->lx); 
            return parse_managed_array_adapter(ps, p);
        }
        if (is_type_start(ps, next))
        {
            lexer_next(ps->lx); 
            Type *ty = parse_type_spec(ps);
            ty = parser_decl_canonicalize_bundle_reference(ty);
            expect(ps, TK_RPAREN, ")");
            Node *operand = parse_unary(ps);
            Node *cs = new_node(ND_CAST);
            cs->lhs = operand;
            cs->type = ty;
            cs->line = p.line;
            cs->col = p.col;
            cs->src = lexer_source(ps->lx);
            return cs;
        }
    }
    if (p.kind == TK_PLUS)
    {
        
        lexer_next(ps->lx);
        return parse_unary(ps);
    }
    if (p.kind == TK_MINUS)
    {
        
        lexer_next(ps->lx);
        Node *rv = parse_unary(ps);
        Node *n = new_node(ND_NEG);
        n->lhs = rv;
        n->line = p.line;
        n->col = p.col;
        n->src = lexer_source(ps->lx);
        return n;
    }
    if (p.kind == TK_PLUSPLUS)
    {
        lexer_next(ps->lx);
        Node *lv = parse_unary(ps);
        Node *n = new_node(ND_PREINC);
        n->lhs = lv;
        n->line = p.line;
        n->col = p.col;
        n->src = lexer_source(ps->lx);
        return n;
    }
    if (p.kind == TK_MINUSMINUS)
    {
        lexer_next(ps->lx);
        Node *lv = parse_unary(ps);
        Node *n = new_node(ND_PREDEC);
        n->lhs = lv;
        n->line = p.line;
        n->col = p.col;
        n->src = lexer_source(ps->lx);
        return n;
    }
    if (p.kind == TK_AMP)
    {
        lexer_next(ps->lx);
        Node *lv = parse_unary(ps);
        Node *n = new_node(ND_ADDR);
        n->lhs = lv;
        n->line = p.line;
        n->col = p.col;
        n->src = lexer_source(ps->lx);
        return n;
    }
    if (p.kind == TK_KW_REF)
    {
        parser_require_h27(ps, p, "ref address-of expression");
        lexer_next(ps->lx);
        Node *lv = parse_unary(ps);
        Node *n = new_node(ND_ADDR);
        n->lhs = lv;
        n->line = p.line;
        n->col = p.col;
        n->src = lexer_source(ps->lx);
        return n;
    }
    if (p.kind == TK_STAR)
    {
        lexer_next(ps->lx);
        Node *operand = parse_unary(ps);
        Node *deref = new_node(ND_DEREF);
        deref->lhs = operand;
        deref->line = p.line;
        deref->col = p.col;
        deref->src = lexer_source(ps->lx);
        return parse_postfix_suffixes(ps, deref);
    }
    if (p.kind == TK_TILDE)
    {
        lexer_next(ps->lx);
        Node *operand = parse_unary(ps);
        Node *n = new_node(ND_BITNOT);
        n->lhs = operand;
        n->line = p.line;
        n->col = p.col;
        n->src = lexer_source(ps->lx);
        return n;
    }
    if (p.kind == TK_BANG)
    {
        lexer_next(ps->lx);
        Node *operand = parse_unary(ps);
        Node *n = new_node(ND_LNOT);
        n->lhs = operand;
        n->line = p.line;
        n->col = p.col;
        n->src = lexer_source(ps->lx);
        return n;
    }
    if (p.kind == TK_KW_NEW)
    {
        parser_require_h27(ps, p, "new expression");
        lexer_next(ps->lx);
        Type *ty = parse_type_base(ps);
        Node *n = new_node(ND_NEW);
        
        
        
        if (ty && ty->kind == TY_ARRAY && ty->array.elem && !ty->array.is_unsized)
        {
            n->type = type_ptr(ty->array.elem);
            Node *count = new_node(ND_INT);
            count->int_val = ty->array.length;
            count->int_uval = (uint64_t)ty->array.length;
            count->int_width = 0;
            count->type = type_i32(); 
            n->lhs = count;
        }
        else
        {
            
            n->type = type_ptr(ty);
        }
        
        Token nxt = lexer_peek(ps->lx);
        if (nxt.kind == TK_LBRACKET)
        {
            lexer_next(ps->lx); 
            Node *count = parse_expr(ps);
            expect(ps, TK_RBRACKET, "]");
            n->lhs = count; 
            nxt = lexer_peek(ps->lx);
            if (nxt.kind == TK_LPAREN)
            {
                diag_error_at(lexer_source(ps->lx), nxt.line, nxt.col,
                              "new array allocation cannot also include constructor arguments");
                exit(1);
            }
        }

        if (nxt.kind == TK_LPAREN)
        {
            lexer_next(ps->lx);
            Node **args = NULL;
            int argc = 0;
            int cap = 0;
            Token arg_start = lexer_peek(ps->lx);
            if (arg_start.kind != TK_RPAREN)
            {
                for (;;)
                {
                    Node *arg = parse_expr(ps);
                    if (argc == cap)
                    {
                        cap = cap ? cap * 2 : 4;
                        Node **grown = (Node **)realloc(args, sizeof(Node *) * (size_t)cap);
                        if (!grown)
                        {
                            diag_error("out of memory while parsing new-expression arguments");
                            exit(1);
                        }
                        args = grown;
                    }
                    args[argc++] = arg;
                    Token comma = lexer_peek(ps->lx);
                    if (comma.kind == TK_COMMA)
                    {
                        lexer_next(ps->lx);
                        continue;
                    }
                    break;
                }
            }
            expect(ps, TK_RPAREN, ")");
            n->args = args;
            n->arg_count = argc;
        }
        n->line = p.line;
        n->col = p.col;
        n->src = lexer_source(ps->lx);
        return n;
    }
    return parse_postfix(ps);
}

static Node *parse_mul(Parser *ps)
{
    Node *lhs = parse_unary(ps);
    for (;;)
    {
        Token p = lexer_peek(ps->lx);
        if (p.kind == TK_STAR)
        {
            Token op = lexer_next(ps->lx);
            Node *rhs = parse_unary(ps);
            Node *mul = new_node(ND_MUL);
            mul->lhs = lhs;
            mul->rhs = rhs;
            mul->line = op.line;
            mul->col = op.col;
            mul->src = lexer_source(ps->lx);
            lhs = mul;
            continue;
        }
        if (p.kind == TK_SLASH)
        {
            Token op = lexer_next(ps->lx);
            Node *rhs = parse_unary(ps);
            Node *div = new_node(ND_DIV);
            div->lhs = lhs;
            div->rhs = rhs;
            div->line = op.line;
            div->col = op.col;
            div->src = lexer_source(ps->lx);
            lhs = div;
            continue;
        }
        if (p.kind == TK_PERCENT)
        {
            Token op = lexer_next(ps->lx);
            Node *rhs = parse_unary(ps);
            Node *mod = new_node(ND_MOD);
            mod->lhs = lhs;
            mod->rhs = rhs;
            mod->line = op.line;
            mod->col = op.col;
            mod->src = lexer_source(ps->lx);
            lhs = mod;
            continue;
        }
        break;
    }
    return lhs;
}

static Node *parse_add(Parser *ps)
{
    Node *lhs = parse_mul(ps);
    for (;;)
    {
        Token p = lexer_peek(ps->lx);
        if (p.kind == TK_PLUS)
        {
            Token op = lexer_next(ps->lx);
            Node *rhs = parse_mul(ps);
            Node *add = new_node(ND_ADD);
            add->lhs = lhs;
            add->rhs = rhs;
            add->line = op.line;
            add->col = op.col;
            add->src = lexer_source(ps->lx);
            lhs = add;
            continue;
        }
        if (p.kind == TK_MINUS)
        {
            Token op = lexer_next(ps->lx);
            Node *rhs = parse_mul(ps);
            Node *sub = new_node(ND_SUB);
            sub->lhs = lhs;
            sub->rhs = rhs;
            sub->line = op.line;
            sub->col = op.col;
            sub->src = lexer_source(ps->lx);
            lhs = sub;
            continue;
        }
        break;
    }
    return lhs;
}

static Node *parse_shift(Parser *ps)
{
    Node *lhs = parse_add(ps);
    for (;;)
    {
        Token p = lexer_peek(ps->lx);
        if (p.kind == TK_SHL || p.kind == TK_SHR)
        {
            Token op = lexer_next(ps->lx);
            Node *rhs = parse_add(ps);
            Node *n = new_node(op.kind == TK_SHL ? ND_SHL : ND_SHR);
            n->lhs = lhs;
            n->rhs = rhs;
            n->line = op.line;
            n->col = op.col;
            n->src = lexer_source(ps->lx);
            lhs = n;
            continue;
        }
        break;
    }
    return lhs;
}

static Node *parse_rel(Parser *ps)
{
    Node *lhs = parse_shift(ps);
    for (;;)
    {
        Token p = lexer_peek(ps->lx);
        if (p.kind == TK_KW_IS)
        {
            parser_require_h27(ps, p, "'is' type-test operator");
            Token op = lexer_next(ps->lx);
            Type *rhs_ty = parse_type_spec(ps);
            rhs_ty = parser_decl_canonicalize_bundle_reference(rhs_ty);
            Node *n = new_node(ND_IS);
            n->lhs = lhs;
            n->is_type = rhs_ty;
            n->line = op.line;
            n->col = op.col;
            n->src = lexer_source(ps->lx);
            lhs = n;
            continue;
        }
        if (p.kind == TK_GT || p.kind == TK_LT || p.kind == TK_LTE || p.kind == TK_GTE)
        {
            Token op = lexer_next(ps->lx);
            Node *rhs = parse_shift(ps);
            Node *n = NULL;
            if (op.kind == TK_GT)
                n = new_node(ND_GT_EXPR);
            else if (op.kind == TK_LT)
                n = new_node(ND_LT);
            else if (op.kind == TK_LTE)
                n = new_node(ND_LE);
            else
                n = new_node(ND_GE);
            n->lhs = lhs;
            n->rhs = rhs;
            n->line = op.line;
            n->col = op.col;
            n->src = lexer_source(ps->lx);
            lhs = n;
            continue;
        }
        break;
    }
    return lhs;
}
static Node *parse_eq(Parser *ps)
{
    Node *lhs = parse_rel(ps);
    for (;;)
    {
        Token p = lexer_peek(ps->lx);
        if (p.kind == TK_EQEQEQ)
        {
            parser_require_h27(ps, p, "'===' strict equality operator");
            Token op = lexer_next(ps->lx);
            Node *rhs = parse_rel(ps);
            Node *n = new_node(ND_STRICT_EQ);
            n->lhs = lhs;
            n->rhs = rhs;
            n->line = op.line;
            n->col = op.col;
            n->src = lexer_source(ps->lx);
            lhs = n;
            continue;
        }
        if (p.kind == TK_EQEQ)
        {
            Token op = lexer_next(ps->lx);
            Node *rhs = parse_rel(ps);
            Node *n = new_node(ND_EQ);
            n->lhs = lhs;
            n->rhs = rhs;
            n->line = op.line;
            n->col = op.col;
            n->src = lexer_source(ps->lx);
            lhs = n;
            continue;
        }
        if (p.kind == TK_BANGEQ)
        {
            Token op = lexer_next(ps->lx);
            Node *rhs = parse_rel(ps);
            Node *n = new_node(ND_NE);
            n->lhs = lhs;
            n->rhs = rhs;
            n->line = op.line;
            n->col = op.col;
            n->src = lexer_source(ps->lx);
            lhs = n;
            continue;
        }
        break;
    }
    return lhs;
}

static Node *parse_bitand(Parser *ps)
{
    Node *lhs = parse_eq(ps);
    for (;;)
    {
        Token p = lexer_peek(ps->lx);
        if (p.kind == TK_AMP)
        {
            Token op = lexer_next(ps->lx);
            Node *rhs = parse_eq(ps);
            Node *n = new_node(ND_BITAND);
            n->lhs = lhs;
            n->rhs = rhs;
            n->line = op.line;
            n->col = op.col;
            n->src = lexer_source(ps->lx);
            lhs = n;
            continue;
        }
        break;
    }
    return lhs;
}

static Node *parse_bitxor(Parser *ps)
{
    Node *lhs = parse_bitand(ps);
    for (;;)
    {
        Token p = lexer_peek(ps->lx);
        if (p.kind == TK_CARET)
        {
            Token op = lexer_next(ps->lx);
            Node *rhs = parse_bitand(ps);
            Node *n = new_node(ND_BITXOR);
            n->lhs = lhs;
            n->rhs = rhs;
            n->line = op.line;
            n->col = op.col;
            n->src = lexer_source(ps->lx);
            lhs = n;
            continue;
        }
        break;
    }
    return lhs;
}

static Node *parse_bitor(Parser *ps)
{
    Node *lhs = parse_bitxor(ps);
    for (;;)
    {
        Token p = lexer_peek(ps->lx);
        if (p.kind == TK_PIPE)
        {
            Token op = lexer_next(ps->lx);
            Node *rhs = parse_bitxor(ps);
            Node *n = new_node(ND_BITOR);
            n->lhs = lhs;
            n->rhs = rhs;
            n->line = op.line;
            n->col = op.col;
            n->src = lexer_source(ps->lx);
            lhs = n;
            continue;
        }
        break;
    }
    return lhs;
}

static Node *parse_and(Parser *ps)
{
    Node *lhs = parse_bitor(ps);
    for (;;)
    {
        Token p = lexer_peek(ps->lx);
        if (p.kind == TK_ANDAND)
        {
            Token op = lexer_next(ps->lx);
            Node *rhs = parse_bitor(ps);
            Node *n = new_node(ND_LAND);
            n->lhs = lhs;
            n->rhs = rhs;
            n->line = op.line;
            n->col = op.col;
            n->src = lexer_source(ps->lx);
            lhs = n;
            continue;
        }
        break;
    }
    return lhs;
}

static Node *parse_or(Parser *ps)
{
    Node *lhs = parse_and(ps);
    for (;;)
    {
        Token p = lexer_peek(ps->lx);
        if (p.kind == TK_OROR)
        {
            Token op = lexer_next(ps->lx);
            Node *rhs = parse_and(ps);
            Node *n = new_node(ND_LOR);
            n->lhs = lhs;
            n->rhs = rhs;
            n->line = op.line;
            n->col = op.col;
            n->src = lexer_source(ps->lx);
            lhs = n;
            continue;
        }
        break;
    }
    return lhs;
}


static Node *parse_cond(Parser *ps)
{
    Node *cond = parse_or(ps);
    Token q = lexer_peek(ps->lx);
    if (q.kind != TK_QUESTION)
        return cond;
    lexer_next(ps->lx); 
    Node *then_e = parse_expr(ps);
    expect(ps, TK_COLON, ":");
    Node *else_e = parse_cond(ps); 
    Node *n = new_node(ND_COND);
    n->lhs = cond;    
    n->rhs = then_e;  
    n->body = else_e; 
    n->line = q.line;
    n->col = q.col;
    n->src = lexer_source(ps->lx);
    return n;
}

static Node *parse_assign(Parser *ps)
{
    Node *lhs = parse_cond(ps);
    Token p = lexer_peek(ps->lx);
    NodeKind assign_kind = ND_ASSIGN;
    switch (p.kind)
    {
    case TK_ASSIGN:
        assign_kind = ND_ASSIGN;
        break;
    case TK_PLUSEQ:
        assign_kind = ND_ADD_ASSIGN;
        break;
    case TK_MINUSEQ:
        assign_kind = ND_SUB_ASSIGN;
        break;
    case TK_STAREQ:
        assign_kind = ND_MUL_ASSIGN;
        break;
    case TK_SLASHEQ:
        assign_kind = ND_DIV_ASSIGN;
        break;
    case TK_PERCENTEQ:
        assign_kind = ND_MOD_ASSIGN;
        break;
    case TK_ANDEQ:
        assign_kind = ND_BITAND_ASSIGN;
        break;
    case TK_OREQ:
        assign_kind = ND_BITOR_ASSIGN;
        break;
    case TK_XOREQ:
        assign_kind = ND_BITXOR_ASSIGN;
        break;
    case TK_SHLEQ:
        assign_kind = ND_SHL_ASSIGN;
        break;
    case TK_SHREQ:
        assign_kind = ND_SHR_ASSIGN;
        break;
    default:
        return lhs;
    }

    Token op = lexer_next(ps->lx);
    Node *rhs = parse_expr(ps);
    Node *as = new_node(assign_kind);
    as->lhs = lhs;
    as->rhs = rhs;
    as->line = op.line;
    as->col = op.col;
    as->src = lexer_source(ps->lx);
    return as;
}

static Node *parse_expr(Parser *ps) { return parse_assign(ps); }

static Node *parse_initializer(Parser *ps)
{
    Token t = lexer_peek(ps->lx);
    if (t.kind != TK_LBRACE)
        return parse_expr(ps);
    lexer_next(ps->lx); 
    Node *init = new_node(ND_INIT_LIST);
    init->line = t.line;
    init->col = t.col;
    init->src = lexer_source(ps->lx);
    Node **elems = NULL;
    const char **designators = NULL;
    int count = 0, cap = 0;
    Token nxt = lexer_peek(ps->lx);
    if (nxt.kind == TK_RBRACE)
    {
        lexer_next(ps->lx);
        init->init.is_zero = 1;
        init->init.elems = NULL;
        init->init.designators = NULL;
        init->init.count = 0;
        return init;
    }
    while (1)
    {
        const char *desig = NULL;
        Token look = lexer_peek(ps->lx);
        if (look.kind == TK_DOT)
        {
            lexer_next(ps->lx);
            Token fld = expect(ps, TK_IDENT, "field designator");
            char *nm = (char *)xmalloc((size_t)fld.length + 1);
            memcpy(nm, fld.lexeme, (size_t)fld.length);
            nm[fld.length] = '\0';
            desig = nm;
            expect(ps, TK_ASSIGN, "=");
        }
        Node *elem = NULL;
        Token look2 = lexer_peek(ps->lx);
        if (look2.kind == TK_LBRACE)
            elem = parse_initializer(ps);
        else
            elem = parse_expr(ps);
        if (count == cap)
        {
            cap = cap ? cap * 2 : 4;
            elems = (Node **)realloc(elems, sizeof(Node *) * cap);
            designators = (const char **)realloc(designators, sizeof(char *) * cap);
        }
        elems[count] = elem;
        designators[count] = desig;
        count++;
        Token sep = lexer_peek(ps->lx);
        if (sep.kind == TK_COMMA)
        {
            lexer_next(ps->lx);
            Token after = lexer_peek(ps->lx);
            if (after.kind == TK_RBRACE)
                break;
            continue;
        }
        break;
    }
    expect(ps, TK_RBRACE, "}");
    init->init.elems = elems;
    init->init.designators = designators;
    init->init.count = count;
    if (count == 0)
        init->init.is_zero = 1;
    else if (count == 1 && !designators[0] && elems[0]->kind == ND_INT && elems[0]->int_val == 0)
        init->init.is_zero = 1;
    return init;
}

static Node *parse_inferred_var_decl(Parser *ps, int expect_semicolon)
{
    Token kw = expect(ps, TK_KW_VAR, "var");
    Token name = expect(ps, TK_IDENT, "identifier");

    Node *decl = new_node(ND_VAR_DECL);
    char *nm = (char *)xmalloc((size_t)name.length + 1);
    memcpy(nm, name.lexeme, (size_t)name.length);
    nm[name.length] = '\0';
    decl->var_name = nm;
    decl->var_is_const = 0;
    decl->var_is_array = 0;
    decl->var_is_function = 0;
    decl->var_is_inferred = 1;
    decl->line = name.line;
    decl->col = name.col;
    decl->src = lexer_source(ps->lx);

    Token assign = lexer_peek(ps->lx);
    if (assign.kind != TK_ASSIGN)
    {
        diag_error_at(lexer_source(ps->lx), name.line, name.col,
                      "'var' declarations require an initializer");
        exit(1);
    }
    lexer_next(ps->lx); 
    decl->rhs = parse_initializer(ps);

    if (expect_semicolon)
        expect(ps, TK_SEMI, ";");
    return decl;
}



static Node *parse_stmt(Parser *ps)
{
    Token t = lexer_peek(ps->lx);
    int starts_with_member_expr = stmt_starts_with_qualified_member_expr(ps);
    if (t.kind == TK_KW_MANAGED)
    {
        parser_require_h27(ps, t, "managed block");
        lexer_next(ps->lx);
        Token next = lexer_peek(ps->lx);
        if (next.kind != TK_LBRACE)
        {
            diag_error_at(lexer_source(ps->lx), next.line, next.col,
                          "expected '{' after 'managed'");
            exit(1);
        }
        ps->managed_scope_depth++;
        Node *blk = parse_block(ps);
        ps->managed_scope_depth--;
        if (blk)
            blk->is_managed = 1;
        return blk;
    }
    if (t.kind == TK_KW_UNMANAGED)
    {
        parser_require_h27(ps, t, "unmanaged block");
        lexer_next(ps->lx);
        Token next = lexer_peek(ps->lx);
        if (next.kind != TK_LBRACE)
        {
            diag_error_at(lexer_source(ps->lx), next.line, next.col,
                          "expected '{' after 'unmanaged'");
            exit(1);
        }
        ps->unmanaged_scope_depth++;
        Node *blk = parse_block(ps);
        ps->unmanaged_scope_depth--;
        return blk;
    }
    if (t.kind == TK_LBRACE)
        return parse_block(ps);
    if (t.kind == TK_KW_IF)
    {
        lexer_next(ps->lx);
        expect(ps, TK_LPAREN, "(");
        Node *cond = parse_expr(ps);
        expect(ps, TK_RPAREN, ")");
        Node *thenb = parse_stmt(ps); 
        Node *elseb = NULL;
        Token e = lexer_peek(ps->lx);
        if (e.kind == TK_KW_ELSE)
        {
            lexer_next(ps->lx);
            elseb = parse_stmt(ps);
        }
        Node *n = new_node(ND_IF);
        n->lhs = cond;
        n->rhs = thenb;
        n->body = elseb;
        n->line = t.line;
        n->col = t.col;
        n->src = lexer_source(ps->lx);
        return n;
    }
    if (t.kind == TK_KW_WHILE)
    {
        return parse_while(ps);
    }
    if (t.kind == TK_KW_FOR)
    {
        return parse_for(ps);
    }
    if (token_is_ident_text(t, "foreach"))
    {
        parser_require_h28(ps, t, "foreach loop");
        return parse_foreach(ps);
    }
    if (token_is_ident_text(t, "yield"))
    {
        parser_require_h28(ps, t, "yield statement");
        if (!ps->current_function_name || strcmp(ps->current_function_name, "iterate") != 0)
        {
            diag_error_at(lexer_source(ps->lx), t.line, t.col,
                          "'yield' is currently only supported inside 'iterate' functions/methods");
            exit(1);
        }
        lexer_next(ps->lx);
        Token nxt = lexer_peek(ps->lx);
        if (nxt.kind == TK_SEMI)
        {
            diag_error_at(lexer_source(ps->lx), nxt.line, nxt.col,
                          "'yield' requires a value expression");
            exit(1);
        }
        Node *expr = parse_expr(ps);
        expect(ps, TK_SEMI, ";");
        Node *y = new_node(ND_RET);
        y->lhs = expr;
        y->line = t.line;
        y->col = t.col;
        y->src = lexer_source(ps->lx);
        return y;
    }
    if (t.kind == TK_KW_SWITCH)
    {
        return parse_switch(ps);
    }
    if (t.kind == TK_KW_TRY)
    {
        parser_require_h27(ps, t, "try/catch/finally");
        return parse_try_stmt(ps);
    }
    if (t.kind == TK_KW_THROW)
    {
        parser_require_h27(ps, t, "throw statement");
        lexer_next(ps->lx);
        Token nxt = lexer_peek(ps->lx);
        Node *expr = NULL;
        Node *payload = NULL;
        if (nxt.kind != TK_SEMI)
        {
            ps->suppress_arrow_member_for_throw++;
            expr = parse_expr(ps);
            if (ps->suppress_arrow_member_for_throw > 0)
                ps->suppress_arrow_member_for_throw--;
            Token maybe_arrow = lexer_peek(ps->lx);
            if (maybe_arrow.kind == TK_ARROW)
            {
                lexer_next(ps->lx);
                payload = parse_expr(ps);
            }
        }
        expect(ps, TK_SEMI, ";");
        Node *th = new_node(ND_THROW);
        th->lhs = expr;
        th->rhs = payload;
        th->line = t.line;
        th->col = t.col;
        th->src = lexer_source(ps->lx);
        return th;
    }
    if (t.kind == TK_KW_BREAK)
    {
        lexer_next(ps->lx);
        expect(ps, TK_SEMI, ";");
        Node *br = new_node(ND_BREAK);
        br->line = t.line;
        br->col = t.col;
        br->src = lexer_source(ps->lx);
        return br;
    }
    if (t.kind == TK_KW_CONTINUE)
    {
        lexer_next(ps->lx);
        expect(ps, TK_SEMI, ";");
        Node *ct = new_node(ND_CONTINUE);
        ct->line = t.line;
        ct->col = t.col;
        ct->src = lexer_source(ps->lx);
        return ct;
    }
    if (t.kind == TK_KW_RET)
    {
        lexer_next(ps->lx);
        
        
        Token nxt = lexer_peek(ps->lx);
        Node *expr = NULL;
        if (nxt.kind != TK_SEMI)
        {
            expr = parse_expr(ps);
            expect(ps, TK_SEMI, ";");
        }
        else
        {
            
            lexer_next(ps->lx);
        }
        Node *r = new_node(ND_RET);
        r->lhs = expr;
        r->line = t.line;
        r->col = t.col;
        r->src = lexer_source(ps->lx);
        return r;
    }
    if (t.kind == TK_KW_JUMP)
    {
        parser_require_h27(ps, t, "jump statement");
        lexer_next(ps->lx);
        Node *expr = parse_expr(ps);
        if (!expr || expr->kind != ND_CALL)
        {
            diag_error_at(lexer_source(ps->lx), t.line, t.col,
                          "'jump' requires a function-style target expression (e.g. jump target())");
            exit(1);
        }
        expr->call_is_jump = 1;
        expect(ps, TK_SEMI, ";");

        Node *es = new_node(ND_EXPR_STMT);
        es->lhs = expr;
        es->line = expr ? expr->line : t.line;
        es->col = expr ? expr->col : t.col;
        es->src = lexer_source(ps->lx);
        return es;
    }
    if (t.kind == TK_KW_DELETE)
    {
        parser_require_h27(ps, t, "delete statement");
        lexer_next(ps->lx);
        Node *expr = parse_expr(ps);
        expect(ps, TK_SEMI, ";");
        Node *n = new_node(ND_DELETE);
        n->lhs = expr;
        n->line = t.line;
        n->col = t.col;
        n->src = lexer_source(ps->lx);
        return n;
    }
    if (t.kind == TK_KW_VAR)
    {
        return parse_inferred_var_decl(ps, 1);
    }
    if ((t.kind == TK_KW_STATIC || t.kind == TK_KW_CONSTANT || is_type_start(ps, t) || parser_is_implicit_ident_decl_start(ps, t)) &&
        !starts_with_member_expr)
    {
        int is_const = 0;
        int is_static = 0;
        while (1)
        {
            if (!is_static && t.kind == TK_KW_STATIC)
            {
                lexer_next(ps->lx);
                is_static = 1;
                t = lexer_peek(ps->lx);
                continue;
            }
            if (!is_const && t.kind == TK_KW_CONSTANT)
            {
                lexer_next(ps->lx);
                is_const = 1;
                t = lexer_peek(ps->lx);
                continue;
            }
            break;
        }

        if (!is_type_start(ps, t) && !parser_is_implicit_ident_decl_start(ps, t))
        {
            diag_error_at(lexer_source(ps->lx), t.line, t.col,
                          "expected a type after storage qualifiers");
            exit(1);
        }
        
        Type *ty = parse_type_spec(ps);
        ty = parser_decl_canonicalize_bundle_reference(ty);
        Token name = expect(ps, TK_IDENT, "identifier");
        Node *decl = new_node(ND_VAR_DECL);
        char *nm = (char *)xmalloc((size_t)name.length + 1);
        memcpy(nm, name.lexeme, (size_t)name.length);
        nm[name.length] = '\0';
        decl->var_name = nm;
        decl->var_type = ty;
        decl->var_is_array = (ty && ty->kind == TY_ARRAY);
        decl->var_is_function = (ty && ty->kind == TY_PTR && ty->pointee && ty->pointee->kind == TY_FUNC);
        decl->var_is_const = is_const;
        decl->var_is_static = is_static;
        decl->line = name.line;
        decl->col = name.col;
        decl->src = lexer_source(ps->lx);
        parse_trailing_funptr_signature(ps, ty);
        Token p2 = lexer_peek(ps->lx);
        if (p2.kind == TK_ASSIGN)
        {
            lexer_next(ps->lx);
            decl->rhs = parse_initializer(ps);
        }
        parser_maybe_register_const_int(ps, is_const, name, decl->rhs);
        expect(ps, TK_SEMI, ";");
        return decl;
    }
    
    Node *e2 = parse_expr(ps);
    expect(ps, TK_SEMI, ";");
    Node *es = new_node(ND_EXPR_STMT);
    es->lhs = e2;
    es->line = e2 ? e2->line : t.line;
    es->col = e2 ? e2->col : t.col;
    es->src = lexer_source(ps->lx);
    return es;
}

static Node *parse_block(Parser *ps)
{
    expect(ps, TK_LBRACE, "{");
    const_scope_push(ps);
    Node **stmts = NULL;
    int cnt = 0, cap = 0;
    for (;;)
    {
        Token t = lexer_peek(ps->lx);

        
        if (t.kind == TK_IDENT && t.lexeme && t.length > 0)
        {
            if (t.length == (int)strlen("__CHANCE_HINT_START_IMPLICIT_VOID_FUNCTION__") && strncmp(t.lexeme, "__CHANCE_HINT_START_IMPLICIT_VOID_FUNCTION__", t.length) == 0)
            {
                lexer_next(ps->lx);
                Token semi = lexer_peek(ps->lx);
                if (semi.kind == TK_SEMI)
                    lexer_next(ps->lx);
                sema_set_allow_implicit_void_function(1);
                continue;
            }
            if (t.length == (int)strlen("__CHANCE_HINT_END_IMPLICIT_VOID_FUNCTION__") && strncmp(t.lexeme, "__CHANCE_HINT_END_IMPLICIT_VOID_FUNCTION__", t.length) == 0)
            {
                lexer_next(ps->lx);
                Token semi = lexer_peek(ps->lx);
                if (semi.kind == TK_SEMI)
                    lexer_next(ps->lx);
                sema_set_allow_implicit_void_function(0);
                continue;
            }
            if (t.length == (int)strlen("__CHANCE_HINT_START_IMPLICIT_SIZEOF__") && strncmp(t.lexeme, "__CHANCE_HINT_START_IMPLICIT_SIZEOF__", t.length) == 0)
            {
                lexer_next(ps->lx);
                Token semi = lexer_peek(ps->lx);
                if (semi.kind == TK_SEMI)
                    lexer_next(ps->lx);
                sema_set_allow_implicit_sizeof(1);
                continue;
            }
            if (t.length == (int)strlen("__CHANCE_HINT_END_IMPLICIT_SIZEOF__") && strncmp(t.lexeme, "__CHANCE_HINT_END_IMPLICIT_SIZEOF__", t.length) == 0)
            {
                lexer_next(ps->lx);
                Token semi = lexer_peek(ps->lx);
                if (semi.kind == TK_SEMI)
                    lexer_next(ps->lx);
                sema_set_allow_implicit_sizeof(0);
                continue;
            }
            if (t.length == (int)strlen("__CHANCE_HINT_START_IMPLICIT_VOIDP__") && strncmp(t.lexeme, "__CHANCE_HINT_START_IMPLICIT_VOIDP__", t.length) == 0)
            {
                lexer_next(ps->lx);
                Token semi = lexer_peek(ps->lx);
                if (semi.kind == TK_SEMI)
                    lexer_next(ps->lx);
                sema_set_allow_implicit_voidp(1);
                continue;
            }
            if (t.length == (int)strlen("__CHANCE_HINT_END_IMPLICIT_VOIDP__") && strncmp(t.lexeme, "__CHANCE_HINT_END_IMPLICIT_VOIDP__", t.length) == 0)
            {
                lexer_next(ps->lx);
                Token semi = lexer_peek(ps->lx);
                if (semi.kind == TK_SEMI)
                    lexer_next(ps->lx);
                sema_set_allow_implicit_voidp(0);
                continue;
            }
        }

        
        if (t.kind == TK_IDENT && t.lexeme && t.length > 0)
        {
            
            if (t.length == (int)strlen("__CHANCE_HINT_START_IMPLICIT_VOID_FUNCTION__") && strncmp(t.lexeme, "__CHANCE_HINT_START_IMPLICIT_VOID_FUNCTION__", t.length) == 0)
            {
                lexer_next(ps->lx);
                
                Token semi = lexer_peek(ps->lx);
                if (semi.kind == TK_SEMI)
                    lexer_next(ps->lx);
                sema_set_allow_implicit_void_function(1);
                continue;
            }
            if (t.length == (int)strlen("__CHANCE_HINT_END_IMPLICIT_VOID_FUNCTION__") && strncmp(t.lexeme, "__CHANCE_HINT_END_IMPLICIT_VOID_FUNCTION__", t.length) == 0)
            {
                lexer_next(ps->lx);
                Token semi = lexer_peek(ps->lx);
                if (semi.kind == TK_SEMI)
                    lexer_next(ps->lx);
                sema_set_allow_implicit_void_function(0);
                continue;
            }
            if (t.length == (int)strlen("__CHANCE_HINT_START_IMPLICIT_SIZEOF__") && strncmp(t.lexeme, "__CHANCE_HINT_START_IMPLICIT_SIZEOF__", t.length) == 0)
            {
                lexer_next(ps->lx);
                Token semi = lexer_peek(ps->lx);
                if (semi.kind == TK_SEMI)
                    lexer_next(ps->lx);
                sema_set_allow_implicit_sizeof(1);
                continue;
            }
            if (t.length == (int)strlen("__CHANCE_HINT_END_IMPLICIT_SIZEOF__") && strncmp(t.lexeme, "__CHANCE_HINT_END_IMPLICIT_SIZEOF__", t.length) == 0)
            {
                lexer_next(ps->lx);
                Token semi = lexer_peek(ps->lx);
                if (semi.kind == TK_SEMI)
                    lexer_next(ps->lx);
                sema_set_allow_implicit_sizeof(0);
                continue;
            }
            if (t.length == (int)strlen("__CHANCE_HINT_START_IMPLICIT_VOIDP__") && strncmp(t.lexeme, "__CHANCE_HINT_START_IMPLICIT_VOIDP__", t.length) == 0)
            {
                lexer_next(ps->lx);
                Token semi = lexer_peek(ps->lx);
                if (semi.kind == TK_SEMI)
                    lexer_next(ps->lx);
                sema_set_allow_implicit_voidp(1);
                continue;
            }
            if (t.length == (int)strlen("__CHANCE_HINT_END_IMPLICIT_VOIDP__") && strncmp(t.lexeme, "__CHANCE_HINT_END_IMPLICIT_VOIDP__", t.length) == 0)
            {
                lexer_next(ps->lx);
                Token semi = lexer_peek(ps->lx);
                if (semi.kind == TK_SEMI)
                    lexer_next(ps->lx);
                sema_set_allow_implicit_voidp(0);
                continue;
            }
        }
        if (t.kind == TK_RBRACE)
        {
            lexer_next(ps->lx);
            break;
        }
        Node *s = parse_stmt(ps);
        if (cnt == cap)
        {
            cap = cap ? cap * 2 : 8;
            stmts = (Node **)realloc(stmts, sizeof(Node *) * cap);
        }
        stmts[cnt++] = s;
    }
    Node *b = new_node(ND_BLOCK);
    b->stmts = stmts;
    b->stmt_count = cnt;
    b->line = 0;
    b->col = 0;
    b->src = lexer_source(ps->lx);
    const_scope_pop(ps);
    return b;
}

static Node *parse_switch(Parser *ps)
{
    Token kw = expect(ps, TK_KW_SWITCH, "switch");
    expect(ps, TK_LPAREN, "(");
    Node *expr = parse_expr(ps);
    expect(ps, TK_RPAREN, ")");
    expect(ps, TK_LBRACE, "{");

    SwitchCase *cases = NULL;
    int case_count = 0;
    int case_cap = 0;
    int saw_default = 0;

    while (1)
    {
        Token label = lexer_peek(ps->lx);
        if (label.kind == TK_RBRACE)
        {
            lexer_next(ps->lx);
            break;
        }
        if (label.kind != TK_KW_CASE && label.kind != TK_KW_DEFAULT)
        {
            diag_error_at(lexer_source(ps->lx), label.line, label.col,
                          "expected 'case' or 'default' in switch body");
            exit(1);
        }

        lexer_next(ps->lx); 
        int is_default = (label.kind == TK_KW_DEFAULT);
        Node *case_value = NULL;
        if (is_default)
        {
            if (saw_default)
            {
                diag_error_at(lexer_source(ps->lx), label.line, label.col,
                              "multiple 'default' labels in switch");
                exit(1);
            }
            saw_default = 1;
        }
        else
        {
            case_value = parse_expr(ps);
        }

        expect(ps, TK_COLON, ":");

        Node **case_stmts = NULL;
        int stmt_count = 0;
        int stmt_cap = 0;
        while (1)
        {
            Token look = lexer_peek(ps->lx);
            if (look.kind == TK_KW_CASE || look.kind == TK_KW_DEFAULT || look.kind == TK_RBRACE)
                break;
            Node *stmt = parse_stmt(ps);
            if (stmt_count == stmt_cap)
            {
                stmt_cap = stmt_cap ? stmt_cap * 2 : 4;
                Node **grown = (Node **)realloc(case_stmts, (size_t)stmt_cap * sizeof(Node *));
                if (!grown)
                {
                    diag_error("out of memory while parsing switch case body");
                    exit(1);
                }
                case_stmts = grown;
            }
            case_stmts[stmt_count++] = stmt;
        }

        Node *body = new_node(ND_BLOCK);
        body->stmts = case_stmts;
        body->stmt_count = stmt_count;
        body->line = label.line;
        body->col = label.col;
        body->src = lexer_source(ps->lx);

        if (case_count == case_cap)
        {
            case_cap = case_cap ? case_cap * 2 : 4;
            SwitchCase *grown = (SwitchCase *)realloc(cases, (size_t)case_cap * sizeof(SwitchCase));
            if (!grown)
            {
                diag_error("out of memory while parsing switch cases");
                exit(1);
            }
            cases = grown;
        }

        SwitchCase *slot = &cases[case_count++];
        slot->value = case_value;
        slot->body = body;
        slot->is_default = is_default;
    }

    Node *sw = new_node(ND_SWITCH);
    sw->switch_stmt.expr = expr;
    sw->switch_stmt.cases = cases;
    sw->switch_stmt.case_count = case_count;
    sw->line = kw.line;
    sw->col = kw.col;
    sw->src = lexer_source(ps->lx);
    return sw;
}

static Node *parse_match_expr(Parser *ps, Token match_tok)
{
    expect(ps, TK_LPAREN, "(");
    Node *subject = parse_expr(ps);
    expect(ps, TK_RPAREN, ")");
    expect(ps, TK_LBRACE, "{");

    MatchArm *arms = NULL;
    int arm_count = 0;
    int arm_cap = 0;

    while (1)
    {
        Token look = lexer_peek(ps->lx);
        if (look.kind == TK_RBRACE)
        {
            lexer_next(ps->lx);
            break;
        }

        Node *pattern = NULL;
        int is_wildcard = 0;

        if (look.kind == TK_IDENT && look.length == 1 && look.lexeme[0] == '_')
        {
            lexer_next(ps->lx);
            is_wildcard = 1;
        }
        else
        {
            parser_push_access_suppression(ps);
            pattern = parse_expr(ps);
            parser_pop_access_suppression(ps);
        }

        expect(ps, TK_ACCESS, "=>");
        Node *body = parse_expr(ps);

        if (arm_count == arm_cap)
        {
            arm_cap = arm_cap ? arm_cap * 2 : 4;
            MatchArm *grown = (MatchArm *)realloc(arms, (size_t)arm_cap * sizeof(MatchArm));
            if (!grown)
            {
                diag_error("out of memory while parsing match arms");
                exit(1);
            }
            arms = grown;
        }

        MatchArm *arm = &arms[arm_count++];
        arm->pattern = is_wildcard ? NULL : pattern;
        arm->guard = NULL;
        arm->body = body;
        arm->binding_name = NULL;

        Token sep = lexer_peek(ps->lx);
        if (sep.kind == TK_COMMA)
        {
            lexer_next(ps->lx);
            continue;
        }
        if (sep.kind == TK_RBRACE)
            continue;
        diag_error_at(lexer_source(ps->lx), sep.line, sep.col,
                      "expected ',' or '}' after match arm");
        exit(1);
    }

    if (arm_count == 0)
    {
        diag_error_at(lexer_source(ps->lx), match_tok.line, match_tok.col,
                      "match expression requires at least one arm");
        exit(1);
    }

    Node *match_node = new_node(ND_MATCH);
    match_node->match_stmt.expr = subject;
    match_node->match_stmt.arms = arms;
    match_node->match_stmt.arm_count = arm_count;
    match_node->line = match_tok.line;
    match_node->col = match_tok.col;
    match_node->src = lexer_source(ps->lx);
    return match_node;
}

static Node *parse_while(Parser *ps)
{
    
    expect(ps, TK_KW_WHILE, "while");
    expect(ps, TK_LPAREN, "(");
    Node *cond = parse_expr(ps);
    expect(ps, TK_RPAREN, ")");
    Node *body = parse_stmt(ps);
    Node *w = new_node(ND_WHILE);
    w->lhs = cond;
    w->rhs = body;
    w->line = 0;
    w->col = 0;
    w->src = lexer_source(ps->lx);
    return w;
}

static Node *parse_for(Parser *ps)
{
    
    expect(ps, TK_KW_FOR, "for");
    expect(ps, TK_LPAREN, "(");
    
    Token next = lexer_peek(ps->lx);
    Node *init = NULL;
    if (next.kind != TK_SEMI)
    {
        
        
        if (next.kind == TK_KW_STATIC || next.kind == TK_KW_CONSTANT || is_type_start(ps, next) || parser_is_implicit_ident_decl_start(ps, next))
        {
            
            int is_const = 0;
            int is_static = 0;
            while (1)
            {
                if (!is_static && next.kind == TK_KW_STATIC)
                {
                    lexer_next(ps->lx);
                    is_static = 1;
                    next = lexer_peek(ps->lx);
                    continue;
                }
                if (!is_const && next.kind == TK_KW_CONSTANT)
                {
                    lexer_next(ps->lx);
                    is_const = 1;
                    next = lexer_peek(ps->lx);
                    continue;
                }
                break;
            }

            if (!is_type_start(ps, next) && !parser_is_implicit_ident_decl_start(ps, next))
            {
                diag_error_at(lexer_source(ps->lx), next.line, next.col,
                              "expected a type after storage qualifiers");
                exit(1);
            }
            Type *ty = parse_type_spec(ps);
            ty = parser_decl_canonicalize_bundle_reference(ty);
            Token name = expect(ps, TK_IDENT, "identifier");
            Node *decl = new_node(ND_VAR_DECL);
            char *nm = (char *)xmalloc((size_t)name.length + 1);
            memcpy(nm, name.lexeme, (size_t)name.length);
            nm[name.length] = '\0';
            decl->var_name = nm;
            decl->var_type = ty;
            decl->var_is_array = (ty && ty->kind == TY_ARRAY);
            decl->var_is_function = (ty && ty->kind == TY_PTR && ty->pointee && ty->pointee->kind == TY_FUNC);
            decl->var_is_const = is_const;
            decl->var_is_static = is_static;
            decl->line = name.line;
            decl->col = name.col;
            decl->src = lexer_source(ps->lx);
            Token p2 = lexer_peek(ps->lx);
            if (p2.kind == TK_ASSIGN)
            {
                lexer_next(ps->lx);
                decl->rhs = parse_initializer(ps);
            }
            parser_maybe_register_const_int(ps, is_const, name, decl->rhs);
            expect(ps, TK_SEMI, ";");
            init = decl;
        }
        else if (next.kind == TK_KW_VAR)
        {
            Node *decl = parse_inferred_var_decl(ps, 0);
            expect(ps, TK_SEMI, ";");
            init = decl;
        }
        else
        {
            Node *e = parse_expr(ps);
            expect(ps, TK_SEMI, ";");
            Node *es = new_node(ND_EXPR_STMT);
            es->lhs = e;
            es->line = e->line;
            es->col = e->col;
            es->src = lexer_source(ps->lx);
            init = es;
        }
    }
    else
    {
        
        lexer_next(ps->lx);
    }
    
    Node *cond = NULL;
    next = lexer_peek(ps->lx);
    if (next.kind != TK_SEMI)
    {
        cond = parse_expr(ps);
    }
    expect(ps, TK_SEMI, ";");
    
    Node *post = NULL;
    next = lexer_peek(ps->lx);
    if (next.kind != TK_RPAREN)
    {
        Node *e = parse_expr(ps);
        Node *es = new_node(ND_EXPR_STMT);
        es->lhs = e;
        es->line = e->line;
        es->col = e->col;
        es->src = lexer_source(ps->lx);
        post = es;
    }
    expect(ps, TK_RPAREN, ")");
    Node *body = parse_stmt(ps);
    
    if (!cond)
    {
        Node *one = new_node(ND_INT);
        one->int_val = 1;
        one->src = lexer_source(ps->lx);
        cond = one;
    }
    Node *wh = new_node(ND_WHILE);
    wh->lhs = cond;
    wh->rhs = body;
    wh->body = post;
    wh->src = lexer_source(ps->lx);
    
    if (init)
    {
        Node **stmts = (Node **)xcalloc(2, sizeof(Node *));
        stmts[0] = init;
        stmts[1] = wh;
        Node *blk = new_node(ND_BLOCK);
        blk->stmts = stmts;
        blk->stmt_count = 2;
        blk->src = lexer_source(ps->lx);
        return blk;
    }
    return wh;
}

static Node *parse_foreach(Parser *ps)
{
    Token foreach_tok = lexer_next(ps->lx);
    if (!token_is_ident_text(foreach_tok, "foreach"))
    {
        diag_error_at(lexer_source(ps->lx), foreach_tok.line, foreach_tok.col,
                      "expected 'foreach'");
        exit(1);
    }

    expect(ps, TK_LPAREN, "(");

    Type *elem_ty = parse_type_spec(ps);
    elem_ty = parser_decl_canonicalize_bundle_reference(elem_ty);
    Token elem_name = expect(ps, TK_IDENT, "identifier");

    Token in_tok = expect(ps, TK_IDENT, "'in'");
    if (!token_is_ident_text(in_tok, "in"))
    {
        diag_error_at(lexer_source(ps->lx), in_tok.line, in_tok.col,
                      "expected 'in' in foreach loop");
        exit(1);
    }

    Node *iter_expr = parse_expr(ps);
    expect(ps, TK_RPAREN, ")");

    Node *user_body = parse_stmt(ps);

    char *iter_name = parser_make_foreach_temp_name(ps, "source");
    char *idx_name = parser_make_foreach_temp_name(ps, "index");

    Node *iter_decl = new_node(ND_VAR_DECL);
    iter_decl->var_name = iter_name;
    iter_decl->var_is_const = 0;
    iter_decl->var_is_static = 0;
    iter_decl->var_is_global = 0;
    iter_decl->var_is_array = 0;
    iter_decl->var_is_function = 0;
    iter_decl->var_is_inferred = 1;
    iter_decl->rhs = iter_expr;
    iter_decl->line = foreach_tok.line;
    iter_decl->col = foreach_tok.col;
    iter_decl->src = lexer_source(ps->lx);

    Node *idx_decl = new_node(ND_VAR_DECL);
    idx_decl->var_name = idx_name;
    idx_decl->var_type = type_i32();
    idx_decl->var_is_const = 0;
    idx_decl->var_is_static = 0;
    idx_decl->var_is_global = 0;
    idx_decl->var_is_array = 0;
    idx_decl->var_is_function = 0;
    idx_decl->var_is_inferred = 0;
    idx_decl->rhs = parser_make_bool_lit(ps, 0, foreach_tok.line, foreach_tok.col);
    idx_decl->line = foreach_tok.line;
    idx_decl->col = foreach_tok.col;
    idx_decl->src = lexer_source(ps->lx);

    Node *count_member = parser_make_member(
        ps,
        parser_make_var_ref(ps, xstrdup(iter_name), foreach_tok.line, foreach_tok.col),
        xstrdup("Count"),
        foreach_tok.line,
        foreach_tok.col);
    Node *count_call = new_node(ND_CALL);
    count_call->lhs = count_member;
    count_call->args = NULL;
    count_call->arg_count = 0;
    count_call->call_name = call_name_from_expr(count_member);
    count_call->line = foreach_tok.line;
    count_call->col = foreach_tok.col;
    count_call->src = lexer_source(ps->lx);

    Node *cond = new_node(ND_LT);
    cond->lhs = parser_make_var_ref(ps, xstrdup(idx_name), foreach_tok.line, foreach_tok.col);
    cond->rhs = count_call;
    cond->line = foreach_tok.line;
    cond->col = foreach_tok.col;
    cond->src = lexer_source(ps->lx);

    Node *index_expr = new_node(ND_INDEX);
    index_expr->lhs = parser_make_var_ref(ps, xstrdup(iter_name), elem_name.line, elem_name.col);
    index_expr->rhs = parser_make_var_ref(ps, xstrdup(idx_name), elem_name.line, elem_name.col);
    index_expr->line = elem_name.line;
    index_expr->col = elem_name.col;
    index_expr->src = lexer_source(ps->lx);

    Node *elem_decl = new_node(ND_VAR_DECL);
    elem_decl->var_name = dup_token_text(elem_name);
    elem_decl->var_type = elem_ty;
    elem_decl->var_is_const = 0;
    elem_decl->var_is_static = 0;
    elem_decl->var_is_global = 0;
    elem_decl->var_is_array = (elem_ty && elem_ty->kind == TY_ARRAY);
    elem_decl->var_is_function = (elem_ty && elem_ty->kind == TY_PTR && elem_ty->pointee && elem_ty->pointee->kind == TY_FUNC);
    elem_decl->var_is_inferred = 0;
    elem_decl->rhs = index_expr;
    elem_decl->line = elem_name.line;
    elem_decl->col = elem_name.col;
    elem_decl->src = lexer_source(ps->lx);

    Node *loop_body = new_node(ND_BLOCK);
    Node **loop_stmts = (Node **)xcalloc(2, sizeof(Node *));
    loop_stmts[0] = elem_decl;
    loop_stmts[1] = user_body;
    loop_body->stmts = loop_stmts;
    loop_body->stmt_count = 2;
    loop_body->line = foreach_tok.line;
    loop_body->col = foreach_tok.col;
    loop_body->src = lexer_source(ps->lx);

    Node *idx_inc_rhs = new_node(ND_ADD);
    idx_inc_rhs->lhs = parser_make_var_ref(ps, xstrdup(idx_name), foreach_tok.line, foreach_tok.col);
    idx_inc_rhs->rhs = parser_make_bool_lit(ps, 1, foreach_tok.line, foreach_tok.col);
    idx_inc_rhs->line = foreach_tok.line;
    idx_inc_rhs->col = foreach_tok.col;
    idx_inc_rhs->src = lexer_source(ps->lx);

    Node *idx_inc_stmt = parser_make_assign_stmt(
        ps,
        parser_make_var_ref(ps, xstrdup(idx_name), foreach_tok.line, foreach_tok.col),
        idx_inc_rhs,
        foreach_tok.line,
        foreach_tok.col);

    Node *wh = new_node(ND_WHILE);
    wh->lhs = cond;
    wh->rhs = loop_body;
    wh->body = idx_inc_stmt;
    wh->line = foreach_tok.line;
    wh->col = foreach_tok.col;
    wh->src = lexer_source(ps->lx);

    Node *outer = new_node(ND_BLOCK);
    Node **outer_stmts = (Node **)xcalloc(3, sizeof(Node *));
    outer_stmts[0] = iter_decl;
    outer_stmts[1] = idx_decl;
    outer_stmts[2] = wh;
    outer->stmts = outer_stmts;
    outer->stmt_count = 3;
    outer->line = foreach_tok.line;
    outer->col = foreach_tok.col;
    outer->src = lexer_source(ps->lx);
    return outer;
}

static Node *parse_function(Parser *ps, int is_noreturn, int is_exposed, int is_managed, int allow_extension_receiver, FunctionBodyKind body_kind, struct PendingAttr *attrs, int attr_count)
{
    expect(ps, TK_KW_FUN, "fun");
    Token name_tok = lexer_peek(ps->lx);
    const char *name_lexeme = NULL;
    int name_length = 0;
    int name_line = 0;
    int name_col = 0;

    if (name_tok.kind == TK_IDENT)
    {
        name_tok = lexer_next(ps->lx);
        name_lexeme = name_tok.lexeme;
        name_length = name_tok.length;
        name_line = name_tok.line;
        name_col = name_tok.col;
    }
    else if (name_tok.kind == TK_LBRACKET)
    {
        Token open = lexer_next(ps->lx);
        expect(ps, TK_RBRACKET, "]");
        name_lexeme = "[]";
        name_length = 2;
        name_line = open.line;
        name_col = open.col;
    }
    else
    {
        diag_error_at(lexer_source(ps->lx), name_tok.line, name_tok.col,
                      "expected function name");
        exit(1);
    }

    int generic_scope_start = ps->generic_param_count;
    int local_generic_count = 0;
    Token maybe_lt = lexer_peek(ps->lx);
    if (maybe_lt.kind == TK_LT)
    {
        lexer_next(ps->lx);
        while (1)
        {
            Token param_tok = expect(ps, TK_IDENT, "generic parameter name");
            for (int i = ps->generic_param_count - 1; i >= generic_scope_start; --i)
            {
                struct GenericParam *existing = &ps->generic_params[i];
                if (existing->name_len == param_tok.length &&
                    strncmp(existing->name, param_tok.lexeme, (size_t)param_tok.length) == 0)
                {
                    diag_error_at(lexer_source(ps->lx), param_tok.line, param_tok.col,
                                  "duplicate generic parameter '%.*s' on function '%.*s'",
                                  param_tok.length, param_tok.lexeme, name_length, name_lexeme);
                    exit(1);
                }
            }
            TemplateConstraintKind constraint_kind = TEMPLATE_CONSTRAINT_NONE;
            Type *default_type = NULL;
            parser_parse_generic_param_options(ps, &constraint_kind, &default_type);
            parser_push_generic_param(ps, param_tok, local_generic_count, constraint_kind, default_type);
            local_generic_count++;

            Token sep = lexer_peek(ps->lx);
            if (sep.kind == TK_COMMA)
            {
                lexer_next(ps->lx);
                continue;
            }
            if (sep.kind == TK_GT)
            {
                lexer_next(ps->lx);
                break;
            }
            diag_error_at(lexer_source(ps->lx), sep.line, sep.col,
                          "expected ',' or '>' in generic parameter list");
            exit(1);
        }
        if (local_generic_count == 0)
        {
            diag_error_at(lexer_source(ps->lx), maybe_lt.line, maybe_lt.col,
                          "generic parameter list cannot be empty");
            exit(1);
        }
    }
    expect(ps, TK_LPAREN, "(");
    
    Type **param_types = NULL;
    const char **param_names = NULL;
    unsigned char *param_const_flags = NULL;
    int param_count = 0, param_cap = 0;
    int saw_varargs = 0;
    int saw_extension_receiver = 0;
    while (1)
    {
        Token next = lexer_peek(ps->lx);
        if (next.kind == TK_RPAREN)
            break;

        if (token_is_varargs(next))
        {
            if (saw_varargs)
            {
                diag_error_at(lexer_source(ps->lx), next.line, next.col,
                              "varargs ('...') may only appear once in a parameter list");
                exit(1);
            }
            lexer_next(ps->lx);
            saw_varargs = 1;
            break;
        }

        if (token_is_ident_text(next, "this"))
        {
            if (!allow_extension_receiver)
            {
                diag_error_at(lexer_source(ps->lx), next.line, next.col,
                              "'this' receiver syntax is only allowed on top-level function declarations");
                exit(1);
            }
            if (param_count != 0)
            {
                diag_error_at(lexer_source(ps->lx), next.line, next.col,
                              "extension receiver marked with 'this' must be the first parameter");
                exit(1);
            }
            lexer_next(ps->lx);
            saw_extension_receiver = 1;
        }

        int param_is_const = 0;
        Token maybe_const = lexer_peek(ps->lx);
        if (maybe_const.kind == TK_KW_CONSTANT)
        {
            lexer_next(ps->lx);
            param_is_const = 1;
        }
        Type *pty = parse_type_spec(ps);
        pty = parser_decl_canonicalize_bundle_reference(pty);
        Token pn = expect(ps, TK_IDENT, "parameter name");
        if (param_count == param_cap)
        {
            param_cap = param_cap ? param_cap * 2 : 4;
            param_types = (Type **)realloc(param_types, sizeof(Type *) * param_cap);
            param_names =
                (const char **)realloc(param_names, sizeof(char *) * param_cap);
            param_const_flags = (unsigned char *)realloc(param_const_flags, sizeof(unsigned char) * param_cap);
        }
        param_types[param_count] = pty;
        char *nm = (char *)xmalloc((size_t)pn.length + 1);
        memcpy(nm, pn.lexeme, (size_t)pn.length);
        nm[pn.length] = '\0';
        param_names[param_count] = nm;
        if (param_const_flags)
            param_const_flags[param_count] = (unsigned char)param_is_const;
        parse_trailing_funptr_signature(ps, pty);
        param_count++;

        Token delim = lexer_peek(ps->lx);
        if (delim.kind == TK_COMMA)
        {
            lexer_next(ps->lx);
            continue;
        }
        break;
    }
    if (saw_varargs)
    {
        Token after = lexer_peek(ps->lx);
        if (after.kind != TK_RPAREN)
        {
            diag_error_at(lexer_source(ps->lx), after.line, after.col,
                          "varargs ('...') must be the final parameter");
            exit(1);
        }
        if (param_count == 0)
        {
            diag_error_at(lexer_source(ps->lx), name_line, name_col,
                          "variadic function '%.*s' must have at least one explicit parameter before '...'",
                          name_length, name_lexeme);
            exit(1);
        }
    }
    expect(ps, TK_RPAREN, ")");
    Token arrow_peek = lexer_peek(ps->lx);
    Type *rtype = NULL;
    int allow_implicit_for_this = 0;
    if (attrs && attr_count > 0)
    {
        for (int i = 0; i < attr_count; ++i)
        {
            if (!attrs[i].name)
                continue;
            if (strcmp(attrs[i].name, "Hint") == 0 && attrs[i].value && strcmp(attrs[i].value, "implicit-void-function") == 0)
            {
                allow_implicit_for_this = 1;
                break;
            }
        }
    }
    if (arrow_peek.kind == TK_ARROW)
    {
        lexer_next(ps->lx);
        
        rtype = parse_type_spec(ps);
        rtype = parser_decl_canonicalize_bundle_reference(rtype);
    }
    else if (allow_implicit_for_this || sema_get_allow_implicit_void_function())
    {
        rtype = type_void();
    }
    else
    {
        expect(ps, TK_ARROW, "->");
        
    }
    if (is_noreturn && rtype && rtype->kind != TY_VOID)
    {
        diag_error_at(lexer_source(ps->lx), name_line, name_col,
                      "noreturn functions must return void");
        exit(1);
    }
    Node *fn = new_node(ND_FUNC);
    
    char *nm = (char *)xmalloc((size_t)name_length + 1);
    memcpy(nm, name_lexeme, (size_t)name_length);
    nm[name_length] = '\0';
    fn->name = nm;
    fn->ret_type = rtype;
    fn->param_types = param_types;
    fn->param_names = param_names;
    fn->param_const_flags = param_const_flags;
    fn->param_count = param_count;
    fn->generic_param_count = local_generic_count;
    fn->is_varargs = saw_varargs;
    fn->line = name_line;
    fn->col = name_col;
    fn->src = lexer_source(ps->lx);
    fn->is_noreturn = is_noreturn;
    fn->is_exposed = is_exposed;
    fn->is_managed = is_managed ? 1 : 0;
    fn->is_extension_method = saw_extension_receiver ? 1 : 0;
    fn->metadata.declared_param_count = -1;
    fn->metadata.declared_local_count = -1;
    fn->metadata.ret_token = NULL;
    fn->is_chancecode = (body_kind == FN_BODY_CHANCECODE) ? 1 : 0;
    fn->is_literal = (body_kind == FN_BODY_LITERAL) ? 1 : 0;
    if (body_kind == FN_BODY_CHANCECODE)
    {
        parse_chancecode_body(ps, fn);
    }
    else if (body_kind == FN_BODY_LITERAL)
    {
        parse_literal_body(ps, fn);
    }
    else
    {
        const char *prev_fn = ps->current_function_name;
        ps->current_function_name = fn->name;
        fn->body = parse_block(ps);
        ps->current_function_name = prev_fn;
    }
    if (local_generic_count > 0)
    {
        fn->generic_param_names = (const char **)xcalloc((size_t)local_generic_count, sizeof(const char *));
        fn->generic_param_types = (Type **)xcalloc((size_t)local_generic_count, sizeof(Type *));
        for (int i = 0; i < local_generic_count; ++i)
        {
            struct GenericParam *gp = &ps->generic_params[generic_scope_start + i];
            fn->generic_param_names[i] = gp->name;
            gp->name = NULL;
            fn->generic_param_types[i] = gp->placeholder;
        }
    }
    parser_pop_generic_params(ps, local_generic_count);
    return fn;
}

static Node *parse_bundle_this_method(Parser *ps, Token sign_tok, int is_noreturn, int is_exposed, int is_managed)
{
    Token this_tok = expect(ps, TK_IDENT, "this");
    if (!(this_tok.length == 4 && strncmp(this_tok.lexeme, "this", 4) == 0))
    {
        diag_error_at(lexer_source(ps->lx), this_tok.line, this_tok.col,
                      "expected 'this' after '%c' in bundle constructor/destructor",
                      sign_tok.kind == TK_PLUS ? '+' : '-');
        exit(1);
    }

    expect(ps, TK_LPAREN, "(");
    Type **param_types = NULL;
    const char **param_names = NULL;
    unsigned char *param_const_flags = NULL;
    int param_count = 0, param_cap = 0;
    int saw_varargs = 0;

    while (1)
    {
        Token next = lexer_peek(ps->lx);
        if (next.kind == TK_RPAREN)
            break;

        if (token_is_varargs(next))
        {
            if (saw_varargs)
            {
                diag_error_at(lexer_source(ps->lx), next.line, next.col,
                              "varargs ('...') may only appear once in a parameter list");
                exit(1);
            }
            lexer_next(ps->lx);
            saw_varargs = 1;
            break;
        }

        int param_is_const = 0;
        Token maybe_const = lexer_peek(ps->lx);
        if (maybe_const.kind == TK_KW_CONSTANT)
        {
            lexer_next(ps->lx);
            param_is_const = 1;
        }

        Type *pty = parse_type_spec(ps);
        pty = parser_decl_canonicalize_bundle_reference(pty);
        Token pn = expect(ps, TK_IDENT, "parameter name");
        if (param_count == param_cap)
        {
            param_cap = param_cap ? param_cap * 2 : 4;
            param_types = (Type **)realloc(param_types, sizeof(Type *) * (size_t)param_cap);
            param_names = (const char **)realloc(param_names, sizeof(char *) * (size_t)param_cap);
            param_const_flags = (unsigned char *)realloc(param_const_flags, sizeof(unsigned char) * (size_t)param_cap);
        }
        param_types[param_count] = pty;
        char *nm = (char *)xmalloc((size_t)pn.length + 1);
        memcpy(nm, pn.lexeme, (size_t)pn.length);
        nm[pn.length] = '\0';
        param_names[param_count] = nm;
        if (param_const_flags)
            param_const_flags[param_count] = (unsigned char)param_is_const;
        parse_trailing_funptr_signature(ps, pty);
        param_count++;

        Token delim = lexer_peek(ps->lx);
        if (delim.kind == TK_COMMA)
        {
            lexer_next(ps->lx);
            continue;
        }
        break;
    }

    if (saw_varargs)
    {
        Token after = lexer_peek(ps->lx);
        if (after.kind != TK_RPAREN)
        {
            diag_error_at(lexer_source(ps->lx), after.line, after.col,
                          "varargs ('...') must be the final parameter");
            exit(1);
        }
        if (param_count == 0)
        {
            diag_error_at(lexer_source(ps->lx), sign_tok.line, sign_tok.col,
                          "variadic constructor/destructor must have at least one explicit parameter before '...'");
            exit(1);
        }
    }

    expect(ps, TK_RPAREN, ")");

    Type *rtype = NULL;
    Token arrow_peek = lexer_peek(ps->lx);
    if (arrow_peek.kind == TK_ARROW)
    {
        lexer_next(ps->lx);
        rtype = parse_type_spec(ps);
        rtype = parser_decl_canonicalize_bundle_reference(rtype);
    }
    else
    {
        rtype = type_void();
    }

    if (is_noreturn && rtype && rtype->kind != TY_VOID)
    {
        diag_error_at(lexer_source(ps->lx), sign_tok.line, sign_tok.col,
                      "noreturn constructors/destructors must return void");
        exit(1);
    }

    Node *fn = new_node(ND_FUNC);
    fn->name = xstrdup(sign_tok.kind == TK_PLUS ? "init" : "deinit");
    fn->ret_type = rtype;
    fn->param_types = param_types;
    fn->param_names = param_names;
    fn->param_const_flags = param_const_flags;
    fn->param_count = param_count;
    fn->generic_param_count = 0;
    fn->is_varargs = saw_varargs;
    fn->line = sign_tok.line;
    fn->col = sign_tok.col;
    fn->src = lexer_source(ps->lx);
    fn->is_noreturn = is_noreturn;
    fn->is_exposed = is_exposed;
    fn->is_managed = is_managed ? 1 : 0;
    fn->metadata.declared_param_count = -1;
    fn->metadata.declared_local_count = -1;
    fn->metadata.ret_token = NULL;

    const char *prev_fn = ps->current_function_name;
    ps->current_function_name = fn->name;
    fn->body = parse_block(ps);
    ps->current_function_name = prev_fn;

    return fn;
}

static Node *parse_bundle_on_method(Parser *ps, Token on_tok, int is_noreturn, int is_exposed, int is_managed)
{
    Token lifecycle = lexer_next(ps->lx);
    int is_ctor = (lifecycle.kind == TK_KW_NEW) || token_is_ident_text(lifecycle, "new");
    int is_dtor = (lifecycle.kind == TK_KW_DELETE) || token_is_ident_text(lifecycle, "delete");
    int is_named_on_method = (lifecycle.kind == TK_IDENT);
    if (!is_ctor && !is_dtor && !is_named_on_method)
    {
        diag_error_at(lexer_source(ps->lx), lifecycle.line, lifecycle.col,
                      "expected 'new', 'delete', or method identifier after 'on' in bundle declaration");
        exit(1);
    }

    char *on_method_name = NULL;
    if (is_ctor)
        on_method_name = xstrdup("init");
    else if (is_dtor)
        on_method_name = xstrdup("deinit");
    else
        on_method_name = dup_token_text(lifecycle);

    expect(ps, TK_LPAREN, "(");
    Type **param_types = NULL;
    const char **param_names = NULL;
    unsigned char *param_const_flags = NULL;
    int param_count = 0, param_cap = 0;
    int saw_varargs = 0;

    while (1)
    {
        Token next = lexer_peek(ps->lx);
        if (next.kind == TK_RPAREN)
            break;

        if (token_is_varargs(next))
        {
            if (saw_varargs)
            {
                diag_error_at(lexer_source(ps->lx), next.line, next.col,
                              "varargs ('...') may only appear once in a parameter list");
                exit(1);
            }
            lexer_next(ps->lx);
            saw_varargs = 1;
            break;
        }

        int param_is_const = 0;
        Token maybe_const = lexer_peek(ps->lx);
        if (maybe_const.kind == TK_KW_CONSTANT)
        {
            lexer_next(ps->lx);
            param_is_const = 1;
        }

        Type *pty = parse_type_spec(ps);
        pty = parser_decl_canonicalize_bundle_reference(pty);
        Token pn = expect(ps, TK_IDENT, "parameter name");
        if (param_count == param_cap)
        {
            param_cap = param_cap ? param_cap * 2 : 4;
            param_types = (Type **)realloc(param_types, sizeof(Type *) * (size_t)param_cap);
            param_names = (const char **)realloc(param_names, sizeof(char *) * (size_t)param_cap);
            param_const_flags = (unsigned char *)realloc(param_const_flags, sizeof(unsigned char) * (size_t)param_cap);
        }
        param_types[param_count] = pty;
        char *nm = (char *)xmalloc((size_t)pn.length + 1);
        memcpy(nm, pn.lexeme, (size_t)pn.length);
        nm[pn.length] = '\0';
        param_names[param_count] = nm;
        if (param_const_flags)
            param_const_flags[param_count] = (unsigned char)param_is_const;
        parse_trailing_funptr_signature(ps, pty);
        param_count++;

        Token delim = lexer_peek(ps->lx);
        if (delim.kind == TK_COMMA)
        {
            lexer_next(ps->lx);
            continue;
        }
        break;
    }

    if (saw_varargs)
    {
        Token after = lexer_peek(ps->lx);
        if (after.kind != TK_RPAREN)
        {
            diag_error_at(lexer_source(ps->lx), after.line, after.col,
                          "varargs ('...') must be the final parameter");
            exit(1);
        }
        if (param_count == 0)
        {
            diag_error_at(lexer_source(ps->lx), lifecycle.line, lifecycle.col,
                          "variadic lifecycle handlers must have at least one explicit parameter before '...'");
            exit(1);
        }
    }

    expect(ps, TK_RPAREN, ")");

    Type *rtype = NULL;
    Token arrow_peek = lexer_peek(ps->lx);
    if (arrow_peek.kind == TK_ARROW)
    {
        lexer_next(ps->lx);
        rtype = parse_type_spec(ps);
        rtype = parser_decl_canonicalize_bundle_reference(rtype);
    }
    else
    {
        rtype = type_void();
    }

    if (is_noreturn && rtype && rtype->kind != TY_VOID)
    {
        diag_error_at(lexer_source(ps->lx), lifecycle.line, lifecycle.col,
                      "noreturn on-handlers must return void");
        exit(1);
    }

    Node *fn = new_node(ND_FUNC);
    fn->name = on_method_name;
    fn->ret_type = rtype;
    fn->param_types = param_types;
    fn->param_names = param_names;
    fn->param_const_flags = param_const_flags;
    fn->param_count = param_count;
    fn->generic_param_count = 0;
    fn->is_varargs = saw_varargs;
    fn->line = on_tok.line;
    fn->col = on_tok.col;
    fn->src = lexer_source(ps->lx);
    fn->is_noreturn = is_noreturn;
    fn->is_exposed = is_exposed;
    fn->is_managed = is_managed ? 1 : 0;
    fn->metadata.declared_param_count = -1;
    fn->metadata.declared_local_count = -1;
    fn->metadata.ret_token = NULL;

    const char *prev_fn = ps->current_function_name;
    ps->current_function_name = fn->name;
    fn->body = parse_block(ps);
    ps->current_function_name = prev_fn;

    return fn;
}

static int parse_extend_decl(Parser *ps, int leading_noreturn)
{
    
    
    
    
    Token extend_tok = expect(ps, TK_KW_EXTEND, "extend");
    int is_noreturn = leading_noreturn;
    int is_jump_target = 0;
    int is_preserve = 0;
    int is_entrypoint = 0;
    Token next = lexer_peek(ps->lx);
    if (!is_noreturn && next.kind == TK_KW_NORETURN)
    {
        lexer_next(ps->lx);
        is_noreturn = 1;
        next = lexer_peek(ps->lx);
    }
    while (next.kind == TK_KW_JUMPTARGET || next.kind == TK_KW_PRESERVE || next.kind == TK_KW_ENTRYPOINT)
    {
        if (next.kind == TK_KW_JUMPTARGET)
            is_jump_target = 1;
        else if (next.kind == TK_KW_PRESERVE)
            is_preserve = 1;
        else if (next.kind == TK_KW_ENTRYPOINT)
        {
            is_entrypoint = 1;
            is_preserve = 1;
        }
        lexer_next(ps->lx);
        next = lexer_peek(ps->lx);
    }
    if (next.kind == TK_KW_FUN)
    {
        
        lexer_next(ps->lx); 
        Token name = expect(ps, TK_IDENT, "identifier");
        expect(ps, TK_LPAREN, "(");
        Type **param_types = NULL;
        unsigned char *param_const_flags = NULL;
        int param_count = 0;
        int param_cap = 0;
        int is_varargs = 0;
        Token peek = lexer_peek(ps->lx);
        if (peek.kind != TK_RPAREN)
        {
            while (1)
            {
                Token maybe_va = lexer_peek(ps->lx);
                if (maybe_va.kind == TK_RPAREN)
                    break;
                if (token_is_varargs(maybe_va))
                {
                    if (is_varargs)
                    {
                        diag_error_at(lexer_source(ps->lx), maybe_va.line, maybe_va.col,
                                      "varargs ('...') may only appear once in a parameter list");
                        exit(1);
                    }
                    lexer_next(ps->lx);
                    is_varargs = 1;
                    break;
                }

                int param_is_const = 0;
                Token maybe_const = lexer_peek(ps->lx);
                if (maybe_const.kind == TK_KW_CONSTANT)
                {
                    lexer_next(ps->lx);
                    param_is_const = 1;
                }

                Type *pty = parse_type_spec(ps);
                if (param_count == param_cap)
                {
                    param_cap = param_cap ? param_cap * 2 : 4;
                    param_types =
                        (Type **)realloc(param_types, sizeof(Type *) * param_cap);
                    param_const_flags = (unsigned char *)realloc(param_const_flags, sizeof(unsigned char) * param_cap);
                }
                param_types[param_count] = pty;
                if (param_const_flags)
                    param_const_flags[param_count] = (unsigned char)param_is_const;
                parse_trailing_funptr_signature(ps, pty);
                param_count++;
                Token maybe_name = lexer_peek(ps->lx);
                if (maybe_name.kind == TK_IDENT && !token_is_varargs(maybe_name) &&
                    !is_type_start(ps, maybe_name))
                {
                    lexer_next(ps->lx);
                }

                Token delim = lexer_peek(ps->lx);
                if (delim.kind == TK_COMMA)
                {
                    lexer_next(ps->lx);
                    continue;
                }
                break;
            }
        }
        if (is_varargs)
        {
            Token after = lexer_peek(ps->lx);
            if (after.kind != TK_RPAREN)
            {
                diag_error_at(lexer_source(ps->lx), after.line, after.col,
                              "varargs ('...') must be the final parameter");
                exit(1);
            }
            if (param_count == 0)
            {
                diag_error_at(lexer_source(ps->lx), name.line, name.col,
                              "variadic function '%.*s' must have at least one explicit parameter before '...'",
                              (int)name.length, name.lexeme);
                exit(1);
            }
        }
        expect(ps, TK_RPAREN, ")");
        expect(ps, TK_ARROW, "->");
        Type *ret_ty = parse_type_spec(ps);
        expect(ps, TK_SEMI, ";");
        
        Symbol s = (Symbol){0};
        s.kind = SYM_FUNC;
        char *nm = (char *)xmalloc((size_t)name.length + 1);
        memcpy(nm, name.lexeme, (size_t)name.length);
        nm[name.length] = '\0';
        s.name = nm;
        s.backend_name = s.name;
        s.is_extern = 1;
        s.abi = xstrdup("C");
        
        
        static Type ti32_ext = {.kind = TY_I32};
        s.sig.ret = ret_ty ? ret_ty : &ti32_ext;
        s.sig.params = param_types;
        s.sig.param_count = param_count;
        s.sig.param_const_flags = param_const_flags;
        s.sig.is_varargs = is_varargs;
        s.is_noreturn = is_noreturn;

        Node *proto = new_node(ND_FUNC);
        proto->name = s.name;
        proto->ret_type = s.sig.ret;
        proto->param_types = param_types;
        proto->param_count = param_count;
        proto->is_varargs = is_varargs;
        proto->is_noreturn = is_noreturn;
        proto->is_jump_target = is_jump_target;
        proto->is_preserve = is_preserve;
        proto->is_entrypoint = is_entrypoint;
        proto->line = name.line;
        proto->col = name.col;
        proto->src = lexer_source(ps->lx);
        proto->metadata.declared_param_count = -1;
        proto->metadata.declared_local_count = -1;
        s.ast_node = proto;

        if (ps->ext_count == ps->ext_cap)
        {
            ps->ext_cap = ps->ext_cap ? ps->ext_cap * 2 : 8;
            ps->externs = (Symbol *)realloc(ps->externs, ps->ext_cap * sizeof(Symbol));
        }
        ps->externs[ps->ext_count++] = s;
        return extend_tok.line;
    }
    if (is_jump_target || is_preserve || is_entrypoint)
    {
        diag_error_at(lexer_source(ps->lx), next.line, next.col,
                      "function-only modifiers on 'extend' must be followed by 'fun'");
        exit(1);
    }
    expect(ps, TK_KW_FROM, "from");
    Token abi = lexer_next(ps->lx);
    if (!(abi.kind == TK_STRING || abi.kind == TK_IDENT))
    {
        diag_error_at(lexer_source(ps->lx), abi.line, abi.col,
                      "expected ABI after 'from'");
        exit(1);
    }
    
    
    
    Type *ret_ty = parse_type_spec(ps);
    Token name = expect(ps, TK_IDENT, "identifier");
    Token after_name = lexer_peek(ps->lx);
    if (after_name.kind == TK_SEMI)
    {
        if (is_noreturn)
        {
            diag_error_at(lexer_source(ps->lx), name.line, name.col,
                          "'noreturn' is only valid on function declarations");
            exit(1);
        }

        lexer_next(ps->lx); 

        Symbol s = {0};
        s.kind = SYM_GLOBAL;
        char *nm = (char *)xmalloc((size_t)name.length + 1);
        memcpy(nm, name.lexeme, (size_t)name.length);
        nm[name.length] = '\0';
        s.name = nm;
        s.backend_name = s.name;
        s.is_extern = 1;
        s.var_type = ret_ty;
        s.is_const = 0;
        if (abi.kind == TK_STRING)
        {
            char *ab = (char *)xmalloc((size_t)abi.length - 1);
            memcpy(ab, abi.lexeme + 1, (size_t)abi.length - 2);
            ab[abi.length - 2] = '\0';
            s.abi = ab;
        }
        else
        {
            char *ab = (char *)xmalloc((size_t)abi.length + 1);
            memcpy(ab, abi.lexeme, (size_t)abi.length);
            ab[abi.length] = '\0';
            s.abi = ab;
        }

        if (ps->ext_count == ps->ext_cap)
        {
            ps->ext_cap = ps->ext_cap ? ps->ext_cap * 2 : 8;
            ps->externs = (Symbol *)realloc(ps->externs, ps->ext_cap * sizeof(Symbol));
        }
        ps->externs[ps->ext_count++] = s;
        return extend_tok.line;
    }

    expect(ps, TK_LPAREN, "(");
    int is_varargs = 0;
    Type **param_types = NULL;
    unsigned char *param_const_flags = NULL;
    int param_count = 0;
    int param_cap = 0;
    Token pstart = lexer_peek(ps->lx);
    if (pstart.kind != TK_RPAREN)
    {
        while (1)
        {
            Token maybe_va = lexer_peek(ps->lx);
            if (maybe_va.kind == TK_RPAREN)
                break;
            if (token_is_varargs(maybe_va))
            {
                if (is_varargs)
                {
                    diag_error_at(lexer_source(ps->lx), maybe_va.line, maybe_va.col,
                                  "varargs ('...') may only appear once in a parameter list");
                    exit(1);
                }
                lexer_next(ps->lx);
                is_varargs = 1;
                break;
            }

            int param_is_const = 0;
            Token maybe_const = lexer_peek(ps->lx);
            if (maybe_const.kind == TK_KW_CONSTANT)
            {
                lexer_next(ps->lx);
                param_is_const = 1;
            }

            Type *pty = parse_type_spec(ps);
            if (param_count == param_cap)
            {
                param_cap = param_cap ? param_cap * 2 : 4;
                param_types =
                    (Type **)realloc(param_types, sizeof(Type *) * param_cap);
                param_const_flags = (unsigned char *)realloc(param_const_flags, sizeof(unsigned char) * param_cap);
            }
            param_types[param_count] = pty;
            if (param_const_flags)
                param_const_flags[param_count] = (unsigned char)param_is_const;
            parse_trailing_funptr_signature(ps, pty);
            param_count++;
            Token maybe_name = lexer_peek(ps->lx);
            if (maybe_name.kind == TK_IDENT && !token_is_varargs(maybe_name) &&
                !is_type_start(ps, maybe_name))
            {
                lexer_next(ps->lx);
            }

            Token delim = lexer_peek(ps->lx);
            if (delim.kind == TK_COMMA)
            {
                lexer_next(ps->lx);
                continue;
            }
            if (delim.kind == TK_RPAREN)
                break;
            if (delim.kind == TK_EOF)
            {
                diag_error_at(lexer_source(ps->lx), 0, 0,
                              "unexpected end of file in extern parameter list");
                exit(1);
            }
        }
    }
    if (is_varargs)
    {
        Token after = lexer_peek(ps->lx);
        if (after.kind != TK_RPAREN)
        {
            diag_error_at(lexer_source(ps->lx), after.line, after.col,
                          "varargs ('...') must be the final parameter");
            exit(1);
        }
        if (param_count == 0)
        {
            diag_error_at(lexer_source(ps->lx), name.line, name.col,
                          "variadic function '%.*s' must have at least one explicit parameter before '...'",
                          (int)name.length, name.lexeme);
            exit(1);
        }
    }
    expect(ps, TK_RPAREN, ")");
    expect(ps, TK_SEMI, ";");
    
    Symbol s = {0};
    s.kind = SYM_FUNC;
    char *nm = (char *)xmalloc((size_t)name.length + 1);
    memcpy(nm, name.lexeme, (size_t)name.length);
    nm[name.length] = '\0';
    s.name = nm;
    s.backend_name = s.name;
    s.is_extern = 1;
    if (abi.kind == TK_STRING)
    {
        char *ab = (char *)xmalloc((size_t)abi.length - 1);
        memcpy(ab, abi.lexeme + 1, (size_t)abi.length - 2);
        ab[abi.length - 2] = '\0';
        s.abi = ab;
    }
    else
    {
        char *ab = (char *)xmalloc((size_t)abi.length + 1);
        memcpy(ab, abi.lexeme, (size_t)abi.length);
        ab[abi.length] = '\0';
        s.abi = ab;
    }
    static Type ti32 = {.kind = TY_I32}, ti64 = {.kind = TY_I64},
                tv = {.kind = TY_VOID}, tch = {.kind = TY_CHAR};
    if (ret_ty)
    {
        s.sig.ret = ret_ty;
    }
    else
        s.sig.ret = &ti32;
    s.sig.params = param_types;
    s.sig.param_count = param_count;
    s.sig.param_const_flags = param_const_flags;
    s.sig.is_varargs = is_varargs;
    s.is_noreturn = is_noreturn;
    if (ps->ext_count == ps->ext_cap)
    {
        ps->ext_cap = ps->ext_cap ? ps->ext_cap * 2 : 8;
        ps->externs = (Symbol *)realloc(ps->externs, ps->ext_cap * sizeof(Symbol));
    }
    ps->externs[ps->ext_count++] = s;
    return extend_tok.line;
}
Parser *parser_create(SourceBuffer src)
{
    Parser *ps = (Parser *)xcalloc(1, sizeof(Parser));
    ps->lx = lexer_create(src);
    return ps;
}

void parser_destroy(Parser *ps)
{
    if (!ps)
        return;
    if (ps->bundle_templates)
    {
        for (int i = 0; i < ps->bundle_template_count; ++i)
        {
            struct BundleTemplate *templ = &ps->bundle_templates[i];
            free(templ->name);
            if (templ->generic_param_names)
            {
                for (int j = 0; j < templ->generic_param_count; ++j)
                    free((void *)templ->generic_param_names[j]);
                free((void *)templ->generic_param_names);
            }
            free(templ->generic_param_types);
            free(templ->method_templates);
            if (templ->instances)
            {
                for (int j = 0; j < templ->instance_count; ++j)
                {
                    free(templ->instances[j].type_args);
                }
                free(templ->instances);
            }
        }
        free(ps->bundle_templates);
    }
    if (ps->bundle_static_members)
    {
        for (int i = 0; i < ps->bundle_static_member_count; ++i)
        {
            free(ps->bundle_static_members[i].bundle_name);
            free(ps->bundle_static_members[i].member_name);
            free(ps->bundle_static_members[i].symbol_name);
        }
        free(ps->bundle_static_members);
    }
    free(ps->pending_instantiated_decls);
    lexer_destroy(ps->lx);
    free(ps);
}

Node *parse_unit(Parser *ps)
{
    Node **decls = NULL;
    int decl_count = 0, decl_cap = 0;
    int fn_count = 0;
    struct PendingAttr *attrs = NULL;
    int attr_count = 0, attr_cap = 0;
    const SourceBuffer *unit_src = lexer_source(ps->lx);
    int bring_seen = 0;
    int bring_block_done = 0;
    int bring_note_emitted = 0;
    int last_bring_line = 0;
    int extend_seen = 0;
    int extend_block_done = 0;
    int extend_note_emitted = 0;
    int last_extend_line = 0;
    int global_seen = 0;
    int global_block_done = 0;
    int global_note_emitted = 0;
    int last_global_line = 0;

    for (;;)
    {
        Token t = lexer_peek(ps->lx);

        
        if (t.kind == TK_IDENT && t.lexeme && t.length > 0)
        {
            if (t.length == (int)strlen("__CHANCE_HINT_START_IMPLICIT_VOID_FUNCTION__") && strncmp(t.lexeme, "__CHANCE_HINT_START_IMPLICIT_VOID_FUNCTION__", t.length) == 0)
            {
                lexer_next(ps->lx);
                Token semi = lexer_peek(ps->lx);
                if (semi.kind == TK_SEMI)
                    lexer_next(ps->lx);
                sema_set_allow_implicit_void_function(1);
                continue;
            }
            if (t.length == (int)strlen("__CHANCE_HINT_END_IMPLICIT_VOID_FUNCTION__") && strncmp(t.lexeme, "__CHANCE_HINT_END_IMPLICIT_VOID_FUNCTION__", t.length) == 0)
            {
                lexer_next(ps->lx);
                Token semi = lexer_peek(ps->lx);
                if (semi.kind == TK_SEMI)
                    lexer_next(ps->lx);
                sema_set_allow_implicit_void_function(0);
                continue;
            }
            if (t.length == (int)strlen("__CHANCE_HINT_START_IMPLICIT_SIZEOF__") && strncmp(t.lexeme, "__CHANCE_HINT_START_IMPLICIT_SIZEOF__", t.length) == 0)
            {
                lexer_next(ps->lx);
                Token semi = lexer_peek(ps->lx);
                if (semi.kind == TK_SEMI)
                    lexer_next(ps->lx);
                sema_set_allow_implicit_sizeof(1);
                continue;
            }
            if (t.length == (int)strlen("__CHANCE_HINT_END_IMPLICIT_SIZEOF__") && strncmp(t.lexeme, "__CHANCE_HINT_END_IMPLICIT_SIZEOF__", t.length) == 0)
            {
                lexer_next(ps->lx);
                Token semi = lexer_peek(ps->lx);
                if (semi.kind == TK_SEMI)
                    lexer_next(ps->lx);
                sema_set_allow_implicit_sizeof(0);
                continue;
            }
            if (t.length == (int)strlen("__CHANCE_HINT_START_IMPLICIT_VOIDP__") && strncmp(t.lexeme, "__CHANCE_HINT_START_IMPLICIT_VOIDP__", t.length) == 0)
            {
                lexer_next(ps->lx);
                Token semi = lexer_peek(ps->lx);
                if (semi.kind == TK_SEMI)
                    lexer_next(ps->lx);
                sema_set_allow_implicit_voidp(1);
                continue;
            }
            if (t.length == (int)strlen("__CHANCE_HINT_END_IMPLICIT_VOIDP__") && strncmp(t.lexeme, "__CHANCE_HINT_END_IMPLICIT_VOIDP__", t.length) == 0)
            {
                lexer_next(ps->lx);
                Token semi = lexer_peek(ps->lx);
                if (semi.kind == TK_SEMI)
                    lexer_next(ps->lx);
                sema_set_allow_implicit_voidp(0);
                continue;
            }
        }

        while (t.kind == TK_LBRACKET || token_is_attribute_keyword(t.kind))
        {
            struct PendingAttr attr =
                (t.kind == TK_LBRACKET) ? parse_attribute(ps) : parse_attribute_keyword(ps);
            if (attr_count == attr_cap)
            {
                int new_cap = attr_cap ? attr_cap * 2 : 4;
                struct PendingAttr *new_attrs = (struct PendingAttr *)realloc(attrs, (size_t)new_cap * sizeof(struct PendingAttr));
                if (!new_attrs)
                {
                    diag_error("out of memory while recording attributes");
                    exit(1);
                }
                attrs = new_attrs;
                attr_cap = new_cap;
            }
            attrs[attr_count++] = attr;
            t = lexer_peek(ps->lx);
        }

        int managed_override_present = 0;
        int managed_override_value = 0;
        if (t.kind == TK_KW_MANAGED || t.kind == TK_KW_UNMANAGED)
        {
            parser_require_h27(ps, t, "managed/unmanaged declaration modifier");
            Token managed_tok = lexer_next(ps->lx);
            managed_override_present = 1;
            managed_override_value = (managed_tok.kind == TK_KW_MANAGED) ? 1 : 0;
            t = lexer_peek(ps->lx);
        }

        if (t.kind == TK_KW_MODULE)
        {
            if (attr_count > 0)
            {
                diag_error_at(lexer_source(ps->lx), attrs[0].line, attrs[0].col,
                              "attributes are not supported on module declarations");
                exit(1);
            }
            parse_module_decl(ps, managed_override_present ? managed_override_value : 0);
            continue;
        }
        if (t.kind == TK_KW_BRING)
        {
            if (attr_count > 0)
            {
                diag_error_at(lexer_source(ps->lx), attrs[0].line, attrs[0].col,
                              "attributes are not supported on bring declarations");
                exit(1);
            }
            if (extend_seen)
                extend_block_done = 1;
            if (global_seen)
                global_block_done = 1;
            int bring_line = parse_bring_decl(ps);
            if (!bring_seen)
            {
                bring_seen = 1;
                last_bring_line = bring_line;
                continue;
            }
            if (!parser_disable_formatting_notes && bring_block_done && !bring_note_emitted && last_bring_line > 0)
            {
                if (!source_only_preprocessor_between(unit_src, last_bring_line, bring_line))
                {
                    diag_note_at(lexer_source(ps->lx), bring_line, t.col,
                                 "formatting: group 'bring' declarations together (disable with -Nno-formatting)");
                    bring_note_emitted = 1;
                }
                else
                {
                    bring_block_done = 0;
                }
            }
            bring_seen = 1;
            last_bring_line = bring_line;
            continue;
        }
        if (t.kind == TK_EOF)
        {
            if (attr_count > 0)
            {
                diag_error_at(lexer_source(ps->lx), attrs[0].line, attrs[0].col,
                              "attribute without following declaration");
                exit(1);
            }
            break;
        }

        int visibility = 0;
        Token vis_tok = {0};
        if (t.kind == TK_KW_HIDE || t.kind == TK_KW_EXPOSE)
        {
            vis_tok = lexer_next(ps->lx);
            visibility = (vis_tok.kind == TK_KW_EXPOSE);
            t = lexer_peek(ps->lx);
        }

        int leading_noreturn = 0;
        Token noreturn_tok = {0};
        if (t.kind == TK_KW_NORETURN)
        {
            noreturn_tok = lexer_next(ps->lx);
            leading_noreturn = 1;
            t = lexer_peek(ps->lx);
        }

        int leading_packed = 0;
        Token packed_tok = {0};
        if (t.kind == TK_KW_PACKED)
        {
            parser_require_h27(ps, t, "packed struct");
            packed_tok = lexer_next(ps->lx);
            leading_packed = 1;
            t = lexer_peek(ps->lx);
        }

        if (t.kind == TK_EOF)
        {
            Token err_tok = visibility ? vis_tok : noreturn_tok;
            if (leading_packed)
                err_tok = packed_tok;
            diag_error_at(lexer_source(ps->lx), err_tok.line, err_tok.col,
                          "unexpected end of file after '%.*s'",
                          err_tok.length, err_tok.lexeme);
            exit(1);
        }

        if (leading_packed && t.kind != TK_KW_STRUCT)
        {
            diag_error_at(lexer_source(ps->lx), packed_tok.line, packed_tok.col,
                          "'packed' is only valid before struct declarations");
            exit(1);
        }

        if (managed_override_present && t.kind != TK_KW_FUN)
        {
            diag_error_at(lexer_source(ps->lx), t.line, t.col,
                          "'managed'/'unmanaged' is only valid before 'module' or 'fun' declarations");
            exit(1);
        }

        if (t.kind == TK_KW_EXTEND)
        {
            if (bring_seen)
                bring_block_done = 1;
            if (global_seen)
                global_block_done = 1;
            if (visibility)
            {
                diag_error_at(lexer_source(ps->lx), vis_tok.line, vis_tok.col,
                              "'hide'/'expose' cannot be applied to 'extend'");
                exit(1);
            }
            int extend_line = parse_extend_decl(ps, leading_noreturn);
            if (!extend_seen)
            {
                extend_seen = 1;
                last_extend_line = extend_line;
                continue;
            }
            if (!parser_disable_formatting_notes && extend_block_done && !extend_note_emitted && last_extend_line > 0)
            {
                if (!source_only_preprocessor_between(unit_src, last_extend_line, extend_line))
                {
                    diag_note_at(lexer_source(ps->lx), extend_line, t.col,
                                 "formatting: group 'extend' declarations together (disable with -Nno-formatting)");
                    extend_note_emitted = 1;
                }
                else
                {
                    extend_block_done = 0;
                }
            }
            extend_seen = 1;
            last_extend_line = extend_line;
            continue;
        }
        if (t.kind == TK_KW_ENUM)
        {
            if (bring_seen)
                bring_block_done = 1;
            if (extend_seen)
                extend_block_done = 1;
            if (global_seen)
                global_block_done = 1;
            if (attr_count > 0)
            {
                diag_error_at(lexer_source(ps->lx), attrs[0].line, attrs[0].col,
                              "attributes are not supported on enum declarations");
                exit(1);
            }
            if (leading_noreturn)
            {
                diag_error_at(lexer_source(ps->lx), noreturn_tok.line, noreturn_tok.col,
                              "'noreturn' is only valid before functions or extern declarations");
                exit(1);
            }
            parse_enum_decl(ps, visibility);
            continue;
        }
        if (t.kind == TK_KW_BUNDLE)
        {
            parser_require_h28(ps, t, "bundle declaration");
            if (bring_seen)
                bring_block_done = 1;
            if (extend_seen)
                extend_block_done = 1;
            if (global_seen)
                global_block_done = 1;
            if (leading_noreturn)
            {
                diag_error_at(lexer_source(ps->lx), noreturn_tok.line, noreturn_tok.col,
                              "'noreturn' is only valid before functions or extern declarations");
                exit(1);
            }
            if (leading_packed)
            {
                diag_error_at(lexer_source(ps->lx), packed_tok.line, packed_tok.col,
                              "'packed' is only valid before struct declarations");
                exit(1);
            }
            if (attr_count > 0)
            {
                diag_error_at(lexer_source(ps->lx), attrs[0].line, attrs[0].col,
                              "attributes are not supported on bundle declarations");
                exit(1);
            }
            parse_bundle_decl(ps, visibility, &decls, &decl_count, &decl_cap, &fn_count);
            continue;
        }
        if (t.kind == TK_KW_STRUCT)
        {
            if (bring_seen)
                bring_block_done = 1;
            if (extend_seen)
                extend_block_done = 1;
            if (global_seen)
                global_block_done = 1;
            if (leading_noreturn)
            {
                diag_error_at(lexer_source(ps->lx), noreturn_tok.line, noreturn_tok.col,
                              "'noreturn' is only valid before functions or extern declarations");
                exit(1);
            }
            int packed_attr = struct_packed_from_attributes(ps, attrs, attr_count, 0);
            clear_pending_attrs(attrs, attr_count);
            attr_count = 0;
            parse_struct_decl(ps, visibility, 0, leading_packed || packed_attr);
            continue;
        }
        if (t.kind == TK_KW_UNION)
        {
            parser_require_h27(ps, t, "union declaration");
            if (bring_seen)
                bring_block_done = 1;
            if (extend_seen)
                extend_block_done = 1;
            if (global_seen)
                global_block_done = 1;
            if (leading_noreturn)
            {
                diag_error_at(lexer_source(ps->lx), noreturn_tok.line, noreturn_tok.col,
                              "'noreturn' is only valid before functions or extern declarations");
                exit(1);
            }
            int packed_attr = struct_packed_from_attributes(ps, attrs, attr_count, 1);
            if (packed_attr || leading_packed)
            {
                diag_error_at(lexer_source(ps->lx), t.line, t.col,
                              "packed layout is only supported on struct declarations");
                exit(1);
            }
            clear_pending_attrs(attrs, attr_count);
            attr_count = 0;
            parse_struct_decl(ps, visibility, 1, 0);
            continue;
        }
        if (t.kind == TK_KW_ALIAS)
        {
            if (bring_seen)
                bring_block_done = 1;
            if (extend_seen)
                extend_block_done = 1;
            if (global_seen)
                global_block_done = 1;
            if (attr_count > 0)
            {
                diag_error_at(lexer_source(ps->lx), attrs[0].line, attrs[0].col,
                              "attributes are not supported on alias declarations");
                exit(1);
            }
            if (leading_noreturn)
            {
                diag_error_at(lexer_source(ps->lx), noreturn_tok.line, noreturn_tok.col,
                              "'noreturn' is only valid before functions or extern declarations");
                exit(1);
            }
            parse_alias_decl(ps, visibility);
            continue;
        }
        if (t.kind == TK_KW_FUN)
        {
            if (bring_seen)
                bring_block_done = 1;
            if (extend_seen)
                extend_block_done = 1;
            if (global_seen)
                global_block_done = 1;
            int has_chancecode = attrs_contains(attrs, attr_count, "ChanceCode");
            int has_literal = attrs_contains(attrs, attr_count, "Literal");
            if (has_chancecode && has_literal)
            {
                const struct PendingAttr *lit_attr = NULL;
                for (int i = 0; i < attr_count; ++i)
                {
                    if (attrs[i].name && strcmp(attrs[i].name, "Literal") == 0)
                    {
                        lit_attr = &attrs[i];
                        break;
                    }
                }
                const struct PendingAttr *err_attr = lit_attr ? lit_attr : &attrs[0];
                diag_error_at(lexer_source(ps->lx), err_attr->line, err_attr->col,
                              "attributes 'ChanceCode' and 'Literal' cannot be combined on the same function");
                exit(1);
            }
            FunctionBodyKind body_kind = FN_BODY_NORMAL;
            if (has_chancecode)
                body_kind = FN_BODY_CHANCECODE;
            else if (has_literal)
                body_kind = FN_BODY_LITERAL;
            (void)0;
            int function_is_managed = managed_override_present ? managed_override_value : ps->module_is_managed;
            Node *fn = parse_function(ps, leading_noreturn, visibility, function_is_managed, 1, body_kind, attrs, attr_count);
            apply_function_attributes(ps, fn, attrs, attr_count);
            clear_pending_attrs(attrs, attr_count);
            attr_count = 0;
            if (decl_count == decl_cap)
            {
                decl_cap = decl_cap ? decl_cap * 2 : 4;
                decls = (Node **)realloc(decls, sizeof(Node *) * decl_cap);
            }
            decls[decl_count++] = fn;
            fn_count++;
            continue;
        }

        if (t.kind == TK_KW_STATIC || t.kind == TK_KW_CONSTANT || is_type_start(ps, t) || parser_is_implicit_ident_decl_start(ps, t))
        {
            if (bring_seen)
                bring_block_done = 1;
            if (extend_seen)
                extend_block_done = 1;
            if (leading_noreturn)
            {
                diag_error_at(lexer_source(ps->lx), noreturn_tok.line, noreturn_tok.col,
                              "'noreturn' is only valid before functions or extern declarations");
                exit(1);
            }

            int is_const = 0;
            int is_static = 0;
            while (1)
            {
                if (!is_static && t.kind == TK_KW_STATIC)
                {
                    lexer_next(ps->lx);
                    is_static = 1;
                    t = lexer_peek(ps->lx);
                    continue;
                }
                if (!is_const && t.kind == TK_KW_CONSTANT)
                {
                    lexer_next(ps->lx);
                    is_const = 1;
                    t = lexer_peek(ps->lx);
                    continue;
                }
                break;
            }

            if (!is_type_start(ps, t) && !parser_is_implicit_ident_decl_start(ps, t))
            {
                diag_error_at(lexer_source(ps->lx), t.line, t.col,
                              "expected a type after storage qualifiers");
                exit(1);
            }

            Type *ty = parse_type_spec(ps);
            ty = parser_decl_canonicalize_bundle_reference(ty);
            Token name = expect(ps, TK_IDENT, "identifier");

            Node *decl = new_node(ND_VAR_DECL);
            char *nm = (char *)xmalloc((size_t)name.length + 1);
            memcpy(nm, name.lexeme, (size_t)name.length);
            nm[name.length] = '\0';
            decl->var_name = nm;
            decl->var_type = ty;
            parse_trailing_funptr_signature(ps, ty);
            decl->var_is_array = (ty && ty->kind == TY_ARRAY);
            decl->var_is_function = (ty && ty->kind == TY_PTR && ty->pointee && ty->pointee->kind == TY_FUNC);
            decl->var_is_const = is_const;
            decl->var_is_static = is_static;
            decl->var_is_global = 1;
            if (is_static && visibility)
            {
                diag_error_at(lexer_source(ps->lx), vis_tok.line, vis_tok.col,
                              "'static' declarations cannot be exposed");
                exit(1);
            }
            decl->is_exposed = visibility && !is_static;
            decl->line = name.line;
            decl->col = name.col;
            decl->src = lexer_source(ps->lx);

            if (attr_count > 0)
            {
                apply_global_attributes(ps, decl, attrs, attr_count);
                clear_pending_attrs(attrs, attr_count);
                attr_count = 0;
            }

            if (!global_seen)
            {
                global_seen = 1;
            }
            else if (!parser_disable_formatting_notes && global_block_done && !global_note_emitted && last_global_line > 0)
            {
                if (!source_only_preprocessor_between(unit_src, last_global_line, decl->line))
                {
                    diag_note_at(lexer_source(ps->lx), decl->line, decl->col,
                                 "formatting: group global variable declarations together (disable with -Nno-formatting)");
                    global_note_emitted = 1;
                }
                else
                {
                    global_block_done = 0;
                }
            }
            last_global_line = decl->line;

            Token next_tok = lexer_peek(ps->lx);
            if (next_tok.kind == TK_ASSIGN)
            {
                lexer_next(ps->lx);
                decl->rhs = parse_initializer(ps);
            }

            parser_maybe_register_const_int(ps, is_const, name, decl->rhs);

            expect(ps, TK_SEMI, ";");

            if (decl_count == decl_cap)
            {
                decl_cap = decl_cap ? decl_cap * 2 : 4;
                decls = (Node **)realloc(decls, sizeof(Node *) * decl_cap);
            }
            decls[decl_count++] = decl;
            continue;
        }

        if (attr_count > 0)
        {
            diag_error_at(lexer_source(ps->lx), attrs[0].line, attrs[0].col,
                          "attributes are only supported before function declarations");
            exit(1);
        }

        diag_error_at(lexer_source(ps->lx), t.line, t.col,
                      "expected declaration; got token kind=%d", t.kind);
        exit(1);
    }

    if (ps->pending_instantiated_decl_count > 0 && ps->pending_instantiated_decls)
    {
        for (int i = 0; i < ps->pending_instantiated_decl_count; ++i)
            parser_unit_append_decl(&decls, &decl_count, &decl_cap,
                                    ps->pending_instantiated_decls[i], &fn_count);
        free(ps->pending_instantiated_decls);
        ps->pending_instantiated_decls = NULL;
        ps->pending_instantiated_decl_count = 0;
        ps->pending_instantiated_decl_cap = 0;
    }

    Node *u = (Node *)xcalloc(1, sizeof(Node));
    u->kind = ND_UNIT;
    u->stmts = decls;
    u->stmt_count = decl_count;
    u->src = lexer_source(ps->lx);
    u->line = 1;
    u->col = 1;

    if (ps->module_part_count > 0)
    {
        const char **parts = (const char **)xcalloc((size_t)ps->module_part_count, sizeof(const char *));
        for (int i = 0; i < ps->module_part_count; ++i)
            parts[i] = ps->module_parts[i];
        u->module_path.parts = parts;
        u->module_path.part_count = ps->module_part_count;
        u->module_path.full_name = ps->module_full_name;
    }
    u->module_is_managed = ps->module_is_managed;

    if (ps->import_count > 0)
    {
        ModulePath *imports = (ModulePath *)xcalloc((size_t)ps->import_count, sizeof(ModulePath));
        for (int i = 0; i < ps->import_count; ++i)
        {
            struct ModuleImport *imp = &ps->imports[i];
            ModulePath *dst = &imports[i];
            if (imp->part_count > 0)
            {
                const char **parts = (const char **)xcalloc((size_t)imp->part_count, sizeof(const char *));
                for (int j = 0; j < imp->part_count; ++j)
                    parts[j] = imp->parts[j];
                dst->parts = parts;
                dst->part_count = imp->part_count;
            }
            dst->full_name = imp->full_name;
            dst->alias = imp->alias;
        }
        u->imports = imports;
        u->import_count = ps->import_count;
    }

    if (fn_count > 0)
    {
        for (int i = 0; i < decl_count; ++i)
        {
            Node *fn = decls[i];
            if (!fn || fn->kind != ND_FUNC)
                continue;
            if (fn->metadata.backend_name || fn->is_entrypoint || fn->export_name)
                continue;
            char *backend = NULL;
            if (ps->module_full_name && *ps->module_full_name)
                backend = module_backend_name(ps->module_full_name, fn->name, fn);
            if (!backend)
                backend = append_param_signature(fn->name, fn);
            if (backend)
                fn->metadata.backend_name = backend;
        }
    }

    clear_pending_attrs(attrs, attr_count);
    free(attrs);

    return u;
}

void parser_export_externs(Parser *ps, SymTable *st)
{
    if (!ps || !st)
        return;
    for (int i = 0; i < ps->ext_count; i++)
    {
        symtab_add(st, ps->externs[i]);
    }
}

const Symbol *parser_get_externs(const Parser *ps, int *count)
{
    if (count)
        *count = ps ? ps->ext_count : 0;
    if (!ps || ps->ext_count == 0)
        return NULL;
    return ps->externs;
}

static void parse_alias_decl(Parser *ps, int is_exposed)
{
    expect(ps, TK_KW_ALIAS, "alias");
    Token name = expect(ps, TK_IDENT, "identifier");
    int is_generic = 0;
    int gen_ptr_depth = 0;
    TypeKind base_kind = TY_VOID;
    int ptr_depth = 0;
    Token maybe_lt = lexer_peek(ps->lx);
    if (maybe_lt.kind == TK_LT)
    {
        
        is_generic = 1;
        lexer_next(ps->lx);
        Token param = expect(ps, TK_IDENT, "type parameter name");
        expect(ps, TK_GT, ">");
        expect(ps, TK_ASSIGN, "=");
        Token t = lexer_next(ps->lx);
        if (!(t.kind == TK_IDENT && t.length == param.length &&
              strncmp(t.lexeme, param.lexeme, t.length) == 0))
        {
            diag_error_at(lexer_source(ps->lx), t.line, t.col,
                          "only simple generic pattern 'T*' is supported");
            exit(1);
        }
        int n = 0;
        Token s = lexer_peek(ps->lx);
        while (s.kind == TK_STAR)
        {
            lexer_next(ps->lx);
            n++;
            s = lexer_peek(ps->lx);
        }
        if (n <= 0)
        {
            diag_error_at(lexer_source(ps->lx), t.line, t.col,
                          "generic alias RHS must be '%.*s*'", param.length,
                          param.lexeme);
            exit(1);
        }
        gen_ptr_depth = n;
        expect(ps, TK_SEMI, ";");
        if (ps->alias_count == ps->alias_cap)
        {
            ps->alias_cap = ps->alias_cap ? ps->alias_cap * 2 : 8;
            ps->aliases = (struct Alias *)realloc(
                ps->aliases, ps->alias_cap * sizeof(*ps->aliases));
        }
        ps->aliases[ps->alias_count].name =
            (char *)xmalloc((size_t)name.length + 1);
        memcpy(ps->aliases[ps->alias_count].name, name.lexeme, (size_t)name.length);
        ps->aliases[ps->alias_count].name[name.length] = '\0';
        ps->aliases[ps->alias_count].name_len = name.length;
        ps->aliases[ps->alias_count].is_generic = 1;
        ps->aliases[ps->alias_count].param =
            (char *)xmalloc((size_t)param.length + 1);
        memcpy(ps->aliases[ps->alias_count].param, param.lexeme,
               (size_t)param.length);
        ps->aliases[ps->alias_count].param[param.length] = '\0';
        ps->aliases[ps->alias_count].param_len = param.length;
        ps->aliases[ps->alias_count].gen_ptr_depth = gen_ptr_depth;
        ps->aliases[ps->alias_count].base_kind = TY_VOID;
        ps->aliases[ps->alias_count].ptr_depth = 0;
        ps->aliases[ps->alias_count].resolved_type = NULL;
        ps->aliases[ps->alias_count].is_exposed = is_exposed;
        ps->alias_count++;
        return;
    }
    
    expect(ps, TK_ASSIGN, "=");
    Type *alias_type = parse_type_spec(ps);
    expect(ps, TK_SEMI, ";");
    if (ps->alias_count == ps->alias_cap)
    {
        ps->alias_cap = ps->alias_cap ? ps->alias_cap * 2 : 8;
        ps->aliases = (struct Alias *)realloc(ps->aliases,
                                              ps->alias_cap * sizeof(*ps->aliases));
    }
    ps->aliases[ps->alias_count].name = (char *)xmalloc((size_t)name.length + 1);
    memcpy(ps->aliases[ps->alias_count].name, name.lexeme, (size_t)name.length);
    ps->aliases[ps->alias_count].name[name.length] = '\0';
    ps->aliases[ps->alias_count].name_len = name.length;
    ps->aliases[ps->alias_count].is_generic = 0;
    ps->aliases[ps->alias_count].param = NULL;
    ps->aliases[ps->alias_count].param_len = 0;
    ps->aliases[ps->alias_count].base_kind = base_kind;
    ps->aliases[ps->alias_count].ptr_depth = ptr_depth;
    ps->aliases[ps->alias_count].gen_ptr_depth = 0;
    ps->aliases[ps->alias_count].resolved_type = alias_type;
    ps->aliases[ps->alias_count].is_exposed = is_exposed;
    ps->alias_count++;
}

static void parse_enum_decl(Parser *ps, int is_exposed)
{
    expect(ps, TK_KW_ENUM, "enum");
    Token name = expect(ps, TK_IDENT, "enum name");
    
    static Type ti32 = {.kind = TY_I32};
    if (named_type_find(ps, name.lexeme, name.length) < 0)
        named_type_add(ps, name.lexeme, name.length, &ti32, is_exposed);
    enum_type_add(ps, name.lexeme, name.length, is_exposed);
    char *enum_name_heap = NULL;
    if (is_exposed && ps->module_full_name)
        enum_name_heap = dup_token_text(name);
    expect(ps, TK_LBRACE, "{");
    int cur = 0;
    for (;;)
    {
        Token t = lexer_peek(ps->lx);
        if (t.kind == TK_RBRACE)
        {
            lexer_next(ps->lx);
            break;
        }
        Token id = expect(ps, TK_IDENT, "enumerator");
        Token p = lexer_peek(ps->lx);
        if (p.kind == TK_ASSIGN)
        {
            lexer_next(ps->lx);
            Node *e = parse_expr(ps);
            
            if (e->kind == ND_INT)
                cur = (int)e->int_val;
            else
                cur = 0;
        }
        
        enum_const_add(ps, id.lexeme, id.length, cur);
        
        int scoped_len = name.length + 2 + id.length;
        char *scoped = (char *)xmalloc((size_t)scoped_len + 1);
        memcpy(scoped, name.lexeme, (size_t)name.length);
        scoped[name.length] = '=';
        scoped[name.length + 1] = '>';
        memcpy(scoped + name.length + 2, id.lexeme, (size_t)id.length);
        scoped[scoped_len] = '\0';
        enum_const_add(ps, scoped, scoped_len, cur);
        free(scoped);
        if (enum_name_heap && ps->module_full_name)
        {
            char *value_name = dup_token_text(id);
            module_registry_register_enum_value(ps->module_full_name, enum_name_heap, value_name, cur);
            free(value_name);
        }
        
        Token sep = lexer_peek(ps->lx);
        if (sep.kind == TK_COMMA)
        {
            lexer_next(ps->lx);
            cur++;
            continue;
        }
        
        
        cur++;
    }
    expect(ps, TK_SEMI, ";");
    if (enum_name_heap && ps->module_full_name)
    {
        module_registry_register_enum(ps->module_full_name, enum_name_heap, &ti32);
        free(enum_name_heap);
    }
}

static Type *parse_inline_union_type(Parser *ps)
{
    expect(ps, TK_LBRACE, "{");
    Type *ut = (Type *)xcalloc(1, sizeof(Type));
    ut->kind = TY_STRUCT;
    ut->is_union = 1;
    ut->strct.field_names = NULL;
    ut->strct.field_types = NULL;
    ut->strct.field_default_values = NULL;
    ut->strct.field_exposed_flags = NULL;
    ut->strct.field_offsets = NULL;
    ut->strct.field_count = 0;
    ut->strct.size_bytes = 0;
    ut->strct.is_packed = 0;

    const char **fnames = NULL;
    Type **ftypes = NULL;
    int *foff = NULL;
    int fcnt = 0, fcap = 0;
    int max_size = 0;
    int max_align = 1;

    while (1)
    {
        Token t = lexer_peek(ps->lx);
        if (t.kind == TK_RBRACE)
        {
            lexer_next(ps->lx);
            break;
        }
        if (!(t.kind == TK_KW_CONSTANT || is_type_start(ps, t)))
        {
            diag_error_at(lexer_source(ps->lx), t.line, t.col,
                          "expected union field declaration or '}'");
            exit(1);
        }
        if (t.kind == TK_KW_CONSTANT)
            lexer_next(ps->lx);
        Type *fty = parse_type_spec(ps);
        Token fname = expect(ps, TK_IDENT, "field name");
        expect(ps, TK_SEMI, ";");

        if (fcnt == fcap)
        {
            fcap = fcap ? fcap * 2 : 4;
            fnames = (const char **)realloc(fnames, sizeof(char *) * fcap);
            ftypes = (Type **)realloc(ftypes, sizeof(Type *) * fcap);
            foff = (int *)realloc(foff, sizeof(int) * fcap);
        }
        char *nm = (char *)xmalloc((size_t)fname.length + 1);
        memcpy(nm, fname.lexeme, (size_t)fname.length);
        nm[fname.length] = '\0';
        fnames[fcnt] = nm;
        ftypes[fcnt] = fty;
        foff[fcnt] = 0;

        int field_align = type_align_simple(fty);
        if (field_align > max_align)
            max_align = field_align;
        int sz = type_sizeof_simple(fty);
        if (fty && fty->kind == TY_STRUCT && sz == 0)
        {
            diag_error_at(lexer_source(ps->lx), fname.line, fname.col,
                          "union field '%.*s' has incomplete type", fname.length,
                          fname.lexeme);
            exit(1);
        }
        if (sz > max_size)
            max_size = sz;
        fcnt++;
    }

    ut->strct.field_names = fnames;
    ut->strct.field_types = ftypes;
    ut->strct.field_default_values = NULL;
    ut->strct.field_exposed_flags = NULL;
    ut->strct.field_offsets = foff;
    ut->strct.field_count = fcnt;
    ut->strct.size_bytes = align_up(max_size, max_align > 0 ? max_align : 1);
    return ut;
}

static void parse_bundle_decl(Parser *ps, int is_exposed, Node ***decls, int *decl_count, int *decl_cap, int *fn_count)
{
    Token kw = lexer_next(ps->lx);
    if (kw.kind != TK_KW_BUNDLE)
    {
        diag_error_at(lexer_source(ps->lx), kw.line, kw.col,
                      "expected bundle declaration");
        exit(1);
    }

    Token name = expect(ps, TK_IDENT, "bundle name");
    int generic_scope_start = ps->generic_param_count;
    int local_generic_count = 0;
    Token maybe_lt = lexer_peek(ps->lx);
    if (maybe_lt.kind == TK_LT)
    {
        lexer_next(ps->lx);
        while (1)
        {
            Token param_tok = expect(ps, TK_IDENT, "generic parameter name");
            for (int i = ps->generic_param_count - 1; i >= generic_scope_start; --i)
            {
                struct GenericParam *existing = &ps->generic_params[i];
                if (existing->name_len == param_tok.length &&
                    strncmp(existing->name, param_tok.lexeme, (size_t)param_tok.length) == 0)
                {
                    diag_error_at(lexer_source(ps->lx), param_tok.line, param_tok.col,
                                  "duplicate generic parameter '%.*s' on bundle '%.*s'",
                                  param_tok.length, param_tok.lexeme, name.length, name.lexeme);
                    exit(1);
                }
            }
            TemplateConstraintKind constraint_kind = TEMPLATE_CONSTRAINT_NONE;
            Type *default_type = NULL;
            parser_parse_generic_param_options(ps, &constraint_kind, &default_type);
            parser_push_generic_param(ps, param_tok, local_generic_count, constraint_kind, default_type);
            local_generic_count++;

            Token sep = lexer_peek(ps->lx);
            if (sep.kind == TK_COMMA)
            {
                lexer_next(ps->lx);
                continue;
            }
            if (sep.kind == TK_GT)
            {
                lexer_next(ps->lx);
                break;
            }
            diag_error_at(lexer_source(ps->lx), sep.line, sep.col,
                          "expected ',' or '>' in generic parameter list");
            exit(1);
        }
        if (local_generic_count == 0)
        {
            diag_error_at(lexer_source(ps->lx), maybe_lt.line, maybe_lt.col,
                          "generic parameter list cannot be empty");
            exit(1);
        }
    }
    int is_template_bundle = (local_generic_count > 0);

    Token after_name = lexer_peek(ps->lx);

    if (after_name.kind == TK_SEMI)
    {
        if (is_template_bundle)
        {
            diag_error_at(lexer_source(ps->lx), after_name.line, after_name.col,
                          "template bundle '%.*s' requires a full definition",
                          name.length, name.lexeme);
            exit(1);
        }
        lexer_next(ps->lx);
        Type *existing = named_type_get(ps, name.lexeme, name.length);
        if (existing)
        {
            if (existing->kind != TY_STRUCT)
            {
                diag_error_at(lexer_source(ps->lx), name.line, name.col,
                              "type '%.*s' already declared with non-aggregate kind",
                              name.length, name.lexeme);
                exit(1);
            }
            if (!existing->is_bundle)
            {
                diag_error_at(lexer_source(ps->lx), name.line, name.col,
                              "type '%.*s' is declared as struct, not bundle",
                              name.length, name.lexeme);
                exit(1);
            }
            if (is_exposed && !existing->is_exposed)
                existing->is_exposed = 1;
            return;
        }
        Type *forward = (Type *)xcalloc(1, sizeof(Type));
        forward->kind = TY_STRUCT;
        forward->is_bundle = 1;
        forward->struct_name = (char *)xmalloc((size_t)name.length + 1);
        memcpy((char *)forward->struct_name, name.lexeme, (size_t)name.length);
        ((char *)forward->struct_name)[name.length] = '\0';
        forward->is_exposed = is_exposed;
        forward->strct.field_count = 0;
        forward->strct.size_bytes = 0;
        forward->strct.field_default_values = NULL;
        forward->strct.field_exposed_flags = NULL;
        named_type_add(ps, name.lexeme, name.length, forward, is_exposed);
        parser_pop_generic_params(ps, local_generic_count);
        return;
    }

    expect(ps, TK_LBRACE, "{");

    struct BundleTemplate *existing_template = parser_find_bundle_template(ps, name.lexeme, name.length);
    if (existing_template)
    {
        diag_error_at(lexer_source(ps->lx), name.line, name.col,
                      "redefinition of template bundle '%.*s'",
                      name.length, name.lexeme);
        exit(1);
    }

    Type *bundle_type = named_type_get(ps, name.lexeme, name.length);
    int is_new_type = 0;
    if (bundle_type)
    {
        if (bundle_type->kind != TY_STRUCT)
        {
            diag_error_at(lexer_source(ps->lx), name.line, name.col,
                          "type '%.*s' already declared with non-aggregate kind",
                          name.length, name.lexeme);
            exit(1);
        }
        if (!bundle_type->is_bundle)
        {
            diag_error_at(lexer_source(ps->lx), name.line, name.col,
                          "type '%.*s' is declared as struct, not bundle",
                          name.length, name.lexeme);
            exit(1);
        }
        if (bundle_type->strct.field_count > 0)
        {
            diag_error_at(lexer_source(ps->lx), name.line, name.col,
                          "redefinition of bundle '%.*s'", name.length, name.lexeme);
            exit(1);
        }
    }
    else
    {
        bundle_type = (Type *)xcalloc(1, sizeof(Type));
        bundle_type->kind = TY_STRUCT;
        bundle_type->is_bundle = 1;
        bundle_type->struct_name = (char *)xmalloc((size_t)name.length + 1);
        memcpy((char *)bundle_type->struct_name, name.lexeme, (size_t)name.length);
        ((char *)bundle_type->struct_name)[name.length] = '\0';
        is_new_type = 1;
    }

    bundle_type->is_bundle = 1;
    bundle_type->is_exposed = bundle_type->is_exposed || is_exposed;
    bundle_type->bundle_ctor_declared = 0;
    bundle_type->bundle_ctor_declared_exposed = 0;
    bundle_type->bundle_ctor_has_zero_arity = 0;
    bundle_type->bundle_ctor_has_exposed_zero_arity = 0;
    bundle_type->strct.field_names = NULL;
    bundle_type->strct.field_types = NULL;
    bundle_type->strct.field_default_values = NULL;
    bundle_type->strct.field_exposed_flags = NULL;
    bundle_type->strct.field_offsets = NULL;
    bundle_type->strct.field_count = 0;
    bundle_type->strct.size_bytes = 0;

    if (is_new_type)
        named_type_add(ps, name.lexeme, name.length, bundle_type, is_exposed);

    const char **fnames = NULL;
    Type **ftypes = NULL;
    const char **fdefs = NULL;
    unsigned char *fexposed = NULL;
    int *foff = NULL;
    int fcnt = 0;
    int fcap = 0;
    int offset = 0;
    int max_align = 1;
    Node **template_methods = NULL;
    int template_method_count = 0;
    int template_method_cap = 0;
    struct BundleTemplate *template_record = NULL;

    if (is_template_bundle)
    {
        if (ps->bundle_template_count == ps->bundle_template_cap)
        {
            int new_cap = ps->bundle_template_cap ? ps->bundle_template_cap * 2 : 4;
            struct BundleTemplate *grown = (struct BundleTemplate *)realloc(
                ps->bundle_templates, (size_t)new_cap * sizeof(struct BundleTemplate));
            if (!grown)
            {
                diag_error("out of memory while registering template bundle");
                exit(1);
            }
            ps->bundle_templates = grown;
            ps->bundle_template_cap = new_cap;
        }

        template_record = &ps->bundle_templates[ps->bundle_template_count++];
        memset(template_record, 0, sizeof(*template_record));
        template_record->name = xstrdup(bundle_type->struct_name ? bundle_type->struct_name : "");
        template_record->name_len = (int)strlen(template_record->name);
        template_record->is_exposed = bundle_type->is_exposed ? 1 : 0;
        template_record->bundle_type = bundle_type;
        template_record->generic_param_count = local_generic_count;

        if (local_generic_count > 0)
        {
            template_record->generic_param_names = (const char **)xcalloc((size_t)local_generic_count, sizeof(const char *));
            template_record->generic_param_types = (Type **)xcalloc((size_t)local_generic_count, sizeof(Type *));
            for (int i = 0; i < local_generic_count; ++i)
            {
                struct GenericParam *gp = &ps->generic_params[generic_scope_start + i];
                template_record->generic_param_names[i] = gp->name ? xstrdup(gp->name) : xstrdup("T");
                template_record->generic_param_types[i] = gp->placeholder;
            }
        }
    }

    while (1)
    {
        Token t = lexer_peek(ps->lx);
        if (t.kind == TK_RBRACE)
        {
            lexer_next(ps->lx);
            break;
        }

        int member_is_exposed = 0;
        int member_is_static = 0;
        int member_is_noreturn = 0;
        int saw_visibility = 0;
        int saw_static = 0;
        int saw_noreturn = 0;

        while (1)
        {
            int consumed = 0;
            if (t.kind == TK_KW_HIDE || t.kind == TK_KW_EXPOSE)
            {
                if (saw_visibility)
                {
                    diag_error_at(lexer_source(ps->lx), t.line, t.col,
                                  "duplicate visibility modifier on bundle member");
                    exit(1);
                }
                Token member_vis = lexer_next(ps->lx);
                member_is_exposed = (member_vis.kind == TK_KW_EXPOSE);
                saw_visibility = 1;
                consumed = 1;
            }
            else if (t.kind == TK_KW_STATIC)
            {
                if (saw_static)
                {
                    diag_error_at(lexer_source(ps->lx), t.line, t.col,
                                  "duplicate 'static' modifier on bundle member");
                    exit(1);
                }
                lexer_next(ps->lx);
                member_is_static = 1;
                saw_static = 1;
                consumed = 1;
            }
            else if (t.kind == TK_KW_NORETURN)
            {
                if (saw_noreturn)
                {
                    diag_error_at(lexer_source(ps->lx), t.line, t.col,
                                  "duplicate 'noreturn' modifier on bundle member");
                    exit(1);
                }
                lexer_next(ps->lx);
                member_is_noreturn = 1;
                saw_noreturn = 1;
                consumed = 1;
            }

            if (!consumed)
                break;
            t = lexer_peek(ps->lx);
        }

        int is_on_lifecycle = (t.kind == TK_IDENT && t.length == 2 && strncmp(t.lexeme, "on", 2) == 0);
        if (t.kind == TK_PLUS || t.kind == TK_MINUS || is_on_lifecycle)
        {
            if (member_is_static)
            {
                diag_error_at(lexer_source(ps->lx), t.line, t.col,
                              "'static' is not valid on bundle constructor/destructor declarations");
                exit(1);
            }

            Node *method = NULL;
            if (is_on_lifecycle)
            {
                Token on_tok = lexer_next(ps->lx);
                method = parse_bundle_on_method(ps, on_tok, member_is_noreturn, member_is_exposed, ps->module_is_managed);
            }
            else
            {
                Token sign_tok = lexer_next(ps->lx);
                method = parse_bundle_this_method(ps, sign_tok, member_is_noreturn, member_is_exposed, ps->module_is_managed);
            }
            if (!method || !method->name)
            {
                diag_error_at(lexer_source(ps->lx), t.line, t.col,
                              "invalid bundle constructor/destructor declaration");
                exit(1);
            }

            method->is_exposed = member_is_exposed;
            method->is_bundle_global_init = 0;
            if (!member_is_static)
                parser_prepend_bundle_this_param(ps, method, bundle_type);
            method->var_is_static = member_is_static ? 1 : 0;

            if (method->name && strcmp(method->name, "init") == 0)
            {
                int ctor_arg_count = method->param_count;
                if (!member_is_static && ctor_arg_count > 0)
                    ctor_arg_count -= 1;
                if (ctor_arg_count < 0)
                    ctor_arg_count = 0;
                bundle_type->bundle_ctor_declared = 1;
                if (member_is_exposed)
                    bundle_type->bundle_ctor_declared_exposed = 1;
                if (ctor_arg_count == 0)
                {
                    bundle_type->bundle_ctor_has_zero_arity = 1;
                    if (member_is_exposed)
                        bundle_type->bundle_ctor_has_exposed_zero_arity = 1;
                }
            }

            if (is_template_bundle)
            {
                if (template_method_count == template_method_cap)
                {
                    int new_cap = template_method_cap ? template_method_cap * 2 : 4;
                    Node **grown = (Node **)realloc(template_methods, (size_t)new_cap * sizeof(Node *));
                    if (!grown)
                    {
                        diag_error("out of memory while storing template bundle methods");
                        exit(1);
                    }
                    template_methods = grown;
                    template_method_cap = new_cap;
                }
                template_methods[template_method_count++] = method;
                continue;
            }

            char *user_name = xstrdup(method->name);
            char *symbol_name = parser_make_bundle_instance_method_symbol(bundle_type->struct_name, user_name);
            if (!symbol_name)
            {
                diag_error("failed to build bundle method symbol name");
                exit(1);
            }

            free((void *)method->name);
            method->name = symbol_name;

            free(user_name);
            parser_unit_append_decl(decls, decl_count, decl_cap, method, fn_count);
            continue;
        }

        if (t.kind == TK_KW_FUN)
        {
            Node *method = parse_function(ps, member_is_noreturn, member_is_exposed, ps->module_is_managed, 0, FN_BODY_NORMAL, NULL, 0);
            if (!method || !method->name)
            {
                diag_error_at(lexer_source(ps->lx), t.line, t.col,
                              "invalid bundle method declaration");
                exit(1);
            }

            parser_validate_bundle_index_overrider_signature(ps, method, member_is_static);

            method->is_exposed = member_is_exposed;
            method->is_bundle_global_init = 0;
            if (!member_is_static)
                parser_prepend_bundle_this_param(ps, method, bundle_type);
            method->var_is_static = member_is_static ? 1 : 0;

            if (is_template_bundle)
            {
                if (template_method_count == template_method_cap)
                {
                    int new_cap = template_method_cap ? template_method_cap * 2 : 4;
                    Node **grown = (Node **)realloc(template_methods, (size_t)new_cap * sizeof(Node *));
                    if (!grown)
                    {
                        diag_error("out of memory while storing template bundle methods");
                        exit(1);
                    }
                    template_methods = grown;
                    template_method_cap = new_cap;
                }
                template_methods[template_method_count++] = method;
                continue;
            }

            char *user_name = xstrdup(method->name);
            char *symbol_name = member_is_static
                                    ? parser_make_bundle_static_method_symbol(bundle_type->struct_name, user_name)
                                    : parser_make_bundle_instance_method_symbol(bundle_type->struct_name, user_name);
            if (!symbol_name)
            {
                diag_error("failed to build bundle method symbol name");
                exit(1);
            }

            free((void *)method->name);
            method->name = symbol_name;
            if (member_is_static)
                parser_record_bundle_static_member(ps, bundle_type->struct_name, user_name, symbol_name, 1, 1);

            free(user_name);
            parser_unit_append_decl(decls, decl_count, decl_cap, method, fn_count);
            continue;
        }

        int field_is_const = 0;
        if (t.kind == TK_KW_CONSTANT)
        {
            lexer_next(ps->lx);
            field_is_const = 1;
            t = lexer_peek(ps->lx);
        }

        if (!is_type_start(ps, t))
        {
            diag_error_at(lexer_source(ps->lx), t.line, t.col,
                          "expected bundle member declaration or '}'");
            exit(1);
        }

        Type *fty = parse_type_spec(ps);
        Token fname = expect(ps, TK_IDENT, "member name");
        parse_trailing_funptr_signature(ps, fty);

        Node *init_expr = NULL;
        char *field_default = NULL;
        Token maybe_assign = lexer_peek(ps->lx);
        if (maybe_assign.kind == TK_ASSIGN)
        {
            lexer_next(ps->lx);
            if (member_is_static)
            {
                init_expr = parse_initializer(ps);
            }
            else
            {
                field_default = parser_serialize_struct_field_default(ps, fty, parse_expr(ps));
            }
        }
        expect(ps, TK_SEMI, ";");

        if (is_template_bundle && member_is_static)
        {
            diag_error_at(lexer_source(ps->lx), fname.line, fname.col,
                          "template bundles currently do not support static fields");
            exit(1);
        }

        if (member_is_static)
        {
            char *field_name = (char *)xmalloc((size_t)fname.length + 1);
            memcpy(field_name, fname.lexeme, (size_t)fname.length);
            field_name[fname.length] = '\0';
            char *symbol_name = parser_make_bundle_static_field_symbol(bundle_type->struct_name, field_name);

            Node *decl = new_node(ND_VAR_DECL);
            decl->var_name = symbol_name;
            decl->var_type = fty;
            decl->var_is_const = field_is_const;
            decl->var_is_static = 1;
            decl->var_is_global = 1;
            decl->var_is_array = (fty && fty->kind == TY_ARRAY) ? 1 : 0;
            decl->var_is_function = (fty && fty->kind == TY_PTR && fty->pointee && fty->pointee->kind == TY_FUNC) ? 1 : 0;
            decl->is_exposed = member_is_exposed;
            decl->rhs = init_expr;
            decl->line = fname.line;
            decl->col = fname.col;
            decl->src = lexer_source(ps->lx);

            parser_record_bundle_static_member(ps, bundle_type->struct_name, field_name, symbol_name, 0, field_is_const);
            free(field_name);

            parser_unit_append_decl(decls, decl_count, decl_cap, decl, fn_count);
            continue;
        }

        if (member_is_exposed && fty && fty->kind == TY_STRUCT && !fty->is_exposed)
        {
            const char *hidden_name = fty->struct_name ? fty->struct_name : "<anonymous>";
            diag_error_at(lexer_source(ps->lx), fname.line, fname.col,
                          "exposed field '%.*s' in bundle '%.*s' uses hidden struct '%s'",
                          fname.length, fname.lexeme,
                          name.length, name.lexeme, hidden_name);
            exit(1);
        }

        if (fcnt == fcap)
        {
            fcap = fcap ? fcap * 2 : 8;
            fnames = (const char **)realloc(fnames, sizeof(char *) * (size_t)fcap);
            ftypes = (Type **)realloc(ftypes, sizeof(Type *) * (size_t)fcap);
            fdefs = (const char **)realloc(fdefs, sizeof(char *) * (size_t)fcap);
            fexposed = (unsigned char *)realloc(fexposed, sizeof(unsigned char) * (size_t)fcap);
            foff = (int *)realloc(foff, sizeof(int) * (size_t)fcap);
            if (!fnames || !ftypes || !fdefs || !fexposed || !foff)
            {
                diag_error("out of memory while parsing bundle fields");
                exit(1);
            }
        }

        char *field_name = (char *)xmalloc((size_t)fname.length + 1);
        memcpy(field_name, fname.lexeme, (size_t)fname.length);
        field_name[fname.length] = '\0';
        fnames[fcnt] = field_name;
        ftypes[fcnt] = fty;
        fdefs[fcnt] = field_default;
        fexposed[fcnt] = member_is_exposed ? 1 : 0;

        int field_align = type_align_simple(fty);
        if (field_align <= 0)
            field_align = 1;
        int field_size = type_sizeof_simple(fty);
        if (fty && fty->kind == TY_STRUCT && field_size == 0)
        {
            diag_error_at(lexer_source(ps->lx), fname.line, fname.col,
                          "bundle field '%.*s' has incomplete type",
                          fname.length, fname.lexeme);
            exit(1);
        }

        offset = align_up(offset, field_align);
        foff[fcnt] = offset;
        offset += field_size;
        if (field_align > max_align)
            max_align = field_align;
        fcnt++;
    }

    bundle_type->strct.field_names = fnames;
    bundle_type->strct.field_types = ftypes;
    bundle_type->strct.field_default_values = fdefs;
    bundle_type->strct.field_exposed_flags = fexposed;
    bundle_type->strct.field_offsets = foff;
    bundle_type->strct.field_count = fcnt;
    offset = align_up(offset, max_align > 0 ? max_align : 1);
    if (bundle_type->is_bundle && fcnt == 0 && offset <= 0)
        offset = 1;
    bundle_type->strct.size_bytes = offset;

    if (is_template_bundle)
    {
        if (!template_record)
        {
            diag_error("internal error: missing template bundle record");
            exit(1);
        }
        template_record->field_names = fnames;
        template_record->field_types = ftypes;
        template_record->field_default_values = fdefs;
        template_record->field_exposed_flags = fexposed;
        template_record->field_count = fcnt;
        template_record->method_templates = template_methods;
        template_record->method_template_count = template_method_count;
        template_record->method_template_cap = template_method_cap;

        parser_auto_seed_template_bundle(ps, template_record, name.line, name.col);
    }

    if (!is_template_bundle && is_exposed && ps->module_full_name)
        module_registry_register_struct(ps->module_full_name, bundle_type);

    expect(ps, TK_SEMI, ";");
    parser_pop_generic_params(ps, local_generic_count);
}

static int parser_field_default_allows_null(const Type *ty)
{
    return ty && (ty->kind == TY_PTR || ty->kind == TY_REF || ty->is_object);
}

static int parser_field_default_allows_string(const Type *ty)
{
    return ty && ty->kind == TY_PTR && ty->pointee && ty->pointee->kind == TY_CHAR;
}

static int parser_field_default_allows_int(const Type *ty)
{
    if (!ty)
        return 0;
    switch (ty->kind)
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

static int parser_field_default_allows_float(const Type *ty)
{
    return ty && (ty->kind == TY_F32 || ty->kind == TY_F64 || ty->kind == TY_F128);
}

static char *parser_serialize_struct_field_default(Parser *ps, const Type *field_type, Node *expr)
{
    Node *cur = expr;
    while (cur && cur->kind == ND_CAST)
        cur = cur->lhs;
    if (!cur)
    {
        diag_error("invalid struct field default initializer");
        exit(1);
    }

    if (cur->kind == ND_NULL)
    {
        if (!parser_field_default_allows_null(field_type))
        {
            diag_error_at(cur->src, cur->line, cur->col,
                          "field default 'null' requires pointer-like field type");
            exit(1);
        }
        return xstrdup("N");
    }

    if (cur->kind == ND_STRING)
    {
        if (!parser_field_default_allows_string(field_type))
        {
            diag_error_at(cur->src, cur->line, cur->col,
                          "string field defaults require a string-compatible pointer field");
            exit(1);
        }
        size_t len = cur->str_len > 0 ? (size_t)cur->str_len : 0;
        int prefix_len = snprintf(NULL, 0, "S%zu:", len);
        char *out = (char *)xmalloc((size_t)prefix_len + len + 1);
        snprintf(out, (size_t)prefix_len + 1, "S%zu:", len);
        if (len > 0 && cur->str_data)
            memcpy(out + prefix_len, cur->str_data, len);
        out[prefix_len + len] = '\0';
        return out;
    }

    if (cur->kind == ND_INT)
    {
        if (!parser_field_default_allows_int(field_type))
        {
            diag_error_at(cur->src, cur->line, cur->col,
                          "integer field default is not compatible with this field type");
            exit(1);
        }
        char buf[64];
        if (cur->int_is_unsigned)
            snprintf(buf, sizeof(buf), "U%llu", (unsigned long long)cur->int_uval);
        else
            snprintf(buf, sizeof(buf), "I%lld", (long long)cur->int_val);
        return xstrdup(buf);
    }

    if (cur->kind == ND_FLOAT)
    {
        if (!parser_field_default_allows_float(field_type))
        {
            diag_error_at(cur->src, cur->line, cur->col,
                          "floating-point field default is not compatible with this field type");
            exit(1);
        }
        char buf[64];
        snprintf(buf, sizeof(buf), "F%.17g", cur->float_val);
        return xstrdup(buf);
    }

    if (cur->kind == ND_NEG && cur->lhs)
    {
        Node *inner = cur->lhs;
        while (inner && inner->kind == ND_CAST)
            inner = inner->lhs;
        if (inner && inner->kind == ND_INT)
        {
            if (!parser_field_default_allows_int(field_type))
            {
                diag_error_at(cur->src, cur->line, cur->col,
                              "integer field default is not compatible with this field type");
                exit(1);
            }
            char buf[64];
            snprintf(buf, sizeof(buf), "I%lld", (long long)(-inner->int_val));
            return xstrdup(buf);
        }
        if (inner && inner->kind == ND_FLOAT)
        {
            if (!parser_field_default_allows_float(field_type))
            {
                diag_error_at(cur->src, cur->line, cur->col,
                              "floating-point field default is not compatible with this field type");
                exit(1);
            }
            char buf[64];
            snprintf(buf, sizeof(buf), "F%.17g", -inner->float_val);
            return xstrdup(buf);
        }
    }

    diag_error_at(cur->src, cur->line, cur->col,
                  "struct field defaults currently support only literal numbers, strings, and null");
    exit(1);
}

static void parse_struct_decl(Parser *ps, int is_exposed, int is_union, int is_packed)
{
    Token kw = lexer_next(ps->lx);
    if (kw.kind != (is_union ? TK_KW_UNION : TK_KW_STRUCT))
    {
        diag_error_at(lexer_source(ps->lx), kw.line, kw.col,
                      "expected %s declaration", is_union ? "union" : "struct");
        exit(1);
    }
    Token name = expect(ps, TK_IDENT, is_union ? "union name" : "struct name");
    Token after_name = lexer_peek(ps->lx);
    
    if (after_name.kind == TK_SEMI)
    {
        lexer_next(ps->lx);
        Type *existing = named_type_get(ps, name.lexeme, name.length);
        if (existing)
        {
            if (existing->kind != TY_STRUCT)
            {
                diag_error_at(lexer_source(ps->lx), name.line, name.col,
                              "type '%.*s' already declared with non-aggregate kind",
                              name.length, name.lexeme);
                exit(1);
            }
            if (existing->is_bundle)
            {
                diag_error_at(lexer_source(ps->lx), name.line, name.col,
                              "type '%.*s' is declared as bundle, not %s",
                              name.length, name.lexeme, is_union ? "union" : "struct");
                exit(1);
            }
            if (!!existing->is_union != !!is_union)
            {
                diag_error_at(lexer_source(ps->lx), name.line, name.col,
                              "type '%.*s' already declared as %s",
                              name.length, name.lexeme,
                              existing->is_union ? "union" : "struct");
                exit(1);
            }
            if (is_exposed && !existing->is_exposed)
                existing->is_exposed = 1;
            if (!is_union && is_packed)
                existing->strct.is_packed = 1;
            return;
        }
        Type *forward = (Type *)xcalloc(1, sizeof(Type));
        forward->kind = TY_STRUCT;
        forward->is_bundle = 0;
        forward->is_union = !!is_union;
        forward->struct_name = (char *)xmalloc((size_t)name.length + 1);
        memcpy((char *)forward->struct_name, name.lexeme, (size_t)name.length);
        ((char *)forward->struct_name)[name.length] = '\0';
        forward->is_exposed = is_exposed;
        forward->strct.field_count = 0;
        forward->strct.size_bytes = 0;
        forward->strct.is_packed = (!is_union && is_packed) ? 1 : 0;
        forward->strct.field_default_values = NULL;
        forward->strct.field_exposed_flags = NULL;
        named_type_add(ps, name.lexeme, name.length, forward, is_exposed);
        return;
    }

    expect(ps, TK_LBRACE, "{");
    
    Type *st = named_type_get(ps, name.lexeme, name.length);
    int is_new_struct = 0;
    if (st)
    {
        if (st->kind != TY_STRUCT)
        {
            diag_error_at(lexer_source(ps->lx), name.line, name.col,
                          "type '%.*s' already declared with non-aggregate kind",
                          name.length, name.lexeme);
            exit(1);
        }
        if (st->is_bundle)
        {
            diag_error_at(lexer_source(ps->lx), name.line, name.col,
                          "type '%.*s' is declared as bundle, not %s",
                          name.length, name.lexeme, is_union ? "union" : "struct");
            exit(1);
        }
        if (!!st->is_union != !!is_union)
        {
            diag_error_at(lexer_source(ps->lx), name.line, name.col,
                          "type '%.*s' already declared as %s",
                          name.length, name.lexeme,
                          st->is_union ? "union" : "struct");
            exit(1);
        }
        if (st->strct.field_count > 0)
        {
            diag_error_at(lexer_source(ps->lx), name.line, name.col,
                          "redefinition of %s '%.*s'",
                          is_union ? "union" : "struct", name.length, name.lexeme);
            exit(1);
        }
    }
    else
    {
        st = (Type *)xcalloc(1, sizeof(Type));
        st->kind = TY_STRUCT;
        st->is_bundle = 0;
        st->is_union = !!is_union;
        st->struct_name = (char *)xmalloc((size_t)name.length + 1);
        memcpy((char *)st->struct_name, name.lexeme, (size_t)name.length);
        ((char *)st->struct_name)[name.length] = '\0';
        is_new_struct = 1;
    }
    st->is_union = !!is_union;
    st->is_bundle = 0;
    st->is_exposed = st->is_exposed || is_exposed;
    st->strct.is_packed = (!is_union && (st->strct.is_packed || is_packed)) ? 1 : 0;
    
    if (is_new_struct)
        named_type_add(ps, name.lexeme, name.length, st, is_exposed);

    
    st->strct.field_names = NULL;
    st->strct.field_types = NULL;
    st->strct.field_default_values = NULL;
    st->strct.field_exposed_flags = NULL;
    st->strct.field_offsets = NULL;
    st->strct.field_count = 0;
    st->strct.size_bytes = 0;

    
    const char **fnames = NULL;
    Type **ftypes = NULL;
    const char **fdefs = NULL;
    int *foff = NULL;
    int fcnt = 0, fcap = 0;
    int offset = 0;
    int max_size = 0;
    int max_align = 1;
    int packed_layout = (!is_union && st->strct.is_packed);
    while (1)
    {
        Token t = lexer_peek(ps->lx);
        if (t.kind == TK_RBRACE)
        {
            lexer_next(ps->lx);
            break;
        }
        if (!(t.kind == TK_KW_CONSTANT || is_type_start(ps, t)))
        {
            diag_error_at(lexer_source(ps->lx), t.line, t.col, "expected field declaration or '}'");
            exit(1);
        }
        int is_const = 0;
        if (t.kind == TK_KW_CONSTANT)
        {
            lexer_next(ps->lx);
            is_const = 1;
        }
        (void)is_const; 
        Type *fty = parse_type_spec(ps);
        Token fname = expect(ps, TK_IDENT, "field name");
        char *field_default = NULL;
        Token maybe_assign = lexer_peek(ps->lx);
        if (maybe_assign.kind == TK_ASSIGN)
        {
            if (is_union)
            {
                diag_error_at(lexer_source(ps->lx), maybe_assign.line, maybe_assign.col,
                              "union fields do not support default initializers");
                exit(1);
            }
            lexer_next(ps->lx);
            field_default = parser_serialize_struct_field_default(ps, fty, parse_expr(ps));
        }
        
        if (is_exposed)
        {
            if (fty && fty->kind == TY_STRUCT && !fty->is_exposed)
            {
                const char *hidden_name = fty->struct_name ? fty->struct_name : "<anonymous>";
                diag_error_at(lexer_source(ps->lx), fname.line, fname.col,
                              "exposed %s '%.*s' field '%.*s' uses hidden %s '%s'",
                              is_union ? "union" : "struct",
                              name.length, name.lexeme, fname.length, fname.lexeme,
                              fty->is_union ? "union" : "struct", hidden_name);
                exit(1);
            }
        }
        expect(ps, TK_SEMI, ";");
        if (fcnt == fcap)
        {
            fcap = fcap ? fcap * 2 : 4;
            fnames = (const char **)realloc(fnames, sizeof(char *) * fcap);
            ftypes = (Type **)realloc(ftypes, sizeof(Type *) * fcap);
            fdefs = (const char **)realloc(fdefs, sizeof(char *) * fcap);
            foff = (int *)realloc(foff, sizeof(int) * fcap);
        }
        char *nm = (char *)xmalloc((size_t)fname.length + 1);
        memcpy(nm, fname.lexeme, (size_t)fname.length);
        nm[fname.length] = '\0';
        fnames[fcnt] = nm;
        ftypes[fcnt] = fty;
        fdefs[fcnt] = field_default;
        fcnt++;
        int field_align = type_align_simple(fty);
        if (field_align > max_align)
            max_align = field_align;
        int sz = type_sizeof_simple(fty);
        if (fty && fty->kind == TY_STRUCT && sz == 0)
        {
            diag_error_at(lexer_source(ps->lx), fname.line, fname.col,
                          "%s field '%.*s' has incomplete type",
                          is_union ? "union" : "struct", fname.length,
                          fname.lexeme);
            exit(1);
        }
        if (is_union)
        {
            foff[fcnt - 1] = 0;
            if (sz > max_size)
                max_size = sz;
        }
        else
        {
            offset = align_up(offset, field_align);
            if (!packed_layout)
                offset = align_up(offset, field_align);
            foff[fcnt - 1] = offset;
            offset += sz;
        }
    }
    st->strct.field_names = fnames;
    st->strct.field_types = ftypes;
    st->strct.field_default_values = fdefs;
    st->strct.field_exposed_flags = NULL;
    st->strct.field_offsets = foff;
    st->strct.field_count = fcnt;
    int struct_align = max_align > 0 ? max_align : 1;
    if (is_union)
        st->strct.size_bytes = align_up(max_size, struct_align);
    else
    {
        if (!packed_layout)
            offset = align_up(offset, struct_align);
        st->strct.size_bytes = offset;
    }
    
    if (is_exposed && ps->module_full_name)
        module_registry_register_struct(ps->module_full_name, st);
    expect(ps, TK_SEMI, ";");
}
