#include "ast.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

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
static Type ty_void_ptr = {.kind = TY_PTR, .pointee = &ty_void};

static Type *make_ptr_chain(Type *base, int depth)
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

static int type_is_int(Type *t)
{
    int needs_malloc;
    int needs_free;
    return t && (t->kind == TY_I8 || t->kind == TY_U8 || t->kind == TY_I16 ||
                 t->kind == TY_U16 || t->kind == TY_I32 || t->kind == TY_U32 ||
                 t->kind == TY_I64 || t->kind == TY_U64);
}
static int type_equal(Type *a, Type *b)
{
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
    if (target->kind == TY_PTR && rhs->kind == ND_NULL)
    {
        rhs->type = target;
        return 1;
    }
    if (rhs->type && type_equal(target, rhs->type))
        return 1;
    if (literal_fits_type(target, rhs))
    {
        rhs->type = target;
        return 1;
    }
    return 0;
}

static void check_array_literal_for_pointer(SemaContext *sc, Node *init, Type *ptr_ty)
{
    if (!init || !ptr_ty)
        return;
    if (ptr_ty->kind != TY_PTR || !ptr_ty->pointee)
    {
        diag_error_at(init->src, init->line, init->col,
                      "array literal requires pointer element type");
        exit(1);
    }
    Type *elem_ty = ptr_ty->pointee;
    if (init->init.count == 0)
    {
        init->type = ptr_ty;
        return;
    }
    for (int i = 0; i < init->init.count; i++)
    {
        Node *elem = (init->init.elems && i < init->init.count) ? init->init.elems[i] : NULL;
        if (!elem)
        {
            diag_error_at(init->src, init->line, init->col,
                          "missing element in array literal");
            exit(1);
        }
        if (elem_ty->kind == TY_PTR && elem->kind == ND_INIT_LIST && elem->init.is_array_literal)
        {
            check_array_literal_for_pointer(sc, elem, elem_ty);
            continue;
        }
        if (elem->kind == ND_INIT_LIST && elem->init.is_array_literal)
        {
            diag_error_at(elem->src, elem->line, elem->col,
                          "nested array literal requires pointer element type");
            exit(1);
        }
        check_expr(sc, elem);
        if (!can_assign(elem_ty, elem))
        {
            diag_error_at(elem->src, elem->line, elem->col,
                          "array element type mismatch");
            exit(1);
        }
    }
    init->type = ptr_ty;
    sc->needs_malloc = 1;
}

static void check_initializer_for_type(SemaContext *sc, Node *init, Type *target)
{
    if (!init || !target)
        return;
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
    if (init->init.is_array_literal)
    {
        if (!target || target->kind != TY_PTR)
        {
            diag_error_at(init->src, init->line, init->col,
                          "array literal can only initialize pointer types");
            exit(1);
        }
        check_array_literal_for_pointer(sc, init, target);
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
    case ND_NULL:
        return "ND_NULL";
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
    case ND_ADDR:
        return "ND_ADDR";
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
    case ND_MEMBER:
        return "ND_MEMBER";
    case ND_INIT_LIST:
        return "ND_INIT_LIST";
    default:
        return "<unknown-node>";
    }
}

static void check_expr(SemaContext *sc, Node *e)
{
    if (e->kind == ND_INT)
    {
        e->type = &ty_i32;
        return;
    }
    if (e->kind == ND_NULL)
    {
        static Type null_ptr = {.kind = TY_PTR, .pointee = &ty_void};
        e->type = &null_ptr;
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
        e->type = t;
        return;
    }
    if (e->kind == ND_ADDR)
    {
        if (!e->lhs)
        {
            diag_error_at(e->src, e->line, e->col,
                          "address-of requires an operand");
            exit(1);
        }
        check_expr(sc, e->lhs);
        Node *base = e->lhs;
        if (base->kind == ND_CAST && base->lhs)
            base = base->lhs;
        int is_lvalue = (base->kind == ND_VAR || base->kind == ND_MEMBER || base->kind == ND_INDEX);
        if (!is_lvalue)
        {
            diag_error_at(e->lhs->src, e->lhs->line, e->lhs->col,
                          "operand of '&' must be an lvalue");
            exit(1);
        }
        if (!e->lhs->type)
        {
            diag_error_at(e->lhs->src, e->lhs->line, e->lhs->col,
                          "cannot take address of incomplete type");
            exit(1);
        }
        Type *ptr = (Type *)xcalloc(1, sizeof(Type));
        ptr->kind = TY_PTR;
        ptr->pointee = e->lhs->type;
        e->type = ptr;
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
        Type *base = e->lhs->type;
        if (e->is_pointer_deref)
        {
            if (!base || base->kind != TY_PTR || !base->pointee)
            {
                diag_error_at(e->src, e->line, e->col,
                              "'->' requires pointer to struct");
                exit(1);
            }
            base = base->pointee;
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
    if (e->kind == ND_ADD)
    {
        check_expr(sc, e->lhs);
        check_expr(sc, e->rhs);
        if (!type_equal(e->lhs->type, e->rhs->type))
        {
            diag_error_at(e->src, e->line, e->col,
                          "'+' requires both operands to have the same type");
            exit(1);
        }
        e->type = e->lhs->type;
        return;
    }
    if (e->kind == ND_SUB)
    {
        check_expr(sc, e->lhs);
        check_expr(sc, e->rhs);
        if (!type_equal(e->lhs->type, e->rhs->type))
        {
            diag_error_at(e->src, e->line, e->col,
                          "'-' requires both operands to have the same type");
            exit(1);
        }
        e->type = e->lhs->type;
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
    if (e->kind == ND_LAND)
    {
        check_expr(sc, e->lhs);
        check_expr(sc, e->rhs);
        if (!(type_is_int(e->lhs->type) && type_is_int(e->rhs->type)))
        {
            diag_error_at(e->src, e->line, e->col, "'&&' requires integer operands");
            exit(1);
        }
        e->type = &ty_i32;
        return;
    }
    if (e->kind == ND_NEW)
    {
        if (!e->new_base_type || e->new_dim_count <= 0)
        {
            diag_error_at(e->src, e->line, e->col,
                          "new expression missing type or dimension");
            exit(1);
        }
        if (e->new_dim_count > 1)
        {
            diag_error_at(e->src, e->line, e->col,
                          "multi-dimensional new is not supported yet");
            exit(1);
        }
        for (int i = 0; i < e->new_dim_count; i++)
        {
            Node *dim = e->new_dims ? e->new_dims[i] : NULL;
            if (!dim)
            {
                diag_error_at(e->src, e->line, e->col,
                              "missing dimension in new expression");
                exit(1);
            }
            check_expr(sc, dim);
            if (!type_is_int(dim->type))
            {
                diag_error_at(dim->src, dim->line, dim->col,
                              "new dimension must be an integer expression");
                exit(1);
            }
        }
        Type *res = make_ptr_chain(e->new_base_type, 1);
        e->type = res;
        sc->needs_malloc = 1;
        return;
    }
    if (e->kind == ND_DELETE)
    {
        if (!e->lhs)
        {
            diag_error_at(e->src, e->line, e->col,
                          "delete expression missing operand");
            exit(1);
        }
        check_expr(sc, e->lhs);
        if (!e->lhs->type || e->lhs->type->kind != TY_PTR)
        {
            diag_error_at(e->lhs->src, e->lhs->line, e->lhs->col,
                          "delete operand must be a pointer");
            exit(1);
        }
        e->type = &ty_void;
        sc->needs_free = 1;
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
        // Result type is element type of the pointer
        e->type = e->lhs->type->pointee;
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
        if (!t || !type_is_int(t))
        {
            diag_error_at(e->src, e->line, e->col, "++/-- requires integer variable");
            exit(1);
        }
        e->type = t;
        return;
    }
    if (e->kind == ND_CALL)
    {
        // lookup symbol
        const Symbol *sym = symtab_get(sc->syms, e->call_name);
        if (!sym)
        {
            diag_error_at(e->src, e->line, e->col, "unknown function '%s'",
                          e->call_name);
            exit(1);
        }
        // check args types minimally
        for (int i = 0; i < e->arg_count; i++)
        {
            check_expr(sc, e->args[i]);
        }
        e->type = sym->sig.ret;
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

static int sema_check_function(SemaContext *sc, Node *fn)
{
    if (!fn->ret_type)
        fn->ret_type = &ty_i32;
    Node *body = fn->body;
    if (!body)
    {
        diag_error_at(fn->src, fn->line, fn->col, "missing function body");
        return 1;
    }
    // bind parameters in a new scope
    scope_push(sc);
    for (int i = 0; i < fn->param_count; i++)
        scope_add(sc, fn->param_names[i], fn->param_types[i], 0);
    Node *ret = NULL;
    if (body->kind == ND_RET)
    {
        ret = body;
    }
    else if (body->kind == ND_BLOCK)
    {
        for (int i = 0; i < body->stmt_count; i++)
        {
            Node *s = body->stmts[i];
            if (s->kind == ND_RET)
            {
                ret = s;
                break;
            }
            if (s->kind == ND_VAR_DECL)
            {
                if (scope_find(sc, s->var_name))
                {
                    diag_error_at(s->src, s->line, s->col, "redeclaration of '%s'",
                                  s->var_name);
                    scope_pop(sc);
                    return 1;
                }
                scope_add(sc, s->var_name, s->var_type, s->var_is_const);
                if (s->rhs)
                {
                    if (s->rhs->kind == ND_INIT_LIST)
                    {
                        check_initializer_for_type(sc, s->rhs, s->var_type);
                    }
                    else
                    {
                        check_expr(sc, s->rhs);
                        if (s->var_type && !can_assign(s->var_type, s->rhs))
                        {
                            diag_error_at(s->rhs->src, s->rhs->line, s->rhs->col,
                                          "cannot initialize '%s' with incompatible type",
                                          s->var_name);
                            scope_pop(sc);
                            return 1;
                        }
                        if (s->var_type)
                            s->rhs->type = s->var_type;
                    }
                }
            }
            else if (s->kind == ND_EXPR_STMT)
            {
                if (s->lhs)
                    check_expr(sc, s->lhs);
            }
            else if (s->kind == ND_IF)
            { /* minimal: type-check cond */
                check_expr(sc, s->lhs);
            }
            else if (s->kind == ND_WHILE)
            {
                check_expr(sc, s->lhs);
            }
        }
        // For void functions, no explicit return is required.
        if (!ret && (!fn->ret_type || fn->ret_type->kind != TY_VOID))
        {
            diag_error_at(fn->src, fn->line, fn->col,
                          "function body must contain a return statement");
            scope_pop(sc);
            return 1;
        }
    }
    else
    {
        // support while-loops in blocks or as top-level stmt
        if (body->kind == ND_WHILE)
        {
            check_expr(sc, body->lhs);
            if (!type_is_int(body->lhs->type))
            {
                diag_error_at(body->lhs->src, body->lhs->line, body->lhs->col,
                              "while condition must be integer");
                return 1;
            }
            // recursively allow body
            if (body->rhs && body->rhs->kind == ND_BLOCK)
            {
                for (int i = 0; i < body->rhs->stmt_count;
                     i++)
                { /* minimal: ensure expressions type-check */
                    Node *s = body->rhs->stmts[i];
                    if (s->kind == ND_EXPR_STMT)
                        check_expr(sc, s->lhs);
                }
            }
            diag_error_at(fn->src, fn->line, fn->col,
                          "function body must contain a return statement");
            scope_pop(sc);
            return 1;
        }
        diag_error_at(fn->src, fn->line, fn->col, "unsupported function body kind");
        return 1;
    }
    if (ret)
    {
        if (fn->ret_type && fn->ret_type->kind == TY_VOID)
        {
            // In void functions, allow bare 'ret;' but forbid returning a value
            if (ret->lhs)
            {
                diag_error_at(ret->src, ret->line, ret->col,
                              "cannot return a value from a void function");
                return 1;
            }
        }
        else
        {
            if (!ret->lhs)
            {
                diag_error_at(ret->src, ret->line, ret->col,
                              "non-void function must return a value");
                return 1;
            }
            Node *e = ret->lhs;
            check_expr(sc, e);
            ret->type = e->type;
            // Enforce that the returned expression type matches the function's
            // declared return type (including pointers). If a cast was used,
            // 'e->type' should already be the cast target.
            Type *decl = fn->ret_type ? fn->ret_type : &ty_i32;
            if (!type_equal(ret->type, decl))
            {
                diag_error_at(ret->src, ret->line, ret->col,
                              "return type mismatch: returning %d but function returns %d",
                              ret->type ? ret->type->kind : -1, decl ? decl->kind : -1);
                return 1;
            }
        }
    }
    scope_pop(sc);
    return 0;
}

int sema_check_unit(SemaContext *sc, Node *unit)
{
    if (!unit)
    {
        diag_error("null unit");
        return 1;
    }
    if (unit->kind == ND_FUNC)
    {
        // single function case
        // add symbol so calls can resolve
        Symbol s = {0};
        s.kind = SYM_FUNC;
        s.name = unit->name;
        s.is_extern = 0;
        s.abi = "C";
        s.sig.ret = unit->ret_type ? unit->ret_type : &ty_i32;
        s.sig.params = NULL;
        s.sig.param_count = 0;
        s.sig.is_varargs = 0;
        symtab_add(sc->syms, s);
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
        Symbol s = {0};
        s.kind = SYM_FUNC;
        s.name = fn->name;
        s.is_extern = 0;
        s.abi = "C";
        s.sig.ret = fn->ret_type ? fn->ret_type : &ty_i32;
        s.sig.params = NULL;
        s.sig.param_count = 0;
        s.sig.is_varargs = 0;
        symtab_add(sc->syms, s);
    }
    // Second pass: type-check bodies
    for (int i = 0; i < unit->stmt_count; i++)
    {
        if (sema_check_function(sc, unit->stmts[i]))
            return 1;
    }
    if (sc->needs_malloc && !symtab_get(sc->syms, "malloc"))
    {
        Symbol s = {0};
        s.kind = SYM_FUNC;
        s.name = "malloc";
        s.is_extern = 1;
        s.abi = "C";
        s.sig.ret = &ty_void_ptr;
        s.sig.param_count = 1;
        s.sig.is_varargs = 0;
        s.sig.params = (Type **)xcalloc(1, sizeof(Type *));
        s.sig.params[0] = &ty_u64;
        symtab_add(sc->syms, s);
    }
    if (sc->needs_free && !symtab_get(sc->syms, "free"))
    {
        Symbol s = {0};
        s.kind = SYM_FUNC;
        s.name = "free";
        s.is_extern = 1;
        s.abi = "C";
        s.sig.ret = &ty_void;
        s.sig.param_count = 1;
        s.sig.is_varargs = 0;
        s.sig.params = (Type **)xcalloc(1, sizeof(Type *));
        s.sig.params[0] = &ty_void_ptr;
        symtab_add(sc->syms, s);
    }
    return 0;
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
