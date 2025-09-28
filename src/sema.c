#include "ast.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

static int type_is_int(Type *t)
{
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
    return 1;
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
        // For simplicity, treat as int in expressions (like char promotes to int)
        e->type = &ty_i32;
        return;
    }
    if (e->kind == ND_ASSIGN)
    {
        // Strict typing: lhs must be lvalue; rhs type must exactly match lhs type
        // unless explicit cast wraps the lhs or rhs
        if (!e->lhs)
        {
            diag_error_at(e->src, e->line, e->col,
                          "assignment missing left-hand side");
            exit(1);
        }
        if (e->lhs->kind != ND_VAR && e->lhs->kind != ND_INDEX &&
            !(e->lhs->kind == ND_CAST &&
              (e->lhs->lhs &&
               (e->lhs->lhs->kind == ND_VAR || e->lhs->lhs->kind == ND_INDEX))))
        {
            diag_error_at(e->src, e->line, e->col,
                          "lvalue required as left operand of assignment");
            exit(1);
        }
        check_expr(sc, e->rhs);
        // Type of assignment expression is type of LHS; approximate with rhs type
        // when lhs unknown
        if (e->lhs->kind == ND_VAR)
        {
            Type *lt = scope_get_type(sc, e->lhs->var_ref);
            if (!lt)
            {
                diag_error_at(e->lhs->src, e->lhs->line, e->lhs->col,
                              "unknown variable '%s' on left-hand side of assignment",
                              e->lhs->var_ref);
                exit(1);
            }
            if (scope_is_const(sc, e->lhs->var_ref))
            {
                diag_error_at(e->lhs->src, e->lhs->line, e->lhs->col,
                              "cannot assign to constant variable '%s'",
                              e->lhs->var_ref);
                exit(1);
            }
            e->type = lt;
            if (!type_equal(lt, e->rhs->type) && !(e->rhs->kind == ND_CAST))
            {
                diag_error_at(e->src, e->line, e->col,
                              "cannot assign '%d' to '%d' without cast",
                              e->rhs->type ? e->rhs->type->kind : -1, lt->kind);
                exit(1);
            }
        }
        else
        {
            // Indexed lvalue: require matching element type; approximated as i8 for
            // now
            e->type = e->rhs->type ? e->rhs->type : &ty_i32;
        }
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
                    check_expr(sc, s->rhs);
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
