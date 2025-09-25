#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ast.h"

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

static Type ty_i32 = {.kind = TY_I32};
static Type ty_i64 = {.kind = TY_I64};
static Type ty_void = {.kind = TY_VOID};
static Type ty_char = {.kind = TY_CHAR};

static int type_is_int(Type *t) { return t && (t->kind == TY_I32 || t->kind == TY_I64); }

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
static void scope_add(SemaContext *sc, const char *name, Type *ty)
{
    if (!sc->scope)
        scope_push(sc);
    struct Scope *s = sc->scope;
    if (s->local_count < 128)
    {
        s->locals[s->local_count].name = name;
        s->locals[s->local_count].type = ty;
        s->local_count++;
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
            diag_error("unknown variable '%s'", e->var_ref);
            exit(1);
        }
        e->type = t;
        return;
    }
    if (e->kind == ND_ADD)
    {
        check_expr(sc, e->lhs);
        check_expr(sc, e->rhs);
        if (!(e->lhs->type->kind == TY_I32 && e->rhs->type->kind == TY_I32))
        {
            diag_error("'+' requires i32 + i32");
            exit(1);
        }
        e->type = &ty_i32;
        return;
    }
    if (e->kind == ND_SUB)
    {
        check_expr(sc, e->lhs);
        check_expr(sc, e->rhs);
        if (!(e->lhs->type->kind == TY_I32 && e->rhs->type->kind == TY_I32))
        {
            diag_error("'-' requires i32 - i32");
            exit(1);
        }
        e->type = &ty_i32;
        return;
    }
    if (e->kind == ND_GT_EXPR)
    {
        check_expr(sc, e->lhs);
        check_expr(sc, e->rhs);
        if (!(type_is_int(e->lhs->type) && type_is_int(e->rhs->type)))
        {
            diag_error("'>' requires integer operands");
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
    if (e->kind == ND_CALL)
    {
        // lookup symbol
        const Symbol *sym = symtab_get(sc->syms, e->call_name);
        if (!sym)
        {
            diag_error("unknown function '%s'", e->call_name);
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
    diag_error("unsupported expression kind %d", e->kind);
    exit(1);
}

static int sema_check_function(SemaContext *sc, Node *fn)
{
    if (!fn->ret_type)
        fn->ret_type = &ty_i32;
    Node *body = fn->body;
    if (!body)
    {
        diag_error("missing function body");
        return 1;
    }
    // bind parameters in a new scope
    scope_push(sc);
    for (int i = 0; i < fn->param_count; i++)
        scope_add(sc, fn->param_names[i], fn->param_types[i]);
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
                    diag_error("redeclaration of '%s'", s->var_name);
                    scope_pop(sc);
                    return 1;
                }
                scope_add(sc, s->var_name, s->var_type);
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
        if (!ret)
        {
            diag_error("function body must contain a return statement");
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
                diag_error("while condition must be integer");
                return 1;
            }
            // recursively allow body
            if (body->rhs && body->rhs->kind == ND_BLOCK)
            {
                for (int i = 0; i < body->rhs->stmt_count; i++)
                { /* minimal: ensure expressions type-check */
                    Node *s = body->rhs->stmts[i];
                    if (s->kind == ND_EXPR_STMT)
                        check_expr(sc, s->lhs);
                }
            }
            diag_error("function body must contain a return statement");
            scope_pop(sc);
            return 1;
        }
        diag_error("unsupported function body kind");
        return 1;
    }
    Node *e = ret->lhs;
    check_expr(sc, e);
    ret->type = e->type;
    if (fn->ret_type == &ty_void)
    {
        diag_error("cannot return a value from a void function");
        return 1;
    }
    if (!type_is_int(ret->type))
    {
        diag_error("return type must be integer");
        return 1;
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
