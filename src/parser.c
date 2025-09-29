#include "ast.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Parser
{
    Lexer *lx;
    // collected externs
    Symbol *externs;
    int ext_count;
    int ext_cap;
    // type aliases (simple + single-parameter generic)
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
    } *aliases;
    int alias_count;
    int alias_cap;
    // named types: structs and typedef-like entries
    struct NamedType { char *name; int name_len; Type *type; } *named_types;
    int nt_count; int nt_cap;
    // enum constants: name->value
    struct EnumConst { char *name; int name_len; int value; } *enum_consts;
    int ec_count; int ec_cap;
    struct EnumType { char *name; int name_len; } *enum_types;
    int et_count; int et_cap;
};

static Node *new_node(NodeKind k)
{
    Node *n = (Node *)xcalloc(1, sizeof(Node));
    n->kind = k;
    n->line = 0;
    n->col = 0;
    n->src = NULL;
    return n;
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

// forward decl
static Node *parse_expr(Parser *ps);
static Node *parse_stmt(Parser *ps);
static Node *parse_block(Parser *ps);
static Node *parse_while(Parser *ps);
static Node *parse_for(Parser *ps);
static void parse_alias_decl(Parser *ps);
static Node *parse_unary(Parser *ps);
static Node *parse_rel(Parser *ps);
static Node *parse_and(Parser *ps);
static Node *parse_eq(Parser *ps);

static Node *parse_initializer(Parser *ps);

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
static void named_type_add(Parser *ps, const char *name, int len, Type *ty)
{
    if (ps->nt_count == ps->nt_cap)
    {
        ps->nt_cap = ps->nt_cap ? ps->nt_cap * 2 : 8;
        ps->named_types = (struct NamedType*)realloc(ps->named_types, ps->nt_cap * sizeof(*ps->named_types));
    }
    ps->named_types[ps->nt_count].name = (char*)xmalloc((size_t)len + 1);
    memcpy(ps->named_types[ps->nt_count].name, name, (size_t)len);
    ps->named_types[ps->nt_count].name[len] = '\0';
    ps->named_types[ps->nt_count].name_len = len;
    ps->named_types[ps->nt_count].type = ty;
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
    if (i >= 0) { *out = ps->enum_consts[i].value; return 1; }
    return 0;
}
static void enum_const_add(Parser *ps, const char *name, int len, int value)
{
    if (ps->ec_count == ps->ec_cap)
    {
        ps->ec_cap = ps->ec_cap ? ps->ec_cap * 2 : 8;
        ps->enum_consts = (struct EnumConst*)realloc(ps->enum_consts, ps->ec_cap * sizeof(*ps->enum_consts));
    }
    ps->enum_consts[ps->ec_count].name = (char*)xmalloc((size_t)len + 1);
    memcpy(ps->enum_consts[ps->ec_count].name, name, (size_t)len);
    ps->enum_consts[ps->ec_count].name[len] = '\0';
    ps->enum_consts[ps->ec_count].name_len = len;
    ps->enum_consts[ps->ec_count].value = value;
    ps->ec_count++;
}
static int enum_type_find(Parser *ps, const char *name, int len)
{
    for (int i = 0; i < ps->et_count; i++)
        if (ps->enum_types[i].name_len == len && strncmp(ps->enum_types[i].name, name, len) == 0)
            return i;
    return -1;
}
static void enum_type_add(Parser *ps, const char *name, int len)
{
    if (enum_type_find(ps, name, len) >= 0)
        return;
    if (ps->et_count == ps->et_cap)
    {
        ps->et_cap = ps->et_cap ? ps->et_cap * 2 : 4;
        ps->enum_types = (struct EnumType *)realloc(ps->enum_types, ps->et_cap * sizeof(*ps->enum_types));
    }
    ps->enum_types[ps->et_count].name = (char *)xmalloc((size_t)len + 1);
    memcpy(ps->enum_types[ps->et_count].name, name, (size_t)len);
    ps->enum_types[ps->et_count].name[len] = '\0';
    ps->enum_types[ps->et_count].name_len = len;
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
    default:
        return 8;
    }
}

static int is_type_start(Parser *ps, Token t)
{
    switch (t.kind)
    {
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
    case TK_KW_STACK:
        return 1;
    case TK_KW_STRUCT:
        return 1;
    default:
        break;
    }
    if (t.kind == TK_IDENT && (alias_find(ps, t.lexeme, t.length) >= 0 || named_type_find(ps, t.lexeme, t.length) >= 0))
        return 1;
    return 0;
}

static Type *parse_type_spec(Parser *ps)
{
    // optional 'stack' storage class (ignored for now)
    Token t = lexer_peek(ps->lx);
    if (t.kind == TK_KW_STACK)
    {
        lexer_next(ps->lx);
        t = lexer_peek(ps->lx);
    }
    Type *base = NULL;
    Token b = lexer_next(ps->lx);
    static Type ti8 = {.kind = TY_I8}, tu8 = {.kind = TY_U8},
                ti16 = {.kind = TY_I16}, tu16 = {.kind = TY_U16},
                ti32 = {.kind = TY_I32}, tu32 = {.kind = TY_U32},
                ti64 = {.kind = TY_I64}, tu64 = {.kind = TY_U64};
    static Type tf32 = {.kind = TY_F32}, tf64 = {.kind = TY_F64},
                tf128 = {.kind = TY_F128};
    static Type tv = {.kind = TY_VOID}, tch = {.kind = TY_CHAR};
    if (b.kind == TK_KW_I8)
        base = &ti8;
    else if (b.kind == TK_KW_U8)
        base = &tu8;
    else if (b.kind == TK_KW_I16)
        base = &ti16;
    else if (b.kind == TK_KW_U16)
        base = &tu16;
    else if (b.kind == TK_KW_I32 || b.kind == TK_KW_INT)
        base = &ti32;
    else if (b.kind == TK_KW_U32 || b.kind == TK_KW_UINT)
        base = &tu32;
    else if (b.kind == TK_KW_I64 || b.kind == TK_KW_LONG)
        base = &ti64;
    else if (b.kind == TK_KW_U64 || b.kind == TK_KW_ULONG)
        base = &tu64;
    else if (b.kind == TK_KW_F32 || b.kind == TK_KW_FLOAT)
        base = &tf32;
    else if (b.kind == TK_KW_DOUBLE)
    {
        // could be 'double' or part of 'long double' handled when previous token
        // was LONG; since we consumed, treat as f64 here
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
    else if (b.kind == TK_KW_LONG)
    {
        // check for 'long double'
        Token p = lexer_peek(ps->lx);
        if (p.kind == TK_KW_DOUBLE)
        {
            lexer_next(ps->lx);
            base = &tf128;
        }
        else
            base = &ti64;
    }
    else if (b.kind == TK_IDENT)
    {
        int ai = alias_find(ps, b.lexeme, b.length);
        if (ai < 0)
        {
            // maybe a named struct type
            Type *nt = named_type_get(ps, b.lexeme, b.length);
            if (nt)
                base = nt;
            else {
                diag_error_at(lexer_source(ps->lx), b.line, b.col, "unknown type '%.*s'",
                              b.length, b.lexeme);
                exit(1);
            }
        }
        if (ai >= 0) {
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
    else if (b.kind == TK_KW_STRUCT)
    {
        // allow 'struct Name' as a type use
        Token nm = expect(ps, TK_IDENT, "struct name");
        Type *nt = named_type_get(ps, nm.lexeme, nm.length);
        if (!nt)
        {
            diag_error_at(lexer_source(ps->lx), nm.line, nm.col, "unknown struct '%.*s'", nm.length, nm.lexeme);
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
    // pointer suffixes '*': allocate a fresh pointer chain per type
    Token p = lexer_peek(ps->lx);
    int depth = 0;
    while (p.kind == TK_STAR)
    {
        lexer_next(ps->lx);
        depth++;
        p = lexer_peek(ps->lx);
    }
    if (depth == 0)
        return base;
    return make_ptr_chain_dyn(base, depth);
}

static Node *parse_primary(Parser *ps)
{
    Token t = lexer_next(ps->lx);
    if (t.kind == TK_INT)
    {
        Node *n = new_node(ND_INT);
        n->int_val = t.int_val;
        n->line = t.line;
        n->col = t.col;
        n->src = lexer_source(ps->lx);
        return n;
    }
    if (t.kind == TK_STRING)
    {
        Node *n = new_node(ND_STRING);
        // strip quotes; t.lexeme points at starting quote
        n->str_data = t.lexeme + 1;
        n->str_len = t.length - 2; // naive; ignores escapes
        n->line = t.line;
        n->col = t.col;
        n->src = lexer_source(ps->lx);
        return n;
    }
    if (t.kind == TK_IDENT)
    {
        // enum constant?
        int ev = 0;
        if (enum_const_get(ps, t.lexeme, t.length, &ev))
        {
            Node *n = new_node(ND_INT);
            n->int_val = ev;
            n->line = t.line; n->col = t.col; n->src = lexer_source(ps->lx);
            return n;
        }
        // Could be a call: ident '(' ... ')'
        Token p = lexer_peek(ps->lx);
        if (p.kind == TK_LPAREN)
        {
            lexer_next(ps->lx); // consume '('
            // args: expr[, expr]*
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
            // duplicate identifier lexeme into a null-terminated string
            char *nm = (char *)xmalloc((size_t)t.length + 1);
            memcpy(nm, t.lexeme, (size_t)t.length);
            nm[t.length] = '\0';
            call->call_name = nm;
            call->args = args;
            call->arg_count = argc;
            call->line = t.line;
            call->col = t.col;
            call->src = lexer_source(ps->lx);
            return call;
        }
        // variable reference
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

// Forward decls for new top-level decls
static void parse_enum_decl(Parser *ps);
static void parse_struct_decl(Parser *ps);
static Node *parse_postfix(Parser *ps)
{
    Node *e = parse_primary(ps);
    for (;;)
    {
        Token p = lexer_peek(ps->lx);
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
            Type *ty = parse_type_spec(ps);
            Node *cs = new_node(ND_CAST);
            cs->lhs = e;
            cs->type = ty;
            cs->line = p.line;
            cs->col = p.col;
            cs->src = lexer_source(ps->lx);
            e = cs;
            continue;
        }
        if (p.kind == TK_ACCESS || p.kind == TK_DOT)
        {
            Token op = lexer_next(ps->lx);
            Token field = expect(ps, TK_IDENT, "member name");
            // Check for enum scoped constant: EnumType=>Value
            if (e->kind == ND_VAR)
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
            m->is_pointer_deref = (op.kind == TK_ACCESS);
            m->line = field.line;
            m->col = field.col;
            m->src = lexer_source(ps->lx);
            e = m;
            continue;
        }
        break;
    }
    return e;
}

static Node *parse_unary(Parser *ps)
{
    Token p = lexer_peek(ps->lx);
    if (p.kind == TK_PLUS)
    {
        // unary plus: no-op
        lexer_next(ps->lx);
        return parse_unary(ps);
    }
    if (p.kind == TK_MINUS)
    {
        // unary minus: lower to 0 - expr
        lexer_next(ps->lx);
        Node *rv = parse_unary(ps);
        Node *zero = new_node(ND_INT);
        zero->int_val = 0;
        zero->src = lexer_source(ps->lx);
        Node *n = new_node(ND_SUB);
        n->lhs = zero;
        n->rhs = rv;
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

static Node *parse_rel(Parser *ps)
{
    Node *lhs = parse_add(ps);
    for(;;){
        Token p = lexer_peek(ps->lx);
        if (p.kind == TK_GT || p.kind == TK_LT || p.kind == TK_LTE || p.kind == TK_GTE)
        {
            Token op = lexer_next(ps->lx);
            Node *rhs = parse_add(ps);
            Node *n = NULL;
            if(op.kind==TK_GT) n = new_node(ND_GT_EXPR);
            else if(op.kind==TK_LT) n = new_node(ND_LT);
            else if(op.kind==TK_LTE) n = new_node(ND_LE);
            else n = new_node(ND_GE);
            n->lhs = lhs; n->rhs = rhs; n->line=op.line; n->col=op.col; n->src=lexer_source(ps->lx);
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

static Node *parse_and(Parser *ps)
{
    Node *lhs = parse_eq(ps);
    for (;;)
    {
        Token p = lexer_peek(ps->lx);
        if (p.kind == TK_ANDAND)
        {
            Token op = lexer_next(ps->lx);
            Node *rhs = parse_rel(ps);
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

// conditional (ternary) has lower precedence than && and ==, higher than assignment
static Node *parse_cond(Parser *ps)
{
    Node *cond = parse_and(ps);
    Token q = lexer_peek(ps->lx);
    if (q.kind != TK_QUESTION)
        return cond;
    lexer_next(ps->lx); // consume '?'
    Node *then_e = parse_expr(ps);
    expect(ps, TK_COLON, ":");
    Node *else_e = parse_cond(ps); // right-associative
    Node *n = new_node(ND_COND);
    n->lhs = cond;   // condition
    n->rhs = then_e; // then
    n->body = else_e; // reuse body for else branch
    n->line = q.line;
    n->col = q.col;
    n->src = lexer_source(ps->lx);
    return n;
}

static Node *parse_assign(Parser *ps)
{
    Node *lhs = parse_cond(ps);
    Token p = lexer_peek(ps->lx);
    if (p.kind == TK_ASSIGN)
    {
        Token op = lexer_next(ps->lx);
        Node *rhs = parse_expr(ps);
        Node *as = new_node(ND_ASSIGN);
        as->lhs = lhs;
        as->rhs = rhs;
        as->line = op.line;
        as->col = op.col;
        as->src = lexer_source(ps->lx);
        return as;
    }
    return lhs;
}



static Node *parse_expr(Parser *ps) { return parse_assign(ps); }

static Node *parse_initializer(Parser *ps)
{
    Token t = lexer_peek(ps->lx);
    if (t.kind != TK_LBRACE)
        return parse_expr(ps);
    lexer_next(ps->lx); // consume '{'
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

// (removed duplicate parse_unary definition)

static Node *parse_stmt(Parser *ps)
{
    Token t = lexer_peek(ps->lx);
    if (t.kind == TK_LBRACE)
        return parse_block(ps);
    if (t.kind == TK_KW_IF)
    {
        lexer_next(ps->lx);
        expect(ps, TK_LPAREN, "(");
        Node *cond = parse_expr(ps);
        expect(ps, TK_RPAREN, ")");
        Node *thenb = parse_stmt(ps); // block or single stmt
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
    if (t.kind == TK_KW_FOR)
    {
        return parse_for(ps);
    }
    if (t.kind == TK_KW_RET)
    {
        lexer_next(ps->lx);
        // allow optional expression in return; if next token is ';', it's a bare
        // return
        Token nxt = lexer_peek(ps->lx);
        Node *expr = NULL;
        if (nxt.kind != TK_SEMI)
        {
            expr = parse_expr(ps);
            expect(ps, TK_SEMI, ";");
        }
        else
        {
            // consume ';'
            lexer_next(ps->lx);
        }
        Node *r = new_node(ND_RET);
        r->lhs = expr;
        r->line = t.line;
        r->col = t.col;
        r->src = lexer_source(ps->lx);
        return r;
    }
    if (t.kind == TK_KW_CONSTANT || is_type_start(ps, t))
    {
        int is_const = 0;
        if (t.kind == TK_KW_CONSTANT)
        {
            lexer_next(ps->lx);
            is_const = 1;
            t = lexer_peek(ps->lx);
        }
        // var decl: [stack] type ident [= expr] ;
        Type *ty = parse_type_spec(ps);
        Token name = expect(ps, TK_IDENT, "identifier");
        Node *decl = new_node(ND_VAR_DECL);
        char *nm = (char *)xmalloc((size_t)name.length + 1);
        memcpy(nm, name.lexeme, (size_t)name.length);
        nm[name.length] = '\0';
        decl->var_name = nm;
        decl->var_type = ty;
        decl->var_is_const = is_const;
        decl->line = name.line;
        decl->col = name.col;
        decl->src = lexer_source(ps->lx);
        Token p2 = lexer_peek(ps->lx);
        if (p2.kind == TK_ASSIGN)
        {
            lexer_next(ps->lx);
            decl->rhs = parse_initializer(ps);
        }
        expect(ps, TK_SEMI, ";");
        return decl;
    }
    // expression statement
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
    Node **stmts = NULL;
    int cnt = 0, cap = 0;
    for (;;)
    {
        Token t = lexer_peek(ps->lx);
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
    return b;
}

static Node *parse_while(Parser *ps)
{
    // while (expr) stmt
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
    // for (init; cond; post) stmt
    expect(ps, TK_KW_FOR, "for");
    expect(ps, TK_LPAREN, "(");
    // init: either empty ';' or a full statement (decl or expr;)
    Token next = lexer_peek(ps->lx);
    Node *init = NULL;
    if (next.kind != TK_SEMI)
    {
        // Reuse parse_stmt for a single statement ending with ';'
        // But we must allow only a simple statement here; parse a declaration or expression-statement
        if (next.kind == TK_KW_CONSTANT || is_type_start(ps, next))
        {
            // var decl statement
            int is_const = 0;
            if (next.kind == TK_KW_CONSTANT)
            {
                lexer_next(ps->lx);
                is_const = 1;
            }
            Type *ty = parse_type_spec(ps);
            Token name = expect(ps, TK_IDENT, "identifier");
            Node *decl = new_node(ND_VAR_DECL);
            char *nm = (char *)xmalloc((size_t)name.length + 1);
            memcpy(nm, name.lexeme, (size_t)name.length);
            nm[name.length] = '\0';
            decl->var_name = nm;
            decl->var_type = ty;
            decl->var_is_const = is_const;
            decl->line = name.line;
            decl->col = name.col;
            decl->src = lexer_source(ps->lx);
            Token p2 = lexer_peek(ps->lx);
            if (p2.kind == TK_ASSIGN)
            {
                lexer_next(ps->lx);
                decl->rhs = parse_initializer(ps);
            }
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
        // consume ';' for empty init
        lexer_next(ps->lx);
    }
    // condition (optional)
    Node *cond = NULL;
    next = lexer_peek(ps->lx);
    if (next.kind != TK_SEMI)
    {
        cond = parse_expr(ps);
    }
    expect(ps, TK_SEMI, ";");
    // post (optional)
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
    // Build while loop: while (cond_or_true) { body; post; }
    Node *while_body = NULL;
    if (post)
    {
        Node **stmts = (Node **)xcalloc(2, sizeof(Node *));
        stmts[0] = body;
        stmts[1] = post;
        Node *blk = new_node(ND_BLOCK);
        blk->stmts = stmts;
        blk->stmt_count = 2;
        blk->src = lexer_source(ps->lx);
        while_body = blk;
    }
    else
    {
        while_body = body;
    }
    // if no condition, use integer literal 1
    if (!cond)
    {
        Node *one = new_node(ND_INT);
        one->int_val = 1;
        one->src = lexer_source(ps->lx);
        cond = one;
    }
    Node *wh = new_node(ND_WHILE);
    wh->lhs = cond;
    wh->rhs = while_body;
    wh->src = lexer_source(ps->lx);
    // If there was an init, create an outer block { init; while(...) }
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


static Node *parse_function(Parser *ps)
{
    expect(ps, TK_KW_FUN, "fun");
    Token name = expect(ps, TK_IDENT, "identifier");
    expect(ps, TK_LPAREN, "(");
    // parameters: [type ident] *(, type ident)
    Type **param_types = NULL;
    const char **param_names = NULL;
    int param_count = 0, param_cap = 0;
    Token peek = lexer_peek(ps->lx);
    if (peek.kind != TK_RPAREN)
    {
        for (;;)
        {
            Type *pty = parse_type_spec(ps);
            Token pn = expect(ps, TK_IDENT, "parameter name");
            if (param_count == param_cap)
            {
                param_cap = param_cap ? param_cap * 2 : 4;
                param_types = (Type **)realloc(param_types, sizeof(Type *) * param_cap);
                param_names =
                    (const char **)realloc(param_names, sizeof(char *) * param_cap);
            }
            param_types[param_count] = pty;
            char *nm = (char *)xmalloc((size_t)pn.length + 1);
            memcpy(nm, pn.lexeme, (size_t)pn.length);
            nm[pn.length] = '\0';
            param_names[param_count] = nm;
            param_count++;
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
    expect(ps, TK_ARROW, "->");
    // return type
    Type *rtype = parse_type_spec(ps);
    Node *body = parse_block(ps);
    Node *fn = new_node(ND_FUNC);
    // duplicate function name to a stable C string
    char *nm = (char *)xmalloc((size_t)name.length + 1);
    memcpy(nm, name.lexeme, (size_t)name.length);
    nm[name.length] = '\0';
    fn->name = nm;
    fn->body = body;
    fn->ret_type = rtype;
    fn->param_types = param_types;
    fn->param_names = param_names;
    fn->param_count = param_count;
    fn->line = name.line;
    fn->col = name.col;
    fn->src = lexer_source(ps->lx);
    return fn;
}

static void parse_extend_decl(Parser *ps)
{
    // Two forms:
    // 1) extend from "C" i32 printf(char*, _vaargs_);
    // 2) extend fun name(params) -> ret;   // default ABI "C"
    expect(ps, TK_KW_EXTEND, "extend");
    Token next = lexer_peek(ps->lx);
    if (next.kind == TK_KW_FUN)
    {
        // Parse: fun name(params) -> ret;
        lexer_next(ps->lx); // consume 'fun'
        Token name = expect(ps, TK_IDENT, "identifier");
        expect(ps, TK_LPAREN, "(");
        // Skip parameter list; we don't need full types for extern symbol table
        for (;;)
        {
            Token t = lexer_next(ps->lx);
            if (t.kind == TK_RPAREN)
                break;
            if (t.kind == TK_EOF)
            {
                diag_error_at(lexer_source(ps->lx), 0, 0,
                              "unexpected end of file in extern parameter list");
                exit(1);
            }
        }
        expect(ps, TK_ARROW, "->");
        Type *ret_ty = parse_type_spec(ps);
        expect(ps, TK_SEMI, ";");
        // Record extern symbol with default ABI "C"
        Symbol s = (Symbol){0};
        s.kind = SYM_FUNC;
        char *nm = (char *)xmalloc((size_t)name.length + 1);
        memcpy(nm, name.lexeme, (size_t)name.length);
        nm[name.length] = '\0';
        s.name = nm;
        s.is_extern = 1;
        s.abi = xstrdup("C");
    // Default to i32 if return type omitted (should not happen here),
    // using a stable static object instead of a temporary.
    static Type ti32_ext = {.kind = TY_I32};
    s.sig.ret = ret_ty ? ret_ty : &ti32_ext;
        s.sig.params = NULL;
        s.sig.param_count = 0;
        s.sig.is_varargs = 0;
        if (ps->ext_count == ps->ext_cap)
        {
            ps->ext_cap = ps->ext_cap ? ps->ext_cap * 2 : 8;
            ps->externs = (Symbol *)realloc(ps->externs, ps->ext_cap * sizeof(Symbol));
        }
        ps->externs[ps->ext_count++] = s;
        return;
    }
    expect(ps, TK_KW_FROM, "from");
    Token abi = lexer_next(ps->lx);
    if (!(abi.kind == TK_STRING || abi.kind == TK_IDENT))
    {
        diag_error_at(lexer_source(ps->lx), abi.line, abi.col,
                      "expected ABI after 'from'");
        exit(1);
    }
    // return type (allow pointers)
    // Reuse type-spec parser without allowing leading 'stack'
    // Simple approach: call parse_type_spec and ignore its optional 'stack'
    Type *ret_ty = parse_type_spec(ps);
    Token name = expect(ps, TK_IDENT, "identifier");
    expect(ps, TK_LPAREN, "(");
    // Very minimal signature capture: detect varargs if present; ignore param
    // types for now
    int is_varargs = 0;
    for (;;)
    {
        Token t = lexer_next(ps->lx);
        if (t.kind == TK_RPAREN)
            break;
        if (t.kind == TK_EOF)
        {
            diag_error_at(lexer_source(ps->lx), 0, 0,
                          "unexpected end of file in extern parameter list");
            exit(1);
        }
        if (t.kind == TK_IDENT && t.length == 8 &&
            strncmp(t.lexeme, "_vaargs_", 8) == 0)
        {
            is_varargs = 1;
        }
    }
    expect(ps, TK_SEMI, ";");
    // Save extern
    Symbol s = {0};
    s.kind = SYM_FUNC;
    char *nm = (char *)xmalloc((size_t)name.length + 1);
    memcpy(nm, name.lexeme, (size_t)name.length);
    nm[name.length] = '\0';
    s.name = nm;
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
    s.sig.params = NULL;
    s.sig.param_count = 0;
    s.sig.is_varargs = is_varargs;
    if (ps->ext_count == ps->ext_cap)
    {
        ps->ext_cap = ps->ext_cap ? ps->ext_cap * 2 : 8;
        ps->externs = (Symbol *)realloc(ps->externs, ps->ext_cap * sizeof(Symbol));
    }
    ps->externs[ps->ext_count++] = s;
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
    lexer_destroy(ps->lx);
    free(ps);
}

Node *parse_unit(Parser *ps)
{
    // Allow externs and multiple functions; build ND_UNIT containing all
    // functions
    Node **fns = NULL;
    int fn_count = 0, fn_cap = 0;
    for (;;)
    {
        Token t = lexer_peek(ps->lx);
        if (t.kind == TK_KW_EXTEND)
        {
            parse_extend_decl(ps);
            continue;
        }
        if (t.kind == TK_KW_ENUM)
        {
            parse_enum_decl(ps);
            continue;
        }
        if (t.kind == TK_KW_STRUCT)
        {
            parse_struct_decl(ps);
            continue;
        }
        if (t.kind == TK_KW_ALIAS)
        {
            parse_alias_decl(ps);
            continue;
        }
        if (t.kind == TK_KW_FUN)
        {
            Node *fn = parse_function(ps);
            if (fn_count == fn_cap)
            {
                fn_cap = fn_cap ? fn_cap * 2 : 4;
                fns = (Node **)realloc(fns, sizeof(Node *) * fn_cap);
            }
            fns[fn_count++] = fn;
            continue;
        }
        if (t.kind == TK_EOF)
            break;
        diag_error_at(lexer_source(ps->lx), t.line, t.col,
                      "expected 'extend', 'fun' or EOF");
        exit(1);
    }
    if (fn_count == 0)
    {
        diag_error_at(lexer_source(ps->lx), 1, 1, "no functions found");
        exit(1);
    }
    Node *u = (Node *)xcalloc(1, sizeof(Node));
    u->kind = ND_UNIT;
    u->stmts = fns;
    u->stmt_count = fn_count;
    u->src = lexer_source(ps->lx);
    u->line = 1;
    u->col = 1;
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

static void parse_alias_decl(Parser *ps)
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
        // alias Name<T> = T*...;
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
        ps->alias_count++;
        return;
    }
    // non-generic: alias Name = <builtin> '*'* ;
    expect(ps, TK_ASSIGN, "=");
    Token b = lexer_next(ps->lx);
    if (!(b.kind == TK_KW_I8 || b.kind == TK_KW_U8 || b.kind == TK_KW_I16 ||
          b.kind == TK_KW_U16 || b.kind == TK_KW_I32 || b.kind == TK_KW_U32 ||
          b.kind == TK_KW_I64 || b.kind == TK_KW_U64 || b.kind == TK_KW_F32 ||
          b.kind == TK_KW_F64 || b.kind == TK_KW_F128 || b.kind == TK_KW_VOID ||
          b.kind == TK_KW_CHAR))
    {
        diag_error_at(lexer_source(ps->lx), b.line, b.col,
                      "alias base must be a built-in type for now");
        exit(1);
    }
    if (b.kind == TK_KW_I8)
        base_kind = TY_I8;
    else if (b.kind == TK_KW_U8)
        base_kind = TY_U8;
    else if (b.kind == TK_KW_I16)
        base_kind = TY_I16;
    else if (b.kind == TK_KW_U16)
        base_kind = TY_U16;
    else if (b.kind == TK_KW_I32)
        base_kind = TY_I32;
    else if (b.kind == TK_KW_U32)
        base_kind = TY_U32;
    else if (b.kind == TK_KW_I64)
        base_kind = TY_I64;
    else if (b.kind == TK_KW_U64)
        base_kind = TY_U64;
    else if (b.kind == TK_KW_F32)
        base_kind = TY_F32;
    else if (b.kind == TK_KW_F64)
        base_kind = TY_F64;
    else if (b.kind == TK_KW_F128)
        base_kind = TY_F128;
    else if (b.kind == TK_KW_VOID)
        base_kind = TY_VOID;
    else
        base_kind = TY_CHAR;
    int n = 0;
    Token s = lexer_peek(ps->lx);
    while (s.kind == TK_STAR)
    {
        lexer_next(ps->lx);
        n++;
        s = lexer_peek(ps->lx);
    }
    ptr_depth = n;
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
    ps->alias_count++;
}

static void parse_enum_decl(Parser *ps)
{
    expect(ps, TK_KW_ENUM, "enum");
    Token name = expect(ps, TK_IDENT, "enum name");
    // For now, enums are i32-typed constants; register named type alias as i32
    static Type ti32 = {.kind = TY_I32};
    if (named_type_find(ps, name.lexeme, name.length) < 0)
        named_type_add(ps, name.lexeme, name.length, &ti32);
    enum_type_add(ps, name.lexeme, name.length);
    expect(ps, TK_LBRACE, "{");
    int cur = 0;
    for (;;)
    {
        Token t = lexer_peek(ps->lx);
        if (t.kind == TK_RBRACE) { lexer_next(ps->lx); break; }
        Token id = expect(ps, TK_IDENT, "enumerator");
        Token p = lexer_peek(ps->lx);
        if (p.kind == TK_ASSIGN)
        {
            lexer_next(ps->lx);
            Node *e = parse_expr(ps);
            // evaluate simple integer constant; fallback 0 if not INT
            if (e->kind == ND_INT)
                cur = (int)e->int_val;
            else
                cur = 0;
        }
        // add constant (global scope)
    enum_const_add(ps, id.lexeme, id.length, cur);
    // Also register scoped name Enum=>Member
    int scoped_len = name.length + 2 + id.length;
    char *scoped = (char *)xmalloc((size_t)scoped_len + 1);
    memcpy(scoped, name.lexeme, (size_t)name.length);
    scoped[name.length] = '=';
    scoped[name.length + 1] = '>';
    memcpy(scoped + name.length + 2, id.lexeme, (size_t)id.length);
    scoped[scoped_len] = '\0';
    enum_const_add(ps, scoped, scoped_len, cur);
    free(scoped);
        // next
        Token sep = lexer_peek(ps->lx);
        if (sep.kind == TK_COMMA) { lexer_next(ps->lx); cur++; continue; }
        // maybe end
        // if next is '}', we'll loop and close; otherwise, error
        cur++;
    }
    expect(ps, TK_SEMI, ";");
}

static void parse_struct_decl(Parser *ps)
{
    expect(ps, TK_KW_STRUCT, "struct");
    Token name = expect(ps, TK_IDENT, "struct name");
    expect(ps, TK_LBRACE, "{");
    // Create a Type object for the struct
    Type *st = (Type*)xcalloc(1, sizeof(Type));
    st->kind = TY_STRUCT;
    st->struct_name = (char*)xmalloc((size_t)name.length + 1);
    memcpy((char*)st->struct_name, name.lexeme, (size_t)name.length);
    ((char*)st->struct_name)[name.length] = '\0';
    // Parse fields: list of type ident ';'
    const char **fnames = NULL; Type **ftypes = NULL; int *foff = NULL; int fcnt=0, fcap=0;
    int offset = 0;
    while (1)
    {
        Token t = lexer_peek(ps->lx);
        if (t.kind == TK_RBRACE) { lexer_next(ps->lx); break; }
        if (!(t.kind == TK_KW_CONSTANT || is_type_start(ps, t)))
        {
            diag_error_at(lexer_source(ps->lx), t.line, t.col, "expected field declaration or '}'");
            exit(1);
        }
        int is_const = 0; if (t.kind == TK_KW_CONSTANT) { lexer_next(ps->lx); is_const = 1; }
        (void)is_const; // fields ignore const for now
        Type *fty = parse_type_spec(ps);
        Token fname = expect(ps, TK_IDENT, "field name");
        // optional multiple declarators not supported; one per line
        expect(ps, TK_SEMI, ";");
        if (fcnt==fcap) { fcap = fcap?fcap*2:4; fnames=(const char**)realloc(fnames, sizeof(char*)*fcap); ftypes=(Type**)realloc(ftypes, sizeof(Type*)*fcap); foff=(int*)realloc(foff, sizeof(int)*fcap);} 
        char *nm = (char*)xmalloc((size_t)fname.length+1); memcpy(nm, fname.lexeme, (size_t)fname.length); nm[fname.length]='\0';
        fnames[fcnt] = nm; ftypes[fcnt] = fty; fcnt++;
        int sz = type_sizeof_simple(fty);
        if (fty && fty->kind == TY_STRUCT && sz == 0)
        {
            diag_error_at(lexer_source(ps->lx), fname.line, fname.col,
                          "struct field '%.*s' has incomplete type", fname.length,
                          fname.lexeme);
            exit(1);
        }
        foff[fcnt-1] = offset;
        offset += sz;
    }
    st->strct.field_names = fnames; st->strct.field_types = ftypes; st->strct.field_offsets = foff; st->strct.field_count = fcnt; st->strct.size_bytes = offset;
    // register named type
    named_type_add(ps, name.lexeme, name.length, st);
    expect(ps, TK_SEMI, ";");
}
