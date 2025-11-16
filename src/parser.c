
#include "ast.h"
#include "module_registry.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>

static int parser_disable_formatting_notes = 0;

void parser_set_disable_formatting_notes(int disable)
{
    parser_disable_formatting_notes = disable ? 1 : 0;
}

struct ModuleImport
{
    char **parts;
    int part_count;
    char *full_name;
    char *alias;
};

struct Parser
{
    Lexer *lx;
    // collected externs
    Symbol *externs;
    int ext_count;
    int ext_cap;
    const char *current_function_name;
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
        int is_exposed;
    } *aliases;
    int alias_count;
    int alias_cap;
    // named types: structs and typedef-like entries
    struct NamedType
    {
        char *name;
        int name_len;
        Type *type;
        int is_exposed;
    } *named_types;
    int nt_count;
    int nt_cap;
    // enum constants: name->value
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
    // module metadata
    char **module_parts;
    int module_part_count;
    int module_part_cap;
    char *module_full_name;
    struct ModuleImport *imports;
    int import_count;
    int import_cap;
};

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
static Type *parse_type_spec(Parser *ps);
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
        }
        else if (strcmp(attr->name, "Inline") == 0)
        {
            fn->wants_inline = 1;
        }
        else
        {
            diag_error_at(lexer_source(ps->lx), attr->line, attr->col,
                          "unknown attribute '%s'", attr->name);
            exit(1);
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
            total += 1; // for '.'
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

static char *module_name_to_prefix(const char *full_name)
{
    if (!full_name || !*full_name)
        return NULL;
    size_t len = strlen(full_name);
    char *copy = (char *)xmalloc(len + 1);
    memcpy(copy, full_name, len + 1);
    for (size_t i = 0; i < len; ++i)
    {
        if (copy[i] == '.')
            copy[i] = '_';
    }
    return copy;
}

static char *make_module_backend_name(const char *module_full, const char *fn_name)
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

static void parse_module_decl(Parser *ps)
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

// forward decl
static Node *parse_expr(Parser *ps);
static Node *parse_stmt(Parser *ps);
static Node *parse_block(Parser *ps);
static Node *parse_while(Parser *ps);
static Node *parse_for(Parser *ps);
static void parse_alias_decl(Parser *ps, int is_exposed);
static Node *parse_unary(Parser *ps);
static Node *parse_rel(Parser *ps);
static Node *parse_and(Parser *ps);
static Node *parse_eq(Parser *ps);
static Node *parse_or(Parser *ps);
static Node *parse_shift(Parser *ps);

static Node *parse_initializer(Parser *ps);
static void parse_struct_decl(Parser *ps, int is_exposed);
static void parse_enum_decl(Parser *ps, int is_exposed);
static void parse_module_decl(Parser *ps);
static int parse_bring_decl(Parser *ps);
static int parse_extend_decl(Parser *ps, int leading_noreturn);

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
    case TK_KW_BOOL:
    case TK_KW_STACK:
        return 1;
    case TK_KW_FUN:
    {
        Token next = lexer_peek_n(ps->lx, 1);
        return next.kind == TK_STAR;
    }
    case TK_KW_STRUCT:
        return 1;
    default:
        break;
    }
    if (t.kind == TK_IDENT)
    {
        if (t.length == 7 && strncmp(t.lexeme, "va_list", 7) == 0)
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
    static Type tv = {.kind = TY_VOID}, tch = {.kind = TY_CHAR}, tbool = {.kind = TY_BOOL};
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
    else if (b.kind == TK_KW_BOOL)
        base = &tbool;
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
    else if (b.kind == TK_KW_FUN)
    {
        Token star = lexer_peek(ps->lx);
        if (star.kind != TK_STAR)
        {
            diag_error_at(lexer_source(ps->lx), star.line, star.col,
                          "expected '*' after 'fun' in function pointer type");
            exit(1);
        }
        lexer_next(ps->lx); // consume '*'
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
                int ai = alias_find(ps, b.lexeme, b.length);
                if (ai < 0)
                {
                    Type *nt = named_type_get(ps, b.lexeme, b.length);
                    if (nt)
                        base = nt;
                    else
                    {
                        diag_error_at(lexer_source(ps->lx), b.line, b.col, "unknown type '%.*s'",
                                      b.length, b.lexeme);
                        exit(1);
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
    // array suffixes '[...]'
    Token p = lexer_peek(ps->lx);
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
            if (len_tok.kind != TK_INT)
            {
                diag_error_at(lexer_source(ps->lx), len_tok.line, len_tok.col,
                              "array length must be an integer literal");
                exit(1);
            }
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
        expect(ps, TK_RBRACKET, "]");
        base = type_array(base, length);
        p = lexer_peek(ps->lx);
    }
    // pointer suffixes '*': allocate a fresh pointer chain per type
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
        lexer_next(ps->lx); // consume else
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
        // sizeof never evaluates its operand; sema computes the constant size
        n->lhs = operand_expr;
        n->rhs = NULL;
        n->line = t.line;
        n->col = t.col;
        n->src = lexer_source(ps->lx);
        n->var_type = operand_type;
        return n;
    }
    if (t.kind == TK_KW_TYPEOF)
    {
        expect(ps, TK_LPAREN, "(");
        // typeof can take a type name or an expression/identifier
        Token p = lexer_peek(ps->lx);
        Node *arg_node = NULL;
        Type *arg_type = NULL;
        char *alias_name = NULL;
        if (is_type_start(ps, p))
        {
            if (p.kind == TK_IDENT)
            {
                // check if it's an alias so we can preserve the alias name for formatting
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
        n->var_type = arg_type; // carry explicit type if provided
        if (alias_name)
            n->var_ref = alias_name; // stash alias text for sema formatting
        n->line = t.line;
        n->col = t.col;
        n->src = lexer_source(ps->lx);
        return n;
    }
    if (t.kind == TK_INT)
    {
        Node *n = new_node(ND_INT);
        n->int_val = t.int_val;
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
        return parse_metadata_call(ps, t);
    }
    if (t.kind == TK_IDENT)
    {
        if (t.length == 4 && strncmp(t.lexeme, "true", 4) == 0)
        {
            Node *n = new_node(ND_INT);
            n->int_val = 1;
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
        // enum constant?
        int ev = 0;
        if (enum_const_get(ps, t.lexeme, t.length, &ev))
        {
            Node *n = new_node(ND_INT);
            n->int_val = ev;
            n->line = t.line;
            n->col = t.col;
            n->src = lexer_source(ps->lx);
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
static void parse_enum_decl(Parser *ps, int is_exposed);
static void parse_struct_decl(Parser *ps, int is_exposed);
static Node *parse_postfix_suffixes(Parser *ps, Node *expr)
{
    Node *e = expr;
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
        if (p.kind == TK_LPAREN)
        {
            lexer_next(ps->lx); // consume '('
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
            call->line = e ? e->line : p.line;
            call->col = e ? e->col : p.col;
            call->src = lexer_source(ps->lx);
            e = call;
            continue;
        }
        break;
    }
    return e;
}

static Node *parse_postfix(Parser *ps)
{
    Node *e = parse_primary(ps);
    return parse_postfix_suffixes(ps, e);
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

// conditional (ternary) has lower precedence than && and ==, higher than assignment
static Node *parse_cond(Parser *ps)
{
    Node *cond = parse_or(ps);
    Token q = lexer_peek(ps->lx);
    if (q.kind != TK_QUESTION)
        return cond;
    lexer_next(ps->lx); // consume '?'
    Node *then_e = parse_expr(ps);
    expect(ps, TK_COLON, ":");
    Node *else_e = parse_cond(ps); // right-associative
    Node *n = new_node(ND_COND);
    n->lhs = cond;    // condition
    n->rhs = then_e;  // then
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
        decl->var_is_array = (ty && ty->kind == TY_ARRAY);
    decl->var_is_function = (ty && ty->kind == TY_PTR && ty->pointee && ty->pointee->kind == TY_FUNC);
        decl->var_is_const = is_const;
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
            decl->var_is_array = (ty && ty->kind == TY_ARRAY);
            decl->var_is_function = (ty && ty->kind == TY_PTR && ty->pointee && ty->pointee->kind == TY_FUNC);
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
    wh->rhs = body;
    wh->body = post;
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

static Node *parse_function(Parser *ps, int is_noreturn, int is_exposed, FunctionBodyKind body_kind)
{
    expect(ps, TK_KW_FUN, "fun");
    Token name = expect(ps, TK_IDENT, "identifier");
    expect(ps, TK_LPAREN, "(");
    // parameters: [type ident] *(, type ident) [,...]
    Type **param_types = NULL;
    const char **param_names = NULL;
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
    }
    expect(ps, TK_RPAREN, ")");
    expect(ps, TK_ARROW, "->");
    // return type
    Type *rtype = parse_type_spec(ps);
    if (is_noreturn && rtype && rtype->kind != TY_VOID)
    {
        diag_error_at(lexer_source(ps->lx), name.line, name.col,
                      "noreturn functions must return void");
        exit(1);
    }
    Node *fn = new_node(ND_FUNC);
    // duplicate function name to a stable C string
    char *nm = (char *)xmalloc((size_t)name.length + 1);
    memcpy(nm, name.lexeme, (size_t)name.length);
    nm[name.length] = '\0';
    fn->name = nm;
    fn->ret_type = rtype;
    fn->param_types = param_types;
    fn->param_names = param_names;
    fn->param_count = param_count;
    fn->is_varargs = saw_varargs;
    fn->line = name.line;
    fn->col = name.col;
    fn->src = lexer_source(ps->lx);
    fn->is_noreturn = is_noreturn;
    fn->is_exposed = is_exposed;
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
    return fn;
}

static int parse_extend_decl(Parser *ps, int leading_noreturn)
{
    // Two forms:
    // 1) extend from "C" i32 printf(char*, _vaargs_);
    // 2) extend fun name(params) -> ret;   // default ABI "C"
    Token extend_tok = expect(ps, TK_KW_EXTEND, "extend");
    int is_noreturn = leading_noreturn;
    Token next = lexer_peek(ps->lx);
    if (!is_noreturn && next.kind == TK_KW_NORETURN)
    {
        lexer_next(ps->lx);
        is_noreturn = 1;
        next = lexer_peek(ps->lx);
    }
    if (next.kind == TK_KW_FUN)
    {
        // Parse: fun name(params) -> ret;
        lexer_next(ps->lx); // consume 'fun'
        Token name = expect(ps, TK_IDENT, "identifier");
        expect(ps, TK_LPAREN, "(");
        Type **param_types = NULL;
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

                Type *pty = parse_type_spec(ps);
                if (param_count == param_cap)
                {
                    param_cap = param_cap ? param_cap * 2 : 4;
                    param_types =
                        (Type **)realloc(param_types, sizeof(Type *) * param_cap);
                }
                param_types[param_count++] = pty;
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
        }
        expect(ps, TK_RPAREN, ")");
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
        s.backend_name = s.name;
        s.is_extern = 1;
        s.abi = xstrdup("C");
        // Default to i32 if return type omitted (should not happen here),
        // using a stable static object instead of a temporary.
        static Type ti32_ext = {.kind = TY_I32};
        s.sig.ret = ret_ty ? ret_ty : &ti32_ext;
        s.sig.params = param_types;
        s.sig.param_count = param_count;
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
    int is_varargs = 0;
    Type **param_types = NULL;
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

            Type *pty = parse_type_spec(ps);
            if (param_count == param_cap)
            {
                param_cap = param_cap ? param_cap * 2 : 4;
                param_types =
                    (Type **)realloc(param_types, sizeof(Type *) * param_cap);
            }
            param_types[param_count++] = pty;
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
    }
    expect(ps, TK_RPAREN, ")");
    expect(ps, TK_SEMI, ";");
    // Save extern
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

        while (t.kind == TK_LBRACKET)
        {
            struct PendingAttr attr = parse_attribute(ps);
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

        if (t.kind == TK_KW_MODULE)
        {
            if (attr_count > 0)
            {
                diag_error_at(lexer_source(ps->lx), attrs[0].line, attrs[0].col,
                              "attributes are not supported on module declarations");
                exit(1);
            }
            parse_module_decl(ps);
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

        if (t.kind == TK_EOF)
        {
            Token err_tok = visibility ? vis_tok : noreturn_tok;
            diag_error_at(lexer_source(ps->lx), err_tok.line, err_tok.col,
                          "unexpected end of file after '%.*s'",
                          err_tok.length, err_tok.lexeme);
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
            parse_struct_decl(ps, visibility);
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
            Node *fn = parse_function(ps, leading_noreturn, visibility, body_kind);
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

        if (t.kind == TK_KW_CONSTANT || is_type_start(ps, t))
        {
            if (bring_seen)
                bring_block_done = 1;
            if (extend_seen)
                extend_block_done = 1;
            if (attr_count > 0)
            {
                diag_error_at(lexer_source(ps->lx), attrs[0].line, attrs[0].col,
                              "attributes are not supported on global variable declarations");
                exit(1);
            }
            if (leading_noreturn)
            {
                diag_error_at(lexer_source(ps->lx), noreturn_tok.line, noreturn_tok.col,
                              "'noreturn' is only valid before functions or extern declarations");
                exit(1);
            }

            int is_const = 0;
            if (t.kind == TK_KW_CONSTANT)
            {
                lexer_next(ps->lx);
                is_const = 1;
                t = lexer_peek(ps->lx);
            }

            Type *ty = parse_type_spec(ps);
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
            decl->var_is_global = 1;
            decl->is_exposed = visibility;
            decl->line = name.line;
            decl->col = name.col;
            decl->src = lexer_source(ps->lx);

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

    if (fn_count == 0)
    {
        diag_error_at(lexer_source(ps->lx), 1, 1, "no functions found");
        exit(1);
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

    if (ps->module_full_name && fn_count > 0)
    {
        for (int i = 0; i < decl_count; ++i)
        {
            Node *fn = decls[i];
            if (!fn || fn->kind != ND_FUNC)
                continue;
            if (fn->metadata.backend_name || fn->is_entrypoint)
                continue;
            char *backend = make_module_backend_name(ps->module_full_name, fn->name);
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
        ps->aliases[ps->alias_count].is_exposed = is_exposed;
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
    ps->aliases[ps->alias_count].is_exposed = is_exposed;
    ps->alias_count++;
}

static void parse_enum_decl(Parser *ps, int is_exposed)
{
    expect(ps, TK_KW_ENUM, "enum");
    Token name = expect(ps, TK_IDENT, "enum name");
    // For now, enums are i32-typed constants; register named type alias as i32
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
        if (enum_name_heap && ps->module_full_name)
        {
            char *value_name = dup_token_text(id);
            module_registry_register_enum_value(ps->module_full_name, enum_name_heap, value_name, cur);
            free(value_name);
        }
        // next
        Token sep = lexer_peek(ps->lx);
        if (sep.kind == TK_COMMA)
        {
            lexer_next(ps->lx);
            cur++;
            continue;
        }
        // maybe end
        // if next is '}', we'll loop and close; otherwise, error
        cur++;
    }
    expect(ps, TK_SEMI, ";");
    if (enum_name_heap && ps->module_full_name)
    {
        module_registry_register_enum(ps->module_full_name, enum_name_heap, &ti32);
        free(enum_name_heap);
    }
}

static void parse_struct_decl(Parser *ps, int is_exposed)
{
    expect(ps, TK_KW_STRUCT, "struct");
    Token name = expect(ps, TK_IDENT, "struct name");
    expect(ps, TK_LBRACE, "{");
    // Create a Type object for the struct
    Type *st = (Type *)xcalloc(1, sizeof(Type));
    st->kind = TY_STRUCT;
    st->struct_name = (char *)xmalloc((size_t)name.length + 1);
    memcpy((char *)st->struct_name, name.lexeme, (size_t)name.length);
    ((char *)st->struct_name)[name.length] = '\0';
    st->is_exposed = is_exposed;
    // Parse fields: list of type ident ';'
    const char **fnames = NULL;
    Type **ftypes = NULL;
    int *foff = NULL;
    int fcnt = 0, fcap = 0;
    int offset = 0;
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
        (void)is_const; // fields ignore const for now
        Type *fty = parse_type_spec(ps);
        Token fname = expect(ps, TK_IDENT, "field name");
        // optional multiple declarators not supported; one per line
        if (is_exposed)
        {
            if (fty && fty->kind == TY_STRUCT && !fty->is_exposed)
            {
                const char *hidden_name = fty->struct_name ? fty->struct_name : "<anonymous>";
                diag_error_at(lexer_source(ps->lx), fname.line, fname.col,
                              "exposed struct '%.*s' field '%.*s' uses hidden struct '%s'",
                              name.length, name.lexeme, fname.length, fname.lexeme,
                              hidden_name);
                exit(1);
            }
        }
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
        fcnt++;
        int sz = type_sizeof_simple(fty);
        if (fty && fty->kind == TY_STRUCT && sz == 0)
        {
            diag_error_at(lexer_source(ps->lx), fname.line, fname.col,
                          "struct field '%.*s' has incomplete type", fname.length,
                          fname.lexeme);
            exit(1);
        }
        foff[fcnt - 1] = offset;
        offset += sz;
    }
    st->strct.field_names = fnames;
    st->strct.field_types = ftypes;
    st->strct.field_offsets = foff;
    st->strct.field_count = fcnt;
    st->strct.size_bytes = offset;
    // register named type
    named_type_add(ps, name.lexeme, name.length, st, is_exposed);
    if (is_exposed && ps->module_full_name)
        module_registry_register_struct(ps->module_full_name, st);
    expect(ps, TK_SEMI, ";");
}
