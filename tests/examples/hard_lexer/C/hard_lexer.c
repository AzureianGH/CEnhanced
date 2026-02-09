#include <stdio.h>
#include <stdint.h>

typedef enum
{
    TOK_EOF = 0,
    TOK_IDENT,
    TOK_INT,
    TOK_HEX,
    TOK_FLOAT,
    TOK_STRING,
    TOK_OP,
    TOK_UNKNOWN
} TokenKind;

typedef struct
{
    TokenKind kind;
    const char *start;
    int length;
} Token;

typedef struct
{
    const char *src;
    int pos;
} Lexer;

static const char *kTokenNames[] = {
    "EOF",
    "IDENT",
    "INT",
    "HEX",
    "FLOAT",
    "STRING",
    "OP",
    "UNKNOWN",
};

static int is_alpha(char c)
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

static int is_digit(char c)
{
    return (c >= '0' && c <= '9');
}

static int is_hex_digit(char c)
{
    return is_digit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

static char peek(const Lexer *lx, int offset)
{
    return lx->src[lx->pos + offset];
}

static void skip_whitespace_and_comments(Lexer *lx)
{
    for (;;)
    {
        char c = peek(lx, 0);
        if (c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\v' || c == '\f')
        {
            lx->pos++;
            continue;
        }
        if (c == '/' && peek(lx, 1) == '/')
        {
            lx->pos += 2;
            while (peek(lx, 0) != 0 && peek(lx, 0) != '\n')
                lx->pos++;
            continue;
        }
        if (c == '/' && peek(lx, 1) == '*')
        {
            lx->pos += 2;
            while (peek(lx, 0) != 0)
            {
                if (peek(lx, 0) == '*' && peek(lx, 1) == '/')
                {
                    lx->pos += 2;
                    break;
                }
                lx->pos++;
            }
            continue;
        }
        break;
    }
}

static Token make_token(Lexer *lx, TokenKind kind, int start)
{
    Token tok;
    tok.kind = kind;
    tok.start = lx->src + start;
    tok.length = lx->pos - start;
    return tok;
}

static Token lex_token(Lexer *lx)
{
    skip_whitespace_and_comments(lx);

    int start = lx->pos;
    char c = peek(lx, 0);
    if (c == 0)
        return make_token(lx, TOK_EOF, start);

    if (is_alpha(c))
    {
        lx->pos++;
        while (is_alpha(peek(lx, 0)) || is_digit(peek(lx, 0)))
            lx->pos++;
        return make_token(lx, TOK_IDENT, start);
    }

    if (is_digit(c))
    {
        if (c == '0' && (peek(lx, 1) == 'x' || peek(lx, 1) == 'X'))
        {
            lx->pos += 2;
            while (is_hex_digit(peek(lx, 0)))
                lx->pos++;
            return make_token(lx, TOK_HEX, start);
        }

        lx->pos++;
        while (is_digit(peek(lx, 0)))
            lx->pos++;

        if (peek(lx, 0) == '.' && is_digit(peek(lx, 1)))
        {
            lx->pos++;
            while (is_digit(peek(lx, 0)))
                lx->pos++;
            return make_token(lx, TOK_FLOAT, start);
        }

        return make_token(lx, TOK_INT, start);
    }

    if (c == '"')
    {
        lx->pos++;
        while (peek(lx, 0) != 0 && peek(lx, 0) != '"')
        {
            if (peek(lx, 0) == '\\' && peek(lx, 1) != 0)
                lx->pos += 2;
            else
                lx->pos++;
        }
        if (peek(lx, 0) == '"')
            lx->pos++;
        return make_token(lx, TOK_STRING, start);
    }

    if ((c == '=' && peek(lx, 1) == '=') || (c == '!' && peek(lx, 1) == '=') ||
        (c == '<' && peek(lx, 1) == '=') || (c == '>' && peek(lx, 1) == '=') ||
        (c == '&' && peek(lx, 1) == '&') || (c == '|' && peek(lx, 1) == '|'))
    {
        lx->pos += 2;
        return make_token(lx, TOK_OP, start);
    }

    if (c == '+' || c == '-' || c == '*' || c == '/' || c == '=' || c == '<' || c == '>' ||
        c == '(' || c == ')' || c == '{' || c == '}' || c == ';' || c == ',' || c == '.')
    {
        lx->pos++;
        return make_token(lx, TOK_OP, start);
    }

    lx->pos++;
    return make_token(lx, TOK_UNKNOWN, start);
}

static void print_token(const Token *tok)
{
    const char *name = kTokenNames[tok->kind];
    printf("%-8s '%.*s'\n", name, tok->length, tok->start);
}

int main(void)
{
    const char *source =
        "// header comment\n"
        "sum = 2 + 3.14 * (value_1 - 0x2A);\n"
        "name == \"a\\n\\t\\\\b\" && flag != 0\n"
        "/* block comment with = and && */\n";

    Lexer lx = {source, 0};
    for (;;)
    {
        Token tok = lex_token(&lx);
        print_token(&tok);
        if (tok.kind == TOK_EOF)
            break;
    }
    return 0;
}
