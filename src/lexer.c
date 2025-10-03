#include "ast.h"
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Lexer
{
    SourceBuffer src;
    int idx;
    int line;
    int col;
    Token lookahead;
    int has_look;
};

static int at_end(Lexer *lx) { return lx->idx >= lx->src.length; }
static char peekc(Lexer *lx)
{
    return at_end(lx) ? '\0' : lx->src.src[lx->idx];
}
static char getc2(Lexer *lx)
{
    if (at_end(lx))
        return '\0';
    char c = lx->src.src[lx->idx++];
    if (c == '\n')
    {
        lx->line++;
        lx->col = 1;
    }
    else
        lx->col++;
    return c;
}

static Token make_tok(Lexer *lx, TokenKind k, const char *start, int len)
{
    Token t;
    t.kind = k;
    t.lexeme = start;
    t.length = len;
    t.int_val = 0;
    t.line = lx->line;
    t.col = lx->col - len;
    return t;
}

static int is_ident_start(int c) { return isalpha(c) || c == '_'; }
static int is_ident_part(int c) { return isalnum(c) || c == '_'; }

Lexer *lexer_create(SourceBuffer src)
{
    Lexer *lx = (Lexer *)xcalloc(1, sizeof(Lexer));
    lx->src = src;
    lx->idx = 0;
    lx->line = 1;
    lx->col = 1;
    lx->has_look = 0;
    return lx;
}

void lexer_destroy(Lexer *lx)
{
    if (lx)
        free(lx);
}

static void skip_ws_and_comments(Lexer *lx)
{
    for (;;)
    {
        char c = peekc(lx);
        if (isspace((unsigned char)c))
        {
            getc2(lx);
            continue;
        }
        if (c == '/' && lx->idx + 1 < lx->src.length &&
            lx->src.src[lx->idx + 1] == '/')
        {
            while (!at_end(lx) && getc2(lx) != '\n')
                ;
            continue;
        }
        break;
    }
}

static Token lex_number(Lexer *lx)
{
    int start = lx->idx;
    // Check for base prefixes: 0x, 0b, 0o, 0d
    if (peekc(lx) == '0' && lx->idx + 1 < lx->src.length)
    {
        char p2 = lx->src.src[lx->idx + 1];
        int base = 0;
        if (p2 == 'x' || p2 == 'X')
            base = 16;
        else if (p2 == 'b' || p2 == 'B')
            base = 2;
        else if (p2 == 'o' || p2 == 'O')
            base = 8;
        else if (p2 == 'd' || p2 == 'D')
            base = 10;
        if (base != 0)
        {
            // consume '0' and prefix char
            getc2(lx);
            getc2(lx);
            long long v = 0;
            int any = 0;
            for (;;)
            {
                char c = peekc(lx);
                int digit = -1;
                if (c >= '0' && c <= '9')
                    digit = c - '0';
                else if (c >= 'a' && c <= 'f')
                    digit = 10 + (c - 'a');
                else if (c >= 'A' && c <= 'F')
                    digit = 10 + (c - 'A');
                else
                    break;
                if (digit >= base)
                    break;
                getc2(lx);
                v = v * base + digit;
                any = 1;
            }
            int len = lx->idx - start;
            Token t = make_tok(lx, TK_INT, lx->src.src + start, len);
            t.int_val = any ? v : 0;
            return t;
        }
    }
    // Default: decimal without prefix
    while (isdigit((unsigned char)peekc(lx)))
        getc2(lx);
    int len = lx->idx - start;
    Token t = make_tok(lx, TK_INT, lx->src.src + start, len);
    long long v = 0;
    for (int i = 0; i < len; i++)
    {
        v = v * 10 + (lx->src.src[start + i] - '0');
    }
    t.int_val = v;
    return t;
}

static Token lex_ident_or_kw(Lexer *lx)
{
    int start = lx->idx;
    getc2(lx);
    while (is_ident_part((unsigned char)peekc(lx)))
        getc2(lx);
    int len = lx->idx - start;
    const char *p = lx->src.src + start;
    TokenKind k = TK_IDENT;
    if (len == 3 && strncmp(p, "fun", 3) == 0)
        k = TK_KW_FUN;
    else if (len == 3 && strncmp(p, "ret", 3) == 0)
        k = TK_KW_RET;
    else if (len == 5 && strncmp(p, "stack", 5) == 0)
        k = TK_KW_STACK;
    else if (len == 3 && strncmp(p, "reg", 3) == 0)
        k = TK_KW_REG;
    else if ((len == 6 && strncmp(p, "struct", 6) == 0) ||
             (len == 5 && strncmp(p, "struc", 5) == 0))
        k = TK_KW_STRUCT;
    else if (len == 6 && strncmp(p, "extend", 6) == 0)
        k = TK_KW_EXTEND;
    else if (len == 4 && strncmp(p, "from", 4) == 0)
        k = TK_KW_FROM;
    else if (len == 2 && strncmp(p, "i8", 2) == 0)
        k = TK_KW_I8;
    else if (len == 2 && strncmp(p, "u8", 2) == 0)
        k = TK_KW_U8;
    else if (len == 3 && strncmp(p, "i16", 3) == 0)
        k = TK_KW_I16;
    else if (len == 3 && strncmp(p, "for", 3) == 0)
        k = TK_KW_FOR;
    else if (len == 3 && strncmp(p, "u16", 3) == 0)
        k = TK_KW_U16;
    else if (len == 3 && strncmp(p, "i32", 3) == 0)
        k = TK_KW_I32;
    else if (len == 3 && strncmp(p, "u32", 3) == 0)
        k = TK_KW_U32;
    else if (len == 3 && strncmp(p, "i64", 3) == 0)
        k = TK_KW_I64;
    else if (len == 3 && strncmp(p, "u64", 3) == 0)
        k = TK_KW_U64;
    else if (len == 3 && strncmp(p, "f32", 3) == 0)
        k = TK_KW_F32;
    else if (len == 3 && strncmp(p, "f64", 3) == 0)
        k = TK_KW_F64;
    else if (len == 4 && strncmp(p, "f128", 4) == 0)
        k = TK_KW_F128;
    else if (len == 4 && strncmp(p, "void", 4) == 0)
        k = TK_KW_VOID;
    else if (len == 4 && strncmp(p, "char", 4) == 0)
        k = TK_KW_CHAR;
    else if (len == 4 && strncmp(p, "long", 4) == 0)
        k = TK_KW_LONG;
    else if (len == 5 && strncmp(p, "ulong", 5) == 0)
        k = TK_KW_ULONG;
    else if (len == 3 && strncmp(p, "int", 3) == 0)
        k = TK_KW_INT;
    else if (len == 4 && strncmp(p, "uint", 4) == 0)
        k = TK_KW_UINT;
    else if (len == 5 && strncmp(p, "short", 5) == 0)
        k = TK_KW_SHORT;
    else if (len == 6 && strncmp(p, "ushort", 6) == 0)
        k = TK_KW_USHORT;
    else if (len == 4 && strncmp(p, "byte", 4) == 0)
        k = TK_KW_BYTE;
    else if (len == 5 && strncmp(p, "ubyte", 5) == 0)
        k = TK_KW_UBYTE;
    else if (len == 5 && strncmp(p, "float", 5) == 0)
        k = TK_KW_FLOAT;
    else if (len == 6 && strncmp(p, "double", 6) == 0)
        k = TK_KW_DOUBLE;
    else if (len == 8 && strncmp(p, "constant", 8) == 0)
        k = TK_KW_CONSTANT;
    else if (len == 2 && strncmp(p, "if", 2) == 0)
        k = TK_KW_IF;
    else if (len == 4 && strncmp(p, "else", 4) == 0)
        k = TK_KW_ELSE;
    else if (len == 5 && strncmp(p, "while", 5) == 0)
        k = TK_KW_WHILE;
    else if (len == 4 && strncmp(p, "enum", 4) == 0)
        k = TK_KW_ENUM;
    else if (len == 3 && strncmp(p, "for", 3) == 0)
        k = TK_KW_FOR;
    else if (len == 5 && strncmp(p, "alias", 5) == 0)
        k = TK_KW_ALIAS;
    else if (len == 2 && strncmp(p, "as", 2) == 0)
        k = TK_KW_AS;
    else if (len == 4 && strncmp(p, "null", 4) == 0)
        k = TK_KW_NULL;
    else if (len == 7 && strncmp(p, "nullptr", 7) == 0)
        k = TK_KW_NULLPTR;
    return make_tok(lx, k, p, len);
}

Token lexer_next(Lexer *lx)
{
    if (lx->has_look)
    {
        lx->has_look = 0;
        return lx->lookahead;
    }
    skip_ws_and_comments(lx);
    if (at_end(lx))
        return make_tok(lx, TK_EOF, lx->src.src + lx->idx, 0);
    char c = peekc(lx);
    if (isdigit((unsigned char)c))
        return lex_number(lx);
    if (c == '"')
    {
        // string literal
        int start = lx->idx;
        getc2(lx);
        while (!at_end(lx))
        {
            char d = getc2(lx);
            if (d == '\\')
            {
                getc2(lx);
                continue;
            }
            if (d == '"')
                break;
        }
        int len = lx->idx - start;
        return make_tok(lx, TK_STRING, lx->src.src + start, len);
    }
    if (is_ident_start((unsigned char)c))
        return lex_ident_or_kw(lx);
    // punctuation
    if (c == '(')
    {
        getc2(lx);
        return make_tok(lx, TK_LPAREN, lx->src.src + lx->idx - 1, 1);
    }
    if (c == ')')
    {
        getc2(lx);
        return make_tok(lx, TK_RPAREN, lx->src.src + lx->idx - 1, 1);
    }
    if (c == '{')
    {
        getc2(lx);
        return make_tok(lx, TK_LBRACE, lx->src.src + lx->idx - 1, 1);
    }
    if (c == '}')
    {
        getc2(lx);
        return make_tok(lx, TK_RBRACE, lx->src.src + lx->idx - 1, 1);
    }
    if (c == '[')
    {
        getc2(lx);
        return make_tok(lx, TK_LBRACKET, lx->src.src + lx->idx - 1, 1);
    }
    if (c == ']')
    {
        getc2(lx);
        return make_tok(lx, TK_RBRACKET, lx->src.src + lx->idx - 1, 1);
    }
    if (c == ';')
    {
        getc2(lx);
        return make_tok(lx, TK_SEMI, lx->src.src + lx->idx - 1, 1);
    }
    if (c == ',')
    {
        getc2(lx);
        return make_tok(lx, TK_COMMA, lx->src.src + lx->idx - 1, 1);
    }
    if (c == '.')
    {
        getc2(lx);
        return make_tok(lx, TK_DOT, lx->src.src + lx->idx - 1, 1);
    }
    if (c == '?')
    {
        getc2(lx);
        return make_tok(lx, TK_QUESTION, lx->src.src + lx->idx - 1, 1);
    }
    if (c == ':')
    {
        getc2(lx);
        return make_tok(lx, TK_COLON, lx->src.src + lx->idx - 1, 1);
    }
    if (c == '+')
    {
        if (lx->idx + 1 < lx->src.length && lx->src.src[lx->idx + 1] == '+')
        {
            getc2(lx);
            getc2(lx);
            return make_tok(lx, TK_PLUSPLUS, lx->src.src + lx->idx - 2, 2);
        }
        getc2(lx);
        return make_tok(lx, TK_PLUS, lx->src.src + lx->idx - 1, 1);
    }
    if (c == '&')
    {
        if (lx->idx + 1 < lx->src.length && lx->src.src[lx->idx + 1] == '&')
        {
            getc2(lx);
            getc2(lx);
            return make_tok(lx, TK_ANDAND, lx->src.src + lx->idx - 2, 2);
        }
        getc2(lx);
        return make_tok(lx, TK_AMP, lx->src.src + lx->idx - 1, 1);
    }
    if (c == '*')
    {
        getc2(lx);
        return make_tok(lx, TK_STAR, lx->src.src + lx->idx - 1, 1);
    }
    if (c == '/')
    {
        // we've already handled '//' comments above; single '/' is division
        getc2(lx);
        return make_tok(lx, TK_SLASH, lx->src.src + lx->idx - 1, 1);
    }
    if (c == '-')
    {
        if (lx->idx + 1 < lx->src.length)
        {
            if (lx->src.src[lx->idx + 1] == '>')
            {
                getc2(lx);
                getc2(lx);
                return make_tok(lx, TK_ARROW, lx->src.src + lx->idx - 2, 2);
            }
            if (lx->src.src[lx->idx + 1] == '-')
            {
                getc2(lx);
                getc2(lx);
                return make_tok(lx, TK_MINUSMINUS, lx->src.src + lx->idx - 2, 2);
            }
        }
        getc2(lx);
        return make_tok(lx, TK_MINUS, lx->src.src + lx->idx - 1, 1);
    }
    if (c == '=')
    {
        if (lx->idx + 1 < lx->src.length && lx->src.src[lx->idx + 1] == '=')
        {
            getc2(lx);
            getc2(lx);
            return make_tok(lx, TK_EQEQ, lx->src.src + lx->idx - 2, 2);
        }
        if (lx->idx + 1 < lx->src.length && lx->src.src[lx->idx + 1] == '>')
        {
            getc2(lx);
            getc2(lx);
            return make_tok(lx, TK_ACCESS, lx->src.src + lx->idx - 2, 2);
        }
        getc2(lx);
        return make_tok(lx, TK_ASSIGN, lx->src.src + lx->idx - 1, 1);
    }
    if (c == '<')
    {
        if (lx->idx + 1 < lx->src.length && lx->src.src[lx->idx + 1] == '=')
        {
            getc2(lx);
            getc2(lx);
            return make_tok(lx, TK_LTE, lx->src.src + lx->idx - 2, 2);
        }
        getc2(lx);
        return make_tok(lx, TK_LT, lx->src.src + lx->idx - 1, 1);
    }
    if (c == '>')
    {
        if (lx->idx + 1 < lx->src.length && lx->src.src[lx->idx + 1] == '=')
        {
            getc2(lx);
            getc2(lx);
            return make_tok(lx, TK_GTE, lx->src.src + lx->idx - 2, 2);
        }
        getc2(lx);
        return make_tok(lx, TK_GT, lx->src.src + lx->idx - 1, 1);
    }
    if (c == '!')
    {
        if (lx->idx + 1 < lx->src.length && lx->src.src[lx->idx + 1] == '=')
        {
            getc2(lx);
            getc2(lx);
            return make_tok(lx, TK_BANGEQ, lx->src.src + lx->idx - 2, 2);
        }
        diag_error_at(&lx->src, lx->line, lx->col,
                      "unexpected character '%c' (did you mean '!='?)", c);
        getc2(lx);
        return make_tok(lx, TK_EOF, lx->src.src + lx->idx, 0);
    }
    // unknown
    diag_error_at(&lx->src, lx->line, lx->col, "unexpected character '%c'", c);
    getc2(lx);
    return make_tok(lx, TK_EOF, lx->src.src + lx->idx, 0);
}

Token lexer_peek(Lexer *lx)
{
    if (!lx->has_look)
    {
        lx->lookahead = lexer_next(lx);
        lx->has_look = 1;
    }
    return lx->lookahead;
}

const SourceBuffer *lexer_source(Lexer *lx) { return lx ? &lx->src : NULL; }
