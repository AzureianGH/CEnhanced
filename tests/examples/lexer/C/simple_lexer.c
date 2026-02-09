// Minimal hand-written lexer demo used alongside the CHance version.
// Build: gcc -std=c99 -Wall -Wextra -o simple_lexer simple_lexer.c

#include <ctype.h>
#include <stdio.h>
#include <string.h>

typedef enum TokenKind {
    TOK_EOF,
    TOK_NUMBER,
    TOK_IDENT,
    TOK_LPAREN,
    TOK_RPAREN,
    TOK_PLUS,
    TOK_MINUS,
    TOK_STAR,
    TOK_SLASH,
    TOK_UNKNOWN
} TokenKind;

typedef struct Token {
    TokenKind kind;
    const char *start;
    int length;
} Token;

typedef struct Lexer {
    const char *src;
    size_t pos;
} Lexer;

static const char *kTokenNames[] = {
    "EOF",
    "NUMBER",
    "IDENT",
    "LPAREN",
    "RPAREN",
    "PLUS",
    "MINUS",
    "STAR",
    "SLASH",
    "UNKNOWN",
};

static int is_alpha(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

static int is_digit(char c) {
    return c >= '0' && c <= '9';
}

static void skip_whitespace(Lexer *lex) {
    while (lex->src[lex->pos] && isspace((unsigned char)lex->src[lex->pos])) {
        lex->pos++;
    }
}

static Token make_token(Lexer *lex, TokenKind kind, size_t start) {
    Token tok;
    tok.kind = kind;
    tok.start = lex->src + start;
    tok.length = (int)(lex->pos - start);
    return tok;
}

static Token lex_token(Lexer *lex) {
    skip_whitespace(lex);

    size_t start = lex->pos;
    char c = lex->src[lex->pos];

    if (c == '\0') {
        return make_token(lex, TOK_EOF, start);
    }

    if (is_alpha(c)) {
        lex->pos++;
        while (is_alpha(lex->src[lex->pos]) || is_digit(lex->src[lex->pos])) {
            lex->pos++;
        }
        return make_token(lex, TOK_IDENT, start);
    }

    if (is_digit(c)) {
        lex->pos++;
        while (is_digit(lex->src[lex->pos])) {
            lex->pos++;
        }
        return make_token(lex, TOK_NUMBER, start);
    }

    lex->pos++;
    switch (c) {
        case '+': return make_token(lex, TOK_PLUS, start);
        case '-': return make_token(lex, TOK_MINUS, start);
        case '*': return make_token(lex, TOK_STAR, start);
        case '/': return make_token(lex, TOK_SLASH, start);
        case '(': return make_token(lex, TOK_LPAREN, start);
        case ')': return make_token(lex, TOK_RPAREN, start);
        default:  return make_token(lex, TOK_UNKNOWN, start);
    }
}

static void print_token(Token tok) {
    printf("%-8s '%.*s'\n", kTokenNames[tok.kind], tok.length, tok.start);
}

int main(int argc, char **argv) {
    const char *source = argc > 1 ? argv[1] : "sum = a1 + 23*(foo - 5)/bar";

    Lexer lex = { .src = source, .pos = 0 };

    for (;;) {
        Token tok = lex_token(&lex);
        print_token(tok);
        if (tok.kind == TOK_EOF) {
            break;
        }
    }

    return 0;
}
