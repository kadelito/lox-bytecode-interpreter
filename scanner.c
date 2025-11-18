#include <string.h>

#include "common.h"
#include "scanner.h"

typedef struct {
    const char* start;   // of current token
    const char* current; // index to source string
    int line;
} Scanner;

Scanner scanner;

#define CASE_RETURN_STR(val) case val: return #val
char* getTokenName(TokenType token) {
    switch (token) {
        CASE_RETURN_STR(TOKEN_LEFT_PAREN);
        CASE_RETURN_STR(TOKEN_RIGHT_PAREN);
        CASE_RETURN_STR(TOKEN_LEFT_BRACE);
        CASE_RETURN_STR(TOKEN_RIGHT_BRACE);
        CASE_RETURN_STR(TOKEN_LEFT_BRACKET);
        CASE_RETURN_STR(TOKEN_RIGHT_BRACKET);

        CASE_RETURN_STR(TOKEN_COMMA);
        CASE_RETURN_STR(TOKEN_DOT);
        CASE_RETURN_STR(TOKEN_MINUS);
        CASE_RETURN_STR(TOKEN_PLUS);
        CASE_RETURN_STR(TOKEN_SEMICOLON);
        CASE_RETURN_STR(TOKEN_FSLASH);
        CASE_RETURN_STR(TOKEN_STAR);
        CASE_RETURN_STR(TOKEN_PERCENT);

        CASE_RETURN_STR(TOKEN_BANG);
        CASE_RETURN_STR(TOKEN_BANG_EQUAL);
        CASE_RETURN_STR(TOKEN_EQUAL);
        CASE_RETURN_STR(TOKEN_EQUAL_EQUAL);
        CASE_RETURN_STR(TOKEN_GREATER);
        CASE_RETURN_STR(TOKEN_GREATER_EQUAL);
        CASE_RETURN_STR(TOKEN_LESS);
        CASE_RETURN_STR(TOKEN_LESS_EQUAL);
        CASE_RETURN_STR(TOKEN_IDENTIFIER);
        CASE_RETURN_STR(TOKEN_STRING);
        CASE_RETURN_STR(TOKEN_NUMBER);
        CASE_RETURN_STR(TOKEN_AND);
        CASE_RETURN_STR(TOKEN_CLASS);
        CASE_RETURN_STR(TOKEN_ELSE);
        CASE_RETURN_STR(TOKEN_FALSE);
        CASE_RETURN_STR(TOKEN_FOR);
        CASE_RETURN_STR(TOKEN_FUN);
        CASE_RETURN_STR(TOKEN_IF);
        CASE_RETURN_STR(TOKEN_NIL);
        CASE_RETURN_STR(TOKEN_OR);
        CASE_RETURN_STR(TOKEN_RETURN);
        CASE_RETURN_STR(TOKEN_SUPER);
        CASE_RETURN_STR(TOKEN_THIS);
        CASE_RETURN_STR(TOKEN_TRUE);
        CASE_RETURN_STR(TOKEN_VAR);
        CASE_RETURN_STR(TOKEN_WHILE);
        CASE_RETURN_STR(TOKEN_BREAK);
        CASE_RETURN_STR(TOKEN_CONTINUE);
        CASE_RETURN_STR(TOKEN_ERROR);
        CASE_RETURN_STR(TOKEN_EOF);
    }
}
#undef CASE_RETURN_STR

void initScanner(const char* source) {
    scanner.start = source;
    scanner.current = source;
    scanner.line = 1;
}

static bool isAlpha(char c) {
    return (c >= 'a' && c <= 'z') ||
           (c >= 'A' && c <= 'Z') ||
            c == '_';
}

static bool isDigit(char c) {
    return c >= '0' && c <= '9';
}

static bool isAtEnd() {
    return *scanner.current == '\0';
}

static char advance() {
    scanner.current++;
    return scanner.current[-1];
}

static char peek() {
    return *scanner.current;
}

static char peekNext() {
    return isAtEnd() ? '\0' : scanner.current[1];
}

static bool match(char expected) {
    if (isAtEnd()) return false;
    if (*scanner.current != expected) return false;
    scanner.current++;
    return true;
}

static Token makeToken(TokenType type) {
    Token token = {
        .type = type,
        .start = scanner.start,
        .length = (int)(scanner.current - scanner.start),
        .line = scanner.line
    };
    return token;
}

static Token errorToken(const char* message) {
    Token token = {
        .type = TOKEN_ERROR,
        .start = message,
        .length = (int)strlen(message),
        .line = scanner.line
    };
    return token;
}

static void skipWhitespace() {
    for (;;) {
        char c = peek();
        switch (c) {
            case '\n':
                scanner.line++;
            case ' ':
            case '\r':
            case '\t':
                advance();
                break;
            case '/':
                if (peekNext() == '/')
                    while (!isAtEnd() && peek() != '\n') advance();
                else return;
                break;
            default:
                return;
        }
    }
}

static TokenType checkKeyword(int start, int length,
        const char* rest, TokenType type) {
    if (scanner.current - scanner.start == start + length &&
        memcmp(scanner.start + start, rest, length) == 0) {
        return type;
        }

    return TOKEN_IDENTIFIER;
}

static TokenType identifierType() {
    switch (scanner.start[0]) {
        case 'a': return checkKeyword(1, 2, "nd", TOKEN_AND);
        case 'b': return checkKeyword(1, 4, "reak", TOKEN_BREAK);
        case 'c':
            if (scanner.current - scanner.start > 1) {
                switch (scanner.start[1]) {
                    case 'l': return checkKeyword(2, 3, "ass", TOKEN_CLASS);
                    case 'o': return checkKeyword(2, 6, "ntinue", TOKEN_CONTINUE);
                }
            }
            break;
        case 'e': return checkKeyword(1, 3, "lse", TOKEN_ELSE);
        case 'f':
            if (scanner.current - scanner.start > 1) {
                switch (scanner.start[1]) {
                    case 'a': return checkKeyword(2, 3, "lse", TOKEN_FALSE);
                    case 'o': return checkKeyword(2, 1, "r", TOKEN_FOR);
                    case 'u': return checkKeyword(2, 1, "n", TOKEN_FUN);
                }
            }
            break;
        case 'i': return checkKeyword(1, 1, "f", TOKEN_IF);
        case 'n': return checkKeyword(1, 2, "il", TOKEN_NIL);
        case 'o': return checkKeyword(1, 1, "r", TOKEN_OR);
        case 'r': return checkKeyword(1, 5, "eturn", TOKEN_RETURN);
        case 's': return checkKeyword(1, 4, "uper", TOKEN_SUPER);
        case 't':
            if (scanner.current - scanner.start > 1) {
                switch (scanner.start[1]) {
                    case 'h': return checkKeyword(2, 2, "is", TOKEN_THIS);
                    case 'r': return checkKeyword(2, 2, "ue", TOKEN_TRUE);
                }
            }
            break;
        case 'v': return checkKeyword(1, 2, "ar", TOKEN_VAR);
        case 'w': return checkKeyword(1, 4, "hile", TOKEN_WHILE);
    }

    return TOKEN_IDENTIFIER;
}

static Token identifier() {
    while (isAlpha(peek()) || isDigit(peek())) advance();
    return makeToken(identifierType());
}

static Token number() {
    while (isDigit(peek())) advance();

    // Look for a fractional part.
    if (peek() == '.' && isDigit(peekNext())) {
        // Consume the '.'
        advance();

        while (isDigit(peek())) advance();
    }

    return makeToken(TOKEN_NUMBER);
}

static Token string() {
    while (peek() != '"' && !isAtEnd()) {
        if (peek() == '\n') scanner.line++;
        advance();
    }

    if (isAtEnd()) return errorToken("Unterminated string.");

    // closing quote
    advance();
    return makeToken(TOKEN_STRING);
}

Token scanToken() {
    skipWhitespace();
    scanner.start = scanner.current;

    if (isAtEnd()) return makeToken(TOKEN_EOF);

    char c = advance();
    if (isAlpha(c)) return identifier();
    if (isDigit(c)) return number();

    switch (c) {
        case '(': return makeToken(TOKEN_LEFT_PAREN);
        case ')': return makeToken(TOKEN_RIGHT_PAREN);
        case '{': return makeToken(TOKEN_LEFT_BRACE);
        case '}': return makeToken(TOKEN_RIGHT_BRACE);
        case '[': return makeToken(TOKEN_LEFT_BRACKET);
        case ']': return makeToken(TOKEN_RIGHT_BRACKET);
        case ';': return makeToken(TOKEN_SEMICOLON);
        case ',': return makeToken(TOKEN_COMMA);
        case '.': return makeToken(TOKEN_DOT);
        case '-': return makeToken(TOKEN_MINUS);
        case '+': return makeToken(TOKEN_PLUS);
        case '/': return makeToken(TOKEN_FSLASH);
        case '*': return makeToken(TOKEN_STAR);
        case '%': return makeToken(TOKEN_PERCENT);

        case '!':
            return makeToken(match('=') ? TOKEN_BANG_EQUAL : TOKEN_BANG);
        case '=':
            return makeToken(match('=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);
        case '<':
            return makeToken(match('=') ? TOKEN_LESS_EQUAL : TOKEN_LESS);
        case '>':
            return makeToken(match('=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);
        case '"': return string();

        default: return errorToken("Unexpected character.");
    }
}
