#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "memory.h"
#include "object.h"
#include "scanner.h"

struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} parser;

typedef enum {
    PREC_NONE,        // idk pratt parsing lol
    PREC_ASSIGNMENT,  // a = b
    PREC_OR,          // or
    PREC_AND,         // and
    PREC_EQUALITY,    // ==, !=
    PREC_COMPARISON,  // <, >, <=, >=
    PREC_TERM,        // +, -
    PREC_FACTOR,      // *, /
    PREC_UNARY,       // !, -
    PREC_CALL,        // a.b, ()
    PREC_PRIMARY      // literals?
} Precedence;

typedef void (*ParseFn)(bool canAssign);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

typedef struct {
    Token name;
    int depth;
    bool isCaptured;
} Local;

typedef struct {
    uint8_t index;
    bool isLocal;
} Upvalue;

typedef enum {
    TYPE_FUNCTION,
    TYPE_SCRIPT
} FunctionType;

typedef struct {
    TokenType type;
    int jump;
} BreakStmt;

typedef struct {
    int start;

    int* breakStmts;
    int breakStmtCount;
    int breakStmtCapacity;
} Loop;

typedef struct Compiler {
    struct Compiler* enclosing;
    ObjFunction* function;
    FunctionType type;

    Loop* curLoop;

    Local locals[UINT8_COUNT];
    int localCount;
    Upvalue upvalues[UINT8_COUNT];
    int scopeDepth;
} Compiler; // One exists for each function

ParseRule rules[]; // defined later

Compiler* current = NULL;

// Uses the previous IDENTIFIER token as the function name
static void initCompiler(Compiler* compiler, FunctionType type) {
    compiler->enclosing = current;
    compiler->function = NULL;
    compiler->type = type;

    compiler->curLoop = NULL;

    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    compiler->function = newFunction();
    current = compiler;

    if (type != TYPE_SCRIPT) {
        current->function->name = copyString(
            parser.previous.start, parser.previous.length);
    }

    // Implicitly claim slot 0 (locals is empty)
    Local* local = &current->locals[current->localCount++];
    local->depth = 0;
    local->isCaptured = false;
    local->name.start = "";
    local->name.length = 0;
}

static Chunk* currentChunk() {
    return &current->function->chunk;
}

static void errorAt(const Token* token, const char* message) {
    // disassembleChunk(currentChunk(), "Bytecode so far");
    if (parser.panicMode) return;
    parser.panicMode = true;
    fprintf(stderr, "[line %d] Error", token->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // Nothing.
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
}

static void errorAtCurrent(const char* message) {
    errorAt(&parser.current, message);
}

static void error(const char* message) {
    errorAt(&parser.previous, message);
}

static void advance() {
    parser.previous = parser.current;

    while (true) {
        // Try to get a token
        parser.current = scanToken();
        // Error token, show error & continue until non-error
        if (parser.current.type == TOKEN_ERROR)
            errorAtCurrent(parser.current.start);
        // Not an error, return normally with token @ current
        else break;
    }
}

static void consume(const TokenType type, const char* message) {
    if (parser.current.type == type) {
        advance();
        return;
    }
    errorAtCurrent(message);
}

static bool check(TokenType type) {
    return parser.current.type == type;
}

static bool match(TokenType type) {
    if (!check(type)) return false;
    advance();
    return true;
}

static void emitByte(uint8_t b) {
    writeChunk(currentChunk(), b, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
    emitByte(byte1);
    emitByte(byte2);
}

// Emits 'instruction' jump and returns index of first argument byte
static int emitJump(uint8_t instruction) {
    emitByte(instruction);
    emitByte(0xff);
    emitByte(0xff);
    return currentChunk()->count - 2;
}

static void emitShort(uint16_t arg) {
    emitByte((arg >> 8) & 0xff); // first most significant 8 bits
    emitByte(arg & 0xff);        // last least significant 8 bits
}

// Returns
static void emitLoop(int loopStart) {
    emitByte(OP_JUMP_BACK);

    int offset = currentChunk()->count - loopStart + 2; // skip 2 argument bytes i think
    if (offset > UINT16_MAX) error("Loop body too large.");

    emitShort(offset);
    // reconstructed =
}

static void emitReturn() {
    emitByte(OP_NIL);
    emitByte(OP_RETURN);
}

// Returns the index into the current chunk's constant pool
static uint8_t makeConstant(Value value) {
    int constant = addConstant(currentChunk(), value);
    if (constant > UINT8_MAX) {
        error("Too many constants in one chunk.");
        return 0;
    }

    return (uint8_t)constant;
}

static void emitConstant(Value value) {
    emitBytes(OP_CONSTANT, makeConstant(value));
}

// Patches code[offset] to jump to current location
static void patchJump(int offset) {
    // -2 to adjust for the bytecode for the jump offset itself
    int jump = currentChunk()->count - offset - 2;

    if (jump > UINT16_MAX) {
        error("Too much code to jump over.");
    }

    currentChunk()->code[offset] = (jump >> 8) & 0xff;
    currentChunk()->code[offset + 1] = jump & 0xff;
}

static void addBreakStmt(int location) {
    Loop* cur = current->curLoop;
    // Resize dynamic array if needed
    // Realistically, this will only happen once (capcity 0 -> 8 breaks)
    if (cur->breakStmtCapacity < cur->breakStmtCount + 1) {
        int oldCapacity = cur->breakStmtCapacity;
        cur->breakStmtCapacity = GROW_CAPACITY(oldCapacity);
        cur->breakStmts = GROW_ARRAY(int, cur->breakStmts,
                oldCapacity, cur->breakStmtCapacity);
    }

    cur->breakStmts[cur->breakStmtCount++] = location;
}

static ObjFunction* endCompiler() {
    emitReturn();
    ObjFunction* function = current->function;
    shrinkChunk(&function->chunk);
    #ifdef DEBUG_PRINT_CODE
    {
        const char* name = function->name != NULL ? function->name->chars : "<main script>";
        if (parser.hadError) {
            printf("Error, no code to print for %s.\n", name);
        } else {
            disassembleChunk(&function->chunk, name);
        }
    }
    #endif
    #ifdef DEBUG_CHUNK_DETAILS
    {
        const char* name = function->name != NULL ? function->name->chars : "<main script>";
        printf("Constants in '%s':\n", name);
        const ValueArray constants = function->chunk.constants;
        int i = 0;
        for (Value* cur = constants.values; i < constants.count; cur++, i++) {
            printf("  [");
            printValueFancy(*cur);
            printf("]\n");
        }

        printf("==== (%s end) ====\n", name);
    }
    #endif
    current = current->enclosing;
    return function;
}

static void beginScope() {
    current->scopeDepth++;
}

static void endScope() {
    current->scopeDepth--;

    // Pop variables in the escaped scope off the stack
    while (current->localCount > 0 && current->locals[
           current->localCount - 1].depth > current->scopeDepth) {
        if (current->locals[current->localCount - 1].isCaptured) {
            emitByte(OP_CLOSE_UPVALUE);
        } else {
            emitByte(OP_POP);
        }
        current->localCount--;
    }
}

static void expression();
static void statement();
static void declaration();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);

static void binary(bool ignored) {
    TokenType operatorType = parser.previous.type;
    ParseRule* rule = getRule(operatorType);
    parsePrecedence((Precedence)(rule->precedence + 1));

    switch (operatorType) {
        case TOKEN_PLUS:          emitByte(OP_ADD); break;
        case TOKEN_MINUS:         emitByte(OP_SUBTRACT); break;
        case TOKEN_STAR:          emitByte(OP_MULTIPLY); break;
        case TOKEN_FSLASH:        emitByte(OP_DIVIDE); break;
        case TOKEN_PERCENT:       emitByte(OP_MODULO); break;
        case TOKEN_BANG_EQUAL:    emitBytes(OP_EQUAL, OP_NOT); break; // !(a == b) <-> a != b
        case TOKEN_EQUAL_EQUAL:   emitByte(OP_EQUAL); break;
        case TOKEN_GREATER:       emitByte(OP_GREATER); break;
        case TOKEN_GREATER_EQUAL: emitBytes(OP_LESS, OP_NOT); break;   // !(a < b) <-> a >= b
        case TOKEN_LESS:          emitByte(OP_LESS); break;
        case TOKEN_LESS_EQUAL:    emitBytes(OP_GREATER, OP_NOT); break;// !(a > b) <-> a <= b
        default: break; // Unreachable.
    }
}

static void literal(bool ignored) {
    switch (parser.previous.type) {
        case TOKEN_FALSE: emitByte(OP_FALSE); break;
        case TOKEN_NIL: emitByte(OP_NIL); break;
        case TOKEN_TRUE: emitByte(OP_TRUE); break;
        default: break; // Unreachable.
    }
}

static void and_(bool ignored) {
    int endJump = emitJump(OP_JUMP_IF_FALSE);

    emitByte(OP_POP);
    parsePrecedence(PREC_AND);

    patchJump(endJump);
}

static void or_(bool ignored) {
    int elseJump = emitJump(OP_JUMP_IF_FALSE);
    int endJump = emitJump(OP_JUMP);

    patchJump(elseJump);
    emitByte(OP_POP);

    parsePrecedence(PREC_OR);
    patchJump(endJump);
}

// Helper for binary()
static ParseRule* getRule(TokenType type) {
    return &rules[type];
}

// Returns the number of arguments parsed.
static uint16_t exprList(int max, TokenType endType) {
    uint16_t argCount = 0;
    if (!check(endType)) {
        do {
            expression();
            if (argCount >= max) {
                error("Can't have more than 255 arguments.");
            }
            argCount++;
        } while (match(TOKEN_COMMA));
    }
    consume(endType, "Expect ')' after arguments.");
    return argCount;
}

static void call(bool ignored) {
    // Emit
    uint8_t argCount = exprList(UINT8_MAX, TOKEN_RIGHT_PAREN);
    emitBytes(OP_CALL, argCount);
}

static void unary(bool ignored) {
    TokenType operatorType = parser.previous.type;

    // Compile the operand.
    parsePrecedence(PREC_UNARY);

    // Emit the operator instruction.
    switch (operatorType) {
        case TOKEN_MINUS: emitByte(OP_NEGATE); break;
        case TOKEN_BANG: emitByte(OP_NOT); break;
        default: break; // Unreachable.
    }
}

static void number(bool ignored) {
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

static void string(bool ignored) {
    // "+ 1" & "- 2" trims quotation marks
    emitConstant(OBJ_VAL(copyString(parser.previous.start + 1,parser.previous.length - 2)));
}

static void grouping(bool ignored) {
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void list(bool ignored) {
    uint16_t length = exprList(UINT16_MAX, TOKEN_RIGHT_BRACKET);
    emitByte(OP_LIST);
    emitShort(length);
}

static void indexOp(bool canAssign) {
    expression();
    consume(TOKEN_RIGHT_BRACKET, "Expect ']' after indexing.");
    if (canAssign && match(TOKEN_EQUAL)) {
        expression();
        emitByte(OP_INDEX_SET);
    } else {
        emitByte(OP_INDEX_GET);
    }
}

static void expression() {
    parsePrecedence(PREC_ASSIGNMENT);
}

static uint8_t identifierConstant(const Token* name) {
    return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

static void addLocal(Token name) {
    if (current->localCount >= UINT8_COUNT) {
        error("Too many local variables in function.");
        return;
    }

    Local* local = &current->locals[current->localCount++];
    local->name = name;
    local->depth = -1;
    local->isCaptured = false;
}

static bool identifiersEqual(Token* a, Token* b) {
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

static void declareVariable() {
    // Skip declaring globals
    if (current->scopeDepth == 0) return;

    Token* name = &parser.previous;
    // Look for variable declared with same name
    for (int i = current->localCount - 1; i >= 0; i--) {
        Local* local = &current->locals[i];
        // End if in previous scope
        // Depths are stored non-decreasing as i++
        if (local->depth != -1 && local->depth < current->scopeDepth) {
            break;
        }

        if (identifiersEqual(name, &local->name)) {
            error("Already a variable with this name in this scope.");
        }
    }
    addLocal(*name);
}

// Returns stack index of variable, assuming the stack contains only locals
static int resolveLocal(Compiler* compiler, Token* name) {
    for (int i = compiler->localCount - 1; i >= 0; i--) {
        Local* local = &compiler->locals[i];
        if (identifiersEqual(name, &local->name)) {
            if (local->depth == -1) {
                error("Local variable used before it was defined.");
            }
            return i;
        }
    }

    return -1;
}

static int addUpvalue(Compiler* compiler, uint8_t index, bool isLocal) {
    int upvalueCount = compiler->function->upvalueCount;

    for (int i = 0; i < upvalueCount; i++) {
        Upvalue* upvalue = &compiler->upvalues[i];
        if (upvalue->index == index && upvalue->isLocal == isLocal) {
            return i;
        }
    }

    if (upvalueCount == UINT8_COUNT) {
        error("Too many enclosed variables in function.");
        return 0;
    }

    compiler->upvalues[upvalueCount].isLocal = isLocal;
    compiler->upvalues[upvalueCount].index = index;
    return compiler->function->upvalueCount++;
}

// Recursively looks for an identifier in current or enclosing functions
static int resolveUpvalue(Compiler* compiler, Token* name) {
    if (compiler->enclosing == NULL) return -1;

    // Look for locals in enclosing scope
    int local = resolveLocal(compiler->enclosing, name);
    if (local != -1) {
        compiler->enclosing->locals[local].isCaptured = true;
        return addUpvalue(compiler, (uint8_t)local, true);
    }

    // Recursively do the same in the outer function
    int upvalue = resolveUpvalue(compiler->enclosing, name);
    if (upvalue != -1) {
        return addUpvalue(compiler, (uint8_t)upvalue, false);
    }

    // Not in this or in enclosing function
    return -1;
}

static void namedVariable(Token name, bool canAssign) {
    uint8_t getOp, setOp;
    int arg = resolveLocal(current, &name); // index to stack
    if (arg != -1) {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else if ((arg = resolveUpvalue(current, &name)) != -1) {
        getOp = OP_GET_UPVALUE;
        setOp = OP_SET_UPVALUE;
    } else {
        arg = identifierConstant(&name); // key to global map
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }

    if (canAssign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(setOp, arg);
    } else {
        emitBytes(getOp, arg);
    }
    #ifdef DEBUG_LOCAL_NAMES
        if (getOp == OP_GET_LOCAL || getOp == OP_GET_UPVALUE) {
            ObjString* obj = copyString(name.start, name.length);
            emitByte(makeConstant(OBJ_VAL(obj)));
        }
    #endif
}

static void variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}

// Tries to declare
static uint8_t parseVariable(const char* errorMessage) {
    consume(TOKEN_IDENTIFIER, errorMessage);

    declareVariable();
    if (current->scopeDepth > 0) return 0;

    return identifierConstant(&parser.previous);
}

// Sets the depth of the last variable declared
static void markInitialized() {
    // Skip marking global variables (functions & classes) as initialized,
    // we just kinda hope they are lol (resolved dynamically)
    if (current->scopeDepth == 0) return;

    // Otherwise, it's a local variable on the stack.
    // Before initialization, last.depth == -1
    Local* last = &current->locals[current->localCount - 1];
    last->depth = current->scopeDepth;
}

static void defineVariable(uint8_t global) {
    if (current->scopeDepth > 0) {
        markInitialized();
        return;
    }
    emitBytes(OP_DEFINE_GLOBAL, global);
}

static void varDeclaration() {
    uint8_t global = parseVariable("Expect variable name.");

    if (match(TOKEN_EQUAL)) {
        expression();
    } else {
        emitByte(OP_NIL);
    }
    defineVariable(global);
    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");
}

static void block() {
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void function(FunctionType type) {
    Compiler compiler;
    initCompiler(&compiler, type);
    beginScope();

    consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            current->function->arity++;
            if (current->function->arity > 255) {
                errorAtCurrent("Can't have more than 255 parameters.");
            }
            uint8_t constant = parseVariable("Expect parameter name.");
            defineVariable(constant);
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");

    consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
    block();

    // No endScope(), endCompiler effectively does that
    ObjFunction* function = endCompiler();
    emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));

    for (int i = 0; i < function->upvalueCount; i++) {
        emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
        emitByte(compiler.upvalues[i].index);
    }
}

static void funDeclaration() {
    uint8_t global = parseVariable("Expected function name.");
    markInitialized();
    function(TYPE_FUNCTION);
    defineVariable(global);
}

static void expressionStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    emitByte(OP_POP);
}

static void returnStatement() {
    // if (current->type == TYPE_SCRIPT) {
    //     error("Can't return from top-level code.");
    // }

    if (match(TOKEN_SEMICOLON)) {
        emitReturn();
    } else {
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
        emitByte(OP_RETURN);
    }
}

static void ifStatement() {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int thenJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP); // if true, no jump then pop the condition
    statement();

    int elseJump = emitJump(OP_JUMP);

    patchJump(thenJump);
    emitByte(OP_POP); // if false, jump to here, pop condition

    if (match(TOKEN_ELSE)) statement();
    patchJump(elseJump);
}

static void loopControlStatement() {
    Token keyword = parser.previous;
    if (current->curLoop == NULL) {
        error("Loop control statement must be inside a loop.");
        return;
    }
    switch (keyword.type) {
        case TOKEN_BREAK:
            addBreakStmt(emitJump(OP_JUMP));
            break;
        case TOKEN_CONTINUE:
            emitLoop(current->curLoop->start);
            break;
    }
    consume(TOKEN_SEMICOLON, "Expect ';' after loop control statement.");
}

static void initAndSetLoop(Loop* loop, int start) {
    loop->start = start;
    loop->breakStmts = NULL;
    loop->breakStmtCount = 0;
    loop->breakStmtCapacity = 0;
    current->curLoop = loop;
}

static void endLoop() {
    // it should never be null atp but i dont know anything anymore
    if (current->curLoop != NULL)
        FREE_ARRAY(int, current->curLoop->breakStmts, current->curLoop->breakStmtCapacity);
    // No need to free the loop itself, it's from stack memory
    current->curLoop = NULL;
}

static void whileStatement() {
    int loopStart = currentChunk()->count;
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);

    Loop newLoop;
    initAndSetLoop(&newLoop, loopStart);
    statement();

    emitLoop(loopStart);

    patchJump(exitJump);

    // Ensure all break statements jump to the loop end
    for (int i = 0; i < newLoop.breakStmtCount; i++) {
        patchJump(newLoop.breakStmts[i]);
    }
    endLoop();

    emitByte(OP_POP); // evaluated condition should be on top
}

static void forStatement() {
    beginScope();
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
    // Immediately parse & emit initializer code
    if (match(TOKEN_SEMICOLON)) {
        // No initializer.
    } else if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        expressionStatement();
    }

    // Record beginning of loop to include condition evaluation
    int loopStart = currentChunk()->count;
    int exitJump = -1;

    // Parse and write condition expression
    if (!match(TOKEN_SEMICOLON)) {
        expression(); // Push condition onto stack

        // Jump out of the loop if the condition is false
        exitJump = emitJump(OP_JUMP_IF_FALSE);
        emitByte(OP_POP); // pop condition if true, it will be pushed on next iteration

        consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");
    }

    // Increment handled differently,
    // compiled immediately but should be executed AFTER body
    int incrementStart = -1;
    if (!match(TOKEN_RIGHT_PAREN)) {
        // Immediately jump to body,
        int bodyJump = emitJump(OP_JUMP);
        incrementStart = currentChunk()->count; // points at next instruction
        expression(); // Evaluate increment
        emitByte(OP_POP);
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

        // Return to loop start
        emitLoop(loopStart);
        loopStart = incrementStart;
        patchJump(bodyJump); // Jump to body start
    }

    Loop newLoop;
    initAndSetLoop(&newLoop, loopStart); // loopStart = incrementStart if it exists
    statement();
    emitLoop(loopStart);

    // Patch exit jump if defined (there's a condition)
    if (exitJump != -1) {
        patchJump(exitJump);
        emitByte(OP_POP); // Pop condition after loop ends
    }

    // Break statements are patched anyway
    for (int i = 0; i < newLoop.breakStmtCount; i++) {
        patchJump(newLoop.breakStmts[i]);
    }
    endLoop();
    endScope();
}

static void parsePrecedence(Precedence precedence) {
    advance();
    ParseFn prefixRule = getRule(parser.previous.type)->prefix;
    if (prefixRule == NULL) {
        error("Expect expression.");
        return;
    }
    bool canAssign = precedence <= PREC_ASSIGNMENT;
    prefixRule(canAssign);
    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule(canAssign);
    }

    // equal sign was not consumed earlier
    if (canAssign && match(TOKEN_EQUAL)) {
        error("Invalid assignment target.");
    }
}

static void synchronize() {
    parser.panicMode = false;

    // Continue until (probably) start of next statement
    while (parser.current.type != TOKEN_EOF) {
        if (parser.previous.type == TOKEN_SEMICOLON) return;
        switch (parser.current.type) {
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_RETURN:
            case TOKEN_BREAK:
            case TOKEN_CONTINUE:
                return;

            default: break; // Keep going
        }

        advance();
    }
}

static void declaration() {
    if (match(TOKEN_FUN)) {
        funDeclaration();
    } else if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        statement();
    }

    if (parser.panicMode) synchronize();
}

static void statement() {
    if (match(TOKEN_RETURN)) {
        returnStatement();
    } else if (match(TOKEN_BREAK) || match(TOKEN_CONTINUE)) {
        loopControlStatement();
    }  else if (match(TOKEN_IF)) {
        ifStatement();
    } else if (match(TOKEN_WHILE)) {
        whileStatement();
    } else if (match(TOKEN_FOR)) {
        forStatement();
    } else if (match(TOKEN_LEFT_BRACE)) {
        beginScope();
        block();
        endScope();
    } else {
        expressionStatement();
    }
}

ObjFunction* compile(const char* source) {
    initScanner(source);
    Compiler compiler;
    initCompiler(&compiler, TYPE_SCRIPT);

    parser.hadError = false;
    parser.panicMode = false;

    advance();

    while (!match(TOKEN_EOF)) {
        declaration();
    }

    ObjFunction* function = endCompiler();
    return parser.hadError ? NULL : function;
}

void markCompilerRoots() {
    Compiler* compiler = current;
    while (compiler != NULL) {
        markObject((Obj*)compiler->function);
        compiler = compiler->enclosing;
    }
}

ParseRule rules[] = {
    [TOKEN_LEFT_PAREN]    = {grouping, call,   PREC_CALL},
    [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
    [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE},
    [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
    [TOKEN_LEFT_BRACKET]  = {list,     indexOp,  PREC_CALL},
    [TOKEN_RIGHT_BRACKET] = {NULL,     NULL,   PREC_NONE},
    [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_DOT]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
    [TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},
    [TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE},
    [TOKEN_FSLASH]         = {NULL,    binary, PREC_FACTOR},
    [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
    [TOKEN_PERCENT]       = {NULL,     binary, PREC_FACTOR},
    [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE},
    [TOKEN_EQUAL]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY},
    [TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_EQUALITY},
    [TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE},
    [TOKEN_STRING]        = {string,   NULL,   PREC_NONE},
    [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE},
    [TOKEN_AND]           = {NULL,     and_,   PREC_AND},
    [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
    [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
    [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
    [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
    [TOKEN_OR]            = {NULL,     or_,    PREC_OR},
    [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
    [TOKEN_SUPER]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_THIS]          = {NULL,     NULL,   PREC_NONE},
    [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
    [TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
};