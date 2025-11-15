#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

typedef enum {
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_POP,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_MODULO,
    OP_NOT,
    OP_NEGATE,
    OP_CLOSE_UPVALUE,
    OP_RETURN,

    OP_CONSTANT,
    OP_GET_GLOBAL,
    OP_SET_GLOBAL,
    OP_DEFINE_GLOBAL,
    OP_JUMP,
    OP_JUMP_IF_FALSE,
    OP_JUMP_BACK,
    OP_CALL,
    OP_INDEX_GET,
    OP_INDEX_SET,
    OP_LIST,

    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_GET_UPVALUE,
    OP_SET_UPVALUE,

    OP_CLOSURE,
} OpCode;

typedef struct {
    int line; // Lines are always increasing but not necessarily by 1; must store
    int count;
} RLELine;

typedef struct {
    int count;
    int capacity;
    uint8_t* code;
    ValueArray constants;

    int uniqueLineCount;
    int uniqueLineCapacity;
    RLELine* lines; // stored separately from instructions, should reduce cpu cache misses
} Chunk;

void initChunk(Chunk* chunk);
void freeChunk(Chunk* chunk);
void shrinkChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte, int line);
int getLine(const Chunk* chunk, size_t instructionIndex);
int addConstant(Chunk* chunk, Value value);
Value getConstant(const Chunk* chunk, uint8_t constantIndex);

#endif
