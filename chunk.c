#include <stdlib.h>

#include "chunk.h"

#include <stdio.h>

#include "memory.h"
#include "vm.h"

void initChunk(Chunk* chunk) {
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->code = NULL;

    chunk->lines = NULL;
    chunk->uniqueLineCount = 0;
    chunk->uniqueLineCapacity = 0;
    initValueArray(&chunk->constants);
}

void freeChunk(Chunk* chunk) {
    FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
    FREE_ARRAY(RLELine*, chunk->lines, chunk->capacity);
    freeValueArray(&chunk->constants);
    initChunk(chunk);
}

void shrinkChunk(Chunk* chunk) {
    chunk->code = GROW_ARRAY(uint8_t, chunk->code, chunk->capacity, chunk->count);
}

void writeChunk(Chunk* chunk, uint8_t byte, int line) {
    if (chunk->capacity < chunk->count + 1) {
        const int oldCapacity = chunk->capacity;
        chunk->capacity = GROW_CAPACITY(oldCapacity);
        chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);
    }

    // Eagerly resize array even though a new RLELine might not be added in this call
    if (chunk->uniqueLineCapacity < chunk->uniqueLineCount + 1) {
        const int oldCapacity = chunk->uniqueLineCapacity;
        chunk->uniqueLineCapacity = GROW_CAPACITY(oldCapacity);
        chunk->lines = GROW_ARRAY(RLELine, chunk->lines, oldCapacity, chunk->uniqueLineCapacity);
    }

    chunk->code[chunk->count] = byte;
    chunk->count++;

    // First instruction
    if (chunk->uniqueLineCount == 0) {
        const RLELine newLine = {.line = line, .count = 1};
        chunk->lines[0] = newLine;
        chunk->uniqueLineCount++;
        return;
    }

    // Assume that lines are written in non-decreasing order,
    // which guarantees that identical lines are consecutive
    int lastIndex = chunk->uniqueLineCount - 1;
    if (line == chunk->lines[lastIndex].line)
        chunk->lines[lastIndex].count++;
    else {
        const RLELine newLine = {.line = line, .count = 1};
        chunk->lines[lastIndex + 1] = newLine;
        chunk->uniqueLineCount++;
    }
}

int getLine(const Chunk* chunk, size_t instructionIndex) {
    if (instructionIndex == 0)
        return chunk->lines[0].line;

    int opCodesPassed = 0;
    RLELine info;
    for (int i = 0; i < chunk->uniqueLineCount; i++) {
        info = chunk->lines[i];
        opCodesPassed += info.count;
        if (opCodesPassed > instructionIndex)
            return info.line;
    }
    if (instructionIndex >= opCodesPassed)
        return -1;
    return chunk->lines[chunk->uniqueLineCount - 1].line;
}

int addConstant(Chunk* chunk, Value value) {
    push(value);
    writeValueArray(&chunk->constants, value);
    pop();
    return chunk->constants.count - 1; // <-- index of constant in the chunk?
}

Value getConstant(const Chunk* chunk, const uint8_t constantIndex) {
    return chunk->constants.values[constantIndex];
}