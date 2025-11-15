#ifndef clox_debug_h
#define clox_debug_h

#include "chunk.h"

void disassembleChunk(Chunk* chunk, const char* name);
void instructionLineDesc();
char* getOpName(OpCode instruction);
int disassembleInstruction(const Chunk* chunk, int offset);

#endif