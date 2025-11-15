#include <stdio.h>

#include "debug.h"
#include "object.h"
#include "value.h"

void disassembleChunk(Chunk* chunk, const char* name) {
    printf("==== %s ====\n", name);

    instructionLineDesc();
    for (int offset = 0; offset < chunk->count;) {
        offset = disassembleInstruction(chunk, offset);
    }
}

void instructionLineDesc() {
    printf(" %-3s %4s %-18s %4s\n", "IP", "Ln", "Opcode", "Arguments");
}

static int constantInstruction(const char* locFormat, const Chunk* chunk, int offset) {
    uint8_t constantLoc = chunk->code[offset + 1];
    printf(locFormat, constantLoc);
    printValueFancy(getConstant(chunk, constantLoc));
    printf("\n");
    return offset + 2; // 1 (Instruction byte) + 1 (argument byte)
}

static int jumpInstruction(int sign, const Chunk* chunk, int offset) {
    uint16_t jump = (uint16_t)(chunk->code[offset + 1] << 8);
    jump |= chunk->code[offset + 2];
    printf("to 0x%02X\n", offset + 3 + jump*sign);
    return offset + 3;
}

static int byteInstruction(const char* param, const Chunk* chunk, int offset) {
    uint8_t byte = chunk->code[offset + 1];
    printf("%s%d\n", param, byte);
    return offset + 2;
}

static int shortInstruction(const char* param, const Chunk* chunk, int offset) {
    uint16_t byte = chunk->code[offset + 1] << 8 | chunk->code[offset + 2];
    printf("%s%d\n", param, byte);
    return offset + 3;
}

static int localVariableInstruction(const Chunk* chunk, int offset) {
    uint8_t slot = chunk->code[offset + 1];
    uint8_t varNameLoc = chunk->code[offset + 2];
    printf("var '");
    printValue(getConstant(chunk, varNameLoc));
    printf("' @ stack[%d]\n", slot);
    return offset + 3;
}

static int closureInstruction(const Chunk* chunk, int offset) {
    offset++;
    uint8_t constant = chunk->code[offset++];
    printf("%d ", constant);
    printValue(chunk->constants.values[constant]);
    printf("\n");

    ObjFunction* function = AS_FUNCTION(chunk->constants.values[constant]);
    for (int j = 0; j < function->upvalueCount; j++) {
        int isLocal = chunk->code[offset++];
        int index = chunk->code[offset++];
        printf("%04d    |      load %s %d\n",
               offset - 2, isLocal ? "local" : "upvalue", index);
    }
    return offset;
}

char* getOpName(OpCode instruction) {
    switch (instruction) {
        CASE_RETURN_STR(OP_NIL);
        CASE_RETURN_STR(OP_TRUE);
        CASE_RETURN_STR(OP_FALSE);
        CASE_RETURN_STR(OP_POP);
        CASE_RETURN_STR(OP_EQUAL);
        CASE_RETURN_STR(OP_GREATER);
        CASE_RETURN_STR(OP_LESS);
        CASE_RETURN_STR(OP_ADD);
        CASE_RETURN_STR(OP_SUBTRACT);
        CASE_RETURN_STR(OP_MULTIPLY);
        CASE_RETURN_STR(OP_DIVIDE);
        CASE_RETURN_STR(OP_MODULO);
        CASE_RETURN_STR(OP_NOT);
        CASE_RETURN_STR(OP_NEGATE);
        CASE_RETURN_STR(OP_RETURN);
        CASE_RETURN_STR(OP_CONSTANT);
        CASE_RETURN_STR(OP_GET_GLOBAL);
        CASE_RETURN_STR(OP_SET_GLOBAL);
        CASE_RETURN_STR(OP_DEFINE_GLOBAL);
        CASE_RETURN_STR(OP_JUMP);
        CASE_RETURN_STR(OP_JUMP_IF_FALSE);
        CASE_RETURN_STR(OP_JUMP_BACK);
        CASE_RETURN_STR(OP_GET_LOCAL);
        CASE_RETURN_STR(OP_SET_LOCAL);
        CASE_RETURN_STR(OP_GET_UPVALUE);
        CASE_RETURN_STR(OP_SET_UPVALUE);
        CASE_RETURN_STR(OP_INDEX_GET);
        CASE_RETURN_STR(OP_INDEX_SET);
        CASE_RETURN_STR(OP_CALL);
        CASE_RETURN_STR(OP_LIST);
        CASE_RETURN_STR(OP_CLOSURE);
        CASE_RETURN_STR(OP_CLOSE_UPVALUE);
    }
    fprintf(stderr, "getOpName(instruction) is not complete!\n");
    return "OP_???";
}
#undef CASE_RETURN_STR

int disassembleInstruction(const Chunk* chunk, int offset) {
    printf("%04X ", offset);
    const int thisLine = getLine(chunk, offset);
    if (offset > 0 && thisLine == getLine(chunk, offset-1))
        printf("   | ");
    else printf("%4d ", thisLine);

    const uint8_t instruction = chunk->code[offset];
    const char* name = getOpName(instruction) + 3;
    printf("%02X: %-18s", instruction, name);
    switch ((OpCode)instruction) {
        case OP_NIL:
        case OP_TRUE:
        case OP_FALSE:
        case OP_POP:
        case OP_EQUAL:
        case OP_GREATER:
        case OP_LESS:
        case OP_ADD:
        case OP_SUBTRACT:
        case OP_MULTIPLY:
        case OP_DIVIDE:
        case OP_MODULO:
        case OP_NEGATE:
        case OP_NOT:
        case OP_RETURN:
        case OP_CLOSE_UPVALUE:
        case OP_INDEX_GET:
        case OP_INDEX_SET:
            printf("\n");
            return offset + 1;

        case OP_LIST:
            return shortInstruction("size=", chunk, offset);

        case OP_SET_GLOBAL:
        case OP_DEFINE_GLOBAL:
            return constantInstruction("use const[%d]=", chunk, offset);
        case OP_GET_GLOBAL:
        case OP_CONSTANT:
            return constantInstruction("push const[%d]=", chunk, offset);

        case OP_JUMP:
        case OP_JUMP_IF_FALSE:
            return jumpInstruction(1, chunk, offset);
        case OP_JUMP_BACK:
            return jumpInstruction(-1, chunk, offset);

        #ifdef DEBUG_LOCAL_NAMES
            case OP_SET_LOCAL:
                printf("set and ");
            case OP_GET_LOCAL:
                printf("push ");
                return localVariableInstruction(chunk, offset);
        #else
            case OP_GET_LOCAL:
            case OP_SET_LOCAL:
                return byteInstruction("index=", chunk, offset);
        #endif

        case OP_CALL:
            return byteInstruction("argc=", chunk, offset);
        case OP_GET_UPVALUE:
        case OP_SET_UPVALUE:
            #ifdef DEBUG_LOCAL_NAMES
                return byteInstruction("upvalue @ ", chunk, offset) + 1;
            #else
                return byteInstruction("upvalue @ ", chunk, offset);
            #endif

        case OP_CLOSURE: {
            return closureInstruction(chunk, offset);
        }
    }
    printf(" ?? Opcode 0x%02X\n", instruction);
    return offset + 1;
}