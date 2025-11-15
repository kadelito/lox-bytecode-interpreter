#ifndef clox_vm_h
#define clox_vm_h

#include "object.h"
#include "table.h"

#define FRAMES_MAX 64
// allow for enough space for each call
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

typedef struct {
    ObjClosure* closure;
    uint8_t* ip;
    // bottom of function's stack.
    // points into the VM's value stack
    Value* slots;
} CallFrame; // represents an ongoing function call

typedef struct {
    CallFrame frames[FRAMES_MAX];
    int frameCount;

    Value stack[STACK_MAX];
    Value* stackTop;// points at NEXT element, NOT last

    Table strings;  // for string interning, not modified during runtime
    ObjUpvalue* openUpvalues;
    Table globals;

    size_t bytesAllocated;
    size_t nextGC;
    // All objects created are in this linked list,
    // allows for freeing all of them at program end
    Obj* objects;

    int unfinishedCount;
    int unfinishedCapacity;
    // Stack of marked objects whose children aren't marked
    Obj** unfinishedStack;
} VM;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

extern VM vm;

void initVM();
void endVM();
InterpretResult interpret(const char* source);
void push(Value value);
Value pop();

#endif