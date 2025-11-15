#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include <limits.h>
#include <stdlib.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "vm.h"
#include "memory.h"
#include "object.h"

VM vm;

void push(const Value value) {
    *vm.stackTop = value;
    vm.stackTop++;
}

Value pop() {
    vm.stackTop--;
    return *vm.stackTop;
}

static Value peek(int distance) {
    return vm.stackTop[-1 - distance];
}

static void defineNative(const char* name, const NativeFn function) {
    // copy, not take bc we need to free the name
    push(OBJ_VAL(copyString(name, (int)strlen(name)))); // so it isnt freed during gc
    push(OBJ_VAL(newNative(function, name)));           // see ^
    tableSet(&vm.globals, AS_STRING(peek(1)), peek(0));
    pop();  // function
    pop();  // the ObjString name
}

static void resetStack() {
    vm.stackTop = vm.stack;
    vm.frameCount = 0;
    vm.openUpvalues = NULL;
}

static void runtimeError(const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);

    for (int i = vm.frameCount - 1; i >= 0; i--) {
        CallFrame* frame = &vm.frames[i];
        ObjFunction* function = frame->closure->function;
        size_t instruction = frame->ip - function->chunk.code - 1;
        fprintf(stderr, "\tat line %d ", getLine(&function->chunk, instruction));
        if (function->name == NULL) {
            fprintf(stderr, "main script\n");
        } else {
            fprintf(stderr, "%s()\n", function->name->chars);
        }
    }

    resetStack();
}

static const char* getTypeName(Value value) {
    return IS_OBJ(value)
        ? getObjName(OBJ_TYPE(value))
        : getValName(value.type);
}

static bool assertArgCount(int expected, int argCount) {
    if (argCount != expected) {
        runtimeError("Expected %d arguments but got %d.", expected, argCount);
        return false;
    }
    return true;
}

static bool assertType(ValueType expected, Value given) {
    if (expected != given.type) {
        runtimeError("Expected an argument of type %s but got one of type %s.",
            getValName(expected) + 4, getValName(given.type) + 4);
        // + 4 to skip 'VAL_' prefix
        return false;
    }
    return true;
}

static bool assertObjType(ObjType expected, Value given) {
    if (!IS_OBJ(given) || expected != OBJ_TYPE(given)) {
        const char *givenType = getTypeName(given);
        runtimeError("Expected an object argument of type %s but got one of type %s.",
            getObjName(expected) + 4, givenType + 4);
        // + 4 to skip 'OBJ_' or 'VAL_' prefix
        return false;
    }
    return true;
}

static Value clockNative(int argCount, Value* argsIgnored) {
    assertArgCount(0, argCount);
    return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

static Value printNative(int argCount, Value* args) {
    if (argCount == 0) assertArgCount(1, argCount);
    printValue(args[0]);
    for (int i = 1; i < argCount; i++) {
        printValue(args[i]);
    }
    return NIL_VAL;
}

static Value printLnNative(int argCount, Value* args) {
    if (argCount > 0)
        printNative(argCount, args);
    printf("\n");
    return NIL_VAL;
}

static Value randNative(int argCount, Value* argsIgnored) {
    assertArgCount(0, argCount);
    return NUMBER_VAL((double) rand() / RAND_MAX);
}

static Value sqrtNative(int argCount, Value* args) {
    assertArgCount(1, argCount);
    assertType(VAL_NUMBER, args[0]);
    return NUMBER_VAL(sqrt(AS_NUMBER(args[0])));
}

static Value listLengthNative(int argCount, Value* args) {
    assertArgCount(1, argCount);
    Value arg1 = args[0];
    assertObjType(OBJ_LIST, arg1);
    int len = AS_LIST(arg1)->array.count;
    return NUMBER_VAL(len);
}

void initVM() {
    resetStack();
    vm.objects = NULL;

    // GC fields
    vm.bytesAllocated = 0;
    vm.nextGC = 1024 * 1024;
    vm.unfinishedCount = 0;
    vm.unfinishedCapacity = 0;
    vm.unfinishedStack = NULL;

    initTable(&vm.globals);
    initTable(&vm.strings);

    srand((unsigned int)time(NULL));
    defineNative("clock", clockNative);
    defineNative("print", printNative);
    defineNative("println", printLnNative);
    defineNative("rand", randNative);
    defineNative("len", listLengthNative);
    defineNative("sqrt", sqrtNative);
}

void endVM() {
#ifdef DEBUG_SUMMARIZE_VM
    printf("==== Post-execution data ====\n");
    printf("  Interned strings (excluding natives):\n");
    int i = 0;
    Value temp;
    for (Entry* e = vm.strings.entries; i < vm.strings.capacity; e++, i++) {
        if (e->key == NULL) continue;
        tableGet(&vm.globals, e->key, &temp);
        if (IS_NATIVE(temp)) continue;
        printf("  - \"%s\",\n", e->key->chars);
    }

    printf("  Globals table:\n");
    i = 0;
    int builtinCount = 0;
    for (Entry* e = vm.globals.entries; i < vm.globals.capacity; e++, i++) {
        if (e->key == NULL) continue;
        if (IS_NATIVE(e->value)) {
            builtinCount++;
            continue;
        }
        printf("  - \"%s\": ", e->key->chars);
        printValue(e->value);
        printf(",\n");
    }
    printf("  - %d native functions\n", builtinCount);
#endif
    freeObjects();

    freeTable(&vm.globals);
    freeTable(&vm.strings);
#ifdef DEBUG_SUMMARIZE_VM
    printf("  %llu bytes still allocated after freeing.\n", vm.bytesAllocated);
    printf("==== (data end) ====\n");
#endif
}

static bool isFalsey(Value value) {
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void concatenate() {
    ObjString* b = AS_STRING(pop());
    ObjString* a = AS_STRING(pop());

    int length = a->length + b->length;
    char* chars = ALLOCATE(char, length + 1);
    memcpy(chars, a->chars, a->length);
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = '\0';

    // Use the newly created string in the new object instead of copying
    ObjString* result = takeString(chars, length);
    push(OBJ_VAL(result));
}

// returns whether the call was successful
static bool call(ObjClosure* closure, int argCount) {
    ObjFunction* function = closure->function;
    if (!assertArgCount(function->arity, argCount))
        return false;

    if (vm.frameCount >= FRAMES_MAX) {
        runtimeError("Stack overflow.");
        return false;
    }

    CallFrame* frame = &vm.frames[vm.frameCount++];
    frame->closure = closure;
    frame->ip = function->chunk.code;
    frame->slots = vm.stackTop - argCount - 1;
    return true;
}

// returns whether the call was successful
static bool callValue(const Value callee, int argCount) {
    if (IS_OBJ(callee)) {
        switch (OBJ_TYPE(callee)) {
            case OBJ_CLOSURE:
#ifdef DEBUG_TRACE_EXECUTION
                printf("        Entering--> ");
                printValue(callee);
                printf("()\n");
#endif
                return call(AS_CLOSURE(callee), argCount);
            case OBJ_NATIVE: {
                NativeFn native = AS_NATIVE(callee);
                Value result = native(argCount, vm.stackTop - argCount);
                vm.stackTop -= argCount + 1;
                push(result);
                return true;
            }
            // Non-callable object, fall through to error
            default: break;
        }
    }
    runtimeError("Can only call functions or classes.");
    return false;
}

static ObjUpvalue* captureUpvalue(Value* local) {
    ObjUpvalue* prevUpvalue = NULL;
    ObjUpvalue* upvalue = vm.openUpvalues;
    while (upvalue != NULL && upvalue->location > local) {
        prevUpvalue = upvalue;
        upvalue = upvalue->next;
    }

    if (upvalue != NULL && upvalue->location == local) {
        return upvalue;
    }

    ObjUpvalue* createdUpvalue = newUpvalue(local);
    createdUpvalue->next = upvalue;

    if (prevUpvalue == NULL) {
        vm.openUpvalues = createdUpvalue;
    } else {
        prevUpvalue->next = createdUpvalue;
    }
    return createdUpvalue;
}

static void closeUpvalues(Value* last) {
    while (vm.openUpvalues != NULL && vm.openUpvalues->location >= last) {
        ObjUpvalue* upvalue = vm.openUpvalues;
        upvalue->closed = *upvalue->location;
        upvalue->location = &upvalue->closed;
        vm.openUpvalues = upvalue->next;
    }
}

static InterpretResult run() {

    CallFrame* frame = &vm.frames[vm.frameCount - 1];

    #define READ_BYTE() (*frame->ip++)
    #define READ_SHORT() (frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))
    #define READ_CONSTANT() (frame->closure->function->chunk.constants.values[READ_BYTE()])
    #define READ_STRING() AS_STRING(READ_CONSTANT())
    #define NUM_BINARY_OP(valueType, op) {                \
        if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) { \
            runtimeError("Operands must be numbers.");    \
            return INTERPRET_RUNTIME_ERROR;               \
        }                                                 \
        double b = AS_NUMBER(pop());                      \
        double a = AS_NUMBER(pop());                      \
        push(valueType(a op b));                          \
    }
    #define NUM_BINARY_FUNC(valueType, func) {            \
        if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) { \
            runtimeError("Operands must be numbers.");    \
            return INTERPRET_RUNTIME_ERROR;               \
        }                                                 \
        double b = AS_NUMBER(pop());                      \
        double a = AS_NUMBER(pop());                      \
        push(valueType(func(a, b)));                      \
    }

    #ifdef DEBUG_TRACE_EXECUTION
        printf("==== Virtual Machine Started ====\n");
        instructionLineDesc();
    #endif
    while (true) {
        #ifdef DEBUG_TRACE_EXECUTION
            for (int i = 1; i < vm.frameCount; i++)
                printf("  ");
            // Display the instruction to be executed
            // disassembleInstruction takes an offset, subtract beginning ptr
            disassembleInstruction(&frame->closure->function->chunk,
                (int)(frame->ip - frame->closure->function->chunk.code));
        #endif
        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
            case OP_CONSTANT: push(READ_CONSTANT()); break;
            case OP_NIL:      push(NIL_VAL); break;
            case OP_TRUE:     push(BOOL_VAL(true)); break;
            case OP_FALSE:    push(BOOL_VAL(false)); break;
            case OP_POP: pop(); break;
            case OP_GET_LOCAL: {
                // Push value of local to top]
                uint8_t slot = READ_BYTE();
                push(frame->slots[slot]);
                #ifdef DEBUG_LOCAL_NAMES
                    READ_BYTE();
                #endif
                break;
            }
            case OP_SET_LOCAL: {
                // Replace value of local, leave assigned value on top as expression
                uint8_t slot = READ_BYTE();
                frame->slots[slot] = peek(0);
                #ifdef DEBUG_LOCAL_NAMES
                    READ_BYTE();
                #endif
                break;
            }
            case OP_GET_GLOBAL: {
                ObjString* name = READ_STRING();
                Value value;
                if (!tableGet(&vm.globals, name, &value)) {
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(value);
                break;
            }
            case OP_SET_GLOBAL: {
                ObjString* name = READ_STRING();
                if (tableSet(&vm.globals, name, peek(0))) {
                    tableDelete(&vm.globals, name);
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                // Don't pop new value, assignment treated as an expression statement
                break;
            }
            case OP_DEFINE_GLOBAL: {
                ObjString* name = READ_STRING();
                tableSet(&vm.globals, name, peek(0));
                // Keep value on stack bc evil garbage collector
                pop(); // value
                break;
            }
            case OP_GET_UPVALUE: {
                uint8_t slot = READ_BYTE();
                push(*frame->closure->upvalues[slot]->location);
                #ifdef DEBUG_LOCAL_NAMES
                    READ_BYTE();
                #endif
                break;
            }
            case OP_SET_UPVALUE: {
                uint8_t slot = READ_BYTE();
                *frame->closure->upvalues[slot]->location = peek(0);
                // again, no popping because setting is an expression
                #ifdef DEBUG_LOCAL_NAMES
                    READ_BYTE();
                #endif
                break;
            }
            case OP_EQUAL: {
                Value b = pop();
                Value a = pop();
                push(BOOL_VAL(valuesEqual(a, b)));
                break;
            }
            case OP_GREATER:  NUM_BINARY_OP(BOOL_VAL, >); break;
            case OP_LESS:     NUM_BINARY_OP(BOOL_VAL, <); break;
            case OP_ADD: {
                if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
                    concatenate();
                } else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
                    double b = AS_NUMBER(pop());
                    double a = AS_NUMBER(pop());
                    push(NUMBER_VAL(a + b));
                } else if (IS_LIST(peek(0)) && IS_LIST(peek(0))) {

                    // Peek first then pop at the end to avoid the garbage collector
                    ValueArray arr2 = AS_LIST(peek(0))->array;
                    ValueArray arr1 = AS_LIST(peek(1))->array;

                    ValueArray newArr;
                    initValueArray(&newArr);
                    newArr.values = ALLOCATE(Value, arr1.count + arr2.count);
                    memcpy(newArr.values, arr1.values,sizeof(Value) * arr1.count);
                    memcpy(newArr.values + arr1.count, arr2.values, sizeof(Value) * arr2.count);
                    newArr.count = arr1.count + arr2.count;
                    newArr.capacity = newArr.count;

                    // magnitude
                    pop();
                    pop();
                    push(OBJ_VAL(newList(&newArr)));

                } else {
                    runtimeError("Can't add operands of types %s and %s.",
                        getTypeName(peek(1)), getTypeName(peek(0)));
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_SUBTRACT: NUM_BINARY_OP(NUMBER_VAL, -); break;
            case OP_MULTIPLY: NUM_BINARY_OP(NUMBER_VAL, *); break;
            case OP_DIVIDE:   NUM_BINARY_OP(NUMBER_VAL, /); break;
            case OP_MODULO:   NUM_BINARY_FUNC(NUMBER_VAL, fmod); break;
            case OP_NOT: push(BOOL_VAL(isFalsey(pop()))); break;
            case OP_NEGATE:
                if (!IS_NUMBER(peek(0))) {
                    runtimeError("Operand must be a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(NUMBER_VAL(-AS_NUMBER(pop())));
                break;
            case OP_JUMP: {
                uint16_t offset = READ_SHORT();
                frame->ip += offset;
                break;
            }
            case OP_JUMP_IF_FALSE: {
                uint16_t offset = READ_SHORT();
                if (isFalsey(peek(0))) frame->ip += offset;
                break;
            }
            case OP_JUMP_BACK: {
                uint16_t offset = READ_SHORT();
                frame->ip -= offset;
                break;
            }
            case OP_CALL: {
                int argCount = READ_BYTE();
                if (!callValue(peek(argCount), argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm.frames[vm.frameCount - 1];
                break;
            }
            case OP_CLOSURE: {
                ObjFunction* function = AS_FUNCTION(READ_CONSTANT());
                ObjClosure* closure = newClosure(function);
                push(OBJ_VAL(closure));
                for (int i = 0; i < closure->upvalueCount; i++) {
                    uint8_t isLocal = READ_BYTE();
                    uint8_t index = READ_BYTE();
                    if (isLocal) {
                        closure->upvalues[i] = captureUpvalue(frame->slots + index);
                    } else {
                        closure->upvalues[i] = frame->closure->upvalues[index];
                    }
                }
                break;
            }
            case OP_LIST: {
                uint16_t arrLength = READ_SHORT();

                ValueArray arr;
                initValueArray(&arr);
                if (arrLength > 0) {
                    arr.values = GROW_ARRAY(Value, NULL, 0, arrLength);
                    arr.capacity = arrLength;
                    for (int i = 0; i < arrLength; i++)
                        arr.values[arrLength - 1 - i] = pop();
                    arr.count = arrLength;
                }

                ObjList* list = newList(&arr);
                push(OBJ_VAL(list));
                break;
            }
            case OP_INDEX_SET: {
                if (!(IS_LIST(peek(2))) || !IS_NUMBER(peek(1))) {
                    runtimeError("Can only index a list with a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                Value newVal = pop();

                double indexVal = AS_NUMBER(pop());
                size_t index = (size_t) round(indexVal);
                // Check if value is close enough to itself rounded
                if (!numbersEqual(index, indexVal)) {
                    runtimeError("Can only index using a whole number (0, 1, 2...).");
                    return INTERPRET_RUNTIME_ERROR;
                }

                const ObjList* list = AS_LIST(pop());
                if (index >= list->array.count) {
                    runtimeError("Index [%d] out of bounds.", index);
                    return INTERPRET_RUNTIME_ERROR;
                }

                list->array.values[index] = newVal;
                push(newVal); // assignment is an expression as usual
                break;
            }
            case OP_INDEX_GET: {
                if (!(IS_LIST(peek(1))) || !IS_NUMBER(peek(0))) {
                    runtimeError("Can only index a list with a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                double indexVal = AS_NUMBER(pop());
                size_t index = (size_t) round(indexVal);
                // Check if value is close enough to itself rounded
                if (!numbersEqual(index, indexVal)) {
                    runtimeError("Can only index using a whole number (0, 1, 2...).");
                    return INTERPRET_RUNTIME_ERROR;
                }

                const ObjList* list = AS_LIST(pop());
                if (index >= list->array.count) {
                    runtimeError("Index [%d] out of bounds.", index);
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(list->array.values[index]);
                break;
            }
            case OP_CLOSE_UPVALUE:
                closeUpvalues(vm.stackTop - 1);
                pop();
                break;
            case OP_RETURN: {
                #ifdef DEBUG_TRACE_EXECUTION
                    printf("        <--Leaving ");
                    printValue(frame->slots[0]);
                    printf("()\n");
                #endif
                Value result = pop();
                closeUpvalues(frame->slots);
                vm.frameCount--;
                if (vm.frameCount == 0) {
                    // Exit interpreter
                    pop();
                    return INTERPRET_OK;
                }

                vm.stackTop = frame->slots;
                push(result);
                frame = &vm.frames[vm.frameCount - 1];
                break;
            }
            default: printf("<VM: Unknown instruction 0x%02X encountered>\n", instruction);
        }
        #ifdef DEBUG_TRACE_EXECUTION
            // Display the stack after the last instruction
            if (vm.stackTop == vm.stack) {
                // printf("...");
            } else {
                for (int i = 1; i < vm.frameCount; i++)
                    printf("  ");
                printf("        | [");
                if (vm.frameCount > 1) {
                    printf("<- %lld... ", frame->slots - vm.stack);
                }
                for (Value* val = frame->slots; val < vm.stackTop; val++) {
                    printValue(*val);
                    if (val + 1 < vm.stackTop) printf(", ");
                }
                printf("] <-\n");
            }
        #endif
    }
    #undef READ_BYTE
    #undef READ_SHORT
    #undef READ_CONSTANT
    #undef READ_STRING
    #undef NUM_BINARY_OP
    #undef NUM_BINARY_FUNC
}

InterpretResult interpret(const char* source) {
    ObjFunction* function = compile(source);
    if (function == NULL) return INTERPRET_COMPILE_ERROR;

    push(OBJ_VAL(function));
    ObjClosure* closure = newClosure(function);
    pop();
    push(OBJ_VAL(closure));
    call(closure, 0);

    #ifdef DEBUG_SUMMARIZE_VM
        size_t start = clock();
    #endif
    InterpretResult result = run();

    #ifdef DEBUG_SUMMARIZE_VM
    size_t end = clock();
    double elapsed = (double) (end - start) / CLOCKS_PER_SEC;
    printf("==== Execution end ====\n");
    printf("  Execution time elapsed: %gs\n", elapsed);
    #endif

    return result;
}