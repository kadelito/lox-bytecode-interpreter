#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType) (type*)allocateObject(sizeof(type), objectType)

static Obj* allocateObject(size_t size, ObjType type) {
    Obj* object = reallocate(NULL, 0, size);
    object->type = type;
    object->isMarked = false;
    object->next = vm.objects;
    vm.objects = object;
    return object;
}

const char* getObjName(ObjType type) {
    switch (type) {
        CASE_RETURN_STR(OBJ_LIST);
        CASE_RETURN_STR(OBJ_CLOSURE);
        CASE_RETURN_STR(OBJ_FUNCTION);
        CASE_RETURN_STR(OBJ_NATIVE);
        CASE_RETURN_STR(OBJ_STRING);
        CASE_RETURN_STR(OBJ_UPVALUE);
    }
}

ObjList* newList(ValueArray* array) {
    ObjList* list = ALLOCATE_OBJ(ObjList, OBJ_LIST);
    list->array = *array;
    return list;
}

ObjClosure* newClosure(ObjFunction* function) {
    ObjUpvalue** upvalues = ALLOCATE(ObjUpvalue*, function->upvalueCount);
    for (int i = 0; i < function->upvalueCount; i++) {
        upvalues[i] = NULL;
    }
    ObjClosure* closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
    closure->function = function;
    closure->upvalues = upvalues;
    closure->upvalueCount = function->upvalueCount;
    return closure;
}

ObjFunction* newFunction() {
    ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
    function->arity = 0;
    function->upvalueCount = 0;
    function->name = NULL;
    initChunk(&function->chunk);
    return function;
}

ObjNative* newNative(NativeFn function, const char* name) {
    ObjNative* native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
    native->function = function;
    native->name = name;
    return native;
}

ObjUpvalue* newUpvalue(Value* slot) {
    ObjUpvalue* upvalue = ALLOCATE_OBJ(ObjUpvalue, OBJ_UPVALUE);
    upvalue->location = slot;
    upvalue->closed = NIL_VAL;
    upvalue->next = NULL;
    return upvalue;
}

static ObjString* allocateString(const char* chars, int length, uint32_t hash) {
    ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
    string->length = length;
    string->chars = chars;
    string->hash = hash;
    push(OBJ_VAL(string));
    tableSet(&vm.strings, string, NIL_VAL);
    pop();
    return string;
}

// FNV-1a hash
// https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
static uint32_t hashString(const char* key, int length) {
    uint32_t hash = 2166136261u;
    for (int i = 0; i < length; i++) {
        hash ^= (uint8_t)key[i];
        hash *= 16777619;
    }
    return hash;
}

ObjString* takeString(const char* chars, int length) {
    uint32_t hash = hashString(chars, length);
    ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
    if (interned != NULL) {
        FREE_ARRAY(char, chars, length + 1);
        return interned;
    }
    return allocateString(chars, length, hash);
}

ObjString* copyString(char* chars, int length) {
    uint32_t hash = hashString(chars, length);
    ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
    if (interned != NULL) {
        return interned;
    }

    char* heapChars = ALLOCATE(char, length + 1);
    memcpy(heapChars, chars, length);
    heapChars[length] = '\0';
    return allocateString(heapChars, length, hash);
}

static void printFunction(const ObjFunction* function) {
    if (function->name == NULL) {
        // User can't reference main script (a function), but useful for us
        printf("<script>");
        return;
    }
    printf("<fun %s>", function->name->chars);
}

void printObject(Value value) {
    switch (OBJ_TYPE(value)) {
        case OBJ_STRING:
            printf("%s", AS_CSTRING(value));
            break;
        case OBJ_FUNCTION: {
            printFunction(AS_FUNCTION(value));
            break;
        }
        case OBJ_CLOSURE:
            printFunction(AS_CLOSURE(value)->function);
            break;
        case OBJ_NATIVE:
            printf("<builtin fun>");
            break;
        case OBJ_UPVALUE: {
            ObjUpvalue* upvalue = (ObjUpvalue*)AS_OBJ(value);
            printf("upvalue of ");
            printValueFancy(*upvalue->location);
            break;
        }
        case OBJ_LIST: {
            const ObjList* list = AS_LIST(value);
            printf("[");
            if (list->array.values != NULL) {
                printValueFancy(*list->array.values);
                for (int i = 1; i < list->array.count; i++) {
                    printf(", ");
                    printValue(list->array.values[i]);
                }
            }
            printf("]");
                break;
        }
    }
}