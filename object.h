#ifndef  clox_object_h
#define clox_object_h

#include "chunk.h"
#include "common.h"
#include "value.h"

#define OBJ_TYPE(value)  (AS_OBJ(value)->type)

#define IS_LIST(value)     isObjType(value, OBJ_LIST)
#define IS_CLOSURE(value)  isObjType(value, OBJ_CLOSURE)
#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)
#define IS_NATIVE(value)   isObjType(value, OBJ_NATIVE)
#define IS_STRING(value)   isObjType(value, OBJ_STRING)

#define AS_LIST(value)     ((ObjList*)AS_OBJ(value))
#define AS_CLOSURE(value)  ((ObjClosure*)AS_OBJ(value))
#define AS_FUNCTION(value) ((ObjFunction*)AS_OBJ(value))
#define AS_NATIVE(value)   (((ObjNative*)AS_OBJ(value))->function)
#define AS_STRING(value)   ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value)  (((ObjString*)AS_OBJ(value))->chars)

typedef enum {
    OBJ_LIST,
    OBJ_CLOSURE,
    OBJ_FUNCTION,
    OBJ_NATIVE,
    OBJ_STRING,
    OBJ_UPVALUE,
} ObjType;

struct Obj {
    ObjType type;
    bool isMarked;
    Obj* next;      // used for freeing the vm's objects
};

typedef struct {
    Obj obj;
    int arity;
    int upvalueCount;
    Chunk chunk;
    ObjString* name;
} ObjFunction;

typedef Value (*NativeFn)(int argCount, Value* args);

typedef struct {
    Obj obj;
    const char* name;
    NativeFn function;
} ObjNative;

struct ObjString {
    Obj obj;
    int length;
    const char* chars;
    uint32_t hash;      // precomputed upon interning, otherwise not necessary during runtime
};

typedef struct ObjUpvalue {
    Obj obj;
    Value* location;    // POINTER TO A VALUE!! modifications should affect the original
    Value closed;
    struct ObjUpvalue* next;
} ObjUpvalue;   // Necessary for GC, closures are heap-allocated

typedef struct {
    Obj obj;
    ObjFunction* function;
    ObjUpvalue** upvalues;
    int upvalueCount;
} ObjClosure;

typedef struct {
    Obj obj;
    ValueArray array;
} ObjList;

const char* getObjName(ObjType type);
ObjList* newList(ValueArray* array);
ObjClosure* newClosure(ObjFunction* function);
ObjFunction* newFunction();
ObjNative* newNative(NativeFn function, const char* name);
ObjUpvalue* newUpvalue(Value* slot);
ObjString* takeString(const char* chars, int length);
ObjString* copyString(char* chars, int length);
void printObject(Value value);

static bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif