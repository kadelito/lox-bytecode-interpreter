#include <stdio.h>
#include <math.h>

#include "memory.h"
#include "value.h"
#include "object.h"

const char* getValName(ValueType type) {
    switch (type) {
        CASE_RETURN_STR(VAL_BOOL);
        CASE_RETURN_STR(VAL_NIL);
        CASE_RETURN_STR(VAL_NUMBER);
        CASE_RETURN_STR(VAL_OBJ);
    }
}

void initValueArray(ValueArray* array) {
    array->values = NULL;
    array->capacity = 0;
    array->count = 0;
}

void writeValueArray(ValueArray* array, const Value value) {
    if (array->capacity < array->count + 1) {
        int oldCapacity = array->capacity;
        array->capacity = GROW_CAPACITY(oldCapacity);
        array->values = GROW_ARRAY(Value, array->values,
                                   oldCapacity, array->capacity);
    }

    array->values[array->count] = value;
    array->count++;
}

void freeValueArray(ValueArray* array) {
    FREE_ARRAY(Value, array->values, array->capacity);
    initValueArray(array);
}

void printValue(const Value value) {
    switch (value.type) {
        case VAL_BOOL:
            printf(AS_BOOL(value) ? "true" : "false");
            break;
        case VAL_NIL: printf("nil"); break;
        case VAL_NUMBER: printf("%g", AS_NUMBER(value)); break;
        case VAL_OBJ: printObject(value); break;
        default: printf("?val?");
    }
}

void printValueFancy(Value value) {
    bool isString = value.type == VAL_OBJ && value.as.obj->type == OBJ_STRING;
    if (isString) printf("\"");
    printValue(value);
    if (isString) printf("\"");
}

inline bool numbersEqual(double a, double b) {
    return fabs(a - b) < DOUBLE_TOLERANCE;
}

bool valuesEqual(Value a, Value b) {
    if (a.type != b.type) return false;
    switch (a.type) { // a.type == b.type
        case VAL_BOOL:   return AS_BOOL(a) == AS_BOOL(b);
        case VAL_NIL:    return true;
        case VAL_NUMBER: return numbersEqual(AS_NUMBER(a), AS_NUMBER(b));
        case VAL_OBJ:    return AS_OBJ(a) == AS_OBJ(b);
        default:         return false; // Unreachable.
    }
}