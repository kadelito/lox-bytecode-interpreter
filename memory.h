#ifndef clox_memory_h
#define clox_memory_h

#include "common.h"
#include "value.h"

#define INITIAL_CAPACITY 8

// Resizes the capacity to INITIAL_CAPACITY
#define GROW_CAPACITY(capacity) \
    ((capacity) < INITIAL_CAPACITY ? INITIAL_CAPACITY : (capacity) * 2)

#define GROW_ARRAY(type, pointer, oldCount, newCount)     \
    (type*)reallocate(pointer, sizeof(type) * (oldCount), \
    sizeof(type) * (newCount))

#define FREE_ARRAY(type, pointer, oldCount) \
    (const void*)reallocate(pointer, sizeof(type) * (oldCount), 0)

#define ALLOCATE(type, count) \
    (type*)reallocate(NULL, 0, sizeof(type) * (count))

#define FREE(type, pointer) reallocate(pointer, sizeof(type), 0)

void* reallocate(void* pointer, size_t oldSize, size_t newSize);
void freeObjects();
void markObject(Obj* object);
void markValue(Value value);
void collectGarbage();

#endif