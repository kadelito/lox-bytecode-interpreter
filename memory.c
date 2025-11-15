#include <stdlib.h>
#include <stdio.h>

#include "memory.h"

#include <string.h>

#include "compiler.h"
#include "object.h"
#include "value.h"
#include "vm.h"

#define GC_GROW_THRESHOLD(bytesAllocated) (bytesAllocated * 2)

void* reallocate(void* pointer, size_t oldSize, size_t newSize) {

    vm.bytesAllocated += newSize - oldSize;

    // Only garbage collect if there are objects to collect
    // Otherwise, nothing is freed
    bool objectsToFree = vm.objects != NULL;
#ifdef DEBUG_DISABLE_GC
    objectsToFree = false;
#endif

#ifdef DEBUG_STRESS_GC
    if (objectsToFree && newSize > oldSize) {
        collectGarbage();
    }
#else
    if (objectsToFree && vm.bytesAllocated > vm.nextGC) {
        collectGarbage();
    }
#endif

    if (newSize == 0) {
        free(pointer);
        return NULL;
    }

    void* result = realloc(pointer, newSize);
    if (result == NULL) {
        fprintf(stderr, "Internal error: Memory failure!");
        // reminder: all memory freed upon program end
        exit(1);
    }
    return result;
}

void markObject(Obj* object) {
    if (object == NULL) return;
    if (object->isMarked)
        // This avoids circular references
        return;

#ifdef DEBUG_LOG_GC
    // printf("%p mark ", (void*)object);
    // printValue(OBJ_VAL(object));
    // printf("\n");
#endif

    object->isMarked = true;

    if (vm.unfinishedCapacity < vm.unfinishedCount + 1) {
        vm.unfinishedCapacity = GROW_CAPACITY(vm.unfinishedCapacity);
        vm.unfinishedStack = (Obj**)realloc(vm.unfinishedStack, sizeof(Obj*) * vm.unfinishedCapacity);
        if (vm.unfinishedStack == NULL) {
            fprintf(stderr, "Internal error: Memory failure during garbage collection!");
            exit(1);
        }
    }

    vm.unfinishedStack[vm.unfinishedCount++] = object;
}

void markValue(Value value) {
    // Only objects are heap-allocated
    if (IS_OBJ(value)) markObject(AS_OBJ(value));
}

static void markArray(ValueArray* array) {
    for (int i = 0; i < array->count; i++) {
        markValue(array->values[i]);
    }
}

// Takes a marked object and shallowly marks its children
static void finishObject(Obj* object) {
#ifdef DEBUG_LOG_GC
    // printf("%p finishing obj type: %s\n", object, getObjName(object->type) + 4);
#endif
    switch (object->type) {
        case OBJ_LIST: {
            ObjList* list = (ObjList*)object;
            if (list->array.values != NULL)
                markArray(&list->array);
            break;
        }
        case OBJ_CLOSURE: {
            ObjClosure* closure = (ObjClosure*)object;
            markObject((Obj*)closure->function);
            for (int i = 0; i < closure->upvalueCount; i++) {
                markObject((Obj*)closure->upvalues[i]);
            }
            break;
        }
        case OBJ_UPVALUE:
            markValue(((ObjUpvalue*)object)->closed);
            break;
        case OBJ_FUNCTION: {
            ObjFunction* function = (ObjFunction*)object;
            markObject((Obj*)function->name);
            markArray(&function->chunk.constants);
            break;
        }
        case OBJ_NATIVE:
        case OBJ_STRING:
            // No outward references, already finished
            break;
    }
}

static void markRoots() {
    // Stack
    for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
        markValue(*slot);
    }

    // Closures for each frame
    for (int i = 0; i < vm.frameCount; i++) {
        markObject((Obj*)vm.frames[i].closure);
    }

    // Upvalues (i still don't get these)
    for (ObjUpvalue* upvalue = vm.openUpvalues; upvalue != NULL; upvalue = upvalue->next) {
        markObject((Obj*)upvalue);
     }

    markTable(&vm.globals);
    markCompilerRoots();
}

static void traceReferences() {
    while (vm.unfinishedCount > 0) {
        Obj* object = vm.unfinishedStack[--vm.unfinishedCount];
        finishObject(object);
    }
}

static void freeObject(Obj* object) {
#ifdef DEBUG_LOG_GC
    printf("  %p: free obj type %s\n", object, getObjName(object->type) + 4);
#endif

    switch (object->type) {
        case OBJ_LIST: {
            ObjList* list = (ObjList*)object;
            freeValueArray(&list->array);
            FREE(ObjList, list);
            break;
        }
        case OBJ_CLOSURE: {
            ObjClosure* closure = (ObjClosure*)object;
            FREE_ARRAY(ObjUpvalue*, closure->upvalues, closure->upvalueCount);
            FREE(ObjClosure, object);
            break;
        }
        case OBJ_STRING: {
            const ObjString* string = (ObjString*)object;
            FREE_ARRAY(char, string->chars, string->length + 1);
            FREE(ObjString, object);
            break;
        }
        case OBJ_FUNCTION: {
            ObjFunction* function = (ObjFunction*)object;
            freeChunk(&function->chunk);
            FREE(ObjFunction, object);
            break;
        }
        case OBJ_NATIVE: {
            const ObjNative* native = (ObjNative*)object;
            // Don't free chars, it's a string literal
            FREE(ObjNative, object);
            break;
        }
        case OBJ_UPVALUE:
            FREE(ObjUpvalue, object);
            break;
    }
}

#ifdef DEBUG_LOG_GC
typedef struct Logged {
    Obj* obj;
    struct Logged* next;
} Logged;
#endif

static void sweep() {
    Obj* previous = NULL;
    Obj* object = vm.objects;
    while (object != NULL) {
        if (object->isMarked) {
            // Continue, reachable from roots
            object->isMarked = false; // ready for next collection
            previous = object;
            object = object->next;
        } else {
            // Not reachable, remove from list and free
            Obj* unreached = object;
            object = object->next;
            if (previous != NULL) {
                // Replace unreachable object
                previous->next = object;
            } else {
                // The first object is
                vm.objects = object;
            }

            freeObject(unreached);
        }
    }
}

#ifdef DEBUG_LOG_GC
static int gcCounter = 1;
#endif

void collectGarbage() {
#ifdef DEBUG_LOG_GC
    printf("-- gc #%d triggered\n", gcCounter);
    size_t before = vm.bytesAllocated;
#endif

    markRoots();
    traceReferences();
    tableRemoveWhite(&vm.strings);  // Delete unmarked strings from table to remove dangling pointers
    sweep(); // Note that calling freeObject decrements vm.bytesAllocated

    vm.nextGC = GC_GROW_THRESHOLD(vm.bytesAllocated);

#ifdef DEBUG_LOG_GC
    gcCounter++;
    printf("-- gc end");
    if (before == vm.bytesAllocated)
        printf(", nothing collected\n");
    else
        printf("\n   collected %zu bytes (from %zu to %zu) next at %zu\n",
            before - vm.bytesAllocated, before, vm.bytesAllocated, vm.nextGC);
#endif
}

// Frees every object in the VM's memory.
void freeObjects() {
    Obj* object = vm.objects;
    while (object != NULL) {
        Obj* next = object->next;
        freeObject(object);
        object = next;
    }
    free(vm.unfinishedStack);
}