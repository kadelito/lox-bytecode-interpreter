#ifndef clox_common_h
#define clox_common_h

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// #define DEBUG_PRINT_CODE
// #define DEBUG_LOCAL_NAMES
// #define DEBUG_CHUNK_DETAILS
// #define DEBUG_TRACE_EXECUTION
#define DEBUG_SUMMARIZE_VM

// #define DEBUG_DISABLE_GC
#define DEBUG_STRESS_GC
// #define DEBUG_LOG_GC

#define UINT8_COUNT (UINT8_MAX + 1)

#define CASE_RETURN_STR(val) case val: return (#val)

#endif