#ifndef Z1_COMMON_H
#define Z1_COMMON_H

#include <stdint.h>
#include <memory.h>
#include <stdlib.h>
#include <stdbool.h>

typedef struct {
    int32_t x_k;
    int32_t y_k;
    int32_t c;
} diophantine_eq;

typedef struct {
    int32_t x_k;
    int32_t x_a;
    int32_t y_k;
    int32_t y_a;
    bool is_present;
} diophantine_sol;

#endif

