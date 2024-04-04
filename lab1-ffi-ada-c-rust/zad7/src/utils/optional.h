#ifndef INC_1Z_OPTIONAL_H
#define INC_1Z_OPTIONAL_H

#include <stdbool.h>
#include <stdint.h>

typedef struct {
    int32_t value;
    bool is_present;
} opt_i32;

typedef struct {
    int64_t value;
    bool is_present;
} opt_i64;

#endif//INC_1Z_OPTIONAL_H

