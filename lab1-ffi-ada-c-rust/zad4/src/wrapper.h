#ifndef Z4_WRAPPER_H
#define Z4_WRAPPER_H

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    int value;
    bool is_present;
} opt_i32;

typedef struct {
    long value;
    bool is_present;
} opt_i64;

typedef struct {
    int x_k;
    int y_k;
    int c;
} diophantine_eq;

typedef struct {
    int x_k;
    int x_a;
    int y_k;
    int y_a;
    bool is_present;
} diophantine_sol;


extern opt_i64 iter_factorial(short n);

extern opt_i32 iter_gcd(int a, int b);

extern void iter_dio_solve(diophantine_eq* dio_eq, diophantine_sol* sol);

extern opt_i64 rec_factorial(short n);

extern opt_i32 rec_gcd(int a, int b);

extern void rec_dio_solve(diophantine_eq* dio_eq, diophantine_sol* sol);

#endif
