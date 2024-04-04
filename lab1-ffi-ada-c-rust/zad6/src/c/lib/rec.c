#include "rec.h"
#include "../utils/hidden.h"

int64_t rec_factorial_impl(int8_t n);
int32_t rec_gcd_impl(int32_t a, int32_t b);
int32_t rec_ext_euclid_impl(int32_t a, int32_t b, int32_t* x, int32_t* y);
euclidean_sol* rec_ext_euclid(const diophantine_eq* dio_eq);


opt_i64 rec_factorial(int8_t n) {
    opt_i64 result = {INT64_MIN, false};

    if (n < 0 || n > 20) {
        return result;
    }

    result.is_present = true;
    result.value = rec_factorial_impl(n);
    return result;
}

opt_i32 rec_gcd(int32_t a, int32_t b) {
    opt_i32 result = {INT32_MIN, false};

    if (a == 0 || a == INT32_MIN || b == 0 || b == INT32_MIN) {
        return result;
    }

    result.is_present = true;
    result.value = rec_gcd_impl(abs(a), abs(b));
    return result;
}

void rec_dio_solve(const diophantine_eq* eq, diophantine_sol* sol) {
    sol->x_k = INT32_MIN;
    sol->x_a = INT32_MIN;
    sol->y_k = INT32_MIN;
    sol->y_a = INT32_MIN;
    sol->is_present = false;
    opt_i32 gcd = rec_gcd(rec_gcd(eq->x_k, eq->y_k).value, eq->c);
    diophantine_eq eq_copy = *eq;

    if (!gcd.is_present) return;
    if (eq->c % gcd.value != 0) {
        return;
    }

    eq_copy.x_k = eq->x_k / gcd.value;
    eq_copy.y_k = eq->y_k / gcd.value;
    eq_copy.c = eq->c / gcd.value;
    euclidean_sol* eu_sol = rec_ext_euclid(&eq_copy);

    sol->x_k = eq_copy.y_k;
    sol->x_a = eu_sol->x_k * eq_copy.c;
    sol->y_k = -eq_copy.x_k;
    sol->y_a = eu_sol->y_k * eq_copy.c;
    sol->is_present = true;
    free(eu_sol);
}

int64_t rec_factorial_impl(int8_t n) {
    if (n == 0) {
        return 1;
    }
    return n * rec_factorial_impl((int8_t) (n - 1));
}

int32_t rec_gcd_impl(int32_t a, int32_t b) {
    if (a == 0) {
        return b;
    }

    return rec_gcd_impl(b % a, a);
}

euclidean_sol* rec_ext_euclid(const diophantine_eq* dio_eq) {
    int32_t x, y;
    int32_t gcd = rec_ext_euclid_impl(dio_eq->x_k, dio_eq->y_k, &x, &y);

    euclidean_sol* d = malloc(sizeof(euclidean_sol));
    d->x_k = x * gcd;
    d->y_k = y * gcd;

    return d;
}

int32_t rec_ext_euclid_impl(int32_t a, int32_t b, int32_t* x, int32_t* y) {
    if (a == 0) {
        *x = 0;
        *y = 1;
        return b;
    }

    int32_t x1, y1;
    int32_t gcd = rec_ext_euclid_impl(b % a, a, &x1, &y1);

    *x = y1 - (b / a) * x1;
    *y = x1;

    return gcd;
}
