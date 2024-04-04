#include "iter.h"
#include "../utils/hidden.h"

euclidean_sol* iter_ext_euclid(const diophantine_eq* dio_eq) {
    int32_t old_r = dio_eq->x_k;
    int32_t r = dio_eq->y_k;
    int32_t old_s = 1;
    int32_t s = 0;
    int32_t old_t = 0;
    int32_t t = 1;

    int32_t temp;

    while (r != 0) {
        int32_t quotient = old_r / r;

        temp = r;
        r = old_r - quotient * temp;
        old_r = temp;

        temp = s;
        s = old_s - quotient * temp;
        old_s = temp;

        temp = t;
        t = old_t - quotient * temp;
        old_t = temp;
    }

    euclidean_sol* d = malloc(sizeof(euclidean_sol));
    d->x_k = old_s * old_r;
    d->y_k = old_t * old_r;

    return d;
}

opt_i64 iter_factorial(int8_t n) {
    opt_i64 result = {INT64_MIN, false};

    if (n < 0 || n > 20) {
        return result;
    } else if (n == 0) {
        result.value = 1;
        result.is_present = true;
        return result;
    }

    result.value = 1;
    for (int i = 1; i <= n; i++) {
        result.value *= i;
    }
    result.is_present = true;
    return result;
}

opt_i32 iter_gcd(int32_t a, int32_t b) {
    opt_i32 result = {INT32_MIN, false};

    if (a == 0 || a == INT32_MIN || b == 0 || b == INT32_MIN) {
        return result;
    }

    a = abs(a);
    b = abs(b);
    while (b != 0) {
        int32_t temp = b;
        b = a % b;
        a = temp;
    }
    result.value = a;
    result.is_present = true;
    return result;
}

void iter_dio_solve(const diophantine_eq* eq, diophantine_sol* sol) {
    sol->x_k = INT32_MIN;
    sol->x_a = INT32_MIN;
    sol->y_k = INT32_MIN;
    sol->y_a = INT32_MIN;
    sol->is_present = false;
    opt_i32 gcd = iter_gcd(iter_gcd(eq->x_k, eq->y_k).value, eq->c);
    diophantine_eq eq_copy = *eq;

    if (!gcd.is_present) return;
    if (eq->c % gcd.value != 0) {
        return;
    }

    eq_copy.x_k = eq->x_k / gcd.value;
    eq_copy.y_k = eq->y_k / gcd.value;
    eq_copy.c = eq->c / gcd.value;
    euclidean_sol* eu_sol = iter_ext_euclid(&eq_copy);

    sol->x_k = eq_copy.y_k;
    sol->x_a = eu_sol->x_k * eq_copy.c;
    sol->y_k = -eq_copy.x_k;
    sol->y_a = eu_sol->y_k * eq_copy.c;
    sol->is_present = true;
    free(eu_sol);
}
