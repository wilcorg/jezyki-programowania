#include "lib/iter.h"
#include "lib/rec.h"
#include <stdio.h>

int main() {
    printf("%ld\n", iter_factorial(20).value);
    printf("%d\n", iter_gcd(12, 24).value);

    printf("%ld\n", rec_factorial(5).value);
    printf("%d\n", rec_gcd(12, 24).value);

    const diophantine_eq eq = {87, -64, 3};
    diophantine_sol sol;
    iter_dio_solve(&eq, &sol);
    printf("%d\n", sol.x_k);
    printf("%d\n", sol.x_a);
    printf("%d\n", sol.y_k);
    printf("%d\n", sol.y_a);

    return 0;
}
