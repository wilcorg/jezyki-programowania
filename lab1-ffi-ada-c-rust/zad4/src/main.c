#include "wrapper.h"

int main() {
    diophantine_eq eq = {87, -64, 3};
    diophantine_sol sol;
    rec_dio_solve(&eq, &sol);

    printf("%ld\n", iter_factorial(20).value);
    printf("%ld\n", rec_factorial(20).value);
    printf("%d\n", iter_gcd(12, 24).value);
    printf("%d\n", rec_gcd(12, 24).value);
    printf("%d\n", sol.x_k);
    printf("%d\n", sol.x_a);
    printf("%d\n", sol.y_k);
    printf("%d\n", sol.y_a);

    return 0;
}
