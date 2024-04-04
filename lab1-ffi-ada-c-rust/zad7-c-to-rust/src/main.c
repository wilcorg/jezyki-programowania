#include "wrapper.h"
#include <stdio.h>


int main(void) {
    printf("%ld\n", lin_factorial(20).value);
    printf("%d\n", lin_gcd(12, 24).value);

    printf("%ld\n", rec_factorial(5).value);
    printf("%d\n", rec_gcd(12, 24).value);

    const diophantine_eq eq = {87, -64, 3};
//    const diophantine_sol* sol = lin_solve(&eq);
    diophantine_sol req_sol = lin_solve(eq);
    printf("%d\n", req_sol.x_k);
    printf("%d\n", req_sol.x_a);
    printf("%d\n", req_sol.y_k);
    printf("%d\n", req_sol.y_a);

    return 0;
}

