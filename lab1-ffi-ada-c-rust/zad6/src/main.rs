use utils::common::{DiophantineEq, DiophantineSol};
use utils::optional::{OptI32, OptI64};

mod utils;

extern "C" {
    pub fn iter_factorial(n: libc::c_short) -> OptI64;
    pub fn iter_gcd(a: libc::c_int, b: libc::c_int) -> OptI32;
    pub fn iter_dio_solve(eq: &DiophantineEq, sol: &mut DiophantineSol);

    pub fn rec_factorial(n: libc::c_short) -> OptI64;
    pub fn rec_gcd(a: libc::c_int, b: libc::c_int) -> OptI32;
    pub fn rec_dio_solve(eq: &DiophantineEq, sol: &mut DiophantineSol);
}

fn main() {
    let eq = DiophantineEq {x_k: 87, y_k: -64, c: 3};
    let mut sol = DiophantineSol {..Default::default() };
    unsafe {
        println!("{}", iter_factorial(20).value);
        println!("{}", iter_gcd(10, 20).value);
        iter_dio_solve(&eq, &mut sol);
        println!("{}", sol.x_k);
        println!("{}", sol.x_a);
        println!("{}", sol.y_k);
        println!("{}", sol.y_a);
    }
}
