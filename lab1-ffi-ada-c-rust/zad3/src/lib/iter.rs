use super::super::utils::common::{DiophantineEq, DiophantineSol};
use super::super::utils::hidden::EuclideanSol;
use super::super::utils::optional::{OptI32, OptI64};

pub fn iter_factorial(n_arg: i8) -> OptI64 {
    let mut result = OptI64 { value: i64::MIN, is_present: false };

    if n_arg < 0 || n_arg > 20 {
        return result;
    } else if n_arg == 0 {
        result.value = 1;
        result.is_present = true;
        return result;
    }

    result.value = 1;
    for i in 1..=n_arg {
        result.value *= i64::from(i);
    }
    result.is_present = true;

    result
}

pub fn iter_gcd(a_arg: i32, b_arg: i32) -> OptI32 {
    let mut result = OptI32 { value: i32::MIN, is_present: false };
    let mut a = a_arg.abs();
    let mut b = b_arg.abs();

    if a == 0 || a == i32::MIN || b == 0 || b == i32::MIN {
        return result;
    }

    while b != 0 {
        let temp = b;
        b = a % b;
        a = temp;
    }
    result.value = a;
    result.is_present = true;

    result
}

pub fn iter_dio_solve(eq: &DiophantineEq, sol: &mut DiophantineSol) {
    sol.x_k = i32::MIN;
    sol.x_a = i32::MIN;
    sol.y_k = i32::MIN;
    sol.y_a = i32::MIN;
    sol.is_present = false;
    let gcd = iter_gcd(iter_gcd(eq.x_k, eq.y_k).value, eq.c);
    let mut eq_copy: DiophantineEq = (*eq).clone();

    if !gcd.is_present || eq.c % gcd.value != 0 { return; }

    eq_copy.x_k = eq.x_k / gcd.value;
    eq_copy.y_k = eq.y_k / gcd.value;
    eq_copy.c = eq.c / gcd.value;
    let eu_sol = iter_ext_euclid(&eq_copy);

    sol.x_k = eq_copy.y_k;
    sol.x_a = eu_sol.x_k * eq_copy.c;
    sol.y_k = -eq_copy.x_k;
    sol.y_a = eu_sol.y_k * eq_copy.c;
}

fn iter_ext_euclid(eq: &DiophantineEq) -> EuclideanSol {
    let mut old_r = eq.x_k;
    let mut r = eq.y_k;
    let mut old_s = 1;
    let mut s = 0;
    let mut old_t = 0;
    let mut t = 1;
    let mut temp: i32;

    while r != 0 {
        let quotient = old_r / r;

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

    EuclideanSol { x_k: old_s * old_r, y_k: old_t * old_r }
}