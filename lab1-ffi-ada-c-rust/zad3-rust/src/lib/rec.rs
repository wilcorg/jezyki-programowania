use super::super::utils::common::{DiophantineEq, DiophantineSol};
use super::super::utils::hidden::EuclideanSol;
use super::super::utils::optional::{OptI32, OptI64};


pub fn rec_factorial(n_arg: i8) -> OptI64 {
    let mut result = OptI64 { value: i64::MIN, is_present: false };

    if n_arg < 0 || n_arg > 20 {
        return result;
    }

    result.is_present = true;
    result.value = factorial_impl(n_arg);

    result
}

fn factorial_impl(n: i8) -> i64 {
    if n == 0 {
        return 1;
    }
    return i64::from(n) * factorial_impl(n - 1);
}

pub fn rec_gcd(a_arg: i32, b_arg: i32) -> OptI32 {
    let mut result = OptI32 { value: i32::MIN, is_present: false };
    let a = a_arg.abs();
    let b = b_arg.abs();

    if a == 0 || a == i32::MIN || b == 0 || b == i32::MIN {
        return result;
    }

    result.is_present = true;
    result.value = gcd_impl(a.abs(), b.abs());
    return result;
}

fn gcd_impl(a: i32, b: i32) -> i32 {
    if a == 0 {
        return b;
    }
    return gcd_impl(b % a, a);
}

fn rec_ext_euclid(eq: &DiophantineEq) -> EuclideanSol {
    let mut x: i32 = 0;
    let mut y: i32 = 0;
    let gcd = rec_ext_euclid_impl(eq.x_k, eq.y_k, &mut x, &mut y);

    EuclideanSol { x_k: x * gcd, y_k: y * gcd }
}

fn rec_ext_euclid_impl(a: i32, b: i32, x: &mut i32, y: &mut i32) -> i32 {
    if a == 0 {
        *x = 0;
        *y = 1;
        return b;
    }

    let mut x1: i32 = 0;
    let mut y1: i32 = 0;
    let gcd = rec_ext_euclid_impl(b % a, a, &mut x1, &mut y1);
    *x = y1 - (b / a) * x1;
    *y = x1;

    gcd
}

pub fn rec_dio_solve(eq: &DiophantineEq, sol: &mut DiophantineSol) {
    sol.x_k = i32::MIN;
    sol.x_a = i32::MIN;
    sol.y_k = i32::MIN;
    sol.y_a = i32::MIN;
    sol.is_present = false;
    let gcd = rec_gcd(rec_gcd(eq.x_k, eq.y_k).value, eq.c);
    let mut eq_copy: DiophantineEq = (*eq).clone();

    if !gcd.is_present || eq.c % gcd.value != 0 { return; }

    eq_copy.x_k = eq.x_k / gcd.value;
    eq_copy.y_k = eq.y_k / gcd.value;
    eq_copy.c = eq.c / gcd.value;
    let eu_sol = rec_ext_euclid(&eq_copy);

    sol.x_k = eq_copy.y_k;
    sol.x_a = eu_sol.x_k * eq_copy.c;
    sol.y_k = -eq_copy.x_k;
    sol.y_a = eu_sol.y_k * eq_copy.c;
}
