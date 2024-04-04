use super::super::utils::common::{diophantine_eq, diophantine_sol};
use super::super::utils::hidden::euclidean_sol;
use super::super::utils::optionals::{opt_i32, opt_i64};


#[no_mangle]
pub extern fn rec_factorial(n: i8) -> opt_i64 {
    let mut result = opt_i64 {value: i64::MIN, is_present: false};

    if n < 0 || n > 20 {
        return result
    }

    result.is_present = true;
    result.value = factorial_impl(n);

    result
}

fn factorial_impl(n: i8) -> i64 {
    if n == 0 {
        return 1
    }
    return i64::from(n) * factorial_impl(n - 1)
}

#[no_mangle]
pub extern fn rec_gcd(a: i32, b: i32) -> opt_i32 {
    let mut result = opt_i32 {value: i32::MIN, is_present: false};

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

fn rec_ext_euclid(dio_eq: &diophantine_eq) -> euclidean_sol {
    let mut x: i32 = 0;
    let mut y: i32 = 0;
    let gcd = ext_euclid_impl(dio_eq.x_k, dio_eq.y_k, &mut x, &mut y);

    euclidean_sol {x_k: x * gcd, y_k: y * gcd}
}

fn ext_euclid_impl(a: i32, b: i32, x: &mut i32, y: &mut i32) -> i32 {
    if a == 0 {
        *x = 0;
        *y = 1;
        return b;
    }

    let mut x1: i32 = 0;
    let mut y1: i32 = 0;
    let gcd = ext_euclid_impl(b % a, a, &mut x1, &mut y1);
    *x = y1 - (b / a) * x1;
    *y = x1;

    gcd
}

#[no_mangle]
pub extern fn rec_solve(dio_eq_arg: diophantine_eq) -> diophantine_sol {
    let mut result = diophantine_sol { x_k: i32::MIN, x_a: i32::MIN, y_k: i32::MIN, y_a: i32::MIN, is_present: false };
    let mut dio_eq = dio_eq_arg;
    let gcd = rec_gcd(rec_gcd(dio_eq.x_k, dio_eq.y_k).value, dio_eq.c);

    if !gcd.is_present { return result; };
    if  dio_eq.c % gcd.value != 0 { return result; }

    dio_eq.x_k /= gcd.value;
    dio_eq.y_k /= gcd.value;
    dio_eq.c /= gcd.value;
    let eu_sol = rec_ext_euclid(&dio_eq);

    result = diophantine_sol {x_k: dio_eq.y_k,
        x_a: eu_sol.x_k * dio_eq.c,
        y_k: -dio_eq.x_k,
        y_a: eu_sol.y_k * dio_eq.c,
        is_present: true};

    result
}
