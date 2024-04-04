use super::super::utils::common::{diophantine_eq, diophantine_sol};
use super::super::utils::hidden::euclidean_sol;
use super::super::utils::optionals::{opt_i32, opt_i64};


#[no_mangle]
pub extern fn lin_factorial(n: i8) -> opt_i64 {
    let mut result = opt_i64 { is_present: false, value: i64::MIN };

    if n < 0 || n > 20 {
        return result;
    } else if n == 0 {
        result.value = 1;
        result.is_present = true;
        return result;
    }

    result.value = 1;
    for i in 1..=n {
        result.value *= i64::from(i);
    }
    result.is_present = true;

    result
}

#[no_mangle]
pub extern fn lin_gcd(a_arg: i32, b_arg: i32) -> opt_i32 {
    let mut result = opt_i32 { value: i32::MIN, is_present: false };
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

#[no_mangle]
pub extern fn lin_solve(dio_eq_arg: diophantine_eq) -> diophantine_sol {
    let mut result = diophantine_sol { x_k: i32::MIN, x_a: i32::MIN, y_k: i32::MIN, y_a: i32::MIN, is_present: false };
    let mut dio_eq = dio_eq_arg;
    let gcd = lin_gcd(lin_gcd(dio_eq.x_k, dio_eq.y_k).value, dio_eq.c);

    if !gcd.is_present { return result; };
    if dio_eq.c % gcd.value != 0 { return result; }

    dio_eq.x_k /= gcd.value;
    dio_eq.y_k /= gcd.value;
    dio_eq.c /= gcd.value;
    let eu_sol = lin_ext_euclid(&dio_eq);

    result = diophantine_sol {x_k: dio_eq.y_k,
                              x_a: eu_sol.x_k * dio_eq.c,
                              y_k: -dio_eq.x_k,
                              y_a: eu_sol.y_k * dio_eq.c,
                              is_present: true};

    result
}

fn lin_ext_euclid(dio_eq: &diophantine_eq) -> euclidean_sol {
    let mut old_r = dio_eq.x_k;
    let mut r = dio_eq.y_k;
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

    let d = euclidean_sol {x_k: old_s * old_r,
                           y_k: old_t * old_r};

    d
}
