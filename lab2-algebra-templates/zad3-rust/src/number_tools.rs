pub fn gcd(a_arg: u32, b_arg: u32) -> Option<u32> {
    let mut a = a_arg;
    let mut b = b_arg;

    if a == 0 || b == 0 {
        return None;
    }

    while b != 0 {
        let temp = b;
        b = a % b;
        a = temp;
    }

    Some(a)
}


pub fn ext_euclid(x_arg: u32, y_arg: u32) -> i32 {
    let mut old_r = x_arg as i64;
    let mut r = y_arg as i64;
    let mut old_s = 1;
    let mut s = 0;
    let mut old_t = 0;
    let mut t = 1;
    let mut temp: i64;

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

    (old_s * old_r) as i32
}

pub fn is_prime(num: u32) -> bool {
    if num < 2 {
        return false;
    } else if num == 2 {
        return true;
    } else {
        let bound = ((num as f64).sqrt() as u32) + 1;

        for i in 2..=bound {
            if num % i == 0 {
                return false;
            }
        }
        true
    }
}
