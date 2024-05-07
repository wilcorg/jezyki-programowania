use num_traits::{PrimInt, Zero};
use rand::{random, Rng, SeedableRng};
use rand::distributions::Uniform;
use rand::rngs::StdRng;

pub fn gcd<T: PrimInt>(mut a: T, mut b: T) -> Option<T> {
    if a.is_zero() || b.is_zero() {
        return None;
    }

    while !b.is_zero() {
        let temp = b;
        b = a % b;
        a = temp;
    }

    Some(a)
}


pub fn ext_euclid<T : PrimInt>(x: T, y: T) -> T {
    let mut old_r = x;
    let mut r = y;
    let mut old_s: T = T::one();
    let mut s: T = T::zero();
    let mut old_t : T = T::zero();
    let mut t: T = T::one();
    let mut temp: T;

    while !r.is_zero() {
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

    old_s * old_r
}

fn floor_sqrt<T: PrimInt>(target: T) -> T {
    let mut low: T = T::one();
    let mut high: T = T::one();
    while high * high < target {
        low = high;
        high = high << 1;
    }

    let mut delta: T = low / T::from(2).unwrap();
    while !delta.is_zero() {
        let test: T = low + delta;
        if test * test > target {
            delta = delta >> 1;
        } else {
            low = low + delta;
        }
    }
    low
}

pub fn is_prime<T: PrimInt>(num: T) -> bool {
    if num < T::from(2).unwrap() { return false; }
    if num == T::from(2).unwrap() { return true; }
    if num % T::from(2).unwrap() == T::zero() { return false; }

    let bound: T = floor_sqrt(num);
    let mut i: T = T::from(3).unwrap();
    while i <= bound {
        if num % i == T::zero() { return false };
        i = i + T::from(2).unwrap();
    }
    true
}

pub fn gen_key<T: PrimInt>(bits: usize) -> T {
    let rng = &mut StdRng::from_entropy();
    let dist = Uniform::new_inclusive(0, 1);
    let mut result: T = T::zero();

    for _ in 0..bits - 1 {
        result = result << 1;
        let bit: T = T::from(rng.sample(dist)).unwrap();
        result = result | bit;
    }

    return if result.is_zero() {
        gen_key(bits)
    } else {
        result
    }
}
