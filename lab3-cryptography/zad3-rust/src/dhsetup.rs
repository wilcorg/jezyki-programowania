use std::fmt::Debug;
use crate::tools::gen_key;

pub trait AlgebraicBody: std::ops::Mul<Output=Self> + std::ops::Div<Output=Self> + Copy + Debug + std::ops::MulAssign + From<i64> {}

impl<T: std::ops::Mul<Output=Self> + std::ops::Div<Output=Self> + Copy + Debug + std::ops::MulAssign + From<i64>> AlgebraicBody for T {}

pub struct DHSetup<T: AlgebraicBody> {
    generator: T,
}

impl<T: AlgebraicBody> DHSetup<T> {
    pub fn new() -> Self {
        Self { generator: T::from(gen_key(64)) }
    }
    pub fn get_generator(&self) -> T {
        self.generator
    }
    pub fn power(a: T, b: i64) -> T {
        let mut a = a;
        let mut b = b;
        let mut res: T = T::from(1);
        while b > 0 {
            if b % 2 == 1 {
                res = res * a;
            }
            a = a * a;
            b /= 2;
        }
        res
    }
}