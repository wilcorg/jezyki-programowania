use std::cmp::Ordering;
use std::fmt::{Display, Formatter};
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Sub, SubAssign};
use crate::number_tools::{ext_euclid, gcd, is_prime};

pub struct GaloisField<const C: u32> {
    value: u32,
}

impl<const CHARACT: u32> GaloisField<CHARACT> {
    pub fn new(value: u32) -> Option<Self> {
        if !is_prime(CHARACT) {
            panic!("Character isn't prime");
        }

        return if value >= CHARACT {
            None
        } else {
            Some(GaloisField { value })
        };
    }

    pub fn characteristic(&self) -> u32 { CHARACT }

    pub fn negative(value: i64) -> u32 {
        return if value > 0 {
            value as u32
        } else {
            ((CHARACT as i64 - value - 1) % CHARACT as i64) as u32
        }
    }

    pub fn inverse(self) -> Option<Self> {
        if gcd(self.value, CHARACT).unwrap_or(0) != 1 {
            panic!("Value and character aren't coprime");
        }
        let inverted = ext_euclid(self.value, CHARACT);
        Self::new(Self::negative(inverted as i64))
    }
}

impl<const CHARACT: u32> From<GaloisField<CHARACT>> for u32 {
    fn from(rhs: GaloisField<CHARACT>) -> Self {
        rhs.value
    }
}

impl<const CHARACT: u32> Display for GaloisField<CHARACT> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "value: {}; characteristic: {}", self.value, CHARACT)
    }
}

impl<const CHARACT: u32> Add for GaloisField<CHARACT> {
    type Output = GaloisField<CHARACT>;

    fn add(self, rhs: Self) -> Self::Output {
        let temp = self.value as i64 + rhs.value as i64;
        Self::new((temp % CHARACT as i64) as u32).unwrap()
    }
}

impl<const CHARACT: u32> Sub for GaloisField<CHARACT> {
    type Output = GaloisField<CHARACT>;

    fn sub(self, rhs: Self) -> Self::Output {
        let temp = self.value as u64 + Self::negative(rhs.value as i64) as u64;
        Self::new((temp % CHARACT as u64) as u32).unwrap()
    }
}

impl<const CHARACT: u32> Mul for GaloisField<CHARACT> {
    type Output = GaloisField<CHARACT>;

    fn mul(self, rhs: Self) -> Self::Output {
        let temp = self.value as u64 * rhs.value as u64;
        Self::new((temp % CHARACT as u64) as u32).unwrap()
    }
}

impl<const CHARACT: u32> Div for GaloisField<CHARACT> {
    type Output = GaloisField<CHARACT>;

    fn div(self, rhs: Self) -> Self::Output {
        let rhs_inv = rhs.inverse().unwrap();
        let temp = self.value as u64 * rhs_inv.value as u64;
        Self::new((temp % CHARACT as u64) as u32).unwrap()
    }
}

impl<const CHARACT: u32> AddAssign for GaloisField<CHARACT> {
    fn add_assign(&mut self, rhs: Self) {
        self.value = ((self.value as u64 + rhs.value as u64) % CHARACT as u64) as u32
    }
}

impl<const CHARACT: u32> SubAssign for GaloisField<CHARACT> {
    fn sub_assign(&mut self, rhs: Self) {
        self.value = ((self.value as u64 + Self::negative(rhs.value as i64) as u64) % CHARACT as u64) as u32
    }
}

impl<const CHARACT: u32> MulAssign for GaloisField<CHARACT> {
    fn mul_assign(&mut self, rhs: Self) {
        self.value = ((self.value as u64 * rhs.value as u64) % CHARACT as u64) as u32
    }
}

impl<const CHARACT: u32> DivAssign for GaloisField<CHARACT> {
    fn div_assign(&mut self, rhs: Self) {
        let rhs_inv = rhs.inverse().unwrap();
        self.value = ((self.value as u64 * rhs_inv.value as u64) % CHARACT as u64) as u32
    }
}

impl<const CHARACT: u32> PartialEq for GaloisField<CHARACT> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<const CHARACT: u32> PartialOrd for GaloisField<CHARACT> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.value.partial_cmp(&other.value)
    }
}
