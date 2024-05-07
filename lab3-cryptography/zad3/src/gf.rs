use crate::tools::{ext_euclid, gcd, is_prime};

const ORDER: i64 = 1234567891;

/// Struct representing a number in GF(1234577)
#[derive(Clone, Copy, Debug)]
pub struct GF {
    value: i64,
}

impl GF {
    pub fn new(value: i64) -> Self {
        if !is_prime(ORDER) {
            panic!("Character isn't prime");
        }

        Self { value: value % ORDER}
    }
    pub fn value(&self) -> i64 { self.value }

    pub fn characteristic(&self) -> i64 { ORDER }

    pub fn negative(&self) -> Self { GF { value: ORDER - self.value } }

    pub fn inv(&self) -> Self {
        if gcd(self.value, ORDER).unwrap_or(0) != 1 {
            panic!("Value and character aren't coprime");
        }
        let mut inverted = ext_euclid(self.value, ORDER);
        if inverted < 0 {
            inverted += ORDER;
        }
        inverted %= ORDER;
        GF { value: inverted }
    }
}

/// Overloads `==`, `!=` operators
impl PartialEq for GF {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}
/// Overloads `<=,` `>=`, `<` i `>` operators
impl PartialOrd for GF {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.value.partial_cmp(&other.value)
    }

}

impl std::fmt::Display for GF {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

/// Overloads `+` operator
impl std::ops::Add for GF {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self::new(self.value + other.value)
    }
}

/// Overloads `+=` operator
impl std::ops::AddAssign for GF {
    fn add_assign(&mut self, other: Self) {
        self.value = (self.value + other.value) % ORDER;
    }
}

/// Overloads `-` operator
impl std::ops::Sub for GF {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self::new(self.value + ORDER - other.value)
    }
}

/// Overloads `-=` operator
impl std::ops::SubAssign for GF {
    fn sub_assign(&mut self, other: Self) {
        self.value = (self.value + ORDER - other.value) % ORDER;
    }
}

/// Overloads `*` operator
impl std::ops::Mul for GF {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        Self::new(self.value * other.value)
    }
}

/// Overloads `*=` operator
impl std::ops::MulAssign for GF {
    fn mul_assign(&mut self, other: Self) {
        self.value = (self.value * other.value) % ORDER;
    }
}

/// Overloads `/` operator
impl std::ops::Div for GF {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        self * other.inv()
    }
}

/// Overloads `/=` operator
impl std::ops::DivAssign for GF {
    fn div_assign(&mut self, other: Self) {
        *self *= other.inv();
    }
}

impl From<i64> for GF {
    fn from(value: i64) -> Self {
        Self::new(value)
    }
}