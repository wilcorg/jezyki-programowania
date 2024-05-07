use crate::dhsetup::{AlgebraicBody, DHSetup};
use crate::tools::gen_key;

pub struct User<'a, T: AlgebraicBody> {
    secret: i64,
    dhsetup: &'a DHSetup<T>,
    key: Option<T>,
}

impl<'a, T: AlgebraicBody> User<'a, T> {
    pub fn new<'b: 'a>(dhsetup: &'b DHSetup<T>) -> Self {
        let secret = gen_key(64);
        println!("secret: {}", secret);
        Self { secret, dhsetup, key: None}
    }
    pub fn get_public_key(&self) -> T {
        DHSetup::power(self.dhsetup.get_generator(), self.secret)
    }
    pub fn set_key(&mut self, a: T) {
        self.key = Some(DHSetup::power(a, self.secret));
        println!("key: {:?}", self.key);
    }
    pub fn encrypt(&self, m: T) -> T {
        m * self.key.unwrap()
    }
    pub fn decrypt(&self, c: T) -> T {
        c / self.key.unwrap()
    }
}

