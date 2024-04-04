#[derive(Clone)]
pub struct DiophantineEq {
    pub x_k: i32,
    pub y_k: i32,
    pub c: i32
}

#[derive(Default)]
pub struct DiophantineSol {
    pub x_k: i32,
    pub x_a: i32,
    pub y_k: i32,
    pub y_a: i32,
    pub is_present: bool
}