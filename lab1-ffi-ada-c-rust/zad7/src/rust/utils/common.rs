#[repr(C)]
pub struct diophantine_eq {
    pub x_k: i32,
    pub y_k: i32,
    pub c: i32
}

#[repr(C)]
pub struct diophantine_sol {
    pub x_k: i32,
    pub x_a: i32,
    pub y_k: i32,
    pub y_a: i32,
    pub is_present: bool
}
