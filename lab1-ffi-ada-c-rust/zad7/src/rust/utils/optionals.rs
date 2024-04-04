#[repr(C)]
pub struct opt_i32 {
    pub value : i32,
    pub is_present : bool,
}

#[repr(C)]
pub struct opt_i64 {
    pub value : i64,
    pub is_present : bool,
}
