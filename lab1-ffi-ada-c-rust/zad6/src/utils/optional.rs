#[repr(C)]
pub struct OptI32 {
    pub value : i32,
    pub is_present : bool,
}

#[repr(C)]
pub struct OptI64 {
    pub value : i64,
    pub is_present : bool,
}
