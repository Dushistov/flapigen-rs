#[no_mangle]
pub extern "C" fn add(lhs: u32, rhs: u32) -> u32 {
    lhs + rhs
}
