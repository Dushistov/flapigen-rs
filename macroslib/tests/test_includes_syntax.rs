#![allow(unused_macros)]

mod jni {
    use jni_sys::*;

    include!(concat!(env!("OUT_DIR"), "/jni-include.rs"));
}

mod cpp {
    include!(concat!(env!("OUT_DIR"), "/cpp-include.rs"));
    include!(concat!(env!("OUT_DIR"), "/cpp-include-slice.rs"));
    include!(concat!(env!("OUT_DIR"), "/cpp-include-vec.rs"));
}

#[test]
fn test_includes_syntax_ok() {}
