/*
#![allow(unused_macros)]
#![allow(dead_code)]
extern crate jni_sys;
#[macro_use]
extern crate log;

mod jni {
    use jni_sys::*;
    use std::cell::{Ref, RefCell, RefMut};
    use std::path::Path;
    use std::rc::Rc;
    use std::sync::{Arc, Mutex, MutexGuard};
    use std::time::SystemTime;

    include!(concat!(env!("OUT_DIR"), "/jni-include.rs"));
}
mod cpp {
    use std::cell::{Ref, RefCell, RefMut};
    use std::path::Path;
    use std::rc::Rc;
    use std::sync::{Arc, Mutex, MutexGuard};
    include!(concat!(env!("OUT_DIR"), "/cpp-include.rs"));
}

#[test]
fn test_includes_syntax_ok() {}
*/
