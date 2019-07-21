#![allow(unused_macros)]

mod jni {
    use jni_sys::*;
    use std::{
        cell::{Ref, RefCell, RefMut},
        path::Path,
        rc::Rc,
        sync::{Arc, Mutex, MutexGuard},
    };

    include!(concat!(env!("OUT_DIR"), "/jni-include.rs"));
}

mod cpp {
    use std::{
        cell::{Ref, RefCell, RefMut},
        path::Path,
        rc::Rc,
        sync::{Arc, Mutex, MutexGuard},
    };

    include!(concat!(env!("OUT_DIR"), "/cpp-include.rs"));
}

#[test]
fn test_includes_syntax_ok() {}
