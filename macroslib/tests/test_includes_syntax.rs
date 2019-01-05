#![allow(unused_macros)]
#![allow(dead_code)]
use log::error;

mod jni {
    use super::*;
    use jni_sys::*;
    use std::{
        cell::{Ref, RefCell, RefMut},
        path::Path,
        rc::Rc,
        sync::{Arc, Mutex, MutexGuard},
        time::SystemTime,
    };

    include!(concat!(env!("OUT_DIR"), "/jni-include.rs"));
}

mod cpp {
    use super::*;
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
