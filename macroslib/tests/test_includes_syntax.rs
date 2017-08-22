#![allow(unused_macros)]
#![allow(dead_code)]
extern crate jni_sys;
#[macro_use]
extern crate log;

use std::time::SystemTime;
use std::path::Path;
use std::sync::{Arc, Mutex, MutexGuard};
use std::rc::Rc;
use std::cell::{Ref, RefCell, RefMut};
use jni_sys::*;

include!(concat!(env!("OUT_DIR"), "/jni-include.rs"));

#[test]
fn test_includes_syntax_ok() {}
