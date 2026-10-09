//! Rust-driven tests of generated JNI glue, with a Rust JNI function table.
#![allow(dead_code, non_snake_case)]

use jni_sys::*;

include!(concat!(env!("OUT_DIR"), "/glue.rs"));

#[cfg(test)]
mod tests;
