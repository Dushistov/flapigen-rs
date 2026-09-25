//! Rust-driven tests of the actual generated C++ backend glue.
//!
//! Foreign callbacks are implemented in Rust so Miri can interpret both sides.
#![allow(dead_code, non_snake_case)]

include!(concat!(env!("OUT_DIR"), "/glue.rs"));

#[cfg(test)]
mod tests;
