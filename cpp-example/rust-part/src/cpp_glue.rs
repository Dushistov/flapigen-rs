#![allow(
    clippy::enum_variant_names,
    clippy::unused_unit,
    clippy::blacklisted_name,
    clippy::let_unit_value,
    clippy::not_unsafe_ptr_arg_deref,
    clippy::borrowed_box
)]
// ANCHOR: connect
include!(concat!(env!("OUT_DIR"), "/cpp_glue.rs"));
// ANCHOR_END: connect
