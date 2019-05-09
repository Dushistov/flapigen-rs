
#[macro_use] extern crate cpython;
use cpython::{ObjectProtocol, PyObject};

mod swig_foreign_types_map {
    #![swig_foreigner_type = "()"]
    #![swig_rust_type = "()"]
    #![swig_foreigner_type = "bool"]
    #![swig_rust_type = "bool"]
    #![swig_foreigner_type = "i8"]
    #![swig_rust_type = "i8"]
    #![swig_foreigner_type = "i16"]
    #![swig_rust_type = "i16"]
    #![swig_foreigner_type = "i32"]
    #![swig_rust_type = "i32"]
    #![swig_foreigner_type = "u8"]
    #![swig_rust_type = "u8"]
    #![swig_foreigner_type = "u16"]
    #![swig_rust_type = "u16"]
    #![swig_foreigner_type = "u32"]
    #![swig_rust_type = "u32"]
    #![swig_foreigner_type = "f32"]
    #![swig_rust_type = "f32"]
    #![swig_foreigner_type = "f64"]
    #![swig_rust_type = "f64"]
    #![swig_foreigner_type = "String"]
    #![swig_rust_type = "String"]
}
