
use cpython::{ObjectProtocol as CPythonObjectProtocol, PythonObjectWithTypeObject as CPythonObjectWithTypeObject};

// It is currently unused.
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

fn swig_collect_error_message(error: &dyn std::error::Error) -> String {
    if let Some(source) = error.source() {
        format!("{}\nCaused by:\n{}", error, swig_collect_error_message(source))
    } else {
        error.to_string()
    }
}
