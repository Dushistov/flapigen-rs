extern crate syntex;
extern crate rust_swig;

use std::env;
use std::path::Path;

fn main() {
    let mut registry = syntex::Registry::new();
    rust_swig::register(&mut registry);

    let src = Path::new("src/lib.rs.in");
    let dst = Path::new(&env::var("OUT_DIR").unwrap()).join("lib.rs");
    env::set_var("RUST_SWIG_JNI_JAVA_PACKAGE", "com.example");
    env::set_var("RUST_SWIG_JNI_JAVA_OUTPUT_DIR", "src/com/example");
    registry.expand("rust_swig_test_jni", &src, &dst).unwrap();
}
