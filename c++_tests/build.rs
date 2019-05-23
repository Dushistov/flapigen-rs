use std::{env, path::Path};

use rust_swig::{CppConfig, LanguageConfig};

fn main() {
    env_logger::init();

    let out_dir = env::var("OUT_DIR").unwrap();

    let cpp_gen_path = Path::new("c++").join("rust_interface");
    println!("cargo:rerun-if-changed={}", cpp_gen_path.display());
    let cpp_cfg = if cfg!(feature = "boost") {
        CppConfig::new(cpp_gen_path, "rust".into()).use_boost()
    } else {
        CppConfig::new(cpp_gen_path, "rust".into())
    };

    let swig_gen = rust_swig::Generator::new(LanguageConfig::CppConfig(cpp_cfg));
    swig_gen.expand(
        "rust_swig_test_c++",
        Path::new("src/cpp_glue.rs.in"),
        &Path::new(&out_dir).join("cpp_glue.rs"),
    );
    println!(
        "cargo:rerun-if-changed={}",
        Path::new("src").join("cpp_glue.rs.in").display()
    );
}
