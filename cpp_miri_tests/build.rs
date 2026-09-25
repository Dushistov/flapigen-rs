use std::{env, fs, path::PathBuf};

use flapigen::{CppConfig, CppOptional, CppStrView, CppVariant, Generator, LanguageConfig};

fn main() {
    let out_dir = PathBuf::from(env::var_os("OUT_DIR").expect("Cargo sets OUT_DIR"));
    let headers = out_dir.join("include");
    fs::create_dir_all(&headers).expect("create generated header directory");
    let config = CppConfig::new(headers, "miri_tests".into())
        .cpp_optional(CppOptional::Std17)
        .cpp_variant(CppVariant::Std17)
        .cpp_str_view(CppStrView::Std17);
    Generator::new(LanguageConfig::CppConfig(config)).expand(
        "cpp_miri_tests",
        "src/glue.rs.in",
        &out_dir.join("glue.rs"),
    );
    println!("cargo:rerun-if-changed=src/glue.rs.in");
}
