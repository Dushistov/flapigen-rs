use rust_swig::{CppConfig, CppOptional, CppStrView, CppVariant, LanguageConfig};
use std::{env, path::Path};

fn main() {
    let out_dir = env::var("OUT_DIR").expect("no OUT_DIR, but cargo should provide it");
    //ANCHOR: cpp_config
    let cpp_cfg = CppConfig::new(
        // ANCHOR: cpp_output
        Path::new("..").join("cpp-part").join("rust-api"),
        // ANCHOR_END: cpp_output
        "rust".into(),
    )
    .cpp_optional(CppOptional::Boost)
    .cpp_variant(CppVariant::Boost)
    .cpp_str_view(CppStrView::Boost);
    //ANCHOR_END: cpp_config
    let swig_gen = rust_swig::Generator::new(LanguageConfig::CppConfig(cpp_cfg));
    swig_gen.expand(
        "c++-api-for-rust",
        // ANCHOR: rust_input
        Path::new("src/cpp_glue.rs.in"),
        // ANCHOR_END: rust_input

        // ANCHOR: rust_output
        &Path::new(&out_dir).join("cpp_glue.rs"),
        // ANCHOR_END: rust_output
    );
    println!(
        "cargo:rerun-if-changed={}",
        Path::new("src").join("cpp_glue.rs.in").display()
    );
}
