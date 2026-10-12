use std::{env, fs, path::PathBuf};

use flapigen::{Generator, JavaConfig, LanguageConfig};

fn main() {
    let out_dir = PathBuf::from(env::var_os("OUT_DIR").expect("Cargo sets OUT_DIR"));
    let java_dir = out_dir.join("java");
    fs::create_dir_all(&java_dir).expect("create generated Java directory");
    let config = JavaConfig::new(java_dir, "miri_tests".into());
    Generator::new(LanguageConfig::JavaConfig(config)).expand(
        "java_miri_tests",
        "src/glue.rs.in",
        &out_dir.join("glue.rs"),
    );
    println!("cargo:rerun-if-changed=src/glue.rs.in");
}
