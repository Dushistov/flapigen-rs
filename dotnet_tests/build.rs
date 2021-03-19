use std::{env, path::Path, time::Instant};

use flapigen::{DotNetConfig, LanguageConfig};

fn main() {
    env_logger::init();

    let now = Instant::now();

    let out_dir = env::var("OUT_DIR").unwrap();
    flapigen_expand(
        Path::new("src/glue.rs.in"),
        &Path::new(&out_dir).join("glue.rs"),
    );
    let expand_time = now.elapsed();
    println!(
        "flapigen expand time: {}",
        expand_time.as_secs() as f64 + (expand_time.subsec_nanos() as f64) / 1_000_000_000.
    );
    println!("cargo:rerun-if-changed=src/glue.rs.in");
    println!("cargo:rerun-if-changed=src/lib.rs");
}

fn flapigen_expand(from: &Path, out: &Path) {
    println!("Run flapigen_expand");
    let config = DotNetConfig::new("flapigen_test_dotnet".to_owned(), "flapigen_test_dotnet".into());
    let swig_gen = flapigen::Generator::new(LanguageConfig::DotNetConfig(config));
    swig_gen.expand("flapigen_test_dotnet_native", from, out);
}
