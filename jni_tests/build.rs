extern crate syntex;
extern crate rust_swig;
extern crate env_logger;
extern crate bindgen;

use std::env;
use std::path::{Path, PathBuf};

fn main() {
    env_logger::init().unwrap();

    let java_home = env::var("JAVA_HOME").expect("JAVA_HOME env variable not settted");

    let java_include_dir = Path::new(&java_home).join("include");

    let target = env::var("TARGET").expect("target env var not setted");
    let java_sys_include_dir = java_include_dir.join(if target.contains("windows") {
        "win32"
    } else {
        "linux"
    });

    let include_dirs = [java_include_dir, java_sys_include_dir];
    println!("jni include dirs {:?}", include_dirs);
    for dir in &include_dirs {
        println!("cargo:rerun-if-changed={}", dir.display());
    }

    let jni_h_path =
        search_file_in_directory(&include_dirs[..], "jni.h").expect("Can not find jni.h");

    let out_dir = env::var("OUT_DIR").unwrap();

    gen_binding(
        &include_dirs[..],
        &jni_h_path,
        &Path::new(&out_dir).join("jni_c_header.rs"),
    ).unwrap();

    rust_swig_expand(
        Path::new("src/lib.rs.in"),
        &Path::new(&out_dir).join("lib.rs"),
    ).unwrap();
    println!("cargo:rerun-if-changed=src");

    //rebuild if user remove generated code
    println!("cargo:rerun-if-changed={}", out_dir);
}

fn search_file_in_directory<P: AsRef<Path>>(dirs: &[P], file: &str) -> Result<PathBuf, ()> {
    for dir in dirs {
        let dir = dir.as_ref().to_path_buf();
        let file_path = dir.join(file);
        if file_path.exists() && file_path.is_file() {
            return Ok(file_path);
        }
    }
    Err(())
}

fn gen_binding<P: AsRef<Path>>(
    include_dirs: &[P],
    c_file_path: &Path,
    output_rust: &Path,
) -> Result<(), String> {
    let mut bindings: ::bindgen::Builder = bindgen::builder().header(c_file_path.to_str().unwrap());
    bindings = include_dirs.iter().fold(bindings, |acc, x| {
        acc.clang_arg("-I".to_string() + x.as_ref().to_str().unwrap())
    });

    bindings = bindings.unstable_rust(false);

    let generated_bindings = bindings
        .generate()
        .map_err(|_| "Failed to generate bindings".to_string())?;
    generated_bindings
        .write_to_file(output_rust)
        .map_err(|err| err.to_string())?;

    Ok(())
}

fn rust_swig_expand(from: &Path, out: &Path) -> Result<(), String> {
    println!("Run rust_swig_expand");
    let mut registry = syntex::Registry::new();
    let swig_gen = rust_swig::Generator::new(rust_swig::LanguageConfig::Java {
        output_dir: Path::new("java").join("com").join("example").join("rust"),
        package_name: "com.example.rust".into(),
    });
    swig_gen.register(&mut registry);
    registry
        .expand("rust_swig_test_jni", from, out)
        .map_err(|err| format!("rust swig macros expand failed: {}", err))
}
