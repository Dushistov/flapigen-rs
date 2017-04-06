extern crate syntex;
extern crate rust_swig;
extern crate env_logger;
extern crate bindgen;

use std::env;
use std::path::{Path, PathBuf};

fn search_file_in_directory(dirs: &[&Path], file: &str) -> Result<PathBuf, ()> {
    for dir in dirs.iter() {
        let file_path = dir.join(file);
        if file_path.exists() && file_path.is_file() {
            return Ok(file_path);
        }
    }
    Err(())
}

fn gen_binding(include_dirs: &[&Path], c_headers: &[&str], output_rust: &Path) -> Result<(), String> {
    assert!(!c_headers.is_empty());
    let c_file_path = try!(search_file_in_directory(include_dirs, c_headers[0]).map_err(|_| format!("Can not find {}", c_headers[0])));

    if let Ok(out_meta) = output_rust.metadata() {
        let mut res_recent_enough = true;
        for header in c_headers.iter() {
            let c_file_path = try!(search_file_in_directory(include_dirs, header)
                                   .map_err(|_| format!("Can not find {}", header)));
            let c_meta = try!(c_file_path.metadata().map_err(|err| err.to_string()));
            if !(c_meta.modified().unwrap() < out_meta.modified().unwrap()) {
                res_recent_enough = false;
                break;
            }
        }
        if res_recent_enough {
            return Ok(());
        }
    }

    let mut bindings: ::bindgen::Builder = bindgen::builder().header(c_file_path.to_str().unwrap());
    bindings = include_dirs.iter().fold(bindings, |acc, x| acc.clang_arg("-I".to_string() + x.to_str().unwrap()));

    bindings = bindings
        .no_unstable_rust()
        .raw_line(r#"
#![allow(non_upper_case_globals, dead_code, non_camel_case_types, improper_ctypes, non_snake_case)]
"#)
        ;
    bindings = try!(c_headers[1..].iter().fold(Ok(bindings), |acc: Result<::bindgen::Builder, String>, header| {
        let c_file_path = try!(search_file_in_directory(include_dirs, header)
                               .map_err(|_| format!("Can not find {}", header)));
        let c_file_str = try!(c_file_path.to_str()
                              .ok_or_else(|| format!("Invalid unicode in path to {}", header)));
        Ok(acc.unwrap().clang_arg("-include").clang_arg(c_file_str))
    }));
    
    let generated_bindings = try!(bindings.generate().map_err(|_| "Failed to generate bindings".to_string()));
    try!(generated_bindings.write_to_file(output_rust).map_err(|err| err.to_string()));

    Ok(())
}

fn main() {
    env_logger::init().unwrap();

    let java_home = env::var("JAVA_HOME").expect("JAVA_HOME env variable not settted");
    let mut java_include_dir = PathBuf::new();
    java_include_dir.push(java_home);
    java_include_dir.push("include");

    let mut java_sys_include_dir = java_include_dir.clone();
    let target = env::var("TARGET").expect("target env var not setted");
    java_sys_include_dir.push(
        if target.contains("windows") {
            "win32"
        } else {
            "linux"
        });
    
    gen_binding(&[&java_include_dir, &java_sys_include_dir], &["jni.h"], &Path::new("src/jni_c_header.rs"))
        .expect("generate binding for c failed");
    
    let mut registry = syntex::Registry::new();
    let swig_gen = rust_swig::GeneratorBuilder::default()
        .java_output_dir(Path::new("src").join("com").join("example"))
        .java_package_name("com.example".into())
        .build()
        .unwrap();
    swig_gen.register(&mut registry);
    let src = Path::new("src/lib.rs.in");
    let dst = Path::new(&env::var("OUT_DIR").unwrap()).join("lib.rs");
    registry.expand("rust_swig_test_jni", &src, &dst).unwrap();
}
