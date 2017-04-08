extern crate bindgen;
extern crate syntex;
extern crate rust_swig;

use std::process::{Command, Stdio};
use std::env;
use std::io::prelude::*;
use std::path::{Path, PathBuf};

fn get_gcc_system_include_dirs() -> Result<Vec<PathBuf>, String> {
    const GCC_CMD: &'static str = "arm-linux-androideabi-gcc";

    let gcc_process = try!(Command::new(GCC_CMD)
                               .args(&["-v", "-x", "c", "-E", "-"])
                               .stderr(Stdio::piped())
                               .stdin(Stdio::piped())
                               .stdout(Stdio::inherit())
                               .spawn()
                               .map_err(|err| err.to_string()));

    try!(try!(gcc_process
                  .stdin
                  .ok_or(format!("can not get stdin of {}", GCC_CMD).as_str()))
                 .write_all("\n".as_bytes())
                 .map_err(|err| err.to_string()));

    let mut gcc_output = String::new();
    try!(try!(gcc_process
                  .stderr
                  .ok_or(format!("can not get stderr of {}", GCC_CMD).as_str()))
                 .read_to_string(&mut gcc_output)
                 .map_err(|err| err.to_string()));

    const BEGIN_PAT: &'static str = "\n#include <...> search starts here:\n";
    const END_PAT: &'static str = "\nEnd of search list.\n";
    let start_includes =
        try!(gcc_output
                 .find(BEGIN_PAT)
                 .ok_or(format!("No '{}' in output from {}", BEGIN_PAT, GCC_CMD).as_str())) +
        BEGIN_PAT.len();
    let end_includes =
        try!((&gcc_output[start_includes..])
                 .find(END_PAT)
                 .ok_or(format!("No '{}' in output from {}", END_PAT, GCC_CMD).as_str())) +
        start_includes;

    Ok((&gcc_output[start_includes..end_includes])
           .split('\n')
           .map(|s| PathBuf::from(s.trim().to_string()))
           .collect())
}

fn search_file_in_directory<P>(dirs: &[P], file: &str) -> Result<PathBuf, ()>
    where P: AsRef<Path>
{
    for dir in dirs {
        let file_path = dir.as_ref().join(file);
        if file_path.exists() && file_path.is_file() {
            return Ok(file_path);
        }
    }
    Err(())
}

pub fn gen_binding<P>(target: &str,
                      include_dirs: &[P],
                      c_headers: &[&str],
                      output_rust: &Path)
                      -> Result<(), String>
    where P: AsRef<Path>
{
    assert!(!c_headers.is_empty());
    let c_file_path = search_file_in_directory(include_dirs, c_headers[0])
                           .map_err(|_| format!("Can not find {}", c_headers[0]))?;

    if let Ok(out_meta) = output_rust.metadata() {
        let mut res_recent_enough = true;
        for header in c_headers.iter() {
            let c_file_path = search_file_in_directory(include_dirs, header)
                .map_err(|_| format!("Can not find {}", header))?;
            let c_meta = c_file_path.metadata().map_err(|err| err.to_string())?;
            if !(c_meta.modified().unwrap() < out_meta.modified().unwrap()) {
                res_recent_enough = false;
                break;
            }
        }
        if res_recent_enough {
            return Ok(());
        }
    }

    let mut bindings: bindgen::Builder = bindgen::builder().header(c_file_path.to_str().unwrap());
    bindings = include_dirs
        .iter()
        .fold(bindings,
              |acc, x| acc.clang_arg("-I".to_string() + x.as_ref().to_str().unwrap()));

    bindings = bindings
        .no_unstable_rust()
        .hide_type("max_align_t")//long double not supported yet, see https://github.com/servo/rust-bindgen/issues/550
        .raw_line("#![allow(non_upper_case_globals, dead_code, non_camel_case_types, improper_ctypes, non_snake_case)]")
        ;
    bindings = if target.contains("windows") {
        //see https://github.com/servo/rust-bindgen/issues/578
        bindings.trust_clang_mangling(false)
    } else {
        bindings
    };
    bindings = c_headers[1..].iter()
        .fold(Ok(bindings),
              |acc: Result<bindgen::Builder, String>, header| {
                  let c_file_path = search_file_in_directory(include_dirs, header)
                               .map_err(|_| format!("Can not find {}", header))?;
                  let c_file_str =
                      c_file_path.to_str()
                          .ok_or_else(|| format!("Invalid unicode in path to {}", header))?;
                  Ok(acc.unwrap().clang_arg("-include").clang_arg(c_file_str))
              })?;

    let generated_bindings = bindings.generate()
        .map_err(|_| "Failed to generate bindings".to_string())?;
    generated_bindings.write_to_file(output_rust)
        .map_err(|err| err.to_string())?;

    Ok(())
}

fn main() {
    let include_dirs = get_gcc_system_include_dirs().expect("Can get gcc's system include dirs");
    let target = env::var("TARGET").unwrap();
    gen_binding(&target,
                &include_dirs,
                &["jni.h", "android/bitmap.h"],
                &Path::new("src/android_c_headers.rs"))
            .unwrap();
    let mut registry = syntex::Registry::new();
    let swig_gen =
        rust_swig::Generator::new(rust_swig::LanguageConfig::Java {
            output_dir:
            Path::new("app")
                .join("src")
                .join("main")
                .join("java")
                .join("net")
                .join("akaame")
                .join("myapplication"),
            package_name: "net.akaame.myapplication".into(),
        });
    swig_gen.register(&mut registry);
    let src = Path::new("src/java_glue.rs.in");
    let dst = Path::new(&env::var("OUT_DIR").unwrap()).join("java_glue.rs");
    registry
        .expand("rust_swig_test_jni", &src, &dst)
        .unwrap();

    let cur_dir = env::current_dir().unwrap();
    let arm_libs_path = cur_dir.join("src/obj/local/armeabi");
    println!("cargo:rustc-link-search=native={}",
             arm_libs_path.to_str().unwrap());
    println!("cargo:rustc-link-lib=jnigraphics");
    println!("cargo:rustc-link-lib=gnustl_shared");
    println!("cargo:rustc-link-lib=atomic");
}
