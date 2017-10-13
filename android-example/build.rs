extern crate bindgen;
extern crate rust_swig;
extern crate syntex;

use std::process::{Command, Stdio};
use std::{env, fmt};
use std::io::prelude::*;
use std::path::{Path, PathBuf};

use bindgen::RustTarget;
use rust_swig::{JavaConfig, LanguageConfig};

fn main() {
    let target = env::var("TARGET").unwrap();
    let include_dirs =
        get_gcc_system_include_dirs(&target).expect("Can get gcc's system include dirs");
    for dir in &include_dirs {
        println!("cargo:rerun-if-changed={}", dir.display());
    }
    let jni_h = search_file_in_directory(&include_dirs, "jni.h").expect("Can not find jni.h");
    let bitmap_h = search_file_in_directory(&include_dirs, "android/bitmap.h")
        .expect("Can not find android/bitmap.h");

    let out_dir = env::var("OUT_DIR").unwrap();

    gen_binding(
        &target,
        &include_dirs,
        &[bitmap_h, jni_h],
        &Path::new(&out_dir).join("android_c_headers.rs"),
    ).unwrap();

    rust_swig_expand(
        Path::new("src/java_glue.rs.in"),
        &Path::new(&out_dir).join("java_glue.rs"),
    ).unwrap();
    println!("cargo:rerun-if-changed=src");
    //if for some reason generated files were deleted, regenerate them
    println!("cargo:rerun-if-changed={}", out_dir);

    let cur_dir = env::current_dir().unwrap();
    let arm_libs_path = cur_dir.join("src/obj/local/armeabi");
    println!(
        "cargo:rustc-link-search=native={}",
        arm_libs_path.to_str().unwrap()
    );
    println!("cargo:rustc-link-lib=jnigraphics");
    println!("cargo:rustc-link-lib=gnustl_shared");
    println!("cargo:rustc-link-lib=atomic");
}

fn get_gcc_system_include_dirs(target: &str) -> Result<Vec<PathBuf>, String> {
    let gcc_cmd = if target == "i686-linux-android" {
        "i686-linux-android-gcc"
    } else {
        "arm-linux-androideabi-gcc"
    };

    let gcc_process = Command::new(gcc_cmd)
        .args(&["-v", "-x", "c", "-E", "-"])
        .stderr(Stdio::piped())
        .stdin(Stdio::piped())
        .stdout(Stdio::inherit())
        .spawn()
        .map_err(|err| err.to_string())?;

    gcc_process
        .stdin
        .ok_or(format!("can not get stdin of {}", gcc_cmd).as_str())?
        .write_all("\n".as_bytes())
        .map_err(|err| err.to_string())?;

    let mut gcc_output = String::new();
    gcc_process
        .stderr
        .ok_or(format!("can not get stderr of {}", gcc_cmd).as_str())?
        .read_to_string(&mut gcc_output)
        .map_err(|err| err.to_string())?;

    const BEGIN_PAT: &'static str = "\n#include <...> search starts here:\n";
    const END_PAT: &'static str = "\nEnd of search list.\n";
    let start_includes = gcc_output.find(BEGIN_PAT).ok_or(
        format!("No '{}' in output from {}", BEGIN_PAT, gcc_cmd).as_str(),
    )? + BEGIN_PAT.len();
    let end_includes = (&gcc_output[start_includes..]).find(END_PAT).ok_or(
        format!("No '{}' in output from {}", END_PAT, gcc_cmd).as_str(),
    )? + start_includes;

    Ok(
        (&gcc_output[start_includes..end_includes])
            .split('\n')
            .map(|s| PathBuf::from(s.trim().to_string()))
            .collect(),
    )
}

fn search_file_in_directory<P>(dirs: &[P], file: &str) -> Result<PathBuf, ()>
where
    P: AsRef<Path>,
{
    for dir in dirs {
        let file_path = dir.as_ref().join(file);
        if file_path.exists() && file_path.is_file() {
            return Ok(file_path);
        }
    }
    Err(())
}

fn gen_binding<P1, P2>(
    target: &str,
    include_dirs: &[P1],
    c_headers: &[P2],
    output_rust: &Path,
) -> Result<(), String>
where
    P1: AsRef<Path> + fmt::Debug,
    P2: AsRef<Path> + fmt::Debug,
{
    assert!(!c_headers.is_empty());
    let c_file_path = &c_headers[0];

    let mut bindings: bindgen::Builder =
        bindgen::builder().header(c_file_path.as_ref().to_str().unwrap());
    bindings = include_dirs.iter().fold(bindings, |acc, x| {
        acc.clang_arg("-I".to_string() + x.as_ref().to_str().unwrap())
    });
    println!("Generate binding for {:?}", c_headers);
    bindings = bindings
        .rust_target(RustTarget::Stable_1_19)
        //long double not supported yet, see https://github.com/servo/rust-bindgen/issues/550
        .hide_type("max_align_t");
    bindings = if target.contains("windows") {
        //see https://github.com/servo/rust-bindgen/issues/578
        bindings.trust_clang_mangling(false)
    } else {
        bindings
    };
    bindings = c_headers[1..].iter().fold(
        Ok(bindings),
        |acc: Result<bindgen::Builder, String>, header| {
            let c_file_path = header;
            let c_file_str = c_file_path.as_ref().to_str().ok_or_else(|| {
                format!("Invalid unicode in path to {:?}", c_file_path.as_ref())
            })?;
            Ok(acc.unwrap().clang_arg("-include").clang_arg(c_file_str))
        },
    )?;

    let generated_bindings = bindings
//        .clang_arg(format!("-target {}", target))
        .generate()
        .map_err(|_| "Failed to generate bindings".to_string())?;
    generated_bindings
        .write_to_file(output_rust)
        .map_err(|err| err.to_string())?;

    Ok(())
}

fn rust_swig_expand(from: &Path, out: &Path) -> Result<(), String> {
    let mut registry = syntex::Registry::new();
    let swig_gen = rust_swig::Generator::new(LanguageConfig::JavaConfig(
        JavaConfig::new(
            Path::new("app")
                .join("src")
                .join("main")
                .join("java")
                .join("net")
                .join("akaame")
                .join("myapplication"),
            "net.akaame.myapplication".into(),
        ).use_null_annotation("android.support.annotation.NonNull".into()),
    ));
    swig_gen.register(&mut registry);
    registry
        .expand("rust_swig_test_jni", from, out)
        .map_err(|err| format!("rust swig expand failed: {}", err))
}
