extern crate syntex_syntax;

use std::env;
use std::fs::File;
use std::path::Path;
use syntex_syntax::parse;
use syntex_syntax::parse::ParseSess;
use syntex_syntax::print::{pp, pprust};

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    for include_path in &[
        Path::new("src/java_jni/jni-include.rs"),
        Path::new("src/cpp/cpp-include.rs"),
    ] {
        let parse_sess = ParseSess::new();
        let mut parser = parse::new_parser_from_file(&parse_sess, include_path);
        let mut krate = match parser.parse_crate_mod() {
            Ok(x) => x,
            Err(mut diag) => {
                diag.emit();
                panic!("{}: {}: Can not parse {:?}", file!(), line!(), include_path);
            }
        };

        let out =
            File::create(Path::new(&out_dir).join(include_path.file_name().unwrap())).unwrap();
        let mut printer = pprust::rust_printer(Box::new(out));

        for item in krate.module.items.drain(..) {
            let mut item = item.unwrap();
            item.attrs
                .retain(|attr| !attr.value.name.as_str().starts_with("swig_"));
            printer.print_item(&item).unwrap();
        }
        pp::eof(&mut printer.s).unwrap();
        println!("cargo:rerun-if-changed={}", include_path.display());
    }
    println!("cargo:rerun-if-changed={}", out_dir);
    println!("cargo:rerun-if-changed=tests/test_includes_syntax.rs");
}
