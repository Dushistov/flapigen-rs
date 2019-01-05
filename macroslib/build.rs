use std::{env, path::Path};

use quote::{quote, ToTokens};
use syn::{parse_quote, visit_mut::VisitMut};

struct FilterSwigAttrs;

impl VisitMut for FilterSwigAttrs {
    fn visit_attribute_mut(&mut self, i: &mut syn::Attribute) {
        if i.path
            .clone()
            .into_token_stream()
            .to_string()
            .starts_with("swig_")
        {
            *i = syn::parse_quote! { #[doc = "swig_ replace"] };
        }
    }
}

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    for include_path in &[
        Path::new("src/java_jni/jni-include.rs"),
        Path::new("src/cpp/cpp-include.rs"),
    ] {
        let src_cnt = std::fs::read_to_string(include_path)
            .expect(&format!("Error during read {}", include_path.display()));
        let mut file = syn::parse_file(&src_cnt)
            .expect(&format!("Error during parse {}", include_path.display()));

        let mut filter_swig_attrs = FilterSwigAttrs;
        filter_swig_attrs.visit_file_mut(&mut file);

        let out_path = Path::new(&out_dir).join(include_path.file_name().expect("No file name"));
        std::fs::write(&out_path, file.into_token_stream().to_string()).expect(&format!(
            "Error during write to file {}",
            out_path.display()
        ));
        println!("cargo:rerun-if-changed={}", out_path.display());
        println!("cargo:rerun-if-changed={}", include_path.display());
    }
    println!("cargo:rerun-if-changed=tests/test_includes_syntax.rs");
}
