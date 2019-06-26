use std::{env, io::Write, path::Path};

use quote::ToTokens;
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
            *i = parse_quote! { #[doc = "swig_ replace"] };
        }
    }
}

mod file_cache {
    include!("src/file_cache.rs");
}

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    for include_path in &[
        Path::new("src/java_jni/jni-include.rs"),
        Path::new("src/cpp/cpp-include.rs"),
    ] {
        let src_cnt_tail = std::fs::read_to_string(include_path)
            .expect(&format!("Error during read {}", include_path.display()));
        let mut src_cnt = r#"
        macro_rules! foreign_typemap {
            ($($tree:tt)*) => {};
        }
"#
        .to_string();

        src_cnt.push_str(&src_cnt_tail);

        let mut file = syn::parse_file(&src_cnt)
            .expect(&format!("Error during parse {}", include_path.display()));

        let mut filter_swig_attrs = FilterSwigAttrs;
        filter_swig_attrs.visit_file_mut(&mut file);

        let out_path = Path::new(&out_dir).join(include_path.file_name().expect("No file name"));
        let mut cache = file_cache::FileWriteCache::new(&out_path);
        let write_err_msg = format!("Error during write to file {}", out_path.display());
        write!(&mut cache, "{}", file.into_token_stream().to_string()).expect(&write_err_msg);
        cache.update_file_if_necessary().expect(&write_err_msg);
        println!("cargo:rerun-if-changed={}", include_path.display());
    }
    println!("cargo:rerun-if-changed=tests/test_includes_syntax.rs");
}
