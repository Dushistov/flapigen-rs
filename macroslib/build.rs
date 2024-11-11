use std::{
    env,
    fs::File,
    io::{BufRead, BufReader, Write},
    path::Path,
};

use quote::ToTokens;
use syn::{parse_quote, visit::Visit, visit_mut::VisitMut};

struct FilterSwigAttrs;

impl VisitMut for FilterSwigAttrs {
    fn visit_attribute_mut(&mut self, i: &mut syn::Attribute) {
        if i.path()
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

mod jni_find_cache {
    include!("src/java_jni/find_cache.rs");
}

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();

    for include_path in &[
        Path::new("src/java_jni/jni-include.rs"),
        Path::new("src/cpp/cpp-include.rs"),
    ] {
        let src_cnt_tail = std::fs::read_to_string(include_path)
            .unwrap_or_else(|err| panic!("Error during read {}: {}", include_path.display(), err));
        let mut src_cnt = r#"
        macro_rules! foreign_typemap {
            ($($tree:tt)*) => {};
        }
"#
        .to_string();

        src_cnt.push_str(&src_cnt_tail);

        let mut file = syn::parse_file(&src_cnt)
            .unwrap_or_else(|err| panic!("Error during parse {}: {}", include_path.display(), err));

        let mut filter_swig_attrs = FilterSwigAttrs;
        filter_swig_attrs.visit_file_mut(&mut file);

        let mut jni_cache_macro_cache = jni_find_cache::JniCacheMacroCalls::default();
        let mut visitor = jni_find_cache::JniCacheMacroCallsVisitor {
            inner: &mut jni_cache_macro_cache,
            errors: vec![],
        };
        visitor.visit_file(&file);
        if !visitor.errors.is_empty() {
            panic!("jni cache macros visiting failed: {}", visitor.errors[0]);
        }
        let mut jni_global_vars = jni_cache_macro_cache.global_vars();
        file.items.append(&mut jni_global_vars);

        let out_path = Path::new(&out_dir).join(include_path.file_name().expect("No file name"));
        let mut cache =
            file_cache::FileWriteCache::new(&out_path, &mut file_cache::NoNeedFsOpsRegistration);
        let write_err_msg = format!("Error during write to file {}", out_path.display());
        write!(&mut cache, "{}", file.into_token_stream()).expect(&write_err_msg);
        cache.update_file_if_necessary().expect(&write_err_msg);
        println!("cargo:rerun-if-changed={}", include_path.display());
    }
    println!("cargo:rerun-if-changed=tests/test_includes_syntax.rs");

    let exp_tests_list_path = Path::new("tests").join("expectations").join("tests.list");
    let expectation_tests = File::open(&exp_tests_list_path)
        .unwrap_or_else(|err| panic!("Can not open {}: {}", exp_tests_list_path.display(), err));
    let expectation_tests = BufReader::new(&expectation_tests);
    let exp_code_path = Path::new(&out_dir).join("test_expectations.rs");
    let mut exp_code =
        file_cache::FileWriteCache::new(&exp_code_path, &mut file_cache::NoNeedFsOpsRegistration);
    for name in expectation_tests.lines() {
        let name = name.unwrap_or_else(|err| {
            panic!("Can not read {}: {}", exp_tests_list_path.display(), err)
        });
        write!(
            &mut exp_code,
            r##"
#[test]
fn test_expectation_{test_name}() {{
   let _ = env_logger::try_init();

   let test_case = Path::new("tests").join("expectations").join("{test_name}.rs");
   let base_name = test_case.file_stem().expect("name without extenstion");
   let test_name = base_name.to_string_lossy();

   let mut test_something = false;
   for lang in &[ForeignLang::Cpp, ForeignLang::Java] {{
       if check_expectation(&test_name, &test_case, *lang) {{
           test_something = true;
       }}
   }}
   assert!(test_something, "empty test");
}}
"##,
            test_name = name,
        )
        .unwrap();
    }
    exp_code
        .update_file_if_necessary()
        .unwrap_or_else(|err| panic!("Can not write to {}: {}", exp_code_path.display(), err));
    println!("cargo:rerun-if-changed={}", exp_tests_list_path.display());
}
