extern crate rust_swig;
extern crate syntex;
extern crate tempdir;

use std::fs;
use tempdir::TempDir;



#[test]
fn test_own_objects_creation() {
    parse_code(
        "own_objects_creation",
        r#"
foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32) -> Foo;
    method Foo::f(&self, _: i32, _: i32) -> i32;
});

foreigner_class!(class Boo {
    self_type Boo;

    constructor Boo::new(_: i32, _: usize) -> Result<Boo, String>;
    method Boo::get_one_foo(&self) -> Foo;
});
"#,
    );
}

#[test]
fn test_generic() {
    parse_code(
        "test_generic",
        r#"
foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32) -> Foo;
    method Foo::f(&self, _: i32, _: i32) -> i32;
});

foreigner_class!(class Boo {
    self_type Boo;

    constructor Boo::new(_: i32, _: usize) -> Result<Boo, String>;
    method Boo::get_foo_arr(&self) -> Vec<Foo>;
    method Boo::get_one_foo(&self) -> Result<Foo, String>;
    static_method now() -> SystemTime;
});
"#,
    );
}

#[test]
fn test_string_containers() {
    parse_code("test_string_array",
               r#"
foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::default() -> Foo;
    method Foo::list(&self) -> Vec<String>;
});"#);
}

fn parse_code(test_name: &str, code: &str) -> String {
    let tmp_dir = TempDir::new(test_name).expect("Can not create tmp directory");
    println!("{}: test name {} tmp_dir {:?}",
             file!(),
             test_name,
             tmp_dir.path());
    let java_path = tmp_dir.path().join("java");
    fs::create_dir_all(&java_path).unwrap_or_else(|why| {
                                                      panic!("! {:?}", why.kind());
                                                  });
    let mut registry = syntex::Registry::new();
    let swig_gen = rust_swig::Generator::new(rust_swig::LanguageConfig::Java {
                                                 output_dir: java_path,
                                                 package_name: "com.example".into(),
                                             });
    swig_gen.register(&mut registry);
    let res_code = registry.expand_str(test_name, "use_case", code).unwrap();
    println!("res_code: {}", res_code);
    res_code
}
