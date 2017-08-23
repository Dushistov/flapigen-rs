extern crate rust_swig;
extern crate syntex;
extern crate tempdir;
extern crate env_logger;
extern crate regex;

use std::fs;
use std::path::Path;
use regex::Regex;
use tempdir::TempDir;

#[macro_use]
#[path = "../src/test_helper.rs"]
mod test_helper;

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
    static_method r_test_u8(v: u8) -> Result<u8, &'static str>;
});
"#,
    );
}

#[test]
fn test_string_containers() {
    parse_code(
        "test_string_array",
        r#"
foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::default() -> Foo;
    method Foo::list(&self) -> Vec<String>;
});"#,
    );
}

#[test]
fn test_int_array() {
    parse_code(
        "test_int_array",
        r#"
foreigner_class!(class Utils {
    static_method f(_: &[i32]) -> &[i32];
});
"#,
    );
}

#[test]
fn test_work_with_rc() {
    parse_code(
        "test_work_with_rc",
        r#"
foreigner_class!(class Boo {
    self_type Boo;
    constructor create_boo() -> Rc<RefCell<Boo>>;
    method Boo::test(&self, _: bool) -> f32;
    method Boo::set_a(&mut self, _: i32);
});
"#,
    );
}

#[test]
fn test_return_foreign_class() {
    let gen_code = parse_code(
        "test_work_with_rc",
        r#"
foreigner_class!(class Boo {
    self_type Boo;
    constructor create_boo() -> Rc<RefCell<Boo>>;
    method Boo::test(&self, _: bool) -> f32;
    method Boo::set_a(&mut self, _: i32);
});
foreigner_class!(class Moo {
    self_type Moo;
    constructor TestPathAndResult::empty() -> Result<Moo, String>;
    method TestPathAndResult::get_boo(&self) -> Rc<RefCell<Boo>>; alias getBoo;
});
"#,
    );
    let get_boo_re = Regex::new(
        r"fn\s+Java_com_example_Moo_do_1getBoo\([^\)]+\)\s*->\s*([[:alnum:]]+)\s*\{",
    ).expect("wrong regexp");
    let caps = get_boo_re.captures(&gen_code).unwrap();
    println!("{:?}", caps);
    assert_eq!("jobject", caps.get(1).unwrap().as_str());
}

#[test]
fn test_return_foreign_class_arc() {
    let gen_code = parse_code(
        "test_work_with_rc",
        r#"
foreigner_class!(class Boo {
    self_type Boo;
    constructor create_boo() -> Arc<Mutex<Boo>>;
    method Boo::test(&self, _: bool) -> f32;
    method Boo::set_a(&mut self, _: i32);
});
foreigner_class!(class Moo {
    self_type Moo;
    constructor TestPathAndResult::empty() -> Result<Moo, String>;
    method TestPathAndResult::get_boo(&self) -> Arc<Mutex<Boo>>; alias getBoo;
});
"#,
    );
    let get_boo_re = Regex::new(
        r"fn\s+Java_com_example_Moo_do_1getBoo\([^\)]+\)\s*->\s*([[:alnum:]]+)\s*\{",
    ).expect("wrong regexp");
    let caps = get_boo_re.captures(&gen_code).unwrap();
    println!("{:?}", caps);
    assert_eq!("jobject", caps.get(1).unwrap().as_str());
}

#[test]
fn test_pass_objects_as_param_simple() {
    parse_code(
        "test_pass_objects_as_param_simple",
        r#"
foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32, _: &str) -> Foo;
    method Foo::f(&self, _: i32, _: i32) -> i32;  alias calcF;
});

foreigner_class!(class TestPassObjectsAsParams {
    self_type TestPassObjectsAsParams;
    constructor TestPassObjectsAsParams::default() -> TestPassObjectsAsParams;
    method TestPassObjectsAsParams::f1(&self, _: &Foo);
    method TestPassObjectsAsParams::f2(&self, _: Foo);
    method TestPassObjectsAsParams::f3(&self, _: &mut Foo);
});
"#,
    );
}

#[test]
fn test_pass_objects_as_param() {
    parse_code(
        "test_pass_objects_as_param",
        r#"
foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32, _: &str) -> Rc<RefCell<Foo>>;
    method Foo::f(&self, _: i32, _: i32) -> i32;
});

foreigner_class!(class TestPassObjectsAsParams {
    self_type TestPassObjectsAsParams;
    constructor TestPassObjectsAsParams::default() -> TestPassObjectsAsParams;
    method TestPassObjectsAsParams::f1(&self, _: &RefCell<Foo>);
    method TestPassObjectsAsParams::f2(&self, _: Rc<RefCell<Foo>>);
    method TestPassObjectsAsParams::f3(&self, _: &mut RefCell<Foo>);
});
"#,
    );
}

fn parse_code(test_name: &str, code: &str) -> String {
    test_helper::logger_init();
    let tmp_dir = TempDir::new(test_name).expect("Can not create tmp directory");
    println!(
        "{}: test name {} tmp_dir {:?}",
        file!(),
        test_name,
        tmp_dir.path()
    );
    let java_path = tmp_dir.path().join("java");
    fs::create_dir_all(&java_path).unwrap_or_else(|why| {
        panic!("! {:?}", why.kind());
    });
    let mut registry = syntex::Registry::new();
    let swig_gen = rust_swig::Generator::new(rust_swig::LanguageConfig::Java {
        output_dir: Path::new("-").into(),
        package_name: "com.example".into(),
    });
    swig_gen.register(&mut registry);
    let res_code = registry.expand_str(test_name, "use_case", code).unwrap();
    println!("res_code: {}", res_code);
    res_code
}
