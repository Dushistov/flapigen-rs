extern crate env_logger;
extern crate regex;
extern crate rust_swig;
extern crate syntex;
extern crate tempdir;

use std::fs;
use std::fs::File;
use std::io::Read;

use regex::Regex;
use tempdir::TempDir;
use rust_swig::{Generator, JavaConfig, LanguageConfig};
use syntex::Registry;

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
    static_method Boo::factory_method() -> Result<Boo, String>;
    method Boo::boo_as_arg(&self, _: Boo) -> i32;
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
    let (gen_code, _) = parse_code(
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
    let (gen_code, _) = parse_code(
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
    let (_, java_code) = parse_code(
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
    static_method TestPassObjectsAsParams::f4(_: &Foo);
    static_method TestPassObjectsAsParams::f5(_: Foo);
});
"#,
    );
    assert!(java_code.contains("public static void f4(Foo"));
    assert!(java_code.contains("public static void f5(Foo"));
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

#[test]
fn test_document_generated_code() {
    parse_code(
        "test_document_generated_code",
        r#"
foreigner_class!(
/// This is class Foo
class Foo {
    self_type Foo;
    /// Some documentation comment
    constructor Foo::new(_: i32, _: &str) -> Rc<RefCell<Foo>>;
    /// 1 Some documentation comment
    /// 2 Some documentation comment
    method Foo::f(&self, _: i32, _: i32) -> i32;
});
"#,
    );
}

#[test]
fn test_return_result_type_with_object() {
    for _ in 0..10 {
        let (_, java_code) = parse_code(
            "test_return_result_type_with_object",
            r#"
foreigner_class!(class Position {
    self_type GnssInfo;
    private constructor create_position() -> GnssInfo;
    method Position::timeStamp(&self) -> SystemTime;
    method Position::getLatitude(&self) -> f64;
});

foreigner_class!(class LocationService {
    static_method LocationService::position() -> Result<GnssInfo, &'static str>;
});
"#,
        );
        println!("{}", java_code);
        assert!(java_code.contains("public static native Position position() throws Exception;"));
    }
}

#[test]
fn test_foreign_interface() {
    parse_code(
        "test_foreign_interface",
        r#"
trait SomeTrait {
    fn on_state_changed(&self, item: i32, is_ok: bool);
}

foreign_interface!(interface SomeObserver {
    self_type SomeTrait;
    onStateChanged = SomeTrait::on_state_changed(&self, _: i32, _: bool);
});

foreigner_class!(class ClassWithCallbacks {
    self_type Foo;
    constructor Foo::default() -> Foo;
    method f1(&mut self, cb: Box<SomeTrait>);
});
"#,
    );
}

#[test]
fn test_foreign_enum_plus_interface() {
    parse_code(
        "test_foreign_enum_plus_interface",
        r#"
foreign_enum!(enum ControlItem {
    GNSS = ControlItem::GnssWorking,
    GPS_PROVIDER = ControlItem::AndroidGPSOn,
});

foreign_interface!(interface ControlStateObserver {
    self_type ControlStateChange;
    onSessionUpdate = ControlStateChange::on_state_changed(&self, item: ControlItem, is_ok: bool);
});
"#,
    );
}

#[test]
fn test_foreign_enum_vs_int() {
    for _ in 0..10 {
        let (rust_code, java_code) = parse_code(
            "test_foreign_enum_vs_int",
            r#"
foreign_enum!(enum MyEnum {
  ITEM1 = MyEnum::Item1,
  ITEM2 = MyEnum::Item2,
  ITEM3 = MyEnum::Item3,
});

foreigner_class!(class TestEnumClass {
    self_type Moo;
    constructor Moo::default() -> Moo;
    method Moo::f1(&mut self, v: MyEnum) -> i32;
    static_method Moo::next_enum(v: MyEnum) -> MyEnum;
});
"#,
        );
        println!("{}", rust_code);
        println!("{}", java_code);
        assert!(java_code.contains("int f1(MyEnum"));
    }
}

#[test]
fn test_static_func_with_foreign_class_as_param() {
    let (_, java_code) = parse_code(
        "static_func_with_foreign_class_as_param",
        r#"
foreigner_class!(class Boo {
    self_type Boo;
    constructor boo_init() -> Rc<RefCell<Boo>>;
});
foreigner_class!(class Foo {
    static_method static_foo(_: &Boo);
});
"#,
    );
    println!("{}", java_code);
    assert!(java_code.contains("public static void static_foo(Boo"));

    //add method
    let (_, java_code) = parse_code(
        "static_func_with_foreign_class_as_param",
        r#"
foreigner_class!(class Boo {
    self_type Boo;
    constructor boo_init() -> Rc<RefCell<Boo>>;
    method Boo::f1(&self);
});
foreigner_class!(class Foo {
    static_method static_foo(_: &Boo);
});
"#,
    );
    println!("{}", java_code);
    assert!(java_code.contains("public static void static_foo(Boo"));
}

#[test]
fn test_lifetime_param_in_result() {
    let (rust_code, _) = parse_code(
        "lifetime_param_in_result",
        r#"
foreigner_class!(class Foo {
    self_type Foo<'a>;
    constructor new<'a>() -> Rc<RefCell<Foo<'a>>>;
    method Foo::f(&self, _: i32);
});
"#,
    );
    println!("{}", rust_code);
    assert!(rust_code.contains("impl <'a> SwigForeignClass for Rc<RefCell<Foo<'a>>> {"));
}

#[test]
fn test_interface_with_str() {
    let (rust_code, java_code) = parse_code(
        "test_interface_with_str",
        r#"
foreign_interface!(interface SomeObserver {
    self_type SomeTrait;
    onStateChanged = SomeTrait::on_state_changed(&self, _: &str);
});

foreigner_class!(class ClassWithCallbacks {
    self_type Foo;
    constructor Foo::default() -> Foo;
    method f1(&mut self, cb: Box<SomeTrait>);
});
"#,
    );
    println!("{}\n{}", rust_code, java_code);
    assert!(rust_code.contains(r#"swig_c_str!("(Ljava/lang/String;)V")"#));
}

fn parse_code(test_name: &str, code: &str) -> (String, String) {
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
    let mut registry = Registry::new();
    let swig_gen = Generator::new_with_pointer_target_width(
        LanguageConfig::JavaConfig(JavaConfig::new(tmp_dir.path().into(), "com.example".into())),
        64,
    );
    swig_gen.register(&mut registry);
    let res_code = registry.expand_str(test_name, "use_case", code).unwrap();
    let mut java_code = String::new();
    for path in fs::read_dir(tmp_dir.path()).unwrap() {
        let path = path.unwrap();
        if path.file_type().unwrap().is_file() && path.path().to_str().unwrap().ends_with(".java") {
            let mut contents = String::new();
            let mut file = File::open(path.path()).unwrap();
            file.read_to_string(&mut contents).unwrap();
            java_code.push_str(&contents);
            java_code.push('\n');
        }
    }
    (res_code, java_code)
}
