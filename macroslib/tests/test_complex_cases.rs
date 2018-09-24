extern crate env_logger;
extern crate regex;
extern crate rust_swig;
extern crate syntex;
extern crate tempdir;

use std::fs;
use std::fs::File;
use std::io::Read;
use std::panic;
use std::path::Path;

use regex::Regex;
use rust_swig::{CppConfig, Generator, JavaConfig, LanguageConfig};
use syntex::Registry;
use tempdir::TempDir;

#[macro_use]
#[path = "../src/test_helper.rs"]
mod test_helper;

#[test]
fn test_class_with_methods_without_constructor() {
    let langs = [ForeignLang::Java, ForeignLang::Cpp];
    parse_code(
        "class_with_methods_without_constructor",
        r#"
foreigner_class!(class Foo {
});
"#,
        &langs,
    );
    parse_code(
        "class_with_methods_without_constructor",
        r#"
foreigner_class!(class Foo {
   self_type SomeType;
});
"#,
        &langs,
    );

    for lang in &langs {
        let result = panic::catch_unwind(|| {
            parse_code(
                "class_with_methods_without_constructor",
                r#"
foreigner_class!(class Foo {
   self_type SomeType;
   method SomeType::f(&self) -> i32;
});
"#,
                &[*lang],
            );
        });
        assert!(result.is_err());
    }
}

#[test]
fn test_parse_without_self_type_err() {
    for lang in &[ForeignLang::Java, ForeignLang::Cpp] {
        eprintln!("test_parse_without_self_type_err: lang {:?}", lang);
        let result = panic::catch_unwind(|| {
            parse_code(
                "test_parse_without_self_type_err",
                r#"
foreigner_class!(class DownloadItem {
    self_type DownloadItem;
    private constructor = empty;
    method DownloadItem::total_size(&self) -> u64;
});

foreigner_class!(class Document {
    constructor Document::new(remote: DownloadItem) -> Document;
    method Document::remote(&self) -> bool;
});
"#,
                &[*lang],
            );
        });
        assert!(result.is_err());
    }
}

#[test]
fn test_class_with_dummy_constructor() {
    let gen_code = parse_code(
        "test_class_with_dummy_constructor",
        r#"
foreigner_class!(class Foo {
   self_type SomeType;
   private constructor = empty;
   method SomeType::f(&self);
});

foreigner_class!(class Boo {
   self_type OtherType;
   private constructor = empty -> Box<OtherType>;
   method OtherType::f(&self);
});
"#,
        &[ForeignLang::Java, ForeignLang::Cpp],
    );
    let cpp_code_pair = gen_code
        .iter()
        .find(|x| x.lang == ForeignLang::Cpp)
        .unwrap();
    println!("c/c++: {}", cpp_code_pair.foreign_code);
    assert!(cpp_code_pair.foreign_code.contains(
        r#"private:

    Foo() noexcept {}"#
    ));
    let java_code_pair = gen_code
        .iter()
        .find(|x| x.lang == ForeignLang::Java)
        .unwrap();
    println!("Java: {}", java_code_pair.foreign_code);
    assert!(java_code_pair.foreign_code.contains("private Foo() {}"));
}

#[test]
fn test_foreign_class_as_return_type_simple() {
    // without result Type and without "foreign" args
    let gen_code = parse_code(
        "foreign_class_as_return_type_simple",
        r#"
foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32) -> Foo;
    method Foo::f(&self, _: i32, _: i32) -> i32;
});

foreigner_class!(class Boo {
    self_type Boo;

    constructor Boo::new(_: i32, _: usize) -> Boo;
    static_method Boo::factory_method() -> Boo;
    method Boo::get_one_foo(&self) -> Foo;
});
"#,
        &[ForeignLang::Java, ForeignLang::Cpp],
    );
    let cpp_code_pair = gen_code
        .iter()
        .find(|x| x.lang == ForeignLang::Cpp)
        .unwrap();
    println!("c/c++: {}", cpp_code_pair.foreign_code);
    assert!(
        cpp_code_pair
            .foreign_code
            .contains("Foo get_one_foo() const")
    );
}

#[test]
fn test_foreign_class_as_arg_type_simple() {
    let gen_code = parse_code(
        "test_foreign_class_as_arg_type_simple",
        r#"
foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32) -> Foo;
    method Foo::f(&self, _: i32, _: i32) -> i32;
});

foreigner_class!(class Boo {
    self_type Boo;

    constructor Boo::new(_: i32, _: usize) -> Boo;
    constructor Boo::with_foo(f: Foo) -> Boo;
    method Boo::f(&self, foo: Foo) -> usize;
    static_method Boo::f2(_: f64, foo: Foo) -> i32;
});
"#,
        &[ForeignLang::Java, ForeignLang::Cpp],
    );

    let cpp_code = code_for(&gen_code, ForeignLang::Cpp);
    println!("c++/rust {}", cpp_code.rust_code);
    assert!(cpp_code.rust_code.contains(
        r#"fn Boo_with_foo(a_0: *mut ::std::os::raw::c_void)
 -> *const ::std::os::raw::c_void {
    assert!(! a_0 . is_null (  ));
    let a_0: *mut Foo = a_0 as *mut Foo;
    let a_0: Box<Foo> = unsafe { Box::from_raw(a_0) };
    let a_0: Foo = *a_0;
    let this: Boo = Boo::with_foo(a_0);
    let this: Box<Boo> = Box::new(this);
    let this: *mut Boo = Box::into_raw(this);
    this as *const ::std::os::raw::c_void
}"#
    ));
    println!("c/c++: {}", cpp_code.foreign_code);
    assert!(cpp_code.foreign_code.contains(
        r#"Boo(Foo a_0)
    {
        this->self_ = Boo_with_foo(a_0.release());
        if (this->self_ == nullptr) {
            std::abort();
        }
    }"#
    ));
    assert!(cpp_code.foreign_code.contains("uintptr_t f(Foo a_0) const"));
    assert!(
        cpp_code
            .foreign_code
            .contains("BooOpaque *Boo_with_foo(FooOpaque * a_0);")
    );
}

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
        &[ForeignLang::Java],
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
        &[ForeignLang::Java],
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
        &[ForeignLang::Java],
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
        &[ForeignLang::Java],
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
        &[ForeignLang::Java, ForeignLang::Cpp],
    );
}

#[test]
fn test_return_foreign_class() {
    let gen_code = parse_code(
        "test_return_foreign_class1",
        r#"
foreigner_class!(class Boo {
    self_type Boo;
    constructor create_boo() -> Rc<RefCell<Boo>>;
    method Boo::test(&self, _: bool) -> f32;
    method Boo::set_a(&mut self, _: i32);
});
foreigner_class!(class Moo {
    self_type Moo;
    constructor Moo::empty() -> Moo;
    method Moo::get_boo(&self) -> Rc<RefCell<Boo>>; alias getBoo;
});
"#,
        &[ForeignLang::Java, ForeignLang::Cpp],
    );
    let java_code_pair = gen_code
        .iter()
        .find(|x| x.lang == ForeignLang::Java)
        .unwrap();
    println!("java: {}", java_code_pair.foreign_code);
    assert!(
        java_code_pair
            .foreign_code
            .contains("public final Boo getBoo()")
    );
    let cpp_code_pair = gen_code
        .iter()
        .find(|x| x.lang == ForeignLang::Cpp)
        .unwrap();
    println!("c/c++: {}", cpp_code_pair.foreign_code);
    assert!(cpp_code_pair.foreign_code.contains("Boo getBoo() const"));

    let gen_code = parse_code(
        "test_return_foreign_class2",
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
        &[ForeignLang::Java],
    );
    assert_eq!(ForeignLang::Java, gen_code[0].lang);
    let get_boo_re = Regex::new(
        r"fn\s+Java_com_example_Moo_do_1getBoo\([^\)]+\)\s*->\s*([[:alnum:]]+)\s*\{",
    ).expect("wrong regexp");
    let caps = get_boo_re.captures(&gen_code[0].rust_code).unwrap();
    println!("{:?}", caps);
    assert_eq!("jobject", caps.get(1).unwrap().as_str());

    let gen_code = parse_code(
        "test_return_foreign_class3",
        r#"
foreigner_class!(class Foo {
    self_type Foo;
    private constructor = empty -> Box<Box<Foo>>;
});
foreigner_class!(class Boo {
    self_type Boo;
    private constructor Boo::default() -> Boo;
    method Boo::f(&mut self) -> Result<Box<Box<Foo>>, String>;
});
"#,
        &[ForeignLang::Java, ForeignLang::Cpp],
    );
    let java_code = code_for(&gen_code, ForeignLang::Java);
    println!("java: {}", java_code.foreign_code);
    assert!(
        java_code
            .foreign_code
            .contains("public final Foo f() throws Exception")
    );
    let cpp_code = code_for(&gen_code, ForeignLang::Cpp);
    println!("c/c++: {}", cpp_code.foreign_code);
    assert!(
        cpp_code
            .foreign_code
            .contains("std::variant<Foo, RustString> f()")
    );
}

#[test]
fn test_return_foreign_interface_opt() {
    let gen_code = parse_code(
        "test_return_foreign_interface_opt",
        r#"
foreigner_class!(class Foo {
    self_type Foo;
    private constructor = empty -> Box<Box<Foo>>;
});
foreigner_class!(class Boo {
    self_type Boo;
    private constructor Boo::default() -> Boo;
    method Boo::f(&mut self) -> Result<Box<Box<Foo>>, String>;
    method Boo::f2(&mut self) -> Option<Box<Box<Foo>>>;
});
"#,
        &[ForeignLang::Cpp],
    );
    let cpp_code = code_for(&gen_code, ForeignLang::Cpp);
    println!("c++rust: {}", cpp_code.rust_code);
    cpp_code.rust_code.contains(
        r#"pub extern "C" fn Boo_f2(this: *mut Boo) -> *mut ::std::os::raw::c_void {
    let this: &mut Boo = unsafe { this.as_mut().unwrap() };
    let mut ret: Option<Box<Box<Foo>>> = Boo::f2(this);
    let mut ret: *mut ::std::os::raw::c_void =
        <*mut ::std::os::raw::c_void>::swig_from(ret);
    ret
}"#,
    );
    let cpp_code = code_for(&gen_code, ForeignLang::Cpp);
    println!("c/c++: {}", cpp_code.foreign_code);
    assert!(
        cpp_code
            .foreign_code
            .contains("std::variant<Foo, RustString> f()")
    );
    assert!(cpp_code.foreign_code.contains("std::optional<Foo> f2()"));
    assert!(
        cpp_code
            .foreign_code
            .contains("FooOpaque * Boo_f2(BooOpaque * const self)")
    );
}

#[test]
fn test_return_foreign_class_ref() {
    for _ in 0..10 {
        let gen_code = parse_code(
            "test_return_foreign_class_ref",
            r#"
foreigner_class!(class Boo {
    self_type Boo;
    constructor create_boo() -> Boo;
    method Boo::test(&self, _: bool) -> f32;
    method Boo::set_a(&mut self, _: i32);
});
foreigner_class!(class Moo {
    self_type Moo;
    constructor TestPathAndResult::default() -> Moo;
    method TestPathAndResult::get_boo(&self) -> &Boo;
    method TestReferences::update_boo(&mut self, foo: &Boo);
});
"#,
            &[ForeignLang::Cpp],
        );
        let cpp_code_pair = gen_code
            .iter()
            .find(|x| x.lang == ForeignLang::Cpp)
            .unwrap();
        println!("c/c++: {}", cpp_code_pair.foreign_code);
        assert!(
            cpp_code_pair
                .foreign_code
                .contains("BooRef get_boo() const")
        );
        assert!(
            cpp_code_pair
                .foreign_code
                .contains("void Moo_update_boo(MooOpaque * const self, const BooOpaque * a_0);")
        );
        assert!(
            cpp_code_pair
                .foreign_code
                .contains("void update_boo(const Boo & a_0)")
        );
    }
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
        &[ForeignLang::Java],
    );
    assert_eq!(ForeignLang::Java, gen_code[0].lang);
    let get_boo_re = Regex::new(
        r"fn\s+Java_com_example_Moo_do_1getBoo\([^\)]+\)\s*->\s*([[:alnum:]]+)\s*\{",
    ).expect("wrong regexp");
    let caps = get_boo_re.captures(&gen_code[0].rust_code).unwrap();
    println!("{:?}", caps);
    assert_eq!("jobject", caps.get(1).unwrap().as_str());
}

#[test]
fn test_pass_objects_as_param_simple() {
    let gen_code = parse_code(
        "test_pass_objects_as_param_simple",
        r#"
foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32, _: &str) -> Foo;
    method Foo::f(&self, _: i32, _: i32) -> i32;  alias calcF;
});

foreigner_class!(class Boo {
    self_type Moo<'a>;
    constructor Moo::new(_: &str) -> Moo<'a>;
    method Moo::f(&self, _: &str) -> String;
});

foreigner_class!(class TestPassObjectsAsParams {
    self_type TestPassObjectsAsParams;
    constructor TestPassObjectsAsParams::default() -> TestPassObjectsAsParams;
    method TestPassObjectsAsParams::f1(&self, _: &Foo);
    method TestPassObjectsAsParams::f2(&self, _: Foo);
    method TestPassObjectsAsParams::f3(&self, _: &mut Foo);
    method TestPassObjectsAsParams::f3_5(&self, _: &mut Moo<'a>);
    static_method TestPassObjectsAsParams::f4(_: &Foo);
    static_method TestPassObjectsAsParams::f5(_: Foo);    
});
"#,
        &[ForeignLang::Java, ForeignLang::Cpp],
    );
    let java_code = code_for(&gen_code, ForeignLang::Java);
    assert!(java_code.foreign_code.contains("public static void f4(Foo"));
    assert!(java_code.foreign_code.contains("public static void f5(Foo"));

    let cpp_code = code_for(&gen_code, ForeignLang::Cpp);
    println!("cpp_code.rust_code\n{}", cpp_code.rust_code);
    assert!(cpp_code.rust_code.contains(
        r##"#[no_mangle]
pub extern "C" fn TestPassObjectsAsParams_f1(this:
                                                 *mut TestPassObjectsAsParams,
                                             a_0:
                                                 *const ::std::os::raw::c_void)
 -> () {
    assert!(! a_0 . is_null (  ));
    let a_0: &Foo = unsafe { &*(a_0 as *const Foo) };
    let this: &TestPassObjectsAsParams = unsafe { this.as_mut().unwrap() };
    let mut ret: () = TestPassObjectsAsParams::f1(this, a_0);
    ret
}"##
    ));
    assert!(cpp_code.rust_code.contains(
        r##"#[no_mangle]
pub extern "C" fn TestPassObjectsAsParams_f3(this:
                                                 *mut TestPassObjectsAsParams,
                                             a_0: *mut ::std::os::raw::c_void)
 -> () {
    assert!(! a_0 . is_null (  ));
    let a_0: &mut Foo = unsafe { &mut *(a_0 as *mut Foo) };
    let this: &TestPassObjectsAsParams = unsafe { this.as_mut().unwrap() };
    let mut ret: () = TestPassObjectsAsParams::f3(this, a_0);
    ret
}"##
    ));
    println!("cpp_code.foreign_code {}", cpp_code.foreign_code);
    assert!(cpp_code.foreign_code.contains(
        r#"void f1(const Foo & a_0) const 
    {
        TestPassObjectsAsParams_f1(this->self_, static_cast<const FooOpaque *>(a_0));
    }"#
    ));
    assert!(cpp_code.foreign_code.contains(
        r#"void f2(Foo a_0) const 
    {
        TestPassObjectsAsParams_f2(this->self_, a_0.release());
    }"#
    ));
    assert!(cpp_code.foreign_code.contains(
        r#"void f3(Foo & a_0) const 
    {
        TestPassObjectsAsParams_f3(this->self_, static_cast<const FooOpaque *>(a_0));
    }"#
    ));
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
        &[ForeignLang::Java],
    );
}

#[test]
fn test_document_generated_code() {
    let gen_code = parse_code(
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
        &[ForeignLang::Java, ForeignLang::Cpp],
    );
    for code in &gen_code {
        println!("rust_code: {}", code.rust_code);
        println!("foreign: {}", code.foreign_code);
    }
}

#[test]
fn test_return_result_with_object_as_value_and_err() {
    let gen_code = parse_code(
        "test_return_result_with_object_as_value_and_err",
        r#"
foreigner_class!(class Position {
    self_type GnssInfo;
    private constructor create_position() -> GnssInfo;
    method Position::getLatitude(&self) -> f64;
});

foreigner_class!(class PosErr {
    self_type PosErr;
    constructor PosErr::new() -> PosErr;
});

foreigner_class!(class LocationService {
    self_type LocationService;

    constructor LocationService::new() -> LocationService;
    static_method LocationService::f1() -> Result<GnssInfo, String>;
    static_method LocationService::f2() -> Result<(), String>;

    method LocationService::f3(&self) -> Result<GnssInfo, PosErr>;
    static_method LocationService::f4() -> Result<(), PosErr>;
    static_method LocationService::f5() -> Result<Vec<GnssInfo>, PosErr>;
    static_method LocationService::create() -> Result<LocationService, String>;
});

foreigner_class!(class Foo {
    self_type Foo<'a>;
    constructor Foo::create() -> Foo<'a>;
    static_method Foo::from_string<'a>(_: &str) -> Result<Foo<'a>, String>;
});
"#,
        &[ForeignLang::Java, ForeignLang::Cpp],
    );

    let cpp_code_pair = code_for(&gen_code, ForeignLang::Cpp);
    println!("c/c++: {}", cpp_code_pair.foreign_code);
    assert!(
        cpp_code_pair
            .foreign_code
            .contains("static std::variant<Position, RustString> f1()")
    );
    assert!(
        cpp_code_pair
            .foreign_code
            .contains("std::variant<void *, RustString> f2()")
    );
    assert!(
        cpp_code_pair
            .foreign_code
            .contains("std::variant<Position, PosErr> f3()")
    );
    assert!(
        cpp_code_pair
            .foreign_code
            .contains("std::variant<void *, PosErr> f4()")
    );

    assert!(
        cpp_code_pair
            .foreign_code
            .contains("static std::variant<Foo, RustString> from_string(")
    );
}

#[test]
fn test_return_result_with_vec() {
    let gen_code = parse_code(
        "test_return_result_with_vec",
        r#"
foreigner_class!(class PosErr {
    self_type PosErr;
    constructor PosErr::new() -> PosErr;
});

foreigner_class!(class LocationService {
    self_type LocationService;

    constructor LocationService::new() -> LocationService;
    method f1(&self) -> Result<Vec<u8>, PosErr>;
});

"#,
        &[ForeignLang::Cpp],
    );
    let cpp_code_pair = code_for(&gen_code, ForeignLang::Cpp);
    println!("c/c++: {}", cpp_code_pair.foreign_code);
    assert!(
        cpp_code_pair
            .foreign_code
            .contains(r#"std::variant<RustVecU8, PosErr> f1() const"#)
    );
}

#[test]
fn test_return_result_type_with_object() {
    for i in 0..10 {
        println!("iter {}", i);
        let gen_code = parse_code(
            "test_return_result_type_with_object",
            r#"
foreigner_class!(class Position {
    self_type GnssInfo;
    private constructor create_position() -> GnssInfo;
    method Position::getLatitude(&self) -> f64;
});

foreigner_class!(class LocationService {
    static_method LocationService::position() -> Result<GnssInfo, String>;
    static_method LocationService::do_something() -> Result<(), String>;
});
"#,
            &[ForeignLang::Java, ForeignLang::Cpp],
        );
        let java_code_pair = gen_code
            .iter()
            .find(|x| x.lang == ForeignLang::Java)
            .unwrap();
        println!("{}", java_code_pair.foreign_code);
        assert!(
            java_code_pair
                .foreign_code
                .contains("public static native Position position() throws Exception;")
        );
        let cpp_code_pair = gen_code
            .iter()
            .find(|x| x.lang == ForeignLang::Cpp)
            .unwrap();
        println!("c/c++: {}", cpp_code_pair.foreign_code);
        assert!(
            cpp_code_pair
                .foreign_code
                .contains("static std::variant<Position, RustString> position()")
        );
    }
}

#[test]
fn test_foreign_interface() {
    let gen_code = parse_code(
        "test_foreign_interface",
        r#"
trait SomeTrait {
    fn on_state_changed(&self, item: i32, is_ok: bool);
    fn on_state_changed_without_args(&self);
}

foreign_interface!(interface SomeObserver {
    self_type SomeTrait;
    onStateChanged = SomeTrait::on_state_changed(&self, _: i32, _: bool);
    onStateChangedWithoutArgs = SomeObserver::on_state_changed_without_args(&self);
});

foreigner_class!(class ClassWithCallbacks {
    self_type Foo;
    constructor Foo::default() -> Foo;
    method f1(&mut self, cb: Box<SomeTrait>);
});
"#,
        &[ForeignLang::Java, ForeignLang::Cpp],
    );
    let cpp_code = code_for(&gen_code, ForeignLang::Cpp);
    println!("cpp_code/c++: {}", cpp_code.foreign_code);
    assert!(
        cpp_code
            .foreign_code
            .contains("virtual void onStateChanged(int32_t a_0, bool a_1) = 0;")
    );
    assert!(
        cpp_code
            .foreign_code
            .contains("virtual void onStateChangedWithoutArgs() = 0;")
    );
    assert!(cpp_code.foreign_code.contains(
        r#"struct C_SomeObserver {
    void *opaque;
    //! call by Rust side when callback not need anymore
    void (*C_SomeObserver_deref)(void *opaque);
    

    void (*onStateChanged)(int32_t a_0, char a_1, void *opaque);


    void (*onStateChangedWithoutArgs)(void *opaque);

};"#
    ));
}

#[test]
fn test_foreign_interface_cpp_return_not_void() {
    let gen_code = parse_code(
        "test_foreign_interface_cpp_return_not_void",
        r#"
foreign_interface!(interface SomeObserver {
    self_type SomeTrait;
    onStateChanged = SomeTrait::on_state_changed(&self, _: i32, _: bool) -> bool;
    onStateChangedWithoutArgs = SomeObserver::on_state_changed_without_args(&self);
});"#,
        &[ForeignLang::Cpp],
    );
    let cpp_code = code_for(&gen_code, ForeignLang::Cpp);
    println!("cpp_code/rust: {}", cpp_code.rust_code);
    assert!(cpp_code.rust_code.contains(
        r#"pub struct C_SomeObserver {
    opaque: *const ::std::os::raw::c_void,
    C_SomeObserver_deref: extern "C" fn(_: *const ::std::os::raw::c_void),
    onStateChanged: extern "C" fn(a_0: i32, a_1: ::std::os::raw::c_char,
                                  _: *const ::std::os::raw::c_void)
                        -> ::std::os::raw::c_char,
    onStateChangedWithoutArgs: extern "C" fn(_: *const ::std::os::raw::c_void)
                                   -> (),
}"#
    ));
    println!("cpp_code/c++: {}", cpp_code.foreign_code);
    assert!(
        cpp_code
            .foreign_code
            .contains("virtual bool onStateChanged(int32_t a_0, bool a_1) = 0;")
    );
    assert!(
        cpp_code
            .foreign_code
            .contains("virtual void onStateChangedWithoutArgs() = 0;")
    );
    assert!(cpp_code.foreign_code.contains(
        r#"struct C_SomeObserver {
    void *opaque;
    //! call by Rust side when callback not need anymore
    void (*C_SomeObserver_deref)(void *opaque);
    

    char (*onStateChanged)(int32_t a_0, char a_1, void *opaque);


    void (*onStateChangedWithoutArgs)(void *opaque);

};"#
    ));
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
        &[ForeignLang::Java, ForeignLang::Cpp],
    );
}

#[test]
fn test_foreign_enum_vs_int() {
    for _ in 0..10 {
        let gen_code = parse_code(
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
            &[ForeignLang::Java, ForeignLang::Cpp],
        );
        let java_code = code_for(&gen_code, ForeignLang::Java);
        println!("{}", java_code.rust_code);
        println!("{}", java_code.foreign_code);
        assert!(java_code.foreign_code.contains("int f1(MyEnum"));
    }
}

#[test]
fn test_static_func_with_foreign_class_as_param() {
    let gen_code = parse_code(
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
        &[ForeignLang::Java, ForeignLang::Cpp],
    );
    let java_code = code_for(&gen_code, ForeignLang::Java);
    println!("{}", java_code.foreign_code);
    assert!(
        java_code
            .foreign_code
            .contains("public static void static_foo(Boo")
    );

    //add method
    let gen_code = parse_code(
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
        &[ForeignLang::Java, ForeignLang::Cpp],
    );
    let java_code = code_for(&gen_code, ForeignLang::Java);
    println!("{}", java_code.foreign_code);
    assert!(
        java_code
            .foreign_code
            .contains("public static void static_foo(Boo")
    );
}

#[test]
fn test_lifetime_param_in_result() {
    let gen_code = parse_code(
        "lifetime_param_in_result",
        r#"
foreigner_class!(class Foo {
    self_type Foo<'a>;
    constructor new<'a>() -> Rc<RefCell<Foo<'a>>>;
    method Foo::f(&self, _: i32);
});
"#,
        &[ForeignLang::Java, ForeignLang::Cpp],
    );
    let java_code = code_for(&gen_code, ForeignLang::Java);
    println!("{}", java_code.rust_code);
    assert!(
        java_code
            .rust_code
            .contains("impl <'a> SwigForeignClass for Rc<RefCell<Foo<'a>>> {")
    );
}

#[test]
fn test_interface_with_str() {
    let gen_code = parse_code(
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
        &[ForeignLang::Java, ForeignLang::Cpp],
    );
    let java_code = code_for(&gen_code, ForeignLang::Java);
    println!("{}\n{}", java_code.rust_code, java_code.foreign_code);
    assert!(
        java_code
            .rust_code
            .contains(r#"swig_c_str!("(Ljava/lang/String;)V")"#)
    );
}

#[test]
fn test_return_bool() {
    let gen_code = parse_code(
        "test_cpp_return_bool",
        r#"
foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::default() -> Foo;
    method f1(&mut self) -> bool;
});
"#,
        &[ForeignLang::Java, ForeignLang::Cpp],
    );
    let cpp_code_pair = gen_code
        .iter()
        .find(|x| x.lang == ForeignLang::Cpp)
        .unwrap();
    println!("c/c++: {}", cpp_code_pair.foreign_code);
    assert!(cpp_code_pair.foreign_code.contains("bool f1()"));
}

#[test]
fn test_return_option() {
    let gen_code = parse_code(
        "test_return_option",
        r#"
foreigner_class!(class Boo {
  self_type Boo;
  constructor Boo::new() -> Boo;
  method Boo::something(&self) -> i32;
});

foreign_enum!(enum ControlItem {
    GNSS = ControlItem::GnssWorking,
    GPS_PROVIDER = ControlItem::AndroidGPSOn,
});

foreigner_class!(class Foo {
   self_type Foo;
   constructor Foo::default() -> Foo;
   method Foo::f1(&self) -> Option<Boo>;
   method Foo::f2(&self) -> Option<f64>;
   method Foo::f3(&self) -> Option<u32>;
   method Foo::f4(&self) -> Option<usize>;
   method Foo::f5(&self) -> Option<&Boo>;
   method Foo::f6(&self) -> Option<ControlItem>;
   method Foo::f7(&self) -> Option<u64>;
   method Foo::f8(&self) -> Option<&str>;
});
"#,
        &[ForeignLang::Cpp],
    );
    let cpp_code_pair = code_for(&gen_code, ForeignLang::Cpp);
    println!("c/c++: {}", cpp_code_pair.foreign_code);
    assert!(
        cpp_code_pair
            .foreign_code
            .contains("std::optional<Boo> f1()")
    );
    assert!(
        cpp_code_pair
            .foreign_code
            .contains("std::optional<double> f2()")
    );
    assert!(
        cpp_code_pair
            .foreign_code
            .contains("std::optional<uint32_t> f3()")
    );

    assert!(
        cpp_code_pair
            .foreign_code
            .contains("std::optional<uintptr_t> f4()")
    );
    assert!(
        cpp_code_pair
            .foreign_code
            .contains("std::optional<BooRef> f5()")
    );

    assert!(
        cpp_code_pair
            .foreign_code
            .contains("std::optional<ControlItem> f6()")
    );
    assert!(
        cpp_code_pair
            .foreign_code
            .contains("std::optional<uint64_t> f7()")
    );
    assert!(
        cpp_code_pair
            .foreign_code
            .contains("std::optional<struct RustStrView> f8()")
    );
}

#[test]
fn test_option_arg_java() {
    let gen_code = parse_code(
        "test_option_arg_java",
        r#"
foreigner_class!(class Boo {
  self_type Boo;
  constructor Boo::new() -> Boo;
  method Boo::something(&self) -> i32;
});

foreign_enum!(enum ControlItem {
    GNSS = ControlItem::GnssWorking,
    GPS_PROVIDER = ControlItem::AndroidGPSOn,
});

foreigner_class!(class Foo {
   self_type Foo;
   constructor Foo::default() -> Foo;
   method Foo::f1(&self, _: Option<f64>) -> Option<f64>;
});
"#,
        &[ForeignLang::Java],
    );
    let java_code = code_for(&gen_code, ForeignLang::Java);
    println!("java: {}", java_code.foreign_code);
    assert!(
        java_code
            .foreign_code
            .contains("public final java.util.OptionalDouble f1(Double a0)")
    );
}

#[test]
fn test_option_arg_cpp() {
    let gen_code = parse_code(
        "test_option_arg_cpp",
        r#"
foreigner_class!(class Boo {
  self_type Boo;
  constructor Boo::new() -> Boo;
  method Boo::something(&self) -> i32;
});

foreign_enum!(enum ControlItem {
    GNSS = ControlItem::GnssWorking,
    GPS_PROVIDER = ControlItem::AndroidGPSOn,
});

foreigner_class!(class Foo {
   self_type Foo;
   constructor Foo::default() -> Foo;
   constructor Foo::new(_: Option<Boo>, _: Option<f64>) -> Foo;
   method Foo::f1(&self, _: Option<f64>);
   method Foo::f2(&mut self, _: Option<Boo>);
   method Foo::f3(&mut self, _: Option<ControlItem>);
   method Foo::f4(&self, x: Option<usize>);
   static_method Foo::f5(x: Option<f64>, y: Option<usize>);
   static_method Foo::f6(x: Option<&str>);
});
"#,
        &[ForeignLang::Cpp],
    );
    let cpp_code_pair = code_for(&gen_code, ForeignLang::Cpp);

    println!("rust: {}", cpp_code_pair.rust_code);

    assert!(
        cpp_code_pair
            .rust_code
            .contains("pub extern \"C\" fn Foo_f3(this: *mut Foo, a_0: CRustOptionU32)")
    );
    assert!(
        cpp_code_pair
            .rust_code
            .contains("pub extern \"C\" fn Foo_f4(this: *mut Foo, a_0: CRustOptionUSize)")
    );
    assert!(
        cpp_code_pair
            .rust_code
            .contains("pub extern \"C\" fn Foo_f5(a_0: CRustOptionF64, a_1: CRustOptionUSize)")
    );
    assert!(
        cpp_code_pair
            .rust_code
            .contains(r#"pub extern "C" fn Foo_f6(a_0: *const ::std::os::raw::c_char) -> () {"#)
    );

    println!("c/c++: {}", cpp_code_pair.foreign_code);

    assert!(
        cpp_code_pair
            .foreign_code
            .contains("void f1(std::optional<double>")
    );

    assert!(
        cpp_code_pair
            .foreign_code
            .contains("void f2(std::optional<Boo>")
    );
    assert!(
        cpp_code_pair
            .foreign_code
            .contains("void f3(std::optional<ControlItem>")
    );
    assert!(
        cpp_code_pair
            .foreign_code
            .contains("void Foo_f6(const char * a_0);")
    );
    assert!(cpp_code_pair.foreign_code.contains(
        r#"static void f6(std::optional<const char *> a_0)
    {
        Foo_f6(!!a_0 ? *a_0 : nullptr);
    }"#
    ));
}

#[test]
fn test_foreign_vec_return() {
    parse_code(
        "test_foreign_vec_return",
        r#"
foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32) -> Foo;
    method Foo::f(&self, _: i32, _: i32) -> i32;
});

foreigner_class!(class Boo {
    self_type Boo;

    constructor Boo::new(_: i32, _: usize) -> Boo;
    method Boo::get_foo_arr(&self) -> Vec<Foo>;
    method Boo::get_foo_with_err(&self) -> Result<Foo, String>;
    method Boo::get_foo_arr_with_err(&self) -> Result<Vec<Foo>, String>;
});
"#,
        &[ForeignLang::Java, ForeignLang::Cpp],
    );
}

#[test]
fn test_foreign_vec_as_arg() {
    let gen_code = parse_code(
        "test_foreign_vec_as_arg",
        r#"
foreigner_class!(class Boo {
    self_type Boo;
    constructor Boo::default() -> Boo;
});
foreigner_class!(class FooImpl {
    self_type Foo<'a>;
    constructor Foo::create() -> Foo<'a>;
    method Foo::alternate_boarding(&self) -> &[Boo]; alias alternateBoarding;
    method Foo::set_alternate_boarding(&mut self, p: Vec<Boo>);
    alias setAlternateBoarding;
});
"#,
        &[ForeignLang::Cpp],
    );
    let cpp_code_pair = code_for(&gen_code, ForeignLang::Cpp);
    println!("c/c++: {}", cpp_code_pair.foreign_code);
    assert!(
        cpp_code_pair
            .foreign_code
            .contains("RustForeignSlice<BooRef> alternateBoarding() const")
    );
    assert!(
        cpp_code_pair
            .foreign_code
            .contains("void setAlternateBoarding(RustForeignVecBoo a_0)")
    );
}

#[test]
fn test_return_slice() {
    let gen_code = parse_code(
        "test_return_slice",
        r#"
foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32) -> Foo;
    method Foo::f(&self, _: i32, _: i32) -> i32;
});

foreigner_class!(class Boo {
    self_type Boo;

    constructor Boo::new(_: i32, _: usize) -> Boo;
    method Boo::f1(&self) -> &[u32];
    method Boo::f2(&self) -> &[Foo];
    method Boo::f3(&self) -> &[usize];
});
"#,
        &[ForeignLang::Cpp],
    );
    let cpp_code_pair = code_for(&gen_code, ForeignLang::Cpp);
    println!("c/c++: {}", cpp_code_pair.foreign_code);
    assert!(
        cpp_code_pair
            .foreign_code
            .contains("struct CRustObjectSlice Boo_f2(const BooOpaque *")
    );
    assert!(
        cpp_code_pair
            .foreign_code
            .contains("RustForeignSlice<FooRef> f2() const")
    );
}

#[test]
fn test_pass_slice_as_args() {
    let gen_code = parse_code(
        "test_pass_slice_as_args",
        r#"
foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32) -> Foo;
    method Foo::f(&self, _: i32, _: i32) -> i32;
});

foreigner_class!(class Boo {
    self_type Boo;

    constructor Boo::new(_: i32, _: usize) -> Boo;
    method Boo::f1(&self, _: &mut [Foo]) -> &[u32];
    method Boo::f2(&self, _: &[Foo]) -> &[u32];
});
"#,
        &[ForeignLang::Cpp],
    );
    let cpp_code_pair = code_for(&gen_code, ForeignLang::Cpp);
    println!("c/c++: {}", cpp_code_pair.foreign_code);
    assert!(
        cpp_code_pair
            .foreign_code
            .contains("CRustSliceU32 f1(RustForeignSlice<FooRef> a_0) const")
    );
    assert!(
        cpp_code_pair
            .foreign_code
            .contains("CRustSliceU32 f2(RustForeignSlice<FooRef> a_0) const")
    );
}

#[test]
fn test_string_handling() {
    let gen_code = parse_code(
        "test_string_handling",
        r#"
foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32) -> Foo;
    method Foo::f(&self, _: i32, _: i32, _: String) -> String;
});
"#,
        &[ForeignLang::Java, ForeignLang::Cpp],
    );
    let cpp_code_pair = gen_code
        .iter()
        .find(|x| x.lang == ForeignLang::Cpp)
        .unwrap();
    println!("Rust(c/c++): {}", cpp_code_pair.rust_code);
    assert!(
        cpp_code_pair
            .rust_code
            .contains("a_2: String = a_2.swig_into();")
    );
    println!("c/c++: {}", cpp_code_pair.foreign_code);
    assert!(
        cpp_code_pair
            .foreign_code
            .contains("    RustString f(int32_t a_0, int32_t a_1, const char * a_2) const")
    );
}

#[test]
fn test_generated_cpp_ref_classes() {
    let gen_code = parse_code(
        "test_generated_cpp_ref_classes",
        r#"
foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32) -> Foo;
    method Foo::eq(&self, _: &Foo) -> bool;
    method Foo::f(&self, _: i32, _: i32, _: String) -> String;
});
"#,
        &[ForeignLang::Java, ForeignLang::Cpp],
    );
    let cpp_code_pair = gen_code
        .iter()
        .find(|x| x.lang == ForeignLang::Cpp)
        .unwrap();
    println!("c/c++: {}", cpp_code_pair.foreign_code);
    assert!(
        cpp_code_pair
            .foreign_code
            .contains("bool eq(const Foo & a_0) const")
    );
    assert!(
        cpp_code_pair
            .foreign_code
            .contains("bool eq(const FooRef & a_0) const")
    );
}

#[test]
fn test_foreign_interface_cpp() {
    for _ in 0..100 {
        let gen_code = parse_code(
            "test_foreign_interface_cpp",
            r#"
foreigner_class!(class Uuid {
    self_type Uuid;
    private constructor uuid_private_constructor() -> Uuid;
    static_method Uuid::new_v4() -> Uuid;
});

foreign_interface!(interface RepoChangedCallback {
    self_type RepoChangedCallback;
    on_save = RepoChangedCallback::on_save(&self, uuid: &Uuid);
    on_remove = RepoChangedCallback::on_remove(&self, uuid: &Uuid);
});
"#,
            &[ForeignLang::Cpp],
        );
        let cpp_code_pair = code_for(&gen_code, ForeignLang::Cpp);
        println!("c/c++: {}", cpp_code_pair.foreign_code);
        assert!(
            cpp_code_pair
                .foreign_code
                .contains("virtual void on_save(UuidRef a_0) = 0;")
        );
        assert!(
            cpp_code_pair
                .foreign_code
                .contains("virtual void on_remove(UuidRef a_0) = 0;")
        );
        assert!(cpp_code_pair.foreign_code.contains(
            r#"
   static void c_on_save(const UuidOpaque * a_0, void *opaque)
   {
        auto p = static_cast<RepoChangedCallback *>(opaque);
        assert(p != nullptr);
        p->on_save(UuidRef{a_0});
   }

   static void c_on_remove(const UuidOpaque * a_0, void *opaque)
   {
        auto p = static_cast<RepoChangedCallback *>(opaque);
        assert(p != nullptr);
        p->on_remove(UuidRef{a_0});
   }
"#
        ));
    }
}

#[test]
fn test_pass_foreign_trait_cpp() {
    let gen_code = parse_code(
        "test_foreign_interface_cpp",
        r#"
fn create_interface() -> Box<Box<Interface>> {
    Box::new(Box::new(InterfaceImpl { base: 17 }))
}

foreigner_class!(class Interface {
    self_type Interface;
    constructor create_interface() -> Box<Box<Interface>>;
    method Interface::f(&self, _: i32) -> i32;
    method Interface::set(&mut self, x: i32);
});

foreigner_class!(class TestPassInterface {
    self_type TestPassInterface;
    constructor TestPassInterface::default() -> TestPassInterface;
    static_method use_interface(a: Box<Box<Interface>>, b: i32) -> i32;
});

"#,
        &[ForeignLang::Cpp],
    );
    let cpp_code = code_for(&gen_code, ForeignLang::Cpp);
    println!("c++/rust {}", cpp_code.rust_code);
    assert!(cpp_code.rust_code.contains(
        r#"pub extern "C" fn TestPassInterface_use_interface(a_0:
                                                      *mut ::std::os::raw::c_void,
                                                  a_1: i32) -> i32 {
    assert!(! a_0 . is_null (  ));
    let a_0: *mut Box<Interface> = a_0 as *mut Box<Interface>;
    let a_0: Box<Box<Interface>> = unsafe { Box::from_raw(a_0) };
    let mut ret: i32 = use_interface(a_0, a_1);
    ret
}"#
    ));
    println!("c++/c++ {}", cpp_code.foreign_code);
    assert!(cpp_code.foreign_code.contains(
        r#"static int32_t use_interface(Interface a_0, int32_t a_1)
    {
        int32_t ret = TestPassInterface_use_interface(a_0.release(), a_1);
        return ret;
    }"#
    ));
}

#[derive(PartialEq, Debug, Clone, Copy)]
enum ForeignLang {
    Java,
    Cpp,
}

struct CodePair {
    lang: ForeignLang,
    rust_code: String,
    foreign_code: String,
}

fn collect_code_in_dir(dir_with_code: &Path, exts: &[&str]) -> String {
    let mut java_code = String::new();
    for path in fs::read_dir(dir_with_code).unwrap() {
        let path = path.unwrap();
        if path.file_type().unwrap().is_file()
            && exts
                .iter()
                .any(|ext| path.path().to_str().unwrap().ends_with(ext))
        {
            let mut contents = String::new();
            let mut file = File::open(path.path()).unwrap();
            file.read_to_string(&mut contents).unwrap();
            java_code.push_str(&contents);
            java_code.push('\n');
        }
    }
    java_code
}

fn code_for(gen_code: &[CodePair], lang: ForeignLang) -> &CodePair {
    gen_code.iter().find(|x| x.lang == lang).unwrap()
}

fn parse_code(test_name: &str, code: &str, langs: &[ForeignLang]) -> Vec<CodePair> {
    test_helper::logger_init();
    let tmp_dir = TempDir::new(test_name).expect("Can not create tmp directory");
    println!(
        "{}: test name {} tmp_dir {:?}",
        file!(),
        test_name,
        tmp_dir.path()
    );
    let mut ret = vec![];
    for lang in langs {
        let mut registry = Registry::new();
        let (rust_code, foreign_code) = match *lang {
            ForeignLang::Java => {
                let swig_gen = Generator::new(LanguageConfig::JavaConfig(JavaConfig::new(
                    tmp_dir.path().into(),
                    "com.example".into(),
                ))).with_pointer_target_width(64);
                swig_gen.register(&mut registry);
                (
                    registry.expand_str(test_name, test_name, code).unwrap(),
                    collect_code_in_dir(tmp_dir.path(), &[".java"]),
                )
            }
            ForeignLang::Cpp => {
                let swig_gen = Generator::new(LanguageConfig::CppConfig(CppConfig::new(
                    tmp_dir.path().into(),
                    "com_examples".into(),
                ))).with_pointer_target_width(64);
                swig_gen.register(&mut registry);
                (
                    registry.expand_str(test_name, "use_case", code).unwrap(),
                    collect_code_in_dir(tmp_dir.path(), &[".h", ".hpp"]),
                )
            }
        };
        ret.push(CodePair {
            lang: *lang,
            rust_code,
            foreign_code,
        });
    }
    ret
}
