use std::{
    ffi::OsString,
    fs, panic,
    path::{Path, PathBuf},
};

use flapigen::{rustfmt_cnt, CppConfig, Generator, JavaConfig, LanguageConfig, RustEdition};
use log::warn;
use syn::Token;
use tempfile::tempdir;

include!(concat!(env!("OUT_DIR"), "/test_expectations.rs"));

#[test]
fn test_expectations_class_with_methods_without_constructor() {
    let _ = env_logger::try_init();

    let langs = [ForeignLang::Java, ForeignLang::Cpp];
    for lang in &langs {
        let name = format!("class_with_methods_without_constructor {:?}", lang);
        parse_code(
            &name,
            Source::Str(
                r#"
foreign_class!(class Foo {
});
"#,
            ),
            *lang,
        )
        .expect(&name);
    }
    for lang in &langs {
        let name = format!("class_with_methods_without_constructor {:?}", lang);
        let ret = panic::catch_unwind(|| {
            parse_code(
                &name,
                Source::Str(
                    r#"
    foreign_class!(class Foo {
       self_type SomeType;
    });
    "#,
                ),
                *lang,
            )
            .expect(&name)
        });
        assert!(ret.is_err());
    }

    for lang in &langs {
        let result = panic::catch_unwind(|| {
            let name = format!("class_with_methods_without_constructor {:?}", lang);
            parse_code(
                &name,
                Source::Str(
                    r#"
    foreign_class!(class Foo {
       self_type SomeType;
       method SomeType::f(&self) -> i32;
    });
    "#,
                ),
                *lang,
            )
            .expect(&name);
        });
        assert!(result.is_err());
    }
}

#[test]
fn test_foreign_typemap_not_direct_intermediate() {
    let name = "test_foreign_typemap_not_direct_intermediate";
    let _ = env_logger::try_init();
    for lang in &[ForeignLang::Java, ForeignLang::Cpp] {
        let ret = panic::catch_unwind(|| {
            parse_code(
                name,
                Source::Str(
                    r###"
foreign_typemap!(
   ($p:r_type) Type2 => Type3 { $out = $p };
   ($p:f_type) => "Type3";
);

foreign_typemap!(
    ($p:r_type) TypeX => Type2 {
        $out = typex_to_type2($p)
    };
    ($p:f_type) => "FType4"
        r#"
        $out = f_type2_to_type4($p);
"#;
);

foreign_class!(class Foo {
    fn f1() -> TypeX;
});

"###,
                ),
                *lang,
            )
            .unwrap_or_else(|err| panic!("Test {} failed for lang {:?}: {}", name, lang, err));
        });
        assert!(ret.is_err());
    }
}

#[test]
fn test_callback_without_self_err() {
    let _ = env_logger::try_init();
    for lang in &[ForeignLang::Java, ForeignLang::Cpp] {
        let result = panic::catch_unwind(|| {
            let name = format!("test_callback_without_self_err {:?}", lang);
            parse_code(
                &name,
                Source::Str(
                    r#"
foreign_interface! (interface FooBar {
    self_type Foo;
    bar = Foo::bar();
});
    "#,
                ),
                *lang,
            )
            .expect(&name);
        });
        println!("result: {:?}", result);
        assert!(result.is_err());
    }
}

#[test]
fn test_expectations_parse_without_self_type_err() {
    let _ = env_logger::try_init();

    for lang in &[ForeignLang::Java, ForeignLang::Cpp] {
        println!("test_parse_without_self_type_err: lang {:?}", lang);
        let result = panic::catch_unwind(|| {
            let name = format!("test_parse_without_self_type_err {:?}", lang);
            parse_code(
                &name,
                Source::Str(
                    r#"
foreign_class!(class DownloadItem {
    self_type DownloadItem;
    private constructor = empty;
    method DownloadItem::total_size(&self) -> u64;
});

foreign_class!(class Document {
    constructor Document::new(remote: DownloadItem) -> Document;
    method Document::remote(&self) -> bool;
});
"#,
                ),
                *lang,
            )
            .expect(&name);
        });
        assert!(result.is_err());
    }
}

#[test]
fn test_expectations_foreign_vec_as_arg() {
    let _ = env_logger::try_init();

    let name = "foreign_vec_as_arg";
    let src = r#"
foreign_class!(
#[derive(Clone)]
class Boo {
    self_type Boo;
    constructor Boo::default() -> Boo;
    fn Boo::clone(&self) -> Boo;
});
foreign_class!(class FooImpl {
    self_type Foo<'a>;
    constructor Foo::create() -> Foo<'a>;
    fn Foo::set_alternate_boarding(&mut self, p: Vec<Boo>);
    alias setAlternateBoarding;
});
"#;

    for _ in 0..100 {
        let cpp_code = parse_code(name, Source::Str(src), ForeignLang::Cpp).expect("parse failed");
        println!("c/c++: {}", cpp_code.foreign_code);
        assert!(cpp_code
            .foreign_code
            .contains("void setAlternateBoarding(RustForeignVecBoo p)"));
        let java_code =
            parse_code(name, Source::Str(src), ForeignLang::Java).expect("parse failed");
        println!("Java: {}", java_code.foreign_code);
        assert!(java_code
            .foreign_code
            .contains("void setAlternateBoarding(@NonNull Boo [] p)"));
    }
}

#[test]
fn test_foreign_enum_vs_int() {
    let _ = env_logger::try_init();

    let name = "foreign_enum_vs_int";
    let src = r#"
foreign_enum!(enum MyEnum {
  ITEM1 = MyEnum::Item1,
  ITEM2 = MyEnum::Item2,
  ITEM3 = MyEnum::Item3,
});

foreign_class!(class TestEnumClass {
    self_type Moo;
    constructor Moo::default() -> Moo;
    fn Moo::f1(&mut self, v: MyEnum) -> i32;
    fn Moo::next_enum(v: MyEnum) -> MyEnum;
});
"#;
    for _ in 0..10 {
        let java_code = parse_code(name, Source::Str(src), ForeignLang::Java).unwrap();
        println!("{}", java_code.foreign_code);
        assert!(java_code.foreign_code.contains("int f1(@NonNull MyEnum"));

        let _cpp_code = parse_code(name, Source::Str(src), ForeignLang::Cpp).unwrap();
    }
}

#[test]
fn test_return_result_type_with_object() {
    let _ = env_logger::try_init();

    let name = "return_result_type_with_object";
    let src = r#"
foreign_class!(class Position {
    self_type GnssInfo;
    private constructor create_position() -> GnssInfo;
    method Position::getLatitude(&self) -> f64;
});

foreign_class!(class LocationService {
    static_method LocationService::position() -> Result<GnssInfo, String>;
    static_method LocationService::do_something() -> Result<(), String>;
});
"#;
    for i in 0..10 {
        println!("iter {}", i);
        let java_code = parse_code(name, Source::Str(src), ForeignLang::Java).unwrap();
        println!("{}", java_code.foreign_code);
        assert!(java_code.foreign_code.contains(
            r#"public static @NonNull Position position() throws Exception {
        long ret = do_position();
        Position convRet = new Position(InternalPointerMarker.RAW_PTR, ret);

        return convRet;
    }
    private static native long do_position() throws Exception;"#
        ));
        let cpp_code = parse_code(name, Source::Str(src), ForeignLang::Cpp).unwrap();
        println!("c/c++: {}", cpp_code.foreign_code);
        assert!(cpp_code
            .foreign_code
            .contains("static std::variant<Position, RustString> position()"));
    }
}

#[test]
fn test_return_foreign_class_ref() {
    let _ = env_logger::try_init();

    for _ in 0..10 {
        let cpp_code = parse_code(
            "return_foreign_class_ref",
            Source::Str(
                r#"
foreign_class!(class Boo {
    self_type Boo;
    constructor create_boo() -> Boo;
    method Boo::test(&self, _: bool) -> f32;
    method Boo::set_a(&mut self, _: i32);
});
foreign_class!(class Moo {
    self_type Moo;
    constructor TestPathAndResult::default() -> Moo;
    method TestPathAndResult::get_boo(&self) -> &Boo;
    method TestReferences::update_boo(&mut self, boo: &Boo);
});
"#,
            ),
            ForeignLang::Cpp,
        )
        .unwrap();
        println!("c/c++: {}", cpp_code.foreign_code);
        assert!(cpp_code.foreign_code.contains("BooRef get_boo() const"));
        assert!(cpp_code
            .foreign_code
            .contains("void Moo_update_boo(MooOpaque * const self, const BooOpaque * boo);"));
        assert!(cpp_code
            .foreign_code
            .contains("void update_boo(const Boo & boo)"));
    }
}

#[test]
fn test_foreign_interface_cpp() {
    let _ = env_logger::try_init();

    let name = "foreign_interface_cpp";
    let src = r#"
foreign_class!(class Uuid {
    self_type Uuid;
    private constructor uuid_private_constructor() -> Uuid;
    static_method Uuid::new_v4() -> Uuid;
});

foreign_interface!(interface RepoChangedCallback {
    self_type RepoChangedCallback;
    on_save = RepoChangedCallback::on_save(&self, uuid: &Uuid);
    on_remove = RepoChangedCallback::on_remove(&self, uuid: &Uuid);
});
"#;
    for _ in 0..100 {
        let cpp_code = parse_code(name, Source::Str(src), ForeignLang::Cpp).unwrap();
        println!("c/c++: {}", cpp_code.foreign_code);
        assert!(cpp_code
            .foreign_code
            .contains("virtual void on_save(UuidRef uuid) const noexcept = 0;"));
        assert!(cpp_code
            .foreign_code
            .contains("virtual void on_remove(UuidRef uuid) const noexcept = 0;"));
        assert!(cpp_code.foreign_code.contains(
            r#"
    static void c_on_save(const UuidOpaque * uuid, void *opaque)
    {
        assert(opaque != nullptr);
        auto pi = static_cast<const RepoChangedCallback *>(opaque);

        pi->on_save(UuidRef{ static_cast<const UuidOpaque *>(uuid) });
    }

    static void c_on_remove(const UuidOpaque * uuid, void *opaque)
    {
        assert(opaque != nullptr);
        auto pi = static_cast<const RepoChangedCallback *>(opaque);

        pi->on_remove(UuidRef{ static_cast<const UuidOpaque *>(uuid) });
    }
"#
        ));
    }
}

#[test]
fn test_derive_extension_usage() {
    let _ = env_logger::try_init();
    let rust_src = r#"
foreign_class!(
#[derive(QObject)]
class MyObj {
    self_type MyObj;
    private constructor MyObj::new() -> MyObj;
    #[Q_INVOKABLE]
    fn MyObj::f();
});

foreign_enum!(
#[derive(EnumClass)]
/// enum comment
enum MyEnum {
  A = MyEnum::A,
  B = MyEnum::B,
}
);
"#;
    let tmp_dir = tempdir().expect("Can not create tmp directory");
    let swig_gen = Generator::new(LanguageConfig::CppConfig(CppConfig::new(
        tmp_dir.path().into(),
        "org_examples".into(),
    )))
    .with_pointer_target_width(64)
    .register_class_attribute_callback("QObject", |code, class_name| {
        println!("class attribute callback class_name {}", class_name);
        let include = b"#include";
        let mut last_pos = 0;
        let mut search_shift = 0;
        while let Some(pos) = find_subsequence(&code[(last_pos + search_shift)..], include) {
            last_pos += pos + search_shift;
            search_shift = 1;
        }
        let last_include_pos = last_pos;
        let new_line = &code[last_include_pos..].iter().position(|x| *x == b'\n');
        let new_line_pos = last_include_pos + new_line.unwrap();
        let addon = br##"
#include <QObject>
"##;
        code.splice(new_line_pos..new_line_pos, addon.iter().copied());

        let needle = format!("class {}Wrapper {{", class_name);
        let class_pos = find_subsequence(&code, needle.as_bytes()).unwrap();
        let end_pos = class_pos + needle.as_bytes().len();
        let new_code = format!(
            r#"class {}Wrapper : public QObject {{
    Q_OBJECT"#,
            class_name
        );
        code.splice(class_pos..end_pos, new_code.as_bytes().iter().copied());
    })
    .register_method_attribute_callback("Q_INVOKABLE", |code, ctx| {
        println!(
            "method attribute callback class {}, method {}",
            ctx.class_name, ctx.method_name
        );
        let needle = match ctx.variant {
            flapigen::MethodVariant::Constructor => {
                panic!("unsupported");
            }
            flapigen::MethodVariant::Method(_) => format!("void {}(", ctx.method_name),
            flapigen::MethodVariant::StaticMethod => format!("static void {}(", ctx.method_name),
        };
        let pos = find_subsequence(&code, needle.as_bytes()).unwrap();
        code.splice(pos..pos, b"Q_INVOKABLE ".iter().copied());
    })
    .register_enum_attribute_callback("EnumClass", |code, ctx| {
        println!("EnumClass callback, ctx {}", ctx);
        let needle = format!("enum {}", ctx);
        let pos = find_subsequence(&code, needle.as_bytes()).unwrap();
        code.splice((pos + 5)..(pos + 5), b"class ".iter().copied());
    });
    let rust_code_path = tmp_dir.path().join("test.rs");

    let rust_src_path = tmp_dir.path().join("src.rs");
    fs::write(&rust_src_path, rust_src).unwrap();
    swig_gen.expand("derive_extension_usage", rust_src_path, &rust_code_path);

    let _rust_code = fs::read_to_string(rust_code_path).unwrap();
    let foreign_code = collect_code_in_dir(tmp_dir.path(), &[".h", ".hpp"]).unwrap();
    println!("foreign_code: {}", foreign_code);
    assert!(foreign_code.contains(
        r##"#include <QObject>
"##
    ));
    assert!(foreign_code.contains(
        r#"
class MyObjWrapper : public QObject {
    Q_OBJECT
"#
    ));
    assert!(foreign_code.contains("Q_INVOKABLE static void f()"));
    assert!(foreign_code.contains(
        r#"//enum comment
enum class MyEnum {
A = 0,
B = 1"#
    ));
    tmp_dir.close().unwrap();
}

fn find_subsequence<T>(haystack: &[T], needle: &[T]) -> Option<usize>
where
    for<'a> &'a [T]: PartialEq,
{
    haystack
        .windows(needle.len())
        .position(|window| window == needle)
}

#[derive(PartialEq, Debug, Clone, Copy)]
enum ForeignLang {
    Java,
    Cpp,
}

#[derive(Clone)]
struct CodePair {
    rust_code: String,
    foreign_code: String,
}

struct PrintTestInfo {
    code_pair: CodePair,
    test_name: String,
    lang: ForeignLang,
    print_on_drop: bool,
    foreign_code_search_pattern: String,
    rust_pat: String,
}

impl PrintTestInfo {
    fn new(code_pair: CodePair, test_name: String, lang: ForeignLang) -> Self {
        PrintTestInfo {
            code_pair,
            test_name,
            lang,
            print_on_drop: true,
            foreign_code_search_pattern: String::new(),
            rust_pat: String::new(),
        }
    }
    fn success(&mut self) {
        self.print_on_drop = false;
    }
}

impl Drop for PrintTestInfo {
    fn drop(&mut self) {
        if self.print_on_drop {
            if !self.foreign_code_search_pattern.is_empty() {
                println!(
                    "{} / {:?}: search foreign pat '{}'",
                    self.test_name, self.lang, self.foreign_code_search_pattern
                );
            }

            if !self.rust_pat.is_empty() {
                println!(
                    "{} / {:?}: search rust pat '{}'",
                    self.test_name, self.lang, self.rust_pat,
                );
            }

            println!(
                "{} / {:?}: flapigen generated such foreign_code: {}",
                self.test_name, self.lang, self.code_pair.foreign_code
            );
            println!(
                "{} / {:?}: flapigen generated such rust_code: {}",
                self.test_name,
                self.lang,
                rustfmt_without_errors(self.code_pair.rust_code.clone()),
            );
        }
    }
}

#[derive(Debug)]
struct Error {
    msg: String,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl From<std::io::Error> for Error {
    fn from(x: std::io::Error) -> Self {
        Error {
            msg: format!("io: {}", x),
        }
    }
}

impl From<syn::Error> for Error {
    fn from(x: syn::Error) -> Self {
        Error {
            msg: format!("syn: {}", x),
        }
    }
}

impl std::error::Error for Error {}

fn collect_code_in_dir(dir_with_code: &Path, exts: &[&str]) -> Result<String, Error> {
    let mut code = String::new();
    for path in fs::read_dir(dir_with_code)? {
        let path = path?;
        if path.file_type()?.is_file()
            && exts
                .iter()
                .any(|ext| path.path().to_str().map_or(false, |x| x.ends_with(ext)))
        {
            code.push_str(format!("<<< generated file: {:?} >>>\n", path.file_name()).as_str());
            code.push_str(&fs::read_to_string(path.path())?);
            code.push('\n');
            code.push_str(format!(">>> end of file: {:?} <<<\n", path.file_name()).as_str());
        }
    }
    Ok(code)
}

enum Source<'a> {
    Str(&'a str),
    Path(&'a Path),
}

fn parse_code(test_name: &str, rust_src: Source, lang: ForeignLang) -> Result<CodePair, Error> {
    let tmp_dir = tempdir().expect("Can not create tmp directory");
    let (swig_gen, ext_list): (Generator, &[&'static str]) = match lang {
        ForeignLang::Java => {
            let swig_gen = Generator::new(LanguageConfig::JavaConfig(
                JavaConfig::new(tmp_dir.path().into(), "org.example".into())
                    .use_null_annotation_from_package("android.support.annotation".into()),
            ))
            .with_pointer_target_width(64);

            (swig_gen, &[".java"])
        }
        ForeignLang::Cpp => {
            let swig_gen = Generator::new(LanguageConfig::CppConfig(CppConfig::new(
                tmp_dir.path().into(),
                "org_examples".into(),
            )))
            .with_pointer_target_width(64);
            (swig_gen, &[".h", ".hpp"])
        }
    };

    let rust_code_path = tmp_dir.path().join("test.rs");
    match rust_src {
        Source::Path(rust_src_path) => swig_gen.expand(test_name, rust_src_path, &rust_code_path),
        Source::Str(rust_src) => {
            let rust_src_path = tmp_dir.path().join("src.rs");
            fs::write(&rust_src_path, rust_src)?;
            swig_gen.expand(test_name, rust_src_path, &rust_code_path);
        }
    }

    let rust_code = fs::read_to_string(rust_code_path)?;
    let foreign_code = collect_code_in_dir(tmp_dir.path(), ext_list)?;
    tmp_dir.close()?;

    Ok(CodePair {
        rust_code,
        foreign_code,
    })
}

struct ExpectationPatterns(Vec<String>);

impl syn::parse::Parse for ExpectationPatterns {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let lit_vec: syn::punctuated::Punctuated<syn::LitStr, Token![;]> =
            syn::punctuated::Punctuated::parse_terminated(input)?;
        Ok(ExpectationPatterns(
            lit_vec.into_iter().map(|v| v.value()).collect(),
        ))
    }
}

fn parse_code_expectation(exp_path: &Path) -> Result<Vec<String>, Error> {
    let patterns_str = fs::read_to_string(exp_path)?;
    let pats: ExpectationPatterns = syn::parse_str(&patterns_str)?;

    Ok(pats.0)
}

fn new_path(main_path: &Path, ext: &str) -> PathBuf {
    let base_name = main_path.file_stem().expect("name without extenstion");
    let mut new_name: OsString = base_name.into();
    new_name.push(ext);
    main_path.with_file_name(new_name)
}

fn check_expectation(test_name: &str, test_case: &Path, lang: ForeignLang) -> bool {
    let (main_ext, rust_ext) = match lang {
        ForeignLang::Cpp => (".cpp", ".cpp_rs"),
        ForeignLang::Java => (".java", ".java_rs"),
    };
    let main_expectation = new_path(test_case, main_ext);
    if main_expectation.exists() {
        let code_pair =
            parse_code(&test_name, Source::Path(&test_case), lang).expect("parse_code failed");
        let pats = parse_code_expectation(&main_expectation).expect("parsing of patterns failed");

        let mut print_test_info = PrintTestInfo::new(code_pair.clone(), test_name.into(), lang);
        for pat in pats {
            print_test_info.foreign_code_search_pattern = pat.clone();
            assert!(code_pair.foreign_code.contains(&pat));
        }
        print_test_info.foreign_code_search_pattern.clear();

        let rust_cpp_expectation = new_path(&test_case, rust_ext);
        if rust_cpp_expectation.exists() {
            let pats =
                parse_code_expectation(&rust_cpp_expectation).expect("parsing of patterns failed");
            let pats: Vec<String> = pats
                .into_iter()
                .map(|v| rustfmt_without_errors(v))
                .collect();
            let rust_code = rustfmt_without_errors(code_pair.rust_code);
            for pat in pats {
                print_test_info.rust_pat = pat.clone();
                assert!(rust_code.contains(&pat));
            }
            print_test_info.rust_pat.clear();
        }
        print_test_info.success();
        true
    } else {
        false
    }
}

fn rustfmt_without_errors(rust_code: String) -> String {
    let rust_code2 = rust_code.clone();
    match rustfmt_cnt(rust_code.into_bytes(), RustEdition::Edition2018) {
        Ok(code) => String::from_utf8(code).expect("not valid utf-8"),
        Err(err) => {
            warn!("rustfmt failed: {}", err);
            rust_code2
        }
    }
}
