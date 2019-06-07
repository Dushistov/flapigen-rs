use std::{
    env,
    ffi::OsString,
    fs, panic,
    path::{Path, PathBuf},
};

use rust_swig::{CppConfig, Generator, JavaConfig, LanguageConfig};
use syn::Token;
use tempfile::tempdir;

#[test]
fn test_expectations_main() {
    let _ = env_logger::builder()
        .default_format_timestamp(false)
        .try_init();

    let test_cases: Vec<PathBuf> = fs::read_dir(Path::new("tests").join("expectations"))
        .expect("read_dir failed")
        .into_iter()
        .filter_map(|p| {
            if let Ok(path) = p {
                if let Ok(ft) = path.file_type() {
                    if ft.is_file() && path.path().to_str().map_or(false, |x| x.ends_with(".rs")) {
                        return Some(path.path());
                    }
                }
            }
            None
        })
        .collect();

    let mut ntests = 0_usize;

    fn check_expectation(test_name: &str, test_case: &Path, lang: ForeignLang) -> bool {
        let (main_ext, rust_ext) = match lang {
            ForeignLang::Cpp => (".cpp", ".cpp_rs"),
            ForeignLang::Java => (".java", ".java_rs"),
        };
        let main_expectation = new_path(test_case, main_ext);
        if main_expectation.exists() {
            let code_pair =
                parse_code(&test_name, Source::Path(&test_case), lang).expect("parse_code failed");
            let pats =
                parse_code_expectation(&main_expectation).expect("parsing of patterns failed");

            let mut print_test_info = PrintTestInfo::new(code_pair.clone(), test_name.into(), lang);
            for pat in pats {
                print_test_info.foreign_code_search_pattern = pat.clone();
                assert!(code_pair.foreign_code.contains(&pat));
            }
            print_test_info.foreign_code_search_pattern.clear();

            let rust_cpp_expectation = new_path(&test_case, rust_ext);
            if rust_cpp_expectation.exists() {
                let pats = parse_code_expectation(&rust_cpp_expectation)
                    .expect("parsing of patterns failed");
                let pats: Vec<String> = pats.into_iter().map(|v| v.replace("\n", "")).collect();
                for pat in pats {
                    print_test_info.rust_pat = pat.clone();
                    assert!(code_pair.rust_code.contains(&pat));
                }
                print_test_info.rust_pat.clear();
            }
            print_test_info.success();
            true
        } else {
            false
        }
    }

    let filter = env::var("RUST_SWIG_EXPECT_RUN_ONLY").ok();

    for test_case in test_cases {
        let base_name = test_case.file_stem().expect("name without extenstion");
        let test_name = base_name.to_string_lossy();
        if filter.as_ref().map(|v| *v != test_name).unwrap_or(false) {
            continue;
        }

        let mut test_something = false;
        for lang in &[ForeignLang::Cpp, ForeignLang::Java] {
            if check_expectation(&test_name, &test_case, *lang) {
                test_something = true;
            }
        }
        if test_something {
            ntests += 1;
        }
    }

    assert_eq!(44, ntests);
}

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
foreigner_class!(class Foo {
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
    foreigner_class!(class Foo {
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
    foreigner_class!(class Foo {
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
foreigner_class!(class Boo {
    self_type Boo;
    constructor Boo::default() -> Boo;
});
foreigner_class!(class FooImpl {
    self_type Foo<'a>;
    constructor Foo::create() -> Foo<'a>;
    method Foo::set_alternate_boarding(&mut self, p: Vec<Boo>);
    alias setAlternateBoarding;
});
"#;
    ;
    for _ in 0..100 {
        let cpp_code = parse_code(name, Source::Str(src), ForeignLang::Cpp).expect("parse failed");
        println!("c/c++: {}", cpp_code.foreign_code);
        assert!(cpp_code
            .foreign_code
            .contains("void setAlternateBoarding(RustForeignVecBoo a_0)"));
        let java_code =
            parse_code(name, Source::Str(src), ForeignLang::Java).expect("parse failed");
        println!("Java: {}", java_code.foreign_code);
        assert!(java_code
            .foreign_code
            .contains("void setAlternateBoarding(@NonNull Boo [] a0)"));
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

foreigner_class!(class TestEnumClass {
    self_type Moo;
    constructor Moo::default() -> Moo;
    method Moo::f1(&mut self, v: MyEnum) -> i32;
    static_method Moo::next_enum(v: MyEnum) -> MyEnum;
});
"#;
    for _ in 0..10 {
        let _cpp_code = parse_code(name, Source::Str(src), ForeignLang::Cpp).unwrap();
        let java_code = parse_code(name, Source::Str(src), ForeignLang::Java).unwrap();
        println!("{}", java_code.rust_code);
        println!("{}", java_code.foreign_code);
        assert!(java_code.foreign_code.contains("int f1(@NonNull MyEnum"));
    }
}

#[test]
fn test_return_result_type_with_object() {
    let _ = env_logger::try_init();

    let name = "return_result_type_with_object";
    let src = r#"
foreigner_class!(class Position {
    self_type GnssInfo;
    private constructor create_position() -> GnssInfo;
    method Position::getLatitude(&self) -> f64;
});

foreigner_class!(class LocationService {
    static_method LocationService::position() -> Result<GnssInfo, String>;
    static_method LocationService::do_something() -> Result<(), String>;
});
"#;
    for i in 0..10 {
        println!("iter {}", i);
        let java_code = parse_code(name, Source::Str(src), ForeignLang::Java).unwrap();
        println!("{}", java_code.foreign_code);
        assert!(java_code
            .foreign_code
            .contains("public static native Position position() throws Exception;"));
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
            ),
            ForeignLang::Cpp,
        )
        .unwrap();
        println!("c/c++: {}", cpp_code.foreign_code);
        assert!(cpp_code.foreign_code.contains("BooRef get_boo() const"));
        assert!(cpp_code
            .foreign_code
            .contains("void Moo_update_boo(MooOpaque * const self, const BooOpaque * a_0);"));
        assert!(cpp_code
            .foreign_code
            .contains("void update_boo(const Boo & a_0)"));
    }
}

#[test]
fn test_foreign_interface_cpp() {
    let _ = env_logger::try_init();

    let name = "foreign_interface_cpp";
    let src = r#"
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
"#;
    for _ in 0..100 {
        let cpp_code = parse_code(name, Source::Str(src), ForeignLang::Cpp).unwrap();
        println!("c/c++: {}", cpp_code.foreign_code);
        assert!(cpp_code
            .foreign_code
            .contains("virtual void on_save(UuidRef a_0) = 0;"));
        assert!(cpp_code
            .foreign_code
            .contains("virtual void on_remove(UuidRef a_0) = 0;"));
        assert!(cpp_code.foreign_code.contains(
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
                "{} / {:?}: rust_swig generated such foreign_code: {}",
                self.test_name, self.lang, self.code_pair.foreign_code
            );
            println!(
                "{} / {:?}: rust_swig generated such rust_code: {}",
                self.test_name, self.lang, self.code_pair.rust_code
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
            code.push_str(&fs::read_to_string(path.path())?);
            code.push('\n');
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
