//! `rust_swig` is a Rust Simplified Wrapper and Interface Generator used
//! to connect other programming languages to Rust.
//! It is designed to be used from
//! [cargo build scripts](https://doc.rust-lang.org/cargo/reference/build-scripts.html).
//! The idea of this softwared based on [swig](http://www.swig.org).
//! For macros expansion it uses [syntex](https://crates.io/crates/syntex).
//! More details can be found at
//! [README](https://github.com/Dushistov/rust_swig/blob/master/README.md)
#[macro_use]
extern crate bitflags;
#[cfg(test)]
extern crate env_logger;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;
extern crate petgraph;
extern crate syntex;
extern crate syntex_errors;
extern crate syntex_pos;
extern crate syntex_syntax;

macro_rules! unwrap_presult {
    ($presult_epxr:expr) => {
        match $presult_epxr {
            Ok(x) => x,
            Err(mut err) => {
                err.emit();
                panic!("rust_swig fatal error, see above");
            }
        }
    };
    ($presult_epxr:expr, $conv_map:expr) => {
        match $presult_epxr {
            Ok(x) => x,
            Err(mut err) => {
                debug!("{}", $conv_map);
                err.emit();
                panic!("rust_swig fatal error, see above");
            }
        }
    };
}

mod cpp;
mod errors;
pub mod file_cache;
mod java_jni;
mod my_ast;
mod parsing;
mod types_conv_map;

use std::cell::RefCell;
use std::collections::HashSet;
use std::env;
use std::path::PathBuf;
use std::rc::Rc;
use std::str::FromStr;

use syntex::Registry;
use syntex_pos::DUMMY_SP;
use syntex_syntax::ast;
use syntex_syntax::ast::DUMMY_NODE_ID;
use syntex_syntax::codemap::Span;
use syntex_syntax::ext::base::{ExtCtxt, MacEager, MacResult, TTMacroExpander};
use syntex_syntax::parse::PResult;
use syntex_syntax::parse::ParseSess;
use syntex_syntax::ptr::P;
use syntex_syntax::symbol::Symbol;
use syntex_syntax::tokenstream::TokenTree;
use syntex_syntax::util::small_vector::SmallVector;

use errors::fatal_error;
use my_ast::RustType;
use parsing::{parse_foreign_enum, parse_foreign_interface, parse_foreigner_class};
use types_conv_map::TypesConvMap;

/// Calculate target pointer width from environment variable
/// that `cargo` inserts
pub fn target_pointer_width_from_env() -> Option<usize> {
    env::var("CARGO_CFG_TARGET_POINTER_WIDTH")
        .ok()
        .map(|p_width| {
            <usize>::from_str(&p_width)
                .expect("Can not convert CARGO_CFG_TARGET_POINTER_WIDTH to usize")
        })
}

/// `LanguageConfig` contains configuration for specific programming language
pub enum LanguageConfig {
    JavaConfig(JavaConfig),
    CppConfig(CppConfig),
}

trait LanguageGenerator {
    fn generate<'a>(
        &self,
        sess: &'a ParseSess,
        conv_map: &mut TypesConvMap,
        pointer_target_width: usize,
        class: &ForeignerClassInfo,
    ) -> PResult<'a, Vec<P<ast::Item>>>;

    fn generate_enum<'a>(
        &self,
        sess: &'a ParseSess,
        conv_map: &mut TypesConvMap,
        pointer_target_width: usize,
        enum_info: &ForeignEnumInfo,
    ) -> PResult<'a, Vec<P<ast::Item>>>;

    fn generate_interface<'a>(
        &self,
        sess: &'a ParseSess,
        conv_map: &mut TypesConvMap,
        pointer_target_width: usize,
        interace: &ForeignInterface,
    ) -> PResult<'a, Vec<P<ast::Item>>>;

    fn place_foreign_lang_helpers(&self, _: &[SourceCode]) -> Result<(), String> {
        Ok(())
    }
}

/// `Generator` is a main point of `rust_swig`.
/// It expands rust macroses and generates not rust code.
/// It designed to use inside `build.rs`.
pub struct Generator {
    pointer_target_width: Option<usize>,
    // Because of API of syntex, to register for several macroses
    data: Rc<RefCell<GeneratorData>>,
}

struct GeneratorData {
    init_done: bool,
    config: LanguageConfig,
    conv_map: TypesConvMap,
    conv_map_source: Vec<SourceCode>,
    foreign_lang_helpers: Vec<SourceCode>,
    pointer_target_width: usize,
}

struct SourceCode {
    id_of_code: String,
    code: String,
}

#[derive(PartialEq, Clone, Copy, Debug)]
enum SelfTypeVariant {
    RptrMut,
    Rptr,
    Mut,
    Default,
}

impl SelfTypeVariant {
    fn is_read_only(&self) -> bool {
        match *self {
            SelfTypeVariant::RptrMut | SelfTypeVariant::Mut => false,
            SelfTypeVariant::Default | SelfTypeVariant::Rptr => true,
        }
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
enum MethodVariant {
    Constructor,
    Method(SelfTypeVariant),
    StaticMethod,
}

#[derive(PartialEq, Clone, Copy, Debug)]
enum MethodAccess {
    Private,
    Public,
    Protected,
}

#[derive(Debug, Clone)]
struct ForeignerMethod {
    variant: MethodVariant,
    rust_id: ast::Path,
    fn_decl: P<ast::FnDecl>,
    name_alias: Option<Symbol>,
    /// cache if rust_fn_decl.output == Result
    may_return_error: bool,
    access: MethodAccess,
    doc_comments: Vec<Symbol>,
}

impl ForeignerMethod {
    fn short_name(&self) -> Symbol {
        if let Some(name) = self.name_alias {
            name
        } else {
            match self.rust_id.segments.len() {
                0 => Symbol::intern(""),
                n => self.rust_id.segments[n - 1].identifier.name,
            }
        }
    }

    fn span(&self) -> Span {
        self.rust_id.span
    }

    fn is_dummy_constructor(&self) -> bool {
        self.rust_id.segments.is_empty()
    }
}

#[derive(Debug, Clone)]
struct ForeignerClassInfo {
    name: Symbol,
    methods: Vec<ForeignerMethod>,
    self_type: Option<RustType>,
    /// Not necessarily equal to self_type, may be for example Rc<self_type>
    this_type_for_method: Option<ast::Ty>,
    foreigner_code: String,
    /// For example if we have `fn new(x: X) -> Result<Y, Z>`, then Result<Y, Z>
    constructor_ret_type: Option<ast::Ty>,
    span: Span,
    doc_comments: Vec<Symbol>,
}

impl ForeignerClassInfo {
    fn self_type_name(&self) -> Symbol {
        self.self_type
            .as_ref()
            .map(|x| x.normalized_name)
            .unwrap_or(Symbol::intern(""))
    }
    fn self_type_as_ty(&self) -> ast::Ty {
        self.self_type
            .as_ref()
            .map(|x| x.ty.clone())
            .unwrap_or_else(|| ast::Ty {
                id: DUMMY_NODE_ID,
                span: DUMMY_SP,
                node: ast::TyKind::Path(
                    None,
                    ast::Path {
                        span: DUMMY_SP,
                        segments: Vec::new(),
                    },
                ),
            })
    }
    /// common for several language binding generator code
    fn validate_class(&self) -> Result<(), String> {
        let mut has_constructor = false;
        let mut has_methods = false;
        for x in &self.methods {
            match x.variant {
                MethodVariant::Constructor => has_constructor = true,
                MethodVariant::Method(_) => has_methods = true,
                _ => {}
            }
        }
        if self.self_type.is_none() && has_constructor {
            Err(format!(
                "class {} has constructor, but no self_type defined",
                self.name
            ))
        } else if self.self_type.is_none() && has_methods {
            Err(format!(
                "class {} has methods, but no self_type defined",
                self.name
            ))
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, Clone)]
struct ForeignEnumItem {
    name: Symbol,
    span: Span,
    rust_name: ast::Path,
    doc_comments: Vec<Symbol>,
}

#[derive(Debug, Clone)]
struct ForeignEnumInfo {
    name: Symbol,
    span: Span,
    items: Vec<ForeignEnumItem>,
    doc_comments: Vec<Symbol>,
}

impl ForeignEnumInfo {
    fn rust_enum_name(&self) -> Symbol {
        self.name
    }
}

struct ForeignInterfaceMethod {
    name: Symbol,
    rust_name: ast::Path,
    fn_decl: P<ast::FnDecl>,
    doc_comments: Vec<Symbol>,
}

struct ForeignInterface {
    name: Symbol,
    self_type: ast::Path,
    doc_comments: Vec<Symbol>,
    items: Vec<ForeignInterfaceMethod>,
    span: Span,
}

impl Generator {
    pub fn new(config: LanguageConfig) -> Generator {
        let pointer_target_width = target_pointer_width_from_env();
        let mut conv_map_source = Vec::new();
        let mut foreign_lang_helpers = Vec::new();
        match config {
            LanguageConfig::JavaConfig(ref java_cfg) => {
                conv_map_source.push(SourceCode {
                    id_of_code: "jni-include.rs".into(),
                    code: include_str!("java_jni/jni-include.rs")
                        .replace(
                            "java.util.Optional",
                            &format!("{}.Optional", java_cfg.optional_package),
                        ).replace(
                            "java/util/Optional",
                            &format!("{}/Optional", java_cfg.optional_package.replace('.', "/")),
                        ),
                });
            }
            LanguageConfig::CppConfig(..) => {
                conv_map_source.push(SourceCode {
                    id_of_code: "cpp-include.rs".into(),
                    code: include_str!("cpp/cpp-include.rs").into(),
                });
                foreign_lang_helpers.push(SourceCode {
                    id_of_code: "rust_str.h".into(),
                    code: include_str!("cpp/rust_str.h").into(),
                });
                foreign_lang_helpers.push(SourceCode {
                    id_of_code: "rust_vec.h".into(),
                    code: include_str!("cpp/rust_vec.h").into(),
                });
                foreign_lang_helpers.push(SourceCode {
                    id_of_code: "rust_result.h".into(),
                    code: include_str!("cpp/rust_result.h").into(),
                });
                foreign_lang_helpers.push(SourceCode {
                    id_of_code: "rust_option.h".into(),
                    code: include_str!("cpp/rust_option.h").into(),
                });
            }
        }
        Generator {
            pointer_target_width,
            data: Rc::new(RefCell::new(GeneratorData {
                init_done: false,
                config,
                conv_map: TypesConvMap::default(),
                conv_map_source,
                foreign_lang_helpers,
                pointer_target_width: pointer_target_width.unwrap_or(0),
            })),
        }
    }

    pub fn with_pointer_target_width(mut self, pointer_target_width: usize) -> Generator {
        self.pointer_target_width = Some(pointer_target_width);
        self.data.borrow_mut().pointer_target_width = pointer_target_width;
        self
    }

    pub fn register(self, registry: &mut Registry) {
        self.pointer_target_width.unwrap_or_else(|| {
            panic!(
                r#"pointer target width unknown,
 set env CARGO_CFG_TARGET_POINTER_WIDTH environment variable,
 or use `with_pointer_target_width` function
"#
            )
        });
        registry.add_macro("foreign_enum", EnumHandler(self.data.clone()));
        registry.add_macro("foreign_interface", InterfaceHandler(self.data.clone()));
        registry.add_macro("foreigner_class", self);
    }

    /// Add new foreign langauge type <-> Rust mapping
    pub fn merge_type_map(self, id_of_code: &str, code: &str) -> Generator {
        self.data.borrow_mut().conv_map_source.push(SourceCode {
            id_of_code: id_of_code.into(),
            code: code.into(),
        });
        self
    }
}

impl TTMacroExpander for Generator {
    fn expand<'a>(
        &self,
        cx: &'a mut ExtCtxt,
        _: Span,
        tokens: &[TokenTree],
    ) -> Box<MacResult + 'a> {
        self.data.borrow_mut().expand_foreigner_class(cx, tokens)
    }
}

struct EnumHandler(Rc<RefCell<GeneratorData>>);

impl TTMacroExpander for EnumHandler {
    fn expand<'a>(
        &self,
        cx: &'a mut ExtCtxt,
        _: Span,
        tokens: &[TokenTree],
    ) -> Box<MacResult + 'a> {
        self.0.borrow_mut().expand_foreign_enum(cx, tokens)
    }
}

struct InterfaceHandler(Rc<RefCell<GeneratorData>>);
impl TTMacroExpander for InterfaceHandler {
    fn expand<'a>(
        &self,
        cx: &'a mut ExtCtxt,
        _: Span,
        tokens: &[TokenTree],
    ) -> Box<MacResult + 'a> {
        self.0.borrow_mut().expand_foreign_interface(cx, tokens)
    }
}

impl GeneratorData {
    fn generate_code_for_foreign_interface<'a>(
        cx: &'a mut ExtCtxt,
        conv_map: &mut TypesConvMap,
        pointer_target_width: usize,
        foreign_interface: &ForeignInterface,
        lang_gen: &LanguageGenerator,
        mut items: Vec<P<ast::Item>>,
    ) -> Box<MacResult + 'a> {
        let mut gen_items = unwrap_presult!(
            lang_gen.generate_interface(
                cx.parse_sess(),
                conv_map,
                pointer_target_width,
                foreign_interface
            ),
            conv_map
        );
        items.append(&mut gen_items);
        MacEager::items(SmallVector::many(items))
    }

    fn language_generator(cfg: &LanguageConfig) -> &LanguageGenerator {
        match cfg {
            LanguageConfig::JavaConfig(ref java_cfg) => java_cfg,
            LanguageConfig::CppConfig(ref cpp_cfg) => cpp_cfg,
        }
    }

    fn expand_foreign_interface<'a>(
        &mut self,
        cx: &'a mut ExtCtxt,
        tokens: &[TokenTree],
    ) -> Box<MacResult + 'a> {
        let pointer_target_width = self.pointer_target_width;
        let items = unwrap_presult!(
            self.init_types_map(cx.parse_sess(), pointer_target_width),
            self.conv_map
        );
        let foreign_interface =
            parse_foreign_interface(cx, tokens).expect("Can not parse foreign_interface");
        GeneratorData::generate_code_for_foreign_interface(
            cx,
            &mut self.conv_map,
            self.pointer_target_width,
            &foreign_interface,
            GeneratorData::language_generator(&self.config),
            items,
        )
    }

    fn generate_code_for_enum<'a>(
        cx: &'a mut ExtCtxt,
        conv_map: &mut TypesConvMap,
        pointer_target_width: usize,
        foreign_enum: &ForeignEnumInfo,
        lang_gen: &LanguageGenerator,
        mut items: Vec<P<ast::Item>>,
    ) -> Box<MacResult + 'a> {
        let mut gen_items = unwrap_presult!(
            lang_gen.generate_enum(
                cx.parse_sess(),
                conv_map,
                pointer_target_width,
                foreign_enum
            ),
            conv_map
        );
        items.append(&mut gen_items);
        MacEager::items(SmallVector::many(items))
    }

    fn expand_foreign_enum<'a>(
        &mut self,
        cx: &'a mut ExtCtxt,
        tokens: &[TokenTree],
    ) -> Box<MacResult + 'a> {
        let pointer_target_width = self.pointer_target_width;
        let items = unwrap_presult!(
            self.init_types_map(cx.parse_sess(), pointer_target_width),
            self.conv_map
        );
        let foreign_enum = parse_foreign_enum(cx, tokens).expect("Can not parse foreign_enum");

        GeneratorData::generate_code_for_enum(
            cx,
            &mut self.conv_map,
            self.pointer_target_width,
            &foreign_enum,
            GeneratorData::language_generator(&self.config),
            items,
        )
    }

    fn generate_code_for_class<'a>(
        cx: &'a mut ExtCtxt,
        conv_map: &mut TypesConvMap,
        pointer_target_width: usize,
        foreign_class: &ForeignerClassInfo,
        lang_gen: &LanguageGenerator,
        mut items: Vec<P<ast::Item>>,
    ) -> Box<MacResult + 'a> {
        let mut gen_items = unwrap_presult!(
            lang_gen.generate(
                cx.parse_sess(),
                conv_map,
                pointer_target_width,
                foreign_class,
            ),
            conv_map
        );
        items.append(&mut gen_items);
        MacEager::items(SmallVector::many(items))
    }

    fn expand_foreigner_class<'a>(
        &mut self,
        cx: &'a mut ExtCtxt,
        tokens: &[TokenTree],
    ) -> Box<MacResult + 'a> {
        let pointer_target_width = self.pointer_target_width;
        let items = unwrap_presult!(
            self.init_types_map(cx.parse_sess(), pointer_target_width),
            self.conv_map
        );
        let foreigner_class = match parse_foreigner_class(cx, &self.config, tokens) {
            Ok(x) => x,
            Err(_) => {
                panic!("Can not parse foreigner_class");
                //return DummyResult::any(span);
            }
        };
        debug!(
            "expand_foreigner_class: self {:?}, this_for_method {:?}, constructor {:?}",
            foreigner_class.self_type,
            foreigner_class.this_type_for_method,
            foreigner_class.constructor_ret_type
        );
        self.conv_map.register_foreigner_class(&foreigner_class);

        GeneratorData::generate_code_for_class(
            cx,
            &mut self.conv_map,
            self.pointer_target_width,
            &foreigner_class,
            GeneratorData::language_generator(&self.config),
            items,
        )
    }

    fn init_types_map<'a>(
        &mut self,
        sess: &'a ParseSess,
        target_pointer_width: usize,
    ) -> PResult<'a, Vec<P<ast::Item>>> {
        if self.init_done {
            return Ok(vec![]);
        }
        self.init_done = true;
        for code in &self.conv_map_source {
            self.conv_map
                .merge(sess, &code.id_of_code, &code.code, target_pointer_width)?;
        }

        if self.conv_map.is_empty() {
            return Err(fatal_error(
                sess,
                DUMMY_SP,
                "After merge all \"types maps\" have no convertion code",
            ));
        }

        GeneratorData::language_generator(&self.config)
            .place_foreign_lang_helpers(&self.foreign_lang_helpers)
            .map_err(|err| {
                fatal_error(
                    sess,
                    DUMMY_SP,
                    &format!("Can not put/generate foreign lang helpers: {}", err),
                )
            })?;

        Ok(self.conv_map.take_utils_code())
    }
}

/// Configuration for Java binding generation
pub struct JavaConfig {
    output_dir: PathBuf,
    package_name: String,
    use_null_annotation: Option<String>,
    optional_package: String,
}

impl JavaConfig {
    /// Create `JavaConfig`
    /// # Arguments
    /// * `output_dir` - directory where place generated java files
    /// * `package_name` - package name for generated java files
    pub fn new(output_dir: PathBuf, package_name: String) -> JavaConfig {
        JavaConfig {
            output_dir,
            package_name,
            use_null_annotation: None,
            optional_package: "java.util".to_string(),
        }
    }
    /// Use @NonNull for types where appropriate
    /// # Arguments
    /// * `import_annotation` - import statement for @NonNull,
    ///                         for example android.support.annotation.NonNull
    pub fn use_null_annotation(mut self, import_annotation: String) -> JavaConfig {
        self.use_null_annotation = Some(import_annotation);
        self
    }
    /// If you use JDK without java.util.Optional*, then you can provide
    /// name of custom package with Optional
    pub fn use_optional_package(mut self, optional_package: String) -> JavaConfig {
        self.optional_package = optional_package;
        self
    }
}

/// To which `C++` type map `std::option::Option`
pub enum CppOptional {
    /// `std::optional` from C++17 standard
    Std17,
    /// `boost::optional`
    Boost,
}

/// To which `C++` type map `std::result::Result`
pub enum CppVariant {
    /// `std::variant` from C++17 standard
    Std17,
    /// `boost::variant`
    Boost,
}

/// Configuration for C++ binding generation
pub struct CppConfig {
    output_dir: PathBuf,
    namespace_name: String,
    cpp_optional: CppOptional,
    cpp_variant: CppVariant,
    generated_helper_files: RefCell<HashSet<PathBuf>>,
    to_generate: RefCell<Vec<P<ast::Item>>>,
}

impl CppConfig {
    /// Create `CppConfig`
    /// # Arguments
    /// * `output_dir` - directory where place generated c++ files
    /// * `namespace_name` - namespace name for generated c++ classes
    pub fn new(output_dir: PathBuf, namespace_name: String) -> CppConfig {
        CppConfig {
            output_dir,
            namespace_name,
            cpp_optional: CppOptional::Std17,
            cpp_variant: CppVariant::Std17,
            generated_helper_files: RefCell::new(HashSet::new()),
            to_generate: RefCell::new(vec![]),
        }
    }
    pub fn cpp_optional(self, cpp_optional: CppOptional) -> CppConfig {
        CppConfig {
            cpp_optional,
            ..self
        }
    }
    pub fn cpp_variant(self, cpp_variant: CppVariant) -> CppConfig {
        CppConfig {
            cpp_variant,
            ..self
        }
    }
    pub fn use_boost(self) -> CppConfig {
        CppConfig {
            cpp_variant: CppVariant::Boost,
            cpp_optional: CppOptional::Boost,
            ..self
        }
    }
}
