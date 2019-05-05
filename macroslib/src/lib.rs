//! `rust_swig` is a Rust Simplified Wrapper and Interface Generator used
//! to connect other programming languages to Rust.
//! It is designed to be used from
//! [cargo build scripts](https://doc.rust-lang.org/cargo/reference/build-scripts.html).
//! The idea of this softwared based on [swig](http://www.swig.org).
//! For macros expansion it uses [syntex](https://crates.io/crates/syntex).
//! More details can be found at
//! [README](https://github.com/Dushistov/rust_swig/blob/master/README.md)

macro_rules! parse_type {
    ($($tt:tt)*) => {{
        let ty: Type = parse_quote! { $($tt)* };
        ty
    }}
}

mod code_parse;
mod cpp;
mod error;
pub mod file_cache;
mod java_jni;
mod typemap;

use std::{
    cell::RefCell,
    env,
    io::Write,
    mem,
    path::{Path, PathBuf},
    str::FromStr,
};

use log::{debug, trace};
use proc_macro2::{Ident, Span, TokenStream};
use rustc_hash::FxHashSet;
use syn::{parse_quote, spanned::Spanned, Token, Type};

use crate::{
    error::{panic_on_parse_error, DiagnosticError, Result},
    typemap::{ast::DisplayToTokens, TypeMap},
};

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

/// Configuration for Java binding generation
pub struct JavaConfig {
    output_dir: PathBuf,
    package_name: String,
    null_annotation_package: Option<String>,
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
            null_annotation_package: None,
            optional_package: "java.util".to_string(),
        }
    }
    /// Use @NonNull for types where appropriate
    /// # Arguments
    /// * `import_annotation` - import statement for @NonNull,
    ///                         for example android.support.annotation.NonNull
    #[deprecated(note = "Use use_null_annotation_from_package instead")]
    pub fn use_null_annotation(mut self, import_annotation: String) -> JavaConfig {
        let suffix = ".NonNull";
        if !import_annotation.ends_with(suffix) {
            panic!(
                "import_annotation({}) should ends with {}",
                import_annotation, suffix
            );
        }
        let package = &import_annotation[0..import_annotation.len() - suffix.len()];
        self.null_annotation_package = Some(package.into());
        self
    }
    /// Use @NonNull/@Nullable for types where appropriate
    /// # Arguments
    /// * `null_annotation_package` - from which package import annotations like NonNull,
    ///                         for example for Android Studio
    ///                         you should pass android.support.annotation
    pub fn use_null_annotation_from_package(
        mut self,
        null_annotation_package: String,
    ) -> JavaConfig {
        self.null_annotation_package = Some(null_annotation_package);
        self
    }
    /// If you use JDK without java.util.Optional*, then you can provide
    /// name of custom package with Optional. Default value is "java.util"
    pub fn use_optional_package(mut self, optional_package: String) -> JavaConfig {
        self.optional_package = optional_package;
        self
    }
}

/// Configuration for C++ binding generation
pub struct CppConfig {
    output_dir: PathBuf,
    namespace_name: String,
    cpp_optional: CppOptional,
    cpp_variant: CppVariant,
    generated_helper_files: RefCell<FxHashSet<PathBuf>>,
    to_generate: RefCell<Vec<TokenStream>>,
    /// Create separate *_impl.hpp files with methods implementations.
    /// Can be necessary for the project with circular dependencies between classes.
    separate_impl_headers: bool,
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
            generated_helper_files: RefCell::new(FxHashSet::default()),
            to_generate: RefCell::new(vec![]),
            separate_impl_headers: false,
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
    /// Create separate *_impl.hpp files with methods' implementations.
    /// Can be necessary for the project with circular dependencies between classes.
    pub fn separate_impl_headers(self, separate_impl_headers: bool) -> CppConfig {
        CppConfig {
            separate_impl_headers,
            ..self
        }
    }
}

/// `Generator` is a main point of `rust_swig`.
/// It expands rust macroses and generates not rust code.
/// It designed to use inside `build.rs`.
pub struct Generator {
    init_done: bool,
    config: LanguageConfig,
    conv_map: TypeMap,
    conv_map_source: Vec<SourceCode>,
    foreign_lang_helpers: Vec<SourceCode>,
    pointer_target_width: usize,
}

struct SourceCode {
    id_of_code: String,
    code: String,
}

static FOREIGNER_CLASS: &str = "foreigner_class";
static FOREIGN_ENUM: &str = "foreign_enum";
static FOREIGN_INTERFACE: &str = "foreign_interface";

enum OutputCode {
    Item(syn::Item),
    Class(ForeignerClassInfo),
    Interface(ForeignInterface),
    Enum(ForeignEnumInfo),
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
                        )
                        .replace(
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
                foreign_lang_helpers.push(SourceCode {
                    id_of_code: "rust_tuple.h".into(),
                    code: include_str!("cpp/rust_tuple.h").into(),
                });
            }
        }
        Generator {
            init_done: false,
            config,
            conv_map: TypeMap::default(),
            conv_map_source,
            foreign_lang_helpers,
            pointer_target_width: pointer_target_width.unwrap_or(0),
        }
    }

    /// By default we get pointer_target_width via cargo (more exactly CARGO_CFG_TARGET_POINTER_WIDTH),
    /// but you can change default value via this method
    pub fn with_pointer_target_width(mut self, pointer_target_width: usize) -> Generator {
        self.pointer_target_width = pointer_target_width;
        self
    }

    /// Add new foreign langauge type <-> Rust mapping
    pub fn merge_type_map(mut self, id_of_code: &str, code: &str) -> Generator {
        self.conv_map_source.push(SourceCode {
            id_of_code: id_of_code.into(),
            code: code.into(),
        });
        self
    }

    /// process `src` and save result of macro expansion to `dst`
    ///
    /// # Panics
    /// Panics on error
    pub fn expand<S, D>(self, crate_name: &str, src: S, dst: D)
    where
        S: AsRef<Path>,
        D: AsRef<Path>,
    {
        let src_cnt = std::fs::read_to_string(src.as_ref()).unwrap_or_else(|err| {
            panic!(
                "Error during read for file {}: {}",
                src.as_ref().display(),
                err
            )
        });
        if let Err(mut err) = self.expand_str(&src_cnt, dst) {
            err.register_src(
                format!("{}: {}", crate_name, src.as_ref().display()),
                src_cnt.into(),
            );
            panic_on_parse_error(&err);
        }
    }

    /// process `src` and save result of macro expansion to `dst`
    ///
    /// # Panics
    /// Panics on I/O errors
    fn expand_str<D>(mut self, src: &str, dst: D) -> Result<()>
    where
        D: AsRef<Path>,
    {
        if self.pointer_target_width == 0 {
            panic!(
                r#"pointer target width unknown,
 set env CARGO_CFG_TARGET_POINTER_WIDTH environment variable,
 or use `with_pointer_target_width` function
"#
            );
        }
        let items = self.init_types_map(self.pointer_target_width)?;

        let syn_file = syn::parse_file(src)?;

        let mut file = file_cache::FileWriteCache::new(dst.as_ref());

        for item in items {
            write!(&mut file, "{}", DisplayToTokens(&item)).expect("mem I/O failed");
        }

        let mut output_code = vec![];

        for item in syn_file.items {
            if let syn::Item::Macro(mut item_macro) = item {
                let is_our_macro = [FOREIGNER_CLASS, FOREIGN_ENUM, FOREIGN_INTERFACE]
                    .iter()
                    .any(|x| item_macro.mac.path.is_ident(x));
                if !is_our_macro {
                    writeln!(&mut file, "{}", DisplayToTokens(&item_macro))
                        .expect("mem I/O failed");
                    continue;
                }
                trace!("Found {:?}", item_macro.mac.path);
                let mut tts = TokenStream::new();
                mem::swap(&mut tts, &mut item_macro.mac.tts);
                if item_macro.mac.path.is_ident(FOREIGNER_CLASS) {
                    let fclass = code_parse::parse_foreigner_class(&self.config, tts)?;
                    debug!(
                        "expand_foreigner_class: self {:?}, constructor {:?}",
                        fclass.self_type, fclass.constructor_ret_type
                    );
                    self.conv_map.register_foreigner_class(&fclass);
                    Generator::language_generator(&self.config)
                        .register_class(&mut self.conv_map, &fclass)?;
                    output_code.push(OutputCode::Class(fclass));
                } else if item_macro.mac.path.is_ident(FOREIGN_ENUM) {
                    let fenum = code_parse::parse_foreign_enum(tts)?;
                    output_code.push(OutputCode::Enum(fenum));
                } else if item_macro.mac.path.is_ident(FOREIGN_INTERFACE) {
                    let finterface = code_parse::parse_foreign_interface(tts)?;
                    output_code.push(OutputCode::Interface(finterface));
                } else {
                    unreachable!();
                }
            } else {
                output_code.push(OutputCode::Item(item));
            }
        }

        for code_item in output_code {
            match code_item {
                OutputCode::Class(fclass) => {
                    let code = Generator::language_generator(&self.config).generate(
                        &mut self.conv_map,
                        self.pointer_target_width,
                        &fclass,
                    )?;
                    for elem in code {
                        writeln!(&mut file, "{}", elem.to_string()).expect("mem I/O failed");
                    }
                }
                OutputCode::Enum(fenum) => {
                    let code = Generator::language_generator(&self.config).generate_enum(
                        &mut self.conv_map,
                        self.pointer_target_width,
                        &fenum,
                    )?;
                    for elem in code {
                        writeln!(&mut file, "{}", elem.to_string()).expect("mem I/O failed");
                    }
                }
                OutputCode::Interface(finterface) => {
                    let code = Generator::language_generator(&self.config).generate_interface(
                        &mut self.conv_map,
                        self.pointer_target_width,
                        &finterface,
                    )?;
                    for elem in code {
                        writeln!(&mut file, "{}", elem.to_string()).expect("mem I/O failed");
                    }
                }
                OutputCode::Item(item) => {
                    writeln!(&mut file, "{}", DisplayToTokens(&item)).expect("mem I/O failed");
                }
            }
        }

        file.update_file_if_necessary().unwrap_or_else(|err| {
            panic!(
                "Error during write to file {}: {}",
                dst.as_ref().display(),
                err
            );
        });
        Ok(())
    }

    fn init_types_map(&mut self, target_pointer_width: usize) -> Result<Vec<syn::Item>> {
        if self.init_done {
            return Ok(vec![]);
        }
        self.init_done = true;
        for code in &self.conv_map_source {
            self.conv_map
                .merge(&code.id_of_code, &code.code, target_pointer_width)?;
        }

        if self.conv_map.is_empty() {
            return Err(DiagnosticError::new(
                Span::call_site(),
                "After merge all \"types maps\" have no convertion code",
            ));
        }

        Generator::language_generator(&self.config)
            .init(&mut self.conv_map, &self.foreign_lang_helpers)
            .map_err(|err| {
                DiagnosticError::new(
                    Span::call_site(),
                    format!("Can not put/generate foreign lang helpers: {}", err),
                )
            })?;

        Ok(self.conv_map.take_utils_code())
    }

    fn language_generator(cfg: &LanguageConfig) -> &LanguageGenerator {
        match cfg {
            LanguageConfig::JavaConfig(ref java_cfg) => java_cfg,
            LanguageConfig::CppConfig(ref cpp_cfg) => cpp_cfg,
        }
    }
}

#[derive(Debug, Clone)]
struct ForeignerClassInfo {
    name: Ident,
    methods: Vec<ForeignerMethod>,
    self_type: Option<Type>,
    foreigner_code: String,
    /// For example if we have `fn new(x: X) -> Result<Y, Z>`, then Result<Y, Z>
    constructor_ret_type: Option<Type>,
    doc_comments: Vec<String>,
    copy_derived: bool,
}

impl ForeignerClassInfo {
    fn span(&self) -> Span {
        self.name.span()
    }
    fn self_type_as_ty(&self) -> Type {
        self.self_type
            .as_ref()
            .cloned()
            .unwrap_or_else(|| parse_quote! { () })
    }
    /// common for several language binding generator code
    fn validate_class(&self) -> Result<()> {
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
            Err(DiagnosticError::new(
                self.span(),
                format!(
                    "class {} has constructor, but no self_type defined",
                    self.name
                ),
            ))
        } else if self.self_type.is_none() && has_methods {
            Err(DiagnosticError::new(
                self.span(),
                format!("class {} has methods, but no self_type defined", self.name),
            ))
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, Clone)]
struct ForeignerMethod {
    variant: MethodVariant,
    rust_id: syn::Path,
    fn_decl: FnDecl,
    name_alias: Option<Ident>,
    access: MethodAccess,
    doc_comments: Vec<String>,
}

#[derive(Debug, Clone)]
struct FnDecl {
    span: Span,
    inputs: syn::punctuated::Punctuated<syn::FnArg, Token![,]>,
    output: syn::ReturnType,
}

impl From<syn::FnDecl> for crate::FnDecl {
    fn from(x: syn::FnDecl) -> Self {
        crate::FnDecl {
            span: x.fn_token.span(),
            inputs: x.inputs,
            output: x.output,
        }
    }
}

impl ForeignerMethod {
    fn short_name(&self) -> String {
        if let Some(ref name) = self.name_alias {
            name.to_string()
        } else {
            match self.rust_id.segments.len() {
                0 => String::new(),
                n => self.rust_id.segments[n - 1].ident.to_string(),
            }
        }
    }

    fn span(&self) -> Span {
        self.rust_id.span()
    }

    fn is_dummy_constructor(&self) -> bool {
        self.rust_id.segments.is_empty()
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
enum MethodAccess {
    Private,
    Public,
    Protected,
}
#[derive(PartialEq, Clone, Copy, Debug)]
enum MethodVariant {
    Constructor,
    Method(SelfTypeVariant),
    StaticMethod,
}

#[derive(PartialEq, Clone, Copy, Debug)]
enum SelfTypeVariant {
    RptrMut,
    Rptr,
    Mut,
    Default,
}

impl SelfTypeVariant {
    fn is_read_only(self) -> bool {
        match self {
            SelfTypeVariant::RptrMut | SelfTypeVariant::Mut => false,
            SelfTypeVariant::Default | SelfTypeVariant::Rptr => true,
        }
    }
}

#[derive(Debug, Clone)]
struct ForeignEnumInfo {
    name: Ident,
    items: Vec<ForeignEnumItem>,
    doc_comments: Vec<String>,
}

impl ForeignEnumInfo {
    fn rust_enum_name(&self) -> String {
        self.name.to_string()
    }
    fn span(&self) -> Span {
        self.name.span()
    }
}

#[derive(Debug, Clone)]
struct ForeignEnumItem {
    name: Ident,
    rust_name: syn::Path,
    doc_comments: Vec<String>,
}

struct ForeignInterface {
    name: Ident,
    self_type: syn::Path,
    doc_comments: Vec<String>,
    items: Vec<ForeignInterfaceMethod>,
}

impl ForeignInterface {
    fn span(&self) -> Span {
        self.name.span()
    }
}

struct ForeignInterfaceMethod {
    name: Ident,
    rust_name: syn::Path,
    fn_decl: FnDecl,
    doc_comments: Vec<String>,
}

trait LanguageGenerator {
    fn register_class(&self, conv_map: &mut TypeMap, class: &ForeignerClassInfo) -> Result<()>;

    fn generate(
        &self,
        conv_map: &mut TypeMap,
        pointer_target_width: usize,
        class: &ForeignerClassInfo,
    ) -> Result<Vec<TokenStream>>;

    fn generate_enum(
        &self,
        conv_map: &mut TypeMap,
        pointer_target_width: usize,
        enum_info: &ForeignEnumInfo,
    ) -> Result<Vec<TokenStream>>;

    fn generate_interface(
        &self,
        conv_map: &mut TypeMap,
        pointer_target_width: usize,
        interace: &ForeignInterface,
    ) -> Result<Vec<TokenStream>>;

    /// Called before any other methods and only once
    fn init(
        &self,
        _type_map: &mut TypeMap,
        _foreign_lang_helpers: &[SourceCode],
    ) -> std::result::Result<(), String> {
        Ok(())
    }
}
