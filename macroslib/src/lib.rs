//! `flapigen` is a Rust Simplified Wrapper and Interface Generator used
//! to connect other programming languages to Rust.
//! It is designed to be used from
//! [cargo build scripts](https://doc.rust-lang.org/cargo/reference/build-scripts.html).
//! The idea of this softwared based on [swig](http://www.swig.org).
//! More details can be found at
//! [README](https://github.com/Dushistov/flapigen-rs/blob/master/README.md)

macro_rules! parse_type {
    ($($tt:tt)*) => {{
        let ty: syn::Type = syn::parse_quote! { $($tt)* };
        ty
    }}
}

macro_rules! parse_spanned {
    ($span:ident, $($tt:tt)*) => {{
	let tt = quote::quote_spanned! { $span=> $($tt)* };
        syn::parse2(tt)
    }}
}

macro_rules! parse_type_spanned_checked {
    ($span:ident, $($tt:tt)*) => {{
	let ty: syn::Type = parse_spanned!($span, $($tt)*)
	    .unwrap_or_else(|err| {
		panic!("Can not parse type {}: {}", stringify!($($tt)*), err);
	    });
	ty
    }}
}

mod code_parse;
mod cpp;
mod error;
mod extension;
pub mod file_cache;
mod java_jni;
mod namegen;
mod python;
mod source_registry;
mod str_replace;
mod typemap;
mod types;

use std::{
    env, io,
    io::Write,
    mem,
    path::{Path, PathBuf},
    process::{Command, Stdio},
    str,
    str::FromStr,
    sync::Arc,
};

use log::debug;
use proc_macro2::TokenStream;
use strum::EnumIter;
use syn::spanned::Spanned;

use crate::{
    error::{panic_on_parse_error, DiagnosticError, Result},
    source_registry::{SourceId, SourceRegistry},
    typemap::{ast::DisplayToTokens, TypeMap},
    types::ItemToExpand,
};

pub(crate) static WRITE_TO_MEM_FAILED_MSG: &str = "Write to memory buffer failed, no free mem?";
pub(crate) static SMART_PTR_COPY_TRAIT: &str = "SmartPtrCopy";
pub(crate) static COPY_TRAIT: &str = "Copy";
pub(crate) static CLONE_TRAIT: &str = "Clone";
pub(crate) static PLAIN_CLASS: &str = "PlainClass";
pub(crate) static CAMEL_CASE_ALIASES: &str = "camelCaseAliases";
pub(crate) static KNOWN_CLASS_DERIVES: [&str; 5] = [
    CLONE_TRAIT,
    COPY_TRAIT,
    SMART_PTR_COPY_TRAIT,
    PLAIN_CLASS,
    CAMEL_CASE_ALIASES,
];

pub use extension::MethodInfo;
use extension::{ClassExtHandlers, EnumExtHandlers, ExtHandlers, MethodExtHandlers};
use rustc_hash::FxHashMap;
pub use types::MethodVariant;

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
    PythonConfig(PythonConfig),
}

/// Configuration for Java binding generation
#[derive(Debug)]
pub struct JavaConfig {
    output_dir: PathBuf,
    package_name: String,
    null_annotation_package: Option<String>,
    optional_package: String,
    reachability_fence: JavaReachabilityFence,
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
            reachability_fence: JavaReachabilityFence::GenerateFence(8),
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
    /// Choose reachability fence variant, `JavaReachabilityFence::Std` provide
    /// ability to generate better code, but not available untill Java 9 and
    /// Android API level 28
    pub fn use_reachability_fence(
        mut self,
        reachability_fence: JavaReachabilityFence,
    ) -> JavaConfig {
        self.reachability_fence = reachability_fence;
        self
    }
}

/// What reachability fence to use
#[derive(Debug, Clone, Copy)]
pub enum JavaReachabilityFence {
    /// java.lang.ref.Reference.reachabilityFence​
    Std,
    /// If Reference.reachabilityFence​ is not available,
    /// generate JNI code to emulate it
    /// Argument is maximum number of parameters, that you would
    /// "fence". By default is big enough number, but may be
    /// you lint tool will be "against" too big number (we generate Java method
    /// with such number of arguments)
    GenerateFence(usize),
}

/// Configuration for C++ binding generation
pub struct CppConfig {
    output_dir: PathBuf,
    namespace_name: String,
    cpp_optional: CppOptional,
    cpp_variant: CppVariant,
    cpp_str_view: CppStrView,
    /// Create separate *_impl.hpp files with methods implementations.
    /// Can be necessary for the project with circular dependencies between classes.
    separate_impl_headers: bool,
}

/// To which `C++` type map `std::option::Option`
#[derive(Clone, Copy, EnumIter)]
pub enum CppOptional {
    /// `std::optional` from C++17 standard
    Std17,
    /// `boost::optional`
    Boost,
}

impl From<CppOptional> for &'static str {
    fn from(x: CppOptional) -> Self {
        match x {
            CppOptional::Std17 => "CppOptional::Std17",
            CppOptional::Boost => "CppOptional::Boost",
        }
    }
}

/// To which `C++` type map `std::result::Result`
#[derive(Clone, Copy, EnumIter)]
pub enum CppVariant {
    /// `std::variant` from C++17 standard
    Std17,
    /// `boost::variant`
    Boost,
}

impl From<CppVariant> for &'static str {
    fn from(x: CppVariant) -> Self {
        match x {
            CppVariant::Std17 => "CppVariant::Std17",
            CppVariant::Boost => "CppVariant::Boost",
        }
    }
}

/// To whcih `C++` type map `&str`
#[derive(Clone, Copy, EnumIter)]
pub enum CppStrView {
    /// `std::string_view` from C++17 standard
    Std17,
    /// `boost::string_view`
    Boost,
}

impl From<CppStrView> for &'static str {
    fn from(x: CppStrView) -> Self {
        match x {
            CppStrView::Std17 => "CppStrView::Std17",
            CppStrView::Boost => "CppStrView::Boost",
        }
    }
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
            cpp_str_view: CppStrView::Std17,
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
    pub fn cpp_str_view(self, cpp_str_view: CppStrView) -> CppConfig {
        CppConfig {
            cpp_str_view,
            ..self
        }
    }
    /// Use boost for that fit: Result -> boost::variant,
    /// Option -> boost::optional, &str -> boost::string_view
    pub fn use_boost(self) -> CppConfig {
        CppConfig {
            cpp_variant: CppVariant::Boost,
            cpp_optional: CppOptional::Boost,
            cpp_str_view: CppStrView::Boost,
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

/// Configuration for Python binding generation
pub struct PythonConfig {
    module_name: String,
}

impl PythonConfig {
    /// Create `PythonConfig`
    /// # Arguments
    pub fn new(module_name: String) -> PythonConfig {
        PythonConfig { module_name }
    }
}

/// `Generator` is a main point of `flapigen`.
/// It expands rust macroses and generates not rust code.
/// It designed to use inside `build.rs`.
pub struct Generator {
    init_done: bool,
    config: LanguageConfig,
    conv_map: TypeMap,
    conv_map_source: Vec<SourceId>,
    foreign_lang_helpers: Vec<SourceCode>,
    pointer_target_width: usize,
    src_reg: SourceRegistry,
    rustfmt_bindings: bool,
    remove_not_generated_files: bool,
    class_ext_handlers: ClassExtHandlers,
    method_ext_handlers: MethodExtHandlers,
    enum_ext_handlers: EnumExtHandlers,
}

struct SourceCode {
    id_of_code: String,
    code: String,
}

static FOREIGNER_CLASS_DEPRECATED: &str = "foreigner_class";
static FOREIGN_CLASS: &str = "foreign_class";
static FOREIGN_ENUM: &str = "foreign_enum";
static FOREIGN_INTERFACE_DEPRECATED: &str = "foreign_interface";
static FOREIGN_CALLBACK: &str = "foreign_callback";
static FOREIGNER_CODE_DEPRECATED: &str = "foreigner_code";
static FOREIGN_CODE: &str = "foreign_code";
static FOREIGN_TYPEMAP: &str = "foreign_typemap";

impl Generator {
    pub fn new(config: LanguageConfig) -> Generator {
        let pointer_target_width = target_pointer_width_from_env();
        let mut conv_map_source = Vec::new();
        let mut foreign_lang_helpers = Vec::new();
        let mut src_reg = SourceRegistry::default();
        match config {
            LanguageConfig::JavaConfig(ref java_cfg) => {
                conv_map_source.push(
                    src_reg.register(SourceCode {
                        id_of_code: "jni-include.rs".into(),
                        code: include_str!("java_jni/jni-include.rs")
                            .replace(
                                "java.util.Optional",
                                &format!("{}.Optional", java_cfg.optional_package),
                            )
                            .replace(
                                "java/util/Optional",
                                &format!(
                                    "{}/Optional",
                                    java_cfg.optional_package.replace('.', "/")
                                ),
                            ),
                    }),
                );
            }
            LanguageConfig::CppConfig(..) => {
                conv_map_source.push(src_reg.register(SourceCode {
                    id_of_code: "cpp-include.rs".into(),
                    code: include_str!("cpp/cpp-include.rs").into(),
                }));
                foreign_lang_helpers.push(SourceCode {
                    id_of_code: "rust_vec_impl.hpp".into(),
                    code: include_str!("cpp/rust_vec_impl.hpp").into(),
                });
                foreign_lang_helpers.push(SourceCode {
                    id_of_code: "rust_foreign_vec_impl.hpp".into(),
                    code: include_str!("cpp/rust_foreign_vec_impl.hpp").into(),
                });
                foreign_lang_helpers.push(SourceCode {
                    id_of_code: "rust_foreign_slice_iter.hpp".into(),
                    code: include_str!("cpp/rust_foreign_slice_iter.hpp").into(),
                });
                foreign_lang_helpers.push(SourceCode {
                    id_of_code: "rust_foreign_slice_impl.hpp".into(),
                    code: include_str!("cpp/rust_foreign_slice_impl.hpp").into(),
                });
                foreign_lang_helpers.push(SourceCode {
                    id_of_code: "rust_slice_tmpl.hpp".into(),
                    code: include_str!("cpp/rust_slice_tmpl.hpp").into(),
                });
                foreign_lang_helpers.push(SourceCode {
                    id_of_code: "rust_slice_access.hpp".into(),
                    code: include_str!("cpp/rust_slice_access.hpp").into(),
                });
                foreign_lang_helpers.push(SourceCode {
                    id_of_code: "rust_vec_access.hpp".into(),
                    code: include_str!("cpp/rust_vec_access.hpp").into(),
                });
            }
            LanguageConfig::PythonConfig(..) => {
                conv_map_source.push(src_reg.register(SourceCode {
                    id_of_code: "python-include.rs".into(),
                    code: include_str!("python/python-include.rs").into(),
                }));
            }
        }
        Generator {
            init_done: false,
            config,
            conv_map: TypeMap::default(),
            conv_map_source,
            foreign_lang_helpers,
            pointer_target_width: pointer_target_width.unwrap_or(0),
            src_reg,
            rustfmt_bindings: false,
            remove_not_generated_files: false,
            class_ext_handlers: FxHashMap::default(),
            method_ext_handlers: FxHashMap::default(),
            enum_ext_handlers: FxHashMap::default(),
        }
    }

    /// By default we get pointer_target_width via cargo (more exactly CARGO_CFG_TARGET_POINTER_WIDTH),
    /// but you can change default value via this method
    pub fn with_pointer_target_width(mut self, pointer_target_width: usize) -> Self {
        self.pointer_target_width = pointer_target_width;
        self
    }

    /// Set whether rustfmt should format the generated bindings.
    pub fn rustfmt_bindings(mut self, doit: bool) -> Self {
        self.rustfmt_bindings = doit;
        self
    }

    /// If true removes not generated by flapigen files from output directory,
    /// suitable for removing obsolete files.
    /// Warning! May remove your files if turn on, so by default false
    pub fn remove_not_generated_files_from_output_directory(mut self, doit: bool) -> Self {
        self.remove_not_generated_files = doit;
        self
    }

    /// Add new foreign langauge type <-> Rust mapping
    pub fn merge_type_map(mut self, id_of_code: &str, code: &str) -> Self {
        self.conv_map_source.push(self.src_reg.register(SourceCode {
            id_of_code: id_of_code.into(),
            code: code.into(),
        }));
        self
    }

    /// Register callback to extend/modify class, if `foreign_class` has #[derive(attr_name)]
    /// then after foreign code generation `cb` would be called, with full code of module,
    /// plus class name
    pub fn register_class_attribute_callback<F>(mut self, attr_name: &str, cb: F) -> Self
    where
        F: Fn(&mut Vec<u8>, &str) + 'static,
    {
        if KNOWN_CLASS_DERIVES.iter().any(|x| *x == attr_name) {
            panic!("This '{}' attribute name is reserved", attr_name);
        }
        if self.class_ext_handlers.contains_key(attr_name) {
            panic!(
                "class attribute callback for name '{}' already registered",
                attr_name
            );
        }
        self.class_ext_handlers
            .insert(attr_name.into(), Box::new(cb));
        self
    }

    /// Register callback to extend/modify enum, if `foreign_enum` has #[derive(attr_name)]
    /// then after foreign code generation `cb` would be called, with full code of module,
    /// plus enum name
    pub fn register_enum_attribute_callback<F>(mut self, attr_name: &str, cb: F) -> Self
    where
        F: Fn(&mut Vec<u8>, &str) + 'static,
    {
        if self.enum_ext_handlers.contains_key(attr_name) {
            panic!(
                "enum attribute callback for name '{}' already registered",
                attr_name
            );
        }
        self.enum_ext_handlers
            .insert(attr_name.into(), Box::new(cb));
        self
    }

    /// Register callback to extend/modify method of class, if `fn` inside `foreign_class`
    /// has attribute, then after foreign code generation `cb` would be called with full code
    /// of moulde, plus class name, plus method name
    pub fn register_method_attribute_callback<F>(mut self, attr_name: &str, cb: F) -> Self
    where
        F: Fn(&mut Vec<u8>, MethodInfo) + 'static,
    {
        if self.method_ext_handlers.contains_key(attr_name) {
            panic!(
                "method attribute callback for name '{}' already registered",
                attr_name
            );
        }
        self.method_ext_handlers
            .insert(attr_name.into(), Box::new(cb));
        self
    }

    /// process `src` and save result of macro expansion to `dst`
    ///
    /// # Panics
    /// Panics on error
    pub fn expand<S, D>(mut self, crate_name: &str, src: S, dst: D)
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

        let src_id = self.src_reg.register(SourceCode {
            id_of_code: format!("{}: {}", crate_name, src.as_ref().display()),
            code: src_cnt,
        });

        if let Err(err) = self.expand_str(&[src_id], dst) {
            panic_on_parse_error(&self.src_reg, &err);
        }
    }

    /// process `srcs` and save result of macro expansion to `dst`
    ///
    /// # Panics
    /// Panics on error
    pub fn expand_many<S, D>(mut self, crate_name: &str, srcs: &[S], dst: D)
    where
        S: AsRef<Path>,
        D: AsRef<Path>,
    {
        let mut src_ids = Vec::with_capacity(srcs.len());
        for src in srcs {
            let src_cnt = std::fs::read_to_string(src.as_ref()).unwrap_or_else(|err| {
                panic!(
                    "Error during read for file {}: {}",
                    src.as_ref().display(),
                    err
                )
            });

            let src_id = self.src_reg.register(SourceCode {
                id_of_code: format!("{}: {}", crate_name, src.as_ref().display()),
                code: src_cnt,
            });
            src_ids.push(src_id);
        }

        if let Err(err) = self.expand_str(&src_ids, dst) {
            panic_on_parse_error(&self.src_reg, &err);
        }
    }

    /// process `src` and save result of macro expansion to `dst`
    ///
    /// # Panics
    /// Panics on I/O errors
    fn expand_str<D>(&mut self, src_ids: &[SourceId], dst: D) -> Result<()>
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

        let mut file =
            file_cache::FileWriteCache::new(dst.as_ref(), &mut file_cache::NoNeedFsOpsRegistration);

        for item in items {
            write!(&mut file, "{}", DisplayToTokens(&item)).expect(WRITE_TO_MEM_FAILED_MSG);
        }

        let mut items_to_expand = Vec::with_capacity(1000);

        for src_id in src_ids {
            let syn_file = syn::parse_file(self.src_reg.src(*src_id))
                .map_err(|err| DiagnosticError::from_syn_err(*src_id, err))?;

            for item in syn_file.items {
                if let syn::Item::Macro(mut item_macro) = item {
                    let is_our_macro = [
                        FOREIGNER_CLASS_DEPRECATED,
                        FOREIGN_CLASS,
                        FOREIGN_ENUM,
                        FOREIGN_INTERFACE_DEPRECATED,
                        FOREIGN_CALLBACK,
                        FOREIGN_TYPEMAP,
                    ]
                    .iter()
                    .any(|x| item_macro.mac.path.is_ident(x));
                    if !is_our_macro {
                        writeln!(&mut file, "{}", DisplayToTokens(&item_macro))
                            .expect("mem I/O failed");
                        continue;
                    }
                    debug!("Found {}", DisplayToTokens(&item_macro.mac.path));
                    if item_macro.mac.tokens.is_empty() {
                        return Err(DiagnosticError::new(
                            *src_id,
                            item_macro.span(),
                            format!(
                                "missing tokens in call of macro '{}'",
                                DisplayToTokens(&item_macro.mac.path)
                            ),
                        ));
                    }
                    let mut tts = TokenStream::new();
                    mem::swap(&mut tts, &mut item_macro.mac.tokens);
                    if item_macro.mac.path.is_ident(FOREIGNER_CLASS_DEPRECATED)
                        || item_macro.mac.path.is_ident(FOREIGN_CLASS)
                    {
                        if item_macro.mac.path.is_ident(FOREIGNER_CLASS_DEPRECATED) {
                            println!(
                                "cargo:warning={} is deprecated, use {} instead",
                                FOREIGNER_CLASS_DEPRECATED, FOREIGN_CLASS
                            );
                        }
                        let fclass = code_parse::parse_foreigner_class(*src_id, &self.config, tts)?;
                        debug!("expand_foreigner_class: self_desc {:?}", fclass.self_desc);
                        self.conv_map.register_foreigner_class(&fclass);
                        items_to_expand.push(ItemToExpand::Class(Box::new(fclass)));
                    } else if item_macro.mac.path.is_ident(FOREIGN_ENUM) {
                        let fenum = code_parse::parse_foreign_enum(*src_id, tts)?;
                        items_to_expand.push(ItemToExpand::Enum(fenum));
                    } else if item_macro.mac.path.is_ident(FOREIGN_INTERFACE_DEPRECATED)
                        || item_macro.mac.path.is_ident(FOREIGN_CALLBACK)
                    {
                        if item_macro.mac.path.is_ident(FOREIGN_INTERFACE_DEPRECATED) {
                            println!(
                                "cargo:warning={} is deprecated, use {} instead",
                                FOREIGN_INTERFACE_DEPRECATED, FOREIGN_CALLBACK
                            );
                        }
                        let finterface = code_parse::parse_foreign_interface(*src_id, tts)?;
                        items_to_expand.push(ItemToExpand::Interface(finterface));
                    } else if item_macro.mac.path.is_ident(FOREIGN_TYPEMAP) {
                        self.conv_map.parse_foreign_typemap_macro(*src_id, tts)?;
                    } else {
                        unreachable!();
                    }
                } else {
                    writeln!(&mut file, "{}", DisplayToTokens(&item)).expect("mem I/O failed");
                }
            }
        }
        let generator = Generator::language_generator(&self.config);
        let code = generator.expand_items(
            &mut self.conv_map,
            self.pointer_target_width,
            &self.foreign_lang_helpers,
            items_to_expand,
            self.remove_not_generated_files,
            ExtHandlers {
                class_ext_handlers: &self.class_ext_handlers,
                method_ext_handlers: &self.method_ext_handlers,
                enum_ext_handlers: &self.enum_ext_handlers,
            },
        )?;
        for elem in code {
            writeln!(&mut file, "{}", elem).expect(WRITE_TO_MEM_FAILED_MSG);
        }

        let source_bytes = file.take_content();
        let source_bytes = generator.post_proccess_code(
            &mut self.conv_map,
            self.pointer_target_width,
            source_bytes,
        )?;
        file.replace_content(source_bytes);

        if self.rustfmt_bindings {
            let source_bytes = file.take_content();
            let new_cnt =
                rustfmt_cnt(source_bytes, RustEdition::Edition2018).unwrap_or_else(|err| {
                    panic!("Error during running of rustfmt: {}", err);
                });
            file.replace_content(new_cnt);
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
        for code_id in &self.conv_map_source {
            let code = self.src_reg.src(*code_id);
            self.conv_map.merge(*code_id, code, target_pointer_width)?;
        }

        if self.conv_map.is_empty() {
            return Err(DiagnosticError::new_without_src_info(
                "After merge all \"types maps\" have no conversion code",
            ));
        }

        Ok(self.conv_map.take_utils_code())
    }

    fn language_generator(cfg: &LanguageConfig) -> &dyn LanguageGenerator {
        match cfg {
            LanguageConfig::JavaConfig(ref java_cfg) => java_cfg,
            LanguageConfig::CppConfig(ref cpp_cfg) => cpp_cfg,
            LanguageConfig::PythonConfig(ref python_cfg) => python_cfg,
        }
    }
}

trait LanguageGenerator {
    fn expand_items(
        &self,
        conv_map: &mut TypeMap,
        pointer_target_width: usize,
        code: &[SourceCode],
        items: Vec<ItemToExpand>,
        remove_not_generated_files: bool,
        ext_handlers: ExtHandlers,
    ) -> Result<Vec<TokenStream>>;

    fn post_proccess_code(
        &self,
        _conv_map: &mut TypeMap,
        _pointer_target_width: usize,
        generated_code: Vec<u8>,
    ) -> Result<Vec<u8>> {
        Ok(generated_code)
    }
}

#[doc(hidden)]
#[derive(Clone, Copy, PartialEq)]
pub enum RustEdition {
    Edition2015,
    Edition2018,
}

#[doc(hidden)]
pub fn rustfmt_cnt(source: Vec<u8>, edition: RustEdition) -> io::Result<Vec<u8>> {
    let rustfmt = which::which("rustfmt")
        .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("{}", e)))?;

    let mut cmd = Command::new(&*rustfmt);
    cmd.arg("--edition");
    match edition {
        RustEdition::Edition2015 => {
            cmd.arg("2015");
        }
        RustEdition::Edition2018 => {
            cmd.arg("2018");
        }
    }

    cmd.stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null());

    let mut child = cmd.spawn()?;
    let mut child_stdin = child.stdin.take().unwrap();
    let mut child_stdout = child.stdout.take().unwrap();
    let src_len = source.len();
    let src = Arc::new(source);
    // Write to stdin in a new thread, so that we can read from stdout on this
    // thread. This keeps the child from blocking on writing to its stdout which
    // might block us from writing to its stdin.
    let stdin_handle = ::std::thread::spawn(move || {
        let _ = child_stdin.write_all(src.as_slice());
        src
    });

    let mut output = Vec::with_capacity(src_len);
    io::copy(&mut child_stdout, &mut output)?;
    let status = child.wait()?;
    let src = stdin_handle.join().expect(
        "The thread writing to rustfmt's stdin doesn't do \
         anything that could panic",
    );
    let src =
        Arc::try_unwrap(src).expect("Internal error: rusftfmt_cnt should only one Arc refernce");
    match status.code() {
        Some(0) => Ok(output),
        Some(2) => Err(io::Error::new(
            io::ErrorKind::Other,
            "Rustfmt parsing errors.".to_string(),
        )),
        Some(3) => {
            println!("cargo:warning=Rustfmt could not format some lines.");
            Ok(src)
        }
        _ => {
            println!("cargo:warning=Internal rustfmt error");
            Ok(src)
        }
    }
}
