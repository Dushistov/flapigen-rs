//! `rust_swig` is a Rust Simplified Wrapper and Interface Generator used
//! to connect other programming languages to Rust.
//! The idea of this softwared based on [swig](http://www.swig.org).
//! For macros expansion it uses [syntex](https://crates.io/crates/syntex).
//! More details can be found at [README](https://github.com/Dushistov/rust_swig/blob/master/README.md)
extern crate syntex;
extern crate syntex_syntax;
extern crate syntex_pos;
extern crate petgraph;

#[macro_use]
extern crate log;
#[macro_use]
extern crate lazy_static;

mod parse;
mod my_ast;
mod types_map;
mod java_jni;
mod utils;

use std::path::PathBuf;
use std::cell::RefCell;

use syntex::Registry;
use syntex_syntax::codemap::Span;
use syntex_syntax::ext::base::{ExtCtxt, MacResult, DummyResult, TTMacroExpander, MacEager};
use syntex_syntax::tokenstream::TokenTree;
use syntex_syntax::ast;
use syntex_syntax::ptr::P;
use syntex_syntax::symbol::InternedString;
use syntex_syntax::util::small_vector::SmallVector;
use syntex_syntax::symbol::Symbol;
use syntex_syntax::parse::ParseSess;

use java_jni::{JniVecRetTypesFix, JniResultRetTypesFix};
use parse::parse_foreigner_class;
use types_map::ForeignTypesMap;

/// `LanguageConfig` contains configuration for specific programming language
pub enum LanguageConfig {
    Java {
        /// directory where place generated java files
        output_dir: PathBuf,
        /// package name for generated java files
        package_name: String,
    },
}

struct TypesMapCode {
    name: &'static str,
    code: &'static str,
}

pub(crate) trait TypesMapUpdater {
    fn update(&self, types_map: &mut ForeignTypesMap, class: &ForeignerClassInfo);
}

/// `Generator` is a main point of `rust_swig`.
/// It expands rust macroses and generates not rust code.
/// It designed to use inside `build.rs`.
pub struct Generator {
    config: LanguageConfig,
    types_map: RefCell<ForeignTypesMap>,
    code_of_types_map: RefCell<Option<Vec<TypesMapCode>>>,
    types_map_updaters: Vec<Box<TypesMapUpdater>>,
}

impl Generator {
    pub fn new(config: LanguageConfig) -> Generator {
        let mut code_of_types_map = Vec::new();
        let mut types_map_updaters = Vec::<Box<TypesMapUpdater>>::new();
        match config {
            LanguageConfig::Java { .. } => {
                code_of_types_map.push(TypesMapCode {
                                           name: "jni-type-map-include.rs",
                                           code: include_str!("java_jni/jni-type-map-include.rs"),
                                       });
                types_map_updaters.push(Box::new(JniVecRetTypesFix));
                types_map_updaters.push(Box::new(JniResultRetTypesFix));
            }
        }
        Generator {
            config,
            types_map: RefCell::new(ForeignTypesMap::default()),
            code_of_types_map: RefCell::new(Some(code_of_types_map)),
            types_map_updaters,
        }
    }

    pub fn register(self, registry: &mut Registry) {
        registry.add_macro("foreigner_class", self);
    }

    /// may panic
    fn init_types_map(&self, parse_session: &ParseSess) -> Vec<P<ast::Item>> {
        if let Some(code_list) = self.code_of_types_map.borrow_mut().take() {
            for code in &code_list {
                self.types_map
                    .borrow_mut()
                    .merge(parse_session, code.name, code.code)
                    .unwrap_or_else(|err| {
                                        panic!("Can not merge {} with previous types map: {}",
                                               code.name,
                                               err)
                                    });
            }
        }
        let mut types_map = self.types_map.borrow_mut();
        if types_map.is_empty() {
            panic!("After merge all types maps with have no convertion code");
        }
        let utils_code: Vec<_> = types_map.take_utils_code();
        utils_code
    }
}

#[derive(PartialEq, Clone, Copy)]
enum MethodVariant {
    Constructor,
    Method,
    StaticMethod,
}

impl MethodVariant {
    pub fn from_ident(ident: &InternedString) -> Option<MethodVariant> {
        match &**ident {
            "constructor" => Some(MethodVariant::Constructor),
            "method" => Some(MethodVariant::Method),
            "static_method" => Some(MethodVariant::StaticMethod),
            _ => None,
        }
    }
}

struct ForeignerMethod {
    variant: MethodVariant,
    rust_id: ast::Path,
    fn_decl: P<ast::FnDecl>,
    name_alias: Option<Symbol>,
    /// cache if rust_fn_decl.output == Result
    may_return_error: bool,
    foreigner_private: bool,
}

impl ForeignerMethod {
    pub(crate) fn short_name(&self) -> Symbol {
        if let Some(name) = self.name_alias {
            name
        } else {
            match self.rust_id.segments.len() {
                0 => Symbol::intern(""),
                n => self.rust_id.segments[n - 1].identifier.name,
            }
        }
    }
}

pub(crate) struct ForeignerClassInfo {
    name: InternedString,
    methods: Vec<ForeignerMethod>,
    self_type: ast::Path,
    /// Not equal self_type, may be for example Box<self_type>
    this_type_for_method: Option<ast::Ty>,
    foreigner_code: String,
    /// For example if we have `fn new(x: X) -> Result<Y, Z>`, then `Y`
    constructor_ret_type: Option<ast::Ty>,
}

impl TTMacroExpander for Generator {
    fn expand<'a>(&self,
                  cx: &'a mut ExtCtxt,
                  _: Span,
                  tokens: &[TokenTree])
                  -> Box<MacResult + 'a> {
        let mut utils_code = self.init_types_map(cx.parse_sess());
        let foreigner_class = match parse_foreigner_class(cx, tokens) {
            Ok(x) => x,
            Err(span) => return DummyResult::any(span),
        };
        for updater in &self.types_map_updaters {
            updater.update(&mut *self.types_map.borrow_mut(), &foreigner_class);
        }

        match self.config {
            LanguageConfig::Java {
                ref output_dir,
                ref package_name,
            } => {
                match java_jni::generate(cx.parse_sess(),
                                         &mut *self.types_map.borrow_mut(),
                                         output_dir,
                                         package_name,
                                         &foreigner_class) {
                    Ok(mut items) => {
                        items.extend(utils_code.drain(..));
                        MacEager::items(SmallVector::many(items))
                    }
                    Err(msg) => panic!("java/jni code generation error: {}", msg),
                }
            }
        }
    }
}
