//! `rust_swig` is a Rust Simplified Wrapper and Interface Generator used
//! to connect other programming languages to Rust.
//! The idea of this softwared based on [swig](http://www.swig.org).
//! For macros expansion it uses [syntex](https://crates.io/crates/syntex).
//! More details can be found at
//! [README](https://github.com/Dushistov/rust_swig/blob/master/README.md)
extern crate syntex_syntax;
extern crate syntex_pos;
extern crate syntex_errors;
extern crate syntex;
extern crate petgraph;
#[macro_use]
extern crate log;
#[cfg(test)]
extern crate env_logger;
#[macro_use]
extern crate lazy_static;


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

mod types_conv_map;
mod java_jni;
mod errors;
mod parsing;
mod my_ast;

use std::path::PathBuf;
use std::cell::RefCell;
use std::rc::Rc;

use syntex_syntax::parse::ParseSess;
use syntex_syntax::codemap::Span;
use syntex::Registry;
use syntex_syntax::tokenstream::TokenTree;
use syntex_syntax::ext::base::{DummyResult, ExtCtxt, MacEager, MacResult, TTMacroExpander};
use syntex_syntax::parse::PResult;
use syntex_syntax::ptr::P;
use syntex_syntax::ast;
use syntex_pos::DUMMY_SP;
use syntex_syntax::symbol::Symbol;
use syntex_syntax::util::small_vector::SmallVector;

use types_conv_map::TypesConvMap;
use errors::fatal_error;
use parsing::parse_foreigner_class;

/// `LanguageConfig` contains configuration for specific programming language
pub enum LanguageConfig {
    Java {
        /// directory where place generated java files
        output_dir: PathBuf,
        /// package name for generated java files
        package_name: String,
    },
}

/// `Generator` is a main point of `rust_swig`.
/// It expands rust macroses and generates not rust code.
/// It designed to use inside `build.rs`.
pub struct Generator {
    // Because of API of syntex, to register for several macroses
    data: Rc<RefCell<GeneratorData>>,
}

struct GeneratorData {
    init_done: bool,
    config: LanguageConfig,
    conv_map: TypesConvMap,
    conv_map_source: Vec<TypesConvMapCode>,
}

struct TypesConvMapCode {
    id_of_code: &'static str,
    code: &'static str,
}

#[derive(PartialEq, Clone, Copy)]
enum SelfTypeVariant {
    RptrMut,
    Rptr,
    Mut,
    Default,
}

#[derive(PartialEq, Clone, Copy)]
enum MethodVariant {
    Constructor,
    Method(SelfTypeVariant),
    StaticMethod,
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
    name: Symbol,
    methods: Vec<ForeignerMethod>,
    self_type: ast::Path,
    /// Not necessarily equal to self_type, may be for example Rc<self_type>
    this_type_for_method: Option<ast::Ty>,
    foreigner_code: String,
    /// For example if we have `fn new(x: X) -> Result<Y, Z>`, then Result<Y, Z>
    constructor_ret_type: Option<ast::Ty>,
    span: Span,
}

impl Generator {
    pub fn new(config: LanguageConfig) -> Generator {
        let mut conv_map_source = Vec::new();
        match config {
            LanguageConfig::Java { .. } => {
                conv_map_source.push(TypesConvMapCode {
                    id_of_code: "jni-include.rs",
                    code: include_str!("java_jni/jni-include.rs"),
                });
            }
        }
        Generator {
            data: Rc::new(RefCell::new(GeneratorData {
                init_done: false,
                config,
                conv_map: TypesConvMap::default(),
                conv_map_source,
            })),
        }
    }

    pub fn register(self, registry: &mut Registry) {
        registry.add_macro("foreigner_class", self);
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

impl GeneratorData {
    fn expand_foreigner_class<'a>(
        &mut self,
        cx: &'a mut ExtCtxt,
        tokens: &[TokenTree],
    ) -> Box<MacResult + 'a> {
        let mut items = unwrap_presult!(self.init_types_map(cx.parse_sess()), self.conv_map);
        let foreigner_class = match parse_foreigner_class(cx, tokens) {
            Ok(x) => x,
            Err(span) => return DummyResult::any(span),
        };
        match self.config {
            LanguageConfig::Java {
                ref output_dir,
                ref package_name,
            } => {
                let mut gen_items = unwrap_presult!(
                    java_jni::generate(
                        cx.parse_sess(),
                        &mut self.conv_map,
                        output_dir,
                        package_name,
                        &foreigner_class,
                    ),
                    self.conv_map
                );
                items.append(&mut gen_items);
                MacEager::items(SmallVector::many(items))
            }
        }
    }

    fn init_types_map<'a>(&mut self, sess: &'a ParseSess) -> PResult<'a, Vec<P<ast::Item>>> {
        if self.init_done {
            return Ok(vec![]);
        }
        self.init_done = true;
        for code in &self.conv_map_source {
            self.conv_map.merge(sess, code.id_of_code, code.code)?;
        }

        if self.conv_map.is_empty() {
            return Err(fatal_error(
                sess,
                DUMMY_SP,
                "After merge all types maps with have no convertion code",
            ));
        }

        Ok(self.conv_map.take_utils_code())
    }
}
