extern crate syntex;
extern crate syntex_syntax;
extern crate syntex_pos;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;

mod parse_utils;
mod jni;
mod core;
mod rust_generator;
mod java_generator;

use std::collections::HashMap;
use std::env;
use std::sync::atomic::{AtomicBool};

use syntex::Registry;
use syntex_syntax::ext::base::{ExtCtxt, MacResult, DummyResult};
use syntex_syntax::parse::{token, parser};
use syntex_syntax::tokenstream::TokenTree;
use syntex_syntax::codemap::Span;
use syntex_syntax::{parse, ast};
use syntex_syntax::print::pprust;

use parse_utils::*;
use core::*;
use rust_generator::generate_rust_code;
use java_generator::generate_java_code;

pub fn register(registry: &mut Registry) {
    registry.add_macro("foreigner_class", expand_foreigner_class);
}



lazy_static! {
    pub static ref COMMON_CODE_GENERATED: AtomicBool = AtomicBool::new(false);
}

fn jstring_to_str(arg_name: &str) -> String {
    format!(r#"
  let {arg_name} = JavaString::new(env, {arg_name});
  let {arg_name} = {arg_name}.to_str();
"#, arg_name = arg_name)
}

fn str_to_jstring() -> String {
    r#"
  let ret = ::std::ffi::CString::new(ret).unwrap();
  let ret = unsafe { (**env).NewStringUTF.unwrap()(env, ret.as_ptr()) };
"#.to_string()
}

fn ret_string_to_jstring() -> String {
    r#"
  let ret = ret.into_bytes();
  let ret = unsafe { ::std::ffi::CString::from_vec_unchecked(ret) };
  let ret = unsafe { (**env).NewStringUTF.unwrap()(env, ret.as_ptr()) };
"#.into()
}

fn jstring_to_path(arg_name: &str) -> String {
    format!(r#"
  let {arg_name} = JavaString::new(env, {arg_name});
  let {arg_name} = Path::new({arg_name}.to_str());
"#, arg_name = arg_name)
}

fn jboolean_to_bool(arg_name: &str) -> String {
    format!(r#"
  let {arg_name} = {arg_name} != 0;
"#, arg_name = arg_name)
}

fn bool_to_jboolean() -> String {
    "let ret = if ret { 1 as jboolean } else { 0 as jboolean };".to_string()
}

fn expand_foreigner_class<'cx>(cx: &'cx mut ExtCtxt,
                               _: Span,
                               tokens: &[TokenTree])
                               -> Box<MacResult + 'cx> {
    let mut parser = parse::new_parser_from_tts(cx.parse_sess, cx.cfg.clone(), tokens.to_vec());
    let class_ident = parser.parse_ident().unwrap();
    if class_ident.name.as_str() != "class" {
        debug!("class_indent {:?}", class_ident);
        cx.span_err(parser.span, "expect class here");
        return DummyResult::any(parser.span);
    }
    let class_name_indent = parser.parse_ident().unwrap();
    debug!("CLASS NAME {:?}", class_name_indent);
    parser.expect(&token::Token::OpenDelim(token::DelimToken::Brace)).unwrap();
    let mut methods = Vec::new();
    let mut rust_self_type = ast::Path{span: parser.span, global: false, segments: Vec::new()};
    let alias = ast::Ident::with_empty_ctxt(token::intern("alias"));
    loop {
        if parser.eat(&token::Token::CloseDelim(token::DelimToken::Brace)) {
            break;
        }
        let func_type_name = parser.parse_ident().unwrap();
        debug!("func_type {:?}", func_type_name);
        if func_type_name.name.as_str() == "self_type" {
            rust_self_type = parser.parse_path(parser::PathStyle::Type).unwrap();
            debug!("self_type: {:?}", rust_self_type);
            parser.expect(&token::Token::Semi).unwrap();
            continue;
        }
        let func_type = FuncVariant::from_str(&func_type_name.name.as_str());
        if func_type.is_none() {
            println!("unknown func type: {:?}", func_type_name);
            cx.span_err(parser.span, "expect 'constructor' or 'method' or 'static_method' here");
            return DummyResult::any(parser.span);
        }
        let func_type = func_type.unwrap();
        let func_name = parser.parse_path(parser::PathStyle::Mod).unwrap();
        debug!("func_name {:?}", func_name);

        let func_decl = match func_type {
            FuncVariant::Constructor | FuncVariant::StaticMethod => parser.parse_fn_decl(false).unwrap(),
            FuncVariant::Method => parse_fn_decl_with_self(&mut parser, |p| p.parse_arg()).unwrap(),
        };
        debug!("func_decl {:?}", func_decl);
        parser.expect(&token::Token::Semi).unwrap();
        let mut func_name_alias = None;
        if parser.eat_contextual_keyword(alias) {
            if func_type == FuncVariant::Constructor {
                cx.span_err(parser.span, "alias not supported for 'constructor'");
                return DummyResult::any(parser.span);
            }
            func_name_alias = Some(parser.parse_ident().unwrap());
            debug!("we have ALIAS `{:?}`", func_name_alias.unwrap());
            parser.expect(&token::Token::Semi).expect("no ; at the end of alias");
        }
        let may_return_error = match &func_decl.output {
            &ast::FunctionRetTy::Default(_) => false,
            &ast::FunctionRetTy::Ty(ref ret_type) => pprust::ty_to_string(&*ret_type).as_str().starts_with("Result<"),
        };
        methods.push(ForeignerMethod{
            func_type: func_type, path: func_name, in_out_type: func_decl,
            name_alias: func_name_alias.map(|v| v.name.as_str()),
            may_return_error: may_return_error,
        });
    }
    let type_handlers = vec![
        TypeHandler{rust_type_name: "bool", jni_type_name: "jboolean", java_type_name: "boolean",
                    from_jni_converter: Some(jboolean_to_bool), to_jni_converter: Some(bool_to_jboolean)},
        TypeHandler{rust_type_name: "i32", jni_type_name: "jint", java_type_name: "int",
                    from_jni_converter: None, to_jni_converter: None},
        TypeHandler{rust_type_name: "f32", jni_type_name: "jfloat", java_type_name: "float",
                    from_jni_converter: None, to_jni_converter: None},
        TypeHandler{rust_type_name: "f64", jni_type_name: "jdouble", java_type_name: "double",
                    from_jni_converter: None, to_jni_converter: None},
        TypeHandler{rust_type_name: "&str", jni_type_name: "jstring", java_type_name: "String",
                    from_jni_converter: Some(jstring_to_str), to_jni_converter: Some(str_to_jstring)},
        TypeHandler{rust_type_name: "&Path", jni_type_name: "jstring", java_type_name: "String",
                    from_jni_converter: Some(jstring_to_path), to_jni_converter: None},
        TypeHandler{rust_type_name: "String", jni_type_name: "jstring", java_type_name: "String",
                    from_jni_converter: None, to_jni_converter: Some(ret_string_to_jstring)},
    ];
    let mut rust_java_types_map = HashMap::new();
    for it in type_handlers.iter() {
        rust_java_types_map.insert(it.rust_type_name, &*it);
    }

    let java_output_dir = env::var("RUST_SWIG_JNI_JAVA_OUTPUT_DIR").unwrap();
    let package_name = env::var("RUST_SWIG_JNI_JAVA_PACKAGE").unwrap();

    generate_java_code(&rust_java_types_map, package_name.as_str(), &class_name_indent.name.as_str(), &methods,
                       &java_output_dir);

    generate_rust_code(cx, &rust_self_type, &rust_java_types_map, package_name.as_str(), &class_name_indent.name.as_str(), &methods)
}
