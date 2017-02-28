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
use std::sync::atomic::AtomicBool;
use std::sync::Mutex;

use syntex::Registry;
use syntex_syntax::ext::base::{ExtCtxt, MacResult, DummyResult};
use syntex_syntax::parse::{token, parser};
use syntex_syntax::tokenstream::TokenTree;
use syntex_syntax::codemap::Span;
use syntex_syntax::{parse, ast};
use syntex_syntax::ptr::P;
use syntex_syntax::print::pprust;

use parse_utils::*;
use core::*;
use rust_generator::{generate_rust_code, RUST_OBJECT_TO_JOBJECT};
use java_generator::generate_java_code;

lazy_static! {
    pub static ref COMMON_CODE_GENERATED: AtomicBool = AtomicBool::new(false);
    static ref TYPE_HANDLERS: Mutex<Vec<TypeHandler>> = {
        Mutex::new(vec![
            TypeHandler{rust_type_name: "bool".into(), jni_type_name: "jboolean", java_type_name: "boolean".into(),
                        from_jni_converter: Some(FromForeignArgConverter("let {arg_name} = {arg_name} != 0;".into())),
                        to_jni_converter: Some(ToForeignRetConverter("let ret = if ret { 1 as jboolean } else { 0 as jboolean };".into()))},
            TypeHandler{rust_type_name: "i32".into(), jni_type_name: "jint", java_type_name: "int".into(),
                        from_jni_converter: None, to_jni_converter: None},
            TypeHandler{rust_type_name: "u64".into(), jni_type_name: "jlong", java_type_name: "long".into(),
                        from_jni_converter: None,
                        to_jni_converter: Some(ToForeignRetConverter(r#"
  let ret: i64 = if (::std::i64::MAX as u64) < ret {
                    error!("u64->jlong type overflow: {}", ret);
                    ::std::i64::MAX
                 } else { ret as i64 };
"#.into())),
            },
            TypeHandler{rust_type_name: "f32".into(), jni_type_name: "jfloat", java_type_name: "float".into(),
                        from_jni_converter: None, to_jni_converter: None},
            TypeHandler{rust_type_name: "f64".into(), jni_type_name: "jdouble", java_type_name: "double".into(),
                        from_jni_converter: None, to_jni_converter: None},
            TypeHandler{rust_type_name: "&str".into(), jni_type_name: "jstring", java_type_name: "String".into(),
                        from_jni_converter: Some(FromForeignArgConverter(r#"
  let {arg_name} = JavaString::new(env, {arg_name});
  let {arg_name} = {arg_name}.to_str();
"#.into())),
                        to_jni_converter: Some(ToForeignRetConverter(r#"
  let ret = ::std::ffi::CString::new(ret).unwrap();
  let ret = unsafe { (**env).NewStringUTF.unwrap()(env, ret.as_ptr()) };
"#.into()))},
            TypeHandler{rust_type_name: "&Path".into(), jni_type_name: "jstring", java_type_name: "String".into(),
                        from_jni_converter: Some(FromForeignArgConverter(
                            r#"
  let {arg_name} = JavaString::new(env, {arg_name});
  let {arg_name} = Path::new({arg_name}.to_str());
"#.into()
                        )),
                        to_jni_converter: None},
            TypeHandler{rust_type_name: "String".into(), jni_type_name: "jstring", java_type_name: "String".into(),
                        from_jni_converter: None,
                        to_jni_converter: Some(ToForeignRetConverter(
                            r#"
  let ret = ret.into_bytes();
  let ret = unsafe { ::std::ffi::CString::from_vec_unchecked(ret) };
  let ret = unsafe { (**env).NewStringUTF.unwrap()(env, ret.as_ptr()) };
"#.into()
                        ))},
            ])
    };
}

pub fn register(registry: &mut Registry) {
    registry.add_macro("foreigner_class", expand_foreigner_class);
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
    let mut constructor_ret_type: Option<ast::Ty> = None;
    let mut this_type_for_method: Option<ast::Ty> = None;
    let mut foreigner_code = String::new();
    loop {
        if parser.eat(&token::Token::CloseDelim(token::DelimToken::Brace)) {
            break;
        }
        let func_type_name = parser.parse_ident().unwrap();
        debug!("func_type {:?}", func_type_name);
        if func_type_name.name.as_str() == "self_type" {
            rust_self_type = parser.parse_path(parser::PathStyle::Type).expect("Can not parse self_type");
            debug!("self_type: {:?}", rust_self_type);
            parser.expect(&token::Token::Semi).unwrap();
            continue;
        }

        if func_type_name.name.as_str() == "foreigner_code" {
            let lit = parser.parse_lit().expect("expect literal after foreigner_code");
            match lit.node {
                ast::LitKind::Str(s, _) => {
                    debug!("foreigner_code s: {:?}", s);
                    foreigner_code.push_str(&s);
                }
                _ => {
                    cx.span_err(parser.span, "expect string literal after foreigner_code");
                    return DummyResult::any(parser.span);
                }
            }
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
        let (may_return_error, ret_type) = match &func_decl.output {
            &ast::FunctionRetTy::Default(_) => (false, None),
            &ast::FunctionRetTy::Ty(ref ret_type) => (is_type_std_result(ret_type), Some(ret_type.clone())),
        };
        if let FuncVariant::Constructor = func_type {
            let ret_type = ret_type.expect(&format!("{}: constructor should return value", class_name_indent));
            if let Some(ref constructor_ret_type) = constructor_ret_type {
                if pprust::ty_to_string(constructor_ret_type) != pprust::ty_to_string(&*ret_type) {
                    cx.span_err(parser.span, &format!("mismatched types of construtors: {:?} {:?}", constructor_ret_type, ret_type));
                    return DummyResult::any(parser.span);
                }
            } else {
                constructor_ret_type = Some(ret_type.unwrap());
                this_type_for_method = Some(unpack_if_type_is_result(constructor_ret_type.as_ref().unwrap()));
            }
        }
        methods.push(ForeignerMethod{
            func_type: func_type, path: func_name, in_out_type: func_decl,
            name_alias: func_name_alias.map(|v| v.name.as_str()),
            may_return_error: may_return_error,
        });
    }
    let mut type_handlers = TYPE_HANDLERS.lock().unwrap();

    let package_name = env::var("RUST_SWIG_JNI_JAVA_PACKAGE").unwrap();

    let class_info = ForeignerClassInfo {
        package_name: package_name,
        class_name: &class_name_indent.name.as_str(),
        methods: methods,
        self_rust_type: rust_self_type,
        constructor_ret_type: constructor_ret_type,
        this_type_for_method: this_type_for_method,
        foreigner_code: foreigner_code,
    };

    if class_info.this_type_for_method.is_some() {
        let mut class_th: TypeHandler = (&class_info).into();        
        class_th.to_jni_converter = Some(ToForeignRetConverter(
            RUST_OBJECT_TO_JOBJECT.replace(
                "{full_class_name}", &class_info.full_java_class_name())));
        type_handlers.push(class_th);
    }

    let mut rust_java_types_map = HashMap::new();
    for it in type_handlers.iter() {
        rust_java_types_map.insert(it.rust_type_name.as_str(), &*it);
    }

    let java_output_dir = env::var("RUST_SWIG_JNI_JAVA_OUTPUT_DIR").unwrap();
    
    generate_java_code(&rust_java_types_map, &class_info, &java_output_dir);
    generate_rust_code(cx, &rust_java_types_map, &class_info)
}

fn is_type_std_result(ty: &ast::Ty) -> bool {
    match ty.node {
        ast::TyKind::Path(_/*self info*/, ref path) => {
            debug!("is_type_std_result_result: path: {:?}, ident {:?}", path.segments, path.segments[0].identifier.name.as_str());
            path.segments.first().map(|v| v.identifier.name.as_str() == "Result").unwrap_or(false)
        }
        _ => false,
    }
}

fn unpack_if_type_is_result(ty: &ast::Ty) -> ast::Ty {
    match ty.node {
        ast::TyKind::Path(_/*self info*/, ref path) => {
            debug!("unpack_if_type_is_result: path: {:?}, ident {:?}", path.segments, path.segments[0].identifier.name.as_str());
            path.segments.first().map(|v|
                                      if v.identifier.name.as_str() == "Result" {
                                          match v.parameters {
                                              ast::PathParameters::AngleBracketed(ref params) => {
                                                  params.types.first().map(|v: &P<ast::Ty>| {
                                                      debug!("unpack_if_type_is_result: result param {:?}", *v);
                                                      (**v).clone()
                                                  }).unwrap_or(ty.clone())
                                              }
                                              _ => ty.clone(),
                                          }
                                      } else {
                                          ty.clone()
                                      }).unwrap_or(ty.clone())
        }
        _ => ty.clone(),
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use syntex_syntax::{parse, ast};

    #[test]
    fn test_is_type_std_result() {
        let session = parse::ParseSess::new();
        let mut parser = parse::new_parser_from_source_str(
            &session, ast::CrateConfig::new(), "test".into(),
            "Result<Foo, String>".into());
        let ty = parser.parse_ty().unwrap();
        assert!(is_type_std_result(&*ty));
    }

    #[test]
    fn test_unpack_if_type_is_result() {
        let session = parse::ParseSess::new();
        let mut parser = parse::new_parser_from_source_str(
            &session, ast::CrateConfig::new(), "test".into(),
            "Result<Foo, String>".into());
        let ty = parser.parse_ty().unwrap();
        assert!(is_type_std_result(&*ty));

        let ok_ty = unpack_if_type_is_result(&*ty);
        assert!(!is_type_std_result(&ok_ty));

        assert_eq!(pprust::ty_to_string(&ok_ty), "Foo");
    }
}
