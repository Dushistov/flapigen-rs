extern crate syntex;
extern crate syntex_syntax;
extern crate syntex_pos;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;
#[macro_use]
extern crate derive_builder;

mod parse_utils;
mod jni;
mod core;
mod rust_generator;
mod java_generator;

use std::path::PathBuf;
use std::collections::HashMap;
use std::sync::atomic::AtomicBool;
use std::sync::Mutex;

use syntex::Registry;
use syntex_syntax::ext::base::{ExtCtxt, MacResult, DummyResult};
use syntex_syntax::parse::{token, parser};
use syntex_syntax::tokenstream::TokenTree;
use syntex_syntax::codemap::Span;
use syntex_syntax::{parse, ast};
use syntex_syntax::print::pprust;
use syntex_syntax::ptr;
use syntex_syntax::ext::base::TTMacroExpander;

use parse_utils::*;
use core::*;
use rust_generator::{generate_rust_code, RUST_OBJECT_TO_JOBJECT, RUST_VEC_TO_JAVA_ARRAY,
                     RUST_RESULT_TO_JAVA_OBJECT};
use java_generator::generate_java_code;

lazy_static! {
    pub static ref COMMON_CODE_GENERATED: AtomicBool = AtomicBool::new(false);
    static ref TYPE_HANDLERS: Mutex<Vec<TypeHandler>> = {
        Mutex::new(vec![
            TypeHandler {
                rust_type_name: "()".into(), jni_type_name: "", java_type_name: "void".into(),
                from_jni_converter: None, to_jni_converter: None,
            },
            TypeHandler{rust_type_name: "bool".into(), jni_type_name: "jboolean", java_type_name: "boolean".into(),
                        from_jni_converter: Some(FromForeignArgConverter("let {arg_name} = {arg_name} != 0;".into())),
                        to_jni_converter: Some(ToForeignRetConverter("let ret = if ret { 1 as jboolean } else { 0 as jboolean };".into()))},
            TypeHandler {
                rust_type_name: "u8".into(), jni_type_name: "jshort", java_type_name: "short/*should be from 0 to 2^8-1*/".into(),
                from_jni_converter: Some(FromForeignArgConverter(r#"
   if {arg_name} < 0 || {arg_name} > (::std::u8::MAX as jshort) {
       panic!("Expect {arg_name} from 0 to {}, got {}", ::std::u8::MAX, {arg_name});
   }
   let {arg_name} = {arg_name} as u8;
"#.into())),
                to_jni_converter: Some(ToForeignRetConverter("let ret = ret as jshort;".into())),
            },
            TypeHandler {
                rust_type_name: "i8".into(), jni_type_name: "jbyte", java_type_name: "byte".into(),
                from_jni_converter: None, to_jni_converter: None,
            },
            TypeHandler {
                rust_type_name: "u16".into(), jni_type_name: "jint", java_type_name: "int/*should be from 0 to 2^16-1*/".into(),
                from_jni_converter: Some(FromForeignArgConverter(r#"
   if {arg_name} < 0 || {arg_name} > (::std::u16::MAX as jint) {
       panic!("Expect {arg_name} from 0 to {}, got {}", ::std::u16::MAX, {arg_name});
   }
   let {arg_name} = {arg_name} as u16;
"#.into())),
                to_jni_converter: Some(ToForeignRetConverter(r#"
  let ret = ret as jint;
"#.into())),
            },
            TypeHandler {
                rust_type_name: "i16".into(), jni_type_name: "jshort", java_type_name: "short".into(),
                from_jni_converter: None, to_jni_converter: None,
            },
            TypeHandler{rust_type_name: "i32".into(), jni_type_name: "jint", java_type_name: "int".into(),
                        from_jni_converter: None, to_jni_converter: None},
            TypeHandler {
                rust_type_name: "u32".into(), jni_type_name: "jlong", java_type_name: "long/*should be from 0 to 2^32-1*/".into(),
                from_jni_converter: Some(FromForeignArgConverter(r#"
   if {arg_name} < 0 || {arg_name} > (::std::u32::MAX as jlong) {
       panic!("Expect {arg_name} from 0 to {}, got {}", ::std::u32::MAX, {arg_name});
   }
   let {arg_name} = {arg_name} as u32;
"#.into())),
                to_jni_converter: Some(ToForeignRetConverter(r#"
  let ret = ret as jlong;
"#.into())),
               },
                TypeHandler {
                    rust_type_name: "u64".into(), jni_type_name: "jlong", java_type_name: "long/*should be >= 0*/".into(),
                    from_jni_converter: Some(FromForeignArgConverter(r#"
   if {arg_name} < 0 {
       panic!("Expect {arg_name} to be positive, got {}", {arg_name});
   }
   let {arg_name} = {arg_name} as u64;
"#.into())),
                    to_jni_converter: Some(ToForeignRetConverter(r#"
  let ret: i64 = if (::std::i64::MAX as u64) < ret {
                    error!("u64->jlong type overflow: {}", ret);
                    ::std::i64::MAX
                 } else { ret as i64 };
"#.into())),
            },
            TypeHandler {
                rust_type_name: "i64".into(), jni_type_name: "jlong", java_type_name: "long".into(),
                from_jni_converter: None, to_jni_converter: None,
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
                        to_jni_converter: Some(ToForeignRetConverter(r#"
  let ret = ret.into_bytes();
  let ret = unsafe { ::std::ffi::CString::from_vec_unchecked(ret) };
  let ret = unsafe { (**env).NewStringUTF.unwrap()(env, ret.as_ptr()) };
"#.into()
                        ))},
            TypeHandler {
                rust_type_name: "SystemTime".into(), jni_type_name: "jobject", java_type_name: "java.util.Date".into(),
                from_jni_converter: None,
                to_jni_converter: Some(ToForeignRetConverter(r#"
  let since_unix_epoch = ret.duration_since(::std::time::UNIX_EPOCH).unwrap();
  let mills: jlong = (since_unix_epoch.as_secs() * 1_000 + (since_unix_epoch.subsec_nanos() / 1_000_000) as u64) as jlong;
  let class_name_c = ::std::ffi::CString::new("java/util/Date").unwrap();
  let date_class: jclass = unsafe { (**env).FindClass.unwrap()(env, class_name_c.as_ptr()) };
  assert!(!date_class.is_null());
  let init_name_c = ::std::ffi::CString::new("<init>").unwrap();
  let method_args_c = ::std::ffi::CString::new("(J)V").unwrap();
  let init: jmethodID = unsafe { (**env).GetMethodID.unwrap()(env, date_class, init_name_c.as_ptr(), method_args_c.as_ptr()) };
  assert!(!init.is_null());
  let ret = unsafe { (**env).NewObject.unwrap()(env, date_class, init, mills) };
  assert!(!ret.is_null());
"#.into())),
            },
            ])
    };
}

#[derive(Default, Builder)]
pub struct Generator {
    java_output_dir: PathBuf,
    java_package_name: String,
}

impl Generator {
    pub fn register(self, registry: &mut Registry) {
        registry.add_macro("foreigner_class", self);
    }
}

impl TTMacroExpander for Generator {
    fn expand<'a>(&self,
                   cx: &'a mut ExtCtxt,
                   _: Span,
                   tokens: &[TokenTree])
                   -> Box<MacResult+'a> {
        let mut parser = parse::new_parser_from_tts(cx.parse_sess, tokens.to_vec());
        let class_ident = parser.parse_ident().unwrap();
        if &*class_ident.name.as_str() != "class" {
            debug!("class_indent {:?}", class_ident);
            cx.span_err(parser.span, "expect class here");
            return DummyResult::any(parser.span);
        }
        let class_name_indent = parser.parse_ident().unwrap();
        debug!("CLASS NAME {:?}", class_name_indent);
        parser.expect(&token::Token::OpenDelim(token::DelimToken::Brace)).unwrap();
        let mut methods = Vec::new();
        let mut rust_self_type = ast::Path {
            span: parser.span,
            segments: Vec::new(),
        };
        let alias_keyword = ast::Ident::from_str("alias");
        let private_keyword = ast::Ident::from_str("private");
        let mut constructor_ret_type: Option<ast::Ty> = None;
        let mut this_type_for_method: Option<ast::Ty> = None;
        let mut foreigner_code = String::new();
        loop {
            if parser.eat(&token::Token::CloseDelim(token::DelimToken::Brace)) {
                break;
            }
            let private_func = parser.eat_contextual_keyword(private_keyword);
            let func_type_name = parser.parse_ident().unwrap();
            debug!("func_type {:?}", func_type_name);
            if &*func_type_name.name.as_str() == "self_type" {
                rust_self_type =
                    parser.parse_path(parser::PathStyle::Type).expect("Can not parse self_type");
                debug!("self_type: {:?}", rust_self_type);
                parser.expect(&token::Token::Semi).unwrap();
                continue;
            }

            if &*func_type_name.name.as_str() == "foreigner_code" {
                let lit = parser.parse_lit().expect("expect literal after foreigner_code");
                match lit.node {
                    ast::LitKind::Str(s, _) => {
                        debug!("foreigner_code s: {:?}", s);
                        foreigner_code.push_str(&*s.as_str());
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
                cx.span_err(parser.span,
                            "expect 'constructor' or 'method' or 'static_method' here");
                return DummyResult::any(parser.span);
            }
            let func_type = func_type.unwrap();
            let func_name = parser.parse_path(parser::PathStyle::Mod).unwrap();
            debug!("func_name {:?}", func_name);

            let func_decl = match func_type {
                FuncVariant::Constructor |
                FuncVariant::StaticMethod => parser.parse_fn_decl(false).unwrap(),
                FuncVariant::Method => parse_fn_decl_with_self(&mut parser, |p| p.parse_arg()).unwrap(),
            };
            debug!("func_decl {:?}", func_decl);
            parser.expect(&token::Token::Semi).unwrap();
            let mut func_name_alias = None;
            if parser.eat_contextual_keyword(alias_keyword) {
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
                &ast::FunctionRetTy::Ty(ref ret_type) => {
                    (is_type_name(ret_type, "Result"), Some(ret_type.clone()))
                }
            };
            if let FuncVariant::Constructor = func_type {
                let ret_type = ret_type.expect(&format!("{}: constructor should return value",
                                                        class_name_indent));
                if let Some(ref constructor_ret_type) = constructor_ret_type {
                    if pprust::ty_to_string(constructor_ret_type) != pprust::ty_to_string(&*ret_type) {
                        cx.span_err(parser.span,
                                    &format!("mismatched types of construtors: {:?} {:?}",
                                             constructor_ret_type,
                                             ret_type));
                        return DummyResult::any(parser.span);
                    }
                } else {
                    constructor_ret_type = Some(ret_type.unwrap());
                    this_type_for_method =
                        Some(unpack_generic_first_paramter(constructor_ret_type.as_ref().unwrap(),
                                                           "Result"));
                }
            }
            methods.push(ForeignerMethod {
                func_type: func_type,
                path: func_name,
                in_out_type: func_decl,
                name_alias: func_name_alias.map(|v| v.name.as_str()),
                may_return_error: may_return_error,
                private: private_func,
            });
        }
        let mut type_handlers = TYPE_HANDLERS.lock().unwrap();

        let class_info = ForeignerClassInfo {
            package_name: self.java_package_name.clone(),
            class_name: &class_name_indent.name.as_str(),
            methods: methods,
            self_rust_type: rust_self_type,
            constructor_ret_type: constructor_ret_type,
            this_type_for_method: this_type_for_method,
            foreigner_code: foreigner_code,
        };

        if class_info.this_type_for_method.is_some() {
            let mut class_th: TypeHandler = (&class_info).into();
            let to_foreign = RUST_OBJECT_TO_JOBJECT.replace("{full_class_name}",
                                                            &class_info.full_java_class_name())
                .replace("{rust_type_name}", &class_th.rust_type_name);
            class_th.to_jni_converter = Some(ToForeignRetConverter(to_foreign.clone()));
            type_handlers.push(class_th);

            let mut class_th: TypeHandler = (&class_info).into();
            class_th.rust_type_name = format!("&{}", class_th.rust_type_name);
            let to_foreign = format!("let ret = ret.clone();\n{}", to_foreign);
            class_th.to_jni_converter = Some(ToForeignRetConverter(to_foreign));
            type_handlers.push(class_th);
        }

        generate_type_info_for_generics(&mut type_handlers, &class_info, &class_info.package_name);

        let mut rust_java_types_map = HashMap::new();
        for it in type_handlers.iter() {
            rust_java_types_map.insert(it.rust_type_name.as_str(), &*it);
        }

        generate_java_code(&rust_java_types_map, &class_info, &self.java_output_dir);
        generate_rust_code(cx, &rust_java_types_map, &class_info)
    }
}

fn is_type_name(ty: &ast::Ty, type_name: &str) -> bool {
    match ty.node {
        ast::TyKind::Path(_ /*self info*/, ref path) => {
            debug!("is_type_name_result: path: {:?}, ident {:?}",
                   path.segments,
                   path.segments[0].identifier.name.as_str());
            path.segments
                .first()
                .map(|v| &*v.identifier.name.as_str() == type_name)
                .unwrap_or(false)
        }
        _ => false,
    }
}

fn unpack_generic_first_paramter(ty: &ast::Ty, generic_name: &str) -> ast::Ty {
    match ty.node {
        ast::TyKind::Path(_ /*self info*/, ref path) => {
            debug!("unpack_generic_first_paramter: path: {:?}, ident {:?}",
                   path.segments,
                   path.segments[0].identifier.name.as_str());
            path.segments
                .first()
                .map(|ps: &ast::PathSegment| if &*ps.identifier.name.as_str() == generic_name {
                         ps.parameters
                             .as_ref()
                             .map(|p: &ptr::P<ast::PathParameters>| {
                        if let ast::PathParameters::AngleBracketed(ref params) = **p {
                            params.types
                                .first()
                                .map(|v: &ptr::P<ast::Ty>| {
                                         debug!("unpack_generic_first_paramter: result param {:?}",
                                                *v);
                                         (**v).clone()
                                     })
                                .unwrap_or(ty.clone())
                        } else {
                            ty.clone()
                        }
                    })
                             .unwrap_or(ty.clone())
                     } else {
                         ty.clone()
                     })
                .unwrap_or(ty.clone())
        }
        _ => ty.clone(),
    }
}

fn in_type_info(type_handlers: &Vec<TypeHandler>, ty: &ast::Ty, generic_name: &str) -> usize {
    let in_type = unpack_generic_first_paramter(ty, generic_name);
    let in_type_name = pprust::ty_to_string(&in_type);
    let index = type_handlers.iter()
        .position(|ref r| r.rust_type_name == in_type_name)
        .expect(&format!("Type {} not found", in_type_name));
    index
}

fn get_default_value_for_rust_type(rust_type_name: &str) -> &'static str {
    match rust_type_name {
        "()" => "()",
        "i8" | "u8" | "u16" | "i16" | "u32" | "i32" | "u64" | "i64" => "0",
        "f32" => "::std::f32::NAN",
        "f64" => "::std::f64::NAN",
        _ => "::std::ptr::null_mut()",
    }
}

fn generate_type_info_for_type(type_handlers: &mut Vec<TypeHandler>,
                               ty: &ast::Ty,
                               package_name: &str) {
    let rust_type_name = pprust::ty_to_string(ty);
    if type_handlers.iter().position(|ref r| r.rust_type_name == rust_type_name).is_some() {
        return;
    }

    if is_type_name(ty, "Vec") {
        let elem_type_index = in_type_info(type_handlers, ty, "Vec");
        let java_type_name = format!("{} []", type_handlers[elem_type_index].java_type_name);
        let elem_java_type_name = type_handlers[elem_type_index].java_type_name.clone();
        type_handlers.push(TypeHandler {
            rust_type_name: rust_type_name,
            jni_type_name: "jobjectArray",
            java_type_name: java_type_name,
            from_jni_converter: None,
            to_jni_converter: Some(ToForeignRetConverter(
                RUST_VEC_TO_JAVA_ARRAY.to_string()
                    .replace("{full_class_name}", &full_java_class_name(package_name, &elem_java_type_name))
                    .replace("{vec_name}", "ret"))),
        });
    } else if is_type_name(ty, "Result") {
        let in_type_index = in_type_info(type_handlers, ty, "Result");
        let in_th = type_handlers[in_type_index].clone();
        type_handlers.push(TypeHandler {
            rust_type_name: rust_type_name,
            jni_type_name: in_th.jni_type_name,
            java_type_name: in_th.java_type_name,
            from_jni_converter: None,
            to_jni_converter: Some(ToForeignRetConverter(
                RUST_RESULT_TO_JAVA_OBJECT.to_string()
                    .replace("{default_value}", get_default_value_for_rust_type(&in_th.rust_type_name))
                    +
                    in_th.to_jni_converter.as_ref().map_or("", |v| v.0.as_str())
            )),
        });
    }
}

fn generate_type_info_for_generics(type_handlers: &mut Vec<TypeHandler>,
                                   class_info: &ForeignerClassInfo,
                                   package_name: &str) {
    for method in class_info.methods.iter() {
        for v in method.in_out_type
                .inputs
                .iter()
                .skip(if method.func_type == FuncVariant::Method {
                          1
                      } else {
                          0
                      }) {
            generate_type_info_for_type(type_handlers, &*v.ty, package_name);
        }
        match &method.in_out_type.output {
            &ast::FunctionRetTy::Ty(ref ret_type) => {
                if method.func_type != FuncVariant::Constructor {
                    //we unpack Result for constructor in other place
                    generate_type_info_for_type(type_handlers, &*ret_type, package_name);
                }
            }
            &ast::FunctionRetTy::Default(_) => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use syntex_syntax::parse;

    #[test]
    fn test_is_type_name() {
        let session = parse::ParseSess::new();
        let mut parser = parse::new_parser_from_source_str(&session,
                                                           "test".into(),
                                                           "Result<Foo, String>".into());
        let ty = parser.parse_ty().unwrap();
        assert!(is_type_name(&*ty, "Result"));
    }

    #[test]
    fn test_unpack_generic_first_paramter() {
        let session = parse::ParseSess::new();
        let mut parser = parse::new_parser_from_source_str(&session,
                                                           "test".into(),
                                                           "Result<Foo, String>".into());
        let ty = parser.parse_ty().unwrap();
        assert!(is_type_name(&*ty, "Result"));

        let ok_ty = unpack_generic_first_paramter(&*ty, "Result");
        assert!(!is_type_name(&ok_ty, "Result"));
        assert_eq!(pprust::ty_to_string(&ok_ty), "Foo");

        let ty = parse::new_parser_from_source_str(&session, "test".into(), "Vec<Foo>".into())
            .parse_ty()
            .unwrap();
        assert!(is_type_name(&ty, "Vec"));
        let in_ty = unpack_generic_first_paramter(&*ty, "Vec");
        assert_eq!(pprust::ty_to_string(&in_ty), "Foo");
    }

    #[test]
    fn test_alias_as_keyword() {
        let session = parse::ParseSess::new();
        let mut parser =
            parse::new_parser_from_source_str(&session, "test_alias".into(), "alias Foo;".into());
        let alias_keyword = ast::Ident::from_str("alias");
        assert!(parser.eat_contextual_keyword(alias_keyword));
    }
}
