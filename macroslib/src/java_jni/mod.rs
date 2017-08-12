mod java_code;
mod rust_code;

use std::path::Path;
use std::fmt;

use petgraph::Direction;
use syntex_syntax::{ast, parse};
use syntex_syntax::ptr::P;
use syntex_syntax::parse::ParseSess;
use syntex_syntax::symbol::Symbol;
use syntex_syntax::parse::PResult;
use syntex_pos::DUMMY_SP;
use syntex_syntax::ast::DUMMY_NODE_ID;

use types_conv_map::{make_unique_rust_typename, ForeignTypeInfo};
use errors::fatal_error;
use {ForeignerClassInfo, ForeignerMethod, MethodVariant, TypesConvMap};
use my_ast::{normalized_ty_string, parse_ty, RustType};

struct ForeignMethodSignature {
    output: ForeignTypeInfo,
    input: Vec<ForeignTypeInfo>,
}

pub(crate) fn generate<'a>(
    sess: &'a ParseSess,
    conv_map: &mut TypesConvMap,
    output_dir: &Path,
    package_name: &str,
    class: &ForeignerClassInfo,
) -> PResult<'a, Vec<P<ast::Item>>> {
    trace!("generate: begin");
    let f_methods_sign = find_suitable_foreign_types_for_methods(sess, conv_map, class)?;
    java_code::generate_java_code(output_dir, package_name, class, &f_methods_sign)
        .map_err(|err| fatal_error(sess, DUMMY_SP, &err))?;
    trace!("generate: java code done");
    let ast_items =
        rust_code::generate_rust_code(sess, conv_map, package_name, class, &f_methods_sign)?;

    if let Some(this_type_for_method) = class.this_type_for_method.as_ref() {
        //

        let this_type: RustType = this_type_for_method.clone().into();
        let this_type = this_type.implements("SwigForeignClass");
        let jobject_name = Symbol::intern("jobject");
        let jobject_ty = parse_ty(sess, DUMMY_SP, jobject_name)?;
        let my_jobj_ti = RustType::new(
            jobject_ty,
            make_unique_rust_typename(jobject_name, this_type.normalized_name),
        );
        conv_map.cache_rust_to_foreign_conv(&this_type, (my_jobj_ti, class.name));
    }
    Ok(ast_items)
}

fn method_name(method: &ForeignerMethod) -> String {
    match method.variant {
        MethodVariant::StaticMethod => method.short_name().as_str().to_string(),
        MethodVariant::Method(_) => format!("do_{}", method.short_name()),
        MethodVariant::Constructor => "init".into(),
    }
}


fn find_suitable_foreign_types_for_methods<'a>(
    sess: &'a ParseSess,
    conv_map: &mut TypesConvMap,
    class: &ForeignerClassInfo,
) -> PResult<'a, Vec<ForeignMethodSignature>> {
    let mut ret = Vec::<ForeignMethodSignature>::with_capacity(class.methods.len());
    let empty_symbol = Symbol::intern("");
    let dummy_ty = ast::Ty {
        id: DUMMY_NODE_ID,
        span: DUMMY_SP,
        node: ast::TyKind::Tup(vec![]),
    };
    for method in &class.methods {
        //skip self argument
        let skip_n = match method.variant {
            MethodVariant::Method(_) => 1,
            _ => 0,
        };
        assert!(method.fn_decl.inputs.len() >= skip_n);
        let mut input = Vec::with_capacity(method.fn_decl.inputs.len() - skip_n);
        for arg in method.fn_decl.inputs.iter().skip(skip_n) {
            let rust_typename = Symbol::intern(&normalized_ty_string(&*arg.ty));
            let f_arg_type = conv_map
                .map_through_conversation_to_foreign(&*arg.ty, Direction::Incoming)
                .ok_or_else(|| {
                    fatal_error(
                        sess,
                        arg.ty.span,
                        &format!(
                            "Do not know conversation from foreign \
                             to such rust type {}",
                            rust_typename
                        ),
                    )
                })?;
            input.push(f_arg_type);
        }
        let output = match method.variant {
            MethodVariant::Constructor => ForeignTypeInfo {
                name: empty_symbol,
                correspoding_rust_type: dummy_ty.clone().into(),
            },
            _ => match method.fn_decl.output {
                ast::FunctionRetTy::Default(sp) => ForeignTypeInfo {
                    name: Symbol::intern("void"),
                    correspoding_rust_type: {
                        let mut ty: ast::Ty = dummy_ty.clone().into();
                        ty.span = sp;
                        ty.into()
                    },
                },
                ast::FunctionRetTy::Ty(ref rt) => conv_map
                    .map_through_conversation_to_foreign(&*rt, Direction::Outgoing)
                    .ok_or_else(|| {
                        fatal_error(
                            sess,
                            rt.span,
                            &format!(
                                "Do not know conversation from \
                                 such rust type {} to foreign",
                                normalized_ty_string(&*rt)
                            ),
                        )
                    })?,
            },
        };
        ret.push(ForeignMethodSignature { output, input });
    }
    Ok(ret)
}

fn fmt_write_err_map(err: fmt::Error) -> String {
    format!("fmt write error: {}", err)
}

fn code_to_item<'a>(
    sess: &'a ParseSess,
    for_func_name: &str,
    code: &str,
) -> PResult<'a, Vec<P<ast::Item>>> {
    let mut parser = parse::new_parser_from_source_str(sess, for_func_name.into(), code.into());

    let krate = parser.parse_crate_mod()?;
    Ok(krate.module.items)
}

fn java_class_full_name(package_name: &str, class_name: &str) -> String {
    let mut ret: String = package_name.into();
    ret.push('.');
    ret.push_str(class_name);
    ret
}

fn java_class_name_to_jni(full_name: &str) -> String {
    full_name.replace(".", "/")
}
