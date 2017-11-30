use syntex_syntax::parse::{PResult, ParseSess};
use syntex_syntax::ptr::P;
use syntex_syntax::ast;
use syntex_syntax::ast::DUMMY_NODE_ID;

use my_ast::RustType;
use {ForeignInterfaceMethod, ForeignerClassInfo, ForeignerMethod, MethodVariant, SelfTypeVariant};
use super::{ForeignMethodSignature, ForeignTypeInfo, TypesConvMap};
use errors::fatal_error;

pub(crate) fn foreign_from_rust_convert_method_output<'a>(
    sess: &'a ParseSess,
    conv_map: &mut TypesConvMap,
    rust_ret_ty: &ast::FunctionRetTy,
    f_output: &ForeignTypeInfo,
    var_name: &str,
    func_ret_type: &str,
) -> PResult<'a, (Vec<P<ast::Item>>, String)> {
    let rust_ret_ty: ast::Ty = match *rust_ret_ty {
        ast::FunctionRetTy::Default(ref span) => if &*(f_output.name.as_str()) != "void" {
            return Err(fatal_error(
                sess,
                *span,
                &format!("Rust type `()` mapped to not void ({})", f_output.name),
            ));
        } else {
            return Ok((Vec::new(), String::new()));
        },
        ast::FunctionRetTy::Ty(ref p_ty) => (**p_ty).clone(),
    };
    let context_span = rust_ret_ty.span;
    conv_map.convert_rust_types(
        sess,
        &rust_ret_ty.into(),
        &f_output.correspoding_rust_type,
        var_name,
        func_ret_type,
        context_span,
    )
}

pub(crate) fn foreign_to_rust_convert_method_inputs<
    'a,
    FTI: AsRef<ForeignTypeInfo>,
    GI: Iterator<Item = String>,
>(
    sess: &'a ParseSess,
    conv_map: &mut TypesConvMap,
    method: &ForeignerMethod,
    f_method: &ForeignMethodSignature<FI = FTI>,
    arg_names: GI,
    func_ret_type: &str,
) -> PResult<'a, (Vec<P<ast::Item>>, String)> {
    let mut code_deps = Vec::<P<ast::Item>>::new();
    let mut ret_code = String::new();

    //skip self
    let skip_n = match method.variant {
        MethodVariant::Method(_) => 1,
        _ => 0,
    };
    for ((to_type, f_from), arg_name) in method
        .fn_decl
        .inputs
        .iter()
        .skip(skip_n)
        .zip(f_method.input().iter())
        .zip(arg_names)
    {
        let to: RustType = (*to_type.ty).clone().into();
        let (mut cur_deps, cur_code) = conv_map.convert_rust_types(
            sess,
            &f_from.as_ref().correspoding_rust_type,
            &to,
            &arg_name,
            func_ret_type,
            to_type.pat.span,
        )?;
        code_deps.append(&mut cur_deps);
        ret_code.push_str(&cur_code);
    }
    Ok((code_deps, ret_code))
}

pub(crate) fn create_suitable_types_for_constructor_and_self(
    self_variant: SelfTypeVariant,
    class: &ForeignerClassInfo,
    constructor_real_type: &ast::Ty,
) -> (ast::Ty, ast::Ty) {
    match self_variant {
        SelfTypeVariant::Default => {
            unimplemented!();
        }
        SelfTypeVariant::Mut => {
            unimplemented!();
        }
        SelfTypeVariant::Rptr | SelfTypeVariant::RptrMut => {
            let mutbl = if self_variant == SelfTypeVariant::Rptr {
                ast::Mutability::Immutable
            } else {
                ast::Mutability::Mutable
            };
            (
                ast::Ty {
                    id: DUMMY_NODE_ID,
                    span: constructor_real_type.span,
                    node: ast::TyKind::Rptr(
                        None,
                        ast::MutTy {
                            mutbl: mutbl,
                            ty: P(constructor_real_type.clone()),
                        },
                    ),
                },
                ast::Ty {
                    id: DUMMY_NODE_ID,
                    span: class.self_type.span,
                    node: ast::TyKind::Rptr(
                        None,
                        ast::MutTy {
                            mutbl: mutbl,
                            ty: P(ast::Ty {
                                id: DUMMY_NODE_ID,
                                span: class.self_type.span,
                                node: ast::TyKind::Path(None, class.self_type.clone()),
                            }),
                        },
                    ),
                },
            )
        }
    }
}

pub(crate) fn rust_to_foreign_convert_method_inputs<
    'a,
    GI: Iterator<Item = String>,
    FTI: AsRef<ForeignTypeInfo>,
>(
    sess: &'a ParseSess,
    conv_map: &mut TypesConvMap,
    method: &ForeignInterfaceMethod,
    f_method: &ForeignMethodSignature<FI = FTI>,
    arg_names: GI,
    func_ret_type: &str,
) -> PResult<'a, (Vec<P<ast::Item>>, String)> {
    let mut code_deps = Vec::<P<ast::Item>>::new();
    let mut ret_code = String::new();

    for ((from_ty, to_f), arg_name) in method
        .fn_decl
        .inputs
        .iter()
        .skip(1)//skip self
        .zip(f_method.input().iter())
        .zip(arg_names)
    {
        let from: RustType = (*from_ty.ty).clone().into();
        let (mut cur_deps, cur_code) = conv_map.convert_rust_types(
            sess,
            &from,
            &to_f.as_ref().correspoding_rust_type,
            &arg_name,
            func_ret_type,
            from_ty.pat.span,
        )?;
        code_deps.append(&mut cur_deps);
        ret_code.push_str(&cur_code);
    }
    Ok((code_deps, ret_code))
}
