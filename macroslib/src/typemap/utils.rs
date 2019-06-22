use proc_macro2::TokenStream;
use rustc_hash::FxHashSet;
use syn::{spanned::Spanned, Type};

use crate::{
    error::{DiagnosticError, Result},
    source_registry::SourceId,
    typemap::{
        ast::{
            check_if_smart_pointer_return_inner_type, normalize_ty_lifetimes,
            parse_ty_with_given_span_checked, DisplayToTokens,
        },
        parse_typemap_macro::{FTypeConvRule, TypeMapConvRuleInfo},
        ty::RustType,
        ForeignTypeInfo, TypeMap,
    },
    types::{
        ForeignInterfaceMethod, ForeignerClassInfo, ForeignerMethod, MethodVariant, SelfTypeVariant,
    },
};

pub(crate) trait ForeignTypeInfoT {
    fn name(&self) -> &str;
    fn correspoding_rust_type(&self) -> &RustType;
}

impl ForeignTypeInfoT for ForeignTypeInfo {
    fn name(&self) -> &str {
        self.name.as_str()
    }
    fn correspoding_rust_type(&self) -> &RustType {
        &self.correspoding_rust_type
    }
}

pub(crate) trait ForeignMethodSignature {
    type FI: ForeignTypeInfoT;
    fn output(&self) -> &ForeignTypeInfoT;
    fn input(&self) -> &[Self::FI];
}

pub(crate) fn foreign_from_rust_convert_method_output(
    conv_map: &mut TypeMap,
    src_id: SourceId,
    rust_ret_ty: &syn::ReturnType,
    f_output: &ForeignTypeInfoT,
    var_name: &str,
    func_ret_type: &str,
) -> Result<(Vec<TokenStream>, String)> {
    let rust_ret_ty: Type = match *rust_ret_ty {
        syn::ReturnType::Default => {
            if f_output.name() != "void" {
                return Err(DiagnosticError::new(
                    src_id,
                    rust_ret_ty.span(),
                    format!("Rust type `()` mapped to not void ({})", f_output.name()),
                ));
            } else {
                return Ok((Vec::new(), String::new()));
            }
        }
        syn::ReturnType::Type(_, ref p_ty) => (**p_ty).clone(),
    };
    let context_span = rust_ret_ty.span();
    let rust_ret_ty = conv_map.find_or_alloc_rust_type(&rust_ret_ty, src_id);
    conv_map.convert_rust_types(
        rust_ret_ty.to_idx(),
        f_output.correspoding_rust_type().to_idx(),
        var_name,
        var_name,
        func_ret_type,
        (src_id, context_span),
    )
}

pub(crate) fn foreign_to_rust_convert_method_inputs<
    FTI: ForeignTypeInfoT,
    GI: Iterator<Item = String>,
>(
    conv_map: &mut TypeMap,
    src_id: SourceId,
    method: &ForeignerMethod,
    f_method: &ForeignMethodSignature<FI = FTI>,
    arg_names: GI,
    func_ret_type: &str,
) -> Result<(Vec<TokenStream>, String)> {
    let mut code_deps = Vec::new();
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
        let to_named_arg = to_type.as_named_arg(src_id)?;
        let to: RustType = conv_map.find_or_alloc_rust_type(&to_named_arg.ty, src_id);
        let (mut cur_deps, cur_code) = conv_map.convert_rust_types(
            f_from.correspoding_rust_type().to_idx(),
            to.to_idx(),
            &arg_name,
            &arg_name,
            func_ret_type,
            (src_id, to_named_arg.ty.span()),
        )?;
        code_deps.append(&mut cur_deps);
        ret_code.push_str(&cur_code);
    }
    Ok((code_deps, ret_code))
}

pub(crate) fn create_suitable_types_for_constructor_and_self(
    self_variant: SelfTypeVariant,
    class: &ForeignerClassInfo,
    constructor_real_type: &Type,
) -> (Type, Type) {
    match self_variant {
        SelfTypeVariant::Default => {
            unimplemented!();
        }
        SelfTypeVariant::Mut => {
            unimplemented!();
        }
        SelfTypeVariant::Rptr | SelfTypeVariant::RptrMut => {
            let self_type = class.self_type_as_ty();
            if self_variant == SelfTypeVariant::Rptr {
                (
                    parse_ty_with_given_span_checked(
                        &format!("& {}", DisplayToTokens(constructor_real_type)),
                        constructor_real_type.span(),
                    ),
                    parse_ty_with_given_span_checked(
                        &format!("& {}", DisplayToTokens(&self_type)),
                        self_type.span(),
                    ),
                )
            } else {
                (
                    parse_ty_with_given_span_checked(
                        &format!("&mut {}", DisplayToTokens(constructor_real_type)),
                        constructor_real_type.span(),
                    ),
                    parse_ty_with_given_span_checked(
                        &format!("&mut {}", DisplayToTokens(&self_type)),
                        self_type.span(),
                    ),
                )
            }
        }
    }
}

pub(crate) fn rust_to_foreign_convert_method_inputs<
    GI: Iterator<Item = String>,
    FTI: ForeignTypeInfoT,
>(
    conv_map: &mut TypeMap,
    src_id: SourceId,
    method: &ForeignInterfaceMethod,
    f_method: &ForeignMethodSignature<FI = FTI>,
    arg_names: GI,
    func_ret_type: &str,
) -> Result<(Vec<TokenStream>, String)> {
    let mut code_deps = Vec::new();
    let mut ret_code = String::new();

    for ((from_ty, to_f), arg_name) in method
        .fn_decl
        .inputs
        .iter()
        .skip(1) //skip self
        .zip(f_method.input().iter())
        .zip(arg_names)
    {
        let from_named_arg = from_ty.as_named_arg(src_id)?;
        let from: RustType = conv_map.find_or_alloc_rust_type(&from_named_arg.ty, src_id);
        let (mut cur_deps, cur_code) = conv_map.convert_rust_types(
            from.to_idx(),
            to_f.correspoding_rust_type().to_idx(),
            &arg_name,
            &arg_name,
            func_ret_type,
            (src_id, from_named_arg.ty.span()),
        )?;
        code_deps.append(&mut cur_deps);
        ret_code.push_str(&cur_code);
    }
    Ok((code_deps, ret_code))
}

pub(crate) fn validate_cfg_options(
    rule: &TypeMapConvRuleInfo,
    avaible_opts: &FxHashSet<&'static str>,
) -> Result<()> {
    let validate_f_type_rules_opts = |rules: &[FTypeConvRule]| -> Result<()> {
        for r in rules {
            if let Some(ref opt) = r.cfg_option {
                if !avaible_opts.contains(opt.as_str()) {
                    return Err(DiagnosticError::new(
                        rule.src_id,
                        opt.sp,
                        format!(
                            "unsupported option {}, avaible options {:?}",
                            opt.as_str(),
                            avaible_opts
                        ),
                    ));
                }
            }
        }
        Ok(())
    };
    validate_f_type_rules_opts(&rule.ftype_left_to_right)?;
    validate_f_type_rules_opts(&rule.ftype_right_to_left)?;

    for fcode in rule.f_code.iter() {
        if let Some(ref opt) = fcode.cfg_option {
            if !avaible_opts.contains(opt.as_str()) {
                return Err(DiagnosticError::new(
                    rule.src_id,
                    opt.sp,
                    format!(
                        "unsupported option {}, avaible options {:?}",
                        opt.as_str(),
                        avaible_opts
                    ),
                ));
            }
        }
    }

    Ok(())
}

pub(crate) fn boxed_type(tmap: &mut TypeMap, from: &RustType) -> RustType {
    for smart_pointer in &["Box", "Rc", "Arc"] {
        if let Some(inner_ty) = check_if_smart_pointer_return_inner_type(from, *smart_pointer) {
            let inner_ty: RustType = tmap.find_or_alloc_rust_type(&inner_ty, from.src_id);
            return inner_ty;
        }
    }
    from.clone()
}

pub(crate) fn convert_to_heap_pointer(
    tmap: &mut TypeMap,
    from: &RustType,
    var_name: &str,
) -> (RustType, String) {
    for smart_pointer in &["Box", "Rc", "Arc"] {
        if let Some(inner_ty) = check_if_smart_pointer_return_inner_type(from, *smart_pointer) {
            let inner_ty: RustType = tmap.find_or_alloc_rust_type(&inner_ty, from.src_id);
            let code = format!(
                r#"
    let {var_name}: *const {inner_ty} = {smart_pointer}::into_raw({var_name});
"#,
                var_name = var_name,
                inner_ty = inner_ty.normalized_name,
                smart_pointer = *smart_pointer,
            );
            return (inner_ty, code);
        }
    }

    let inner_ty = from.clone();
    let inner_ty_str = normalize_ty_lifetimes(&inner_ty.ty);
    (
        inner_ty,
        format!(
            r#"
    let {var_name}: Box<{inner_ty}> = Box::new({var_name});
    let {var_name}: *mut {inner_ty} = Box::into_raw({var_name});
"#,
            var_name = var_name,
            inner_ty = inner_ty_str
        ),
    )
}

pub(crate) fn unpack_from_heap_pointer(
    from: &RustType,
    var_name: &str,
    unbox_if_boxed: bool,
) -> String {
    for smart_pointer in &["Box", "Rc", "Arc"] {
        if check_if_smart_pointer_return_inner_type(from, *smart_pointer).is_some() {
            return format!(
                r#"
    let {var_name}: {rc_type}  = unsafe {{ {smart_pointer}::from_raw({var_name}) }};
"#,
                var_name = var_name,
                rc_type = from.normalized_name,
                smart_pointer = *smart_pointer,
            );
        }
    }
    let unbox_code = if unbox_if_boxed {
        format!(
            r#"
    let {var_name}: {inside_box_type} = *{var_name};
"#,
            var_name = var_name,
            inside_box_type = from.normalized_name
        )
    } else {
        String::new()
    };
    format!(
        r#"
    let {var_name}: Box<{inside_box_type}> = unsafe {{ Box::from_raw({var_name}) }};
{unbox_code}
"#,
        var_name = var_name,
        inside_box_type = from.normalized_name,
        unbox_code = unbox_code
    )
}
