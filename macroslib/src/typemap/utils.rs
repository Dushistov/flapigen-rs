use proc_macro2::{Span, TokenStream};
use quote::quote;
use rustc_hash::FxHashSet;
use std::{
    fs,
    path::{Path, PathBuf},
};
use syn::{spanned::Spanned, Ident, Type};

use crate::{
    error::{panic_on_syn_error, DiagnosticError, Result},
    file_cache::FileOperationsRegistrator,
    source_registry::SourceId,
    typemap::{
        ast::check_if_smart_pointer_return_inner_type,
        ty::RustType,
        typemap_macro::{FTypeConvRule, TypeMapConvRuleInfo},
        ForeignTypeInfo, RustTypeIdx, TypeMap,
    },
    types::{
        ForeignClassInfo, ForeignInterfaceMethod, ForeignMethod, MethodVariant, SelfTypeVariant,
    },
};

pub(crate) trait ForeignTypeInfoT {
    fn corresponding_rust_type(&self) -> &RustType;
}

impl ForeignTypeInfoT for ForeignTypeInfo {
    fn corresponding_rust_type(&self) -> &RustType {
        &self.corresponding_rust_type
    }
}

pub(crate) trait ForeignMethodSignature {
    type FI: ForeignTypeInfoT;
    fn input(&self) -> &[Self::FI];
}

pub(crate) fn foreign_from_rust_convert_method_output(
    conv_map: &mut TypeMap,
    src_id: SourceId,
    rust_ret_ty: &syn::ReturnType,
    gen_code_output_ty: RustTypeIdx,
    var_name: &str,
    func_ret_type: &str,
) -> Result<(Vec<TokenStream>, String)> {
    let context_span = rust_ret_ty.span();
    let rust_ret_ty: Type = match *rust_ret_ty {
        syn::ReturnType::Default => {
            return Ok((Vec::new(), String::new()));
        }
        syn::ReturnType::Type(_, ref p_ty) => (**p_ty).clone(),
    };
    let rust_ret_ty = conv_map.find_or_alloc_rust_type(&rust_ret_ty, src_id);
    conv_map.convert_rust_types(
        rust_ret_ty.to_idx(),
        gen_code_output_ty,
        var_name,
        var_name,
        func_ret_type,
        (src_id, context_span),
    )
}

pub(crate) fn foreign_to_rust_convert_method_inputs<
    FTI: ForeignTypeInfoT,
    S: AsRef<str>,
    GI: Iterator<Item = S>,
>(
    conv_map: &mut TypeMap,
    src_id: SourceId,
    method: &ForeignMethod,
    f_method: &dyn ForeignMethodSignature<FI = FTI>,
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
        let to_named_arg = to_type
            .as_named_arg()
            .map_err(|err| DiagnosticError::from_syn_err(src_id, err))?;
        let to: RustType = conv_map.find_or_alloc_rust_type(&to_named_arg.ty, src_id);
        let (mut cur_deps, cur_code) = conv_map.convert_rust_types(
            f_from.corresponding_rust_type().to_idx(),
            to.to_idx(),
            arg_name.as_ref(),
            arg_name.as_ref(),
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
    class: &ForeignClassInfo,
    constructor_real_type: &Type,
) -> (Type, Type) {
    match self_variant {
        SelfTypeVariant::Default => {
            unimplemented!("self not supported, use &self or &mut self");
        }
        SelfTypeVariant::Mut => {
            unimplemented!("'mut self' not supported, use &self or &mut self");
        }
        SelfTypeVariant::Rptr | SelfTypeVariant::RptrMut => {
            let c_sp = constructor_real_type.span();
            let self_type = class.self_type_as_ty();
            let s_sp = self_type.span();

            if self_variant == SelfTypeVariant::Rptr {
                (
                    parse_type_spanned_checked!(c_sp, & #constructor_real_type),
                    parse_type_spanned_checked!(s_sp, & #self_type),
                )
            } else {
                (
                    parse_type_spanned_checked!(c_sp, &mut #constructor_real_type),
                    parse_type_spanned_checked!(s_sp, &mut #self_type),
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
    f_method: &dyn ForeignMethodSignature<FI = FTI>,
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
        let from_named_arg = from_ty
            .as_named_arg()
            .map_err(|err| DiagnosticError::from_syn_err(src_id, err))?;
        let from: RustType = conv_map.find_or_alloc_rust_type(&from_named_arg.ty, src_id);
        let (mut cur_deps, cur_code) = conv_map.convert_rust_types(
            from.to_idx(),
            to_f.corresponding_rust_type().to_idx(),
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
        if let Some(inner_ty) = check_if_smart_pointer_return_inner_type(from, smart_pointer) {
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
) -> (RustType, TokenStream) {
    let var_name = Ident::new(var_name, Span::call_site());

    for smart_pointer in &["Box", "Rc", "Arc"] {
        if let Some(inner_ty) = check_if_smart_pointer_return_inner_type(from, smart_pointer) {
            let inner_ty: RustType = tmap.find_or_alloc_rust_type(&inner_ty, from.src_id);
            let inner_ty_norm: Type = inner_ty.to_type_without_lifetimes();
            let smart_pointer_ty: Type = syn::parse_str(smart_pointer).unwrap_or_else(|err| {
                panic_on_syn_error(
                    "typemap::utils internal error, can not parse smart ptr",
                    (*smart_pointer).into(),
                    err,
                )
            });
            let code: TokenStream = quote! {
                let #var_name: *const #inner_ty_norm = #smart_pointer_ty::into_raw(#var_name);
            };
            return (inner_ty, code);
        }
    }
    let inner_ty = from.clone();
    let inner_ty_norm = inner_ty.to_type_without_lifetimes();

    let code = quote! {
        let #var_name: Box<#inner_ty_norm> = Box::new(#var_name);
        let #var_name: *mut #inner_ty_norm = Box::into_raw(#var_name);
    };

    (inner_ty, code)
}

pub(crate) fn unpack_from_heap_pointer(
    from: &RustType,
    var_name: &str,
    unbox_if_boxed: bool,
) -> String {
    for smart_pointer in &["Box", "Rc", "Arc"] {
        if check_if_smart_pointer_return_inner_type(from, smart_pointer).is_some() {
            return format!(
                r#"
    let {var_name}: {rc_type}  = unsafe {{ {smart_pointer}::from_raw({var_name}) }};
"#,
                var_name = var_name,
                rc_type = from,
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
            inside_box_type = from
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
        inside_box_type = from,
        unbox_code = unbox_code
    )
}

pub(crate) fn configure_ftype_rule(
    f_type_rules: &mut Vec<FTypeConvRule>,
    rule_type: &str,
    rule_src_id: SourceId,
    options: &FxHashSet<&'static str>,
) -> Result<()> {
    f_type_rules.retain(|rule| {
        rule.cfg_option
            .as_ref()
            .map(|opt| options.contains(opt.as_str()))
            .unwrap_or(true)
    });
    if f_type_rules.len() > 1 {
        let first_rule = f_type_rules.remove(0);
        let mut err = DiagnosticError::new(
            rule_src_id,
            first_rule.left_right_ty.span(),
            format!(
                "multiply f_type '{}' rules, that possible to use in this configuration, first",
                rule_type,
            ),
        );
        for other in f_type_rules.iter() {
            err.span_note(
                (rule_src_id, other.left_right_ty.span()),
                format!("other f_type '{}' rule", rule_type),
            );
        }
        return Err(err);
    }
    if f_type_rules.len() == 1 {
        f_type_rules[0].cfg_option = None;
    }
    Ok(())
}

pub(crate) fn remove_files_if<Filter>(
    output_dir: &Path,
    if_remove_filter: Filter,
) -> std::result::Result<(), String>
where
    Filter: Fn(&Path) -> bool,
{
    let entries = fs::read_dir(output_dir)
        .map_err(|err| format!("read_dir({}) failed: {}", output_dir.display(), err))?;

    for path in entries {
        let path = path
            .map_err(|err| {
                format!(
                    "Can not get next entry during parsing read_dir output: {}",
                    err
                )
            })?
            .path();
        if if_remove_filter(&path) {
            fs::remove_file(&path)
                .map_err(|err| format!("error during remove {}: {}", path.display(), err))?;
        }
    }
    Ok(())
}

impl FileOperationsRegistrator for FxHashSet<PathBuf> {
    fn register(&mut self, p: &Path) {
        self.insert(p.into());
    }
}
