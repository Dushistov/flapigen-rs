mod fclass;
mod fenum;
mod finterface;
mod java_code;
mod map_class_self_type;
mod map_type;
mod rust_code;

use log::debug;
use proc_macro2::TokenStream;
use rustc_hash::FxHashSet;
use smol_str::SmolStr;
use std::fmt;
use syn::Type;

use crate::{
    error::{DiagnosticError, Result},
    typemap::{
        ast::{if_result_return_ok_err_types, if_ty_result_return_ok_type, DisplayToTokens},
        ty::RustType,
        utils::{
            configure_ftype_rule, validate_cfg_options, ForeignMethodSignature, ForeignTypeInfoT,
        },
        ForeignTypeInfo, TypeMapConvRuleInfo,
    },
    types::{ForeignerClassInfo, ForeignerMethod, ItemToExpand, MethodVariant},
    JavaConfig, LanguageGenerator, SourceCode, TypeMap,
};
use map_class_self_type::register_typemap_for_self_type;

#[derive(Debug)]
struct JavaContext<'a> {
    cfg: &'a JavaConfig,
    conv_map: &'a mut TypeMap,
    pointer_target_width: usize,
    rust_code: &'a mut Vec<TokenStream>,
}

#[derive(Clone, Copy, Debug)]
enum NullAnnotation {
    NonNull,
    Nullable,
}

#[derive(Debug)]
struct JavaForeignTypeInfo {
    pub base: ForeignTypeInfo,
    pub java_converter: Option<JavaConverter>,
    annotation: Option<NullAnnotation>,
}

impl ForeignTypeInfoT for JavaForeignTypeInfo {
    fn name(&self) -> &str {
        self.base.name.as_str()
    }
    fn correspoding_rust_type(&self) -> &RustType {
        &self.base.correspoding_rust_type
    }
}

#[derive(Debug)]
struct JavaConverter {
    java_transition_type: SmolStr,
    converter: String,
}

impl AsRef<ForeignTypeInfo> for JavaForeignTypeInfo {
    fn as_ref(&self) -> &ForeignTypeInfo {
        &self.base
    }
}

impl From<ForeignTypeInfo> for JavaForeignTypeInfo {
    fn from(x: ForeignTypeInfo) -> Self {
        JavaForeignTypeInfo {
            base: ForeignTypeInfo {
                name: x.name,
                correspoding_rust_type: x.correspoding_rust_type,
            },
            java_converter: None,
            annotation: None,
        }
    }
}

struct JniForeignMethodSignature {
    output: JavaForeignTypeInfo,
    input: Vec<JavaForeignTypeInfo>,
}

impl ForeignMethodSignature for JniForeignMethodSignature {
    type FI = JavaForeignTypeInfo;
    fn output(&self) -> &ForeignTypeInfoT {
        &self.output
    }
    fn input(&self) -> &[JavaForeignTypeInfo] {
        &self.input[..]
    }
}

impl JavaConfig {
    fn register_class(&self, conv_map: &mut TypeMap, class: &ForeignerClassInfo) -> Result<()> {
        class
            .validate_class()
            .map_err(|err| DiagnosticError::new(class.src_id, class.span(), &err))?;
        if let Some(self_desc) = class.self_desc.as_ref() {
            let constructor_ret_type = &self_desc.constructor_ret_type;
            let this_type_for_method = if_ty_result_return_ok_type(constructor_ret_type)
                .unwrap_or_else(|| constructor_ret_type.clone());

            let mut traits = vec!["SwigForeignClass"];
            if class.clone_derived {
                traits.push("Clone");
            }
            let this_type: RustType = conv_map.find_or_alloc_rust_type_that_implements(
                &this_type_for_method,
                &traits,
                class.src_id,
            );
            register_typemap_for_self_type(conv_map, class, this_type, self_desc)?;
        }

        let _ = conv_map.find_or_alloc_rust_type(&class.self_type_as_ty(), class.src_id);

        Ok(())
    }
}

impl LanguageGenerator for JavaConfig {
    fn expand_items(
        &self,
        conv_map: &mut TypeMap,
        pointer_target_width: usize,
        code: &[SourceCode],
        items: Vec<ItemToExpand>,
    ) -> Result<Vec<TokenStream>> {
        let mut ret = Vec::with_capacity(items.len());
        init(
            &mut JavaContext {
                cfg: self,
                conv_map,
                pointer_target_width,
                rust_code: &mut ret,
            },
            code,
        )?;
        for item in &items {
            if let ItemToExpand::Class(ref fclass) = item {
                self.register_class(conv_map, fclass)?;
            }
        }
        let mut ctx = JavaContext {
            cfg: self,
            conv_map,
            pointer_target_width,
            rust_code: &mut ret,
        };
        for item in items {
            match item {
                ItemToExpand::Class(fclass) => {
                    fclass::generate(&mut ctx, &fclass)?;
                }
                ItemToExpand::Enum(fenum) => {
                    fenum::generate_enum(&mut ctx, &fenum)?;
                }
                ItemToExpand::Interface(finterface) => {
                    finterface::generate_interface(&mut ctx, &finterface)?;
                }
            }
        }
        Ok(ret)
    }
}

fn method_name(method: &ForeignerMethod, f_method: &JniForeignMethodSignature) -> String {
    let need_conv = f_method.input.iter().any(|v| v.java_converter.is_some())
        || f_method.output.java_converter.is_some();
    match method.variant {
        MethodVariant::StaticMethod if !need_conv => method.short_name().as_str().to_string(),
        MethodVariant::Method(_) | MethodVariant::StaticMethod => {
            format!("do_{}", method.short_name())
        }
        MethodVariant::Constructor => "init".into(),
    }
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

fn calc_this_type_for_method(tm: &TypeMap, class: &ForeignerClassInfo) -> Option<Type> {
    if let Some(constructor_ret_type) = class.self_desc.as_ref().map(|x| &x.constructor_ret_type) {
        Some(
            if_result_return_ok_err_types(
                &tm.ty_to_rust_type_checked(constructor_ret_type)
                    .unwrap_or_else(|| {
                        panic!(
                            "Internal error: constructor type {} for class {} unknown",
                            DisplayToTokens(constructor_ret_type),
                            class.name
                        );
                    }),
            )
            .map(|(ok_ty, _err_ty)| ok_ty)
            .unwrap_or_else(|| constructor_ret_type.clone()),
        )
    } else {
        None
    }
}

fn merge_rule(ctx: &mut JavaContext, mut rule: TypeMapConvRuleInfo) -> Result<()> {
    debug!("merge_rule begin {:?}", rule);
    if rule.is_empty() {
        return Err(DiagnosticError::new(
            rule.src_id,
            rule.span,
            format!("rule {:?} is empty", rule),
        ));
    }
    let all_options = {
        let mut opts = FxHashSet::<&'static str>::default();
        opts.insert("NullAnnotations");
        opts.insert("NoNullAnnotations");
        opts
    };
    validate_cfg_options(&rule, &all_options)?;
    let options = {
        let mut opts = FxHashSet::<&'static str>::default();
        if ctx.cfg.null_annotation_package.is_some() {
            opts.insert("NullAnnotations");
        } else {
            opts.insert("NoNullAnnotations");
        }
        opts
    };
    if rule.c_types.is_some() {
        return Err(DiagnosticError::new(
            rule.src_id,
            rule.span,
            format!("c_types not supported for Java/JNI"),
        ));
    }
    if !rule.f_code.is_empty() {
        unimplemented!();
    }
    configure_ftype_rule(&mut rule.ftype_left_to_right, "=>", rule.src_id, &options)?;
    configure_ftype_rule(&mut rule.ftype_right_to_left, "<=", rule.src_id, &options)?;
    ctx.conv_map.merge_conv_rule(rule.src_id, rule)?;
    Ok(())
}

fn init(ctx: &mut JavaContext, _code: &[SourceCode]) -> Result<()> {
    ctx.conv_map
        .find_or_alloc_rust_type_no_src_id(&parse_type! { jint });
    ctx.conv_map
        .find_or_alloc_rust_type_no_src_id(&parse_type! { jlong });
    let not_merged_data = ctx.conv_map.take_not_merged_not_generic_rules();
    for rule in not_merged_data {
        merge_rule(ctx, rule)?;
    }
    Ok(())
}

fn map_write_err<Err: fmt::Display>(err: Err) -> String {
    format!("write failed: {}", err)
}
