use log::{debug, trace};
use petgraph::Direction;
use std::rc::Rc;

use super::{
    calc_this_type_for_method, java_code, merge_rule, JavaContext, JavaConverter,
    JavaForeignTypeInfo, NullAnnotation,
};
use crate::{
    error::{invalid_src_id_span, DiagnosticError, Result, SourceIdSpan},
    typemap::{
        ast::{if_option_return_some_type, DisplayToTokens, TyParamsSubstList},
        ty::{ForeignType, RustType, TraitNamesSet},
        ExpandedFType, ForeignTypeInfo, MapToForeignFlag, TypeMapConvRuleInfoExpanderHelper,
        FROM_VAR_TEMPLATE,
    },
    TypeMap,
};

struct JavaContextForArg<'a, 'b> {
    ctx: &'a mut JavaContext<'b>,
    arg_ty_span: SourceIdSpan,
    direction: Direction,
}

pub(in crate::java_jni) fn map_type(
    ctx: &mut JavaContext,
    arg_ty: &RustType,
    direction: Direction,
    arg_ty_span: SourceIdSpan,
) -> Result<JavaForeignTypeInfo> {
    debug!("map_type: arg_ty {}", arg_ty);

    let fti = do_map_type(ctx, arg_ty, direction, arg_ty_span)?;
    let ftype = &ctx.conv_map[fti];
    let origin_ftype_span = ftype.src_id_span();
    if !ftype.provided_by_module.is_empty() {
        unimplemented!();
    }
    let rule = match direction {
        petgraph::Direction::Outgoing => ftype.into_from_rust.as_ref(),
        petgraph::Direction::Incoming => ftype.from_into_rust.as_ref(),
    }
    .ok_or_else(|| {
        DiagnosticError::new2(
            origin_ftype_span,
            format!(
                "No rule to convert foreign type {} as input/output type",
                ftype.name
            ),
        )
    })?;
    let base_rt;
    let base_ft_name = ftype.typename().typename.clone();
    let mut java_converter = None;
    if let Some(intermediate) = rule.intermediate.as_ref() {
        if intermediate.input_to_output {
            unimplemented!();
        }
        base_rt = intermediate.intermediate_ty;
        let converter = intermediate.conv_code.to_string();
        let intermediate_ty = intermediate.intermediate_ty;

        let rty = ctx.conv_map[intermediate_ty].clone();
        let arg_span = intermediate.conv_code.full_span();
        let inter_ft = map_type(ctx, &rty, direction, arg_span)?;
        if inter_ft.java_converter.is_some()
            || base_rt != inter_ft.base.corresponding_rust_type.to_idx()
        {
            return Err(DiagnosticError::new2(
                origin_ftype_span,
                format!(
                    "Error during conversion {} for {},\n
                    intermediate type '{}' can not be directly converted to Java",
                    base_ft_name,
                    match direction {
                        petgraph::Direction::Outgoing => "output",
                        petgraph::Direction::Incoming => "input",
                    },
                    rty
                ),
            )
            .add_span_note(
                invalid_src_id_span(),
                if let Some(java_conv) = inter_ft.java_converter {
                    format!(
                        "it requires Java code to convert from '{}' to '{}'",
                        java_conv.java_transition_type, inter_ft.base.name
                    )
                } else {
                    format!(
                        "Type '{}' require conversion to type '{}' before usage as Java type",
                        ctx.conv_map[base_rt], inter_ft.base.corresponding_rust_type
                    )
                },
            ));
        }
        let annotation = type_annotation(inter_ft.base.name.display());
        java_converter = Some(JavaConverter {
            java_transition_type: inter_ft.base.name,
            converter,
            annotation,
        });
    } else {
        base_rt = rule.rust_ty;
    }
    let annotation = type_annotation(base_ft_name.display());
    let mut fti = JavaForeignTypeInfo {
        base: ForeignTypeInfo {
            name: base_ft_name,
            corresponding_rust_type: ctx.conv_map[base_rt].clone(),
        },
        java_converter,
        annotation,
    };
    if fti.annotation.is_none() && !java_code::is_primitive_type(fti.base.name.display()) {
        fti.annotation = Some(if if_option_return_some_type(arg_ty).is_none() {
            NullAnnotation::NonNull
        } else {
            NullAnnotation::Nullable
        });
    }
    debug!("map_type: return fti: {:?}", fti);
    Ok(fti)
}

fn do_map_type(
    ctx: &mut JavaContext,
    arg_ty: &RustType,
    direction: Direction,
    arg_ty_span: SourceIdSpan,
) -> Result<ForeignType> {
    debug!("do_map_type: arg_ty {}, direction {:?}", arg_ty, direction);
    if let Some(ftype) = ctx.conv_map.map_through_conversion_to_foreign(
        arg_ty.to_idx(),
        direction,
        MapToForeignFlag::FastSearch,
        arg_ty_span,
        calc_this_type_for_method,
    ) {
        return Ok(ftype);
    }

    let idx_subst_map: Option<(Rc<_>, TyParamsSubstList)> =
        ctx.conv_map.generic_rules().iter().find_map(|grule| {
            grule
                .is_ty_subst_of_my_generic_rtype(&arg_ty.ty, direction, |ty, traits| -> bool {
                    is_ty_implement_traits(ctx.conv_map, ty, traits)
                })
                .map(|sm| (grule.clone(), sm.into()))
        });
    if let Some((grule, subst_list)) = idx_subst_map {
        debug!(
            "do_map_type: we found generic rule for {}: {:?}",
            arg_ty, subst_list
        );
        if grule.c_types.is_some() || grule.generic_c_types.is_some() {
            return Err(DiagnosticError::new(
                grule.src_id,
                grule.span,
                "Can not handle C types for Java/JNI",
            ));
        }
        let subst_map = subst_list.as_slice().into();
        let new_rule = grule
            .subst_generic_params(
                subst_map,
                direction,
                &mut JavaContextForArg {
                    ctx,
                    arg_ty_span,
                    direction,
                },
            )
            .map_err(|err| {
                err.add_span_note(
                    (grule.src_id, grule.span),
                    "subst. of generic params into rule failed",
                )
            })?;
        debug_assert!(!new_rule.is_empty());
        merge_rule(ctx, new_rule)?;
        if let Some(ftype) = ctx.conv_map.map_through_conversion_to_foreign(
            arg_ty.to_idx(),
            direction,
            MapToForeignFlag::FullSearch,
            arg_ty_span,
            calc_this_type_for_method,
        ) {
            return Ok(ftype);
        }
    } else if let Some(ftype) = ctx.conv_map.map_through_conversion_to_foreign(
        arg_ty.to_idx(),
        direction,
        MapToForeignFlag::FullSearch,
        arg_ty_span,
        calc_this_type_for_method,
    ) {
        return Ok(ftype);
    }

    match direction {
        Direction::Outgoing => Err(DiagnosticError::new2(
            arg_ty_span,
            format!(
                "Do not know conversion from \
                 such rust type '{}' to Java type",
                arg_ty
            ),
        )),

        Direction::Incoming => Err(DiagnosticError::new2(
            arg_ty_span,
            format!(
                "Do not know conversion from Java type \
                 to such rust type '{}'",
                arg_ty
            ),
        )),
    }
}

fn is_ty_implement_traits(tmap: &TypeMap, ty: &syn::Type, traits: &TraitNamesSet) -> bool {
    if let Some(rty) = tmap.ty_to_rust_type_checked(ty) {
        for tname in traits.iter() {
            if !rty.implements.contains_path(tname) {
                return false;
            }
        }
        true
    } else {
        println!(
            "cargo:warning=mapping types: type {} unknown",
            DisplayToTokens(ty)
        );
        false
    }
}

impl TypeMapConvRuleInfoExpanderHelper for JavaContextForArg<'_, '_> {
    fn swig_i_type(&mut self, ty: &syn::Type, _opt_arg: Option<&str>) -> Result<syn::Type> {
        let rust_ty = self
            .ctx
            .conv_map
            .find_or_alloc_rust_type(ty, self.arg_ty_span.0);
        let f_info = map_type(self.ctx, &rust_ty, self.direction, self.arg_ty_span)?;
        trace!("swig_i_type return {}", f_info.base.corresponding_rust_type);
        Ok(f_info.base.corresponding_rust_type.ty.clone())
    }
    fn swig_from_rust_to_i_type(
        &mut self,
        ty: &syn::Type,
        in_var_name: &str,
        out_var_name: &str,
    ) -> Result<String> {
        let rust_ty = self
            .ctx
            .conv_map
            .find_or_alloc_rust_type(ty, self.arg_ty_span.0);
        let f_info = map_type(self.ctx, &rust_ty, Direction::Outgoing, self.arg_ty_span)?;

        let (mut conv_deps, conv_code) = self.ctx.conv_map.convert_rust_types(
            rust_ty.to_idx(),
            f_info.base.corresponding_rust_type.to_idx(),
            in_var_name,
            out_var_name,
            "#error",
            self.arg_ty_span,
        )?;
        self.ctx.rust_code.append(&mut conv_deps);
        Ok(conv_code)
    }
    fn swig_from_i_type_to_rust(
        &mut self,
        _ty: &syn::Type,
        _in_var_name: &str,
        _out_var_name: &str,
    ) -> Result<String> {
        unimplemented!();
    }
    fn swig_f_type(&mut self, ty: &syn::Type, param1: Option<&str>) -> Result<ExpandedFType> {
        let rust_ty = self
            .ctx
            .conv_map
            .find_or_alloc_rust_type(ty, self.arg_ty_span.0);
        let f_info = map_type(self.ctx, &rust_ty, self.direction, self.arg_ty_span)?;
        let fname = match param1 {
            Some("NoNullAnnotations") => {
                java_code::filter_null_annotation(f_info.base.name.display())
                    .trim()
                    .into()
            }
            None => f_info.base.name,
            Some(param) => {
                return Err(DiagnosticError::new2(
                    self.arg_ty_span,
                    format!("Invalid argument '{}' for swig_f_type", param),
                ))
            }
        };
        Ok(ExpandedFType {
            name: fname,
            provided_by_module: vec![],
        })
    }
    fn swig_foreign_to_i_type(&mut self, _ty: &syn::Type, _var_name: &str) -> Result<String> {
        unimplemented!();
    }
    fn swig_foreign_from_i_type(&mut self, ty: &syn::Type, var_name: &str) -> Result<String> {
        let rust_ty = self
            .ctx
            .conv_map
            .find_or_alloc_rust_type(ty, self.arg_ty_span.0);
        let f_info = map_type(self.ctx, &rust_ty, Direction::Outgoing, self.arg_ty_span)?;
        if let Some(java_conv) = f_info.java_converter {
            Ok(java_conv
                .converter
                .as_str()
                .replace(FROM_VAR_TEMPLATE, var_name))
        } else {
            Ok(String::new())
        }
    }
}

fn type_annotation(typename: &str) -> Option<NullAnnotation> {
    if typename.contains("@Nullable") {
        Some(NullAnnotation::Nullable)
    } else if typename.contains("@NonNull") {
        Some(NullAnnotation::NonNull)
    } else {
        None
    }
}
