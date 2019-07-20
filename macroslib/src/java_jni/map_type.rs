use log::{debug, trace};
use petgraph::Direction;
use std::rc::Rc;
use syn::Type;

use super::{
    calc_this_type_for_method, merge_rule, JavaContext, JavaConverter, JavaForeignTypeInfo,
    NullAnnotation,
};
use crate::{
    error::{DiagnosticError, Result, SourceIdSpan},
    source_registry::SourceId,
    typemap::{
        ast::{if_option_return_some_type, DisplayToTokens, TyParamsSubstList},
        ty::{ForeignType, RustType, TraitNamesSet},
        ExpandedFType, ForeignTypeInfo, MapToForeignFlag, TypeMapConvRuleInfoExpanderHelper,
        FROM_VAR_TEMPLATE, TO_VAR_TEMPLATE,
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
    if direction == Direction::Incoming {
        if let Some(fti) = special_type(ctx.conv_map, &arg_ty, arg_ty_span)? {
            return Ok(fti);
        }
    }
    let fti = do_map_type(ctx, arg_ty, direction, arg_ty_span)?;
    let ftype = &ctx.conv_map[fti];
    if !ftype.provides_by_module.is_empty() {
        unimplemented!();
    }
    let rule = match direction {
        petgraph::Direction::Outgoing => ftype.into_from_rust.as_ref(),
        petgraph::Direction::Incoming => ftype.from_into_rust.as_ref(),
    }
    .ok_or_else(|| {
        DiagnosticError::new2(
            ftype.src_id_span(),
            format!(
                "No rule to convert foreign type {} as input/output type",
                ftype.name
            ),
        )
    })?;
    let base_rt;
    let base_ft_name;
    let mut java_converter = None;
    if let Some(intermediate) = rule.intermediate.as_ref() {
        if intermediate.input_to_output {
            unimplemented!();
        }
        base_rt = intermediate.intermediate_ty;
        let typename = ftype.typename();
        let converter = intermediate.conv_code.to_string();
        let intermediate_ty = intermediate.intermediate_ty;

        let rty = ctx.conv_map[intermediate_ty].clone();
        let arg_span = intermediate.conv_code.full_span();
        let inter_ft = do_map_type(ctx, &rty, direction, arg_span)?;
        if !ctx.conv_map[inter_ft].provides_by_module.is_empty() {
            unimplemented!();
        }
        base_ft_name = typename;
        java_converter = Some(JavaConverter {
            java_transition_type: ctx.conv_map[inter_ft].typename(),
            converter,
        });
    } else {
        base_rt = rule.rust_ty;
        base_ft_name = ftype.typename();
    }
    let mut fti = JavaForeignTypeInfo {
        base: ForeignTypeInfo {
            name: base_ft_name,
            correspoding_rust_type: ctx.conv_map[base_rt].clone(),
        },
        java_converter,
        annotation: None,
    };
    if !is_primitive_type(&fti.base.name) {
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
    if let Some(ftype) = ctx.conv_map.map_through_conversation_to_foreign(
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
        if let Some(ftype) = ctx.conv_map.map_through_conversation_to_foreign(
            arg_ty.to_idx(),
            direction,
            MapToForeignFlag::FullSearch,
            arg_ty_span,
            calc_this_type_for_method,
        ) {
            return Ok(ftype);
        }
    } else if let Some(ftype) = ctx.conv_map.map_through_conversation_to_foreign(
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
                "Do not know conversation from \
                 such rust type '{}' to Java type",
                arg_ty
            ),
        )),

        Direction::Incoming => Err(DiagnosticError::new2(
            arg_ty_span,
            format!(
                "Do not know conversation from Java type \
                 to such rust type '{}'",
                arg_ty
            ),
        )),
    }
}

pub(in crate::java_jni) fn special_type(
    conv_map: &mut TypeMap,
    arg_ty: &RustType,
    arg_ty_span: SourceIdSpan,
) -> Result<Option<JavaForeignTypeInfo>> {
    trace!("special_type: check is arg.ty({}) Option", arg_ty);

    if let Some(ty) = if_option_return_some_type(arg_ty) {
        return handle_option_type_in_input(conv_map, &ty, arg_ty_span.0);
    }

    trace!("special_type: oridinary type {}", arg_ty);
    Ok(None)
}

fn handle_option_type_in_input(
    conv_map: &mut TypeMap,
    opt_inside_ty: &Type,
    arg_src_id: SourceId,
) -> Result<Option<JavaForeignTypeInfo>> {
    let opt_inside_rust_ty = conv_map.find_or_alloc_rust_type(opt_inside_ty, arg_src_id);
    if let Some(fclass) =
        conv_map.find_foreigner_class_with_such_self_type(&opt_inside_rust_ty, false)
    {
        let jlong_ti = conv_map.ty_to_rust_type(&parse_type! { jlong });
        Ok(Some(JavaForeignTypeInfo {
            base: ForeignTypeInfo {
                name: fclass.name.to_string().into(),
                correspoding_rust_type: jlong_ti,
            },
            java_converter: Some(JavaConverter {
                converter: format!(
                    r#"
        long {to_var} = 0;//TODO: use ptr::null() for corresponding constant
        if ({from_var} != null) {{
            {to_var} = {from_var}.mNativeObj;
            {from_var}.mNativeObj = 0;
        }}
"#,
                    to_var = TO_VAR_TEMPLATE,
                    from_var = FROM_VAR_TEMPLATE
                ),
                java_transition_type: "long".into(),
            }),
            annotation: Some(NullAnnotation::Nullable),
        }))
    } else {
        Ok(None)
    }
}

fn is_primitive_type(type_name: &str) -> bool {
    match type_name {
        "void" | "boolean" | "byte" | "short" | "int" | "long" | "float" | "double" => true,
        _ => false,
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
            "warning=mapping types: type {} unknown",
            DisplayToTokens(ty)
        );
        false
    }
}

impl<'a, 'b> TypeMapConvRuleInfoExpanderHelper for JavaContextForArg<'a, 'b> {
    fn swig_i_type(&mut self, _ty: &syn::Type) -> Result<syn::Type> {
        unimplemented!();
    }
    fn swig_from_rust_to_i_type(
        &mut self,
        _ty: &syn::Type,
        _in_var_name: &str,
        _out_var_name: &str,
    ) -> Result<String> {
        unimplemented!();
    }
    fn swig_from_i_type_to_rust(
        &mut self,
        _ty: &syn::Type,
        _in_var_name: &str,
        _out_var_name: &str,
    ) -> Result<String> {
        unimplemented!();
    }
    fn swig_f_type(
        &mut self,
        ty: &syn::Type,
        direction: Option<Direction>,
    ) -> Result<ExpandedFType> {
        let rust_ty = self
            .ctx
            .conv_map
            .find_or_alloc_rust_type(ty, self.arg_ty_span.0);
        let direction = direction.unwrap_or(self.direction);
        let f_info = map_type(self.ctx, &rust_ty, direction, self.arg_ty_span)?;
        let fname = f_info.base.name.clone();

        Ok(ExpandedFType {
            name: fname,
            provides_by_module: vec![],
        })
    }
    fn swig_foreign_to_i_type(&mut self, _ty: &syn::Type, _var_name: &str) -> Result<String> {
        unimplemented!();
    }
    fn swig_foreign_from_i_type(&mut self, _ty: &syn::Type, _var_name: &str) -> Result<String> {
        unimplemented!();
    }
}
