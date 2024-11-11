use std::rc::Rc;

use log::{debug, trace, warn};
use petgraph::Direction;
use syn::Type;

use crate::{
    cpp::{merge_c_types, merge_rule, CppContext, CppForeignTypeInfo, MergeCItemsFlags},
    error::{DiagnosticError, Result, SourceIdSpan},
    typemap::{
        ast::{DisplayToTokens, TyParamsSubstList, UniqueName},
        ty::{ForeignType, RustType, TraitNamesSet},
        ExpandedFType, MapToForeignFlag, TypeMapConvRuleInfoExpanderHelper, FROM_VAR_TEMPLATE,
    },
    types::ForeignClassInfo,
    TypeMap,
};

pub(in crate::cpp) fn map_type(
    ctx: &mut CppContext,
    arg_ty: &RustType,
    direction: Direction,
    arg_ty_span: SourceIdSpan,
) -> Result<CppForeignTypeInfo> {
    debug!("map_type: arg_ty {}, direction {:?}", arg_ty, direction);
    let ftype = do_map_type(ctx, arg_ty, direction, arg_ty_span)?;
    CppForeignTypeInfo::try_new(ctx, direction, ftype)
}

fn do_map_type(
    ctx: &mut CppContext,
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
        let subst_map = subst_list.as_slice().into();
        let c_types = grule
            .subst_generic_params_to_c_items(
                &subst_map,
                &mut CppContextForArg {
                    ctx,
                    arg_ty_span,
                    direction,
                },
            )
            .map_err(|err| {
                err.add_span_note(
                    (grule.src_id, grule.span),
                    "subst. of generic params in define_c_type failed",
                )
            })?;
        if let Some(c_types) = c_types {
            merge_c_types(
                ctx,
                c_types,
                MergeCItemsFlags::DefineAlsoRustType,
                grule.src_id,
            )?;
        }
        let new_rule = grule
            .subst_generic_params(
                subst_map,
                direction,
                &mut CppContextForArg {
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
                 such rust type '{}' to C++ type",
                arg_ty
            ),
        )),

        Direction::Incoming => Err(DiagnosticError::new2(
            arg_ty_span,
            format!(
                "Do not know conversion from C++ type \
                 to such rust type '{}'",
                arg_ty
            ),
        )),
    }
}

struct CppContextForArg<'a, 'b> {
    ctx: &'a mut CppContext<'b>,
    arg_ty_span: SourceIdSpan,
    direction: Direction,
}

impl CppContextForArg<'_, '_> {
    fn arg_direction(&self, param1: Option<&str>) -> Result<Direction> {
        match param1 {
            Some("output") => Ok(Direction::Outgoing),
            Some("input") => Ok(Direction::Incoming),
            None => Ok(self.direction),
            Some(param) => Err(DiagnosticError::new2(
                self.arg_ty_span,
                format!("Invalid argument '{param}' for swig_f_type"),
            )),
        }
    }
}

impl TypeMapConvRuleInfoExpanderHelper for CppContextForArg<'_, '_> {
    fn swig_i_type(&mut self, ty: &syn::Type, opt_arg: Option<&str>) -> Result<syn::Type> {
        let rust_ty = self
            .ctx
            .conv_map
            .find_or_alloc_rust_type(ty, self.arg_ty_span.0);
        let direction = self.arg_direction(opt_arg)?;
        let f_info = map_type(self.ctx, &rust_ty, direction, self.arg_ty_span)?;
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
        ty: &syn::Type,
        in_var_name: &str,
        out_var_name: &str,
    ) -> Result<String> {
        let rust_ty = self
            .ctx
            .conv_map
            .find_or_alloc_rust_type(ty, self.arg_ty_span.0);
        let f_info = map_type(self.ctx, &rust_ty, Direction::Incoming, self.arg_ty_span)?;

        let (mut conv_deps, conv_code) = self.ctx.conv_map.convert_rust_types(
            f_info.base.corresponding_rust_type.to_idx(),
            rust_ty.to_idx(),
            in_var_name,
            out_var_name,
            "#error",
            self.arg_ty_span,
        )?;
        self.ctx.rust_code.append(&mut conv_deps);
        Ok(conv_code)
    }
    fn swig_f_type(&mut self, ty: &syn::Type, param1: Option<&str>) -> Result<ExpandedFType> {
        let rust_ty = self
            .ctx
            .conv_map
            .find_or_alloc_rust_type(ty, self.arg_ty_span.0);

        let direction = self.arg_direction(param1)?;
        let f_info = map_type(self.ctx, &rust_ty, direction, self.arg_ty_span)?;
        let fname = if let Some(ref cpp_conv) = f_info.cpp_converter {
            &cpp_conv.typename
        } else {
            &f_info.base.name
        };
        Ok(ExpandedFType {
            name: UniqueName::new(
                fname.value().replace("struct ", "").replace("union ", ""),
                fname.unique_prefix().unwrap_or(""),
            ),
            provided_by_module: f_info.provided_by_module.clone(),
        })
    }
    fn swig_foreign_to_i_type(&mut self, ty: &syn::Type, var_name: &str) -> Result<String> {
        let rust_ty = self
            .ctx
            .conv_map
            .find_or_alloc_rust_type(ty, self.arg_ty_span.0);
        let f_info = map_type(self.ctx, &rust_ty, Direction::Incoming, self.arg_ty_span)?;
        if let Some(cpp_conv) = f_info.cpp_converter {
            Ok(cpp_conv
                .converter
                .as_str()
                .replace(FROM_VAR_TEMPLATE, var_name))
        } else {
            Ok(var_name.into())
        }
    }
    fn swig_foreign_from_i_type(&mut self, ty: &syn::Type, var_name: &str) -> Result<String> {
        let rust_ty = self
            .ctx
            .conv_map
            .find_or_alloc_rust_type(ty, self.arg_ty_span.0);
        let f_info = map_type(self.ctx, &rust_ty, Direction::Outgoing, self.arg_ty_span)?;
        if let Some(cpp_conv) = f_info.cpp_converter {
            Ok(cpp_conv
                .converter
                .as_str()
                .replace(FROM_VAR_TEMPLATE, var_name))
        } else {
            Ok(var_name.into())
        }
    }
}

pub(in crate::cpp) fn calc_this_type_for_method(
    _: &TypeMap,
    class: &ForeignClassInfo,
) -> Option<Type> {
    class
        .self_desc
        .as_ref()
        .map(|x| x.constructor_ret_type.clone())
}

fn is_ty_implement_traits(tmap: &TypeMap, ty: &syn::Type, traits: &TraitNamesSet) -> bool {
    if let Some(rty) = tmap.ty_to_rust_type_checked(ty) {
        for tname in traits.iter() {
            if tname.is_ident("SwigTypeIsReprC") {
                if tmap
                    .find_foreign_type_related_to_rust_ty(rty.to_idx())
                    .is_none()
                {
                    return false;
                }
            } else if !rty.implements.contains_path(tname) {
                return false;
            }
        }
        true
    } else {
        warn!(
            "is_ty_implement_traits: type {} unknown",
            DisplayToTokens(ty)
        );
        false
    }
}

pub(in crate::cpp) fn map_repr_c_type(
    ctx: &mut CppContext,
    rty: &RustType,
    arg_ty_span: SourceIdSpan,
) -> Result<CppForeignTypeInfo> {
    debug!("map_repr_c_type: rty {}", rty);
    let fti = map_type(
        ctx,
        rty,
        Direction::Incoming, /*not important*/
        arg_ty_span,
    )?;

    if fti.cpp_converter.is_some() || fti.base.corresponding_rust_type.to_idx() != rty.to_idx() {
        return Err(DiagnosticError::new2(
            arg_ty_span,
            format!(
                "we map {} to {}, but this is not repr(C) type",
                rty, fti.base.name
            ),
        ));
    }

    Ok(fti)
}
