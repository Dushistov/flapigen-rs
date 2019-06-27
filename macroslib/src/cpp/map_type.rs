use std::{io::Write, rc::Rc};

use log::{debug, trace};
use petgraph::Direction;
use proc_macro2::Span;
use quote::ToTokens;
use smol_str::SmolStr;
use syn::{parse_quote, spanned::Spanned, Type};

use crate::{
    cpp::{
        cpp_code::{c_class_type, cpp_header_name, cpp_header_name_for_enum},
        merge_c_types, merge_rule, CppContext, CppConverter, CppForeignTypeInfo, MergeCTypesFlags,
    },
    error::{panic_on_syn_error, DiagnosticError, Result, SourceIdSpan},
    file_cache::FileWriteCache,
    typemap::{
        ast::{
            if_result_return_ok_err_types, if_type_slice_return_elem_type, if_vec_return_elem_type,
            TyParamsSubstList,
        },
        ty::RustType,
        ExpandedFType, MapToForeignFlag, TypeMapConvRuleInfoExpanderHelper, FROM_VAR_TEMPLATE,
    },
    types::ForeignerClassInfo,
    CppVariant, TypeMap,
};

fn special_type(
    ctx: &mut CppContext,
    arg_ty: &RustType,
    direction: Direction,
    arg_ty_span: SourceIdSpan,
) -> Result<Option<CppForeignTypeInfo>> {
    trace!(
        "special_type: begin arg.ty({}) direction {:?}",
        arg_ty,
        direction
    );

    if let Some(elem_ty) = if_vec_return_elem_type(arg_ty) {
        return map_type_vec(ctx, arg_ty, &elem_ty, arg_ty_span, direction);
    }
    if direction == Direction::Outgoing {
        if let Some((ok_ty, err_ty)) = if_result_return_ok_err_types(arg_ty) {
            trace!(
                "special_type: return type is Result<{:?}, {:?}>",
                ok_ty,
                err_ty
            );
            return handle_result_type_as_return_type(ctx, arg_ty, &ok_ty, &err_ty, arg_ty_span);
        }
        if let Some(elem_ty) = if_type_slice_return_elem_type(&arg_ty.ty, false) {
            return map_return_slice_type(ctx, arg_ty, &elem_ty, arg_ty_span);
        }
    } else {
        if let Some(elem_ty) = if_type_slice_return_elem_type(&arg_ty.ty, true) {
            return map_arg_with_slice_type(ctx, arg_ty, &elem_ty, arg_ty_span);
        }
    }

    trace!("special_type: Oridinary type {}", arg_ty);
    Ok(None)
}

pub(in crate::cpp) fn map_type(
    ctx: &mut CppContext,
    arg_ty: &RustType,
    direction: Direction,
    arg_ty_span: SourceIdSpan,
) -> Result<CppForeignTypeInfo> {
    let ret: CppForeignTypeInfo = match direction {
        Direction::Incoming => {
            if let Some(converter) = special_type(ctx, arg_ty, Direction::Incoming, arg_ty_span)? {
                return Ok(converter);
            }
            map_ordinal_type(ctx, arg_ty, arg_ty_span, Direction::Incoming)?
        }
        Direction::Outgoing => {
            if let Some(converter) = special_type(ctx, arg_ty, Direction::Outgoing, arg_ty_span)? {
                converter
            } else {
                map_ordinal_type(ctx, arg_ty, arg_ty_span, Direction::Outgoing)?
            }
        }
    };
    Ok(ret)
}

fn map_ordinal_type(
    ctx: &mut CppContext,
    arg_ty: &RustType,
    arg_ty_span: SourceIdSpan,
    direction: Direction,
) -> Result<CppForeignTypeInfo> {
    if let Some(ftype) = ctx.conv_map.map_through_conversation_to_foreign(
        arg_ty,
        direction,
        MapToForeignFlag::FastSearch,
        arg_ty_span,
        calc_this_type_for_method,
    ) {
        return CppForeignTypeInfo::try_new(ctx.conv_map, direction, ftype);
    }

    let idx_subst_map: Option<(Rc<_>, TyParamsSubstList)> =
        ctx.conv_map.generic_rules().iter().find_map(|grule| {
            grule
                .is_ty_subst_of_my_generic_rtype(&arg_ty.ty, direction)
                .map(|sm| (grule.clone(), sm.into()))
        });
    if let Some((grule, subst_list)) = idx_subst_map {
        debug!(
            "map_ordinal_type: we found generic rule for {}: {:?}",
            arg_ty, subst_list
        );
        let subst_map = subst_list.as_slice().into();
        let c_types = grule.subst_generic_params_to_c_types(
            &subst_map,
            &mut CppContextForArg {
                ctx,
                arg_ty_span,
                direction,
            },
        )?;
        if let Some(c_types) = c_types {
            merge_c_types(
                ctx,
                c_types,
                MergeCTypesFlags::DefineAlsoRustType,
                grule.src_id,
            )?;
        }

        let new_rule = grule.subst_generic_params(
            subst_map,
            direction,
            &mut CppContextForArg {
                ctx,
                arg_ty_span,
                direction,
            },
        )?;
        merge_rule(ctx, new_rule)?;
        if let Some(ftype) = ctx.conv_map.map_through_conversation_to_foreign(
            arg_ty,
            direction,
            MapToForeignFlag::FullSearch,
            arg_ty_span,
            calc_this_type_for_method,
        ) {
            return CppForeignTypeInfo::try_new(ctx.conv_map, direction, ftype);
        }
    } else {
        if let Some(ftype) = ctx.conv_map.map_through_conversation_to_foreign(
            arg_ty,
            direction,
            MapToForeignFlag::FullSearch,
            arg_ty_span,
            calc_this_type_for_method,
        ) {
            return CppForeignTypeInfo::try_new(ctx.conv_map, direction, ftype);
        }
    }
    match direction {
        Direction::Outgoing => Err(DiagnosticError::new2(
            arg_ty_span,
            format!(
                "Do not know conversation from \
                 such rust type '{}' to C++ type",
                arg_ty
            ),
        )),

        Direction::Incoming => Err(DiagnosticError::new2(
            arg_ty_span,
            format!(
                "Do not know conversation from C++ type \
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

impl<'a, 'b> TypeMapConvRuleInfoExpanderHelper for CppContextForArg<'a, 'b> {
    fn swig_i_type(&mut self, ty: &syn::Type) -> Result<syn::Type> {
        let rust_ty = self
            .ctx
            .conv_map
            .find_or_alloc_rust_type(ty, self.arg_ty_span.0);
        let f_info = map_ordinal_type(self.ctx, &rust_ty, self.arg_ty_span, self.direction)?;
        trace!("swig_i_type return {}", f_info.base.correspoding_rust_type);
        Ok(f_info.base.correspoding_rust_type.ty.clone())
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
        let f_info = map_ordinal_type(self.ctx, &rust_ty, self.arg_ty_span, Direction::Outgoing)?;

        let (mut conv_deps, conv_code) = self.ctx.conv_map.convert_rust_types(
            rust_ty.to_idx(),
            f_info.base.correspoding_rust_type.to_idx(),
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
        let f_info = map_ordinal_type(self.ctx, &rust_ty, self.arg_ty_span, Direction::Incoming)?;

        let (mut conv_deps, conv_code) = self.ctx.conv_map.convert_rust_types(
            f_info.base.correspoding_rust_type.to_idx(),
            rust_ty.to_idx(),
            in_var_name,
            out_var_name,
            "#error",
            self.arg_ty_span,
        )?;
        self.ctx.rust_code.append(&mut conv_deps);
        Ok(conv_code)
    }
    fn swig_f_type(&mut self, ty: &syn::Type) -> Result<ExpandedFType> {
        let rust_ty = self
            .ctx
            .conv_map
            .find_or_alloc_rust_type(ty, self.arg_ty_span.0);
        let f_info = map_ordinal_type(self.ctx, &rust_ty, self.arg_ty_span, self.direction)?;
        let fname = if let Some(ref cpp_conv) = f_info.cpp_converter {
            cpp_conv.typename.as_str()
        } else {
            f_info.base.name.as_str()
        };
        Ok(ExpandedFType {
            name: fname.replace("struct ", "").replace("union ", "").into(),
            provides_by_module: f_info.provides_by_module.clone(),
        })
    }
    fn swig_foreign_to_i_type(&mut self, ty: &syn::Type, var_name: &str) -> Result<String> {
        let rust_ty = self
            .ctx
            .conv_map
            .find_or_alloc_rust_type(ty, self.arg_ty_span.0);
        let f_info = map_ordinal_type(self.ctx, &rust_ty, self.arg_ty_span, Direction::Incoming)?;
        if let Some(cpp_conv) = f_info.cpp_converter {
            Ok(cpp_conv.converter.replace(FROM_VAR_TEMPLATE, var_name))
        } else {
            Ok(var_name.into())
        }
    }
    fn swig_foreign_from_i_type(&mut self, ty: &syn::Type, var_name: &str) -> Result<String> {
        let rust_ty = self
            .ctx
            .conv_map
            .find_or_alloc_rust_type(ty, self.arg_ty_span.0);
        let f_info = map_ordinal_type(self.ctx, &rust_ty, self.arg_ty_span, Direction::Outgoing)?;
        if let Some(cpp_conv) = f_info.cpp_converter {
            Ok(cpp_conv.converter.replace(FROM_VAR_TEMPLATE, var_name))
        } else {
            Ok(var_name.into())
        }
    }
}

fn map_arg_with_slice_type(
    ctx: &mut CppContext,
    arg_ty: &RustType,
    elem_ty: &Type,
    arg_ty_span: SourceIdSpan,
) -> Result<Option<CppForeignTypeInfo>> {
    let mut ftype_info = map_ordinal_type(ctx, arg_ty, arg_ty_span, Direction::Incoming)?;
    let elem_rust_ty = ctx.conv_map.find_or_alloc_rust_type(elem_ty, arg_ty_span.0);
    if let Some(foreign_class) = ctx
        .conv_map
        .find_foreigner_class_with_such_self_type(&elem_rust_ty, false)
    {
        let typename = format!("RustForeignSlice<{}Ref>", foreign_class.name);
        ftype_info.cpp_converter = Some(CppConverter {
            typename: typename.into(),
            converter: FROM_VAR_TEMPLATE.to_string(),
        });
        return Ok(Some(ftype_info));
    } else {
        Ok(None)
    }
}

fn map_return_slice_type(
    ctx: &mut CppContext,
    arg_ty: &RustType,
    elem_ty: &Type,
    arg_ty_span: SourceIdSpan,
) -> Result<Option<CppForeignTypeInfo>> {
    let mut ftype_info = map_ordinal_type(ctx, arg_ty, arg_ty_span, Direction::Outgoing)?;
    let elem_rust_ty = ctx.conv_map.find_or_alloc_rust_type(elem_ty, arg_ty_span.0);
    if let Some(foreign_class) = ctx
        .conv_map
        .find_foreigner_class_with_such_self_type(&elem_rust_ty, false)
    {
        let typename = format!("RustForeignSlice<{}Ref>", foreign_class.name);
        let converter = format!(
            "{cpp_type}{{{var}}}",
            cpp_type = typename,
            var = FROM_VAR_TEMPLATE
        );
        ftype_info.cpp_converter = Some(CppConverter {
            typename: typename.into(),
            converter,
        });
        return Ok(Some(ftype_info));
    } else {
        Ok(None)
    }
}

fn map_type_vec(
    ctx: &mut CppContext,
    arg_ty: &RustType,
    elem_ty: &Type,
    arg_ty_span: SourceIdSpan,
    direction: Direction,
) -> Result<Option<CppForeignTypeInfo>> {
    let mut ftype_info = map_ordinal_type(ctx, arg_ty, arg_ty_span, Direction::Outgoing)?;
    let elem_rust_ty = ctx.conv_map.find_or_alloc_rust_type(elem_ty, arg_ty_span.0);
    if let Some(foreign_class) = ctx
        .conv_map
        .find_foreigner_class_with_such_self_type(&elem_rust_ty, false)
    {
        let typename = format!("RustForeignVec{}", foreign_class.name);
        let vec_module_name: SmolStr = format!("{}.h", typename).into();

        if ctx.common_files.get(&vec_module_name).is_none() {
            debug!(
                "map_result_type_vec: we generate code for {}",
                vec_module_name
            );
            let module_name = &vec_module_name;
            let common_files = &mut ctx.common_files;
            let c_vec_f = file_for_module!(ctx, common_files, module_name);
            let free_mem_func = format!("{}_free", typename);
            let push_func = format!("{}_push", typename);
            let remove_func = format!("{}_remove", typename);
            write!(
                c_vec_f,
                r##"
#include "rust_vec.h"

#ifdef __cplusplus
extern "C" {{
#endif
extern void {free_mem_func}(struct CRustForeignVec);
extern void {push_func}(struct CRustForeignVec *, void *);
extern void *{remove_func}(struct CRustForeignVec *, uintptr_t);
#ifdef __cplusplus
}} // extern "C"

namespace {namespace_name} {{
using {vec_type} = RustForeignVec<{class}Ref, CRustForeignVec,
                                  {free_mem_func}, {push_func}, {remove_func}>;
}}
#endif
"##,
                free_mem_func = free_mem_func,
                namespace_name = ctx.cfg.namespace_name,
                vec_type = typename,
                class = foreign_class.name,
                push_func = push_func,
                remove_func = remove_func,
            )
            .map_err(|err| {
                DiagnosticError::new2(
                    arg_ty_span,
                    format!("write to {} failed: {}", vec_module_name, err),
                )
            })?;

            let self_rust_ty = ctx
                .conv_map
                .find_or_alloc_rust_type(&foreign_class.self_type_as_ty(), foreign_class.src_id);

            let func_id = syn::Ident::new(&free_mem_func, Span::call_site());
            let self_type_id: Type =
                syn::parse_str(&self_rust_ty.normalized_name).unwrap_or_else(|err| {
                    panic_on_syn_error(
                        "c++/internal self_rust_ty",
                        self_rust_ty.normalized_name.clone().into(),
                        err,
                    )
                });

            let free_vec_func_item: syn::Item = parse_quote! {
            #[allow(unused_variables, unused_mut, non_snake_case, unused_unsafe)]
            #[no_mangle]
            pub extern "C" fn #func_id(v: CRustForeignVec) {
                assert_eq!(::std::mem::size_of::<#self_type_id>(), v.step);
                drop_foreign_class_vec::<#self_type_id>(v.data as *mut #self_type_id, v.len, v.capacity);
            }
            };
            ctx.rust_code.push(free_vec_func_item.into_token_stream());

            let func_id = syn::Ident::new(&push_func, Span::call_site());
            let push_vec_func_item: syn::Item = parse_quote! {
            #[allow(unused_variables, unused_mut, non_snake_case, unused_unsafe)]
            #[no_mangle]
            pub extern "C" fn #func_id(v: *mut CRustForeignVec, e: *mut ::std::os::raw::c_void) {
                push_foreign_class_to_vec::<#self_type_id>(v, e);
            }
                        };
            ctx.rust_code.push(push_vec_func_item.into_token_stream());

            let func_id = syn::Ident::new(&remove_func, Span::call_site());
            let remove_elem_func_item: syn::Item = parse_quote! {
            #[allow(unused_variables, unused_mut, non_snake_case, unused_unsafe)]
            #[no_mangle]
            pub extern "C" fn #func_id(v: *mut CRustForeignVec, idx: usize) -> *mut ::std::os::raw::c_void {
                remove_foreign_class_from_vec::<#self_type_id>(v, idx)
            }
            };
            ctx.rust_code
                .push(remove_elem_func_item.into_token_stream());
        }
        let converter = match direction {
            Direction::Outgoing => format!(
                "{cpp_type}{{{var}}}",
                cpp_type = typename,
                var = FROM_VAR_TEMPLATE
            ),
            Direction::Incoming => format!("{var}.release()", var = FROM_VAR_TEMPLATE),
        };
        ftype_info.cpp_converter = Some(CppConverter {
            typename: typename.into(),
            converter,
        });
        return Ok(Some(ftype_info));
    }
    let typename = match ftype_info
        .base
        .correspoding_rust_type
        .normalized_name
        .as_str()
    {
        "CRustVecU8" => "RustVecU8",
        "CRustVecI32" => "RustVecI32",
        "CRustVecU32" => "RustVecU32",
        "CRustVecUsize" => "RustVecUsize",
        "CRustVecF32" => "RustVecF32",
        "CRustVecF64" => "RustVecF64",
        _ => unimplemented!(),
    };
    let converter = match direction {
        Direction::Outgoing => format!(
            "{cpp_type}{{{var}}}",
            cpp_type = typename,
            var = FROM_VAR_TEMPLATE
        ),
        Direction::Incoming => format!("{var}.release()", var = FROM_VAR_TEMPLATE),
    };
    ftype_info.cpp_converter = Some(CppConverter {
        typename: typename.into(),
        converter,
    });
    Ok(Some(ftype_info))
}

fn handle_result_type_as_return_type(
    ctx: &mut CppContext,
    arg_ty: &RustType,
    ok_ty: &Type,
    err_ty: &Type,
    arg_ty_span: SourceIdSpan,
) -> Result<Option<CppForeignTypeInfo>> {
    let err_rust_ty = ctx.conv_map.find_or_alloc_rust_type(err_ty, arg_ty_span.0);
    let ok_rust_ty = ctx.conv_map.find_or_alloc_rust_type(ok_ty, arg_ty_span.0);
    debug!(
        "handle_result_type_as_return_type: ok_ty: {:?}, err_ty: {}",
        ok_rust_ty, err_rust_ty
    );
    if let Some(foreign_class_this_ty) = ctx
        .conv_map
        .is_ty_implements(&ok_rust_ty, "SwigForeignClass")
    {
        let foreign_class = ctx
            .conv_map
            .find_foreigner_class_with_such_this_type(
                &foreign_class_this_ty.ty,
                calc_this_type_for_method,
            )
            .ok_or_else(|| {
                DiagnosticError::new2(
                    arg_ty_span,
                    format!("Can not find foreigner_class for '{:?}'", arg_ty),
                )
            })?;
        let c_class = c_class_type(foreign_class);
        if err_rust_ty.normalized_name == "String" {
            let foreign_info = ctx
                .conv_map
                .find_foreign_type_info_by_name("struct CResultObjectString")
                .expect("Can not find info about struct CResultObjectString");
            let (typename, var_include) = match ctx.cfg.cpp_variant {
                CppVariant::Std17 => (
                    format!("std::variant<{}, RustString>", foreign_class.name),
                    "<variant>".into(),
                ),
                CppVariant::Boost => (
                    format!("boost::variant<{}, RustString>", foreign_class.name),
                    "<boost/variant.hpp>".into(),
                ),
            };
            let converter = format!(
                "{var}.is_ok != 0 ?
 {VarType}{{{Type}(static_cast<{C_Type} *>({var}.data.ok))}} :
 {VarType}{{RustString{{{var}.data.err}}}}",
                VarType = typename,
                Type = foreign_class.name,
                C_Type = c_class,
                var = FROM_VAR_TEMPLATE,
            );
            return Ok(Some(CppForeignTypeInfo {
                base: foreign_info,
                provides_by_module: vec![
                    "\"rust_result.h\"".into(),
                    "\"rust_str.h\"".into(),
                    format!("\"{}\"", cpp_header_name(foreign_class)).into(),
                    var_include,
                ],
                cpp_converter: Some(CppConverter {
                    typename: typename.into(),
                    converter,
                }),
            }));
        } else if let Some(err_class) = ctx
            .conv_map
            .find_foreigner_class_with_such_self_type(&err_rust_ty, false)
        {
            let foreign_info = ctx
                .conv_map
                .find_foreign_type_info_by_name("struct CResultObjectObject")
                .expect("Can not find info about struct CResultObjectObject");
            let c_err_class = c_class_type(err_class);
            let (typename, var_inc) = match ctx.cfg.cpp_variant {
                CppVariant::Std17 => (
                    format!("std::variant<{}, {}>", foreign_class.name, err_class.name),
                    "<variant>".into(),
                ),
                CppVariant::Boost => (
                    format!("boost::variant<{}, {}>", foreign_class.name, err_class.name),
                    "<boost/variant.hpp>".into(),
                ),
            };
            let converter = format!(
                "{var}.is_ok != 0 ?
 {VarType} {{ {Type}(static_cast<{C_Type} *>({var}.data.ok))}} :
 {VarType} {{ {ErrType}(static_cast<{C_ErrType} *>({var}.data.err))}}",
                VarType = typename,
                Type = foreign_class.name,
                C_Type = c_class,
                var = FROM_VAR_TEMPLATE,
                ErrType = err_class.name,
                C_ErrType = c_err_class,
            );
            return Ok(Some(CppForeignTypeInfo {
                base: foreign_info,
                provides_by_module: vec![
                    "\"rust_result.h\"".into(),
                    format!("\"{}\"", cpp_header_name(foreign_class)).into(),
                    format!("\"{}\"", cpp_header_name(err_class)).into(),
                    var_inc,
                ],
                cpp_converter: Some(CppConverter {
                    typename: typename.into(),
                    converter,
                }),
            }));
        } else if let Some(err_enum) = ctx.conv_map.is_this_exported_enum(&err_rust_ty) {
            let foreign_info = ctx
                .conv_map
                .find_foreign_type_info_by_name("struct CResultObjectEnum")
                .expect("Can not find info about struct CResultObjectEnum");

            let (typename, var_inc) = match ctx.cfg.cpp_variant {
                CppVariant::Std17 => (
                    format!("std::variant<{}, {}>", foreign_class.name, err_enum.name),
                    "<variant>".into(),
                ),
                CppVariant::Boost => (
                    format!("boost::variant<{}, {}>", foreign_class.name, err_enum.name,),
                    "<boost/variant.hpp>".into(),
                ),
            };
            let converter = format!(
                "{var}.is_ok != 0 ?
 {VarType}{{{Type}(static_cast<{C_Type} *>({var}.data.ok))}} :
 {VarType}{{static_cast<{EnumName}>({var}.data.err)}}",
                VarType = typename,
                Type = foreign_class.name,
                C_Type = c_class,
                var = FROM_VAR_TEMPLATE,
                EnumName = err_enum.name,
            );
            return Ok(Some(CppForeignTypeInfo {
                base: foreign_info,
                provides_by_module: vec![
                    "\"rust_result.h\"".into(),
                    format!("\"{}\"", cpp_header_name(foreign_class)).into(),
                    format!("\"{}\"", cpp_header_name_for_enum(err_enum)).into(),
                    var_inc,
                ],
                cpp_converter: Some(CppConverter {
                    typename: typename.into(),
                    converter,
                }),
            }));
        } else {
            return Ok(None);
        }
    }

    if let Some(elem_ty) = if_vec_return_elem_type(&ok_rust_ty) {
        let elem_rust_ty = ctx
            .conv_map
            .find_or_alloc_rust_type(&elem_ty, arg_ty_span.0);
        trace!(
            "handle_result_type_as_return_type ok_ty is Vec, elem_ty {}",
            elem_rust_ty
        );
        let vec_foreign_info = map_type(
            ctx,
            &ok_rust_ty,
            Direction::Outgoing,
            (ok_rust_ty.src_id, ok_ty.span()),
        )?;
        let mut f_type_info = map_ordinal_type(ctx, arg_ty, arg_ty_span, Direction::Outgoing)?;
        if err_rust_ty.normalized_name == "String" {
            let foreign_name = ctx
                .conv_map
                .find_foreigner_class_with_such_self_type(&elem_rust_ty, false)
                .map(|v| v.name.clone());
            if let Some(foreign_name) = foreign_name {
                let ok_typename = format!("RustForeignVec{}", foreign_name);
                let (typename, var_inc) = match ctx.cfg.cpp_variant {
                    CppVariant::Std17 => (
                        format!("std::variant<{}, RustString>", ok_typename),
                        "<variant>".into(),
                    ),
                    CppVariant::Boost => (
                        format!("boost::variant<{}, RustString>", ok_typename),
                        "<boost/variant.hpp>".into(),
                    ),
                };
                let converter = format!(
                    "{var}.is_ok != 0 ?
 {VarType}{{{Type}{{{var}.data.ok}}}} :
 {VarType}{{RustString{{{var}.data.err}}}}",
                    VarType = typename,
                    Type = ok_typename,
                    var = FROM_VAR_TEMPLATE,
                );
                f_type_info.cpp_converter = Some(CppConverter {
                    typename: typename.into(),
                    converter,
                });
                f_type_info.provides_by_module = vec!["\"rust_str.h\"".into(), var_inc];
                return Ok(Some(f_type_info));
            } else {
                return Ok(None);
            }
        } else if let Some(err_class) = ctx
            .conv_map
            .find_foreigner_class_with_such_self_type(&err_rust_ty, false)
        {
            // Result<Vec<T>, Err>
            let foreign_name = ctx
                .conv_map
                .find_foreigner_class_with_such_self_type(&elem_rust_ty, false)
                .map(|v| v.name.clone());
            if let Some(foreign_name) = foreign_name {
                let ok_typename = format!("RustForeignVec{}", foreign_name);
                let c_err_class = c_class_type(err_class);
                let (typename, var_inc) = match ctx.cfg.cpp_variant {
                    CppVariant::Std17 => (
                        format!("std::variant<{}, {}>", ok_typename, err_class.name),
                        "<variant>".into(),
                    ),

                    CppVariant::Boost => (
                        format!("boost::variant<{}, {}>", ok_typename, err_class.name),
                        "<boost/variant.hpp>".into(),
                    ),
                };
                let converter = format!(
                    "{var}.is_ok != 0 ?
 {VarType} {{ {Type}{{{var}.data.ok}} }} :
 {VarType} {{ {ErrType}(static_cast<{C_ErrType} *>({var}.data.err)) }}",
                    VarType = typename,
                    Type = ok_typename,
                    var = FROM_VAR_TEMPLATE,
                    ErrType = err_class.name,
                    C_ErrType = c_err_class,
                );
                f_type_info.cpp_converter = Some(CppConverter {
                    typename: typename.into(),
                    converter,
                });
                f_type_info.provides_by_module = vec![var_inc];
                return Ok(Some(f_type_info));
            } else {
                if let Some(cpp_conv) = vec_foreign_info.cpp_converter.as_ref() {
                    trace!(
                        "handle_result_type_as_return_type: Result<Vec<Not class, but C++>, class>"
                    );
                    let ok_typename = &cpp_conv.typename;
                    let c_err_class = c_class_type(err_class);
                    let typename = match ctx.cfg.cpp_variant {
                        CppVariant::Std17 => {
                            format!("std::variant<{}, {}>", ok_typename, err_class.name)
                        }
                        CppVariant::Boost => {
                            format!("boost::variant<{}, {}>", ok_typename, err_class.name)
                        }
                    };
                    let converter = format!(
                        "{var}.is_ok != 0 ?
 {VarType} {{ {Type}{{{var}.data.ok}} }} :
 {VarType} {{ {ErrType}(static_cast<{C_ErrType} *>({var}.data.err)) }}",
                        VarType = typename,
                        Type = ok_typename,
                        var = FROM_VAR_TEMPLATE,
                        ErrType = err_class.name,
                        C_ErrType = c_err_class,
                    );
                    f_type_info.cpp_converter = Some(CppConverter {
                        typename: typename.into(),
                        converter,
                    });
                    return Ok(Some(f_type_info));
                }
                return Ok(None);
            }
        } else {
            return Ok(None);
        }
    } else {
        trace!("return result, but not foreign_class / Vec<foreign_class>");
        if *ok_ty == parse_type! { () } || *ok_ty == parse_type! { i64 } {
            handle_result_with_primitive_type_as_ok_ty(ctx, arg_ty, ok_ty, err_ty, arg_ty_span)
        } else {
            Ok(None)
        }
    }
}

fn handle_result_with_primitive_type_as_ok_ty(
    ctx: &mut CppContext,
    arg_ty: &RustType,
    ok_ty: &Type,
    err_ty: &Type,
    arg_ty_span: SourceIdSpan,
) -> Result<Option<CppForeignTypeInfo>> {
    let empty_ok_ty = *ok_ty == parse_type! { () };

    let err_rust_ty = ctx.conv_map.find_or_alloc_rust_type(err_ty, arg_ty_span.0);

    let c_ok_type_name: SmolStr = if empty_ok_ty {
        "void *".into()
    } else {
        let ok_rust_ty = ctx.conv_map.find_or_alloc_rust_type(ok_ty, arg_ty_span.0);
        map_ordinal_type(ctx, &ok_rust_ty, arg_ty_span, Direction::Outgoing)?
            .base
            .name
    };

    if err_rust_ty.normalized_name == "String" {
        let typename = match ctx.cfg.cpp_variant {
            CppVariant::Std17 => format!("std::variant<{}, RustString>", c_ok_type_name),
            CppVariant::Boost => format!("boost::variant<{}, RustString>", c_ok_type_name),
        };
        let converter = format!(
            "{var}.is_ok != 0 ?
 {VarType}{{{var}.data.ok}} :
 {VarType}{{RustString{{{var}.data.err}}}}",
            VarType = typename,
            var = FROM_VAR_TEMPLATE
        );
        let foreign_info = map_ordinal_type(ctx, arg_ty, arg_ty_span, Direction::Outgoing)?;
        if empty_ok_ty {
            assert_eq!(foreign_info.base.name, "struct CResultObjectString");
        }
        Ok(Some(CppForeignTypeInfo {
            provides_by_module: vec!["\"rust_result.h\"".into(), "\"rust_str.h\"".into()],
            base: foreign_info.base,
            cpp_converter: Some(CppConverter {
                typename: typename.into(),
                converter,
            }),
        }))
    } else if let Some(err_class) = ctx
        .conv_map
        .find_foreigner_class_with_such_self_type(&err_rust_ty, false)
    {
        let c_err_class = c_class_type(err_class);
        let typename = match ctx.cfg.cpp_variant {
            CppVariant::Std17 => format!("std::variant<{}, {}>", c_ok_type_name, err_class.name),
            CppVariant::Boost => format!("boost::variant<{}, {}>", c_ok_type_name, err_class.name),
        };
        let converter = format!(
            "{var}.is_ok != 0 ?
 {VarType} {{ {var}.data.ok }} :
 {VarType} {{ {ErrType}(static_cast<{C_ErrType} *>({var}.data.err)) }}",
            VarType = typename,
            var = FROM_VAR_TEMPLATE,
            ErrType = err_class.name,
            C_ErrType = c_err_class,
        );
        let err_cpp_header: SmolStr = format!("\"{}\"", cpp_header_name(err_class)).into();
        let foreign_info = map_ordinal_type(ctx, arg_ty, arg_ty_span, Direction::Outgoing)?;
        if empty_ok_ty {
            assert_eq!(foreign_info.base.name, "struct CResultObjectObject");
        }
        Ok(Some(CppForeignTypeInfo {
            base: foreign_info.base,
            provides_by_module: vec!["\"rust_result.h\"".into(), err_cpp_header],
            cpp_converter: Some(CppConverter {
                typename: typename.into(),
                converter,
            }),
        }))
    } else {
        Ok(None)
    }
}

pub(in crate::cpp) fn calc_this_type_for_method(
    _: &TypeMap,
    class: &ForeignerClassInfo,
) -> Option<Type> {
    class
        .self_desc
        .as_ref()
        .map(|x| x.constructor_ret_type.clone())
}
