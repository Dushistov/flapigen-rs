use std::io::Write;

use log::{debug, trace};
use petgraph::Direction;
use proc_macro2::Span;
use quote::quote;
use smol_str::SmolStr;
use syn::{parse_quote, spanned::Spanned, Type};

use crate::{
    ast::{
        if_option_return_some_type, if_result_return_ok_err_types, if_type_slice_return_elem_type,
        if_vec_return_elem_type, normalize_ty_lifetimes, DisplayToTokens, RustType,
    },
    cpp::{
        cpp_code::c_class_type,
        {CppConverter, CppForeignTypeInfo},
    },
    error::{DiagnosticError, Result},
    file_cache::FileWriteCache,
    typemap::{make_unique_rust_typename, ForeignTypeInfo, FROM_VAR_TEMPLATE, TO_VAR_TEMPLATE},
    CppConfig, CppOptional, CppVariant, ForeignEnumInfo, ForeignerClassInfo, TypeMap,
};

fn special_type(
    conv_map: &mut TypeMap,
    cpp_cfg: &CppConfig,
    arg_ty: &Type,
    direction: Direction,
) -> Result<Option<CppForeignTypeInfo>> {
    trace!(
        "special_type: begin arg.ty({:?}) input {:?}",
        arg_ty,
        direction
    );

    if let Some(foreign_enum) = conv_map.is_this_exported_enum(arg_ty) {
        let converter = calc_converter_for_enum(foreign_enum);
        return Ok(Some(converter));
    }

    if *arg_ty == parse_type! { bool } {
        let fti = conv_map
            .find_foreign_type_info_by_name("char")
            .expect("expect to find `char` in type map");
        return Ok(Some(CppForeignTypeInfo {
            base: fti,
            c_converter: String::new(),
            cpp_converter: Some(CppConverter {
                typename: "bool".into(),
                input_converter: format!("{} ? 1 : 0", FROM_VAR_TEMPLATE),
                output_converter: format!("{} != 0", FROM_VAR_TEMPLATE),
            }),
        }));
    }

    if *arg_ty == parse_type! { String } && direction == Direction::Outgoing {
        let fti = conv_map
            .find_foreign_type_info_by_name("struct CRustString")
            .expect("expect to find `struct CRustString`  in type map");
        return Ok(Some(CppForeignTypeInfo {
            base: fti,
            c_converter: String::new(),
            cpp_converter: Some(CppConverter {
                typename: "RustString".into(),
                input_converter: "#error".into(),
                output_converter: format!("RustString{{{from_var}}}", from_var = FROM_VAR_TEMPLATE),
            }),
        }));
    }

    if let syn::Type::Reference(syn::TypeReference {
        elem: ref ret_ty,
        mutability: None,
        ..
    }) = arg_ty
    {
        if let Some(foreign_class) =
            conv_map.find_foreigner_class_with_such_self_type(ret_ty, false)
        {
            trace!(
                "special_type is immutable ref to foreign_class ty {:?}, foreign_class {}",
                arg_ty,
                foreign_class.name
            );
            let foreign_info =
                foreign_class_foreign_name(conv_map, foreign_class, arg_ty.span(), true)?;
            if direction == Direction::Outgoing {
                let cpp_type = format!("{}Ref", foreign_class.name);
                let output_converter = format!("{}{{{}}}", cpp_type, FROM_VAR_TEMPLATE);
                return Ok(Some(CppForeignTypeInfo {
                    base: foreign_info,
                    c_converter: String::new(),
                    cpp_converter: Some(CppConverter {
                        typename: cpp_type.into(),
                        output_converter,
                        input_converter: format!("UNREACHABLE {}", line!()),
                    }),
                }));
            } else {
                let cpp_type = format!("const {} &", foreign_class.name);
                let c_type = &foreign_info.name;
                let input_converter = format!("static_cast<{}>({})", c_type, FROM_VAR_TEMPLATE);
                return Ok(Some(CppForeignTypeInfo {
                    base: foreign_info,
                    c_converter: String::new(),
                    cpp_converter: Some(CppConverter {
                        typename: cpp_type.into(),
                        output_converter: format!("UNREACHABLE {}", line!()),
                        input_converter,
                    }),
                }));
            }
        }
    }

    if let syn::Type::Reference(syn::TypeReference {
        elem: ref ret_ty,
        mutability: Some(_),
        ..
    }) = arg_ty
    {
        if let Some(foreign_class) =
            conv_map.find_foreigner_class_with_such_self_type(ret_ty, false)
        {
            trace!(
                "special_type is mutable ref to foreign_class ty {:?}, foreign_class {}",
                arg_ty,
                foreign_class.name
            );
            let foreign_info =
                foreign_class_foreign_name(conv_map, foreign_class, arg_ty.span(), true)?;
            if direction == Direction::Outgoing {
                return Err(DiagnosticError::new(
                    arg_ty.span(),
                    format!(
                        "Not supported conversation &mut {} as return type to foreign",
                        foreign_class.name
                    ),
                ));
            } else {
                let mut_void_ptr_typename = format!(
                    "{}",
                    DisplayToTokens(&parse_type! { *mut ::std::os::raw::c_void })
                );
                let this_type = if let Some(this_type_for_method) =
                    foreign_class.this_type_for_method.as_ref()
                {
                    let this_type: RustType = this_type_for_method.clone().into();
                    this_type
                } else {
                    return Err(DiagnosticError::new(
                        arg_ty.span(),
                        format!(
                            "Unknown this type for method(s) of class {}",
                            foreign_class.name
                        ),
                    ));
                };
                let my_mut_void_ptr_ti = RustType::new(
                    parse_quote! { *mut ::std::os::raw::c_void },
                    make_unique_rust_typename(&mut_void_ptr_typename, &this_type.normalized_name),
                );
                let cpp_type = format!("{} &", foreign_class.name);
                let c_type = &foreign_info.name;
                let input_converter = format!("static_cast<{}>({})", c_type, FROM_VAR_TEMPLATE);
                return Ok(Some(CppForeignTypeInfo {
                    base: ForeignTypeInfo {
                        name: foreign_info.name,
                        correspoding_rust_type: my_mut_void_ptr_ti,
                    },
                    c_converter: String::new(),
                    cpp_converter: Some(CppConverter {
                        typename: cpp_type.into(),
                        output_converter: "#error".to_string(),
                        input_converter,
                    }),
                }));
            }
        }
    }

    if let Some(foreign_class_this_ty) = conv_map.is_ty_implements(arg_ty, "SwigForeignClass") {
        let foreign_class = conv_map
            .find_foreigner_class_with_such_this_type(&foreign_class_this_ty.ty)
            .ok_or_else(|| {
                DiagnosticError::new(
                    arg_ty.span(),
                    format!("Can not find foreigner_class for '{:?}'", arg_ty),
                )
            })?;
        trace!(
            "special_type: {:?} is foreign_class {}",
            arg_ty,
            foreign_class.name
        );
        let foreign_info =
            foreign_class_foreign_name(conv_map, foreign_class, arg_ty.span(), false)?;
        return Ok(Some(CppForeignTypeInfo {
            base: foreign_info,
            c_converter: String::new(),
            cpp_converter: Some(CppConverter {
                typename: foreign_class.name.to_string().into(),
                output_converter: format!("{}({})", foreign_class.name, FROM_VAR_TEMPLATE),
                input_converter: format!("{}.release()", FROM_VAR_TEMPLATE),
            }),
        }));
    }
    if let Some(elem_ty) = if_vec_return_elem_type(arg_ty) {
        return map_type_vec(conv_map, cpp_cfg, arg_ty, &elem_ty);
    }
    if direction == Direction::Outgoing {
        if let Some((ok_ty, err_ty)) = if_result_return_ok_err_types(arg_ty) {
            trace!(
                "special_type: return type is Result<{:?}, {:?}>",
                ok_ty,
                err_ty
            );
            return handle_result_type_as_return_type(conv_map, cpp_cfg, arg_ty, &ok_ty, &err_ty);
        }
        if let Some(ty) = if_option_return_some_type(arg_ty) {
            return handle_option_type_in_return(conv_map, cpp_cfg, arg_ty, &ty);
        }
        if let Some(elem_ty) = if_type_slice_return_elem_type(arg_ty, false) {
            return map_return_slice_type(conv_map, arg_ty, &elem_ty);
        }
    } else {
        if let Some(ty) = if_option_return_some_type(arg_ty) {
            return handle_option_type_in_input(conv_map, cpp_cfg, arg_ty, &ty);
        }
        if let Some(elem_ty) = if_type_slice_return_elem_type(arg_ty, true) {
            return map_arg_with_slice_type(conv_map, arg_ty, &elem_ty);
        }
    }

    if direction == Direction::Outgoing {
        if let syn::Type::Tuple(ref tupple) = arg_ty {
            if tupple.elems.len() == 2 {
                let mut ret = map_ordinal_result_type(conv_map, arg_ty)?;
                if let (Some(fc1), Some(fc2)) = (
                    conv_map.find_foreigner_class_with_such_this_type(&tupple.elems[0]),
                    conv_map.find_foreigner_class_with_such_this_type(&tupple.elems[1]),
                ) {
                    ret.cpp_converter = Some(CppConverter {
                        typename: format!("std::pair<{}, {}>", fc1.name, fc2.name).into(),
                        input_converter: "#error".into(),
                        output_converter: format!(
                            "std::make_pair({FirstType}{{static_cast<{CFirstType} *>({from}.first)}},
 {SecondType}{{static_cast<{CSecondType} *>({from}.second)}})",
                            FirstType = fc1.name,
                            SecondType = fc2.name,
                            from = FROM_VAR_TEMPLATE,
                            CFirstType = c_class_type(fc1),
                            CSecondType = c_class_type(fc2)
                        ),
                    });
                    return Ok(Some(ret));
                }
            }
        }
    }

    trace!("Oridinary type {:?}", arg_ty);
    Ok(None)
}

fn foreign_class_foreign_name(
    conv_map: &TypeMap,
    foreign_class: &ForeignerClassInfo,
    foreign_class_span: Span,
    readonly_fptr: bool,
) -> Result<ForeignTypeInfo> {
    let c_type = c_class_type(foreign_class);
    let foreign_typename = if readonly_fptr {
        format!("const {} *", c_type)
    } else {
        format!("{} *", c_type)
    };
    conv_map
        .find_foreign_type_info_by_name(&foreign_typename)
        .ok_or_else(|| {
            DiagnosticError::new(
                foreign_class_span,
                format!("type {} unknown", foreign_class.name),
            )
        })
}

fn calc_converter_for_enum(foreign_enum: &ForeignEnumInfo) -> CppForeignTypeInfo {
    let u32_ti: RustType = parse_type! { u32 }.into();
    let c_converter: String = r#"
        uint32_t {to_var} = {from_var};
"#
    .into();
    CppForeignTypeInfo {
        base: ForeignTypeInfo {
            name: foreign_enum.name.to_string().into(),
            correspoding_rust_type: u32_ti,
        },
        c_converter,
        cpp_converter: None,
    }
}

pub(in crate::cpp) fn map_type(
    conv_map: &mut TypeMap,
    cpp_cfg: &CppConfig,
    arg_ty: &Type,
    direction: Direction,
) -> Result<CppForeignTypeInfo> {
    let ret: CppForeignTypeInfo = match direction {
        Direction::Incoming => {
            if let Some(converter) = special_type(conv_map, cpp_cfg, arg_ty, Direction::Incoming)? {
                return Ok(converter);
            }
            map_ordinal_input_type(conv_map, arg_ty)?
        }
        Direction::Outgoing => {
            if let Some(converter) = special_type(conv_map, cpp_cfg, arg_ty, Direction::Outgoing)? {
                converter
            } else {
                map_ordinal_result_type(conv_map, arg_ty)?
            }
        }
    };
    Ok(ret)
}

fn map_ordinal_result_type(conv_map: &mut TypeMap, arg_ty: &Type) -> Result<CppForeignTypeInfo> {
    Ok(conv_map
        .map_through_conversation_to_foreign(arg_ty, Direction::Outgoing, arg_ty.span())
        .ok_or_else(|| {
            DiagnosticError::new(
                arg_ty.span(),
                format!(
                    "Do not know conversation from \
                     such rust type '{}' to foreign",
                    normalize_ty_lifetimes(arg_ty)
                ),
            )
        })?
        .into())
}

fn map_ordinal_input_type(conv_map: &mut TypeMap, arg_ty: &Type) -> Result<CppForeignTypeInfo> {
    Ok(conv_map
        .map_through_conversation_to_foreign(arg_ty, Direction::Incoming, arg_ty.span())
        .ok_or_else(|| {
            DiagnosticError::new(
                arg_ty.span(),
                format!(
                    "Do not know conversation from foreign \
                     to such rust type '{}'",
                    normalize_ty_lifetimes(arg_ty)
                ),
            )
        })?
        .into())
}

fn map_arg_with_slice_type(
    conv_map: &mut TypeMap,
    arg_ty: &Type,
    elem_ty: &Type,
) -> Result<Option<CppForeignTypeInfo>> {
    let mut ftype_info = map_ordinal_input_type(conv_map, arg_ty)?;
    if let Some(foreign_class) = conv_map.find_foreigner_class_with_such_self_type(&elem_ty, false)
    {
        let typename = format!("RustForeignSlice<{}Ref>", foreign_class.name);
        ftype_info.cpp_converter = Some(CppConverter {
            typename: typename.into(),
            input_converter: FROM_VAR_TEMPLATE.to_string(),
            output_converter: "#error".to_string(),
        });
        return Ok(Some(ftype_info));
    } else {
        Ok(None)
    }
}

fn map_return_slice_type(
    conv_map: &mut TypeMap,
    arg_ty: &Type,
    elem_ty: &Type,
) -> Result<Option<CppForeignTypeInfo>> {
    let mut ftype_info = map_ordinal_result_type(conv_map, arg_ty)?;
    if let Some(foreign_class) = conv_map.find_foreigner_class_with_such_self_type(&elem_ty, false)
    {
        let typename = format!("RustForeignSlice<{}Ref>", foreign_class.name);
        let output_converter = format!(
            "{cpp_type}{{{var}}}",
            cpp_type = typename,
            var = FROM_VAR_TEMPLATE
        );
        ftype_info.cpp_converter = Some(CppConverter {
            typename: typename.into(),
            output_converter,
            input_converter: "#error".to_string(),
        });
        return Ok(Some(ftype_info));
    } else {
        Ok(None)
    }
}

fn map_type_vec(
    conv_map: &mut TypeMap,
    cpp_cfg: &CppConfig,
    arg_ty: &Type,
    elem_ty: &Type,
) -> Result<Option<CppForeignTypeInfo>> {
    let mut ftype_info = map_ordinal_result_type(conv_map, arg_ty)?;
    if let Some(foreign_class) = conv_map.find_foreigner_class_with_such_self_type(&elem_ty, false)
    {
        let typename = format!("RustForeignVec{}", foreign_class.name);
        let fc_vec_path = cpp_cfg.output_dir.join(format!("{}.h", typename));
        let generate_cpp_part = !cpp_cfg
            .generated_helper_files
            .borrow()
            .contains(&fc_vec_path);
        if generate_cpp_part {
            trace!(
                "map_result_type_vec: we generate code for {:?}",
                fc_vec_path
            );
            let mut c_vec_f = FileWriteCache::new(&fc_vec_path);
            let free_mem_func = format!("{}_free", typename);
            let push_func = format!("{}_push", typename);
            let remove_func = format!("{}_remove", typename);
            write!(
                c_vec_f,
                r##"// Automaticaly generated by rust_swig
#pragma once

#include "rust_vec.h"

#ifdef __cplusplus
extern "C" {{
#endif
extern void {free_mem_func}(struct CRustForeignVec);
extern void {push_func}(struct CRustForeignVec *, void *);
extern void *{remove_func}(struct CRustForeignVec *, uintptr_t);
#ifdef __cplusplus

namespace {namespace_name} {{
using {vec_type} = RustForeignVec<{class}Ref, CRustForeignVec,
                                  {free_mem_func}, {push_func}, {remove_func}>;
}}
}}
#endif
"##,
                free_mem_func = free_mem_func,
                namespace_name = cpp_cfg.namespace_name,
                vec_type = typename,
                class = foreign_class.name,
                push_func = push_func,
                remove_func = remove_func,
            )
            .map_err(|err| {
                DiagnosticError::new(
                    arg_ty.span(),
                    format!("write to {:?} failed: {}", fc_vec_path, err),
                )
            })?;
            c_vec_f.update_file_if_necessary().map_err(|err| {
                DiagnosticError::new(
                    arg_ty.span(),
                    format!("update of {:?} failed: {}", fc_vec_path, err),
                )
            })?;
            cpp_cfg
                .to_generate
                .borrow_mut()
                .push(syn::parse_str(&format!(
                    r#"
#[allow(unused_variables, unused_mut, non_snake_case)]
#[no_mangle]
pub extern "C" fn {func_name}(v: CRustForeignVec) {{
    assert_eq!(::std::mem::size_of::<{self_type}>(), v.step);
    drop_foreign_class_vec::<{self_type}>(v.data as *mut {self_type}, v.len, v.capacity);
}}
"#,
                    func_name = free_mem_func,
                    self_type = foreign_class.self_type_name()
                ))?);
            cpp_cfg
                .to_generate
                .borrow_mut()
                .push(syn::parse_str(&format!(
                    r#"
#[allow(unused_variables, unused_mut, non_snake_case)]
#[no_mangle]
pub extern "C" fn {func_name}(v: *mut CRustForeignVec, e: *mut ::std::os::raw::c_void) {{
    push_foreign_class_to_vec::<{self_type}>(v, e);
}}
"#,
                    func_name = push_func,
                    self_type = foreign_class.self_type_name()
                ))?);
            cpp_cfg
                .to_generate
                .borrow_mut()
                .push(syn::parse_str(&format!(
                    r#"
#[allow(unused_variables, unused_mut, non_snake_case)]
#[no_mangle]
pub extern "C" fn {func_name}(v: *mut CRustForeignVec, idx: usize) -> *mut ::std::os::raw::c_void {{
    remove_foreign_class_from_vec::<{self_type}>(v, idx)
}}
"#,
                    func_name = remove_func,
                    self_type = foreign_class.self_type_name()
                ))?);
            cpp_cfg
                .generated_helper_files
                .borrow_mut()
                .insert(fc_vec_path);
        }
        let output_converter = format!(
            "{cpp_type}{{{var}}}",
            cpp_type = typename,
            var = FROM_VAR_TEMPLATE
        );
        ftype_info.cpp_converter = Some(CppConverter {
            typename: typename.into(),
            output_converter,
            input_converter: format!("{var}.release()", var = FROM_VAR_TEMPLATE),
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
    ftype_info.cpp_converter = Some(CppConverter {
        typename: typename.into(),
        output_converter: format!(
            "{cpp_type}{{{var}}}",
            cpp_type = typename,
            var = FROM_VAR_TEMPLATE
        ),
        input_converter: format!("{var}.release()", var = FROM_VAR_TEMPLATE),
    });
    Ok(Some(ftype_info))
}

fn handle_result_type_as_return_type(
    conv_map: &mut TypeMap,
    cpp_cfg: &CppConfig,
    arg_ty: &Type,
    ok_ty: &Type,
    err_ty: &Type,
) -> Result<Option<CppForeignTypeInfo>> {
    let err_ty_name = normalize_ty_lifetimes(&err_ty);
    debug!(
        "handle_result_type_as_return_type: ok_ty: {}",
        normalize_ty_lifetimes(&ok_ty)
    );
    if let Some(foreign_class_this_ty) = conv_map.is_ty_implements(ok_ty, "SwigForeignClass") {
        let foreign_class = conv_map
            .find_foreigner_class_with_such_this_type(&foreign_class_this_ty.ty)
            .ok_or_else(|| {
                DiagnosticError::new(
                    arg_ty.span(),
                    format!("Can not find foreigner_class for '{:?}'", arg_ty),
                )
            })?;
        let c_class = c_class_type(foreign_class);
        if err_ty_name == "String" {
            let foreign_info = conv_map
                .find_foreign_type_info_by_name("struct CResultObjectString")
                .expect("Can not find info about struct CResultObjectString");
            let typename = match cpp_cfg.cpp_variant {
                CppVariant::Std17 => format!("std::variant<{}, RustString>", foreign_class.name),
                CppVariant::Boost => format!("boost::variant<{}, RustString>", foreign_class.name),
            };
            let output_converter = format!(
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
                c_converter: String::new(),
                cpp_converter: Some(CppConverter {
                    typename: typename.into(),
                    output_converter,
                    input_converter: String::new(),
                }),
            }));
        } else if let Some(err_class) =
            conv_map.find_foreigner_class_with_such_self_type(&err_ty, false)
        {
            let foreign_info = conv_map
                .find_foreign_type_info_by_name("struct CResultObjectObject")
                .expect("Can not find info about struct CResultObjectObject");
            let c_err_class = c_class_type(err_class);
            let typename = match cpp_cfg.cpp_variant {
                CppVariant::Std17 => {
                    format!("std::variant<{}, {}>", foreign_class.name, err_class.name)
                }
                CppVariant::Boost => {
                    format!("boost::variant<{}, {}>", foreign_class.name, err_class.name)
                }
            };
            let output_converter = format!(
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
                c_converter: String::new(),
                cpp_converter: Some(CppConverter {
                    typename: typename.into(),
                    output_converter,
                    input_converter: "#error".into(),
                }),
            }));
        } else if let Some(err_enum) = conv_map.is_this_exported_enum(err_ty) {
            let foreign_info = conv_map
                .find_foreign_type_info_by_name("struct CResultObjectEnum")
                .expect("Can not find info about struct CResultObjectEnum");
            let typename = match cpp_cfg.cpp_variant {
                CppVariant::Std17 => {
                    format!("std::variant<{}, {}>", foreign_class.name, err_enum.name)
                }
                CppVariant::Boost => {
                    format!("boost::variant<{}, {}>", foreign_class.name, err_enum.name,)
                }
            };
            let output_converter = format!(
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
                c_converter: String::new(),
                cpp_converter: Some(CppConverter {
                    typename: typename.into(),
                    output_converter,
                    input_converter: String::new(),
                }),
            }));
        } else {
            return Ok(None);
        }
    }

    if let Some(elem_ty) = if_vec_return_elem_type(ok_ty) {
        trace!(
            "handle_result_type_as_return_type ok_ty is Vec, elem_ty {:?}",
            elem_ty
        );
        let vec_foreign_info = map_type(conv_map, cpp_cfg, ok_ty, Direction::Outgoing)?;
        let mut f_type_info = map_ordinal_result_type(conv_map, arg_ty)?;
        if err_ty_name == "String" {
            let foreign_name = conv_map
                .find_foreigner_class_with_such_self_type(&elem_ty, false)
                .map(|v| v.name.clone());
            if let Some(foreign_name) = foreign_name {
                let ok_typename = format!("RustForeignVec{}", foreign_name);
                let typename = match cpp_cfg.cpp_variant {
                    CppVariant::Std17 => format!("std::variant<{}, RustString>", ok_typename),
                    CppVariant::Boost => format!("boost::variant<{}, RustString>", ok_typename),
                };
                let output_converter = format!(
                    "{var}.is_ok != 0 ?
 {VarType}{{{Type}{{{var}.data.ok}}}} :
 {VarType}{{RustString{{{var}.data.err}}}}",
                    VarType = typename,
                    Type = ok_typename,
                    var = FROM_VAR_TEMPLATE,
                );
                f_type_info.cpp_converter = Some(CppConverter {
                    typename: typename.into(),
                    output_converter,
                    input_converter: "#error".to_string(),
                });
                return Ok(Some(f_type_info));
            } else {
                return Ok(None);
            }
        } else if let Some(err_class) =
            conv_map.find_foreigner_class_with_such_self_type(&err_ty, false)
        {
            // Result<Vec<T>, Err>
            let foreign_name = conv_map
                .find_foreigner_class_with_such_self_type(&elem_ty, false)
                .map(|v| v.name.clone());
            if let Some(foreign_name) = foreign_name {
                let ok_typename = format!("RustForeignVec{}", foreign_name);
                let c_err_class = c_class_type(err_class);
                let typename = match cpp_cfg.cpp_variant {
                    CppVariant::Std17 => {
                        format!("std::variant<{}, {}>", ok_typename, err_class.name)
                    }
                    CppVariant::Boost => {
                        format!("boost::variant<{}, {}>", ok_typename, err_class.name)
                    }
                };
                let output_converter = format!(
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
                    output_converter,
                    input_converter: "#error".to_string(),
                });
                return Ok(Some(f_type_info));
            } else {
                if let Some(cpp_conv) = vec_foreign_info.cpp_converter.as_ref() {
                    trace!(
                        "handle_result_type_as_return_type: Result<Vec<Not class, but C++>, class>"
                    );
                    let ok_typename = &cpp_conv.typename;
                    let c_err_class = c_class_type(err_class);
                    let typename = match cpp_cfg.cpp_variant {
                        CppVariant::Std17 => {
                            format!("std::variant<{}, {}>", ok_typename, err_class.name)
                        }
                        CppVariant::Boost => {
                            format!("boost::variant<{}, {}>", ok_typename, err_class.name)
                        }
                    };
                    let output_converter = format!(
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
                        output_converter,
                        input_converter: "#error".to_string(),
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
            handle_result_with_primitive_type_as_ok_ty(conv_map, cpp_cfg, arg_ty, ok_ty, err_ty)
        } else {
            Ok(None)
        }
    }
}

fn handle_option_type_in_input(
    conv_map: &mut TypeMap,
    cpp_cfg: &CppConfig,
    arg_ty: &Type,
    opt_ty: &Type,
) -> Result<Option<CppForeignTypeInfo>> {
    if let Some(fclass) = conv_map.find_foreigner_class_with_such_self_type(opt_ty, false) {
        let foreign_info = foreign_class_foreign_name(conv_map, fclass, opt_ty.span(), false)?;
        let (typename, input_converter) = match cpp_cfg.cpp_optional {
            CppOptional::Std17 => (
                format!("std::optional<{}>", fclass.name),
                format!(
                    " !!{var} ? {var}->release() : nullptr",
                    var = FROM_VAR_TEMPLATE,
                ),
            ),
            CppOptional::Boost => (
                format!("boost::optional<{}>", fclass.name),
                format!(
                    " !!{var} ? {var}->release() : nullptr",
                    var = FROM_VAR_TEMPLATE,
                ),
            ),
        };
        return Ok(Some(CppForeignTypeInfo {
            base: foreign_info,
            c_converter: String::new(),
            cpp_converter: Some(CppConverter {
                typename: typename.into(),
                output_converter: "#error".to_string(),
                input_converter,
            }),
        }));
    }

    if let Type::Reference(syn::TypeReference {
        elem: ref ref_ty,

        mutability: None,
        ..
    }) = opt_ty
    {
        if let Type::Path(syn::TypePath { ref path, .. }) = **ref_ty {
            if path.segments.len() == 1 && path.segments[0].ident == "str" {
                trace!("Catch Option<&str>");
                let mut cpp_info_opt = map_ordinal_input_type(conv_map, arg_ty)?;
                let cpp_info_ty = map_ordinal_input_type(conv_map, opt_ty)?;
                let f_opt_ty = cpp_info_ty.base.name;
                let (typename, input_converter) = match cpp_cfg.cpp_optional {
                    CppOptional::Std17 => (
                        format!("std::optional<{}>", f_opt_ty),
                        format!("!!{var} ? *{var} : nullptr", var = FROM_VAR_TEMPLATE,),
                    ),
                    CppOptional::Boost => (
                        format!("boost::optional<{}>", f_opt_ty),
                        format!("!!{var} ? *{var} : nullptr", var = FROM_VAR_TEMPLATE,),
                    ),
                };
                cpp_info_opt.cpp_converter = Some(CppConverter {
                    typename: typename.into(),
                    output_converter: "#error".to_string(),
                    input_converter,
                });
                return Ok(Some(cpp_info_opt));
            }
        }
    }
    trace!("handle_option_type_in_input arg_ty {:?}", arg_ty);
    let mut cpp_info_opt = map_ordinal_input_type(conv_map, arg_ty)?;
    let cpp_info_ty = map_ordinal_input_type(conv_map, opt_ty)?;
    let f_opt_ty = cpp_info_ty.base.name;
    let mut c_option_name: &str = &cpp_info_opt.base.name;
    if c_option_name.starts_with("struct ") {
        c_option_name = &c_option_name[7..];
    }
    let conv: &'static str = if conv_map.is_this_exported_enum(opt_ty).is_some() {
        "static_cast<uint32_t>"
    } else {
        ""
    };
    let (typename, input_converter) = match cpp_cfg.cpp_optional {
        CppOptional::Std17 => (
            format!("std::optional<{}>", f_opt_ty),
            format!(
                "!!{var} ? {CType}{{{conv}(*{var}), 1}} : c_option_empty<{CType}>()",
                CType = c_option_name,
                var = FROM_VAR_TEMPLATE,
                conv = conv,
            ),
        ),
        CppOptional::Boost => (
            format!("boost::optional<{}>", f_opt_ty),
            format!(
                "!!{var} ? {CType}{{{conv}(*{var}), 1}} : c_option_empty<{CType}>()",
                CType = c_option_name,
                var = FROM_VAR_TEMPLATE,
                conv = conv,
            ),
        ),
    };
    cpp_info_opt.cpp_converter = Some(CppConverter {
        typename: typename.into(),
        output_converter: "#error".to_string(),
        input_converter,
    });
    Ok(Some(cpp_info_opt))
}

fn handle_option_type_in_return(
    conv_map: &mut TypeMap,
    cpp_cfg: &CppConfig,
    arg_ty: &Type,
    opt_ty: &Type,
) -> Result<Option<CppForeignTypeInfo>> {
    if let Some(foreign_class_this_ty) = conv_map.is_ty_implements_exact(opt_ty, "SwigForeignClass")
    {
        let foreign_class = conv_map
            .find_foreigner_class_with_such_this_type(&foreign_class_this_ty.ty)
            .ok_or_else(|| {
                DiagnosticError::new(
                    arg_ty.span(),
                    format!("Can not find foreigner_class for '{:?}'", arg_ty),
                )
            })?;
        let foreign_info =
            foreign_class_foreign_name(conv_map, foreign_class, opt_ty.span(), false)?;
        let (typename, output_converter) = match cpp_cfg.cpp_optional {
            CppOptional::Std17 => (
                format!("std::optional<{}>", foreign_class.name),
                format!(
                    "{var} != nullptr ? {Type}({var}) : std::optional<{Type}>()",
                    Type = foreign_class.name,
                    var = FROM_VAR_TEMPLATE,
                ),
            ),
            CppOptional::Boost => (
                format!("boost::optional<{}>", foreign_class.name),
                format!(
                    "{var} != nullptr ? {Type}({var}) : boost::optional<{Type}>()",
                    Type = foreign_class.name,
                    var = FROM_VAR_TEMPLATE,
                ),
            ),
        };
        return Ok(Some(CppForeignTypeInfo {
            base: foreign_info,
            c_converter: String::new(),
            cpp_converter: Some(CppConverter {
                typename: typename.into(),
                output_converter,
                input_converter: "#error".to_string(),
            }),
        }));
    }

    //handle Option<&ForeignClass> case
    if let Type::Reference(syn::TypeReference {
        elem: ref under_ref_ty,
        mutability: None,
        ..
    }) = opt_ty
    {
        if let Some(fclass) = conv_map
            .find_foreigner_class_with_such_self_type(under_ref_ty, false)
            .cloned()
        {
            let foreign_info =
                foreign_class_foreign_name(conv_map, &fclass, under_ref_ty.span(), false)?;
            let this_type_for_method = fclass.this_type_for_method.as_ref().ok_or_else(|| {
                DiagnosticError::new(
                    fclass.span(),
                    format!(
                        "Class {} (namespace {}) return as reference, but there is no constructor",
                        fclass.name, cpp_cfg.namespace_name,
                    ),
                )
            })?;
            let this_type: RustType = this_type_for_method.clone().into();
            let void_ptr_ty = parse_type! { *mut ::std::os::raw::c_void };
            let void_ptr_typename = format!("{}", DisplayToTokens(&void_ptr_ty));
            let my_void_ptr_ti = RustType::new(
                void_ptr_ty,
                make_unique_rust_typename(&void_ptr_typename, &this_type.normalized_name),
            );
            let arg_rust_ty: RustType = arg_ty.clone().into();
            conv_map.add_type(arg_rust_ty.clone());
            conv_map.add_conversation_rule(
                arg_rust_ty,
                my_void_ptr_ti,
                format!(
                    r#"
    let {to_var}: *mut ::std::os::raw::c_void = match {from_var} {{
        Some(x) => x as *const {self_type} as *mut ::std::os::raw::c_void,
        None => ::std::ptr::null_mut(),
    }};
"#,
                    to_var = TO_VAR_TEMPLATE,
                    from_var = FROM_VAR_TEMPLATE,
                    self_type = this_type.normalized_name,
                )
                .into(),
            );

            let (typename, output_converter) = match cpp_cfg.cpp_optional {
                CppOptional::Std17 => (
                    format!("std::optional<{}Ref>", fclass.name),
                    format!(
                        "{var} != nullptr ? {Type}Ref({var}) : std::optional<{Type}Ref>()",
                        Type = fclass.name,
                        var = FROM_VAR_TEMPLATE,
                    ),
                ),
                CppOptional::Boost => (
                    format!("boost::optional<{}Ref>", fclass.name),
                    format!(
                        "{var} != nullptr ? {Type}Ref({var}) : boost::optional<{Type}Ref>()",
                        Type = fclass.name,
                        var = FROM_VAR_TEMPLATE,
                    ),
                ),
            };
            return Ok(Some(CppForeignTypeInfo {
                base: foreign_info,
                c_converter: String::new(),
                cpp_converter: Some(CppConverter {
                    typename: typename.into(),
                    output_converter,
                    input_converter: "#error".to_string(),
                }),
            }));
        }
    }
    let mut cpp_info_opt = map_ordinal_result_type(conv_map, arg_ty)?;
    let cpp_info_ty = map_ordinal_result_type(conv_map, opt_ty)?;

    let f_opt_ty = if *opt_ty != parse_type! {bool} {
        cpp_info_ty.base.name
    } else {
        "bool".into()
    };
    debug!("is_this_exported_enum {:?}", opt_ty);
    let (typename, output_converter) =
        if let Some(foreign_enum) = conv_map.is_this_exported_enum(opt_ty) {
            match cpp_cfg.cpp_optional {
                CppOptional::Std17 => (
                    format!("std::optional<{}>", f_opt_ty),
                    format!(
                        "{var}.is_some ? static_cast<{EnumType}>({var}.val)
 : std::optional<{Type}>()",
                        Type = f_opt_ty,
                        var = FROM_VAR_TEMPLATE,
                        EnumType = foreign_enum.name,
                    ),
                ),
                CppOptional::Boost => (
                    format!("boost::optional<{}>", f_opt_ty),
                    format!(
                        "{var}.is_some ? static_cast<{EnumType}>({var}.val)
 : boost::optional<{Type}>()",
                        Type = f_opt_ty,
                        var = FROM_VAR_TEMPLATE,
                        EnumType = foreign_enum.name,
                    ),
                ),
            }
        } else {
            match cpp_cfg.cpp_optional {
                CppOptional::Std17 => (
                    format!("std::optional<{}>", f_opt_ty),
                    format!(
                        "{var}.is_some ? {var}.val : std::optional<{Type}>()",
                        Type = f_opt_ty,
                        var = FROM_VAR_TEMPLATE,
                    ),
                ),
                CppOptional::Boost => (
                    format!("boost::optional<{}>", f_opt_ty),
                    format!(
                        "{var}.is_some ? {var}.val : boost::optional<{Type}>()",
                        Type = f_opt_ty,
                        var = FROM_VAR_TEMPLATE,
                    ),
                ),
            }
        };
    if let Type::Path(syn::TypePath { ref path, .. }) = opt_ty {
        if path.segments.len() == 1 && path.segments[0].ident == "String" {
            trace!("Catch return of Option<String>");
            let cpp_info_ty = special_type(conv_map, cpp_cfg, opt_ty, Direction::Outgoing)?;
            let cpp_typename = cpp_info_ty
                .expect("We should know how convert String as output type")
                .cpp_converter
                .expect("C++ converter from C struct")
                .typename;
            let (typename, output_converter) = match cpp_cfg.cpp_optional {
                CppOptional::Std17 => (
                    format!("std::optional<{}>", cpp_typename),
                    format!(
                        "{var}.is_some ? {ty}{{{var}.val}} : std::optional<{ty}>()",
                        var = FROM_VAR_TEMPLATE,
                        ty = cpp_typename
                    ),
                ),
                CppOptional::Boost => (
                    format!("boost::optional<{}>", cpp_typename),
                    format!(
                        "{var}.is_some ? {ty}{{{var}.val}} : boost::optional<{ty}>()",
                        var = FROM_VAR_TEMPLATE,
                        ty = cpp_typename
                    ),
                ),
            };
            cpp_info_opt.cpp_converter = Some(CppConverter {
                typename: typename.into(),
                output_converter,
                input_converter: "#error".to_string(),
            });
            return Ok(Some(cpp_info_opt));
        }
    }
    cpp_info_opt.cpp_converter = Some(CppConverter {
        typename: typename.into(),
        output_converter,
        input_converter: "#error".to_string(),
    });
    Ok(Some(cpp_info_opt))
}

fn handle_result_with_primitive_type_as_ok_ty(
    conv_map: &mut TypeMap,
    cpp_cfg: &CppConfig,
    arg_ty: &Type,
    ok_ty: &Type,
    err_ty: &Type,
) -> Result<Option<CppForeignTypeInfo>> {
    let empty_ok_ty = *ok_ty == parse_type! { () };
    let c_ok_type_name: SmolStr = if empty_ok_ty {
        "void *".into()
    } else {
        map_ordinal_result_type(conv_map, ok_ty)?.base.name
    };

    if *err_ty == parse_type! { String } {
        let typename = match cpp_cfg.cpp_variant {
            CppVariant::Std17 => format!("std::variant<{}, RustString>", c_ok_type_name),
            CppVariant::Boost => format!("boost::variant<{}, RustString>", c_ok_type_name),
        };
        let output_converter = format!(
            "{var}.is_ok != 0 ?
 {VarType}{{{var}.data.ok}} :
 {VarType}{{RustString{{{var}.data.err}}}}",
            VarType = typename,
            var = FROM_VAR_TEMPLATE
        );
        let foreign_info = map_ordinal_result_type(conv_map, arg_ty)?;
        if empty_ok_ty {
            assert_eq!(foreign_info.base.name, "struct CResultObjectString");
        }
        Ok(Some(CppForeignTypeInfo {
            base: foreign_info.base,
            c_converter: String::new(),
            cpp_converter: Some(CppConverter {
                typename: typename.into(),
                output_converter,
                input_converter: String::new(),
            }),
        }))
    } else if let Some(err_class) =
        conv_map.find_foreigner_class_with_such_self_type(&err_ty, false)
    {
        let c_err_class = c_class_type(err_class);
        let typename = match cpp_cfg.cpp_variant {
            CppVariant::Std17 => format!("std::variant<{}, {}>", c_ok_type_name, err_class.name),
            CppVariant::Boost => format!("boost::variant<{}, {}>", c_ok_type_name, err_class.name),
        };
        let output_converter = format!(
            "{var}.is_ok != 0 ?
 {VarType} {{ {var}.data.ok }} :
 {VarType} {{ {ErrType}(static_cast<{C_ErrType} *>({var}.data.err)) }}",
            VarType = typename,
            var = FROM_VAR_TEMPLATE,
            ErrType = err_class.name,
            C_ErrType = c_err_class,
        );
        let foreign_info = map_ordinal_result_type(conv_map, arg_ty)?;
        if empty_ok_ty {
            assert_eq!(foreign_info.base.name, "struct CResultObjectObject");
        }
        Ok(Some(CppForeignTypeInfo {
            base: foreign_info.base,
            c_converter: String::new(),
            cpp_converter: Some(CppConverter {
                typename: typename.into(),
                output_converter,
                input_converter: "#error".into(),
            }),
        }))
    } else {
        Ok(None)
    }
}
