use petgraph::Direction;
use std::io::Write;
use syntex_pos::{Span, DUMMY_SP};
use syntex_syntax::ast;
use syntex_syntax::parse::{PResult, ParseSess};
use syntex_syntax::symbol::Symbol;

use cpp::cpp_code::c_class_type;
use cpp::{CppConverter, CppForeignTypeInfo};
use errors::fatal_error;
use file_cache::FileWriteCache;
use my_ast::{code_to_item, if_option_return_some_type, if_result_return_ok_err_types,
             if_vec_return_elem_type, normalized_ty_string, parse_ty, RustType};
use types_conv_map::{make_unique_rust_typename, ForeignTypeInfo, FROM_VAR_TEMPLATE,
                     TO_VAR_TEMPLATE};
use {CppConfig, CppOptional, CppVariant, ForeignEnumInfo, ForeignerClassInfo, TypesConvMap};

fn special_type<'a>(
    sess: &'a ParseSess,
    conv_map: &mut TypesConvMap,
    cpp_cfg: &CppConfig,
    arg_ty: &ast::Ty,
    direction: Direction,
) -> PResult<'a, Option<CppForeignTypeInfo>> {
    trace!(
        "special_type: begin arg.ty({:?}) input {:?}",
        arg_ty,
        direction
    );

    if let Some(foreign_enum) = conv_map.is_this_exported_enum(arg_ty) {
        let converter = calc_converter_for_enum(foreign_enum);
        return Ok(Some(converter));
    }

    let ty_name = normalized_ty_string(arg_ty);
    if ty_name == "bool" {
        let fti = conv_map
            .find_foreign_type_info_by_name(Symbol::intern("char"))
            .expect("expect find char in type map");
        return Ok(Some(CppForeignTypeInfo {
            base: fti,
            c_converter: String::new(),
            cpp_converter: Some(CppConverter {
                typename: Symbol::intern("bool"),
                input_converter: format!("{} ? 1 : 0", FROM_VAR_TEMPLATE),
                output_converter: format!("{} != 0", FROM_VAR_TEMPLATE),
            }),
        }));
    }

    if let ast::TyKind::Rptr(
        _,
        ast::MutTy {
            ty: ref ret_ty,
            mutbl: ast::Mutability::Immutable,
        },
    ) = arg_ty.node
    {
        if let Some(foreign_class) =
            conv_map.find_foreigner_class_with_such_self_type(ret_ty, false)
        {
            let foreign_info =
                foreign_class_foreign_name(sess, conv_map, foreign_class, arg_ty.span, true)?;
            if direction == Direction::Outgoing {
                let cpp_type = Symbol::intern(&format!("{}Ref", foreign_class.name));
                return Ok(Some(CppForeignTypeInfo {
                    base: foreign_info,
                    c_converter: String::new(),
                    cpp_converter: Some(CppConverter {
                        typename: cpp_type,
                        output_converter: format!("{}{{{}}}", cpp_type, FROM_VAR_TEMPLATE),
                        input_converter: "UNREACHABLE".to_string(),
                    }),
                }));
            } else {
                let cpp_type = Symbol::intern(&format!("const {} &", foreign_class.name));
                let c_type = foreign_info.name;
                return Ok(Some(CppForeignTypeInfo {
                    base: foreign_info,
                    c_converter: String::new(),
                    cpp_converter: Some(CppConverter {
                        typename: cpp_type,
                        output_converter: "UNREACHABLE".to_string(),
                        input_converter: format!("static_cast<{}>({})", c_type, FROM_VAR_TEMPLATE),
                    }),
                }));
            }
        }
    }

    if let Some(foreign_class) = conv_map.find_foreigner_class_with_such_self_type(arg_ty, false) {
        trace!(
            "special_type: {:?} is foreign_class {}",
            arg_ty,
            foreign_class.name
        );
        let foreign_info =
            foreign_class_foreign_name(sess, conv_map, foreign_class, arg_ty.span, false)?;
        return Ok(Some(CppForeignTypeInfo {
            base: foreign_info,
            c_converter: String::new(),
            cpp_converter: Some(CppConverter {
                typename: foreign_class.name,
                output_converter: format!("{}({})", foreign_class.name, FROM_VAR_TEMPLATE),
                input_converter: format!("{}.release()", FROM_VAR_TEMPLATE),
            }),
        }));
    }

    if direction == Direction::Outgoing {
        if let Some((ok_ty, err_ty)) = if_result_return_ok_err_types(arg_ty) {
            trace!(
                "special_type: return type is Result<{:?}, {:?}>",
                ok_ty,
                err_ty
            );
            return handle_result_type_in_result(sess, conv_map, cpp_cfg, arg_ty, &ok_ty, &err_ty);
        }
        if let Some(ty) = if_option_return_some_type(arg_ty) {
            return handle_option_type_in_return(sess, conv_map, cpp_cfg, arg_ty, &ty);
        }
    } else {
        if let Some(ty) = if_option_return_some_type(arg_ty) {
            return handle_option_type_in_input(sess, conv_map, cpp_cfg, arg_ty, &ty);
        }
    }

    if direction == Direction::Outgoing {
        if let Some(elem_ty) = if_vec_return_elem_type(arg_ty) {
            return map_return_type_vec(sess, conv_map, cpp_cfg, arg_ty, elem_ty);
        }
    }

    trace!("Oridinary type {:?}", arg_ty);
    Ok(None)
}

fn foreign_class_foreign_name<'a>(
    sess: &'a ParseSess,
    conv_map: &TypesConvMap,
    foreign_class: &ForeignerClassInfo,
    foreign_class_span: Span,
    readonly_fptr: bool,
) -> PResult<'a, ForeignTypeInfo> {
    let c_type = c_class_type(foreign_class);
    let foreign_typename = Symbol::intern(&if readonly_fptr {
        format!("const {} *", c_type)
    } else {
        format!("{} *", c_type)
    });
    conv_map
        .find_foreign_type_info_by_name(foreign_typename)
        .ok_or_else(|| {
            fatal_error(
                sess,
                foreign_class_span,
                &format!("type {} unknown", foreign_class.name),
            )
        })
}

fn calc_converter_for_enum(foreign_enum: &ForeignEnumInfo) -> CppForeignTypeInfo {
    let sess = ParseSess::new();
    let u32_ti: RustType = parse_ty(&sess, DUMMY_SP, Symbol::intern("u32"))
        .unwrap()
        .into();
    let c_converter: String = r#"
        uint32_t {to_var} = {from_var};
"#.into();
    CppForeignTypeInfo {
        base: ForeignTypeInfo {
            name: foreign_enum.name,
            correspoding_rust_type: u32_ti,
        },
        c_converter,
        cpp_converter: None,
    }
}

pub(in cpp) fn map_type<'a>(
    sess: &'a ParseSess,
    conv_map: &mut TypesConvMap,
    cpp_cfg: &CppConfig,
    arg_ty: &ast::Ty,
    direction: Direction,
) -> PResult<'a, CppForeignTypeInfo> {
    let ret: CppForeignTypeInfo = match direction {
        Direction::Incoming => {
            if let Some(converter) =
                special_type(sess, conv_map, cpp_cfg, arg_ty, Direction::Incoming)?
            {
                return Ok(converter);
            }
            map_ordinal_input_type(sess, conv_map, arg_ty)?
        }
        Direction::Outgoing => {
            if let Some(converter) =
                special_type(sess, conv_map, cpp_cfg, arg_ty, Direction::Outgoing)?
            {
                converter
            } else {
                map_ordinal_result_type(sess, conv_map, arg_ty)?
            }
        }
    };
    Ok(ret)
}

fn map_ordinal_result_type<'a>(
    sess: &'a ParseSess,
    conv_map: &mut TypesConvMap,
    arg_ty: &ast::Ty,
) -> PResult<'a, CppForeignTypeInfo> {
    Ok(conv_map
        .map_through_conversation_to_foreign(arg_ty, Direction::Outgoing, arg_ty.span)
        .ok_or_else(|| {
            fatal_error(
                sess,
                arg_ty.span,
                &format!(
                    "Do not know conversation from \
                     such rust type '{}' to foreign",
                    normalized_ty_string(arg_ty)
                ),
            )
        })?
        .into())
}

fn map_ordinal_input_type<'a>(
    sess: &'a ParseSess,
    conv_map: &mut TypesConvMap,
    arg_ty: &ast::Ty,
) -> PResult<'a, CppForeignTypeInfo> {
    Ok(conv_map
        .map_through_conversation_to_foreign(arg_ty, Direction::Incoming, arg_ty.span)
        .ok_or_else(|| {
            fatal_error(
                sess,
                arg_ty.span,
                &format!(
                    "Do not know conversation from foreign \
                     to such rust type '{}'",
                    normalized_ty_string(arg_ty)
                ),
            )
        })?
        .into())
}

fn map_return_type_vec<'a>(
    sess: &'a ParseSess,
    conv_map: &mut TypesConvMap,
    cpp_cfg: &CppConfig,
    arg_ty: &ast::Ty,
    elem_ty: ast::Ty,
) -> PResult<'a, Option<CppForeignTypeInfo>> {
    let mut ftype_info = map_ordinal_result_type(sess, conv_map, arg_ty)?;
    if let Some(foreign_class) = conv_map.find_foreigner_class_with_such_self_type(&elem_ty, false)
    {
        let typename = Symbol::intern(&format!("RustForeignVec{}", foreign_class.name));
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
            write!(
                c_vec_f,
                r##"// Automaticaly generated by rust_swig
#pragma once

#include "rust_vec.h"

#ifdef __cplusplus
extern "C" {{
#endif
void {free_mem_func}(struct CRustForeignVec);
void {push_func}(struct CRustForeignVec *, void *);
#ifdef __cplusplus

namespace {namespace_name} {{
using {vec_type} = RustForeignVec<{class}Ref, CRustForeignVec, {free_mem_func}, {push_func}>;
}}
}}
#endif
"##,
                free_mem_func = free_mem_func,
                namespace_name = cpp_cfg.namespace_name,
                vec_type = typename,
                class = foreign_class.name,
                push_func = push_func,
            ).map_err(|err| {
                fatal_error(
                    sess,
                    arg_ty.span,
                    &format!("write to {:?} failed: {}", fc_vec_path, err),
                )
            })?;
            c_vec_f.update_file_if_necessary().map_err(|err| {
                fatal_error(
                    sess,
                    arg_ty.span,
                    &format!("update of {:?} failed: {}", fc_vec_path, err),
                )
            })?;
            cpp_cfg.to_generate.borrow_mut().append(&mut code_to_item(
                sess,
                &free_mem_func,
                &format!(
                    r#"
#[allow(unused_variables, unused_mut, non_snake_case)]
#[no_mangle]
pub extern "C" fn {func_name}(v: CRustForeignVec) {{
    assert_eq!(::std::mem::size_of::<{self_type}>(), v.step);
    drop_foreign_class_vec::<{self_type}>(v.data as *mut {self_type}, v.len, v.capacity);
}}
"#,
                    func_name = free_mem_func,
                    self_type = foreign_class.self_type
                ),
            )?);
            cpp_cfg.to_generate.borrow_mut().append(&mut code_to_item(
                sess,
                &free_mem_func,
                &format!(
                    r#"
#[allow(unused_variables, unused_mut, non_snake_case)]
#[no_mangle]
pub extern "C" fn {func_name}(v: *mut CRustForeignVec, e: *mut ::std::os::raw::c_void) {{
    push_foreign_class_to_vec::<{self_type}>(v, e);
}}
"#,
                    func_name = push_func,
                    self_type = foreign_class.self_type
                ),
            )?);
            cpp_cfg
                .generated_helper_files
                .borrow_mut()
                .insert(fc_vec_path);
        }
        ftype_info.cpp_converter = Some(CppConverter {
            typename,
            output_converter: format!(
                "{cpp_type}{{{var}}}",
                cpp_type = typename,
                var = FROM_VAR_TEMPLATE
            ),
            input_converter: "#error".to_string(),
        });
        return Ok(Some(ftype_info));
    }
    let typename = Symbol::intern(match &*ftype_info
        .base
        .correspoding_rust_type
        .normalized_name
        .as_str()
    {
        "CRustVecU8" => "RustVecU8",
        "CRustVecU32" => "RustVecU32",
        "CRustVecF32" => "RustVecF32",
        "CRustVecF64" => "RustVecF64",
        _ => unimplemented!(),
    });
    ftype_info.cpp_converter = Some(CppConverter {
        typename,
        output_converter: format!(
            "{cpp_type}{{{var}}}",
            cpp_type = typename,
            var = FROM_VAR_TEMPLATE
        ),
        input_converter: "#error".to_string(),
    });
    Ok(Some(ftype_info))
}

fn handle_result_type_in_result<'a>(
    sess: &'a ParseSess,
    conv_map: &mut TypesConvMap,
    cpp_cfg: &CppConfig,
    arg_ty: &ast::Ty,
    ok_ty: &ast::Ty,
    err_ty: &ast::Ty,
) -> PResult<'a, Option<CppForeignTypeInfo>> {
    let err_ty_name = normalized_ty_string(&err_ty);
    if let Some(foreign_class) = conv_map.find_foreigner_class_with_such_self_type(&ok_ty, false) {
        let c_class = c_class_type(foreign_class);
        if err_ty_name == "String" {
            let foreign_info = conv_map
                .find_foreign_type_info_by_name(Symbol::intern("struct CResultObjectString"))
                .expect("Can not find info about struct CResultObjectString");
            let typename = match cpp_cfg.cpp_variant {
                CppVariant::Std17 => {
                    Symbol::intern(&format!("std::variant<{}, RustString>", foreign_class.name))
                }
                CppVariant::Boost => Symbol::intern(&format!(
                    "boost::variant<{}, RustString>",
                    foreign_class.name
                )),
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
                    typename,
                    output_converter,
                    input_converter: String::new(),
                }),
            }));
        } else if let Some(err_class) =
            conv_map.find_foreigner_class_with_such_self_type(&err_ty, false)
        {
            let foreign_info = conv_map
                .find_foreign_type_info_by_name(Symbol::intern("struct CResultObjectObject"))
                .expect("Can not find info about struct CResultObjectObject");
            let c_err_class = c_class_type(err_class);
            let typename = match cpp_cfg.cpp_variant {
                CppVariant::Std17 => Symbol::intern(&format!(
                    "std::variant<{}, {}>",
                    foreign_class.name, err_class.name
                )),
                CppVariant::Boost => Symbol::intern(&format!(
                    "boost::variant<{}, {}>",
                    foreign_class.name, err_class.name
                )),
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
                    typename,
                    output_converter,
                    input_converter: "#error".into(),
                }),
            }));
        } else {
            unimplemented!();
        }
    }

    if let Some(elem_ty) = if_vec_return_elem_type(ok_ty) {
        map_type(sess, conv_map, cpp_cfg, ok_ty, Direction::Outgoing)?;
        let mut f_type_info = map_ordinal_result_type(sess, conv_map, arg_ty)?;
        if err_ty_name == "String" {
            let foreign_name = conv_map
                .find_foreigner_class_with_such_self_type(&elem_ty, false)
                .map(|v| v.name);
            if let Some(foreign_name) = foreign_name {
                let ok_typename = format!("RustForeignVec{}", foreign_name);
                let typename = match cpp_cfg.cpp_variant {
                    CppVariant::Std17 => {
                        Symbol::intern(&format!("std::variant<{}, RustString>", ok_typename))
                    }
                    CppVariant::Boost => {
                        Symbol::intern(&format!("boost::variant<{}, RustString>", ok_typename))
                    }
                };
                f_type_info.cpp_converter = Some(CppConverter {
                    typename,
                    output_converter: format!(
                        "{var}.is_ok != 0 ?
 {VarType}{{{Type}{{{var}.data.ok}}}} :
 {VarType}{{RustString{{{var}.data.err}}}}",
                        VarType = typename,
                        Type = ok_typename,
                        var = FROM_VAR_TEMPLATE,
                    ),
                    input_converter: "#error".to_string(),
                });
                return Ok(Some(f_type_info));
            } else {
                unimplemented!();
            }
        } else if let Some(err_class) =
            conv_map.find_foreigner_class_with_such_self_type(&err_ty, false)
        {
            // Result<Vec<T>, Err>
            let foreign_name = conv_map
                .find_foreigner_class_with_such_self_type(&elem_ty, false)
                .map(|v| v.name);
            if let Some(foreign_name) = foreign_name {
                let ok_typename = format!("RustForeignVec{}", foreign_name);
                let c_err_class = c_class_type(err_class);
                let typename = match cpp_cfg.cpp_variant {
                    CppVariant::Std17 => Symbol::intern(&format!(
                        "std::variant<{}, {}>",
                        ok_typename, err_class.name
                    )),
                    CppVariant::Boost => Symbol::intern(&format!(
                        "boost::variant<{}, {}>",
                        ok_typename, err_class.name
                    )),
                };
                f_type_info.cpp_converter = Some(CppConverter {
                    typename,
                    output_converter: format!(
                        "{var}.is_ok != 0 ?
 {VarType} {{ {Type}{{{var}.data.ok}} }} :
 {VarType} {{ {ErrType}(static_cast<{C_ErrType} *>({var}.data.err)) }}",
                        VarType = typename,
                        Type = ok_typename,
                        var = FROM_VAR_TEMPLATE,
                        ErrType = err_class.name,
                        C_ErrType = c_err_class,
                    ),
                    input_converter: "#error".to_string(),
                });
                return Ok(Some(f_type_info));
            } else {
                unimplemented!();
            }
        } else {
            unimplemented!();
        }
    } else {
        trace!("return result, but not foreign_class / Vec<foreign_class>");
        match ok_ty.node {
            //Result<(), err_ty>
            ast::TyKind::Tup(ref tup_types) if tup_types.is_empty() => {
                let err_ty_name = normalized_ty_string(&err_ty);
                if err_ty_name == "String" {
                    let typename = match cpp_cfg.cpp_variant {
                        CppVariant::Std17 => Symbol::intern("std::variant<void *, RustString>"),
                        CppVariant::Boost => Symbol::intern("boost::variant<void *, RustString>"),
                    };
                    let output_converter = format!(
                        "{var}.is_ok != 0 ?
 {VarType}{{{var}.data.ok}} :
 {VarType}{{RustString{{{var}.data.err}}}}",
                        VarType = typename,
                        var = FROM_VAR_TEMPLATE
                    );
                    let foreign_info = conv_map
                        .find_foreign_type_info_by_name(Symbol::intern(
                            "struct CResultObjectString",
                        ))
                        .expect("Can not find struct CResultObjectString");
                    return Ok(Some(CppForeignTypeInfo {
                        base: foreign_info,
                        c_converter: String::new(),
                        cpp_converter: Some(CppConverter {
                            typename,
                            output_converter,
                            input_converter: String::new(),
                        }),
                    }));
                } else if let Some(err_class) =
                    conv_map.find_foreigner_class_with_such_self_type(&err_ty, false)
                {
                    let c_err_class = c_class_type(err_class);
                    let typename = match cpp_cfg.cpp_variant {
                        CppVariant::Std17 => {
                            Symbol::intern(&format!("std::variant<void *, {}>", err_class.name))
                        }
                        CppVariant::Boost => {
                            Symbol::intern(&format!("boost::variant<void *, {}>", err_class.name))
                        }
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
                    let foreign_info = conv_map
                        .find_foreign_type_info_by_name(Symbol::intern(
                            "struct CResultObjectObject",
                        ))
                        .expect("Can not find info about struct CResultObjectObject");
                    return Ok(Some(CppForeignTypeInfo {
                        base: foreign_info,
                        c_converter: String::new(),
                        cpp_converter: Some(CppConverter {
                            typename,
                            output_converter,
                            input_converter: "#error".into(),
                        }),
                    }));
                } else {
                    unimplemented!();
                }
            }
            _ => unimplemented!(),
        }
    }
}

fn handle_option_type_in_input<'a>(
    sess: &'a ParseSess,
    conv_map: &mut TypesConvMap,
    cpp_cfg: &CppConfig,
    arg_ty: &ast::Ty,
    opt_ty: &ast::Ty,
) -> PResult<'a, Option<CppForeignTypeInfo>> {
    if let Some(fclass) = conv_map.find_foreigner_class_with_such_self_type(opt_ty, false) {
        let foreign_info = foreign_class_foreign_name(sess, conv_map, fclass, opt_ty.span, false)?;
        let (typename, input_converter) = match cpp_cfg.cpp_optional {
            CppOptional::Std17 => (
                Symbol::intern(&format!("std::optional<{}>", fclass.name)),
                format!(
                    " !!{var} ? {var}->release() : nullptr",
                    var = FROM_VAR_TEMPLATE,
                ),
            ),
            CppOptional::Boost => (
                Symbol::intern(&format!("boost::optional<{}>", fclass.name)),
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
                typename,
                output_converter: "#error".to_string(),
                input_converter,
            }),
        }));
    }
    trace!("handle_option_type_in_input arg_ty {:?}", arg_ty);
    let mut cpp_info_opt = map_ordinal_input_type(sess, conv_map, arg_ty)?;
    let cpp_info_ty = map_ordinal_input_type(sess, conv_map, opt_ty)?;
    let f_opt_ty = cpp_info_ty.base.name;
    let c_option_name = cpp_info_opt.base.name;
    let mut c_option_name: &str = &c_option_name.as_str();
    if c_option_name.starts_with("struct ") {
        c_option_name = &c_option_name[7..];
    }
    let (typename, input_converter) = match cpp_cfg.cpp_optional {
        CppOptional::Std17 => (
            Symbol::intern(&format!("std::optional<{}>", f_opt_ty)),
            format!(
                "!!{var} ? {CType}{{*{var}, 1}} : c_option_empty<{CType}>()",
                CType = c_option_name,
                var = FROM_VAR_TEMPLATE,
            ),
        ),
        CppOptional::Boost => (
            Symbol::intern(&format!("boost::optional<{}>", f_opt_ty)),
            format!(
                "!!{var} ? {CType}{{*{var}, 1}} : c_option_empty<{CType}>()",
                CType = c_option_name,
                var = FROM_VAR_TEMPLATE,
            ),
        ),
    };
    cpp_info_opt.cpp_converter = Some(CppConverter {
        typename,
        output_converter: "#error".to_string(),
        input_converter,
    });
    Ok(Some(cpp_info_opt))
}

fn handle_option_type_in_return<'a>(
    sess: &'a ParseSess,
    conv_map: &mut TypesConvMap,
    cpp_cfg: &CppConfig,
    arg_ty: &ast::Ty,
    opt_ty: &ast::Ty,
) -> PResult<'a, Option<CppForeignTypeInfo>> {
    if let Some(foreign_class) = conv_map.find_foreigner_class_with_such_self_type(opt_ty, false) {
        let foreign_info =
            foreign_class_foreign_name(sess, conv_map, foreign_class, opt_ty.span, false)?;
        let (typename, output_converter) = match cpp_cfg.cpp_optional {
            CppOptional::Std17 => (
                Symbol::intern(&format!("std::optional<{}>", foreign_class.name)),
                format!(
                    "{var} != nullptr ? {Type}({var}) : std::optional<{Type}>()",
                    Type = foreign_class.name,
                    var = FROM_VAR_TEMPLATE,
                ),
            ),
            CppOptional::Boost => (
                Symbol::intern(&format!("boost::optional<{}>", foreign_class.name)),
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
                typename,
                output_converter,
                input_converter: "#error".to_string(),
            }),
        }));
    }

    //handle Option<&ForeignClass> case
    if let ast::TyKind::Rptr(
        _,
        ast::MutTy {
            ty: ref under_ref_ty,
            mutbl: ast::Mutability::Immutable,
        },
    ) = opt_ty.node
    {
        if let Some(fclass) = conv_map
            .find_foreigner_class_with_such_self_type(under_ref_ty, false)
            .map(|v| v.clone())
        {
            let foreign_info =
                foreign_class_foreign_name(sess, conv_map, &fclass, under_ref_ty.span, false)?;
            let this_type_for_method = fclass.this_type_for_method.as_ref().ok_or_else(|| {
                fatal_error(
                    sess,
                    fclass.span,
                    &format!(
                        "Class {} (namespace {}) return as reference, but there is no constructor",
                        fclass.name, cpp_cfg.namespace_name,
                    ),
                )
            })?;
            let this_type: RustType = this_type_for_method.clone().into();
            let void_ptr_typename = Symbol::intern("*mut ::std::os::raw::c_void");
            let my_void_ptr_ti = RustType::new(
                parse_ty(sess, DUMMY_SP, void_ptr_typename)?,
                make_unique_rust_typename(void_ptr_typename, this_type.normalized_name),
            );
            let arg_rust_ty: RustType = arg_ty.clone().into();
            conv_map.add_type(arg_rust_ty.clone());
            conv_map.add_conversation_rule(
                arg_rust_ty,
                my_void_ptr_ti,
                Symbol::intern(&format!(
                    r#"
    let {to_var}: *mut ::std::os::raw::c_void = match {from_var} {{
        Some(x) => x as *const {self_type} as *mut ::std::os::raw::c_void,
        None => ::std::ptr::null_mut(),
    }};
"#,
                    to_var = TO_VAR_TEMPLATE,
                    from_var = FROM_VAR_TEMPLATE,
                    self_type = this_type.normalized_name,
                )).into(),
            );

            let (typename, output_converter) = match cpp_cfg.cpp_optional {
                CppOptional::Std17 => (
                    Symbol::intern(&format!("std::optional<{}Ref>", fclass.name)),
                    format!(
                        "{var} != nullptr ? {Type}Ref({var}) : std::optional<{Type}Ref>()",
                        Type = fclass.name,
                        var = FROM_VAR_TEMPLATE,
                    ),
                ),
                CppOptional::Boost => (
                    Symbol::intern(&format!("boost::optional<{}Ref>", fclass.name)),
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
                    typename,
                    output_converter,
                    input_converter: "#error".to_string(),
                }),
            }));
        }
    }
    let mut cpp_info_opt = map_ordinal_result_type(sess, conv_map, arg_ty)?;
    let cpp_info_ty = map_ordinal_result_type(sess, conv_map, opt_ty)?;
    let f_opt_ty = cpp_info_ty.base.name;
    debug!("is_this_exported_enum {:?}", opt_ty);
    let (typename, output_converter) =
        if let Some(foreign_enum) = conv_map.is_this_exported_enum(opt_ty) {
            match cpp_cfg.cpp_optional {
                CppOptional::Std17 => (
                    Symbol::intern(&format!("std::optional<{}>", f_opt_ty)),
                    format!(
                        "{var}.is_some ? static_cast<{EnumType}>({var}.val)
 : std::optional<{Type}>()",
                        Type = f_opt_ty,
                        var = FROM_VAR_TEMPLATE,
                        EnumType = foreign_enum.name,
                    ),
                ),
                CppOptional::Boost => (
                    Symbol::intern(&format!("boost::optional<{}>", f_opt_ty)),
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
                    Symbol::intern(&format!("std::optional<{}>", f_opt_ty)),
                    format!(
                        "{var}.is_some ? {var}.val : std::optional<{Type}>()",
                        Type = f_opt_ty,
                        var = FROM_VAR_TEMPLATE,
                    ),
                ),
                CppOptional::Boost => (
                    Symbol::intern(&format!("boost::optional<{}>", f_opt_ty)),
                    format!(
                        "{var}.is_some ? {var}.val : boost::optional<{Type}>()",
                        Type = f_opt_ty,
                        var = FROM_VAR_TEMPLATE,
                    ),
                ),
            }
        };
    cpp_info_opt.cpp_converter = Some(CppConverter {
        typename,
        output_converter,
        input_converter: "#error".to_string(),
    });
    Ok(Some(cpp_info_opt))
}
