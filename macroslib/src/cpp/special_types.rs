use syntex_syntax::parse::{PResult, ParseSess};
use syntex_syntax::ast;

use syntex_pos::DUMMY_SP;
use syntex_syntax::symbol::Symbol;

use my_ast::{if_option_return_some_type, normalized_ty_string, parse_ty, RustType};
use errors::fatal_error;
use types_conv_map::{ForeignTypeInfo, FROM_VAR_TEMPLATE};
use {CppConfig, CppOptional, ForeignEnumInfo, TypesConvMap};
use cpp::{CppConverter, CppForeignTypeInfo};
use cpp::cpp_code::c_class_type;

pub(in cpp) fn special_type<'a>(
    sess: &'a ParseSess,
    conv_map: &TypesConvMap,
    cpp_cfg: &CppConfig,
    arg_ty: &ast::Ty,
    input: bool,
) -> PResult<'a, Option<CppForeignTypeInfo>> {
    trace!(
        "special_type: check is arg.ty({:?}) implements exported enum",
        arg_ty
    );
    if let Some(foreign_enum) = conv_map.is_this_exported_enum(&arg_ty) {
        let converter = calc_converter_for_enum(foreign_enum);
        return Ok(Some(converter));
    }

    let ty_name = normalized_ty_string(&arg_ty);
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

    if let Some(foreign_class) = conv_map.find_foreigner_class_with_such_self_type(arg_ty) {
        let foreign_typename = Symbol::intern(&format!("{} *", c_class_type(foreign_class)));
        let foreign_info = conv_map
            .find_foreign_type_info_by_name(foreign_typename)
            .ok_or_else(|| {
                fatal_error(
                    sess,
                    arg_ty.span,
                    &format!("type {} unknown", foreign_class.name),
                )
            })?;

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

    if !input {
        if let Some(ty) = if_option_return_some_type(arg_ty) {
            if let Some(foreign_class) = conv_map.find_foreigner_class_with_such_self_type(&ty) {
                let foreign_typename =
                    Symbol::intern(&format!("{} *", c_class_type(foreign_class)));
                let foreign_info = conv_map
                    .find_foreign_type_info_by_name(foreign_typename)
                    .ok_or_else(|| {
                        fatal_error(
                            sess,
                            arg_ty.span,
                            &format!("type {} unknown", foreign_class.name),
                        )
                    })?;
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
                        input_converter: String::new(),
                    }),
                }));
            } else {
                unimplemented!();
            }
        }
    }

    trace!("Oridinary type {:?}", arg_ty);
    Ok(None)
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
