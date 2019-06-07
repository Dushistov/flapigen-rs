use std::{fmt::Write, io, mem};

use proc_macro2::TokenStream;
use rustc_hash::FxHashSet;
use smol_str::SmolStr;
use syn::spanned::Spanned;

use crate::{
    cpp::{fmt_write_err_map, map_any_err_to_our_err, CppForeignMethodSignature},
    error::{panic_on_syn_error, DiagnosticError},
    typemap::{ast::DisplayToTokens, CType, CTypes, TypeMap, FROM_VAR_TEMPLATE},
    types::{ForeignEnumInfo, ForeignerClassInfo},
};

pub(in crate::cpp) fn doc_comments_to_c_comments(
    doc_comments: &[String],
    class_comments: bool,
) -> String {
    use std::fmt::Write;
    let mut comments = String::new();
    for (i, comment) in doc_comments.iter().enumerate() {
        if i != 0 {
            comments.push('\n');
        }
        if !class_comments {
            comments.push_str("    ");
        }
        write!(&mut comments, "//{}", comment.trim()).unwrap();
    }
    comments
}

pub(in crate::cpp) fn c_generate_args_with_types(
    f_method: &CppForeignMethodSignature,
    append_comma_if_not_empty: bool,
) -> Result<String, String> {
    use std::fmt::Write;

    let mut buf = String::new();
    for (i, f_type_info) in f_method.input.iter().enumerate() {
        if i > 0 {
            write!(&mut buf, ", ").map_err(fmt_write_err_map)?;
        }
        write!(&mut buf, "{} a_{}", f_type_info.as_ref().name, i).map_err(fmt_write_err_map)?;
    }
    if !buf.is_empty() && append_comma_if_not_empty {
        write!(&mut buf, ", ").map_err(fmt_write_err_map)?;
    }
    Ok(buf)
}

pub(in crate::cpp) fn c_class_type(class: &ForeignerClassInfo) -> String {
    format!("{}Opaque", class.name)
}

pub(in crate::cpp) fn cpp_generate_args_with_types(
    f_method: &CppForeignMethodSignature,
) -> Result<String, String> {
    use std::fmt::Write;
    let mut ret = String::new();
    for (i, f_type_info) in f_method.input.iter().enumerate() {
        if i > 0 {
            write!(&mut ret, ", ").map_err(fmt_write_err_map)?;
        }

        write!(
            &mut ret,
            "{} a_{}",
            if let Some(conv) = f_type_info.cpp_converter.as_ref() {
                conv.typename.clone()
            } else {
                f_type_info.as_ref().name.clone()
            },
            i
        )
        .map_err(fmt_write_err_map)?;
    }
    Ok(ret)
}

pub(in crate::cpp) fn cpp_generate_args_to_call_c(
    f_method: &CppForeignMethodSignature,
) -> Result<String, String> {
    use std::fmt::Write;
    let mut ret = String::new();
    for (i, f_type_info) in f_method.input.iter().enumerate() {
        if i > 0 {
            write!(&mut ret, ", ").map_err(fmt_write_err_map)?;
        }
        if let Some(conv) = f_type_info.cpp_converter.as_ref() {
            let arg_name = format!("a_{}", i);
            let conv_arg = conv
                .converter
                .as_str()
                .replace(FROM_VAR_TEMPLATE, &arg_name);
            write!(&mut ret, "{}", conv_arg)
        } else {
            write!(&mut ret, "a_{}", i)
        }
        .map_err(fmt_write_err_map)?;
    }
    Ok(ret)
}

pub(in crate::cpp) fn cpp_header_name(class: &ForeignerClassInfo) -> String {
    format!("{}.hpp", class.name)
}

pub(in crate::cpp) fn c_header_name(class: &ForeignerClassInfo) -> String {
    format!("c_{}.h", class.name)
}

pub(in crate::cpp) fn cpp_header_name_for_enum(enum_info: &ForeignEnumInfo) -> String {
    format!("c_{}.h", enum_info.name)
}

pub(in crate::cpp) fn cpp_list_required_includes(
    methods: &mut [CppForeignMethodSignature],
) -> Vec<SmolStr> {
    let mut includes = FxHashSet::<SmolStr>::default();
    for m in methods {
        for p in &mut m.input {
            includes.extend(mem::replace(&mut p.provides_by_module, Vec::new()).into_iter());
        }
        includes.extend(mem::replace(&mut m.output.provides_by_module, Vec::new()).into_iter());
    }

    let mut ret: Vec<_> = includes.into_iter().collect();
    ret.sort();
    ret
}

pub(in crate::cpp) fn generate_c_type(
    tmap: &TypeMap,
    c_types: &CTypes,
    out: &mut io::Write,
) -> Result<Vec<TokenStream>, DiagnosticError> {
    let mut rust_code = vec![];

    for c_type in &c_types.types {
        match c_type {
            CType::Struct(ref s) => {
                let mut rust_layout_test = format!(
                    r#"
#[allow(non_snake_case)]
#[test]
fn test_{name}_layout() {{
#[repr(C)]
struct My{name} {{
"#,
                    name = s.ident,
                );
                writeln!(out, "struct {} {{", s.ident).map_err(map_any_err_to_our_err)?;
                let fields = match s.fields {
                    syn::Fields::Named(ref x) => x,
                    _ => {
                        return Err(DiagnosticError::new(
                            c_types.src_id,
                            s.fields.span(),
                            "only fields with names accepted in this context",
                        ))
                    }
                };
                let mut fields_asserts_code = String::new();
                for f in &fields.named {
                    let id = f.ident.as_ref().ok_or_else(|| {
                        DiagnosticError::new(
                            c_types.src_id,
                            f.span(),
                            "only fields with names accepted in this context",
                        )
                    })?;
                    let field_rty = tmap.ty_to_rust_type_checked(&f.ty).ok_or_else(|| {
                        DiagnosticError::new(c_types.src_id, f.ty.span(), "unknown Rust type")
                    })?;
                    let field_fty = tmap
                        .find_foreign_type_related_to_rust_ty(field_rty.to_idx())
                        .ok_or_else(|| {
                            DiagnosticError::new(
                                c_types.src_id,
                                f.ty.span(),
                                "no foreign type related to this type",
                            )
                        })?;
                    writeln!(out, "    {} {};", tmap[field_fty].name.as_str(), id)
                        .map_err(map_any_err_to_our_err)?;
                    writeln!(&mut rust_layout_test, "{}: {},", id, DisplayToTokens(&f.ty))
                        .map_err(map_any_err_to_our_err)?;

                    writeln!(
                        &mut fields_asserts_code,
                        r#"
#[allow(dead_code)]
fn check_{struct_name}_{field_name}_type_fn(s: &{struct_name}) -> &{field_type} {{
    &s.{field_name}
}}
    let offset_our = ((&our_s.{field_name} as *const {field_type}) as usize) - ((&our_s as *const My{struct_name}) as usize);
    let offset_user = ((&user_s.{field_name} as *const {field_type}) as usize) - ((&user_s as *const {struct_name}) as usize);
    assert_eq!(offset_our, offset_user);
"#,
                        struct_name = s.ident,
                        field_name = id,
                        field_type = DisplayToTokens(&f.ty),
                    )
                    .map_err(map_any_err_to_our_err)?;
                }
                out.write_all(b"};\n").map_err(map_any_err_to_our_err)?;

                writeln!(
                    &mut rust_layout_test,
                    r#"}}
    assert_eq!(::std::mem::size_of::<My{name}>(), ::std::mem::size_of::<{name}>());
    assert_eq!(::std::mem::align_of::<My{name}>(), ::std::mem::align_of::<{name}>());
    let our_s: My{name} = unsafe {{ ::std::mem::zeroed() }};
    let user_s: {name} = unsafe {{ ::std::mem::zeroed() }};

{fields_asserts}
}}
"#,
                    name = s.ident,
                    fields_asserts = fields_asserts_code,
                )
                .map_err(map_any_err_to_our_err)?;
                let tt: TokenStream = syn::parse_str(&rust_layout_test).unwrap_or_else(|err| {
                    panic_on_syn_error("Internal: layout unit test", rust_layout_test, err)
                });
                rust_code.push(tt);
            }
            CType::Union(ref u) => {
                unimplemented!();
            }
        }
    }

    Ok(rust_code)
}
