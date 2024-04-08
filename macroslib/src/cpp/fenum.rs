use log::trace;
use quote::quote;
use std::{io::Write, rc::Rc};
use syn::Type;

use crate::{
    cpp::{cpp_code, CppContext},
    error::{invalid_src_id_span, DiagnosticError, Result},
    extension::extend_foreign_enum,
    file_cache::FileWriteCache,
    typemap::{
        ast::{parse_ty_with_given_span, ForeignTypeName},
        ty::{ForeignConversionIntermediate, ForeignConversionRule, ForeignTypeS},
        TypeConvCode, FROM_VAR_TEMPLATE,
    },
    types::ForeignEnumInfo,
    WRITE_TO_MEM_FAILED_MSG,
};

pub(in crate::cpp) fn generate_enum(ctx: &mut CppContext, fenum: &ForeignEnumInfo) -> Result<()> {
    if (fenum.items.len() as u64) >= u64::from(u32::max_value()) {
        return Err(DiagnosticError::new(
            fenum.src_id,
            fenum.span(),
            "Too many items in enum",
        ));
    }

    trace!("enum_ti: {}", fenum.name);
    let enum_name = &fenum.name;
    let enum_ti: Type = parse_ty_with_given_span(&enum_name.to_string(), fenum.name.span())
        .map_err(|err| DiagnosticError::from_syn_err(fenum.src_id, err))?;
    let enum_rty = ctx.conv_map.find_or_alloc_rust_type_that_implements(
        &enum_ti,
        &["SwigForeignEnum"],
        fenum.src_id,
    );

    generate_cpp_code_for_enum(ctx, fenum)
        .map_err(|err| DiagnosticError::new(fenum.src_id, fenum.span(), err))?;
    generate_c_code_for_enum(ctx, fenum)
        .map_err(|err| DiagnosticError::new(fenum.src_id, fenum.span(), err))?;
    generate_rust_trait_for_enum(ctx, fenum)?;

    let u32_rty = ctx
        .conv_map
        .find_or_alloc_rust_type_no_src_id(&parse_type! { u32 });

    let enum_ftype = ForeignTypeS {
        name: ForeignTypeName::new(fenum.name.to_string(), (fenum.src_id, fenum.name.span())),
        provided_by_module: vec![
            format!("\"{}\"", cpp_code::cpp_header_name_for_enum(fenum)).into()
        ],
        into_from_rust: Some(ForeignConversionRule {
            rust_ty: enum_rty.to_idx(),
            intermediate: Some(ForeignConversionIntermediate {
                input_to_output: false,
                intermediate_ty: u32_rty.to_idx(),
                conv_code: Rc::new(TypeConvCode::new(
                    format!(
                        "static_cast<{enum_name}>({var})",
                        enum_name = fenum.name,
                        var = FROM_VAR_TEMPLATE
                    ),
                    invalid_src_id_span(),
                )),
            }),
        }),
        from_into_rust: Some(ForeignConversionRule {
            rust_ty: enum_rty.to_idx(),
            intermediate: Some(ForeignConversionIntermediate {
                input_to_output: false,
                intermediate_ty: u32_rty.to_idx(),
                conv_code: Rc::new(TypeConvCode::new(
                    format!("static_cast<uint32_t>({FROM_VAR_TEMPLATE})"),
                    invalid_src_id_span(),
                )),
            }),
        }),
    };
    ctx.conv_map.alloc_foreign_type(enum_ftype)?;
    Ok(())
}

fn generate_cpp_code_for_enum(
    ctx: &mut CppContext,
    enum_info: &ForeignEnumInfo,
) -> std::result::Result<(), DiagnosticError> {
    let c_path = ctx
        .cfg
        .output_dir
        .join(cpp_code::cpp_header_name_for_enum(enum_info));
    let mut file = FileWriteCache::new(&c_path, ctx.generated_foreign_files);
    let enum_doc_comments = cpp_code::doc_comments_to_c_comments(&enum_info.doc_comments, true);

    writeln!(
        file,
        r#"// Automatically generated by flapigen
#pragma once

namespace {namespace} {{
{doc_comments}
enum {enum_name} {{"#,
        enum_name = enum_info.name,
        doc_comments = enum_doc_comments,
        namespace = ctx.cfg.namespace_name,
    )
    .expect(WRITE_TO_MEM_FAILED_MSG);

    for (i, item) in enum_info.items.iter().enumerate() {
        //Enums are aligned left, so we pass true to get left aligned comments.
        let mut doc_comments = cpp_code::doc_comments_to_c_comments(&item.doc_comments, true);
        if !doc_comments.is_empty() && !doc_comments.ends_with('\n') {
            doc_comments.push('\n');
        }
        writeln!(
            file,
            "{doc_comments}{item_name} = {index}{separator}",
            item_name = item.name,
            index = i,
            doc_comments = doc_comments,
            separator = if i == enum_info.items.len() - 1 {
                ""
            } else {
                ","
            },
        )
        .expect(WRITE_TO_MEM_FAILED_MSG);
    }

    writeln!(
        file,
        r#"}};
}} // namespace {namespace}
"#,
        namespace = ctx.cfg.namespace_name
    )
    .expect(WRITE_TO_MEM_FAILED_MSG);
    let mut cnt = file.take_content();
    extend_foreign_enum(enum_info, &mut cnt, ctx.enum_ext_handlers)?;
    file.replace_content(cnt);
    file.update_file_if_necessary()
        .map_err(DiagnosticError::map_any_err_to_our_err)?;
    Ok(())
}

fn generate_c_code_for_enum(
    ctx: &mut CppContext,
    enum_info: &ForeignEnumInfo,
) -> std::result::Result<(), DiagnosticError> {
    let c_path = ctx
        .cfg
        .output_dir
        .join(cpp_code::c_header_name_for_enum(enum_info));
    let mut file = FileWriteCache::new(&c_path, ctx.generated_foreign_files);
    let enum_doc_comments = cpp_code::doc_comments_to_c_comments(&enum_info.doc_comments, true);

    writeln!(
        file,
        r#"// Automatically generated by flapigen
#pragma once

{doc_comments}

enum {enum_name} {{"#,
        enum_name = enum_info.name,
        doc_comments = enum_doc_comments,
    )
    .expect(WRITE_TO_MEM_FAILED_MSG);

    for (i, item) in enum_info.items.iter().enumerate() {
        //Enums are aligned left, so we pass true to get left aligned comments.
        let mut doc_comments = cpp_code::doc_comments_to_c_comments(&item.doc_comments, true);
        if !doc_comments.is_empty() && !doc_comments.ends_with('\n') {
            doc_comments.push('\n');
        }
        writeln!(
            file,
            "{doc_comments}    {enum_name}_{item_name} = {index}{separator}",
            item_name = item.name,
            enum_name = enum_info.name,
            index = i,
            doc_comments = doc_comments,
            separator = if i != enum_info.items.len() - 1 {
                ",\n"
            } else {
                ""
            },
        )
        .expect(WRITE_TO_MEM_FAILED_MSG);
    }

    // Need to name it C<enum_name>, else this may conflicts with C++ bindings code.
    writeln!(
        file,
        r"}};

typedef enum {enum_name} C{enum_name};
",
        enum_name = enum_info.name
    )
    .expect(WRITE_TO_MEM_FAILED_MSG);

    let mut cnt = file.take_content();
    extend_foreign_enum(enum_info, &mut cnt, ctx.enum_ext_handlers)?;
    file.replace_content(cnt);
    file.update_file_if_necessary()
        .map_err(DiagnosticError::map_any_err_to_our_err)?;
    Ok(())
}

fn generate_rust_trait_for_enum(ctx: &mut CppContext, enum_info: &ForeignEnumInfo) -> Result<()> {
    let mut arms_to_u32 = Vec::with_capacity(enum_info.items.len());
    let mut arms_from_u32 = Vec::with_capacity(enum_info.items.len());
    assert!((enum_info.items.len() as u64) <= u64::from(u32::max_value()));
    for (i, item) in enum_info.items.iter().enumerate() {
        let item_name = &item.rust_name;
        let idx = i as u32;
        arms_to_u32.push(quote! { #item_name => #idx });
        arms_from_u32.push(quote! { #idx => #item_name });
    }

    let rust_enum_name = &enum_info.name;

    ctx.rust_code.push(quote! {
        impl SwigForeignEnum for #rust_enum_name {
            fn as_u32(&self) -> u32 {
                match *self {
                    #(#arms_to_u32),*
                }
            }
            fn from_u32(x: u32) -> Self {
                match x {
                    #(#arms_from_u32),*
                    ,
                    _ => panic!(concat!("{} not expected for ", stringify!(#rust_enum_name)), x),
                }
            }
        }
    });

    Ok(())
}
