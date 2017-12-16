use syntex_syntax::symbol::Symbol;
use syntex_syntax::parse::lexer::comments::strip_doc_comment_decoration;

use std::io::Write;
use std::fs::File;
use std::fmt;
use std::path::Path;

use {ForeignEnumInfo, ForeignInterface, ForeignerClassInfo};
use super::{fmt_write_err_map, CppForeignMethodSignature};
use types_conv_map::FROM_VAR_TEMPLATE;

pub(in cpp) fn generate_code_for_enum(
    output_dir: &Path,
    enum_info: &ForeignEnumInfo,
) -> Result<(), String> {
    let c_path = output_dir.join(format!("c_{}.h", enum_info.name));
    let mut file =
        File::create(&c_path).map_err(|err| format!("Couldn't create {:?}: {}", c_path, err))?;
    let enum_doc_comments = doc_comments_to_c_comments(&enum_info.doc_comments, true);

    write!(
        file,
        r#"// Automaticaly generated by rust_swig
#pragma once

{doc_comments}
enum {enum_name} {{
"#,
        enum_name = enum_info.name,
        doc_comments = enum_doc_comments,
    ).map_err(&map_write_err)?;

    for (i, item) in enum_info.items.iter().enumerate() {
        write!(
            file,
            "{doc_comments}{item_name} = {index}{separator}\n",
            item_name = item.name,
            index = i,
            doc_comments = doc_comments_to_c_comments(&item.doc_comments, false),
            separator = if i == enum_info.items.len() - 1 {
                '\n'
            } else {
                ','
            },
        ).map_err(&map_write_err)?;
    }

    write!(
        file,
        r#"
}};
"#
    ).map_err(&map_write_err)?;

    Ok(())
}

pub(in cpp) fn doc_comments_to_c_comments(doc_comments: &[Symbol], class_comments: bool) -> String {
    use std::fmt::Write;
    let mut comments = String::new();
    for (i, comment) in doc_comments.iter().enumerate() {
        if i != 0 {
            comments.push('\n');
        }
        if !class_comments {
            comments.push_str("    ");
        }
        write!(
            &mut comments,
            "//{}",
            strip_doc_comment_decoration(&*comment.as_str())
        ).unwrap();
    }
    comments
}

pub(in cpp) fn generate_for_interface(
    output_dir: &Path,
    interface: &ForeignInterface,
    f_methods: &[CppForeignMethodSignature],
) -> Result<(), String> {
    let c_path = output_dir.join(format!("c_{}.h", interface.name));
    let mut file =
        File::create(&c_path).map_err(|err| format!("Couldn't create {:?}: {}", c_path, err))?;
    let interface_comments = doc_comments_to_c_comments(&interface.doc_comments, true);
    write!(
        file,
        r#"// Automaticaly generated by rust_swig
#pragma once
{doc_comments}
struct C_{interface_name} {{
    void *opaque;
    //! call by Rust side when callback not need anymore
    void (*C_{interface_name}_deref)(void *opaque);
    "#,
        interface_name = interface.name,
        doc_comments = interface_comments
    ).map_err(&map_write_err)?;
    for (method, f_method) in interface.items.iter().zip(f_methods) {
        write!(
            file,
            r#"
{doc_comments}
    void (*{method_name})({single_args_with_types}, void *opaque);
"#,
            method_name = method.name,
            doc_comments = doc_comments_to_c_comments(&method.doc_comments, false),
            single_args_with_types = c_generate_args_with_types(f_method)?,
        ).map_err(&map_write_err)?;
    }
    write!(
        file,
        r#"
}};
"#
    ).map_err(map_write_err)?;
    Ok(())
}

fn map_write_err<Err: fmt::Display>(err: Err) -> String {
    format!("write failed: {}", err)
}

pub(in cpp) fn c_generate_args_with_types(
    f_method: &CppForeignMethodSignature,
) -> Result<String, String> {
    use std::fmt::Write;

    let mut buf = String::new();
    for (i, f_type_info) in f_method.input.iter().enumerate() {
        if i > 0 {
            write!(&mut buf, ", ").map_err(fmt_write_err_map)?;
        }
        write!(&mut buf, "{} a_{}", f_type_info.as_ref().name, i).map_err(fmt_write_err_map)?;
    }
    Ok(buf)
}

pub(in cpp) fn c_class_type(class: &ForeignerClassInfo) -> String {
    format!("{}Opaque", class.name)
}

pub(in cpp) fn cpp_generate_args_with_types(
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
                conv.typename
            } else {
                f_type_info.as_ref().name
            },
            i
        ).map_err(fmt_write_err_map)?;
    }
    Ok(ret)
}

pub(in cpp) fn cpp_generate_args_to_call_c(
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
            let conv_arg = conv.converter
                .as_str()
                .replace(FROM_VAR_TEMPLATE, &arg_name);
            write!(&mut ret, "{}", conv_arg)
        } else {
            write!(&mut ret, "a_{}", i)
        }.map_err(fmt_write_err_map)?;
    }
    Ok(ret)
}
