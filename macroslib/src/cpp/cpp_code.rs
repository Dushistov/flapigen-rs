use crate::{
    cpp::{fmt_write_err_map, CppForeignMethodSignature},
    typemap::FROM_VAR_TEMPLATE,
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

pub(in crate::cpp) fn cpp_header_name_for_enum(enum_info: &ForeignEnumInfo) -> String {
    format!("c_{}.h", enum_info.name)
}
