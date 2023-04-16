use bitflags::bitflags;

use super::{JniForeignMethodSignature, NullAnnotation};
use crate::WRITE_TO_MEM_FAILED_MSG;

bitflags! {
    pub(in crate::java_jni) struct ArgsFormatFlags: u8 {
        const NONE = 0;
        const USE_COMMA_IF_NEED = 1;
        const EXTERNAL = 2;
        const INTERNAL = 4;
        const COMMA_BEFORE = 8;
    }
}

pub(in crate::java_jni) fn args_with_java_types<'a, NI: Iterator<Item = &'a str>>(
    method: &JniForeignMethodSignature,
    arg_name_iter: NI,
    flags: ArgsFormatFlags,
    use_null_annotation: bool,
) -> String {
    use std::fmt::Write;

    assert!(flags.contains(ArgsFormatFlags::INTERNAL) || flags.contains(ArgsFormatFlags::EXTERNAL));

    let mut res = String::new();
    if flags.contains(ArgsFormatFlags::USE_COMMA_IF_NEED) && !method.input.is_empty() {
        res.push_str(", ");
    }
    let external = flags.contains(ArgsFormatFlags::EXTERNAL);

    for (i, (arg, arg_name)) in method.input.iter().zip(arg_name_iter).enumerate() {
        let type_name = match arg.java_converter.as_ref() {
            Some(converter) if flags.contains(ArgsFormatFlags::INTERNAL) => {
                converter.java_transition_type.display()
            }
            _ => arg.as_ref().name.display(),
        };
        let type_arlready_null_anotated =
            type_name.contains("@NonNull") || type_name.contains("@Nullable");
        let annotation = match arg.annotation {
            Some(NullAnnotation::NonNull)
                if external && use_null_annotation && !type_arlready_null_anotated =>
            {
                "@NonNull "
            }
            Some(NullAnnotation::Nullable)
                if external && use_null_annotation && !type_arlready_null_anotated =>
            {
                "@Nullable "
            }
            _ => "",
        };
        if i == (method.input.len() - 1) {
            write!(&mut res, "{}{} {}", annotation, type_name, arg_name)
        } else {
            write!(&mut res, "{}{} {}, ", annotation, type_name, arg_name)
        }
        .expect(WRITE_TO_MEM_FAILED_MSG);
    }
    res
}

pub(in crate::java_jni) fn doc_comments_to_java_comments(
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
        if i == 0 {
            comments.push_str("/**\n");
            if !class_comments {
                comments.push_str("    ");
            }
        }

        write!(&mut comments, " * {}", comment.trim()).unwrap();

        if i == doc_comments.len() - 1 {
            comments.push('\n');
            if !class_comments {
                comments.push_str("    ");
            }
            comments.push_str(" */");
        }
    }
    comments
}

pub(in crate::java_jni) fn get_null_annotation_imports(
    null_annotation_package: Option<&str>,
    methods_sign: &[JniForeignMethodSignature],
) -> String {
    if let Some(null_annotation_package) = null_annotation_package {
        let mut has_non_null = false;
        let mut has_nullable = false;

        for f_method in methods_sign {
            for arg in f_method
                .input
                .iter()
                .chain(std::iter::once(&f_method.output))
            {
                let mut process_null_annotation = |x: Option<NullAnnotation>| match x {
                    Some(NullAnnotation::NonNull) => has_non_null = true,
                    Some(NullAnnotation::Nullable) => has_nullable = true,
                    _ => {}
                };
                process_null_annotation(arg.annotation);
                if let Some(conv) = arg.java_converter.as_ref() {
                    process_null_annotation(conv.annotation);
                }
                if has_non_null && has_nullable {
                    return format!(
                        "import {package}.NonNull;\nimport {package}.Nullable;",
                        package = null_annotation_package
                    );
                }
            }
        }
        if has_non_null {
            return format!(
                "import {package}.NonNull;",
                package = null_annotation_package
            );
        }

        if has_nullable {
            return format!(
                "import {package}.Nullable;",
                package = null_annotation_package
            );
        }
    }

    String::new()
}

pub(in crate::java_jni) fn filter_null_annotation(type_name: &str) -> String {
    type_name.replace("@NonNull", "").replace("@Nullable", "")
}

pub(in crate::java_jni) fn is_primitive_type(type_name: &str) -> bool {
    matches!(
        type_name,
        "void" | "boolean" | "byte" | "short" | "int" | "long" | "float" | "double"
    )
}
