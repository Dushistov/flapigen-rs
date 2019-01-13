use std::{fmt, io::Write, path::Path};

use bitflags::bitflags;

use crate::{
    file_cache::FileWriteCache,
    java_jni::{fmt_write_err_map, method_name, JniForeignMethodSignature},
    ForeignEnumInfo, ForeignInterface, ForeignerClassInfo, MethodAccess, MethodVariant,
};

bitflags! {
    struct ArgsFormatFlags: u8 {
        const NONE = 0;
        const USE_COMMA_IF_NEED = 1;
        const EXTERNAL = 2;
        const INTERNAL = 4;
        const COMMA_BEFORE = 8;
    }
}

pub(in crate::java_jni) fn generate_java_code_for_enum(
    output_dir: &Path,
    package_name: &str,
    enum_info: &ForeignEnumInfo,
) -> Result<(), String> {
    let path = output_dir.join(format!("{}.java", enum_info.name));
    let mut file = FileWriteCache::new(&path);
    let enum_doc_comments = doc_comments_to_java_comments(&enum_info.doc_comments, true);
    write!(
        file,
        r#"// Automaticaly generated by rust_swig
package {package_name};

{doc_comments}
public enum {enum_name} {{
"#,
        package_name = package_name,
        enum_name = enum_info.name,
        doc_comments = enum_doc_comments,
    )
    .map_err(&map_write_err)?;

    for (i, item) in enum_info.items.iter().enumerate() {
        writeln!(
            file,
            "{doc_comments}{item_name}({index}){separator}",
            item_name = item.name,
            index = i,
            doc_comments = doc_comments_to_java_comments(&item.doc_comments, false),
            separator = if i == enum_info.items.len() - 1 {
                ';'
            } else {
                ','
            },
        )
        .map_err(&map_write_err)?;
    }

    write!(
        file,
        r#"
    private int value;
    {enum_name}(int value) {{
        this.value = value;
    }}
    public final int getValue() {{ return value; }}
}}
"#,
        enum_name = enum_info.name
    )
    .map_err(&map_write_err)?;

    file.update_file_if_necessary().map_err(&map_write_err)?;
    Ok(())
}

pub(in crate::java_jni) fn generate_java_code_for_interface(
    output_dir: &Path,
    package_name: &str,
    interface: &ForeignInterface,
    methods_sign: &[JniForeignMethodSignature],
    use_null_annotation: Option<&str>,
) -> Result<(), String> {
    let path = output_dir.join(format!("{}.java", interface.name));
    let mut file = FileWriteCache::new(&path);
    let imports = get_null_annotation_imports(use_null_annotation, methods_sign);
    let interface_comments = doc_comments_to_java_comments(&interface.doc_comments, true);
    write!(
        file,
        r#"// Automaticaly generated by rust_swig
package {package_name};
{imports}
{doc_comments}
public interface {interface_name} {{
"#,
        package_name = package_name,
        interface_name = interface.name,
        doc_comments = interface_comments,
        imports = imports,
    )
    .map_err(&map_write_err)?;

    for (method, f_method) in interface.items.iter().zip(methods_sign) {
        write!(
            file,
            r#"
{doc_comments}
    void {method_name}({single_args_with_types});
"#,
            method_name = method.name,
            doc_comments = doc_comments_to_java_comments(&method.doc_comments, false),
            single_args_with_types = args_with_java_types(
                f_method,
                ArgsFormatFlags::EXTERNAL,
                use_null_annotation.is_some()
            )?,
        )
        .map_err(&map_write_err)?;
    }

    write!(
        file,
        r#"
}}
"#,
    )
    .map_err(&map_write_err)?;
    file.update_file_if_necessary().map_err(&map_write_err)?;
    Ok(())
}

pub(in crate::java_jni) fn generate_java_code(
    output_dir: &Path,
    package_name: &str,
    class: &ForeignerClassInfo,
    methods_sign: &[JniForeignMethodSignature],
    use_null_annotation: Option<&str>,
) -> Result<(), String> {
    let path = output_dir.join(format!("{}.java", class.name));
    let mut file = FileWriteCache::new(&path);

    let imports = get_null_annotation_imports(use_null_annotation, methods_sign);

    let class_doc_comments = doc_comments_to_java_comments(&class.doc_comments, true);
    write!(
        file,
        r#"// Automaticaly generated by rust_swig
package {package_name};
{imports}
{doc_comments}
public final class {class_name} {{
"#,
        package_name = package_name,
        imports = imports,
        class_name = class.name,
        doc_comments = class_doc_comments,
    )
    .map_err(&map_write_err)?;

    let mut have_methods = false;
    let mut have_constructor = false;

    for (method, f_method) in class.methods.iter().zip(methods_sign) {
        write!(
            &mut file,
            "{doc_comments}",
            doc_comments = doc_comments_to_java_comments(&method.doc_comments, false)
        )
        .map_err(&map_write_err)?;
        let exception_spec = if method.may_return_error {
            "throws Exception"
        } else {
            ""
        };

        let method_access = match method.access {
            MethodAccess::Private => "private",
            MethodAccess::Public => "public",
            MethodAccess::Protected => unreachable!(),
        };

        let convert_code = convert_code_for_method(f_method);
        let func_name = method_name(method, f_method);
        match method.variant {
            MethodVariant::StaticMethod => {
                let ret_type = &f_method.output.name;

                if convert_code.is_empty() {
                    write!(
                        file,
                        r#"
    {method_access} static native {ret_type} {func_name}({args_with_types}) {exception_spec};
"#,
                        method_access = method_access,
                        ret_type = ret_type,
                        func_name = func_name,
                        args_with_types = args_with_java_types(
                            f_method,
                            ArgsFormatFlags::EXTERNAL,
                            use_null_annotation.is_some()
                        )?,
                        exception_spec = exception_spec,
                    )
                    .map_err(&map_write_err)?;
                } else {
                    write!(
                        file,
                        r#"
    {method_access} static {ret_type} {method_name}({single_args_with_types}) {exception_spec} {{
{convert_code}
         {return_code}{func_name}({args});
    }}
    private static native {ret_type} {func_name}({args_with_types}) {exception_spec};
"#,
                        method_name = method.short_name(),
                        method_access = method_access,
                        ret_type = ret_type,
                        func_name = func_name,
                        return_code = if ret_type != "void" { "return " } else { "" },
                        args_with_types = args_with_java_types(
                            f_method,
                            ArgsFormatFlags::INTERNAL,
                            use_null_annotation.is_some()
                        )?,
                        exception_spec = exception_spec,
                        single_args_with_types = args_with_java_types(
                            f_method,
                            ArgsFormatFlags::EXTERNAL,
                            use_null_annotation.is_some()
                        )?,
                        convert_code = convert_code,
                        args = list_of_args_for_call_method(f_method, ArgsFormatFlags::INTERNAL)?,
                    )
                    .map_err(&map_write_err)?;
                }
            }
            MethodVariant::Method(_) => {
                have_methods = true;
                let ret_type = &f_method.output.name;
                write!(
                    file,
                    r#"
    {method_access} final {ret_type} {method_name}({single_args_with_types}) {exception_spec} {{
{convert_code}
        {return_code}{func_name}(mNativeObj{args});
    }}
    private static native {ret_type} {func_name}(long me{args_with_types}) {exception_spec};
"#,
                    method_access = method_access,
                    ret_type = ret_type,
                    method_name = method.short_name(),
                    exception_spec = exception_spec,
                    return_code = if ret_type != "void" { "return " } else { "" },
                    func_name = func_name,
                    convert_code = convert_code,
                    single_args_with_types = args_with_java_types(
                        f_method,
                        ArgsFormatFlags::EXTERNAL,
                        use_null_annotation.is_some()
                    )?,
                    args_with_types = args_with_java_types(
                        f_method,
                        ArgsFormatFlags::USE_COMMA_IF_NEED | ArgsFormatFlags::INTERNAL,
                        use_null_annotation.is_some()
                    )?,
                    args = list_of_args_for_call_method(
                        f_method,
                        ArgsFormatFlags::COMMA_BEFORE | ArgsFormatFlags::INTERNAL
                    )?,
                )
                .map_err(&map_write_err)?;
            }
            MethodVariant::Constructor => {
                have_constructor = true;

                if method.is_dummy_constructor() {
                    write!(
                        file,
                        "
    {method_access} {class_name}() {{}}
",
                        method_access = method_access,
                        class_name = class.name,
                    )
                    .map_err(&map_write_err)?;
                } else {
                    write!(
                        file,
                        "
    {method_access} {class_name}({ext_args_with_types}) {exception_spec} {{
{convert_code}
        mNativeObj = init({args});
    }}
    private static native long {func_name}({args_with_types}) {exception_spec};
",
                        method_access = method_access,
                        class_name = class.name,
                        exception_spec = exception_spec,
                        func_name = func_name,
                        ext_args_with_types = args_with_java_types(
                            f_method,
                            ArgsFormatFlags::EXTERNAL,
                            use_null_annotation.is_some()
                        )?,
                        args_with_types = args_with_java_types(
                            f_method,
                            ArgsFormatFlags::INTERNAL,
                            use_null_annotation.is_some()
                        )?,
                        convert_code = convert_code,
                        args = list_of_args_for_call_method(f_method, ArgsFormatFlags::INTERNAL)?
                    )
                    .map_err(&map_write_err)?;
                }
            }
        }
    }

    if have_methods && !have_constructor {
        return Err(format!(
            "package {}, class {}: has methods, but no constructor\n
May be you need to use `private constructor = empty;` syntax?",
            package_name, class.name
        ));
    }
    if have_constructor {
        write!(
            file,
            "
    public synchronized void delete() {{
        if (mNativeObj != 0) {{
            do_delete(mNativeObj);
            mNativeObj = 0;
       }}
    }}
    @Override
    protected void finalize() throws Throwable {{
        try {{
            delete();
        }}
        finally {{
             super.finalize();
        }}
    }}
    private static native void do_delete(long me);
    /*package*/ long mNativeObj;
"
        )
        .map_err(&map_write_err)?;
    }

    //utility class, so add private constructor
    //to prevent object creation
    if !have_constructor && !have_methods {
        write!(
            file,
            r#"
    private {class_name}() {{}}
"#,
            class_name = class.name
        )
        .map_err(&map_write_err)?;
    }

    file.write_all(class.foreigner_code.as_bytes())
        .map_err(&map_write_err)?;
    write!(file, "}}").map_err(&map_write_err)?;

    file.update_file_if_necessary().map_err(&map_write_err)?;
    Ok(())
}

fn args_with_java_types(
    method: &JniForeignMethodSignature,
    flags: ArgsFormatFlags,
    use_null_annotation: bool,
) -> Result<String, String> {
    use std::fmt::Write;

    assert!(flags.contains(ArgsFormatFlags::INTERNAL) || flags.contains(ArgsFormatFlags::EXTERNAL));

    let mut res = String::new();
    if flags.contains(ArgsFormatFlags::USE_COMMA_IF_NEED) && !method.input.is_empty() {
        write!(&mut res, ", ").map_err(fmt_write_err_map)?;
    }
    let annotation = if flags.contains(ArgsFormatFlags::EXTERNAL) && use_null_annotation {
        "@NonNull "
    } else {
        ""
    };
    for (i, arg) in method.input.iter().enumerate() {
        let type_name = if flags.contains(ArgsFormatFlags::INTERNAL) && arg.java_need_conversation()
        {
            arg.java_transition_type.as_ref().unwrap()
        } else {
            &arg.as_ref().name
        };
        let annotation = gen_annotation_if_need(&type_name, annotation);
        if i == (method.input.len() - 1) {
            write!(&mut res, "{}{} a{}", annotation, type_name, i)
        } else {
            write!(&mut res, "{}{} a{}, ", annotation, type_name, i)
        }
        .map_err(&fmt_write_err_map)?;
    }
    Ok(res)
}

fn list_of_args_for_call_method(
    f_method: &JniForeignMethodSignature,
    flags: ArgsFormatFlags,
) -> Result<String, String> {
    use std::fmt::Write;

    assert!(flags.contains(ArgsFormatFlags::INTERNAL) || flags.contains(ArgsFormatFlags::EXTERNAL));

    let mut res = String::new();
    if f_method.input.is_empty() {
        return Ok(res);
    }

    if flags.contains(ArgsFormatFlags::COMMA_BEFORE) {
        res.push_str(", ");
    }

    for (i, arg) in f_method.input.iter().enumerate() {
        let need_conv = flags.contains(ArgsFormatFlags::INTERNAL) && arg.java_need_conversation();
        if i == (f_method.input.len() - 1) {
            if need_conv {
                write!(&mut res, "a{}C0", i)
            } else {
                write!(&mut res, "a{}", i)
            }
        } else if need_conv {
            write!(&mut res, "a{}C0, ", i)
        } else {
            write!(&mut res, "a{}, ", i)
        }
        .map_err(|err| format!("write fmt failed: {}", err))?;
    }

    Ok(res)
}

fn convert_code_for_method(f_method: &JniForeignMethodSignature) -> String {
    let mut ret = String::new();
    for (i, arg) in f_method.input.iter().enumerate() {
        if let Some(java_code) = arg.java_convert(|| (format!("a{}", i), format!("a{}C0", i))) {
            ret.push_str(&java_code);
        }
    }
    ret
}

fn doc_comments_to_java_comments(doc_comments: &[String], class_comments: bool) -> String {
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

fn gen_annotation_if_need(type_name: &str, annotation: &'static str) -> &'static str {
    match type_name {
        "void" | "boolean" | "byte" | "short" | "int" | "long" | "float" | "double" => "",
        _ => annotation,
    }
}

fn get_null_annotation_imports(
    use_null_annotation: Option<&str>,
    methods_sign: &[JniForeignMethodSignature],
) -> String {
    if let Some(import) = use_null_annotation {
        for f_method in methods_sign {
            let has_annotation = f_method
                .input
                .iter()
                .any(|arg| !gen_annotation_if_need(&arg.as_ref().name, "x").is_empty());
            if has_annotation {
                return format!("import {};", import);
            }
        }
    }

    String::new()
}

fn map_write_err<Err: fmt::Display>(err: Err) -> String {
    format!("write failed: {}", err)
}
