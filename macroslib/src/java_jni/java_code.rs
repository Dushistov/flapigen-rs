use std::path::Path;
use std::fs::File;
use std::io::Write;
use std::io;

use super::{fmt_write_err_map, method_name, ForeignMethodSignature};
use {ForeignerClassInfo, MethodVariant};

mod args_format_flags {
    bitflags! {
        pub struct ArgsFormatFlags: u8 {
            const NONE = 0;
            const USE_COMMA_IF_NEED = 1;
            const EXTERNAL = 2;
            const INTERNAL = 4;
            const COMMA_BEFORE = 8;
        }
    }
}
use self::args_format_flags::ArgsFormatFlags;

pub(in java_jni) fn generate_java_code(
    output_dir: &Path,
    package_name: &str,
    class: &ForeignerClassInfo,
    methods_sign: &[ForeignMethodSignature],
) -> Result<(), String> {
    let mut file: Box<Write> = if output_dir.to_str() == Some("-") {
        Box::new(io::stdout())
    } else {
        let path = output_dir.join(format!("{}.java", class.name));
        Box::new(
            File::create(&path)
                .map_err(|err| format!("Couldn't create {:?}: {}", path, err))?,
        )
    };

    let map_write_err = |err| format!("write failed: {}", err);

    write!(
        file,
        "package {package_name};
public final class {class_name} {{
",
        package_name = package_name,
        class_name = class.name
    ).map_err(&map_write_err)?;

    let mut have_methods = false;
    let mut have_constructor = false;

    for (method, f_method) in class.methods.iter().zip(methods_sign) {
        let exception_spec = if method.may_return_error {
            "throws Exception"
        } else {
            ""
        };

        let method_access = if method.foreigner_private {
            "private"
        } else {
            "public"
        };

        let convert_code = convert_code_for_method(f_method);
        let func_name = method_name(method, f_method);
        match method.variant {
            MethodVariant::StaticMethod => {
                let ret_type = f_method.output.name;

                if convert_code.is_empty() {
                    write!(
                        file,
                        r#"
    {method_access} static native {ret_type} {func_name}({args_with_types}) {exception_spec};
"#,
                        method_access = method_access,
                        ret_type = ret_type,
                        func_name = func_name,
                        args_with_types =
                            args_with_java_types(f_method, args_format_flags::EXTERNAL)?,
                        exception_spec = exception_spec,
                    ).map_err(&map_write_err)?;
                } else {
                    write!(
                        file,
                        r#"
    {method_access} static {ret_type} {method_name}({single_args_with_types}) {exception_spec} {{
{convert_code}
         {return_code} {func_name}({args});
    }}
    private static native {ret_type} {func_name}({args_with_types}) {exception_spec};
"#,
                        method_name = method.short_name(),
                        method_access = method_access,
                        ret_type = ret_type,
                        func_name = func_name,
                        return_code = if ret_type != "void" { "return" } else { "" },
                        args_with_types =
                            args_with_java_types(f_method, args_format_flags::INTERNAL)?,
                        exception_spec = exception_spec,
                        single_args_with_types =
                            args_with_java_types(f_method, args_format_flags::EXTERNAL)?,
                        convert_code = convert_code,
                        args = list_of_args_for_call_method(f_method, args_format_flags::INTERNAL)?,
                    ).map_err(&map_write_err)?;
                }
            }
            MethodVariant::Method(_) => {
                have_methods = true;
                let ret_type = f_method.output.name;
                write!(
                    file,
                    r#"
    {method_access} final {ret_type} {method_name}({single_args_with_types}) {exception_spec} {{
{convert_code}
         {return_code} {func_name}(mNativeObj{args});
    }}
    private static native {ret_type} {func_name}(long me{args_with_types}) {exception_spec};
"#,
                    method_access = method_access,
                    ret_type = ret_type,
                    method_name = method.short_name(),
                    exception_spec = exception_spec,
                    return_code = if ret_type != "void" { "return" } else { "" },
                    func_name = func_name,
                    convert_code = convert_code,
                    single_args_with_types =
                        args_with_java_types(f_method, args_format_flags::EXTERNAL)?,
                    args_with_types = args_with_java_types(
                        f_method,
                        args_format_flags::USE_COMMA_IF_NEED | args_format_flags::INTERNAL
                    )?,
                    args = list_of_args_for_call_method(
                        f_method,
                        args_format_flags::COMMA_BEFORE | args_format_flags::INTERNAL
                    )?,
                ).map_err(&map_write_err)?;
            }
            MethodVariant::Constructor => {
                have_constructor = true;

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
                    ext_args_with_types =
                        args_with_java_types(f_method, args_format_flags::EXTERNAL)?,
                    args_with_types = args_with_java_types(f_method, args_format_flags::INTERNAL)?,
                    convert_code = convert_code,
                    args = list_of_args_for_call_method(f_method, args_format_flags::INTERNAL)?
                ).map_err(&map_write_err)?;
            }
        }
    }

    if have_methods && !have_constructor {
        return Err(format!(
            "package_name {}, class_name {}, have methods, but no constructor",
            package_name,
            class.name
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
        ).map_err(&map_write_err)?;
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
        ).map_err(&map_write_err)?;
    }

    file.write_all(class.foreigner_code.as_bytes())
        .map_err(&map_write_err)?;
    write!(file, "}}").map_err(&map_write_err)?;

    Ok(())
}

fn args_with_java_types(
    method: &ForeignMethodSignature,
    flags: ArgsFormatFlags,
) -> Result<String, String> {
    use std::fmt::Write;

    assert!(
        flags.contains(args_format_flags::INTERNAL) || flags.contains(args_format_flags::EXTERNAL)
    );

    let mut res = String::new();
    if flags.contains(args_format_flags::USE_COMMA_IF_NEED) && !method.input.is_empty() {
        write!(&mut res, ", ").map_err(fmt_write_err_map)?;
    }
    for (i, arg) in method.input.iter().enumerate() {
        let type_name =
            if flags.contains(args_format_flags::INTERNAL) && arg.java_need_conversation() {
                arg.java_transition_type.unwrap()
            } else {
                arg.name
            };
        if i == (method.input.len() - 1) {
            write!(&mut res, "{} a_{}", type_name, i)
        } else {
            write!(&mut res, "{} a_{}, ", type_name, i)
        }.map_err(&fmt_write_err_map)?;
    }
    Ok(res)
}

fn list_of_args_for_call_method(
    f_method: &ForeignMethodSignature,
    flags: ArgsFormatFlags,
) -> Result<String, String> {
    use std::fmt::Write;

    assert!(
        flags.contains(args_format_flags::INTERNAL) || flags.contains(args_format_flags::EXTERNAL)
    );

    let mut res = String::new();
    if f_method.input.is_empty() {
        return Ok(res);
    }

    if flags.contains(args_format_flags::COMMA_BEFORE) {
        res.push_str(", ");
    }

    for (i, arg) in f_method.input.iter().enumerate() {
        let need_conv = flags.contains(args_format_flags::INTERNAL) && arg.java_need_conversation();
        if i == (f_method.input.len() - 1) {
            if need_conv {
                write!(&mut res, "a_{}_0", i)
            } else {
                write!(&mut res, "a_{}", i)
            }
        } else if need_conv {
            write!(&mut res, "a_{}_0, ", i)
        } else {
            write!(&mut res, "a_{}, ", i)
        }.map_err(|err| format!("write fmt failed: {}", err))?;
    }

    Ok(res)
}

fn convert_code_for_method(f_method: &ForeignMethodSignature) -> String {
    let mut ret = String::new();
    for (i, arg) in f_method.input.iter().enumerate() {
        if let Some(java_code) = arg.java_convert(|| (format!("a_{}", i), format!("a_{}_0", i))) {
            ret.push_str(&java_code);
        }
    }
    ret
}