use std::path::Path;
use std::fs::File;
use std::io;
use std::io::Write;

use super::{fmt_write_err_map, method_name, ForeignMethodSignature};
use {ForeignerClassInfo, ForeignerMethod, MethodVariant};

pub(in java_jni) fn generate_java_code(
    output_dir: &Path,
    package_name: &str,
    class: &ForeignerClassInfo,
    methods_sign: &[ForeignMethodSignature],
) -> Result<(), String> {
    let path = output_dir.join(format!("{}.java", class.name));
    let mut file = File::create(&path)
        .map_err(|err| format!("Couldn't create {:?}: {}", path, err))?;
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
        match method.variant {
            MethodVariant::StaticMethod => {
                let ret_type = f_method.output.name;
                write!(
                    file,
                    "
    {method_access} static native {ret_type} {func_name}({args_with_types}) {exception_spec};
",
                    method_access = method_access,
                    ret_type = ret_type,
                    func_name = method_name(&*method),
                    args_with_types = args_with_java_types(&f_method, false)?,
                    exception_spec = exception_spec,
                ).map_err(&map_write_err)?;
            }
            MethodVariant::Method(_) => {
                have_methods = true;
                let ret_type = f_method.output.name;
                write!(
                    file,
                    "
    {method_access} {ret_type} {method_name}({single_args_with_types}) {exception_spec} {{
         {return_code} {func_name}(mNativeObj{args});
    }}
    private static native {ret_type} {func_name}(long me{args_with_types}) {exception_spec};
",
                    method_access = method_access,
                    ret_type = ret_type,
                    method_name = method.short_name(),
                    exception_spec = exception_spec,
                    return_code = if ret_type != "void" { "return" } else { "" },
                    func_name = method_name(&*method),
                    single_args_with_types = args_with_java_types(&f_method, false)?,
                    args_with_types = args_with_java_types(&f_method, true)?,
                    args = list_of_args_for_call_method(&*method, true)?,
                ).map_err(&map_write_err)?;
            }
            MethodVariant::Constructor => {
                have_constructor = true;

                write!(
                    file,
                    "
    {method_access} {class_name}({args_with_types}) {exception_spec} {{
        mNativeObj = init({args});
    }}
    private static native long {func_name}({args_with_types}) {exception_spec};
",
                    method_access = method_access,
                    class_name = class.name,
                    exception_spec = exception_spec,
                    func_name = method_name(&*method),
                    args_with_types = args_with_java_types(&f_method, false)?,
                    args = list_of_args_for_call_method(&*method, false)?
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
    protected void finalize() {{ delete(); }}
    private static native void do_delete(long me);
    private long mNativeObj;
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

fn map_write_err(err: io::Error) -> String {
    format!("write failed: {}", err)
}

fn args_with_java_types(
    method: &ForeignMethodSignature,
    use_comma_if_need: bool,
) -> Result<String, String> {
    use std::fmt::Write;

    let mut res = String::new();
    if use_comma_if_need && !method.input.is_empty() {
        write!(&mut res, ", ").map_err(fmt_write_err_map)?;
    }
    for (i, type_name) in method.input.iter().map(|v| v.name).enumerate() {
        if i == (method.input.len() - 1) {
            write!(&mut res, "{} a_{}", type_name, i)
        } else {
            write!(&mut res, "{} a_{}, ", type_name, i)
        }.map_err(&fmt_write_err_map)?;
    }
    Ok(res)
}

fn list_of_args_for_call_method(
    method: &ForeignerMethod,
    comma_before: bool,
) -> Result<String, String> {
    use std::fmt::Write;

    let n = method.fn_decl.inputs.len();
    let skip_n = match method.variant {
        MethodVariant::Method(_) => 1,
        _ => 0,
    };
    let mut res = String::new();
    if skip_n >= n {
        return Ok(res);
    }

    if comma_before {
        res.push_str(", ");
    }
    let count = n - skip_n;
    for i in 0..count {
        if i == (count - 1) {
            write!(&mut res, "a_{}", i)
        } else {
            write!(&mut res, "a_{}, ", i)
        }.map_err(|err| format!("write fmt failed: {}", err))?;
    }

    Ok(res)
}
