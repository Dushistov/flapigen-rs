use std::io::Write;
use std::path::PathBuf;
use std::fs::File;
use std::error::Error;

use core::*;

pub fn generate_java_code(rust_java_types_map: &RustToJavaTypes, class_info: &ForeignerClassInfo, output_dir: &str) {
    let mut path = PathBuf::from(output_dir);
    path.push(format!("{}.java", class_info.class_name));
    let display = path.display();

    let mut file = match File::create(&path) {
        Err(why) => panic!("couldn't create {}: {}",
                           display,
                           why.description()),
        Ok(file) => file,
    };
    write!(file,
"package {package_name};
public final class {class_name} {{
    private long mNativeObj;
", package_name = class_info.package_name, class_name = class_info.class_name).unwrap();

    let mut have_constructor = false;
    let mut have_methods = false;
    for method_it in class_info.methods.iter() {
        let exception_spec = if method_it.may_return_error { "throws Exception" } else { "" };
        let method_access = if method_it.private { "private" } else { "public" };
        match method_it.func_type {
            FuncVariant::StaticMethod => {
                let return_type = method_it.java_return_type(rust_java_types_map);
                write!(file,
"
    {method_access} static native {return_type} {func_name}({func_args_with_types}) {exception_spec};
",
                       method_access = method_access,
                       return_type = return_type,
                       func_name = method_it.short_name(),
                       func_args_with_types  = method_it.args_with_java_types(false, rust_java_types_map),
                       exception_spec = exception_spec,
                ).unwrap();
            }
            FuncVariant::Constructor => {
                have_constructor = true;
                write!(file,
"
    {method_access} {class_name}({args_with_types}) {exception_spec} {{
        mNativeObj = init({args});
    }}
    private static native long init({args_with_types}) {exception_spec};
",
                       method_access = method_access,
                       class_name = class_info.class_name,
                       exception_spec = exception_spec,
                       args_with_types = method_it.args_with_java_types(false, rust_java_types_map),
                       args = method_it.args(false)).unwrap();
            }
            FuncVariant::Method => {
                have_methods = true;
                let return_type = method_it.java_return_type(rust_java_types_map);
                write!(file,
"
    {method_access} {return_type} {func_name}({single_args_with_types}) {exception_spec} {{ {return_code} do_{func_name}(mNativeObj{args}); }}
    private static native {return_type} do_{func_name}(long me{func_args_with_types}) {exception_spec};
",
                       method_access = method_access,
                       return_type = return_type,
                       exception_spec = exception_spec,
                       return_code = if return_type != "void" { "return" } else { "" },
                       func_name = method_it.short_name(),
                       single_args_with_types = method_it.args_with_java_types(false, rust_java_types_map),
                       func_args_with_types  = method_it.args_with_java_types(true, rust_java_types_map),
                       args = method_it.args(true),
                ).unwrap();
            }
        }
    }
    if have_methods && !have_constructor {
        panic!("package_name {}, class_name {}, have methods, but no constructor",
               class_info.package_name, class_info.class_name);
    }
    if have_constructor {
        write!(file,
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
").unwrap();
    }
    file.write_all(class_info.foreigner_code.as_bytes()).unwrap();
    write!(file, "}}").unwrap();
}
