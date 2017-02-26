use std::io::Write;
use std::path::PathBuf;
use std::fs::File;
use std::error::Error;

use syntex_syntax::parse::{token};

use core::*;

pub fn generate_java_code(rust_java_types_map: &RustToJavaTypes, package_name: &str, class_name: &token::InternedString, methods: &[ForeignerMethod], output_dir: &str) {
    let mut path = PathBuf::from(output_dir);
    path.push(format!("{}.java", class_name));
    let display = path.display();

    let mut file = match File::create(&path) {
        Err(why) => panic!("couldn't create {}: {}",
                           display,
                           why.description()),
        Ok(file) => file,
    };
    write!(file,
"package {};
public final class {} {{
    private long mNativeObj;
", package_name, class_name).unwrap();

    let mut have_constructor = false;
    let mut have_methods = false;
    for method_it in methods.iter() {
        let exception_spec = if method_it.may_return_error { "throws Exception" } else { "" };
        match method_it.func_type {
            FuncVariant::StaticMethod => (),
            FuncVariant::Constructor => {
                have_constructor = true;
                write!(file,
"
    public {class_name}({args_with_types}) {exception_spec} {{
        mNativeObj = init({args});
    }}
    private static native long init({args_with_types}) {exception_spec};
",
                       class_name = class_name,
                       exception_spec = exception_spec,
                       args_with_types = method_it.args_with_java_types(false, rust_java_types_map),
                       args = method_it.args(false)).unwrap();
            }
            FuncVariant::Method => {
                have_methods = true;
                write!(file,
"
    public {return_type} {func_name}({single_args_with_types}) {{ return do_{func_name}(mNativeObj{args}); }}
    private static native {return_type} do_{func_name}(long me{func_args_with_types});
",
                       return_type = method_it.java_return_type(rust_java_types_map),
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
               package_name, class_name);
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
    write!(file, "}}").unwrap();
}
