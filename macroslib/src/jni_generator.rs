use std::collections::HashMap;
use std::path::Path;
use std::fs::File;
use std::error::Error;
use std::fmt::Write;
use std::iter::Iterator;

use syntex_syntax::ext::base::ExtCtxt;
use syntex_syntax::{parse, ast, ptr};
use syntex_syntax::print::pprust;

use core::*;

lazy_static! {
    static ref JAVA_TYPE_NAMES_FOR_JNI_SIGNATURE: HashMap<&'static str, &'static str> = {
        let mut m = HashMap::new();
        m.insert("String", "Ljava.lang.String;");
        m.insert("boolean", "Z");
        m.insert("byte", "B");
        m.insert("char", "C");
        m.insert("double", "D");
        m.insert("float", "F");
        m.insert("int", "I");
        m.insert("long", "J");
        m.insert("object", "L");
        m.insert("short", "S");
        m.insert("void", "V");
        m
    };
}

fn jni_generate_func_name<'a, IterType>(package_name: &str,
                                        class_name: &str,
                                        func_name: &str,
                                        overloaded: bool,
                                        args_types_iter: IterType)
                                        -> String
    where IterType: Iterator<Item = &'a String>
{
    let mut output = String::new();
    output.push_str("Java_");
    fn escape_underscore(input: &str, mut output: &mut String) {
        for c in input.chars() {
            match c {
                '.' => output.push('_'),
                '[' => output.push_str("_3"),
                '_' => output.push_str("_1"),
                ';' => output.push_str("_2"),
                _ => output.push(c),
            }
        }
    }
    escape_underscore(package_name, &mut output);
    output.push_str("_");
    escape_underscore(class_name, &mut output);
    output.push_str("_");
    escape_underscore(func_name, &mut output);

    if overloaded {
        output.push_str("__");
        for it in args_types_iter {
            escape_underscore(JAVA_TYPE_NAMES_FOR_JNI_SIGNATURE
                                  .get(it.as_str())
                                  .expect(&format!("jni gen func name: Unknown Java type `{}`",
                                                   *it)),
                              &mut output);
        }
    }

    output
}


pub static RUST_OBJECT_TO_JOBJECT: &'static str = r#"
  let class_id = ::std::ffi::CString::new("{full_class_name}").unwrap();
  let jcls: jclass = unsafe { (**env).FindClass.unwrap()(env, class_id.as_ptr()) };
  assert!(!jcls.is_null());
  let jobj: jobject = unsafe { (**env).AllocObject.unwrap()(env, jcls) };
  assert!(!jobj.is_null());
  let field_id = ::std::ffi::CString::new("mNativeObj").unwrap();
  let type_id = ::std::ffi::CString::new("J").unwrap();
  let field_id: jfieldID = unsafe {
      (**env).GetFieldID.unwrap()(env, jcls, field_id.as_ptr(), type_id.as_ptr())
  };
  assert!(!field_id.is_null());
  let b: Box<{rust_type_name}> = Box::new(ret);
  let ret = Box::into_raw(b) as jlong;
  unsafe {
    (**env).SetLongField.unwrap()(env, jobj, field_id, ret);
    if (**env).ExceptionCheck.unwrap()(env) != 0 {
      panic!("Can not mNativeObj field: catch exception");
    }
  }
  let ret = jobj;
"#;

pub static RUST_VEC_TO_JAVA_ARRAY: &'static str = r#"
    let class_id = ::std::ffi::CString::new("{full_class_name}").unwrap();
    let jcls: jclass = unsafe { (**env).FindClass.unwrap()(env, class_id.as_ptr()) };
    assert!(!jcls.is_null());
    let obj_arr: jobjectArray = unsafe {
        (**env).NewObjectArray.unwrap()(env, {vec_name}.len() as jsize,
                                        jcls, ::std::ptr::null_mut())
    };
    assert!(!obj_arr.is_null());

    let field_id = ::std::ffi::CString::new("mNativeObj").unwrap();
    let type_id = ::std::ffi::CString::new("J").unwrap();
    let field_id: jfieldID = unsafe {
        (**env).GetFieldID.unwrap()(env, jcls, field_id.as_ptr(), type_id.as_ptr())
    };
    assert!(!field_id.is_null());

    for (i, r_obj) in {vec_name}.drain(..).enumerate() {
        let jobj: jobject = unsafe { (**env).AllocObject.unwrap()(env, jcls) };
        assert!(!jobj.is_null());

        let r_obj = Box::into_raw(Box::new(r_obj)) as jlong;
        unsafe {
            (**env).SetLongField.unwrap()(env, jobj, field_id, r_obj);
            if (**env).ExceptionCheck.unwrap()(env) != 0 {
                panic!("Can not mNativeObj field: catch exception");
            }
            (**env).SetObjectArrayElement.unwrap()(env, obj_arr, i as jsize, jobj);
            if (**env).ExceptionCheck.unwrap()(env) != 0 {
                panic!("SetObjectArrayElement({}) failed", i);
            }
            (**env).DeleteLocalRef.unwrap()(env, jobj);
        }
    }
    let ret = obj_arr;
"#;

pub static RUST_RESULT_TO_JAVA_OBJECT: &'static str = r#"
  let ret = match ret {
    Ok(val) => val,
    Err(msg) => {
      jni_throw_exception(env, &msg);
      return {default_value};
    }
  };
"#;

fn jni_func_args_for_decl(rust_java_types_map: &RustToJavaTypes,
                          method: &ForeignerMethod,
                          skip: usize)
                          -> String {
    let mut buf = String::new();
    for (i, it) in method
            .in_out_type
            .inputs
            .iter()
            .skip(skip)
            .enumerate() {
        let type_name = pprust::ty_to_string(&*it.ty);
        write!(&mut buf, "a_{}: {}, ", i, get_type_handler(rust_java_types_map,
            type_name.as_str())
            .jni_type_name).unwrap();
    }
    buf
}

fn jni_convert_args(rust_java_types_map: &RustToJavaTypes,
                    method: &ForeignerMethod,
                    skip_args: usize)
                    -> String {
    let mut buf = String::new();
    for (i, it) in method
            .in_out_type
            .inputs
            .iter()
            .skip(skip_args)
            .enumerate() {
        let type_name = pprust::ty_to_string(&*it.ty);
        let th: &TypeHandler = get_type_handler(rust_java_types_map, type_name.as_str());
        if let Some(ref from_jni_converter) = th.from_jni_converter {
            write!(&mut buf,
                   "{}",
                   from_jni_converter.apply(&format!("a_{}", i)))
                    .unwrap();
        }
    }
    buf
}

fn jni_result_type(rust_java_types_map: &RustToJavaTypes, method: &ForeignerMethod) -> String {
    match &method.in_out_type.output {
        &ast::FunctionRetTy::Default(_) => String::new(),
        &ast::FunctionRetTy::Ty(ref ret_type) => {
            let jni_type_name = &get_type_handler(rust_java_types_map,
                                                 pprust::ty_to_string(&*ret_type).as_str())
                    .jni_type_name;
            if jni_type_name.is_empty() {
                String::new()
            } else {
                format!("-> {}", jni_type_name)
            }
        }
    }
}

fn jni_convert_output_type<'a>(rust_java_types_map: &'a RustToJavaTypes,
                               method: &'a ForeignerMethod)
                               -> &'a str {
    match &method.in_out_type.output {
        &ast::FunctionRetTy::Default(_) => "",
        &ast::FunctionRetTy::Ty(ref ret_type) => {
            let type_name = pprust::ty_to_string(&*ret_type);
            let th: &TypeHandler = get_type_handler(rust_java_types_map, type_name.as_str());
            if let Some(ref to_jni_converter) = th.to_jni_converter {
                to_jni_converter.apply()
            } else {
                ""
            }
        }
    }
}

fn jni_convert_this_in_constructor(constructor_ret_type: &str, rust_self_type: &str) -> String {
    if constructor_ret_type.starts_with(&format!("Result<{},", rust_self_type)) {
        r#"
  let this = match this {
    Ok(val) => val,
    Err(msg) => {
      jni_throw_exception(env, &msg);
      return 0;
    }
  };
"#
                .into()
    } else {
        String::new()
    }
}

fn generate_rust_to_convert_this(constructor_ret_type: &str, rust_self_type: &str) -> String {
    if constructor_ret_type == rust_self_type {
        return String::new();
    }
    if format!("Arc<Mutex<{}>>", rust_self_type) == constructor_ret_type {
        return format!("let mut this = this.lock().unwrap();\n");
    } else if format!("Rc<RefCell<{}>>", rust_self_type) == constructor_ret_type {
        return format!("let mut this = this.borrow_mut();\n");
    } else if format!("Arc<{}>", rust_self_type) == constructor_ret_type {
        return String::new();
    } else {
        panic!("Not implemented conversation from '{}' to '{}'",
               constructor_ret_type,
               rust_self_type);
    }
}

pub fn generate_rust_code<'cx>(cx: &'cx mut ExtCtxt,
                               rust_java_types_map: &RustToJavaTypes,
                               class_info: &ForeignerClassInfo)
                               -> Vec<ptr::P<ast::Item>> {
    let mut jni_methods = Vec::new();
    let constructor = class_info
        .methods
        .iter()
        .find(|&x| if let FuncVariant::Constructor = x.func_type {
                  true
              } else {
                  false
              });
    let constructor_ret_type = class_info.constructor_ret_type.as_ref();

    let constructor_ret_type_for_method = class_info.this_type_for_method.as_ref();

    let mut generated_func_names = HashMap::<String, usize>::new();
    for it in class_info.methods.iter() {
        let val_ref =
            generated_func_names.entry(match it.func_type {
                                           FuncVariant::StaticMethod => {
                let func_name: &str = &*(it.short_name());
                func_name.into()
            }
                                           FuncVariant::Constructor => "init".to_string(),
                                           FuncVariant::Method => format!("do_{}", it.short_name()),
                                       });
        *val_ref.or_insert(0) += 1;
    }
    for it in class_info.methods.iter() {
        let java_input_args_iter = it.in_out_type
            .inputs
            .iter()
            .skip(if it.func_type == FuncVariant::Method {
                      1
                  } else {
                      0
                  })
            .map(|v| {
                     &get_type_handler(rust_java_types_map,
                                           &pprust::ty_to_string(&*v.ty).as_str())
                                  .java_type_name
                 });
        match it.func_type {
            FuncVariant::StaticMethod => {
                let gen_func_name: &str = &*(it.short_name());
                let mut code = String::new();
                write!(&mut code, r#"
#[allow(non_snake_case, unused_variables, unused_mut)]
#[no_mangle]
pub fn {func_name}(env: *mut JNIEnv, _: jclass, {decl_func_args}) {jni_result_type} {{
{convert_jni_args}
    let mut ret = {rust_func_name}({jni_func_args});
    {convert_output}
    ret
}}
"#,
                       func_name = jni_generate_func_name(
                           &class_info.package_name, &class_info.class_name,
                           &gen_func_name,
                           *generated_func_names.get(gen_func_name).unwrap() > 1,
                           java_input_args_iter),
                       decl_func_args = jni_func_args_for_decl(rust_java_types_map, &*it, 0),
                       convert_jni_args = jni_convert_args(rust_java_types_map, &*it, 0),
                       jni_result_type = jni_result_type(rust_java_types_map, &*it),
                       rust_func_name = it.path,
                       jni_func_args = it.in_out_type.inputs
                       .iter()
                       .enumerate()
                       .map(|a| format!("a_{}, ", a.0))
                       .fold(String::new(), |acc, x| acc + &x),
                       convert_output = jni_convert_output_type(rust_java_types_map, &*it),
                )
                        .unwrap();
                debug!("we generate and parse code: {}", code);
                jni_methods.push(parse::parse_item_from_source_str("method".to_string(),
                                                                   code,
                                                                   cx.parse_sess)
                                         .unwrap()
                                         .unwrap());
            }
            FuncVariant::Constructor => {
                let constructor_ret_type = constructor_ret_type.as_ref().unwrap().clone();
                let mut code = String::new();
                write!(&mut code, r#"
#[allow(non_snake_case)]
#[no_mangle]
#[allow(unused_variables)]
pub fn {func_name}(env: *mut JNIEnv, _: jclass, {decl_func_args}) -> jlong {{
{convert_jni_args}
  let this = {create_jni_obj}({jni_func_args});
{convert_this}
  Box::into_raw(Box::new(this)) as jlong
}}
"#,
                       func_name = jni_generate_func_name(
                           &class_info.package_name, &class_info.class_name, "init",
                           *generated_func_names.get("init").unwrap() > 1,
                           java_input_args_iter
                       ),
                       convert_this = jni_convert_this_in_constructor(
                           &pprust::ty_to_string(&constructor_ret_type),
                           &class_info.self_rust_type.to_string()),
                       decl_func_args = jni_func_args_for_decl(rust_java_types_map, &*it, 0),
                       convert_jni_args = jni_convert_args(rust_java_types_map, &*it, 0),
                       create_jni_obj = it.full_name(),
                       jni_func_args = it.in_out_type.inputs
                       .iter()
                       .enumerate()
                       .map(|a| format!("a_{}, ", a.0))
                       .fold(String::new(), |acc, x| acc + &x),
                )
                        .unwrap();
                debug!("we generate and parse code: {}", code);
                jni_methods.push(parse::parse_item_from_source_str("constructor".to_string(),
                                                                   code,
                                                                   cx.parse_sess)
                                         .unwrap()
                                         .unwrap());
            }
            FuncVariant::Method => {
                if constructor.is_none() {
                    panic!("package_name {}, class_name {}, have methods, but no constructor",
                           class_info.package_name,
                           class_info.class_name);
                }
                if class_info.self_rust_type.segments.is_empty() {
                    panic!("package_name {}, class_name {} have constructor, but self_type \
                            not defined",
                           class_info.package_name,
                           class_info.class_name);
                }
                let constructor_ret_type = constructor_ret_type_for_method
                    .as_ref()
                    .unwrap()
                    .clone();

                let mut code = String::new();
                let gen_func_name = format!("do_{}", &it.short_name());
                write!(&mut code, r#"
#[allow(non_snake_case, unused_variables, unused_mut)]
#[no_mangle]
pub fn {func_name}(env: *mut JNIEnv, _: jclass, this: jlong, {decl_func_args}) {jni_result_type} {{
{convert_jni_args}
    let this: &mut {this_type} = unsafe {{
        jlong_to_pointer::<{this_type}>(this).as_mut().unwrap()
    }};
{convert_this}
    let mut ret = {rust_func_name}(&mut *this, {jni_func_args});
    {convert_output}
    ret
}}
"#,
                       func_name = jni_generate_func_name(
                           &class_info.package_name, &class_info.class_name, &gen_func_name,
                           *generated_func_names.get(&gen_func_name).unwrap() > 1,
                           java_input_args_iter),
                       decl_func_args = jni_func_args_for_decl(rust_java_types_map, &*it, 1),
                       convert_jni_args = jni_convert_args(rust_java_types_map, &*it, 1),
                       jni_result_type = jni_result_type(rust_java_types_map, &*it),
                       this_type = pprust::ty_to_string(&constructor_ret_type),
                       convert_this = generate_rust_to_convert_this(
                           &pprust::ty_to_string(&constructor_ret_type),
                           &class_info.self_rust_type.to_string()),
                       rust_func_name = it.path,
                       jni_func_args = it.in_out_type.inputs
                       .iter().skip(1).enumerate().map(|a| format!("a_{}, ", a.0))
                       .fold(String::new(), |acc, x| acc + &x),
                       convert_output = jni_convert_output_type(rust_java_types_map, &*it),
                )
                        .unwrap();
                debug!("we generate and parse code: {}", code);
                jni_methods.push(parse::parse_item_from_source_str("method".to_string(),
                                                                   code,
                                                                   cx.parse_sess)
                                         .unwrap()
                                         .unwrap());
            }
        }
    }

    if constructor.is_some() {
        let constructor_ret_type = constructor_ret_type_for_method
            .as_ref()
            .unwrap()
            .clone();
        let mut code = String::new();
        write!(&mut code, r#"
#[allow(non_snake_case)]
#[no_mangle]
pub fn {jni_destructor_name}(_: *mut JNIEnv, _: jclass, this: jlong) {{
    let this: &mut {type_name} = unsafe {{
        jlong_to_pointer::<{type_name}>(this).as_mut().unwrap()
    }};
    let this = unsafe {{ Box::from_raw(this) }};
    drop(this)
}}
"#,
               jni_destructor_name = jni_generate_func_name(
                   &class_info.package_name, &class_info.class_name,
                   "do_delete", false, ::std::iter::empty::<&String>()),
               type_name = pprust::ty_to_string(&constructor_ret_type),
        )
                .unwrap();
        debug!("we generate and parse code: {}", code);
        jni_methods.push(parse::parse_item_from_source_str("destructor".to_string(),
                                                           code,
                                                           cx.parse_sess)
                                 .unwrap()
                                 .unwrap());
    }

    jni_methods
}

pub fn generate_java_code(rust_java_types_map: &RustToJavaTypes,
                          class_info: &ForeignerClassInfo,
                          output_dir: &Path) {
    use std::io::Write;
    
    let path = output_dir.join(format!("{}.java", class_info.class_name));
    let display = path.display();

    let mut file = match File::create(&path) {
        Err(why) => panic!("couldn't create {}: {}", display, why.description()),
        Ok(file) => file,
    };
    write!(file,
           "package {package_name};
public final class {class_name} {{
    private long mNativeObj;
",
           package_name = class_info.package_name,
           class_name = class_info.class_name)
            .unwrap();

    let mut have_constructor = false;
    let mut have_methods = false;
    for method_it in class_info.methods.iter() {
        let exception_spec = if method_it.may_return_error {
            "throws Exception"
        } else {
            ""
        };
        let method_access = if method_it.private {
            "private"
        } else {
            "public"
        };
        match method_it.func_type {
            FuncVariant::StaticMethod => {
                let return_type = method_it.java_return_type(rust_java_types_map);
                write!(file,
"
    {access} static native {ret_type} {func_name}({args_with_types}) {exception_spec};
",
                       access = method_access,
                       ret_type = return_type,
                       func_name = method_it.short_name(),
                       args_with_types  = method_it.args_with_java_types(false,
                                                                         rust_java_types_map),
                       exception_spec = exception_spec,
                )
                        .unwrap();
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
                       args_with_types =
                           method_it.args_with_java_types(false, rust_java_types_map),
                       args = method_it.args(false))
                        .unwrap();
            }
            FuncVariant::Method => {
                have_methods = true;
                let return_type = method_it.java_return_type(rust_java_types_map);
                write!(file,
"
    {access} {ret_type} {func_name}({single_args_with_types}) {exception_spec} {{
         {return_code} do_{func_name}(mNativeObj{args});
    }}
    private static native {ret_type} do_{func_name}(long me{args_with_types}) {exception_spec};
",
                       access = method_access,
                       ret_type = return_type,
                       exception_spec = exception_spec,
                       return_code = if return_type != "void" { "return" } else { "" },
                       func_name = method_it.short_name(),
                       single_args_with_types = method_it.args_with_java_types(false,
                                                                               rust_java_types_map),
                       args_with_types  = method_it.args_with_java_types(true, rust_java_types_map),
                       args = method_it.args(true),
                )
                        .unwrap();
            }
        }
    }
    if have_methods && !have_constructor {
        panic!("package_name {}, class_name {}, have methods, but no constructor",
               class_info.package_name,
               class_info.class_name);
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
")
                .unwrap();
    }
    file.write_all(class_info.foreigner_code.as_bytes())
        .unwrap();
    write!(file, "}}").unwrap();
}
