use std::collections::HashMap;
use std::sync::atomic::{Ordering};
use std::ops::Deref;
use std::fmt::Write;

use syntex_syntax::util::small_vector::SmallVector;
use syntex_syntax::ext::base::{ExtCtxt, MacResult, MacEager};
use syntex_syntax::{parse, ast};
use syntex_syntax::parse::{token};
use syntex_syntax::ptr::P;
use syntex_syntax::print::pprust;

use core::*;
use jni::generate_func_name as jni_generate_func_name;
use COMMON_CODE_GENERATED;

fn unpack_if_type_is_result(ty: ast::Ty) -> ast::Ty {
    match ty.node {
        ast::TyKind::Path(_/*self info*/, ref path) => {
            debug!("unpack_if_type_is_result: path: {:?}, ident {:?}", path.segments, path.segments[0].identifier.name.as_str());
            path.segments.first().map(|v|
                                      if v.identifier.name.as_str() == "Result" {
                                          match v.parameters {
                                              ast::PathParameters::AngleBracketed(ref params) => {
                                                  params.types.first().map(|v| {
                                                      debug!("unpack_if_type_is_result: result param {:?}", *v);
                                                      let v = v.deref();
                                                      v.clone()
                                                  }).unwrap_or(ty.clone())
                                              }
                                              _ => ty.clone(),
                                          }
                                      } else {
                                          ty.clone()
                                      }).unwrap_or(ty.clone())
        }
        _ => ty,
    }
}

fn jni_func_args_for_decl(rust_java_types_map: &RustToJavaTypes, method: &ForeignerMethod, skip: usize) -> String {
    let mut buf = String::new();
    for (i, it) in method.in_out_type.inputs.iter().skip(skip).enumerate() {
        let type_name = pprust::ty_to_string(&*it.ty);
        let type_name = rust_java_types_map.get(type_name.as_str()).unwrap().jni_type_name;
        write!(&mut buf, "a_{}: {}, ", i, type_name).unwrap();
    }
    buf
}

fn jni_convert_args(rust_java_types_map: &RustToJavaTypes, method: &ForeignerMethod, skip_args: usize) -> String {
    let mut buf = String::new();
    for (i, it) in method.in_out_type.inputs.iter().skip(skip_args).enumerate() {
        let type_name = pprust::ty_to_string(&*it.ty);
        let th: &TypeHandler = get_type_handler(rust_java_types_map, type_name.as_str());
        if let Some(from_jni_converter) = th.from_jni_converter {
            write!(&mut buf, "{}", from_jni_converter(&format!("a_{}", i))).unwrap();
        }
    }
    buf
}

fn jni_result_type(rust_java_types_map: &RustToJavaTypes, method: &ForeignerMethod) -> String {
    match &method.in_out_type.output {
        &ast::FunctionRetTy::Default(_) => String::new(),
        &ast::FunctionRetTy::Ty(ref ret_type) =>
            format!("-> {}", get_type_handler(rust_java_types_map, pprust::ty_to_string(&*ret_type).as_str()).jni_type_name),
    }
}

fn jni_convert_output_type(rust_java_types_map: &RustToJavaTypes, method: &ForeignerMethod) -> String {
    match &method.in_out_type.output {
        &ast::FunctionRetTy::Default(_) => String::new(),
        &ast::FunctionRetTy::Ty(ref ret_type) => {
            let type_name = pprust::ty_to_string(&*ret_type);
            let th: &TypeHandler = get_type_handler(rust_java_types_map, type_name.as_str());
            if let Some(to_jni_converter) = th.to_jni_converter {
                to_jni_converter()
            } else {
                String::new()
            }
        }
    }
}

fn jni_convert_this_in_constructor(constructor_ret_type: &str, rust_self_type: &str) -> String {
    if constructor_ret_type.starts_with(&format!("Result<{},", rust_self_type)) {
        format!(r#"
  let this = match this {{
    Ok(val) => val,
    Err(msg) => {{
      jni_throw_exception(env, &msg);
      return 0;
    }}
  }};
"#)
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
    } else {
        unimplemented!();
    }
}

pub fn generate_rust_code<'cx>(cx: &'cx mut ExtCtxt, rust_self_type: &ast::Path, rust_java_types_map: &RustToJavaTypes, package_name: &str, class_name: &token::InternedString, methods: &[ForeignerMethod]) -> Box<MacResult> {
    let mut jni_methods = Vec::new();
    let constructor = methods.iter().find(|&x| if let FuncVariant::Constructor = x.func_type { true } else { false });
    let constructor_ret_type: Option<_> = constructor.map(|v| match &v.in_out_type.output {
        &ast::FunctionRetTy::Default(_) => panic!("{} {} constructor should return value",
                                                  package_name, class_name),
        &ast::FunctionRetTy::Ty(ref ret_type) => Some(ret_type.clone()),
    }).unwrap_or(None);

    let constructor_ret_type_for_method = constructor_ret_type.as_ref()
        .map(|v: &P<ast::Ty>| {
            let t: &ast::Ty = v.deref();
            unpack_if_type_is_result(t.clone())
        });

    let mut generated_func_names = HashMap::<String, usize>::new();
    for it in methods.iter() {
        let val_ref = generated_func_names.entry(match it.func_type {
            FuncVariant::StaticMethod => unimplemented!(),
            FuncVariant::Constructor => {
                "init".to_string()
            }
            FuncVariant::Method => {
                format!("do_{}", it.short_name())
            }
        });
        *val_ref.or_insert(0) += 1;
    }
    for it in methods.iter() {
        let java_input_args: Vec<&'static str> = it.in_out_type.inputs
            .iter()
            .skip(if it.func_type == FuncVariant::Method { 1 } else { 0 })
            .map(|v| get_type_handler(rust_java_types_map, pprust::ty_to_string(&*v.ty).as_str()).java_type_name)
            .collect();
        match it.func_type {
            FuncVariant::StaticMethod => unimplemented!(),
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
                           &package_name, &class_name, "init",
                           *generated_func_names.get("init").unwrap() > 1,
                           &java_input_args
                       ),
                       convert_this = jni_convert_this_in_constructor(&pprust::ty_to_string(&*constructor_ret_type), &rust_self_type.to_string()),
                       decl_func_args = jni_func_args_for_decl(rust_java_types_map, &*it, 0),
                       convert_jni_args = jni_convert_args(rust_java_types_map, &*it, 0),
                       create_jni_obj = it.full_name(),
                       jni_func_args = it.in_out_type.inputs.iter().enumerate().map(|a| format!("a_{}, ", a.0))
                       .fold(String::new(), |acc, x| acc + &x),
                ).unwrap();
                debug!("we generate and parse code: {}", code);
                jni_methods.push(parse::parse_item_from_source_str(
                    "constructor".to_string(), code, cx.cfg.clone(), cx.parse_sess).unwrap().unwrap());
            }
            FuncVariant::Method => {
                if constructor.is_none() {
                    panic!("package_name {}, class_name {}, have methods, but no constructor",
                           package_name, class_name);
                }
                if rust_self_type.segments.is_empty() {
                    panic!("package_name {}, class_name {} have constructor, but self_type not defined",
                           package_name, class_name);
                }
                let constructor_ret_type = constructor_ret_type_for_method.as_ref().unwrap().clone();

                let mut code = String::new();
                let gen_func_name = format!("do_{}", &it.short_name());
                write!(&mut code, r#"
#[allow(non_snake_case)]
#[no_mangle]
#[allow(unused_variables)]
pub fn {func_name}(env: *mut JNIEnv, _: jclass, this: jlong, {decl_func_args}) {jni_result_type} {{
{convert_jni_args}
    let this: &mut {this_type} = unsafe {{ jlong_to_pointer::<{this_type}>(this).as_mut().unwrap() }};
{convert_this}
    let ret = {rust_func_name}(&mut *this, {jni_func_args});
    {convert_output}
    ret
}}
"#,
                       func_name = jni_generate_func_name(&package_name, &class_name, &gen_func_name,
                                                           *generated_func_names.get(&gen_func_name).unwrap() > 1,
                                                           &java_input_args),
                       decl_func_args = jni_func_args_for_decl(rust_java_types_map, &*it, 1),
                       convert_jni_args = jni_convert_args(rust_java_types_map, &*it, 1),
                       jni_result_type = jni_result_type(rust_java_types_map, &*it),
                       this_type = pprust::ty_to_string(&constructor_ret_type),
                       convert_this = generate_rust_to_convert_this(&pprust::ty_to_string(&constructor_ret_type), &rust_self_type.to_string()),
                       rust_func_name = it.path,
                       jni_func_args = it.in_out_type.inputs.iter().skip(1).enumerate().map(|a| format!("a_{}, ", a.0))
                       .fold(String::new(), |acc, x| acc + &x),
                       convert_output = jni_convert_output_type(rust_java_types_map, &*it),
                ).unwrap();
                debug!("we generate and parse code: {}", code);
                jni_methods.push(parse::parse_item_from_source_str(
                    "method".to_string(), code, cx.cfg.clone(), cx.parse_sess).unwrap().unwrap());
            }
        }
    }

    if constructor.is_some() {
        let constructor_ret_type = constructor_ret_type_for_method.as_ref().unwrap().clone();
        let mut code = String::new();
        write!(&mut code, r#"
#[allow(non_snake_case)]
#[no_mangle]
pub fn {jni_destructor_name}(_: *mut JNIEnv, _: jclass, this: jlong) {{
    let this: &mut {type_name} = unsafe {{ jlong_to_pointer::<{type_name}>(this).as_mut().unwrap() }};
    let this = unsafe {{ Box::from_raw(this) }};
    drop(this)
}}
"#,
               jni_destructor_name = jni_generate_func_name(&package_name, &class_name, "do_delete", false, &[]),
               type_name = pprust::ty_to_string(&constructor_ret_type),
        ).unwrap();
        debug!("we generate and parse code: {}", code);
        jni_methods.push(parse::parse_item_from_source_str(
            "destructor".to_string(), code, cx.cfg.clone(), cx.parse_sess).unwrap().unwrap());
    }

    if !COMMON_CODE_GENERATED.swap(true, Ordering::SeqCst) {
        let mut parser = parse::new_parser_from_source_str(
            cx.parse_sess, cx.cfg.clone(), "addon".to_string(),
            r#"
#[cfg(target_pointer_width = "32")]
unsafe fn jlong_to_pointer<T>(val: jlong) -> *mut T {
    mem::transmute::<u32, *mut T>(val as u32)
}

#[cfg(target_pointer_width = "64")]
unsafe fn jlong_to_pointer<T>(val: jlong) -> *mut T {
    mem::transmute::<jlong, *mut T>(val)
}

struct JavaString {
    string: jstring,
    chars: *const ::std::os::raw::c_char,
    env: *mut JNIEnv
}
impl JavaString {
    fn new(env: *mut JNIEnv, js: jstring) -> JavaString {
        let chars = unsafe { (**env).GetStringUTFChars.unwrap()(env, js, ptr::null_mut()) };
        JavaString{string: js, chars: chars, env: env}
    }

    fn to_str(&self) -> &str {
        let s = unsafe { CStr::from_ptr(self.chars) };
        s.to_str().unwrap()
    }
}
impl Drop for JavaString {
    fn drop(&mut self) {
        assert!(self.env != ptr::null_mut() && self.chars != ptr::null_mut());
        unsafe { (**self.env).ReleaseStringUTFChars.unwrap()(self.env, self.string, self.chars) };
        self.env = ptr::null_mut();
        self.chars = ptr::null_mut();
    }
}

#[allow(dead_code)]
fn jni_throw(env: *mut JNIEnv, class_name: &'static str, message: &str) {
    let class_name_c = ::std::ffi::CString::new(class_name).unwrap();

    let ex_class = unsafe {
        (**env).FindClass.unwrap()(env, class_name_c.as_ptr())
    };
    if ex_class.is_null() {
        error!("throw_exception: can not find exp class {}, msg {}", class_name, message);
        return;
    }
    let c_message = ::std::ffi::CString::new(message).unwrap();
    let res = unsafe {
        (**env).ThrowNew.unwrap()(env, ex_class, c_message.as_ptr())
    };
    if res != 0 {
        error!("ThrowNew({}) for class {} failed", message, class_name);
    }
}

#[allow(dead_code)]
fn jni_throw_exception(env: *mut JNIEnv, message: &str) {
    jni_throw(env, "java/lang/Exception", message)
}
"#.to_string());
        let mut my_crate = parser.parse_crate_mod().unwrap();
        jni_methods.append(&mut my_crate.module.items);
    }
    MacEager::items(SmallVector::many(jni_methods))
}
