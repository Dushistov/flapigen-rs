extern crate syntex;
extern crate syntex_syntax;
extern crate syntex_pos;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;

mod parse_utils;
mod jni;

use std::collections::HashMap;
use std::env;
use std::path::PathBuf;
use std::fs::File;
use std::error::Error;
use std::fmt::Write;
use std::sync::atomic::{AtomicBool, Ordering};

use syntex::Registry;
use syntex_syntax::ext::base::{ExtCtxt, MacResult, DummyResult, MacEager};
use syntex_syntax::parse::{token, parser};
use syntex_syntax::tokenstream::TokenTree;
use syntex_syntax::codemap::Span;
use syntex_syntax::{parse, ast};
use syntex_syntax::ptr::P;
use syntex_syntax::util::small_vector::SmallVector;
use syntex_syntax::print::pprust;

use parse_utils::*;

#[derive(PartialEq)]
enum FuncVariant {
    Constructor, Method, StaticMethod
}

impl FuncVariant {
    fn from_str(ident: &token::InternedString) -> Option<FuncVariant> {
        if ident == "constructor" {
            Some(FuncVariant::Constructor)
        } else if ident == "method" {
            Some(FuncVariant::Method)
        } else if ident == "static_method" {
            Some(FuncVariant::StaticMethod)
        } else {
            None
        }
    }
}

struct ForeignerMethod {
    func_type: FuncVariant,
    path: ast::Path,
    in_out_type: P<ast::FnDecl>,
    name_alias: Option<token::InternedString>,
}

struct TypeHandler {
    rust_type_name: &'static str,
    jni_type_name: &'static str,
    java_type_name: &'static str,
    from_jni_converter: Option<fn(&str)->String>,
    to_jni_converter: Option<fn() -> String>,
}

type RustToJavaTypes<'a> = HashMap<&'static str, &'a TypeHandler>;

fn get_type_handler<'a, 'b>(types_map: &RustToJavaTypes<'a>, name: &'b str) -> &'a TypeHandler {
    types_map.get(name).expect(&format!("Unknown type `{}`", name))
}

impl ForeignerMethod {
    fn args(&self, use_comma_if_need: bool) -> String {
        let skip_n =  if self.func_type == FuncVariant::Method { 1 } else { 0 };
        let mut res = String::new();
        if use_comma_if_need && skip_n < self.in_out_type.inputs.len() {
            write!(&mut res, ", ").unwrap();
        }
        for (i, _) in self.in_out_type.inputs.iter().skip(skip_n).enumerate() {
            if i == (self.in_out_type.inputs.len() - 1 - skip_n) {
                write!(&mut res, "a_{}", i)
            } else {
                write!(&mut res, "a_{}, ", i)
            }
            .unwrap();
        }
        res
    }

    fn args_with_java_types(&self, use_comma_if_need: bool, types_map: &RustToJavaTypes) -> String {
        let skip_n =  if self.func_type == FuncVariant::Method { 1 } else { 0 };
        let mut res = String::new();
        if use_comma_if_need && skip_n < self.in_out_type.inputs.len() {
            write!(&mut res, ", ").unwrap();
        }
        for (i, item_arg) in self.in_out_type.inputs.iter().skip(skip_n).enumerate() {
            let type_name = get_type_handler(types_map, pprust::ty_to_string(&*item_arg.ty).as_str()).java_type_name;
            if i == (self.in_out_type.inputs.len() - 1 - skip_n) {
                write!(&mut res, "{} a_{}", type_name, i)
            } else {
                write!(&mut res, "{} a_{}, ", type_name, i)
            }
            .unwrap();
        }
        res
    }

    fn java_return_type(&self, types_map: &RustToJavaTypes) -> &'static str {
        match &self.in_out_type.output {
            &ast::FunctionRetTy::Default(_) => "void",
            &ast::FunctionRetTy::Ty(ref ret_type) =>
                get_type_handler(types_map, pprust::ty_to_string(&*ret_type).as_str()).java_type_name
        }
    }

    fn short_name(&self) -> token::InternedString {
        if let Some(ref name) = self.name_alias {
            name.clone()
        } else {
            match self.path.segments.len() {
                0 => token::InternedString::new(""),
                n => self.path.segments[n - 1].identifier.name.as_str()
            }
        }
    }

    fn full_name(&self) -> String {
        format!("{}", self.path)
    }
}

pub fn register(registry: &mut Registry) {
    registry.add_macro("foreigner_class", expand_foreigner_class);
}

fn generate_java_code(rust_java_types_map: &RustToJavaTypes, package_name: &str, class_name: &token::InternedString, methods: &[ForeignerMethod], output_dir: &str) {
    use std::io::Write;
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
        match method_it.func_type {
            FuncVariant::StaticMethod => (),
            FuncVariant::Constructor => {
                have_constructor = true;
                write!(file,
"
    public {class_name}({args_with_types}) {{
        mNativeObj = init({args});
    }}
    private static native long init({args_with_types});
",
                       class_name = class_name,
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

lazy_static! {
    static ref COMMON_CODE_GENERATED: AtomicBool = AtomicBool::new(false);
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

fn generate_rust_code<'cx>(cx: &'cx mut ExtCtxt, rust_self_type: &ast::Path, rust_java_types_map: &RustToJavaTypes, package_name: &str, class_name: &token::InternedString, methods: &[ForeignerMethod]) -> Box<MacResult> {
    let mut jni_methods = Vec::new();
    let constructor = methods.iter().find(|&x| if let FuncVariant::Constructor = x.func_type { true } else { false });
    let constructor_ret_type: Option<_> = constructor.map(|v| match &v.in_out_type.output {
        &ast::FunctionRetTy::Default(_) => panic!("{} {} constructor should return value",
                                                  package_name, class_name),
        &ast::FunctionRetTy::Ty(ref ret_type) => Some(ret_type.clone()),
    }).unwrap_or(None);
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
                let mut code = String::new();
                write!(&mut code, r#"
#[allow(non_snake_case)]
#[no_mangle]
#[allow(unused_variables)]
pub fn {func_name}(env: *mut JNIEnv, _: jclass, {decl_func_args}) -> jlong {{
{convert_jni_args}
  let this = {create_jni_obj}({jni_func_args});
  Box::into_raw(Box::new(this)) as jlong
}}
"#,
                       func_name = jni::generate_func_name(
                           &package_name, &class_name, "init",
                           *generated_func_names.get("init").unwrap() > 1,
                           &java_input_args
                       ),
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
                let constructor_ret_type = constructor_ret_type.as_ref().unwrap().clone();

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
                       func_name = jni::generate_func_name(&package_name, &class_name, &gen_func_name,
                                                           *generated_func_names.get(&gen_func_name).unwrap() > 1,
                                                           &java_input_args),
                       decl_func_args = jni_func_args_for_decl(rust_java_types_map, &*it, 1),
                       convert_jni_args = jni_convert_args(rust_java_types_map, &*it, 1),
                       jni_result_type = jni_result_type(rust_java_types_map, &*it),
                       this_type = pprust::ty_to_string(&*constructor_ret_type),
                       convert_this = generate_rust_to_convert_this(&pprust::ty_to_string(&*constructor_ret_type), &rust_self_type.to_string()),
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
        let constructor_ret_type = constructor_ret_type.as_ref().unwrap().clone();
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
               jni_destructor_name = jni::generate_func_name(&package_name, &class_name, "do_delete", false, &[]),
               type_name = pprust::ty_to_string(&*constructor_ret_type),
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
"#.to_string());
        let mut my_crate = parser.parse_crate_mod().unwrap();
        jni_methods.append(&mut my_crate.module.items);
    }
    MacEager::items(SmallVector::many(jni_methods))
}

fn jstring_to_str(arg_name: &str) -> String {
    format!(r#"
  let {arg_name} = JavaString::new(env, {arg_name});
  let {arg_name} = {arg_name}.to_str();
"#, arg_name = arg_name)
}

fn str_to_jstring() -> String {
    r#"
  let ret = ::std::ffi::CString::new(ret).unwrap();
  let ret = unsafe { (**env).NewStringUTF.unwrap()(env, ret.as_ptr()) };
"#.to_string()
}

fn jstring_to_path(arg_name: &str) -> String {
    format!(r#"
  let {arg_name} = JavaString::new(env, {arg_name});
  let {arg_name} = Path::new({arg_name}.to_str());
"#, arg_name = arg_name)
}

fn jboolean_to_bool(arg_name: &str) -> String {
    format!(r#"
  let {arg_name} = {arg_name} != 0;
"#, arg_name = arg_name)
}

fn bool_to_jboolean() -> String {
    "let ret = if ret { 1 as jboolean } else { 0 as jboolean };".to_string()
}

fn expand_foreigner_class<'cx>(cx: &'cx mut ExtCtxt,
                               _: Span,
                               tokens: &[TokenTree])
                               -> Box<MacResult + 'cx> {
    let mut parser = parse::new_parser_from_tts(cx.parse_sess, cx.cfg.clone(), tokens.to_vec());
    let class_ident = parser.parse_ident().unwrap();
    if class_ident.name.as_str() != "class" {
        debug!("class_indent {:?}", class_ident);
        cx.span_err(parser.span, "expect class here");
        return DummyResult::any(parser.span);
    }
    let class_name_indent = parser.parse_ident().unwrap();
    debug!("CLASS NAME {:?}", class_name_indent);
    parser.expect(&token::Token::OpenDelim(token::DelimToken::Brace)).unwrap();
    let mut methods = Vec::new();
    let mut rust_self_type = ast::Path{span: parser.span, global: false, segments: Vec::new()};
    let alias = ast::Ident::with_empty_ctxt(token::intern("alias"));
    loop {
        if parser.eat(&token::Token::CloseDelim(token::DelimToken::Brace)) {
            break;
        }
        let func_type_name = parser.parse_ident().unwrap();
        debug!("func_type {:?}", func_type_name);
        if func_type_name.name.as_str() == "self_type" {
            rust_self_type = parser.parse_path(parser::PathStyle::Type).unwrap();
            debug!("self_type: {:?}", rust_self_type);
            parser.expect(&token::Token::Semi).unwrap();
            continue;
        }
        let func_type = FuncVariant::from_str(&func_type_name.name.as_str());
        if func_type.is_none() {
            println!("unknown func type: {:?}", func_type_name);
            cx.span_err(parser.span, "expect 'constructor' or 'method' or 'static_method' here");
            return DummyResult::any(parser.span);
        }
        let func_type = func_type.unwrap();
        let func_name = parser.parse_path(parser::PathStyle::Mod).unwrap();
        debug!("func_name {:?}", func_name);

        let func_decl = match func_type {
            FuncVariant::Constructor | FuncVariant::StaticMethod => parser.parse_fn_decl(false).unwrap(),
            FuncVariant::Method => parse_fn_decl_with_self(&mut parser, |p| p.parse_arg()).unwrap(),
        };
        debug!("func_decl {:?}", func_decl);
        parser.expect(&token::Token::Semi).unwrap();
        let mut func_name_alias = None;
        if parser.eat_contextual_keyword(alias) {
            if func_type == FuncVariant::Constructor {
                cx.span_err(parser.span, "alias not supported for 'constructor'");
                return DummyResult::any(parser.span);
            }
            func_name_alias = Some(parser.parse_ident().unwrap());
            debug!("we have ALIAS `{:?}`", func_name_alias.unwrap());
            parser.expect(&token::Token::Semi).expect("no ; at the end of alias");
        }
        methods.push(ForeignerMethod{
            func_type: func_type, path: func_name, in_out_type: func_decl,
            name_alias: func_name_alias.map(|v| v.name.as_str()),
        });
    }
    let type_handlers = vec![
        TypeHandler{rust_type_name: "bool", jni_type_name: "jboolean", java_type_name: "boolean",
                    from_jni_converter: Some(jboolean_to_bool), to_jni_converter: Some(bool_to_jboolean)},
        TypeHandler{rust_type_name: "i32", jni_type_name: "jint", java_type_name: "int",
                    from_jni_converter: None, to_jni_converter: None},
        TypeHandler{rust_type_name: "f32", jni_type_name: "jfloat", java_type_name: "float",
                    from_jni_converter: None, to_jni_converter: None},
        TypeHandler{rust_type_name: "f64", jni_type_name: "jdouble", java_type_name: "double",
                    from_jni_converter: None, to_jni_converter: None},
        TypeHandler{rust_type_name: "&str", jni_type_name: "jstring", java_type_name: "String",
                    from_jni_converter: Some(jstring_to_str), to_jni_converter: Some(str_to_jstring)},
        TypeHandler{rust_type_name: "&Path", jni_type_name: "jstring", java_type_name: "String",
                    from_jni_converter: Some(jstring_to_path), to_jni_converter: None},
    ];
    let mut rust_java_types_map = HashMap::new();
    for it in type_handlers.iter() {
        rust_java_types_map.insert(it.rust_type_name, &*it);
    }

    let java_output_dir = env::var("RUST_SWIG_JNI_JAVA_OUTPUT_DIR").unwrap();
    let package_name = env::var("RUST_SWIG_JNI_JAVA_PACKAGE").unwrap();

    generate_java_code(&rust_java_types_map, package_name.as_str(), &class_name_indent.name.as_str(), &methods,
                       &java_output_dir);

    generate_rust_code(cx, &rust_self_type, &rust_java_types_map, package_name.as_str(), &class_name_indent.name.as_str(), &methods)
}
