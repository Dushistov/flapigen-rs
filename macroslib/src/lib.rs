extern crate syntex;
extern crate syntex_syntax;
extern crate syntex_pos;
extern crate aster;

mod parse_utils;

use syntex::Registry;
use syntex_syntax::ext::base::{ExtCtxt, MacResult, DummyResult, MacEager};
use syntex_syntax::parse::{token, parser};
use syntex_syntax::tokenstream::TokenTree;
use syntex_syntax::codemap::Span;
use syntex_syntax::{parse, ast};
use syntex_syntax::ptr::P;
use syntex_syntax::ast::{Arg};
use syntex_syntax::util::small_vector::SmallVector;
use syntex_syntax::print::pprust;
use std::collections::HashMap;
use std::env;
use std::path::PathBuf;
use std::fs::File;
use std::error::Error;
use std::io::prelude::*;
use std::fmt;
use aster::AstBuilder;
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
    in_out_type: P<ast::FnDecl>
}

struct TypeHandler {
    jni_type_name: &'static str,
    java_type_name: &'static str
}

type RustToJavaTypes = HashMap<&'static str, TypeHandler>;

impl ForeignerMethod {
    fn args(&self) -> String {
        use std::fmt::Write;

        let skip_n =  if self.func_type == FuncVariant::Method { 1 } else { 0 };
        let mut res = String::new();
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

    fn args_with_java_types(&self, types_map: &RustToJavaTypes) -> String {
        use std::fmt::Write;

        let skip_n =  if self.func_type == FuncVariant::Method { 1 } else { 0 };
        let mut res = String::new();
        for (i, item_arg) in self.in_out_type.inputs.iter().skip(skip_n).enumerate() {
            let type_name = types_map.get(pprust::ty_to_string(&*item_arg.ty).as_str()).unwrap().java_type_name;
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
                types_map.get(pprust::ty_to_string(&*ret_type).as_str()).unwrap().java_type_name
        }
    }

    fn short_name(&self) -> token::InternedString {
        match self.path.segments.len() {
            0 => token::InternedString::new(""),
            n => self.path.segments[n - 1].identifier.name.as_str()
        }
    }

    fn full_name(&self) -> String {
        format!("{}", self.path)
    }

    fn method_rust_self_type(&self) -> String {
        match self.path.segments.len() {
            0 => String::new(),
            n => {
                let mut path = self.path.clone();
                path.segments.truncate(n - 1);
                format!("{}", path)
//                pprust::to_string(|s| s.print_path(self.path, false, 1))
            }
        }
    }
}

pub fn register(registry: &mut Registry) {
    registry.add_macro("foreigner_class", expand_foreigner_class);
}

fn add_args<'a, I, BuilderType>(builder: aster::expr::ExprCallArgsBuilder<BuilderType>, mut iter: I, niter: usize) -> aster::expr::ExprCallArgsBuilder<BuilderType>
    where I: Iterator<Item=&'a Arg>, BuilderType: aster::invoke::Invoke<syntex_syntax::ptr::P<syntex_syntax::ast::Expr>> {
    match iter.next() {
        Some(_) => { add_args(builder.arg().id(format!("a_{}", niter)), iter, niter + 1) }
        None => builder
    }
}

fn generate_java_code(rust_java_types_map: &RustToJavaTypes, package_name: &str, class_name: &token::InternedString, methods: &[ForeignerMethod], output_dir: &str) {
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
    public {}({}) {{
        mNativeObj = init({});
    }}
    private static native long init({});
", class_name, method_it.args_with_java_types(rust_java_types_map),
                       method_it.args(), method_it.args_with_java_types(rust_java_types_map)).unwrap();
            }
            FuncVariant::Method => {
                have_methods = true;
                write!(file,
"
    public {} {}({}) {{ return do_{}(mNativeObj, {}); }}
    private static native {} do_{}(long me, {});
",
                       method_it.java_return_type(rust_java_types_map), method_it.short_name(), method_it.args_with_java_types(rust_java_types_map),
                       method_it.short_name(), method_it.args(),
                       method_it.java_return_type(rust_java_types_map), method_it.short_name(), method_it.args_with_java_types(rust_java_types_map)
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

fn add_args_and_types<'a, I>(rust_java_types_map: &RustToJavaTypes, builder: aster::fn_decl::FnDeclBuilder<aster::item::ItemFnDeclBuilder<aster::invoke::Identity>>, mut iter: I, niter: usize) -> aster::fn_decl::FnDeclBuilder<aster::item::ItemFnDeclBuilder<aster::invoke::Identity>>
    where I: Iterator<Item=&'a Arg> {
    match iter.next() {
        Some(arg) => {
            let type_name = pprust::ty_to_string(&*arg.ty);
            let type_name = rust_java_types_map.get(type_name.as_str()).unwrap().jni_type_name;
            add_args_and_types(rust_java_types_map, builder.arg_id(format!("a_{}", niter)).ty().id(type_name), iter, niter + 1)
        }
        None => builder
    }
}

fn jni_func_name(package_name: &str, class_name: &str, func_name: &str, add_do: bool) -> String {
    let mut output = String::new();
    output.push_str("Java_");
    let mut underscore_count = 1_usize;
    fn escape_underscore(input: &str, underscore_count: &mut usize, mut output: &mut String) {
        for c in input.chars() {
            if c == '_' {
                fmt::write(&mut output, format_args!("_{}", *underscore_count)).unwrap();
                *underscore_count = *underscore_count + 1;
            } else {
                fmt::write(&mut output, format_args!("{}", c)).unwrap();
            }
        }
    }
    output.push_str(package_name);
    output.push_str("_");
    escape_underscore(class_name, &mut underscore_count, &mut output);
    output.push_str("_");
    if add_do {
        escape_underscore("do_", &mut underscore_count, &mut output);
    }
    escape_underscore(func_name, &mut underscore_count, &mut output);
    output
}


fn convert_args<'a, I, BuilderType>(rust_java_types_map: &RustToJavaTypes, iter: I, builder: aster::block::BlockBuilder<BuilderType>) -> aster::block::BlockBuilder<BuilderType>
    where I: Iterator<Item=&'a Arg>, BuilderType: aster::invoke::Invoke<syntex_syntax::ptr::P<syntex_syntax::ast::Block>> {
    let mut block = builder;

    for (i, arg) in iter.enumerate() {
        let type_name = pprust::ty_to_string(&*arg.ty);
        let th: &TypeHandler = rust_java_types_map.get(type_name.as_str()).unwrap();
        if th.jni_type_name == "jstring" {
            let arg_name = format!("a_{}", i);
            block = block.stmt().let_id(&arg_name).expr().call().id("JavaString::new").arg().id("env").arg().id(&arg_name).build()
                .stmt().let_id(&arg_name).expr().method_call("to_str").id(&arg_name).build();
        }
        println!("!!!arg {}, tn {}", i, type_name);
    }
    block
}

fn generate_rust_code<'cx>(cx: &'cx mut ExtCtxt, rust_self_type: &ast::Path, rust_java_types_map: &RustToJavaTypes, package_name: &str, class_name: &token::InternedString, methods: &[ForeignerMethod]) -> Box<MacResult> {
    let package_name = package_name.replace(".", "_");
    let builder = AstBuilder::new();
    let mut jni_methods = Vec::new();
    let mut have_constructor = false;
    let mut have_methods = false;
    for it in methods.iter() {
        match it.func_type {
            FuncVariant::StaticMethod => (),
            FuncVariant::Constructor => {
                have_constructor = true;
                let body_block = convert_args(rust_java_types_map, it.in_out_type.inputs.iter(), builder.block());
                let body_block = body_block
                    .stmt().let_id("obj").expr().call().id("Box::into_raw").arg().call().id("Box::new").arg().call().id(it.full_name());
                let body_block = add_args(body_block, it.in_out_type.inputs.iter(), 0);
                let body_block = body_block
                    .build().build().build().stmt().expr().block().unsafe_().expr()
                    .call().id("ptr_to_jlong").arg().id("obj")
                    .build();

                let fn_ = builder.item().fn_(jni_func_name(&package_name, &class_name, "init", false))
                    .arg_id("env").ty().id("*mut JNIEnv")
                    .arg_id("_").ty().id("jclass");
                let fn_ = add_args_and_types(&rust_java_types_map, fn_, it.in_out_type.inputs.iter(), 0);
                let fn_ = fn_.return_().id("jlong").build(body_block.build())
                    .map(|mut p| {
                        p.attrs.push(builder.attr().word("no_mangle"));
                        p.vis = ast::Visibility::Public;
                        p
                    });
                jni_methods.push(fn_);
            }
            FuncVariant::Method => {
                have_methods = true;
                let body_block = builder.block()
                    .stmt().let_id("this").expr().block().unsafe_().expr().call().id(format!("jlong_to_pointer::<{}>", it.method_rust_self_type())).arg().id("this").build()
                    .stmt().let_id("this").expr().block().unsafe_().expr().method_call("as_mut").id("this").build()
                    .stmt().let_id("this").expr().block().expr().method_call("unwrap").id("this").build()
                    .expr().call().id(format!("{}", it.path)).arg().id("this");
                let body_block = add_args(body_block, it.in_out_type.inputs.iter().skip(1), 0);
                let func_name = it.short_name();
                let fn_ = builder.item().fn_(jni_func_name(&package_name, &class_name, &func_name, true))
                    .arg_id("_").ty().id("*mut JNIEnv")
                    .arg_id("_").ty().id("jclass")
                    .arg_id("this").ty().id("jlong");

                let fn_ = add_args_and_types(&rust_java_types_map, fn_, it.in_out_type.inputs.iter().skip(1), 0);
                let fn_ = match &it.in_out_type.output {
                    &ast::FunctionRetTy::Default(_) => fn_.default_return(),
                    &ast::FunctionRetTy::Ty(ref ret_type) => fn_.return_().id(
                        rust_java_types_map.get(pprust::ty_to_string(&*ret_type).as_str()).unwrap().jni_type_name
                    )
                };
                let fn_ = fn_.build(body_block.build());
                let fn_ = fn_.map(|mut p| {
                    p.attrs.push(builder.attr().word("no_mangle"));
                    p.vis = ast::Visibility::Public;
                    p
                });
                jni_methods.push(fn_);
            }
        }
    }
    if have_methods && !have_constructor {
        panic!("package_name {}, class_name {}, have methods, but no constructor",
               package_name, class_name);
    }
    if have_constructor && rust_self_type.segments.is_empty() {
        panic!("package_name {}, class_name {} have constructor, but self_type not defined",
               package_name, class_name);
    }
    if have_constructor {
        let body_block = builder.block()
            .stmt().let_id("this").expr().block().unsafe_().expr().call().id(format!("jlong_to_pointer::<{}>", rust_self_type)).arg().id("this").build()
            .stmt().let_id("this").expr().block().unsafe_().expr().method_call("as_mut").id("this").build()
            .stmt().let_id("this").expr().method_call("unwrap").id("this").build()
            .stmt().let_id("this").expr().block().unsafe_().expr().call().id("Box::from_raw").arg().id("this").build()
            .expr().call().id("drop").arg().id("this");
        let fn_ = builder.item().fn_(jni_func_name(&package_name, &class_name, "delete", true))
            .arg_id("_").ty().id("*mut JNIEnv")
            .arg_id("_").ty().id("jclass")
            .arg_id("this").ty().id("jlong").default_return();
        let fn_ = fn_.build(body_block.build());
        let fn_ = fn_.map(|mut p| {
            p.attrs.push(builder.attr().word("no_mangle"));
            p.vis = ast::Visibility::Public;
            p
        });
        jni_methods.push(fn_);
    }

        jni_methods.push(parse::parse_item_from_source_str("addon".to_string(),
                                                       r#"
#[cfg(target_pointer_width = "32")]
unsafe fn jlong_to_pointer<T>(val: jlong) -> *mut T {
    mem::transmute::<u32, *mut T>(val as u32)
}
"#.to_string(),
                                                           cx.cfg.clone(), cx.parse_sess).unwrap().unwrap());

    jni_methods.push(parse::parse_item_from_source_str("addon".to_string(),
                                                       r#"
#[cfg(target_pointer_width = "64")]
unsafe fn jlong_to_pointer<T>(val: jlong) -> *mut T {
    mem::transmute::<jlong, *mut T>(val)
}
"#.to_string(),
                                                       cx.cfg.clone(), cx.parse_sess).unwrap().unwrap());

    jni_methods.push(parse::parse_item_from_source_str("addon".to_string(),
                                                       r#"
fn ptr_to_jlong<T>(val: *const T) -> jlong {
    val as jlong
}
"#.to_string(),
                                                       cx.cfg.clone(), cx.parse_sess).unwrap().unwrap());

       jni_methods.push(parse::parse_item_from_source_str("addon".to_string(),
                                                       r#"
struct JavaString {
    string: jstring,
    chars: *const ::std::os::raw::c_char,
    env: *mut JNIEnv
}
"#.to_string(),
                                                       cx.cfg.clone(), cx.parse_sess).unwrap().unwrap());

    jni_methods.push(parse::parse_item_from_source_str("addon".to_string(),
                                                       r#"
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
"#.to_string(),
                                                       cx.cfg.clone(), cx.parse_sess).unwrap().unwrap());

    jni_methods.push(parse::parse_item_from_source_str("addon".to_string(),
                                                       r#"
impl Drop for JavaString {
    fn drop(&mut self) {
        assert!(self.env != ptr::null_mut() && self.chars != ptr::null_mut());
        unsafe { (**self.env).ReleaseStringUTFChars.unwrap()(self.env, self.string, self.chars) };
        self.env = ptr::null_mut();
        self.chars = ptr::null_mut();
    }
}
"#.to_string(),
                                                       cx.cfg.clone(), cx.parse_sess).unwrap().unwrap());
    MacEager::items(SmallVector::many(jni_methods))
}

fn expand_foreigner_class<'cx>(cx: &'cx mut ExtCtxt,
                               _: Span,
                               tokens: &[TokenTree])
                               -> Box<MacResult + 'cx> {
    let mut parser = parse::new_parser_from_tts(cx.parse_sess, cx.cfg.clone(), tokens.to_vec());
    let class_ident = parser.parse_ident().unwrap();
    if class_ident.name.as_str() != "class" {
        println!("class_indent {:?}", class_ident);
        cx.span_err(parser.span, "expect class here");
        return DummyResult::any(parser.span);
    }
    let class_name_indent = parser.parse_ident().unwrap();
    println!("CLASS NAME {:?}", class_name_indent);
    parser.expect(&token::Token::OpenDelim(token::DelimToken::Brace)).unwrap();
    let mut methods = Vec::new();
    let mut rust_self_type = ast::Path{span: parser.span, global: false, segments: Vec::new()};
    loop {
        if parser.eat(&token::Token::CloseDelim(token::DelimToken::Brace)) {
            break;
        }
        let func_type_name = parser.parse_ident().unwrap();
        println!("func_type {:?}", func_type_name);
        if func_type_name.name.as_str() == "self_type" {
            rust_self_type = parser.parse_path(parser::PathStyle::Type).unwrap();
            println!("self_type: {:?}", rust_self_type);
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
        println!("func_name {:?}", func_name);

        let func_decl = match func_type {
            FuncVariant::Constructor | FuncVariant::StaticMethod => parser.parse_fn_decl(false).unwrap(),
            FuncVariant::Method => parse_fn_decl_with_self(&mut parser, |p| p.parse_arg()).unwrap(),
        };
        println!("func_decl {:?}", func_decl);
        parser.expect(&token::Token::Semi).unwrap();

        methods.push(ForeignerMethod{func_type: func_type, path: func_name, in_out_type: func_decl});
    }

    let mut rust_java_types_map = HashMap::new();
    rust_java_types_map.insert("i32", TypeHandler{jni_type_name: "jint", java_type_name: "int"});
    rust_java_types_map.insert("&str", TypeHandler{jni_type_name: "jstring", java_type_name: "String"});

    let java_output_dir = env::var("RUST_SWIG_JNI_JAVA_OUTPUT_DIR").unwrap();
    let package_name = env::var("RUST_SWIG_JNI_JAVA_PACKAGE").unwrap();

    generate_java_code(&rust_java_types_map, package_name.as_str(), &class_name_indent.name.as_str(), &methods,
                       &java_output_dir);

    generate_rust_code(cx, &rust_self_type, &rust_java_types_map, package_name.as_str(), &class_name_indent.name.as_str(), &methods)
}
