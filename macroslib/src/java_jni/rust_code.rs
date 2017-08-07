use std::collections::HashMap;
use std::fmt::Write;

use syntex_syntax::{ast, parse};
use syntex_syntax::ptr::P;
use syntex_syntax::symbol::Symbol;
use syntex_syntax::print::pprust;
use syntex_syntax::parse::ParseSess;

use ForeignerClassInfo;
use ForeignTypesMap;
use MethodVariant;
use types_map::{fn_decl_output_typename, unpack_unique_typename, MethodSignatureWithForeignTypes};
use java_jni::java_code;
use utils::fmt_write_err_map;

pub(crate) fn generate_rust_code(
    parse_sess: &ParseSess,
    types_map: &mut ForeignTypesMap,
    package_name: &str,
    class: &ForeignerClassInfo,
    methods_sign: &[MethodSignatureWithForeignTypes],
) -> Result<Vec<P<ast::Item>>, String> {
    let mut gen_fnames = HashMap::<String, usize>::new(); //handle overload names
    for method in &class.methods {
        let val_ref = gen_fnames.entry(java_code::method_name(&*method));
        *val_ref.or_insert(0) += 1;
    }

    let get_constructor_ret_type = |cls: &ForeignerClassInfo| -> Result<ast::Ty, String> {
        let ret: ast::Ty = cls.constructor_ret_type
            .as_ref()
            .ok_or_else(|| {
                format!(
                    "package_name {}, class_name {}, have methods, \
                     but no constructor",
                    package_name,
                    class.name
                )
            })?
            .clone();
        Ok(ret)
    };

    let get_real_constructor_ret_type = |cls: &ForeignerClassInfo| -> Result<ast::Ty, String> {
        let ret: ast::Ty = cls.this_type_for_method
            .as_ref()
            .ok_or_else(|| {
                format!(
                    "package_name {}, class_name {}, have methods, \
                     but no constructor this type for method",
                    package_name,
                    class.name
                )
            })?
            .clone();
        Ok(ret)
    };

    let mut gen_code = Vec::<P<ast::Item>>::new();

    let mut have_constructor = false;

    for (method, method_sign) in class.methods.iter().zip(methods_sign.iter()) {
        let java_method_name = java_code::method_name(&*method);
        let method_overloading = gen_fnames[&java_method_name] > 1;
        let jni_func_name = generate_jni_func_name(
            package_name,
            &class.name,
            &java_method_name,
            &method_sign,
            method_overloading,
        );
        let n_args = method_sign.rust_input.len();
        let (mut deps_code_in, convert_input_code) = types_map.convert_foreign_input(
            parse_sess,
            &*method,
            &method_sign,
            (0..n_args).map(|v| format!("a_{}", v)),
        )?;
        let args_names = method_sign
            .rust_input
            .iter()
            .enumerate()
            .map(|a| format!("a_{}, ", a.0))
            .fold(String::new(), |acc, x| acc + &x);
        let decl_func_args = generate_jni_args_with_types(&method_sign)?;
        let real_output_typename = fn_decl_output_typename(&*method.fn_decl);

        let mut code = String::new();
        match method.variant {
            MethodVariant::StaticMethod => {
                let (mut deps_code_out, convert_output_code) = types_map
                    .convert_output_to_foreign(parse_sess, &*method, &method_sign, "ret")?;
                write!(
                    &mut code,
                    r#"
#[allow(non_snake_case, unused_variables, unused_mut)]
#[no_mangle]
pub fn {func_name}(env: *mut JNIEnv, _: jclass, {decl_func_args}) -> {jni_ret_type} {{
{convert_input_code}
    let mut ret: {real_output_typename} = {rust_func_name}({args_names});
{convert_output_code}
    ret
}}
"#,
                    func_name = jni_func_name,
                    decl_func_args = decl_func_args,
                    jni_ret_type = unpack_unique_typename(method_sign.rust_output),
                    convert_input_code = convert_input_code,
                    rust_func_name = method.rust_id,
                    args_names = args_names,
                    convert_output_code = convert_output_code,
                    real_output_typename = real_output_typename,
                ).map_err(fmt_write_err_map)?;



                for item in deps_code_in.drain(..).chain(deps_code_out.drain(..)) {
                    gen_code.push(P(item));
                }
                let mut jni_func_items = code_to_item(&code);
                gen_code.extend(jni_func_items.drain(..));
            }
            MethodVariant::Method => {
                let constructor_ret_type = get_real_constructor_ret_type(class)?;

                let (mut deps_code_out, convert_output_code) = types_map
                    .convert_output_to_foreign(parse_sess, &*method, &method_sign, "ret")?;
                write!(
                    &mut code,
                    r#"
#[allow(non_snake_case, unused_variables, unused_mut)]
#[no_mangle]
pub fn {func_name}(env: *mut JNIEnv, _: jclass, this: jlong, {decl_func_args}) -> {jni_ret_type} {{
{convert_input_code}
    let this: &mut {this_type} = unsafe {{
        jlong_to_pointer::<{this_type}>(this).as_mut().unwrap()
    }};
{convert_this}
    let mut ret: {real_output_typename} = {rust_func_name}(&mut *this, {args_names});
{convert_output_code}
    ret
}}
"#,
                    func_name = jni_func_name,
                    decl_func_args = decl_func_args,
                    convert_input_code = convert_input_code,
                    jni_ret_type = unpack_unique_typename(method_sign.rust_output),
                    this_type = pprust::ty_to_string(&constructor_ret_type),
                    convert_this = generate_rust_to_convert_this(class, &constructor_ret_type)?,
                    rust_func_name = method.rust_id,
                    args_names = args_names,
                    convert_output_code = convert_output_code,
                    real_output_typename = real_output_typename,
                ).map_err(fmt_write_err_map)?;
                for item in deps_code_in.drain(..).chain(deps_code_out.drain(..)) {
                    gen_code.push(P(item));
                }
                let mut jni_func_items = code_to_item(&code);
                gen_code.extend(jni_func_items.drain(..));
            }
            MethodVariant::Constructor => {
                have_constructor = true;
                let constructor_ret_type = get_constructor_ret_type(class)?;
                write!(
                    &mut code,
                    r#"
#[allow(non_snake_case)]
#[no_mangle]
#[allow(unused_variables)]
pub fn {func_name}(env: *mut JNIEnv, _: jclass, {decl_func_args}) -> jlong {{
{convert_input_code}
  let this: {real_output_typename} = {rust_func_name}({args_names});
{convert_this}
  Box::into_raw(Box::new(this)) as jlong
}}
"#,
                    func_name = jni_func_name,
                    convert_this = jni_handle_result_from_constructor(class, &constructor_ret_type),
                    decl_func_args = decl_func_args,
                    convert_input_code = convert_input_code,
                    rust_func_name = method.rust_id,
                    args_names = args_names,
                    real_output_typename = real_output_typename,
                ).map_err(fmt_write_err_map)?;
                for item in deps_code_in.drain(..) {
                    gen_code.push(P(item));
                }
                let mut jni_func_items = code_to_item(&code);
                gen_code.extend(jni_func_items.drain(..));
            }
        }
    }

    if have_constructor {
        let constructor_ret_type = get_real_constructor_ret_type(class)?;
        let mut code = String::new();
        write!(
            &mut code,
            r#"
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
            jni_destructor_name = generate_jni_func_name(
                package_name,
                &class.name,
                "do_delete",
                &MethodSignatureWithForeignTypes {
                    foreign_input: Vec::new(),
                    rust_input: Vec::new(),
                    foreign_output: Symbol::intern(""),
                    rust_output: Symbol::intern(""),
                },
                false
            ),
            type_name = pprust::ty_to_string(&constructor_ret_type),
        ).map_err(fmt_write_err_map)?;
        debug!("we generate and parse code: {}", code);
        let mut jni_func_items = code_to_item(&code);
        gen_code.extend(jni_func_items.drain(..));
    }


    Ok(gen_code)
}

lazy_static! {
    static ref JAVA_TYPE_NAMES_FOR_JNI_SIGNATURE: HashMap<Symbol, &'static str> = {
        let mut m = HashMap::new();
        m.insert(Symbol::intern("String"), "Ljava.lang.String;");
        m.insert(Symbol::intern("boolean"), "Z");
        m.insert(Symbol::intern("byte"), "B");
        m.insert(Symbol::intern("char"), "C");
        m.insert(Symbol::intern("double"), "D");
        m.insert(Symbol::intern("float"), "F");
        m.insert(Symbol::intern("int"), "I");
        m.insert(Symbol::intern("long"), "J");
        m.insert(Symbol::intern("object"), "L");
        m.insert(Symbol::intern("short"), "S");
        m.insert(Symbol::intern("void"), "V");
        m
    };
}

fn generate_jni_func_name(
    package_name: &str,
    class_name: &str,
    java_method_name: &str,
    method: &MethodSignatureWithForeignTypes,
    overloaded: bool,
) -> String {
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
    escape_underscore(java_method_name, &mut output);

    if overloaded {
        output.push_str("__");
        for arg in &method.foreign_input {
            escape_underscore(
                JAVA_TYPE_NAMES_FOR_JNI_SIGNATURE
                    .get(&*arg)
                    .unwrap_or_else(|| {
                        panic!("generate_jni_func_name: Unknown Java type `{}`", *arg)
                    }),
                &mut output,
            );
        }
    }

    output
}

fn generate_jni_args_with_types(
    method: &MethodSignatureWithForeignTypes,
) -> Result<String, String> {
    use std::fmt::Write;

    let mut buf = String::new();
    for (i, rust_type_name) in method.rust_input.iter().enumerate() {
        write!(
            &mut buf,
            "a_{}: {}, ",
            i,
            unpack_unique_typename(*rust_type_name)
        ).map_err(fmt_write_err_map)?;
    }
    Ok(buf)
}

/// may panics
fn code_to_item(code: &str) -> Vec<P<ast::Item>> {
    let session = parse::ParseSess::new();
    let mut parser = parse::new_parser_from_source_str(&session, "conv code".into(), code.into());

    let mut krate = match parser.parse_crate_mod() {
        Ok(x) => x,
        Err(mut diag) => {
            diag.emit();
            panic!("{}: {}: Can not parse '{}'", file!(), line!(), code);
        }
    };
    let items: Vec<_> = krate.module.items.drain(..).collect();
    items
}

/// may panic
fn jni_handle_result_from_constructor(
    class: &ForeignerClassInfo,
    constructor_ret_type: &ast::Ty,
) -> String {
    let ret_typename = pprust::ty_to_string(constructor_ret_type);
    let self_typename = format!("{}", class.self_type);
    if ret_typename.starts_with(&format!("Result<{},", self_typename)) {
        r#"
  let this = match this {
    Ok(val) => val,
    Err(msg) => {
      jni_throw_exception(env, &msg);
      return 0;
    }
  };
"#.into()
    } else {
        String::new()
    }
}

fn generate_rust_to_convert_this(
    class: &ForeignerClassInfo,
    constructor_ret_type: &ast::Ty,
) -> Result<String, String> {
    let constructor_ret_type = pprust::ty_to_string(constructor_ret_type);
    let self_type = format!("{}", class.self_type);
    if constructor_ret_type == self_type {
        return Ok(String::new());
    }

    if format!("Arc<Mutex<{}>>", self_type) == constructor_ret_type {
        Ok("    let mut this = this.lock().unwrap();\n".into())
    } else if format!("Rc<RefCell<{}>>", self_type) == constructor_ret_type {
        Ok("    let mut this = this.borrow_mut();\n".into())
    } else if format!("Arc<{}>", self_type) == constructor_ret_type {
        Ok(String::new())
    } else {
        Err(format!(
            "Not implemented conversation from '{}' to '{}'",
            constructor_ret_type,
            self_type
        ))
    }
}
