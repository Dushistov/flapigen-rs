use lazy_static::lazy_static;
use log::{debug, trace};
use proc_macro2::TokenStream;
use rustc_hash::FxHashMap;
use syn::{parse_quote, spanned::Spanned, Type};

use crate::{
    error::{panic_on_syn_error, DiagnosticError, Result},
    java_jni::{
        calc_this_type_for_method, fmt_write_err_map, java_class_full_name, java_class_name_to_jni,
        method_name, ForeignTypeInfo, JniForeignMethodSignature,
    },
    source_registry::SourceId,
    typemap::ast::{fn_arg_type, list_lifetimes, normalize_ty_lifetimes, DisplayToTokens},
    typemap::{
        ty::RustType,
        utils::{
            convert_to_heap_pointer, create_suitable_types_for_constructor_and_self,
            foreign_from_rust_convert_method_output, foreign_to_rust_convert_method_inputs,
            rust_to_foreign_convert_method_inputs, unpack_from_heap_pointer,
        },
        TO_VAR_TEMPLATE,
    },
    types::{
        ForeignEnumInfo, ForeignInterface, ForeignerClassInfo, ForeignerMethod, MethodVariant,
        SelfTypeVariant,
    },
    TypeMap,
};

struct MethodContext<'a> {
    class: &'a ForeignerClassInfo,
    method: &'a ForeignerMethod,
    f_method: &'a JniForeignMethodSignature,
    jni_func_name: &'a str,
    decl_func_args: &'a str,
    args_names: &'a str,
    real_output_typename: &'a str,
}

pub(in crate::java_jni) fn generate_rust_code(
    conv_map: &mut TypeMap,
    package_name: &str,
    class: &ForeignerClassInfo,
    f_methods_sign: &[JniForeignMethodSignature],
) -> Result<Vec<TokenStream>> {
    //to handle java method overload
    let mut gen_fnames = FxHashMap::<String, usize>::default();
    for (method, f_method) in class.methods.iter().zip(f_methods_sign.iter()) {
        let val_ref = gen_fnames.entry(method_name(method, f_method));
        *val_ref.or_insert(0) += 1;
    }

    let dummy_ty = parse_type! { () };
    let dummy_rust_ty = conv_map.find_or_alloc_rust_type_no_src_id(&dummy_ty);

    let mut gen_code = Vec::<TokenStream>::new();
    let (this_type_for_method, code_box_this) =
        if let Some(this_type) = calc_this_type_for_method(conv_map, class) {
            let this_type = conv_map.ty_to_rust_type(&this_type);
            debug!(
                "generate_rust_code: add implements SwigForeignClass for {}",
                this_type.normalized_name
            );

            let (this_type_for_method, code_box_this) =
                convert_to_heap_pointer(conv_map, &this_type, "this");
            let class_name_for_user = java_class_full_name(package_name, &class.name.to_string());
            let class_name_for_jni = java_class_name_to_jni(&class_name_for_user);
            let lifetimes = {
                let mut ret = String::new();
                let lifetimes = list_lifetimes(&this_type.ty);
                for (i, l) in lifetimes.iter().enumerate() {
                    ret.push_str(&*l.as_str());
                    if i != lifetimes.len() - 1 {
                        ret.push(',');
                    }
                }
                ret
            };

            let unpack_code = unpack_from_heap_pointer(&this_type, TO_VAR_TEMPLATE, true);

            let fclass_impl_code = format!(
                r#"impl<{lifetimes}> SwigForeignClass for {class_name} {{
    fn jni_class_name() -> *const ::std::os::raw::c_char {{
        swig_c_str!("{jni_class_name}")
    }}
    fn box_object(this: Self) -> jlong {{
{code_box_this}
       this as jlong
    }}
    fn unbox_object(x: jlong) -> Self {{
        let x: *mut {this_type} = unsafe {{
           jlong_to_pointer::<{this_type}>(x).as_mut().unwrap()
        }};
    {unpack_code}
        x
    }}
}}"#,
                lifetimes = lifetimes,
                class_name = DisplayToTokens(&this_type.ty),
                jni_class_name = class_name_for_jni,
                code_box_this = code_box_this,
                unpack_code = unpack_code.replace(TO_VAR_TEMPLATE, "x"),
                this_type = this_type_for_method.normalized_name,
            );

            gen_code.push(syn::parse_str(&fclass_impl_code).unwrap_or_else(|err| {
                panic_on_syn_error("java internal fclass impl code", fclass_impl_code, err)
            }));

            (this_type_for_method, code_box_this)
        } else {
            (dummy_rust_ty.clone(), String::new())
        };

    let no_this_info = || {
        DiagnosticError::new(
            class.src_id,
            class.span(),
            format!(
                "Class {} (package {}) has methods, but there is no constructor\n
May be you need to use `private constructor = empty;` syntax?",
                class.name, package_name,
            ),
        )
    };

    let mut have_constructor = false;

    for (method, f_method) in class.methods.iter().zip(f_methods_sign.iter()) {
        let java_method_name = method_name(method, f_method);
        let method_overloading = gen_fnames[&java_method_name] > 1;
        let jni_func_name = generate_jni_func_name(
            package_name,
            class,
            &java_method_name,
            f_method,
            method_overloading,
        )?;
        trace!("generate_rust_code jni name: {}", jni_func_name);

        let args_names = f_method
            .input
            .iter()
            .enumerate()
            .map(|a| format!("a_{}, ", a.0))
            .fold(String::new(), |acc, x| acc + &x);

        let decl_func_args = generate_jni_args_with_types(f_method)
            .map_err(|err| DiagnosticError::new(class.src_id, class.span(), &err))?;
        let real_output_typename = match method.fn_decl.output {
            syn::ReturnType::Default => "()",
            syn::ReturnType::Type(_, ref ty) => normalize_ty_lifetimes(&*ty),
        };

        let method_ctx = MethodContext {
            class,
            method,
            f_method,
            jni_func_name: &jni_func_name,
            decl_func_args: &decl_func_args,
            args_names: &args_names,
            real_output_typename: &real_output_typename,
        };

        match method.variant {
            MethodVariant::StaticMethod => {
                gen_code.append(&mut generate_static_method(conv_map, &method_ctx)?);
            }
            MethodVariant::Method(ref self_variant) => {
                gen_code.append(&mut generate_method(
                    conv_map,
                    &method_ctx,
                    *self_variant,
                    &this_type_for_method,
                )?);
            }
            MethodVariant::Constructor => {
                have_constructor = true;
                if !method.is_dummy_constructor() {
                    let constructor_ret_type = class
                        .self_desc
                        .as_ref()
                        .map(|x| &x.constructor_ret_type)
                        .ok_or_else(&no_this_info)?
                        .clone();
                    let this_type =
                        calc_this_type_for_method(conv_map, class).ok_or_else(&no_this_info)?;
                    gen_code.append(&mut generate_constructor(
                        conv_map,
                        &method_ctx,
                        constructor_ret_type,
                        this_type,
                        &code_box_this,
                    )?);
                }
            }
        }
    }

    if have_constructor {
        let this_type: RustType = conv_map.find_or_alloc_rust_type(
            &calc_this_type_for_method(conv_map, class).ok_or_else(&no_this_info)?,
            class.src_id,
        );

        let unpack_code = unpack_from_heap_pointer(&this_type, "this", false);

        let jni_destructor_name = generate_jni_func_name(
            package_name,
            class,
            "do_delete",
            &JniForeignMethodSignature {
                output: ForeignTypeInfo {
                    name: "".into(),
                    correspoding_rust_type: dummy_rust_ty.clone(),
                },
                input: vec![],
            },
            false,
        )?;
        let code = format!(
            r#"
#[allow(unused_variables, unused_mut, non_snake_case)]
#[no_mangle]
pub extern "C" fn {jni_destructor_name}(env: *mut JNIEnv, _: jclass, this: jlong) {{
    let this: *mut {this_type} = unsafe {{
        jlong_to_pointer::<{this_type}>(this).as_mut().unwrap()
    }};
{unpack_code}
    drop(this);
}}
"#,
            jni_destructor_name = jni_destructor_name,
            unpack_code = unpack_code,
            this_type = this_type_for_method.normalized_name,
        );
        debug!("we generate and parse code: {}", code);
        gen_code.push(
            syn::parse_str(&code).unwrap_or_else(|err| {
                panic_on_syn_error("java/jni internal desctructor", code, err)
            }),
        );
    }

    Ok(gen_code)
}

pub(in crate::java_jni) fn generate_rust_code_for_enum(
    package_name: &str,
    conv_map: &mut TypeMap,
    pointer_target_width: usize,
    enum_info: &ForeignEnumInfo,
) -> Result<Vec<TokenStream>> {
    use std::fmt::Write;

    let rust_enum_name = enum_info.rust_enum_name();
    let mut code = format!(
        r#"
impl SwigFrom<jint> for {rust_enum_name} {{
    fn swig_from(x: jint, _: *mut JNIEnv) -> {rust_enum_name} {{
        match x {{

"#,
        rust_enum_name = rust_enum_name,
    );
    for (i, item) in enum_info.items.iter().enumerate() {
        writeln!(
            &mut code,
            "{index} => {item_name},",
            index = i,
            item_name = DisplayToTokens(&item.rust_name),
        )
        .unwrap();
    }
    write!(
        &mut code,
        r#"
        _ => panic!("{{}} not expected for {rust_enum_name}", x),
        }}
    }}
}}
"#,
        rust_enum_name = rust_enum_name,
    )
    .unwrap();

    let java_enum_full_name = java_class_full_name(package_name, &enum_info.name.to_string());
    let enum_class_name = java_class_name_to_jni(&java_enum_full_name);

    write!(
        &mut code,
        r#"
mod swig_foreign_types_map {{
    #![swig_foreigner_type = "{enum_name}"]
    #![swig_rust_type_not_unique = "jobject"]
}}
#[swig_to_foreigner_hint = "{enum_name}"]
impl SwigFrom<{rust_enum_name}> for jobject {{
   fn swig_from(x: {rust_enum_name}, env: *mut JNIEnv) -> jobject {{
       let cls: jclass = unsafe {{ (**env).FindClass.unwrap()(env, swig_c_str!("{class_name}")) }};
       assert!(!cls.is_null(), "FindClass {class_name} failed");
       let static_field_id = match x {{
"#,
        enum_name = enum_info.name,
        rust_enum_name = rust_enum_name,
        class_name = enum_class_name,
    )
    .unwrap();

    for item in &enum_info.items {
        write!(
            &mut code,
            r#"
           {rust_item} => swig_c_str!("{java_item}"),
"#,
            rust_item = DisplayToTokens(&item.rust_name),
            java_item = item.name,
        )
        .unwrap();
    }
    write!(
        &mut code,
        r#"
      }};
      let item_id: jfieldID = unsafe {{
          (**env).GetStaticFieldID.unwrap()(env, cls , static_field_id,
                                             swig_c_str!("L{class_name};"))
      }};
      assert!(!item_id.is_null(), "Can not find item in {class_name}");
      let ret: jobject = unsafe {{
        (**env).GetStaticObjectField.unwrap()(env, cls, item_id)
      }};
      assert!(!ret.is_null(), "Can get value of item in {class_name}");
      ret
   }}
}}
"#,
        class_name = enum_class_name,
    )
    .unwrap();
    conv_map.register_exported_enum(enum_info);
    conv_map.merge(SourceId::none(), &code, pointer_target_width)?;
    Ok(vec![])
}

pub(in crate::java_jni) fn generate_interface(
    package_name: &str,
    conv_map: &mut TypeMap,
    pointer_target_width: usize,
    interface: &ForeignInterface,
    methods_sign: &[JniForeignMethodSignature],
) -> Result<Vec<TokenStream>> {
    use std::fmt::Write;

    let mut new_conv_code = format!(
        r#"
#[swig_from_foreigner_hint = "{interface_name}"]
impl SwigFrom<jobject> for Box<{trait_name}> {{
    fn swig_from(this: jobject, env: *mut JNIEnv) -> Self {{
        let mut cb = JavaCallback::new(this, env);
        cb.methods.reserve({methods_len});
        let class = unsafe {{ (**env).GetObjectClass.unwrap()(env, cb.this) }};
        assert!(!class.is_null(), "GetObjectClass return null class for {interface_name}");
"#,
        interface_name = interface.name,
        trait_name = DisplayToTokens(&interface.self_type),
        methods_len = interface.items.len(),
    );
    for (method, f_method) in interface.items.iter().zip(methods_sign) {
        write!(
            &mut new_conv_code,
            r#"
        let method_id: jmethodID = unsafe {{
            (**env).GetMethodID.unwrap()(env, class, swig_c_str!("{method_name}"),
                                         swig_c_str!("{method_sig}"))
        }};
        assert!(!method_id.is_null(), "Can not find {method_name} id");
        cb.methods.push(method_id);
"#,
            method_name = method.name,
            method_sig = jni_method_signature(f_method, package_name, conv_map),
        )
        .unwrap();
    }
    write!(
        &mut new_conv_code,
        r#"
        Box::new(cb)
    }}
}}
"#
    )
    .unwrap();
    conv_map.merge(SourceId::none(), &new_conv_code, pointer_target_width)?;

    let mut gen_items = Vec::<TokenStream>::new();

    let mut impl_trait_code = format!(
        r#"
impl {trait_name} for JavaCallback {{
"#,
        trait_name = DisplayToTokens(&interface.self_type)
    );

    for (method_idx, (method, f_method)) in interface.items.iter().zip(methods_sign).enumerate() {
        let func_name = &method
            .rust_name
            .segments
            .last()
            .ok_or_else(|| {
                DiagnosticError::new(
                    interface.src_id,
                    method.rust_name.span(),
                    "Empty trait function name",
                )
            })?
            .value()
            .ident;
        let rest_args_with_types: String = method
            .fn_decl
            .inputs
            .iter()
            .skip(1)
            .enumerate()
            .map(|(i, v)| format!("a_{}: {}", i, DisplayToTokens(fn_arg_type(v))))
            .fold(String::new(), |mut acc, x| {
                acc.push_str(", ");
                acc.push_str(&x);
                acc
            });
        let self_arg = format!("{}", DisplayToTokens(&method.fn_decl.inputs[0]));
        let args_with_types: String = [self_arg.to_string(), rest_args_with_types].concat();
        assert!(!method.fn_decl.inputs.is_empty());
        let n_args = method.fn_decl.inputs.len() - 1;
        let (args, type_size_asserts) = convert_args_for_variadic_function_call(f_method);
        let (mut conv_deps, convert_args) = rust_to_foreign_convert_method_inputs(
            conv_map,
            interface.src_id,
            method,
            f_method,
            (0..n_args).map(|v| format!("a_{}", v)),
            "()",
        )?;

        write!(
            &mut impl_trait_code,
            r#"
    #[allow(unused_mut)]
    fn {func_name}({args_with_types}) {{
{type_size_asserts}
        let env = self.get_jni_env();
        if let Some(env) = env.env {{
{convert_args}
            unsafe {{
                (**env).CallVoidMethod.unwrap()(env, self.this, self.methods[{method_idx}]
                                                {args});
                if (**env).ExceptionCheck.unwrap()(env) != 0 {{
                    error!("{func_name}: java throw exception");
                    (**env).ExceptionDescribe.unwrap()(env);
                    (**env).ExceptionClear.unwrap()(env);
                }}
            }};
        }}
    }}
"#,
            func_name = func_name,
            args_with_types = args_with_types,
            method_idx = method_idx,
            args = args,
            convert_args = convert_args,
            type_size_asserts = type_size_asserts,
        )
        .unwrap();
        gen_items.append(&mut conv_deps);
    }

    write!(
        &mut impl_trait_code,
        r#"
}}
"#
    )
    .unwrap();
    gen_items.push(syn::parse_str(&impl_trait_code).unwrap_or_else(|err| {
        panic_on_syn_error("java/jni internal impl_trait_code", impl_trait_code, err)
    }));
    Ok(gen_items)
}

lazy_static! {
    static ref JAVA_TYPE_NAMES_FOR_JNI_SIGNATURE: FxHashMap<&'static str, &'static str> = {
        let mut m = FxHashMap::default();
        m.insert("String", "Ljava.lang.String;");
        m.insert("Integer", "Ljava.lang.Integer");
        m.insert("Long", "Ljava.lang.Long");
        m.insert("Double", "Ljava.lang.Double");
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
    static ref JNI_FOR_VARIADIC_C_FUNC_CALL: FxHashMap<&'static str, &'static str> = {
        let mut m = FxHashMap::default();
        m.insert("jboolean", "::std::os::raw::c_uint");
        m.insert("jbyte", "::std::os::raw::c_int");
        m.insert("jshort", "::std::os::raw::c_int");
        m.insert("jfloat", "f64");
        m
    };
}

fn generate_jni_func_name(
    package_name: &str,
    class: &ForeignerClassInfo,
    java_method_name: &str,
    f_method: &JniForeignMethodSignature,
    overloaded: bool,
) -> Result<String> {
    let mut output = String::new();
    output.push_str("Java_");
    fn escape_underscore(input: &str, output: &mut String) {
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
    escape_underscore(&class.name.to_string(), &mut output);
    output.push_str("_");
    escape_underscore(java_method_name, &mut output);

    if overloaded {
        output.push_str("__");
        for arg in &f_method.input {
            let type_name = arg
                .java_converter
                .as_ref()
                .map(|x| x.java_transition_type.as_str())
                .unwrap_or_else(|| arg.as_ref().name.as_str());

            let type_name = JAVA_TYPE_NAMES_FOR_JNI_SIGNATURE
                .get(type_name)
                .ok_or_else(|| {
                    DiagnosticError::new(
                        class.src_id,
                        class.span(),
                        format!(
                            "Can not generate JNI function name for overload method '{}',\
                             unknown java type '{}'",
                            java_method_name,
                            arg.as_ref().name
                        ),
                    )
                })?;

            escape_underscore(type_name, &mut output);
        }
    }

    Ok(output)
}

fn generate_jni_args_with_types(
    f_method: &JniForeignMethodSignature,
) -> std::result::Result<String, String> {
    use std::fmt::Write;

    let mut buf = String::new();
    for (i, f_type_info) in f_method.input.iter().enumerate() {
        write!(
            &mut buf,
            "a_{}: {}, ",
            i,
            f_type_info.as_ref().correspoding_rust_type.typename()
        )
        .map_err(fmt_write_err_map)?;
    }
    Ok(buf)
}

fn generate_static_method(conv_map: &mut TypeMap, mc: &MethodContext) -> Result<Vec<TokenStream>> {
    let jni_ret_type = mc.f_method.output.correspoding_rust_type.typename();
    let (mut deps_code_out, convert_output_code) = foreign_from_rust_convert_method_output(
        conv_map,
        mc.class.src_id,
        &mc.method.fn_decl.output,
        &mc.f_method.output,
        "ret",
        &jni_ret_type,
    )?;
    let n_args = mc.f_method.input.len();
    let (deps_code_in, convert_input_code) = foreign_to_rust_convert_method_inputs(
        conv_map,
        mc.class.src_id,
        mc.method,
        mc.f_method,
        (0..n_args).map(|v| format!("a_{}", v)),
        &jni_ret_type,
    )?;

    let code = format!(
        r#"
#[allow(non_snake_case, unused_variables, unused_mut)]
#[no_mangle]
pub extern "C" fn {func_name}(env: *mut JNIEnv, _: jclass, {decl_func_args}) -> {jni_ret_type} {{
{convert_input_code}
    let mut ret: {real_output_typename} = {rust_func_name}({args_names});
{convert_output_code}
    ret
}}
"#,
        func_name = mc.jni_func_name,
        decl_func_args = mc.decl_func_args,
        jni_ret_type = jni_ret_type,
        convert_input_code = convert_input_code,
        rust_func_name = DisplayToTokens(&mc.method.rust_id),
        args_names = mc.args_names,
        convert_output_code = convert_output_code,
        real_output_typename = mc.real_output_typename,
    );
    let mut gen_code = deps_code_in;
    gen_code.append(&mut deps_code_out);
    gen_code
        .push(syn::parse_str(&code).unwrap_or_else(|err| {
            panic_on_syn_error("java/jni internal static method", code, err)
        }));
    Ok(gen_code)
}

fn generate_constructor(
    conv_map: &mut TypeMap,
    mc: &MethodContext,
    construct_ret_type: Type,
    this_type: Type,
    code_box_this: &str,
) -> Result<Vec<TokenStream>> {
    let n_args = mc.f_method.input.len();
    let (deps_code_in, convert_input_code) = foreign_to_rust_convert_method_inputs(
        conv_map,
        mc.class.src_id,
        mc.method,
        mc.f_method,
        (0..n_args).map(|v| format!("a_{}", v)),
        "jlong",
    )?;

    let this_type = conv_map.ty_to_rust_type(&this_type);
    let construct_ret_type = conv_map.ty_to_rust_type(&construct_ret_type);

    let (mut deps_this, convert_this) = conv_map.convert_rust_types(
        construct_ret_type.to_idx(),
        this_type.to_idx(),
        "this",
        "this",
        "jlong",
        (mc.class.src_id, mc.method.span()),
    )?;

    let code = format!(
        r#"
#[no_mangle]
#[allow(unused_variables, unused_mut, non_snake_case)]
pub extern "C" fn {func_name}(env: *mut JNIEnv, _: jclass, {decl_func_args}) -> jlong {{
{convert_input_code}
    let this: {real_output_typename} = {rust_func_name}({args_names});
{convert_this}
{box_this}
    this as jlong
}}
"#,
        func_name = mc.jni_func_name,
        convert_this = convert_this,
        decl_func_args = mc.decl_func_args,
        convert_input_code = convert_input_code,
        rust_func_name = DisplayToTokens(&mc.method.rust_id),
        args_names = mc.args_names,
        box_this = code_box_this,
        real_output_typename = mc.real_output_typename,
    );
    let mut gen_code = deps_code_in;
    gen_code.append(&mut deps_this);
    gen_code.push(
        syn::parse_str(&code)
            .unwrap_or_else(|err| panic_on_syn_error("java/jni internal constructor", code, err)),
    );
    Ok(gen_code)
}

fn generate_method(
    conv_map: &mut TypeMap,
    mc: &MethodContext,
    self_variant: SelfTypeVariant,
    this_type_for_method: &RustType,
) -> Result<Vec<TokenStream>> {
    let jni_ret_type = mc.f_method.output.correspoding_rust_type.typename();
    let n_args = mc.f_method.input.len();
    let (deps_code_in, convert_input_code) = foreign_to_rust_convert_method_inputs(
        conv_map,
        mc.class.src_id,
        mc.method,
        mc.f_method,
        (0..n_args).map(|v| format!("a_{}", v)),
        &jni_ret_type,
    )?;

    let (mut deps_code_out, convert_output_code) = foreign_from_rust_convert_method_output(
        conv_map,
        mc.class.src_id,
        &mc.method.fn_decl.output,
        &mc.f_method.output,
        "ret",
        &jni_ret_type,
    )?;

    //&mut constructor_real_type -> &mut class.self_type

    let (from_ty, to_ty): (Type, Type) = create_suitable_types_for_constructor_and_self(
        self_variant,
        mc.class,
        &this_type_for_method.ty,
    );
    let from_ty = conv_map.find_or_alloc_rust_type(&from_ty, mc.class.src_id);
    let this_type_ref = from_ty.normalized_name.as_str();
    let to_ty = conv_map.find_or_alloc_rust_type(&to_ty, mc.class.src_id);

    let (mut deps_this, convert_this) = conv_map.convert_rust_types(
        from_ty.to_idx(),
        to_ty.to_idx(),
        "this",
        "this",
        jni_ret_type,
        (mc.class.src_id, mc.method.span()),
    )?;

    let code = format!(
        r#"
#[allow(non_snake_case, unused_variables, unused_mut)]
#[no_mangle]
pub extern "C"
 fn {func_name}(env: *mut JNIEnv, _: jclass, this: jlong, {decl_func_args}) -> {jni_ret_type} {{
{convert_input_code}
    let this: {this_type_ref} = unsafe {{
        jlong_to_pointer::<{this_type}>(this).as_mut().unwrap()
    }};
{convert_this}
    let mut ret: {real_output_typename} = {rust_func_name}(this, {args_names});
{convert_output_code}
    ret
}}
"#,
        func_name = mc.jni_func_name,
        decl_func_args = mc.decl_func_args,
        convert_input_code = convert_input_code,
        jni_ret_type = jni_ret_type,
        this_type_ref = this_type_ref,
        this_type = this_type_for_method.normalized_name,
        convert_this = convert_this,
        rust_func_name = DisplayToTokens(&mc.method.rust_id),
        args_names = mc.args_names,
        convert_output_code = convert_output_code,
        real_output_typename = mc.real_output_typename,
    );
    let mut gen_code = deps_code_in;
    gen_code.append(&mut deps_code_out);
    gen_code.append(&mut deps_this);
    gen_code.push(
        syn::parse_str(&code)
            .unwrap_or_else(|err| panic_on_syn_error("java/jni internal method", code, err)),
    );
    Ok(gen_code)
}

fn jni_method_signature(
    method: &JniForeignMethodSignature,
    package_name: &str,
    conv_map: &TypeMap,
) -> String {
    let mut ret: String = "(".into();
    for arg in &method.input {
        let mut gen_sig = String::new();
        let sig = JAVA_TYPE_NAMES_FOR_JNI_SIGNATURE
            .get(&arg.as_ref().name.as_str())
            .cloned()
            .or_else(|| {
                if conv_map.is_generated_foreign_type(&arg.as_ref().name) {
                    gen_sig = format!(
                        "L{};",
                        &java_class_full_name(package_name, &*arg.as_ref().name.as_str())
                    );
                    Some(&gen_sig)
                } else {
                    None
                }
            })
            .unwrap_or_else(|| {
                panic!(
                    "Unknown type `{}`, can not generate jni signature",
                    arg.as_ref().name
                )
            });
        let sig = sig.replace('.', "/");
        ret.push_str(&sig);
    }
    ret.push(')');
    let sig = JAVA_TYPE_NAMES_FOR_JNI_SIGNATURE
        .get(&*method.output.name.as_str())
        .unwrap_or_else(|| {
            panic!(
                "Unknown type `{}`, can not generate jni signature",
                method.output.name
            )
        });
    ret.push_str(sig);
    ret
}

// To use `C` function with variable number of arguments,
// we need automatic type conversation, see
// http://en.cppreference.com/w/c/language/conversion#Default_argument_promotions
// for more details.
// return arg with conversation plus asserts
fn convert_args_for_variadic_function_call(
    f_method: &JniForeignMethodSignature,
) -> (String, &'static str) {
    use std::fmt::Write;

    let mut ret = String::new();
    for (i, arg) in f_method.input.iter().enumerate() {
        if let Some(conv_type) = JNI_FOR_VARIADIC_C_FUNC_CALL
            .get(&*arg.as_ref().correspoding_rust_type.normalized_name.as_str())
        {
            write!(&mut ret, ", a_{} as {}", i, conv_type).unwrap();
        } else {
            write!(&mut ret, ", a_{}", i).unwrap();
        }
    }
    let check_sizes = r#"
    swig_assert_eq_size!(::std::os::raw::c_uint, u32);
    swig_assert_eq_size!(::std::os::raw::c_int, i32);
"#;
    (ret, check_sizes)
}
