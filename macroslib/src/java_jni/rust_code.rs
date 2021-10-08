use quote::quote;
use rustc_hash::FxHashMap;
use smol_str::SmolStr;
use std::{io::Write, str};
use syn::{parse_quote, visit::Visit};

use super::{
    find_cache::{JniCacheMacroCalls, JniCacheMacroCallsVisitor},
    java_code::filter_null_annotation,
    JavaContext, JniForeignMethodSignature,
};
use crate::{
    error::{invalid_src_id_span, panic_on_syn_error, DiagnosticError, Result, SourceIdSpan},
    typemap::ast::DisplayToTokens,
    types::MethodVariant,
    WRITE_TO_MEM_FAILED_MSG,
};

pub(in crate::java_jni) fn predefined_java_type_to_jni_sig() -> FxHashMap<SmolStr, SmolStr> {
    let mut m = FxHashMap::default();
    m.insert("String".into(), "Ljava.lang.String;".into());
    m.insert("Byte".into(), "Ljava.lang.Byte".into());
    m.insert("Short".into(), "Ljava.lang.Short".into());
    m.insert("Integer".into(), "Ljava.lang.Integer".into());
    m.insert("Long".into(), "Ljava.lang.Long".into());
    m.insert("Float".into(), "Ljava.lang.Float".into());
    m.insert("Double".into(), "Ljava.lang.Double".into());
    m.insert("boolean".into(), "Z".into());
    m.insert("byte".into(), "B".into());
    m.insert("char".into(), "C".into());
    m.insert("double".into(), "D".into());
    m.insert("float".into(), "F".into());
    m.insert("int".into(), "I".into());
    m.insert("long".into(), "J".into());
    m.insert("object".into(), "L".into());
    m.insert("short".into(), "S".into());
    m.insert("void".into(), "V".into());
    for elem in &[
        "char", "byte", "double", "float", "int", "long", "short", "boolean",
    ] {
        let jni_sig = m.get(*elem).expect("Internal error: no type");
        let arr_sig = format!("[{}", jni_sig);
        m.insert(format!("{} []", elem).into(), arr_sig.into());
    }
    m
}

fn java_type_to_jni_signature<'a>(ctx: &'a JavaContext, java_type: &str) -> Option<&'a str> {
    if java_type.contains("@NonNull") || java_type.contains("@Nullable") {
        let java_type = filter_null_annotation(java_type);
        ctx.java_type_to_jni_sig_map
            .get(java_type.trim())
            .map(SmolStr::as_str)
    } else {
        ctx.java_type_to_jni_sig_map
            .get(java_type)
            .map(SmolStr::as_str)
    }
}

pub(in crate::java_jni) fn generate_jni_func_name(
    ctx: &JavaContext,
    class_name: &str,
    class_span: SourceIdSpan,
    java_method_name: &str,
    method_type: MethodVariant,
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
    escape_underscore(&ctx.cfg.package_name, &mut output);
    output.push('_');
    escape_underscore(class_name, &mut output);
    output.push('_');
    escape_underscore(java_method_name, &mut output);

    if overloaded {
        output.push_str("__");
        if let MethodVariant::Method(_) = method_type {
            output.push('J');
        }
        for arg in &f_method.input {
            let type_name = arg
                .java_converter
                .as_ref()
                .map(|x| x.java_transition_type.display())
                .unwrap_or_else(|| arg.as_ref().name.display());

            let type_name = java_type_to_jni_signature(ctx, type_name).ok_or_else(|| {
                DiagnosticError::new2(
                    class_span,
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

pub(in crate::java_jni) fn jni_method_signature(
    ctx: &JavaContext,
    method: &JniForeignMethodSignature,
) -> String {
    let mut ret: String = "(".into();
    for arg in &method.input {
        let java_type: String = filter_null_annotation(arg.as_ref().name.display())
            .trim()
            .into();
        let sig = java_type_to_jni_signature(ctx, &java_type).unwrap_or_else(|| {
            panic!(
                "Unknown type `{}`, can not generate JNI signature",
                java_type
            )
        });
        let sig = sig.replace('.', "/");
        ret.push_str(&sig);
    }
    ret.push(')');
    let sig =
        java_type_to_jni_signature(ctx, method.output.base.name.display()).unwrap_or_else(|| {
            panic!(
                "Unknown type `{}`, can not generate JNI signature",
                method.output.base.name
            )
        });
    ret.push_str(sig);
    ret
}

pub(in crate::java_jni) fn generate_load_unload_jni_funcs(
    generated_code: &mut Vec<u8>,
) -> Result<()> {
    let code = str::from_utf8(generated_code).map_err(|err| {
        DiagnosticError::new2(
            invalid_src_id_span(),
            format!("Generated code not valid utf-8: {}", err),
        )
    })?;

    let file = syn::parse_file(code)
        .unwrap_or_else(|err| panic_on_syn_error("generated code", code.into(), err));
    let mut jni_cache_macro_calls = JniCacheMacroCalls::default();
    let mut visitor = JniCacheMacroCallsVisitor {
        inner: &mut jni_cache_macro_calls,
        errors: vec![],
    };
    visitor.visit_file(&file);
    if !visitor.errors.is_empty() {
        panic_on_syn_error("generated code", code.to_string(), visitor.errors.remove(0));
    }

    let mut addon_code = Vec::with_capacity(3);

    addon_code.append(&mut jni_cache_macro_calls.global_vars());

    let mut find_calls = Vec::with_capacity(jni_cache_macro_calls.calls.len());
    let mut free_find_calls = Vec::with_capacity(jni_cache_macro_calls.calls.len());
    for find_class in jni_cache_macro_calls.calls.values() {
        let mut class_get_method_id_calls = Vec::new();
        let class_name = &find_class.path;
        for m in &find_class.methods {
            let method_id = &m.id;
            let method_name = &m.name;
            let method_sig = &m.sig;
            class_get_method_id_calls.push(quote! {
                let method_id: jmethodID = (**env).GetMethodID.unwrap()(
                    env,
                    class,
                    swig_c_str!(#method_name),
                    swig_c_str!(#method_sig),
                );
                assert!(!method_id.is_null(),
                        concat!("GetMethodID for class ", #class_name,
                                " method ", #method_name,
                                " sig ", #method_sig, " failed"));
                #method_id = method_id;
            });
        }

        for m in &find_class.static_methods {
            let method_id = &m.id;
            let method_name = &m.name;
            let method_sig = &m.sig;
            class_get_method_id_calls.push(quote! {
                let method_id: jmethodID = (**env).GetStaticMethodID.unwrap()(
                    env,
                    class,
                    swig_c_str!(#method_name),
                    swig_c_str!(#method_sig),
                );
                assert!(!method_id.is_null(),
                        concat!("GetStaticMethodID for class ", #class_name,
                                " method ", #method_name,
                                " sig ", #method_sig, " failed"));
                #method_id = method_id;
            });
        }

        for m in &find_class.static_fields {
            let field_id = &m.id;
            let field_name = &m.name;
            let method_sig = &m.sig;
            class_get_method_id_calls.push(quote! {
                let field_id: jfieldID = (**env).GetStaticFieldID.unwrap()(
                    env,
                    class,
                    swig_c_str!(#field_name),
                    swig_c_str!(#method_sig),
                );
                assert!(!field_id.is_null(),
                        concat!("GetStaticFieldID for class ", #class_name,
                                " method ", #field_name,
                                " sig ", #method_sig, " failed"));
                #field_id = field_id;
            });
        }

        for m in &find_class.fields {
            let field_id = &m.id;
            let field_name = &m.name;
            let method_sig = &m.sig;
            class_get_method_id_calls.push(quote! {
                let field_id: jfieldID = (**env).GetFieldID.unwrap()(
                    env,
                    class,
                    swig_c_str!(#field_name),
                    swig_c_str!(#method_sig),
                );
                assert!(!field_id.is_null(),
                        concat!("GetStaticFieldID for class ", #class_name,
                                " method ", #field_name,
                                " sig ", #method_sig, " failed"));
                #field_id = field_id;
            });
        }

        let id = &find_class.id;
        find_calls.push(quote! {
            unsafe {
                let class_local_ref = (**env).FindClass.unwrap()(env, swig_c_str!(#class_name));
                assert!(!class_local_ref.is_null(), concat!("FindClass failed for ", #class_name));
                let class = (**env).NewGlobalRef.unwrap()(env, class_local_ref);
                assert!(!class.is_null(), concat!("FindClass failed for ", #class_name));
                (**env).DeleteLocalRef.unwrap()(env, class_local_ref);
                #id = class;
                #(#class_get_method_id_calls)*
            }
        });
        free_find_calls.push(quote! {
            unsafe {
                (**env).DeleteGlobalRef.unwrap()(env, #id);
                #id = ::std::ptr::null_mut()
            }
        });
    }

    let jni_load_func: syn::Item = parse_quote! {
        #[no_mangle]
        pub extern "system" fn JNI_OnLoad(java_vm: *mut JavaVM, _reserved: *mut ::std::os::raw::c_void) -> jint {
            log::debug!("JNI_OnLoad begin");
            assert!(!java_vm.is_null());
            let mut env: *mut JNIEnv = ::std::ptr::null_mut();
            let res = unsafe {
                (**java_vm).GetEnv.unwrap()(
                    java_vm,
                    (&mut env) as *mut *mut JNIEnv as *mut *mut ::std::os::raw::c_void,
                    SWIG_JNI_VERSION,
                )
            };
            if res != (JNI_OK as jint) {
                panic!("JNI GetEnv in JNI_OnLoad failed, return code {}", res);
            }
            assert!(!env.is_null());
            #(#find_calls)*

            SWIG_JNI_VERSION
        }
    };
    addon_code.push(jni_load_func);
    let jni_unload_func: syn::Item = parse_quote! {
        #[no_mangle]
        pub extern "system" fn JNI_OnUnload(java_vm: *mut JavaVM, _reserved: *mut ::std::os::raw::c_void) {
            log::debug!("JNI_OnUnLoad begin");
            assert!(!java_vm.is_null());
            let mut env: *mut JNIEnv = ::std::ptr::null_mut();
            let res = unsafe {
                (**java_vm).GetEnv.unwrap()(
                    java_vm,
                    (&mut env) as *mut *mut JNIEnv as *mut *mut ::std::os::raw::c_void,
                    SWIG_JNI_VERSION,
                )
            };
            if res != (JNI_OK as jint) {
                panic!("JNI GetEnv in JNI_OnLoad failed, return code {}", res);
            }
            assert!(!env.is_null());
            #(#free_find_calls)*
        }
    };
    addon_code.push(jni_unload_func);

    for elem in addon_code {
        write!(generated_code, "{}", DisplayToTokens(&elem)).expect(WRITE_TO_MEM_FAILED_MSG);
    }

    Ok(())
}
