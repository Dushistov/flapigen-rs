use lazy_static::lazy_static;
use quote::quote;
use rustc_hash::FxHashMap;
use std::{io::Write, str};
use syn::{parse_quote, visit::Visit};

use super::{
    find_cache::JniCacheMacroCalls, java_class_full_name, java_code::filter_null_annotation,
    JniForeignMethodSignature,
};
use crate::{
    error::{invalid_src_id_span, panic_on_syn_error, DiagnosticError, Result},
    typemap::ast::DisplayToTokens,
    types::{ForeignerClassInfo, MethodVariant},
    TypeMap, WRITE_TO_MEM_FAILED_MSG,
};

lazy_static! {
    static ref JAVA_TYPE_NAMES_FOR_JNI_SIGNATURE: FxHashMap<&'static str, &'static str> = {
        let mut m = FxHashMap::default();
        m.insert("String", "Ljava.lang.String;");
        m.insert("Byte", "Ljava.lang.Byte");
        m.insert("Short", "Ljava.lang.Short");
        m.insert("Integer", "Ljava.lang.Integer");
        m.insert("Long", "Ljava.lang.Long");
        m.insert("Float", "Ljava.lang.Float");
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
}

fn java_type_to_jni_signature(java_type: &str) -> Option<&'static str> {
    if java_type.contains("@NonNull") || java_type.contains("@Nullable") {
        let java_type = filter_null_annotation(java_type);
        JAVA_TYPE_NAMES_FOR_JNI_SIGNATURE
            .get(&java_type.trim())
            .cloned()
    } else {
        JAVA_TYPE_NAMES_FOR_JNI_SIGNATURE.get(&java_type).cloned()
    }
}

pub(in crate::java_jni) fn generate_jni_func_name(
    package_name: &str,
    class: &ForeignerClassInfo,
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
    escape_underscore(package_name, &mut output);
    output.push_str("_");
    escape_underscore(&class.name.to_string(), &mut output);
    output.push_str("_");
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
                .map(|x| x.java_transition_type.as_str())
                .unwrap_or_else(|| arg.as_ref().name.as_str());

            let type_name = java_type_to_jni_signature(type_name).ok_or_else(|| {
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

pub(in crate::java_jni) fn jni_method_signature(
    method: &JniForeignMethodSignature,
    package_name: &str,
    conv_map: &TypeMap,
) -> String {
    let mut ret: String = "(".into();
    for arg in &method.input {
        let mut gen_sig = String::new();
        let java_type: String = filter_null_annotation(arg.as_ref().name.as_str())
            .trim()
            .into();
        let sig = java_type_to_jni_signature(&java_type)
            .or_else(|| {
                if conv_map.is_generated_foreign_type(&java_type) {
                    gen_sig = format!("L{};", &java_class_full_name(package_name, &java_type));
                    Some(&gen_sig)
                } else {
                    None
                }
            })
            .unwrap_or_else(|| {
                panic!(
                    "Unknown type `{}`, can not generate jni signature",
                    java_type
                )
            });
        let sig = sig.replace('.', "/");
        ret.push_str(&sig);
    }
    ret.push(')');
    let sig = java_type_to_jni_signature(method.output.base.name.as_str()).unwrap_or_else(|| {
        panic!(
            "Unknown type `{}`, can not generate jni signature",
            method.output.base.name
        )
    });
    ret.push_str(sig);
    ret
}

pub(in crate::java_jni) fn generate_load_unload_jni_funcs(
    generated_code: &mut Vec<u8>,
) -> Result<()> {
    let code = str::from_utf8(&generated_code).map_err(|err| {
        DiagnosticError::new2(
            invalid_src_id_span(),
            format!("Generated code not valid utf-8: {}", err),
        )
    })?;

    let file = syn::parse_file(code)
        .unwrap_or_else(|err| panic_on_syn_error("generated code", code.into(), err));
    let mut jni_cache_macro_calls = JniCacheMacroCalls::default();
    jni_cache_macro_calls.visit_file(&file);

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
            let method_name = &m.name;
            let method_sig = &m.sig;
            class_get_method_id_calls.push(quote! {
                let field_id: jfieldID = (**env).GetStaticFieldID.unwrap()(
                    env,
                    class,
                    swig_c_str!(#method_name),
                    swig_c_str!(#method_sig),
                );
                assert!(!field_id.is_null(),
                        concat!("GetStaticFieldID for class ", #class_name,
                                " method ", #method_name,
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
        pub extern "C" fn JNI_OnLoad(java_vm: *mut JavaVM, _reserved: *mut ::std::os::raw::c_void) -> jint {
            println!("JNI_OnLoad begin");
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
        pub extern "C" fn JNI_OnUnload(java_vm: *mut JavaVM, _reserved: *mut ::std::os::raw::c_void) {
            println!("JNI_OnUnLoad begin");
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
