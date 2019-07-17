use lazy_static::lazy_static;
use rustc_hash::FxHashMap;

use crate::{
    error::{DiagnosticError, Result},
    java_jni::{java_class_full_name, JniForeignMethodSignature},
    types::{ForeignerClassInfo, MethodVariant},
    TypeMap,
};

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

pub(in crate::java_jni) fn jni_method_signature(
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
        .get(&*method.output.base.name.as_str())
        .unwrap_or_else(|| {
            panic!(
                "Unknown type `{}`, can not generate jni signature",
                method.output.base.name
            )
        });
    ret.push_str(sig);
    ret
}
