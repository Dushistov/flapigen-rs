mod java_code;
mod rust_code;

use std::path::Path;

use syntex_syntax::ast;
use syntex_syntax::ptr::P;
use syntex_syntax::parse::ParseSess;
use syntex_syntax::symbol::Symbol;

use ForeignerClassInfo;
use ForeignTypesMap;
use TypesMapUpdater;
use types_map::norm_ty::normalized_ty_string;
use types_map::make_unique_rust_typename;
use my_ast::{is_type_name, unpack_generic_first_parameter};

pub(crate) fn generate(
    parse_sess: &ParseSess,
    types_map: &mut ForeignTypesMap,
    output_dir: &Path,
    package_name: &str,
    class: &ForeignerClassInfo,
) -> Result<Vec<P<ast::Item>>, String> {
    let mut methods_sign = Vec::with_capacity(class.methods.len());
    for method in &class.methods {
        methods_sign.push(types_map.resolve_types(parse_sess, &*method)?);
    }
    java_code::generate_java_code(output_dir, package_name, class, &methods_sign)?;
    let ret =
        rust_code::generate_rust_code(parse_sess, types_map, package_name, class, &methods_sign);
    if let Some(ref this_type_for_method) = class.this_type_for_method {
        let from_typename = Symbol::intern(&normalized_ty_string(this_type_for_method));
        let class_name_for_user = java_class_full_name(package_name, &class.name);
        let class_name_for_jni = java_class_name_to_jni(&class_name_for_user);
        let class_name_for_user = Symbol::intern(&class_name_for_user);
        types_map.add_conversation(
            from_typename,
            make_unique_rust_typename(Symbol::intern("jobject"), class_name_for_user),
            Some(class_name_for_user),
            format!(
                "\nlet {{to_var}}: jobject = \
                 object_to_jobject({{from_var}}, \"{}\", env);",
                class_name_for_jni
            ),
        )?;
    }

    ret
}

fn java_class_full_name(package_name: &str, class_name: &str) -> String {
    let mut ret: String = package_name.into();
    ret.push('.');
    ret.push_str(class_name);
    ret
}

fn java_class_name_to_jni(full_name: &str) -> String {
    full_name.replace(".", "/")
}

pub(crate) struct JniVecRetTypesFix;

impl TypesMapUpdater for JniVecRetTypesFix {
    fn update(&self, types_map: &mut ForeignTypesMap, class: &ForeignerClassInfo) {
        fn generate_conversation(
            types_map: &mut ForeignTypesMap,
            from_ty: &ast::Ty,
            foreign_in_type: Symbol,
            rust_in_type: Symbol,
        ) {
            let in_type_class_name_for_jni = java_class_name_to_jni(&foreign_in_type.as_str());

            let code = format!(
                "\nlet {{to_var}}: jobjectArray = \
                 vec_of_objects_to_jobject_array({{from_var}}, \"{}\", env);",
                in_type_class_name_for_jni
            );
            let from_typename = Symbol::intern(&normalized_ty_string(from_ty));
            let foreign_res_typename = Symbol::intern(&format!("{} []", foreign_in_type));
            types_map
                .add_conversation(
                    from_typename,
                    make_unique_rust_typename(Symbol::intern("jobjectArray"), rust_in_type),
                    Some(foreign_res_typename),
                    code,
                )
                .unwrap_or_else(|err| {
                    panic!(
                        "Can not add conversation from {} to {}: {}",
                        from_typename,
                        foreign_res_typename,
                        err
                    );
                });
        }
        for method in &class.methods {
            if let ast::FunctionRetTy::Ty(ref ret_type) = method.fn_decl.output {

                if types_map
                    .foreign_return_type(&method.fn_decl.output)
                    .is_none() && is_type_name(ret_type, "Vec")
                {
                    let in_type = unpack_generic_first_parameter(ret_type, "Vec");
                    let in_type_name = Symbol::intern(&normalized_ty_string(&in_type));

                    debug!(
                        "{}:{} we have vec with type {:?}",
                        file!(),
                        line!(),
                        in_type
                    );
                    if let Some((foreign_in_type, rust_in_type)) =
                        types_map.to_foreign_type_name(in_type_name)
                    {
                        if rust_in_type.as_str().starts_with("jobject") {
                            generate_conversation(
                                types_map,
                                ret_type,
                                foreign_in_type,
                                rust_in_type,
                            );
                        }
                    }
                }
            }
        }
    }
}

pub(crate) struct JniResultRetTypesFix;

impl TypesMapUpdater for JniResultRetTypesFix {
    fn update(&self, types_map: &mut ForeignTypesMap, class: &ForeignerClassInfo) {
        fn get_default_value_for_rust_type(rust_type_name: &str) -> &'static str {
            match rust_type_name {
                "()" => "()",
                "i8" | "u8" | "u16" | "i16" | "u32" | "i32" | "u64" | "i64" => "0",
                "f32" => "::std::f32::NAN",
                "f64" => "::std::f64::NAN",
                _ => "::std::ptr::null_mut()",
            }
        }


        fn generate_conversation(
            types_map: &mut ForeignTypesMap,
            from_ty: &ast::Ty,
            rust_in_type: Symbol,
        ) {
            let from_typename = Symbol::intern(&normalized_ty_string(from_ty));
            debug!(
                "{}:{} gen conv for type {} ({})",
                file!(),
                line!(),
                rust_in_type,
                from_typename
            );
            let def_val = get_default_value_for_rust_type(&rust_in_type.as_str());
            let code = format!(
                "\nlet {{to_var}} = jni_unpack_return!({{from_var}}, {}, env);",
                def_val
            );
            types_map
                .add_conversation(from_typename, rust_in_type, None, code)
                .unwrap_or_else(|err| {
                    panic!(
                        "Can not add conversation from {} to {}: {}",
                        from_typename,
                        rust_in_type,
                        err
                    );
                });
        }
        for method in &class.methods {
            if let ast::FunctionRetTy::Ty(ref ret_type) = method.fn_decl.output {

                if types_map
                    .foreign_return_type(&method.fn_decl.output)
                    .is_none() && is_type_name(ret_type, "Result")
                {

                    let in_type = unpack_generic_first_parameter(ret_type, "Result");
                    let in_type_name = Symbol::intern(&normalized_ty_string(&in_type));
                    debug!("{}:{} we have Result<{:?},...>", file!(), line!(), in_type);

                    if let Some((_, _)) = types_map.to_foreign_type_name(in_type_name) {
                        generate_conversation(types_map, ret_type, in_type_name);
                    }
                }
            }
        }
    }
}
