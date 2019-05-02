mod java_code;
mod map_type;
mod rust_code;

use std::fmt;

use log::debug;
use petgraph::Direction;
use proc_macro2::TokenStream;
use smol_str::SmolStr;
use syn::{parse_quote, spanned::Spanned, Type};

use self::map_type::special_type;
use crate::{
    ast::{change_span, fn_arg_type, if_option_return_some_type, normalize_ty_lifetimes},
    error::{DiagnosticError, Result},
    typemap::{
        make_unique_rust_typename, ty::RustType, ForeignMethodSignature, ForeignTypeInfo,
        FROM_VAR_TEMPLATE, TO_VAR_TEMPLATE,
    },
    ForeignEnumInfo, ForeignInterface, ForeignerClassInfo, ForeignerMethod, JavaConfig,
    LanguageGenerator, MethodVariant, TypeMap,
};

#[derive(Clone, Copy)]
enum NullAnnotation {
    NonNull,
    Nullable,
}

struct JavaForeignTypeInfo {
    pub base: ForeignTypeInfo,
    pub java_transition_type: Option<SmolStr>,
    java_converter: String,
    annotation: Option<NullAnnotation>,
}

impl AsRef<ForeignTypeInfo> for JavaForeignTypeInfo {
    fn as_ref(&self) -> &ForeignTypeInfo {
        &self.base
    }
}

impl JavaForeignTypeInfo {
    fn java_need_conversation(&self) -> bool {
        !self.java_converter.is_empty()
    }
    fn java_convert<NameArg: Fn() -> (String, String)>(&self, name_arg: NameArg) -> Option<String> {
        if !self.java_converter.is_empty() {
            let (from_name, to_name) = name_arg();
            Some(
                self.java_converter
                    .replace(TO_VAR_TEMPLATE, &to_name)
                    .replace(FROM_VAR_TEMPLATE, &from_name),
            )
        } else {
            None
        }
    }
}

impl From<ForeignTypeInfo> for JavaForeignTypeInfo {
    fn from(x: ForeignTypeInfo) -> Self {
        JavaForeignTypeInfo {
            base: ForeignTypeInfo {
                name: x.name,
                correspoding_rust_type: x.correspoding_rust_type,
            },
            java_transition_type: None,
            java_converter: String::new(),
            annotation: None,
        }
    }
}

struct JniForeignMethodSignature {
    output: ForeignTypeInfo,
    input: Vec<JavaForeignTypeInfo>,
}

impl ForeignMethodSignature for JniForeignMethodSignature {
    type FI = JavaForeignTypeInfo;
    fn output(&self) -> &ForeignTypeInfo {
        &self.output
    }
    fn input(&self) -> &[JavaForeignTypeInfo] {
        &self.input[..]
    }
}

impl LanguageGenerator for JavaConfig {
    fn register_class(&self, conv_map: &mut TypeMap, class: &ForeignerClassInfo) -> Result<()> {
        class
            .validate_class()
            .map_err(|err| DiagnosticError::new(class.span(), &err))?;
        if let Some(this_type_for_method) = class.this_type_for_method.as_ref() {
            let this_type: RustType = this_type_for_method.clone().into();
            let this_type = this_type.implements("SwigForeignClass");
            debug!(
                "register_class: add implements SwigForeignClass for {}",
                this_type.normalized_name
            );
            let jobject_name = "jobject";
            let jobject_ty = parse_type! { jobject };
            let my_jobj_ti = RustType::new(
                jobject_ty,
                make_unique_rust_typename(jobject_name, &this_type.normalized_name),
            );
            conv_map.cache_rust_to_foreign_conv(
                &this_type,
                ForeignTypeInfo {
                    correspoding_rust_type: my_jobj_ti,
                    name: class.name.to_string().into(),
                },
            );

            if let Some(constructor_ret_type) = class.constructor_ret_type.as_ref() {
                conv_map.add_type(this_type.clone());

                let constructor_ret_type: RustType = constructor_ret_type.clone().into();
                conv_map.add_type(constructor_ret_type);

                let (this_type_for_method, _code_box_this) =
                    TypeMap::convert_to_heap_pointer(&this_type, "this");

                let jlong_ti: RustType = parse_type! { jlong }.into();
                let this_type_for_method_ty = &this_type_for_method.ty;
                //handle foreigner_class as input arg
                conv_map.add_conversation_rule(
                    jlong_ti.clone(),
                    parse_type! { & #this_type_for_method_ty }.into(),
                    format!(
                        r#"
        let {to_var}: &{this_type} = unsafe {{
            jlong_to_pointer::<{this_type}>({from_var}).as_mut().unwrap()
        }};
    "#,
                        to_var = TO_VAR_TEMPLATE,
                        from_var = FROM_VAR_TEMPLATE,
                        this_type = this_type_for_method.normalized_name,
                    )
                    .into(),
                );

                //handle foreigner_class as input arg
                conv_map.add_conversation_rule(
                    jlong_ti.clone(),
                    parse_type! { &mut #this_type_for_method_ty }.into(),
                    format!(
                        r#"
        let {to_var}: &mut {this_type} = unsafe {{
            jlong_to_pointer::<{this_type}>({from_var}).as_mut().unwrap()
        }};
    "#,
                        to_var = TO_VAR_TEMPLATE,
                        from_var = FROM_VAR_TEMPLATE,
                        this_type = this_type_for_method.normalized_name,
                    )
                    .into(),
                );

                let unpack_code =
                    TypeMap::unpack_from_heap_pointer(&this_type_for_method, TO_VAR_TEMPLATE, true);
                conv_map.add_conversation_rule(
                    jlong_ti,
                    this_type,
                    format!(
                        r#"
        let {to_var}: *mut {this_type} = unsafe {{
            jlong_to_pointer::<{this_type}>({from_var}).as_mut().unwrap()
        }};
    {unpack_code}
    "#,
                        to_var = TO_VAR_TEMPLATE,
                        from_var = FROM_VAR_TEMPLATE,
                        this_type = this_type_for_method.normalized_name,
                        unpack_code = unpack_code,
                    )
                    .into(),
                );
            }
        }

        let _ = conv_map.find_or_alloc_rust_type(&class.self_type_as_ty());

        Ok(())
    }

    fn generate(
        &self,
        conv_map: &mut TypeMap,
        _: usize,
        class: &ForeignerClassInfo,
    ) -> Result<Vec<TokenStream>> {
        debug!(
            "generate: begin for {}, this_type_for_method {:?}",
            class.name, class.this_type_for_method
        );

        let f_methods_sign = find_suitable_foreign_types_for_methods(conv_map, class)?;
        java_code::generate_java_code(
            &self.output_dir,
            &self.package_name,
            class,
            &f_methods_sign,
            self.null_annotation_package.as_ref().map(String::as_str),
        )
        .map_err(|err| DiagnosticError::new(class.span(), &err))?;
        debug!("generate: java code done");
        let ast_items =
            rust_code::generate_rust_code(conv_map, &self.package_name, class, &f_methods_sign)?;

        Ok(ast_items)
    }

    fn generate_enum(
        &self,
        conv_map: &mut TypeMap,
        pointer_target_width: usize,
        enum_info: &ForeignEnumInfo,
    ) -> Result<Vec<TokenStream>> {
        if (enum_info.items.len() as u64) >= (i32::max_value() as u64) {
            return Err(DiagnosticError::new(
                enum_info.span(),
                "Too many items in enum",
            ));
        }

        java_code::generate_java_code_for_enum(&self.output_dir, &self.package_name, enum_info)
            .map_err(|err| DiagnosticError::new(enum_info.span(), &err))?;

        rust_code::generate_rust_code_for_enum(
            &self.package_name,
            conv_map,
            pointer_target_width,
            enum_info,
        )
    }

    fn generate_interface(
        &self,
        conv_map: &mut TypeMap,
        pointer_target_width: usize,
        interface: &ForeignInterface,
    ) -> Result<Vec<TokenStream>> {
        let f_methods = find_suitable_ftypes_for_interace_methods(conv_map, interface)?;
        java_code::generate_java_code_for_interface(
            &self.output_dir,
            &self.package_name,
            interface,
            &f_methods,
            self.null_annotation_package.as_ref().map(String::as_str),
        )
        .map_err(|err| DiagnosticError::new(interface.span(), err))?;
        let items = rust_code::generate_interface(
            &self.package_name,
            conv_map,
            pointer_target_width,
            interface,
            &f_methods,
        )?;
        let jobject_name = "jobject";
        let jobject_ty = parse_type! { jobject };
        let my_jobj_ti = RustType::new(
            jobject_ty,
            make_unique_rust_typename(jobject_name, &interface.name.to_string()),
        );
        conv_map.add_foreign(my_jobj_ti, interface.name.to_string().into());
        Ok(items)
    }
}

fn method_name(method: &ForeignerMethod, f_method: &JniForeignMethodSignature) -> String {
    let need_conv = f_method.input.iter().any(|v| v.java_need_conversation());
    match method.variant {
        MethodVariant::StaticMethod if !need_conv => method.short_name().as_str().to_string(),
        MethodVariant::Method(_) | MethodVariant::StaticMethod => {
            format!("do_{}", method.short_name())
        }
        MethodVariant::Constructor => "init".into(),
    }
}

fn find_suitable_ftypes_for_interace_methods(
    conv_map: &mut TypeMap,
    interace: &ForeignInterface,
) -> Result<Vec<JniForeignMethodSignature>> {
    let void_sym = "void";
    let dummy_ty = parse_type! { () };
    let mut f_methods = vec![];

    for method in &interace.items {
        let mut input = Vec::<JavaForeignTypeInfo>::with_capacity(method.fn_decl.inputs.len() - 1);
        for arg in method.fn_decl.inputs.iter().skip(1) {
            let f_arg_type = conv_map
                .map_through_conversation_to_foreign(
                    fn_arg_type(arg),
                    Direction::Outgoing,
                    fn_arg_type(arg).span(),
                )
                .ok_or_else(|| {
                    DiagnosticError::new(
                        fn_arg_type(arg).span(),
                        format!(
                            "Do not know conversation to foreign \
                             from such rust type '{}'",
                            normalize_ty_lifetimes(fn_arg_type(arg))
                        ),
                    )
                })?;
            input.push(f_arg_type.into());
        }
        let output = match method.fn_decl.output {
            syn::ReturnType::Default => ForeignTypeInfo {
                name: void_sym.into(),
                correspoding_rust_type: {
                    let mut ty: Type = dummy_ty.clone();
                    change_span(&mut ty, method.fn_decl.span());
                    ty.into()
                },
            },
            _ => unimplemented!(),
        };
        f_methods.push(JniForeignMethodSignature { output, input });
    }
    Ok(f_methods)
}

fn find_suitable_foreign_types_for_methods(
    conv_map: &mut TypeMap,
    class: &ForeignerClassInfo,
) -> Result<Vec<JniForeignMethodSignature>> {
    let mut ret = Vec::<JniForeignMethodSignature>::with_capacity(class.methods.len());
    let empty_symbol = "";
    let dummy_ty = parse_type! { () };

    for method in &class.methods {
        //skip self argument
        let skip_n = match method.variant {
            MethodVariant::Method(_) => 1,
            _ => 0,
        };
        assert!(method.fn_decl.inputs.len() >= skip_n);
        let mut input =
            Vec::<JavaForeignTypeInfo>::with_capacity(method.fn_decl.inputs.len() - skip_n);
        for arg in method.fn_decl.inputs.iter().skip(skip_n) {
            if let Some(converter) = special_type(conv_map, fn_arg_type(arg))? {
                input.push(converter);
                continue;
            }

            let f_arg_type = conv_map
                .map_through_conversation_to_foreign(
                    fn_arg_type(arg),
                    Direction::Incoming,
                    fn_arg_type(arg).span(),
                )
                .ok_or_else(|| {
                    DiagnosticError::new(
                        fn_arg_type(arg).span(),
                        format!(
                            "Do not know conversation from foreign \
                             to such rust type '{}'",
                            normalize_ty_lifetimes(fn_arg_type(arg))
                        ),
                    )
                })?;

            let mut f_arg_type: JavaForeignTypeInfo = f_arg_type.into();
            if !primitive_type(&f_arg_type.base.name) {
                f_arg_type.annotation =
                    Some(if if_option_return_some_type(fn_arg_type(arg)).is_none() {
                        NullAnnotation::NonNull
                    } else {
                        NullAnnotation::Nullable
                    });
            }
            input.push(f_arg_type);
        }
        let output = match method.variant {
            MethodVariant::Constructor => ForeignTypeInfo {
                name: empty_symbol.into(),
                correspoding_rust_type: dummy_ty.clone().into(),
            },
            _ => match method.fn_decl.output {
                syn::ReturnType::Default => ForeignTypeInfo {
                    name: "void".into(),
                    correspoding_rust_type: {
                        let mut ty: Type = dummy_ty.clone();
                        change_span(&mut ty, method.fn_decl.output.span());
                        ty.into()
                    },
                },
                syn::ReturnType::Type(_, ref rt) => conv_map
                    .map_through_conversation_to_foreign(&*rt, Direction::Outgoing, rt.span())
                    .ok_or_else(|| {
                        DiagnosticError::new(
                            rt.span(),
                            format!(
                                "Do not know conversation from \
                                 such rust type '{}' to foreign",
                                normalize_ty_lifetimes(&*rt)
                            ),
                        )
                    })?,
            },
        };
        ret.push(JniForeignMethodSignature { output, input });
    }
    Ok(ret)
}

fn fmt_write_err_map(err: fmt::Error) -> String {
    format!("fmt write error: {}", err)
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

fn primitive_type(type_name: &str) -> bool {
    match type_name {
        "void" | "boolean" | "byte" | "short" | "int" | "long" | "float" | "double" => true,
        _ => false,
    }
}
