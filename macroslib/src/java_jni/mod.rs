mod java_code;
mod map_type;
mod rust_code;

use std::fmt;

use log::debug;
use petgraph::Direction;
use proc_macro2::TokenStream;
use smol_str::SmolStr;
use syn::{parse_quote, spanned::Spanned, Type};

use self::map_type::map_type;
use crate::{
    error::{DiagnosticError, Result},
    source_registry::SourceId,
    typemap::ast::{
        if_result_return_ok_err_types, if_ty_result_return_ok_type,
        parse_ty_with_given_span_checked, DisplayToTokens, TypeName,
    },
    typemap::{
        ty::RustType,
        utils::{
            convert_to_heap_pointer, unpack_from_heap_pointer, ForeignMethodSignature,
            ForeignTypeInfoT,
        },
        ForeignTypeInfo, FROM_VAR_TEMPLATE, TO_VAR_TEMPLATE,
    },
    types::{
        ForeignEnumInfo, ForeignInterface, ForeignerClassInfo, ForeignerMethod, ItemToExpand,
        MethodVariant,
    },
    JavaConfig, LanguageGenerator, SourceCode, TypeMap,
};

#[derive(Clone, Copy)]
enum NullAnnotation {
    NonNull,
    Nullable,
}

struct JavaForeignTypeInfo {
    pub base: ForeignTypeInfo,
    pub java_converter: Option<JavaConverter>,
    annotation: Option<NullAnnotation>,
}

impl ForeignTypeInfoT for JavaForeignTypeInfo {
    fn name(&self) -> &str {
        self.base.name.as_str()
    }
    fn correspoding_rust_type(&self) -> &RustType {
        &self.base.correspoding_rust_type
    }
}

struct JavaConverter {
    java_transition_type: SmolStr,
    converter: String,
}

impl AsRef<ForeignTypeInfo> for JavaForeignTypeInfo {
    fn as_ref(&self) -> &ForeignTypeInfo {
        &self.base
    }
}

impl From<ForeignTypeInfo> for JavaForeignTypeInfo {
    fn from(x: ForeignTypeInfo) -> Self {
        JavaForeignTypeInfo {
            base: ForeignTypeInfo {
                name: x.name,
                correspoding_rust_type: x.correspoding_rust_type,
            },
            java_converter: None,
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
    fn output(&self) -> &ForeignTypeInfoT {
        &self.output
    }
    fn input(&self) -> &[JavaForeignTypeInfo] {
        &self.input[..]
    }
}

impl JavaConfig {
    fn init(&self, conv_map: &mut TypeMap, _code: &[SourceCode]) {
        conv_map.find_or_alloc_rust_type_no_src_id(&parse_type! { jint });
        conv_map.find_or_alloc_rust_type_no_src_id(&parse_type! { jlong });
    }
    fn register_class(&self, conv_map: &mut TypeMap, class: &ForeignerClassInfo) -> Result<()> {
        class
            .validate_class()
            .map_err(|err| DiagnosticError::new(class.src_id, class.span(), &err))?;
        if let Some(constructor_ret_type) =
            class.self_desc.as_ref().map(|x| &x.constructor_ret_type)
        {
            let this_type_for_method = if_ty_result_return_ok_type(constructor_ret_type)
                .unwrap_or_else(|| constructor_ret_type.clone());

            let mut traits = vec!["SwigForeignClass"];
            if class.clone_derived {
                traits.push("Clone");
            }
            let this_type: RustType = conv_map.find_or_alloc_rust_type_that_implements(
                &this_type_for_method,
                &traits,
                class.src_id,
            );
            debug!(
                "register_class: add implements SwigForeignClass for {}",
                this_type
            );

            let my_jobj_ti = conv_map.find_or_alloc_rust_type_with_suffix(
                &parse_type! { jobject },
                &this_type.normalized_name,
                SourceId::none(),
            );

            conv_map.cache_rust_to_foreign_conv(
                &this_type,
                ForeignTypeInfo {
                    correspoding_rust_type: my_jobj_ti,
                    name: class.name.to_string().into(),
                },
            )?;

            conv_map.find_or_alloc_rust_type(constructor_ret_type, class.src_id);

            let (this_type_for_method, _code_box_this) =
                convert_to_heap_pointer(conv_map, &this_type, "this");

            let jlong_ti: RustType =
                conv_map.find_or_alloc_rust_type_no_src_id(&parse_type! { jlong });
            let this_type_for_method_ty = &this_type_for_method.ty;
            let code = format!("& {}", DisplayToTokens(this_type_for_method_ty));
            let gen_ty = parse_ty_with_given_span_checked(&code, this_type_for_method_ty.span());
            let this_type_ref =
                conv_map.find_or_alloc_rust_type(&gen_ty, this_type_for_method.src_id);
            //handle foreigner_class as input arg
            conv_map.add_conversation_rule(
                jlong_ti.to_idx(),
                this_type_ref.to_idx(),
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
            let code = format!("&mut {}", DisplayToTokens(this_type_for_method_ty));
            let gen_ty = parse_ty_with_given_span_checked(&code, this_type_for_method_ty.span());
            let this_type_mut_ref =
                conv_map.find_or_alloc_rust_type(&gen_ty, this_type_for_method.src_id);
            //handle foreigner_class as input arg
            conv_map.add_conversation_rule(
                jlong_ti.to_idx(),
                this_type_mut_ref.to_idx(),
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
                unpack_from_heap_pointer(&this_type_for_method, TO_VAR_TEMPLATE, true);
            conv_map.add_conversation_rule(
                jlong_ti.to_idx(),
                this_type.to_idx(),
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

        let _ = conv_map.find_or_alloc_rust_type(&class.self_type_as_ty(), class.src_id);

        Ok(())
    }

    fn generate(
        &self,
        conv_map: &mut TypeMap,
        class: &ForeignerClassInfo,
    ) -> Result<Vec<TokenStream>> {
        debug!(
            "generate: begin for {}, this_type_for_method {:?}",
            class.name, class.self_desc
        );

        let f_methods_sign = find_suitable_foreign_types_for_methods(conv_map, class)?;
        java_code::generate_java_code(
            conv_map,
            &self.output_dir,
            &self.package_name,
            class,
            &f_methods_sign,
            self.null_annotation_package.as_ref().map(String::as_str),
        )
        .map_err(|err| DiagnosticError::new(class.src_id, class.span(), err))?;
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
                enum_info.src_id,
                enum_info.span(),
                "Too many items in enum",
            ));
        }

        java_code::generate_java_code_for_enum(&self.output_dir, &self.package_name, enum_info)
            .map_err(|err| DiagnosticError::new(enum_info.src_id, enum_info.span(), &err))?;

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
        .map_err(|err| DiagnosticError::new(interface.src_id, interface.span(), err))?;
        let items = rust_code::generate_interface(
            &self.package_name,
            conv_map,
            pointer_target_width,
            interface,
            &f_methods,
        )?;

        let my_jobj_ti = conv_map.find_or_alloc_rust_type_with_suffix(
            &parse_type! { jobject },
            &interface.name.to_string(),
            SourceId::none(),
        );
        conv_map.add_foreign(
            my_jobj_ti,
            TypeName::from_ident(&interface.name, interface.src_id),
        )?;
        Ok(items)
    }
}

impl LanguageGenerator for JavaConfig {
    fn expand_items(
        &self,
        conv_map: &mut TypeMap,
        pointer_target_width: usize,
        code: &[SourceCode],
        items: Vec<ItemToExpand>,
    ) -> Result<Vec<TokenStream>> {
        self.init(conv_map, code);
        for item in &items {
            if let ItemToExpand::Class(ref fclass) = item {
                self.register_class(conv_map, fclass)?;
            }
        }
        let mut ret = Vec::with_capacity(items.len());
        for item in items {
            match item {
                ItemToExpand::Class(fclass) => ret.append(&mut self.generate(conv_map, &fclass)?),
                ItemToExpand::Enum(fenum) => {
                    ret.append(&mut self.generate_enum(conv_map, pointer_target_width, &fenum)?)
                }
                ItemToExpand::Interface(finterface) => ret.append(&mut self.generate_interface(
                    conv_map,
                    pointer_target_width,
                    &finterface,
                )?),
            }
        }
        Ok(ret)
    }
}

fn method_name(method: &ForeignerMethod, f_method: &JniForeignMethodSignature) -> String {
    let need_conv = f_method.input.iter().any(|v| v.java_converter.is_some());
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
    let dummy_rust_ty = conv_map.find_or_alloc_rust_type_no_src_id(&dummy_ty);
    let mut f_methods = vec![];

    for method in &interace.items {
        let mut input = Vec::<JavaForeignTypeInfo>::with_capacity(method.fn_decl.inputs.len() - 1);
        for arg in method.fn_decl.inputs.iter().skip(1) {
            let named_arg = arg
                .as_named_arg()
                .map_err(|err| DiagnosticError::from_syn_err(interace.src_id, err))?;
            let arg_rust_ty = conv_map.find_or_alloc_rust_type(&named_arg.ty, interace.src_id);
            let f_arg_type = map_type(
                conv_map,
                &arg_rust_ty,
                Direction::Outgoing,
                (interace.src_id, named_arg.ty.span()),
            )?;

            input.push(f_arg_type);
        }
        let output = match method.fn_decl.output {
            syn::ReturnType::Default => ForeignTypeInfo {
                name: void_sym.into(),
                correspoding_rust_type: dummy_rust_ty.clone(),
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
    let dummy_rust_ty = conv_map.find_or_alloc_rust_type_no_src_id(&dummy_ty);

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
            let named_arg = arg
                .as_named_arg()
                .map_err(|err| DiagnosticError::from_syn_err(class.src_id, err))?;
            let arg_rust_ty = conv_map.find_or_alloc_rust_type(&named_arg.ty, class.src_id);

            let fti = map_type(
                conv_map,
                &arg_rust_ty,
                Direction::Incoming,
                (class.src_id, named_arg.ty.span()),
            )?;
            input.push(fti);
        }
        let output = match method.variant {
            MethodVariant::Constructor => ForeignTypeInfo {
                name: empty_symbol.into(),
                correspoding_rust_type: dummy_rust_ty.clone(),
            },
            _ => match method.fn_decl.output {
                syn::ReturnType::Default => ForeignTypeInfo {
                    name: "void".into(),
                    correspoding_rust_type: dummy_rust_ty.clone(),
                },
                syn::ReturnType::Type(_, ref rt) => {
                    let ret_rust_ty = conv_map.find_or_alloc_rust_type(rt, class.src_id);
                    let fti = map_type(
                        conv_map,
                        &ret_rust_ty,
                        Direction::Outgoing,
                        (class.src_id, rt.span()),
                    )?;
                    fti.base
                }
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

fn calc_this_type_for_method(tm: &TypeMap, class: &ForeignerClassInfo) -> Option<Type> {
    if let Some(constructor_ret_type) = class.self_desc.as_ref().map(|x| &x.constructor_ret_type) {
        Some(
            if_result_return_ok_err_types(
                &tm.ty_to_rust_type_checked(constructor_ret_type)
                    .unwrap_or_else(|| {
                        panic!(
                            "Internal error: constructor type {} for class {} unknown",
                            DisplayToTokens(constructor_ret_type),
                            class.name
                        );
                    }),
            )
            .map(|(ok_ty, _err_ty)| ok_ty)
            .unwrap_or_else(|| constructor_ret_type.clone()),
        )
    } else {
        None
    }
}
