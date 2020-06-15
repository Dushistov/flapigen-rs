use log::debug;
use std::rc::Rc;
use syn::spanned::Spanned;

use crate::{
    error::*,
    typemap::{
        ast::{self, TypeName},
        ty::{ForeignConversationIntermediate, ForeignConversationRule, ForeignTypeS, RustType},
        utils::{boxed_type, unpack_from_heap_pointer},
        RustTypeIdx, TypeConvCode, FROM_VAR_TEMPLATE, TO_VAR_TEMPLATE,
    },
    types::{ForeignerClassInfo, MethodVariant, SelfTypeDesc},
    TypeMap, SMART_PTR_COPY_TRAIT,
};

pub(crate) fn register_class(conv_map: &mut TypeMap, class: &ForeignerClassInfo) -> Result<()> {
    class
        .validate_class()
        .map_err(|err| DiagnosticError::new(class.src_id, class.span(), err))?;
    if let Some(self_desc) = class.self_desc.as_ref() {
        // let constructor_ret_type = &self_desc.constructor_ret_type;
        // let this_type_for_method = constructor_ret_type;
        let self_ty = &self_desc.self_type;
        let mut traits = vec!["SwigForeignClass"];
        if class.clone_derived {
            traits.push("Clone");
        }
        if class.copy_derived {
            if !class.clone_derived {
                traits.push("Clone");
            }
            traits.push("Copy");
        }

        if class.smart_ptr_copy_derived {
            traits.push(SMART_PTR_COPY_TRAIT);
        }

        let self_type = conv_map.find_or_alloc_rust_type_that_implements(
            self_ty,
            &traits,
            class.src_id,
        );

        // if class.smart_ptr_copy_derived {
        //     if class.copy_derived {
        //         println!(
        //             "warning=class {} marked as Copy and {}, ignore Copy",
        //             class.name, SMART_PTR_COPY_TRAIT
        //         );
        //     }
        //     if ast::check_if_smart_pointer_return_inner_type(&this_type, "Rc").is_none()
        //         && ast::check_if_smart_pointer_return_inner_type(&this_type, "Arc").is_none()
        //     {
        //         return Err(DiagnosticError::new(
        //             class.src_id,
        //             this_type.ty.span(),
        //             format!(
        //                 "class {} marked as {}, but type '{}' is not Arc<> or Rc<>",
        //                 class.name, SMART_PTR_COPY_TRAIT, this_type
        //             ),
        //         ));
        //     }

            // let has_clone = class.methods.iter().any(|x| match x.variant {
            //     MethodVariant::Method(_) | MethodVariant::StaticMethod => {
            //         x.rust_id.is_ident("clone")
            //     }
            //     MethodVariant::Constructor => false,
            // });
            // if has_clone {
            //     return Err(DiagnosticError::new(
            //         class.src_id,
            //         this_type.ty.span(),
            //         format!(
            //             "class {} marked as {}, but has clone method. Error: can not generate clone method.",
            //             class.name, SMART_PTR_COPY_TRAIT,
            //         ),
            //     ));
            // }
        // }

        register_typemap_for_self_type(conv_map, class, self_type, self_desc)?;
    }
    conv_map.find_or_alloc_rust_type(&class.self_type_as_ty(), class.src_id);
    Ok(())
}

fn register_typemap_for_self_type(
    conv_map: &mut TypeMap,
    class: &ForeignerClassInfo,
    self_type: RustType,
    self_desc: &SelfTypeDesc,
) -> Result<()> {
    let span = self_type.ty.span();

    // let void_ptr_ty = parse_type_spanned_checked!(span, *mut ::std::os::raw::c_void);
    // let void_ptr_rust_ty = conv_map.find_or_alloc_rust_type_with_suffix(
    //     &void_ptr_ty,
    //     &this_type.normalized_name,
    //     class.src_id,
    // );

    // let const_void_ptr_ty = parse_type_spanned_checked!(span, *const ::std::os::raw::c_void);
    // let const_void_ptr_rust_ty = conv_map.find_or_alloc_rust_type_with_suffix(
    //     &const_void_ptr_ty,
    //     &this_type.normalized_name,
    //     class.src_id,
    // );

    // let self_type = boxed_type(conv_map, &storage_type);
    let self_ty = &self_type.ty;
    let storage_ty = parse_type_spanned_checked!(span, Box<#self_ty>);
    let storage_type = conv_map.find_or_alloc_rust_type(&storage_ty, class.src_id);
    let storage_ptr_ty = parse_type_spanned_checked!(span, *mut #storage_ty);
    let storage_ptr_type = conv_map.find_or_alloc_rust_type(&storage_ptr_ty, class.src_id);

    // let span = self_type.ty.span();
    // let ty = self_type.to_type_without_lifetimes();
    let gen_ty = parse_type_spanned_checked!(span, & #self_ty);
    let self_type_ref = conv_map.find_or_alloc_rust_type(&gen_ty, class.src_id);

    let gen_ty = parse_type_spanned_checked!(span, &mut #self_ty);
    let self_type_mut_ref = conv_map.find_or_alloc_rust_type(&gen_ty, class.src_id);

    register_intermediate_pointer_types(
        conv_map,
        class,
        storage_ptr_type.to_idx(),
        // void_ptr_rust_ty.to_idx(),
        // const_void_ptr_rust_ty.to_idx(),
    )?;
    register_rust_ty_conversation_rules(
        conv_map,
        class,
        storage_ptr_type.to_idx(),
        storage_type.to_idx(),
        // this_type.clone(),
        self_type.to_idx(),
        // void_ptr_rust_ty.to_idx(),
        // const_void_ptr_rust_ty.to_idx(),
        self_type_ref.to_idx(),
        self_type_mut_ref.to_idx(),
    )?;

    // let this_type = conv_map.find_or_alloc_rust_type(&self_desc.self_type, class.src_id);

    register_main_foreign_types(
        conv_map,
        class,
        storage_ptr_type.to_idx(),
        self_type.to_idx(),
        // void_ptr_rust_ty.to_idx(),
        // const_void_ptr_rust_ty.to_idx(),
        // self_type_ref.to_idx(),
        // self_type_mut_ref.to_idx(),
    )?;
    Ok(())
}

fn register_intermediate_pointer_types(
    conv_map: &mut TypeMap,
    class: &ForeignerClassInfo,
    storage_ptr_type: RustTypeIdx,
    // void_ptr_rust_ty: RustTypeIdx,
    // const_void_ptr_rust_ty: RustTypeIdx,
) -> Result<()> {
    let c_ftype = ForeignTypeS {
        name: TypeName::new(
            format!("/* {} */ IntPtr", class.name),
            (class.src_id, class.name.span()),
        ),
        provides_by_module: vec![],
        into_from_rust: Some(ForeignConversationRule {
            rust_ty: storage_ptr_type,
            intermediate: None,
        }),
        from_into_rust: Some(ForeignConversationRule {
            rust_ty: storage_ptr_type,
            intermediate: None,
        }),
        name_prefix: None,
    };
    conv_map.alloc_foreign_type(c_ftype)?;

    // let c_const_ftype = ForeignTypeS {
    //     name: TypeName::new(
    //         format!("/* &{} */ IntPtr", class.name),
    //         (class.src_id, class.name.span()),
    //     ),
    //     provides_by_module: vec![],
    //     into_from_rust: Some(ForeignConversationRule {
    //         rust_ty: const_void_ptr_rust_ty,
    //         intermediate: None,
    //     }),
    //     from_into_rust: Some(ForeignConversationRule {
    //         rust_ty: const_void_ptr_rust_ty,
    //         intermediate: None,
    //     }),
    //     name_prefix: None,
    // };
    // conv_map.alloc_foreign_type(c_const_ftype)?;
    Ok(())
}

fn register_rust_ty_conversation_rules(
    conv_map: &mut TypeMap,
    class: &ForeignerClassInfo,
    storage_ptr_type: RustTypeIdx,
    storage_type: RustTypeIdx,
    self_type: RustTypeIdx,
    // void_ptr_rust_ty: RustTypeIdx,
    // const_void_ptr_rust_ty: RustTypeIdx,
    self_type_ref: RustTypeIdx,
    self_type_mut_ref: RustTypeIdx,
) -> Result<()> {
    // *mut "storage_type" -> "storage_type"
    conv_map.add_conversation_rule(
        storage_ptr_type,
        storage_type,
        TypeConvCode::new2(
            format!(
                r#"
    assert!(!{from_var}.is_null());
    let {to_var} = unsafe {{ &*{from_var} }}.clone();
"#,
                to_var = TO_VAR_TEMPLATE,
                from_var = FROM_VAR_TEMPLATE,
                // this_type_ref = conv_map[this_type_ref],
                // this_type_inner = conv_map[this_type_inner],
            ),
            invalid_src_id_span(),
        )
        .into(),
    );

    // "storage_type" -> *mut "storage_type"
    conv_map.add_conversation_rule(
        storage_type,
        storage_ptr_type,
        TypeConvCode::new2(
            format!(
                r#"
    let {to_var} = {from_var}.swig_leak_into_raw();
"#,
                to_var = TO_VAR_TEMPLATE,
                from_var = FROM_VAR_TEMPLATE,
                // this_type_ref = conv_map[this_type_ref],
                // this_type_inner = conv_map[this_type_inner],
            ),
            invalid_src_id_span(),
        )
        .into(),
    );

    // *mut "storage_type" -> &"class"
    conv_map.add_conversation_rule(
        storage_ptr_type,
        self_type_ref,
        TypeConvCode::new2(
            format!(
                r#"
    assert!(!{from_var}.is_null());
    let {to_var} = unsafe {{ &*{from_var} }}.swig_as_ref();
"#,
                to_var = TO_VAR_TEMPLATE,
                from_var = FROM_VAR_TEMPLATE,
                // this_type_ref = conv_map[this_type_ref],
                // this_type_inner = conv_map[this_type_inner],
            ),
            invalid_src_id_span(),
        )
        .into(),
    );

    // *mut "storage_type" -> &mut "class"
    conv_map.add_conversation_rule(
        storage_ptr_type,
        self_type_mut_ref,
        TypeConvCode::new2(
            format!(
                r#"
    assert!(!{from_var}.is_null());
    let {to_var} = unsafe {{ &mut *{from_var} }}.swig_as_mut();
"#,
                to_var = TO_VAR_TEMPLATE,
                from_var = FROM_VAR_TEMPLATE,
                // this_type_mut_ref = conv_map[this_type_mut_ref],
                // this_type_inner = conv_map[this_type_inner],
            ),
            invalid_src_id_span(),
        )
        .into(),
    );

    // *mut "storage_type" -> "class"
    conv_map.add_conversation_rule(
        storage_ptr_type,
        self_type,
        TypeConvCode::new2(
            format!(
                r#"
    assert!(!{from_var}.is_null());
    let {to_var} = unsafe {{ &mut *{from_var} }}.swig_cloned();
"#,
                to_var = TO_VAR_TEMPLATE,
                from_var = FROM_VAR_TEMPLATE,
                // this_type_mut_ref = conv_map[this_type_mut_ref],
                // this_type_inner = conv_map[this_type_inner],
            ),
            invalid_src_id_span(),
        )
        .into(),
    );

    // *const c_void -> "class", two steps to make it more expensive
    // for type graph path search
    // let ty = conv_map[this_type_inner].to_type_without_lifetimes();
    // let span = ty.span();
    // let gen_ty = parse_type_spanned_checked!(span, *mut #ty);
    // let this_type_mut_ptr = conv_map.find_or_alloc_rust_type(&gen_ty, class.src_id);

    // conv_map.add_conversation_rule(
    //     storage_ptr_type,
    //     this_type_mut_ptr.to_idx(),
    //     TypeConvCode::new2(
    //         format!(
    //             r#"
    //         assert!(!{from_var}.is_null());
    //         let {to_var}: {this_type_mut_ptr} = {from_var} as {this_type_mut_ptr};
    //     "#,
    //             to_var = TO_VAR_TEMPLATE,
    //             from_var = FROM_VAR_TEMPLATE,
    //             this_type_mut_ptr = this_type_mut_ptr,
    //         ),
    //         invalid_src_id_span(),
    //     )
    //     .into(),
    // );

    // //let unpack_code = unpack_from_heap_pointer(&this_type, TO_VAR_TEMPLATE, true);
    // conv_map.add_conversation_rule(
    //     this_type_mut_ptr.to_idx(),
    //     this_type.to_idx(),
    //     TypeConvCode::new(
    //         format!(
    //             r#"
    //         let {to_var} = unsafe {{ (*{to_var}).clone() }};
    //     "#,
    //             //unpack_code = unpack_code,
    //             to_var = TO_VAR_TEMPLATE
    //         ),
    //         invalid_src_id_span(),
    //     )
    //     .into(),
    // );

    // "class" -> *mut "storage_type"
    conv_map.add_conversation_rule(
        self_type,
        storage_ptr_type,
        TypeConvCode::new(
            format!(
                "let {to_var} = {from_var}.swig_into_storage_type().swig_leak_into_raw();",
                to_var = TO_VAR_TEMPLATE,
                from_var = FROM_VAR_TEMPLATE
            ),
            invalid_src_id_span(),
        )
        .into(),
    );

    //&"class" -> *mut "storage_type"
    conv_map.add_conversation_rule(
        self_type_ref,
        storage_ptr_type,
        TypeConvCode::new(
            format!(
                "let {to_var} = {from_var}.clone().swig_into_storage_type().swig_leak_into_raw();",
                to_var = TO_VAR_TEMPLATE,
                from_var = FROM_VAR_TEMPLATE
            ),
            invalid_src_id_span(),
        )
        .into(),
    );

    Ok(())
}

fn register_main_foreign_types(
    conv_map: &mut TypeMap,
    class: &ForeignerClassInfo,
    storage_ptr_type: RustTypeIdx,
    self_type: RustTypeIdx,
    // void_ptr_rust_ty: RustTypeIdx,
    // const_void_ptr_rust_ty: RustTypeIdx,
    // this_type_ref: RustTypeIdx,
    // this_type_mut_ref: RustTypeIdx,
) -> Result<()> {
    debug!(
        "register_main_foreign_types: self {}, storage {}",
        conv_map[self_type], conv_map[storage_ptr_type]
    );
    let class_ftype = ForeignTypeS {
        name: TypeName::new(class.name.to_string(), (class.src_id, class.name.span())),
        provides_by_module: vec![],
        into_from_rust: Some(ForeignConversationRule {
            rust_ty: self_type,
            intermediate: Some(ForeignConversationIntermediate {
                input_to_output: false,
                intermediate_ty: storage_ptr_type,
                conv_code: Rc::new(TypeConvCode::new(
                    format!(
                        "new {class_name}({from})",
                        class_name = class.name,
                        from = FROM_VAR_TEMPLATE,
                    ),
                    invalid_src_id_span(),
                )),
                finalizer_code: None,
            }),
        }),
        from_into_rust: Some(ForeignConversationRule {
            rust_ty: self_type,
            intermediate: Some(ForeignConversationIntermediate {
                input_to_output: false,
                intermediate_ty: storage_ptr_type,
                conv_code: Rc::new(TypeConvCode::new(
                    format!("{from}.nativePtr", from = FROM_VAR_TEMPLATE),
                    invalid_src_id_span(),
                )),
                finalizer_code: None,
            }),
        }),
        name_prefix: None,
    };
    conv_map.alloc_foreign_type(class_ftype)?;

    // let class_ftype_ref_in = ForeignTypeS {
    //     name: TypeName::new(
    //         format!("/* const ref */{}", class.name),
    //         (class.src_id, class.name.span()),
    //     ),
    //     provides_by_module: vec![],
    //     from_into_rust: Some(ForeignConversationRule {
    //         rust_ty: this_type_ref,
    //         intermediate: Some(ForeignConversationIntermediate {
    //             input_to_output: false,
    //             intermediate_ty: storage_ptr_type,
    //             conv_code: Rc::new(TypeConvCode::new(
    //                 format!("{from}.nativePtr", from = FROM_VAR_TEMPLATE),
    //                 invalid_src_id_span(),
    //             )),
    //             finalizer_code: None,
    //         }),
    //     }),
    //     into_from_rust: None,
    //     name_prefix: None,
    // };
    // conv_map.alloc_foreign_type(class_ftype_ref_in)?;

    // let class_ftype_ref_out = ForeignTypeS {
    //     name: TypeName::new(
    //         format!("/* mut ref */{}", class.name),
    //         (class.src_id, class.name.span()),
    //     ),
    //     provides_by_module: vec![],
    //     into_from_rust: Some(ForeignConversationRule {
    //         rust_ty: this_type_ref,
    //         intermediate: Some(ForeignConversationIntermediate {
    //             input_to_output: false,
    //             intermediate_ty: const_void_ptr_rust_ty,
    //             conv_code: Rc::new(TypeConvCode::new(
    //                 format!(
    //                     "new {class}({var})",
    //                     class = class.name,
    //                     var = FROM_VAR_TEMPLATE
    //                 ),
    //                 invalid_src_id_span(),
    //             )),
    //         }),
    //     }),
    //     from_into_rust: None,
    //     name_prefix: None,
    // };
    // conv_map.alloc_foreign_type(class_ftype_ref_out)?;

    // let class_ftype_mut_ref_in = ForeignTypeS {
    //     name: TypeName::new(
    //         format!("/* mut ref */{}", class.name),
    //         (class.src_id, class.name.span()),
    //     ),
    //     provides_by_module: vec![],
    //     from_into_rust: Some(ForeignConversationRule {
    //         rust_ty: this_type_mut_ref,
    //         intermediate: Some(ForeignConversationIntermediate {
    //             input_to_output: false,
    //             intermediate_ty: storage_ptr_type,
    //             conv_code: Rc::new(TypeConvCode::new(
    //                 format!("{from}.nativePtr", from = FROM_VAR_TEMPLATE),
    //                 invalid_src_id_span(),
    //             )),
    //             finalizer_code: None,
    //         }),
    //     }),
    //     into_from_rust: None,
    //     name_prefix: None,
    // };
    // conv_map.alloc_foreign_type(class_ftype_mut_ref_in)?;

    // if self_type != this_type {
    //     let self_type = conv_map[self_type].clone();
    //     {
    //         let span = self_type.ty.span();
    //         let self_type_ty = self_type.to_type_without_lifetimes();
    //         let gen_ty = parse_type_spanned_checked!(span, &mut #self_type_ty);
    //         let self_type_mut_ref = conv_map.find_or_alloc_rust_type(&gen_ty, class.src_id);

    //         let class_ftype_mut_ref_in = ForeignTypeS {
    //             name: TypeName::new(
    //                 format!("/**/{} &", class.name),
    //                 (class.src_id, class.name.span()),
    //             ),
    //             provides_by_module: vec![format!("\"{}\"", cpp_code::cpp_header_name(class)).into()],
    //             from_into_rust: Some(ForeignConversationRule {
    //                 rust_ty: self_type_mut_ref.to_idx(),
    //                 intermediate: Some(ForeignConversationIntermediate {
    //                     input_to_output: false,
    //                     intermediate_ty: void_ptr_rust_ty,
    //                     conv_code: Rc::new(TypeConvCode::new(
    //                         format!(
    //                             "static_cast<{} *>({})",
    //                             cpp_code::c_class_type(class),
    //                             FROM_VAR_TEMPLATE
    //                         ),
    //                         invalid_src_id_span(),
    //                     )),
    //                 }),
    //             }),
    //             into_from_rust: None,
    //             name_prefix: Some("/**/".into()),
    //         };
    //         conv_map.alloc_foreign_type(class_ftype_mut_ref_in)?;
    //     }
    //     {
    //         let self_type_ty = self_type.to_type_without_lifetimes();
    //         let span = self_type.ty.span();
    //         let gen_ty = parse_type_spanned_checked!(span, & #self_type_ty);
    //         let self_type_ref = conv_map.find_or_alloc_rust_type(&gen_ty, class.src_id);

    //         let class_ftype_ref_in = ForeignTypeS {
    //             name: TypeName::new(
    //                 format!("/**/const {} &", class.name),
    //                 (class.src_id, class.name.span()),
    //             ),
    //             provides_by_module: vec![format!("\"{}\"", cpp_code::cpp_header_name(class)).into()],
    //             from_into_rust: Some(ForeignConversationRule {
    //                 rust_ty: self_type_ref.to_idx(),
    //                 intermediate: Some(ForeignConversationIntermediate {
    //                     input_to_output: false,
    //                     intermediate_ty: const_void_ptr_rust_ty,
    //                     conv_code: Rc::new(TypeConvCode::new(
    //                         format!(
    //                             "static_cast<const {} *>({})",
    //                             cpp_code::c_class_type(class),
    //                             FROM_VAR_TEMPLATE
    //                         ),
    //                         invalid_src_id_span(),
    //                     )),
    //                 }),
    //             }),
    //             into_from_rust: None,
    //             name_prefix: Some("/**/".into()),
    //         };
    //         conv_map.alloc_foreign_type(class_ftype_ref_in)?;
    //     }
    // }

    Ok(())
}
