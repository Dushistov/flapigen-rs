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
    types::{ForeignerClassInfo, SelfTypeDesc, MethodVariant},
    TypeMap, SMART_PTR_COPY_TRAIT,
};

pub(crate) fn register_class(
    conv_map: &mut TypeMap,
    class: &ForeignerClassInfo,
) -> Result<()> {
    class
            .validate_class()
            .map_err(|err| DiagnosticError::new(class.src_id, class.span(), err))?;
        if let Some(self_desc) = class.self_desc.as_ref() {
            let constructor_ret_type = &self_desc.constructor_ret_type;
            let this_type_for_method = constructor_ret_type;
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

            let this_type = conv_map.find_or_alloc_rust_type_that_implements(
                this_type_for_method,
                &traits,
                class.src_id,
            );

            if class.smart_ptr_copy_derived {
                if class.copy_derived {
                    println!(
                        "warning=class {} marked as Copy and {}, ignore Copy",
                        class.name, SMART_PTR_COPY_TRAIT
                    );
                }
                if ast::check_if_smart_pointer_return_inner_type(&this_type, "Rc").is_none()
                    && ast::check_if_smart_pointer_return_inner_type(&this_type, "Arc").is_none()
                {
                    return Err(DiagnosticError::new(
                        class.src_id,
                        this_type.ty.span(),
                        format!(
                            "class {} marked as {}, but type '{}' is not Arc<> or Rc<>",
                            class.name, SMART_PTR_COPY_TRAIT, this_type
                        ),
                    ));
                }

                let has_clone = class.methods.iter().any(|x| match x.variant {
                    MethodVariant::Method(_) | MethodVariant::StaticMethod => {
                        x.rust_id.is_ident("clone")
                    }
                    MethodVariant::Constructor => false,
                });
                if has_clone {
                    return Err(DiagnosticError::new(
                    class.src_id,
                    this_type.ty.span(),
                    format!(
                        "class {} marked as {}, but has clone method. Error: can not generate clone method.",
                        class.name, SMART_PTR_COPY_TRAIT,
                    ),
                ));
                }
            }

            register_typemap_for_self_type(conv_map, class, this_type, self_desc)?;
        }
        conv_map.find_or_alloc_rust_type(&class.self_type_as_ty(), class.src_id);
        Ok(())
}

fn register_typemap_for_self_type(
    conv_map: &mut TypeMap,
    class: &ForeignerClassInfo,
    this_type: RustType,
    self_desc: &SelfTypeDesc,
) -> Result<()> {
    let span = this_type.ty.span();

    let void_ptr_ty = parse_type_spanned_checked!(span, *mut ::std::os::raw::c_void);
    let void_ptr_rust_ty = conv_map.find_or_alloc_rust_type_with_suffix(
        &void_ptr_ty,
        &this_type.normalized_name,
        class.src_id,
    );

    let const_void_ptr_ty = parse_type_spanned_checked!(span, *const ::std::os::raw::c_void);
    let const_void_ptr_rust_ty = conv_map.find_or_alloc_rust_type_with_suffix(
        &const_void_ptr_ty,
        &this_type.normalized_name,
        class.src_id,
    );

    let this_type_inner = boxed_type(conv_map, &this_type);

    let span = this_type_inner.ty.span();
    let ty = this_type_inner.to_type_without_lifetimes();
    let gen_ty = parse_type_spanned_checked!(span, & #ty);
    let this_type_ref = conv_map.find_or_alloc_rust_type(&gen_ty, class.src_id);

    let gen_ty = parse_type_spanned_checked!(span, &mut #ty);
    let this_type_mut_ref = conv_map.find_or_alloc_rust_type(&gen_ty, class.src_id);

    register_intermediate_pointer_types(
        conv_map,
        class,
        void_ptr_rust_ty.to_idx(),
        const_void_ptr_rust_ty.to_idx(),
    )?;
    register_rust_ty_conversation_rules(
        conv_map,
        class,
        this_type.clone(),
        this_type_inner.to_idx(),
        void_ptr_rust_ty.to_idx(),
        const_void_ptr_rust_ty.to_idx(),
        this_type_ref.to_idx(),
        this_type_mut_ref.to_idx(),
    )?;

    let self_type = conv_map.find_or_alloc_rust_type(&self_desc.self_type, class.src_id);

    register_main_foreign_types(
        conv_map,
        class,
        this_type.to_idx(),
        self_type.to_idx(),
        void_ptr_rust_ty.to_idx(),
        const_void_ptr_rust_ty.to_idx(),
        this_type_ref.to_idx(),
        this_type_mut_ref.to_idx(),
    )?;
    Ok(())
}

fn register_intermediate_pointer_types(
    conv_map: &mut TypeMap,
    class: &ForeignerClassInfo,
    void_ptr_rust_ty: RustTypeIdx,
    const_void_ptr_rust_ty: RustTypeIdx,
) -> Result<()> {
    let c_ftype = ForeignTypeS {
        name: TypeName::new(
            format!("/* {} */ IntPtr", class.name),
            (class.src_id, class.name.span()),
        ),
        provides_by_module: vec![],
        into_from_rust: Some(ForeignConversationRule {
            rust_ty: void_ptr_rust_ty,
            intermediate: None,
        }),
        from_into_rust: Some(ForeignConversationRule {
            rust_ty: void_ptr_rust_ty,
            intermediate: None,
        }),
        name_prefix: None,
    };
    conv_map.alloc_foreign_type(c_ftype)?;

    let c_const_ftype = ForeignTypeS {
        name: TypeName::new(
            format!("/* &{} */ IntPtr", class.name),
            (class.src_id, class.name.span()),
        ),
        provides_by_module: vec![],
        into_from_rust: Some(ForeignConversationRule {
            rust_ty: const_void_ptr_rust_ty,
            intermediate: None,
        }),
        from_into_rust: Some(ForeignConversationRule {
            rust_ty: const_void_ptr_rust_ty,
            intermediate: None,
        }),
        name_prefix: None,
    };
    conv_map.alloc_foreign_type(c_const_ftype)?;
    Ok(())
}

fn register_rust_ty_conversation_rules(
    conv_map: &mut TypeMap,
    class: &ForeignerClassInfo,
    this_type: RustType,
    this_type_inner: RustTypeIdx,
    void_ptr_rust_ty: RustTypeIdx,
    const_void_ptr_rust_ty: RustTypeIdx,
    this_type_ref: RustTypeIdx,
    this_type_mut_ref: RustTypeIdx,
) -> Result<()> {
    // *const c_void -> &"class"
    //let 
    conv_map.add_conversation_rule(
        const_void_ptr_rust_ty,
        this_type_ref,
        TypeConvCode::new2(
            format!(
                r#"
    assert!(!{from_var}.is_null());
    let {to_var}: {this_type_ref} = unsafe {{ &*({from_var} as *const {this_type_inner}) }};
"#,
                to_var = TO_VAR_TEMPLATE,
                from_var = FROM_VAR_TEMPLATE,
                this_type_ref = conv_map[this_type_ref],
                this_type_inner = conv_map[this_type_inner],
            ),
            invalid_src_id_span(),
        )
        .into(),
    );

    // *mut c_void -> &mut "class"
    conv_map.add_conversation_rule(
        void_ptr_rust_ty,
        this_type_mut_ref,
        TypeConvCode::new2(
            format!(
                r#"
    assert!(!{from_var}.is_null());
    let {to_var}: {this_type_mut_ref} = unsafe {{ &mut *({from_var} as *mut {this_type_inner}) }};
"#,
                to_var = TO_VAR_TEMPLATE,
                from_var = FROM_VAR_TEMPLATE,
                this_type_mut_ref = conv_map[this_type_mut_ref],
                this_type_inner = conv_map[this_type_inner],
            ),
            invalid_src_id_span(),
        )
        .into(),
    );

    // *const c_void -> "class", two steps to make it more expensive
    // for type graph path search
    let ty = conv_map[this_type_inner].to_type_without_lifetimes();
    let span = ty.span();
    let gen_ty = parse_type_spanned_checked!(span, *mut #ty);
    let this_type_mut_ptr = conv_map.find_or_alloc_rust_type(&gen_ty, class.src_id);

    conv_map.add_conversation_rule(
        void_ptr_rust_ty,
        this_type_mut_ptr.to_idx(),
        TypeConvCode::new2(
            format!(
                r#"
            assert!(!{from_var}.is_null());
            let {to_var}: {this_type_mut_ptr} = {from_var} as {this_type_mut_ptr};
        "#,
                to_var = TO_VAR_TEMPLATE,
                from_var = FROM_VAR_TEMPLATE,
                this_type_mut_ptr = this_type_mut_ptr,
            ),
            invalid_src_id_span(),
        )
        .into(),
    );

    let unpack_code = unpack_from_heap_pointer(&this_type, TO_VAR_TEMPLATE, true);
    conv_map.add_conversation_rule(
        this_type_mut_ptr.to_idx(),
        this_type.to_idx(),
        TypeConvCode::new(format!("\n{}\n", unpack_code,), invalid_src_id_span()).into(),
    );

    //"class" -> *mut void
    conv_map.add_conversation_rule(
        this_type.to_idx(),
        void_ptr_rust_ty,
        TypeConvCode::new(
            format!(
                "let {to_var}: {ptr_type} = <{this_type}>::box_object({from_var});",
                to_var = TO_VAR_TEMPLATE,
                ptr_type = conv_map[void_ptr_rust_ty].typename(),
                this_type = this_type,
                from_var = FROM_VAR_TEMPLATE
            ),
            invalid_src_id_span(),
        )
        .into(),
    );

    //&"class" -> *const void
    conv_map.add_conversation_rule(
        this_type_ref,
        const_void_ptr_rust_ty,
        TypeConvCode::new(
            format!(
                "let {to_var}: {ptr_type} = ({from_var} as *const {this_type}) as {ptr_type};",
                to_var = TO_VAR_TEMPLATE,
                ptr_type = conv_map[const_void_ptr_rust_ty].typename(),
                this_type = conv_map[this_type_inner],
                from_var = FROM_VAR_TEMPLATE,
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
    this_type: RustTypeIdx,
    self_type: RustTypeIdx,
    void_ptr_rust_ty: RustTypeIdx,
    const_void_ptr_rust_ty: RustTypeIdx,
    this_type_ref: RustTypeIdx,
    this_type_mut_ref: RustTypeIdx,
) -> Result<()> {
    debug!(
        "register_main_foreign_types: this {}, self {}",
        conv_map[this_type], conv_map[self_type]
    );
    let class_ftype = ForeignTypeS {
        name: TypeName::new(class.name.to_string(), (class.src_id, class.name.span())),
        provides_by_module: vec![],
        into_from_rust: Some(ForeignConversationRule {
            rust_ty: this_type,
            intermediate: Some(ForeignConversationIntermediate {
                input_to_output: false,
                intermediate_ty: void_ptr_rust_ty,
                conv_code: Rc::new(TypeConvCode::new(
                    format!(
                        "new {class_name}({var})",
                        class_name = class.name,
                        var = FROM_VAR_TEMPLATE
                    ),
                    invalid_src_id_span(),
                )),
            }),
        }),
        from_into_rust: Some(ForeignConversationRule {
            rust_ty: this_type,
            intermediate: Some(ForeignConversationIntermediate {
                input_to_output: false,
                intermediate_ty: void_ptr_rust_ty,
                conv_code: Rc::new(TypeConvCode::new(
                    format!("{}.nativePtr", FROM_VAR_TEMPLATE),
                    invalid_src_id_span(),
                )),
            }),
        }),
        name_prefix: None,
    };
    conv_map.alloc_foreign_type(class_ftype)?;

    let class_ftype_ref_in = ForeignTypeS {
        name: TypeName::new(
            format!("/* const ref */{}", class.name),
            (class.src_id, class.name.span()),
        ),
        provides_by_module: vec![],
        from_into_rust: Some(ForeignConversationRule {
            rust_ty: this_type_ref,
            intermediate: Some(ForeignConversationIntermediate {
                input_to_output: false,
                intermediate_ty: const_void_ptr_rust_ty,
                conv_code: Rc::new(TypeConvCode::new(
                    format!(
                        "{}.nativePtr",
                        FROM_VAR_TEMPLATE
                    ),
                    invalid_src_id_span(),
                )),
            }),
        }),
        into_from_rust: None,
        name_prefix: None,
    };
    conv_map.alloc_foreign_type(class_ftype_ref_in)?;

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

    let class_ftype_mut_ref_in = ForeignTypeS {
        name: TypeName::new(
            format!("/* mut ref */{}", class.name),
            (class.src_id, class.name.span()),
        ),
        provides_by_module: vec![],
        from_into_rust: Some(ForeignConversationRule {
            rust_ty: this_type_mut_ref,
            intermediate: Some(ForeignConversationIntermediate {
                input_to_output: false,
                intermediate_ty: void_ptr_rust_ty,
                conv_code: Rc::new(TypeConvCode::new(
                    format!(
                        "{}.nativePtr",
                        FROM_VAR_TEMPLATE
                    ),
                    invalid_src_id_span(),
                )),
            }),
        }),
        into_from_rust: None,
        name_prefix: None,
    };
    conv_map.alloc_foreign_type(class_ftype_mut_ref_in)?;

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
