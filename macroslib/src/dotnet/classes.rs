use log::debug;
use std::rc::Rc;
use syn::{Type, spanned::Spanned};

use crate::{
    error::*,
    typemap::{
        ast::{self, TypeName},
        ty::{ForeignConversationIntermediate, ForeignConversationRule, ForeignTypeS, RustType},
        utils::{boxed_type, unpack_from_heap_pointer},
        RustTypeIdx, TypeConvCode, FROM_VAR_TEMPLATE, TO_VAR_TEMPLATE,
    },
    types::{ForeignerClassInfo, MethodVariant, SelfTypeDesc},
    TypeMap, SMART_PTR_COPY_TRAIT, source_registry::SourceId,
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) enum SmartPointerType {
    None,
    Box,
    Rc,
    Arc,
    Mutex,
    ArcMutex,
}

impl SmartPointerType {
    pub(crate) fn new(
        storage_type: &RustType,
        conv_map: &mut TypeMap,
        src_id: SourceId,
    ) -> SmartPointerType {
        if let Some(inner_ty) = ast::check_if_smart_pointer_return_inner_type(storage_type, "Arc") {
            let rust_inner_ty = conv_map.find_or_alloc_rust_type(&inner_ty, src_id);
            if ast::check_if_smart_pointer_return_inner_type(&rust_inner_ty, "Mutex").is_some()
            {
                SmartPointerType::ArcMutex
            } else {
                SmartPointerType::Arc
            }
        } else if ast::check_if_smart_pointer_return_inner_type(storage_type, "Mutex").is_some() {
            SmartPointerType::Mutex
        } else if ast::check_if_smart_pointer_return_inner_type(storage_type, "Box").is_some() {
            SmartPointerType::Box
        } else if ast::check_if_smart_pointer_return_inner_type(storage_type, "Rc").is_some() {
            SmartPointerType::Rc
        } else {
            SmartPointerType::None
        }
    }

    pub(crate) fn intermediate_ptr_ty(&self, storage_ty: &Type) -> Type {
        let span = invalid_src_id_span().1;
        match self {
            Self::None | Self::Box => parse_type_spanned_checked!(span, *mut #storage_ty),
            SmartPointerType::Rc | SmartPointerType::Arc |
            SmartPointerType::Mutex | SmartPointerType::ArcMutex => parse_type_spanned_checked!(span, *const #storage_ty),
        }
    }


    pub(crate) fn pointer_can_be_mutable(&self) -> bool {
        match self {
            SmartPointerType::None | SmartPointerType::Box => true,
            SmartPointerType::Rc | SmartPointerType::Arc | SmartPointerType::ArcMutex | SmartPointerType::Mutex  => false,
        }
    }

    pub(crate) fn self_can_be_mutable(&self) -> bool {
        match self {
            SmartPointerType::None | SmartPointerType::Box | SmartPointerType::ArcMutex | SmartPointerType::Mutex  => true,
            SmartPointerType::Rc | SmartPointerType::Arc => false,
        }
    }

    pub(crate) fn conversion_code_from_intermediate_to_self_ref(&self) -> String {
        let convert_code = match self {
            SmartPointerType::None | SmartPointerType::Box  => "let {to_var} = unsafe { (&*{from_var}) };",
            SmartPointerType::Rc | SmartPointerType::Arc => "let {to_var} = unsafe { (*{from_var}).as_ref() };",
            SmartPointerType::Mutex => "let {to_var} = unsafe { (*{from_var}).lock().unwrap() };",
            SmartPointerType::ArcMutex => "let {to_var} = unsafe { (*{from_var}).lock().unwrap() };",
        };
        "assert!(!{from_var}.is_null());".to_owned() + convert_code
    }

    pub(crate) fn conversion_code_from_intermediate_to_self_mut_ref(&self) -> String {
        let convert_code = match self {
            SmartPointerType::None | SmartPointerType::Box  => "let {to_var} = unsafe { (&mut *{from_var}) };",
            SmartPointerType::Rc | SmartPointerType::Arc => panic!("You can't deref_mut on `Arc` or `Rc` types."),
            SmartPointerType::Mutex => "let {to_var} = unsafe { (*{from_var}).lock().unwrap() };",
            SmartPointerType::ArcMutex => "let {to_var} = unsafe { (*{from_var}).lock().unwrap() };",
        };
        "assert!(!{from_var}.is_null());".to_owned() + convert_code
    }

    // pub fn destructor_code(&self) -> &'static str {
        
    // }
}

struct ClassTypesInfo {
    smart_pointer_type: SmartPointerType,
    self_has_clone: bool,
    self_type: RustType,
    self_type_ref: RustType,
    self_type_mut_ref: RustType,
    storage_type: RustType,
    storage_type_ref: RustType,
    storage_type_mut_ref: RustType,
    intermediate_ptr_type: RustType,
    source_span: SourceIdSpan,
}

impl ClassTypesInfo {
    fn new(conv_map: &mut TypeMap, class: &ForeignerClassInfo, self_type: RustType, self_desc: &SelfTypeDesc, source_span: SourceIdSpan) -> Self {
        let src_id = source_span.0;
        let span = source_span.1;
        let self_ty = &self_type.ty;
        let storage_ty = &self_desc.constructor_ret_type;

        let storage_type = conv_map.find_or_alloc_rust_type(storage_ty, src_id);
        let smart_pointer_type = SmartPointerType::new(&storage_type, conv_map, src_id);

        let intermediate_ptr_ty = smart_pointer_type.intermediate_ptr_ty(storage_ty);
        let intermediate_ptr_type = conv_map.find_or_alloc_rust_type(&intermediate_ptr_ty, src_id);

        let self_ty_ref = parse_type_spanned_checked!(span, & #self_ty);
        let self_type_ref = conv_map.find_or_alloc_rust_type(&self_ty_ref, src_id);
    
        let self_ty_mut_ref = parse_type_spanned_checked!(span, &mut #self_ty);
        let self_type_mut_ref = conv_map.find_or_alloc_rust_type(&self_ty_mut_ref, src_id);

        let storage_ty_ref = parse_type_spanned_checked!(span, & #storage_ty);
        let storage_type_ref = conv_map.find_or_alloc_rust_type(&storage_ty_ref, src_id);
    
        let storage_ty_mut_ref = parse_type_spanned_checked!(span, &mut #storage_ty);
        let storage_type_mut_ref = conv_map.find_or_alloc_rust_type(&storage_ty_mut_ref, src_id);

        Self {
            smart_pointer_type,
            self_has_clone: class.clone_derived,
            self_type,
            self_type_ref,
            self_type_mut_ref,
            storage_type,
            storage_type_ref,
            storage_type_mut_ref,
            intermediate_ptr_type,
            source_span,
        }
    }

    // fn storage_implements_as_mut(&self) -> bool {
    //     unimplemented!()
    // }


    fn storage_has_clone(&self) -> bool {
        match self.smart_pointer_type {
            SmartPointerType::None | SmartPointerType::Box => self.self_has_clone,
            SmartPointerType::Rc | SmartPointerType::Arc | SmartPointerType::ArcMutex => true,
            SmartPointerType::Mutex => false
        }
    }


    // fn conversion_code_from_storage_to_intermediate(&self) -> &'static str {
    //     "let {from_var} = Box::into_raw(Box::new({to_var}));"
    // }
}


fn register_typemap_for_self_type(
    conv_map: &mut TypeMap,
    class: &ForeignerClassInfo,
    self_type: RustType,
    self_desc: &SelfTypeDesc,
) -> Result<()> {
    //let span = self_type.ty.span();
    let types_info = ClassTypesInfo::new(conv_map, class, self_type, self_desc, (class.src_id, class.span()));

    // let self_ty = &self_type.ty;
    // let storage_ty = parse_type_spanned_checked!(span, Box<#self_ty>);
    // let storage_type = conv_map.find_or_alloc_rust_type(&storage_ty, class.src_id);
    // let storage_ptr_ty = parse_type_spanned_checked!(span, *mut #storage_ty);
    // let storage_ptr_type = conv_map.find_or_alloc_rust_type(&storage_ptr_ty, class.src_id);

    // let gen_ty = parse_type_spanned_checked!(span, & #self_ty);
    // let self_type_ref = conv_map.find_or_alloc_rust_type(&gen_ty, class.src_id);

    // let gen_ty = parse_type_spanned_checked!(span, &mut #self_ty);
    // let self_type_mut_ref = conv_map.find_or_alloc_rust_type(&gen_ty, class.src_id);

    register_intermediate_pointer_types(
        conv_map,
        class,
        &types_info,
        // void_ptr_rust_ty.to_idx(),
        // const_void_ptr_rust_ty.to_idx(),
    )?;
    register_rust_ty_conversation_rules(
        conv_map,
        // class,
        &types_info,
    )?;

    // let this_type = conv_map.find_or_alloc_rust_type(&self_desc.self_type, class.src_id);

    register_main_foreign_types(
        conv_map,
        class,
        &types_info,
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
    types_info: &ClassTypesInfo,
) -> Result<()> {
    let c_ftype = ForeignTypeS {
        name: TypeName::new(
            format!("/* {} */ IntPtr", class.name),
            (class.src_id, class.name.span()),
        ),
        provides_by_module: vec![],
        into_from_rust: Some(ForeignConversationRule {
            rust_ty: types_info.intermediate_ptr_type.to_idx(),
            intermediate: None,
        }),
        from_into_rust: Some(ForeignConversationRule {
            rust_ty: types_info.intermediate_ptr_type.to_idx(),
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
    //class: &ForeignerClassInfo,
    types_info: &ClassTypesInfo,
) -> Result<()> {
    // intermediate_ptr_type -> &self_type
    conv_map.add_conversation_rule(
        types_info.intermediate_ptr_type.to_idx(),
        types_info.self_type_ref.to_idx(),
        TypeConvCode::new2(
            types_info.smart_pointer_type.conversion_code_from_intermediate_to_self_ref(),
            invalid_src_id_span(),
        )
        .into(),
    );

    if types_info.smart_pointer_type.self_can_be_mutable() {
        // intermediate_ptr_type -> &mut self_type
        conv_map.add_conversation_rule(
            types_info.intermediate_ptr_type.to_idx(),
            types_info.self_type_mut_ref.to_idx(),
            TypeConvCode::new2(
                types_info.smart_pointer_type.conversion_code_from_intermediate_to_self_mut_ref(),
                invalid_src_id_span(),
            )
            .into(),
        );
    }

    // storage_type -> intermediate_ptr_type
    conv_map.add_conversation_rule(
        types_info.storage_type.to_idx(),
        types_info.intermediate_ptr_type.to_idx(),
        TypeConvCode::new2(
            "let {to_var} = Box::into_raw(Box::new({from_var}));",
            invalid_src_id_span(),
        )
        .into(),
    );

    if types_info.smart_pointer_type != SmartPointerType::None {
        // intermediate_type -> &storage_type
        conv_map.add_conversation_rule(
            types_info.intermediate_ptr_type.to_idx(),
            types_info.storage_type_ref.to_idx(),
            TypeConvCode::new2(
                    r#"
        assert!(!{from_var}.is_null());
        let {to_var} = unsafe { &*{from_var} };
    "#,
                invalid_src_id_span(),
            )
            .into(),
        );
    }

    if types_info.smart_pointer_type.pointer_can_be_mutable() && types_info.smart_pointer_type != SmartPointerType::None {
        // intermediate_type -> &mut storage_type
        conv_map.add_conversation_rule(
            types_info.intermediate_ptr_type.to_idx(),
            types_info.storage_type_mut_ref.to_idx(),
            TypeConvCode::new2(
                    r#"
        assert!(!{from_var}.is_null());
        let {to_var} = unsafe { &mut *{from_var} };
    "#,
                invalid_src_id_span(),
            )
            .into(),
        );
    }

    if types_info.storage_has_clone() {
        // &storage_type -> intermediate_ptr_type
        conv_map.add_conversation_rule(
            types_info.storage_type_ref.to_idx(),
            types_info.intermediate_ptr_type.to_idx(),
            TypeConvCode::new2(
                "let {to_var} = Box::into_raw(Box::new({from_var}.clone()));",
                invalid_src_id_span(),
            )
            .into(),
        );

        // intermediate_ptr_type -> storage_type
        conv_map.add_conversation_rule(
            types_info.intermediate_ptr_type.to_idx(),
            types_info.storage_type.to_idx(),
            TypeConvCode::new2(
                r#"
        assert!(!{from_var}.is_null());
        let {to_var} = unsafe { (*{from_var}).clone() };
    "#,
                invalid_src_id_span(),
            )
            .into(),
        );
    }

    if types_info.self_has_clone && types_info.smart_pointer_type != SmartPointerType::None {
        // intermediate_ptr_type -> self_type
        conv_map.add_conversation_rule(
            types_info.storage_type_ref.to_idx(),
            types_info.storage_type.to_idx(),
            TypeConvCode::new2(
                types_info.smart_pointer_type.conversion_code_from_intermediate_to_self_ref().to_owned() + 
                    "let {to_var} = {to_var}.clone()",
                invalid_src_id_span(),
            )
            .into(),
        );
    }
    Ok(())
}

fn register_main_foreign_types(
    conv_map: &mut TypeMap,
    class: &ForeignerClassInfo,
    types_info: &ClassTypesInfo,
    // void_ptr_rust_ty: RustTypeIdx,
    // const_void_ptr_rust_ty: RustTypeIdx,
    // this_type_ref: RustTypeIdx,
    // this_type_mut_ref: RustTypeIdx,
) -> Result<()> {
    debug!(
        "register_main_foreign_types: self {}, storage {}",
        types_info.self_type, types_info.storage_type,
    );
    let class_ftype = ForeignTypeS {
        name: TypeName::new(class.name.to_string(), (class.src_id, class.name.span())),
        provides_by_module: vec![],
        into_from_rust: Some(ForeignConversationRule {
            rust_ty: types_info.self_type.to_idx(),
            intermediate: Some(ForeignConversationIntermediate {
                input_to_output: false,
                intermediate_ty: types_info.intermediate_ptr_type.to_idx(),
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
            rust_ty: types_info.self_type.to_idx(),
            intermediate: Some(ForeignConversationIntermediate {
                input_to_output: false,
                intermediate_ty: types_info.intermediate_ptr_type.to_idx(),
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
    //         rust_ty: types_info.self_type_ref.to_idx(),
    //         intermediate: Some(ForeignConversationIntermediate {
    //             input_to_output: false,
    //             intermediate_ty: types_info.intermediate_ptr_type.to_idx(),
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

    // let class_ftype_ref_in = ForeignTypeS {
    //     name: TypeName::new(
    //         format!("/* mut ref */{}", class.name),
    //         (class.src_id, class.name.span()),
    //     ),
    //     provides_by_module: vec![],
    //     from_into_rust: Some(ForeignConversationRule {
    //         rust_ty: types_info.self_type_mut_ref.to_idx(),
    //         intermediate: Some(ForeignConversationIntermediate {
    //             input_to_output: false,
    //             intermediate_ty: types_info.intermediate_ptr_type.to_idx(),
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
