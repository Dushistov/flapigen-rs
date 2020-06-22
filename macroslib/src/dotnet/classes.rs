use log::debug;
use std::rc::Rc;
use syn::{Type};

use crate::{
    error::*,
    typemap::{
        ast::{self, TypeName},
        ty::{ForeignConversationIntermediate, ForeignConversationRule, ForeignTypeS, RustType},
        TypeConvCode, FROM_VAR_TEMPLATE,
    },
    types::{ForeignerClassInfo, SelfTypeDesc},
    TypeMap, SMART_PTR_COPY_TRAIT, source_registry::SourceId,
};

pub(crate) fn register_class(conv_map: &mut TypeMap, class: &ForeignerClassInfo) -> Result<()> {
    class
        .validate_class()
        .map_err(|err| DiagnosticError::new(class.src_id, class.span(), err))?;
    if let Some(self_desc) = class.self_desc.as_ref() {
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
        parse_type_spanned_checked!(span, *mut #storage_ty)
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
            SmartPointerType::Mutex => "let {to_var}_lock = unsafe { (*{from_var}).lock().unwrap() }; let {to_var} = &*{to_var}_lock;",
            SmartPointerType::ArcMutex => "let {to_var}_lock = unsafe { (*{from_var}).lock().unwrap() }; let {to_var} = &*{to_var}_lock;",
        };
        "assert!(!{from_var}.is_null());".to_owned() + convert_code
    }

    pub(crate) fn conversion_code_from_intermediate_to_self_mut_ref(&self) -> String {
        let convert_code = match self {
            SmartPointerType::None | SmartPointerType::Box  => "let {to_var} = unsafe { (&mut *{from_var}) };",
            SmartPointerType::Rc | SmartPointerType::Arc => panic!("You can't deref_mut on `Arc` or `Rc` types."),
            SmartPointerType::Mutex => "let mut {to_var}_lock = unsafe { (*{from_var}).lock().unwrap() }; let {to_var} = &mut *{to_var}_lock;",
            SmartPointerType::ArcMutex => "let mut {to_var}_lock = unsafe { (*{from_var}).lock().unwrap() }; let {to_var} = & mut *{to_var}_lock;",
        };
        "assert!(!{from_var}.is_null());".to_owned() + convert_code
    }
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
        }
    }

    fn storage_has_clone(&self) -> bool {
        match self.smart_pointer_type {
            SmartPointerType::None | SmartPointerType::Box => self.self_has_clone,
            SmartPointerType::Rc | SmartPointerType::Arc | SmartPointerType::ArcMutex => true,
            SmartPointerType::Mutex => false
        }
    }
}


fn register_typemap_for_self_type(
    conv_map: &mut TypeMap,
    class: &ForeignerClassInfo,
    self_type: RustType,
    self_desc: &SelfTypeDesc,
) -> Result<()> {
    let types_info = ClassTypesInfo::new(conv_map, class, self_type, self_desc, (class.src_id, class.span()));

    register_intermediate_pointer_types(
        conv_map,
        class,
        &types_info,
    )?;
    register_rust_ty_conversation_rules(
        conv_map,
        &types_info,
    )?;

    register_main_foreign_types(
        conv_map,
        class,
        &types_info,
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

    Ok(())
}

fn register_rust_ty_conversation_rules(
    conv_map: &mut TypeMap,
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
) -> Result<()> {
    debug!(
        "register_main_foreign_types: self {}, storage {}",
        types_info.self_type, types_info.storage_type,
    );
    let class_ftype = ForeignTypeS {
        name: TypeName::new(class.name.to_string(), (class.src_id, class.name.span())),
        provides_by_module: vec![],
        into_from_rust: Some(ForeignConversationRule {
            rust_ty: types_info.storage_type.to_idx(),
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
            rust_ty: types_info.storage_type.to_idx(),
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
    Ok(())
}
