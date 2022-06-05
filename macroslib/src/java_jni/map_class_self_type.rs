use log::debug;
use std::{fmt::Write, rc::Rc};
use syn::spanned::Spanned;

use super::{JavaContext, INTERNAL_PTR_MARKER, JAVA_RUST_SELF_NAME};
use crate::{
    error::{invalid_src_id_span, Result},
    source_registry::SourceId,
    typemap::{
        ast::ForeignTypeName,
        ty::{ForeignConversionIntermediate, ForeignConversionRule, ForeignTypeS, RustType},
        utils::{boxed_type, convert_to_heap_pointer, unpack_from_heap_pointer},
        RustTypeIdx, TypeConvCode, FROM_VAR_TEMPLATE, TO_VAR_TEMPLATE,
    },
    types::{ForeignClassInfo, SelfTypeDesc},
    WRITE_TO_MEM_FAILED_MSG,
};

pub(in crate::java_jni) fn register_typemap_for_self_type(
    ctx: &mut JavaContext,
    class: &ForeignClassInfo,
    this_type: RustType,
    self_desc: &SelfTypeDesc,
) -> Result<()> {
    debug!(
        "register_typemap_for_self_type: add implements SwigForeignClass for {}",
        this_type
    );
    let constructor_ret_type = &self_desc.constructor_ret_type;

    ctx.conv_map.find_or_alloc_rust_type_with_suffix(
        &parse_type! { jobject },
        &this_type.normalized_name,
        SourceId::none(),
    );

    ctx.conv_map
        .find_or_alloc_rust_type(constructor_ret_type, class.src_id);

    let this_type_inner = boxed_type(ctx.conv_map, &this_type);

    let span = this_type_inner.ty.span();
    let this_type_inner_ty = this_type_inner.to_type_without_lifetimes();
    let gen_ty = parse_type_spanned_checked!(span, & #this_type_inner_ty);
    let this_type_ref = ctx.conv_map.find_or_alloc_rust_type(&gen_ty, class.src_id);

    let gen_ty = parse_type_spanned_checked!(span, &mut #this_type_inner_ty);
    let this_type_mut_ref = ctx.conv_map.find_or_alloc_rust_type(&gen_ty, class.src_id);

    register_rust_ty_conversion_rules(ctx, &this_type)?;
    let self_type = ctx
        .conv_map
        .find_or_alloc_rust_type(&self_desc.self_type, class.src_id);
    register_main_foreign_types(
        ctx,
        class,
        this_type.to_idx(),
        self_type.to_idx(),
        this_type_ref.to_idx(),
        this_type_mut_ref.to_idx(),
    )?;

    Ok(())
}

fn register_rust_ty_conversion_rules(ctx: &mut JavaContext, this_type: &RustType) -> Result<()> {
    let (this_type_for_method, _code_box_this) =
        convert_to_heap_pointer(ctx.conv_map, this_type, "this");

    let jlong_ti: RustType = ctx
        .conv_map
        .find_or_alloc_rust_type_no_src_id(&parse_type! { jlong });
    let span = this_type_for_method.ty.span();
    let this_type_for_method_ty = this_type_for_method.to_type_without_lifetimes();
    let gen_ty = parse_type_spanned_checked!(span, & #this_type_for_method_ty);
    let this_type_ref = ctx
        .conv_map
        .find_or_alloc_rust_type(&gen_ty, this_type_for_method.src_id);
    //handle foreigner_class as input arg
    ctx.conv_map.add_conversion_rule(
        jlong_ti.to_idx(),
        this_type_ref.to_idx(),
        TypeConvCode::new2(
            format!(
                r#"
        let {to_var}: &{this_type} = unsafe {{
            jlong_to_pointer::<{this_type}>({from_var}).as_mut().unwrap()
        }};
    "#,
                to_var = TO_VAR_TEMPLATE,
                from_var = FROM_VAR_TEMPLATE,
                this_type = this_type_for_method,
            ),
            invalid_src_id_span(),
        )
        .into(),
    );

    let gen_ty = parse_type_spanned_checked!(span, &mut #this_type_for_method_ty);
    let this_type_mut_ref = ctx
        .conv_map
        .find_or_alloc_rust_type(&gen_ty, this_type_for_method.src_id);
    //handle foreigner_class as input arg
    ctx.conv_map.add_conversion_rule(
        jlong_ti.to_idx(),
        this_type_mut_ref.to_idx(),
        TypeConvCode::new2(
            format!(
                r#"
        let {to_var}: &mut {this_type} = unsafe {{
            jlong_to_pointer::<{this_type}>({from_var}).as_mut().unwrap()
        }};
    "#,
                to_var = TO_VAR_TEMPLATE,
                from_var = FROM_VAR_TEMPLATE,
                this_type = this_type_for_method,
            ),
            invalid_src_id_span(),
        )
        .into(),
    );

    let jobject_ty = ctx
        .conv_map
        .find_or_alloc_rust_type_no_src_id(&parse_type! { jobject });
    ctx.conv_map.add_conversion_rule(
        this_type.to_idx(),
        jobject_ty.to_idx(),
        TypeConvCode::new2(
            format!(
                r#"
        let {to_var}: jobject = object_to_jobject(env, {from_var});
"#,
                to_var = TO_VAR_TEMPLATE,
                from_var = FROM_VAR_TEMPLATE,
            ),
            invalid_src_id_span(),
        )
        .into(),
    );

    Ok(())
}

fn register_main_foreign_types(
    ctx: &mut JavaContext,
    class: &ForeignClassInfo,
    this_type: RustTypeIdx,
    self_type: RustTypeIdx,
    this_type_ref: RustTypeIdx,
    this_type_mut_ref: RustTypeIdx,
) -> Result<()> {
    debug!(
        "register_main_foreign_types: ftype for this_type {}",
        ctx.conv_map[this_type]
    );
    let span = ctx.conv_map[this_type].ty.span();
    let jlong_ty = parse_type_spanned_checked!(span, jlong);
    let out_val_prefix = format!("{}OutVal", class.name);
    let jlong_out_val_rty =
        ctx.conv_map
            .find_or_alloc_rust_type_with_suffix(&jlong_ty, &out_val_prefix, class.src_id);
    {
        ctx.conv_map.add_conversion_rule(
            this_type,
            jlong_out_val_rty.to_idx(),
            TypeConvCode::new2(
                format!(
                    r#"
    let {to_var}: jlong = <{this_type}>::box_object({from_var});
"#,
                    this_type = ctx.conv_map[this_type],
                    to_var = TO_VAR_TEMPLATE,
                    from_var = FROM_VAR_TEMPLATE,
                ),
                invalid_src_id_span(),
            )
            .into(),
        );
        let name_prefix = format!("/*{}*/", out_val_prefix);
        ctx.conv_map.alloc_foreign_type(ForeignTypeS {
            name: ForeignTypeName::new_with_unique_prefix(
                format!("{}long", name_prefix),
                &name_prefix,
                (class.src_id, class.name.span()),
            ),
            provided_by_module: vec![],
            from_into_rust: None,
            into_from_rust: Some(ForeignConversionRule {
                rust_ty: jlong_out_val_rty.to_idx(),
                intermediate: None,
            }),
        })?;
    }
    let in_val_prefix = format!("{}InVal", class.name);
    let jlong_in_val_rty =
        ctx.conv_map
            .find_or_alloc_rust_type_with_suffix(&jlong_ty, &in_val_prefix, class.src_id);
    {
        let this_type2 = ctx.conv_map[this_type].clone();
        let (this_type_for_method, _code_box_this) =
            convert_to_heap_pointer(ctx.conv_map, &this_type2, "this");

        if class.smart_ptr_copy_derived() {
            let unpack_code = unpack_from_heap_pointer(&this_type2, TO_VAR_TEMPLATE, true);
            ctx.conv_map.add_conversion_rule(
                jlong_in_val_rty.to_idx(),
                this_type,
                TypeConvCode::new2(
                    format!(
                        r#"
        let {to_var}: *mut {ptr_this_type} = unsafe {{
            jlong_to_pointer::<{ptr_this_type}>({from_var}).as_mut().unwrap()
        }};
    {unpack_code}
        let tmp: {this_type} = {to_var};
        let {to_var}: {this_type} = tmp.clone();
        ::std::mem::forget(tmp);
    "#,
                        to_var = TO_VAR_TEMPLATE,
                        from_var = FROM_VAR_TEMPLATE,
                        ptr_this_type = this_type_for_method,
                        this_type = this_type2,
                        unpack_code = unpack_code,
                    ),
                    invalid_src_id_span(),
                )
                .into(),
            );
        } else if class.copy_derived() {
            ctx.conv_map.add_conversion_rule(
                jlong_in_val_rty.to_idx(),
                this_type,
                TypeConvCode::new2(
                    format!(
                        r#"
        let {to_var}: &{this_type} = unsafe {{
            jlong_to_pointer::<{this_type}>({from_var}).as_mut().unwrap()
        }};
        let {to_var}: {this_type} = {to_var}.clone();
    "#,
                        to_var = TO_VAR_TEMPLATE,
                        from_var = FROM_VAR_TEMPLATE,
                        this_type = this_type_for_method,
                    ),
                    invalid_src_id_span(),
                )
                .into(),
            );
        } else {
            let unpack_code = unpack_from_heap_pointer(&this_type2, TO_VAR_TEMPLATE, true);
            ctx.conv_map.add_conversion_rule(
                jlong_in_val_rty.to_idx(),
                this_type,
                TypeConvCode::new2(
                    format!(
                        r#"
        let {to_var}: *mut {this_type} = unsafe {{
            jlong_to_pointer::<{this_type}>({from_var}).as_mut().unwrap()
        }};
    {unpack_code}
    "#,
                        to_var = TO_VAR_TEMPLATE,
                        from_var = FROM_VAR_TEMPLATE,
                        this_type = this_type_for_method,
                        unpack_code = unpack_code,
                    ),
                    invalid_src_id_span(),
                )
                .into(),
            );
        }

        let name_prefix = format!("/*{}*/", in_val_prefix);
        ctx.conv_map.alloc_foreign_type(ForeignTypeS {
            name: ForeignTypeName::new_with_unique_prefix(
                format!("{}long", name_prefix),
                &name_prefix,
                (class.src_id, class.name.span()),
            ),
            provided_by_module: vec![],
            into_from_rust: None,
            from_into_rust: Some(ForeignConversionRule {
                rust_ty: jlong_in_val_rty.to_idx(),
                intermediate: None,
            }),
        })?;
    }

    let mut java_code_in_val_to_long = format!(
        r#"
        long {to_var} = {from_var}.{class_raw_ptr};
"#,
        to_var = TO_VAR_TEMPLATE,
        from_var = FROM_VAR_TEMPLATE,
        class_raw_ptr = JAVA_RUST_SELF_NAME,
    );
    if !class.copy_derived() && !class.smart_ptr_copy_derived() {
        writeln!(
            &mut java_code_in_val_to_long,
            "        {from_var}.{class_raw_ptr} = 0;",
            from_var = FROM_VAR_TEMPLATE,
            class_raw_ptr = JAVA_RUST_SELF_NAME,
        )
        .expect(WRITE_TO_MEM_FAILED_MSG);
    }

    let null_annot = if ctx.cfg.null_annotation_package.is_some() {
        "@NonNull "
    } else {
        ""
    };

    let class_ftype = ForeignTypeS {
        name: ForeignTypeName::new(format!("{}{}", null_annot, class.name), (class.src_id, class.name.span())),
        provided_by_module: vec![],
        into_from_rust: Some(ForeignConversionRule {
            rust_ty: this_type,
            intermediate: Some(ForeignConversionIntermediate {
                input_to_output: false,
                intermediate_ty: jlong_out_val_rty.to_idx(),
                conv_code: Rc::new(TypeConvCode::new(
                    format!(
                        "        {class_name} {out} = new {class_name}({internal_ptr_marker}.RAW_PTR, {var});",
                        class_name = class.name,
                        var = FROM_VAR_TEMPLATE,
                        out = TO_VAR_TEMPLATE,
                        internal_ptr_marker = INTERNAL_PTR_MARKER,
                    ),
                    invalid_src_id_span(),
                )),
            }),
        }),
        from_into_rust: Some(ForeignConversionRule {
            rust_ty: this_type,
            intermediate: Some(ForeignConversionIntermediate {
                input_to_output: false,
                intermediate_ty: jlong_in_val_rty.to_idx(),
                conv_code: Rc::new(TypeConvCode::new(
                    java_code_in_val_to_long,
                    invalid_src_id_span(),
                )),
            }),
        }),
    };
    ctx.conv_map.alloc_foreign_type(class_ftype)?;

    let jlong_ty = ctx.conv_map.ty_to_rust_type(&parse_type! { jlong });
    debug!(
        "register_main_foreign_types: ftype for this_type_ref {}",
        ctx.conv_map[this_type_ref]
    );
    let class_ftype_ref_in = ForeignTypeS {
        name: ForeignTypeName::new_with_unique_prefix(
            format!("/*ref*/{}{}", null_annot, class.name),
            "/*ref*/",
            (class.src_id, class.name.span()),
        ),
        provided_by_module: vec![],
        from_into_rust: Some(ForeignConversionRule {
            rust_ty: this_type_ref,
            intermediate: Some(ForeignConversionIntermediate {
                input_to_output: false,
                intermediate_ty: jlong_ty.to_idx(),
                conv_code: Rc::new(TypeConvCode::new(
                    format!(
                        "        long {out} = {from}.{self_raw_ptr};",
                        from = FROM_VAR_TEMPLATE,
                        out = TO_VAR_TEMPLATE,
                        self_raw_ptr = JAVA_RUST_SELF_NAME,
                    ),
                    invalid_src_id_span(),
                )),
            }),
        }),
        into_from_rust: None,
    };
    ctx.conv_map.alloc_foreign_type(class_ftype_ref_in)?;

    debug!(
        "register_main_foreign_types: ftype for this_type_mut_ref {}",
        ctx.conv_map[this_type_mut_ref]
    );
    let class_ftype_mut_ref_in = ForeignTypeS {
        name: ForeignTypeName::new_with_unique_prefix(
            format!("/*mut ref*/{}{}", null_annot, class.name),
            "/*mut ref*/",
            (class.src_id, class.name.span()),
        ),
        provided_by_module: vec![],
        from_into_rust: Some(ForeignConversionRule {
            rust_ty: this_type_mut_ref,
            intermediate: Some(ForeignConversionIntermediate {
                input_to_output: false,
                intermediate_ty: jlong_ty.to_idx(),
                conv_code: Rc::new(TypeConvCode::new(
                    format!(
                        "        long {out} = {from}.{self_raw_ptr};",
                        from = FROM_VAR_TEMPLATE,
                        out = TO_VAR_TEMPLATE,
                        self_raw_ptr = JAVA_RUST_SELF_NAME,
                    ),
                    invalid_src_id_span(),
                )),
            }),
        }),
        into_from_rust: None,
    };
    ctx.conv_map.alloc_foreign_type(class_ftype_mut_ref_in)?;

    if self_type != this_type {
        debug!(
            "register_main_foreign_types: self_type {} != this_type {}",
            ctx.conv_map[self_type], ctx.conv_map[this_type]
        );
        let self_type = ctx.conv_map[self_type].clone();
        let span = self_type.ty.span();
        let self_type_ty = self_type.to_type_without_lifetimes();
        {
            let gen_ty = parse_type_spanned_checked!(span, &mut #self_type_ty);
            let self_type_mut_ref = ctx.conv_map.find_or_alloc_rust_type(&gen_ty, class.src_id);

            debug!(
                "register_main_foreign_types: ftype for self_type_mut_ref {}",
                self_type_mut_ref
            );
            let class_ftype_mut_ref_in = ForeignTypeS {
                name: ForeignTypeName::new_with_unique_prefix(
                    format!("/*ref 2*/{}{}", null_annot, class.name),
                    "/*ref 2*/",
                    (class.src_id, class.name.span()),
                ),
                provided_by_module: vec![],
                from_into_rust: Some(ForeignConversionRule {
                    rust_ty: self_type_mut_ref.to_idx(),
                    intermediate: Some(ForeignConversionIntermediate {
                        input_to_output: false,
                        intermediate_ty: jlong_ty.to_idx(),
                        conv_code: Rc::new(TypeConvCode::new(
                            format!(
                                "        long {out} = {from}.{self_raw_ptr};",
                                from = FROM_VAR_TEMPLATE,
                                out = TO_VAR_TEMPLATE,
                                self_raw_ptr = JAVA_RUST_SELF_NAME,
                            ),
                            invalid_src_id_span(),
                        )),
                    }),
                }),
                into_from_rust: None,
            };
            ctx.conv_map.alloc_foreign_type(class_ftype_mut_ref_in)?;
        }
        {
            let gen_ty = parse_type_spanned_checked!(span, & #self_type_ty);
            let self_type_ref = ctx.conv_map.find_or_alloc_rust_type(&gen_ty, class.src_id);

            let class_ftype_ref_in = ForeignTypeS {
                name: ForeignTypeName::new_with_unique_prefix(
                    format!("/*mut ref 2*/{}{}", null_annot, class.name),
                    "/*mut ref 2*/",
                    (class.src_id, class.name.span()),
                ),
                provided_by_module: vec![],
                from_into_rust: Some(ForeignConversionRule {
                    rust_ty: self_type_ref.to_idx(),
                    intermediate: Some(ForeignConversionIntermediate {
                        input_to_output: false,
                        intermediate_ty: jlong_ty.to_idx(),
                        conv_code: Rc::new(TypeConvCode::new(
                            format!(
                                "        long {out} = {from}.{self_raw_ptr};",
                                from = FROM_VAR_TEMPLATE,
                                out = TO_VAR_TEMPLATE,
                                self_raw_ptr = JAVA_RUST_SELF_NAME,
                            ),
                            invalid_src_id_span(),
                        )),
                    }),
                }),
                into_from_rust: None,
            };
            ctx.conv_map.alloc_foreign_type(class_ftype_ref_in)?;
        }
    }

    Ok(())
}
