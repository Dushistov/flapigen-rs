use log::trace;
use petgraph::Direction;
use syn::Type;

use crate::{
    error::{DiagnosticError, Result, SourceIdSpan},
    java_jni::{calc_this_type_for_method, JavaConverter, JavaForeignTypeInfo, NullAnnotation},
    source_registry::SourceId,
    typemap::{
        ast::{if_option_return_some_type, normalize_ty_lifetimes},
        ty::RustType,
        ForeignTypeInfo, MapToForeignFlag, FROM_VAR_TEMPLATE, TO_VAR_TEMPLATE,
    },
    types::{ForeignEnumInfo, ForeignerClassInfo},
    TypeMap,
};

pub(in crate::java_jni) fn map_type(
    conv_map: &mut TypeMap,
    arg_ty: &RustType,
    direction: Direction,
    arg_ty_span: SourceIdSpan,
) -> Result<JavaForeignTypeInfo> {
    if direction == Direction::Incoming {
        if let Some(fti) = special_type(conv_map, &arg_ty, arg_ty_span)? {
            return Ok(fti);
        }
    }

    let fti = {
        let fti = conv_map
            .map_through_conversation_to_foreign(
                arg_ty.to_idx(),
                direction,
                MapToForeignFlag::FullSearch,
                arg_ty_span,
                calc_this_type_for_method,
            )
            .ok_or_else(|| {
                DiagnosticError::new2(
                    arg_ty_span,
                    format!(
                        "can not find conversation Java type {} \
                         Rust type '{}'",
                        match direction {
                            Direction::Outgoing => "=>",
                            Direction::Incoming => "<=",
                        },
                        arg_ty,
                    ),
                )
            })?;
        let ftype = &conv_map[fti];
        let rtype_idx = match direction {
            petgraph::Direction::Outgoing => {
                ftype
                    .into_from_rust
                    .as_ref()
                    .expect("Internal error: into_from_rust not defined")
                    .rust_ty
            }
            petgraph::Direction::Incoming => {
                ftype
                    .from_into_rust
                    .as_ref()
                    .expect("Internal error: from_into_rust not defined")
                    .rust_ty
            }
        };
        ForeignTypeInfo {
            name: ftype.name.typename.clone(),
            correspoding_rust_type: conv_map[rtype_idx].clone(),
        }
    };
    let mut fti: JavaForeignTypeInfo = fti.into();
    if !is_primitive_type(&fti.base.name) {
        fti.annotation = Some(if if_option_return_some_type(arg_ty).is_none() {
            NullAnnotation::NonNull
        } else {
            NullAnnotation::Nullable
        });
    }
    Ok(fti)
}

pub(in crate::java_jni) fn special_type(
    conv_map: &mut TypeMap,
    arg_ty: &RustType,
    arg_ty_span: SourceIdSpan,
) -> Result<Option<JavaForeignTypeInfo>> {
    let foreign_class_trait = "SwigForeignClass";

    trace!(
        "special_type: check is arg.ty({}) implements foreign_class_trait",
        arg_ty
    );

    if let Some(foreign_class_this_ty) = conv_map.is_ty_implements(arg_ty, foreign_class_trait) {
        let foreigner_class = conv_map
            .find_foreigner_class_with_such_this_type(
                &foreign_class_this_ty.ty,
                calc_this_type_for_method,
            )
            .ok_or_else(|| {
                DiagnosticError::new(
                    arg_ty_span.0,
                    arg_ty_span.1,
                    format!("Can not find foreigner_class for '{}'", arg_ty),
                )
            })?;
        let converter = calc_converter_for_foreign_class_arg(conv_map, foreigner_class, arg_ty);
        return Ok(Some(converter));
    }
    trace!(
        "special_type: check is arg.ty({}) implements exported enum",
        arg_ty
    );
    if let Some(foreign_enum) = conv_map.is_this_exported_enum(arg_ty) {
        let converter = calc_converter_for_enum(conv_map, foreign_enum);
        return Ok(Some(converter));
    }

    trace!(
        "special_type: check is arg.ty({}) self type of foreign class",
        arg_ty
    );
    if let Some(foreign_class) = conv_map.find_foreigner_class_with_such_self_type(arg_ty, true) {
        let jlong_ti = conv_map.ty_to_rust_type(&parse_type! { jlong });
        let converter = JavaForeignTypeInfo {
            base: ForeignTypeInfo {
                name: foreign_class.name.to_string().into(),
                correspoding_rust_type: jlong_ti,
            },
            java_converter: Some(JavaConverter {
                converter: format!(
                    "        long {to_var} = {from_var}.mNativeObj;",
                    to_var = TO_VAR_TEMPLATE,
                    from_var = FROM_VAR_TEMPLATE
                ),
                java_transition_type: "long".into(),
            }),
            annotation: Some(NullAnnotation::NonNull),
        };
        return Ok(Some(converter));
    }

    if let Some(ty) = if_option_return_some_type(arg_ty) {
        return handle_option_type_in_input(conv_map, &ty, arg_ty_span.0);
    }

    trace!("special_type: oridinary type {}", arg_ty);
    Ok(None)
}

fn calc_converter_for_foreign_class_arg(
    conv_map: &TypeMap,
    foreigner_class: &ForeignerClassInfo,
    arg_ty: &RustType,
) -> JavaForeignTypeInfo {
    let this_ty = calc_this_type_for_method(conv_map, foreigner_class).unwrap();
    let this_ty = conv_map.ty_to_rust_type(&this_ty);

    let converter = if this_ty.normalized_name == arg_ty.normalized_name {
        format!(
            r#"
        long {to_var} = {from_var}.mNativeObj;
        {from_var}.mNativeObj = 0;
"#,
            to_var = TO_VAR_TEMPLATE,
            from_var = FROM_VAR_TEMPLATE
        )
    } else if let syn::Type::Reference(syn::TypeReference { ref elem, .. }) = arg_ty.ty {
        assert_eq!(normalize_ty_lifetimes(elem), this_ty.normalized_name);
        format!(
            r#"
        long {to_var} = {from_var}.mNativeObj;
"#,
            to_var = TO_VAR_TEMPLATE,
            from_var = FROM_VAR_TEMPLATE
        )
    } else {
        unreachable!();
    };
    let jlong_ti = conv_map.ty_to_rust_type(&parse_type! { jlong });
    JavaForeignTypeInfo {
        base: ForeignTypeInfo {
            name: foreigner_class.name.to_string().into(),
            correspoding_rust_type: jlong_ti,
        },
        java_converter: Some(JavaConverter {
            java_transition_type: "long".into(),
            converter,
        }),
        annotation: Some(NullAnnotation::NonNull),
    }
}

fn calc_converter_for_enum(
    conv_map: &TypeMap,
    foreign_enum: &ForeignEnumInfo,
) -> JavaForeignTypeInfo {
    let jint_ti = conv_map.ty_to_rust_type(&parse_type! { jint });
    let converter = format!(
        r#"
        int {to_var} = {from_var}.getValue();
"#,
        to_var = TO_VAR_TEMPLATE,
        from_var = FROM_VAR_TEMPLATE
    );
    JavaForeignTypeInfo {
        base: ForeignTypeInfo {
            name: foreign_enum.name.to_string().into(),
            correspoding_rust_type: jint_ti,
        },
        java_converter: Some(JavaConverter {
            java_transition_type: "int".into(),
            converter,
        }),
        annotation: Some(NullAnnotation::NonNull),
    }
}

fn handle_option_type_in_input(
    conv_map: &mut TypeMap,
    opt_inside_ty: &Type,
    arg_src_id: SourceId,
) -> Result<Option<JavaForeignTypeInfo>> {
    let opt_inside_rust_ty = conv_map.find_or_alloc_rust_type(opt_inside_ty, arg_src_id);
    if let Some(fclass) =
        conv_map.find_foreigner_class_with_such_self_type(&opt_inside_rust_ty, false)
    {
        let jlong_ti = conv_map.ty_to_rust_type(&parse_type! { jlong });
        Ok(Some(JavaForeignTypeInfo {
            base: ForeignTypeInfo {
                name: fclass.name.to_string().into(),
                correspoding_rust_type: jlong_ti,
            },
            java_converter: Some(JavaConverter {
                converter: format!(
                    r#"
        long {to_var} = 0;//TODO: use ptr::null() for corresponding constant
        if ({from_var} != null) {{
            {to_var} = {from_var}.mNativeObj;
            {from_var}.mNativeObj = 0;
        }}
"#,
                    to_var = TO_VAR_TEMPLATE,
                    from_var = FROM_VAR_TEMPLATE
                ),
                java_transition_type: "long".into(),
            }),
            annotation: Some(NullAnnotation::Nullable),
        }))
    } else {
        Ok(None)
    }
}

fn is_primitive_type(type_name: &str) -> bool {
    match type_name {
        "void" | "boolean" | "byte" | "short" | "int" | "long" | "float" | "double" => true,
        _ => false,
    }
}
