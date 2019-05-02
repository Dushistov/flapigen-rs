use log::trace;
use syn::{parse_quote, spanned::Spanned, Type};

use crate::{
    ast::{if_option_return_some_type, normalize_ty_lifetimes},
    error::{DiagnosticError, Result},
    java_jni::{calc_this_type_for_method, JavaForeignTypeInfo, NullAnnotation},
    typemap::{ty::RustType, ForeignTypeInfo, FROM_VAR_TEMPLATE, TO_VAR_TEMPLATE},
    ForeignEnumInfo, ForeignerClassInfo, TypeMap,
};

pub(in crate::java_jni) fn special_type(
    conv_map: &mut TypeMap,
    arg_ty: &Type,
) -> Result<Option<JavaForeignTypeInfo>> {
    let foreign_class_trait = "SwigForeignClass";

    trace!(
        "Check is arg.ty({:?}) implements foreign_class_trait",
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
                    arg_ty.span(),
                    format!("Can not find foreigner_class for '{:?}'", arg_ty),
                )
            })?;
        let converter = calc_converter_for_foreign_class_arg(foreigner_class, arg_ty);
        return Ok(Some(converter));
    }
    trace!("Check is arg.ty({:?}) implements exported enum", arg_ty);
    if let Some(foreign_enum) = conv_map.is_this_exported_enum(arg_ty) {
        let converter = calc_converter_for_enum(foreign_enum);
        return Ok(Some(converter));
    }

    trace!("Check is arg.ty({:?}) self type of foreign class", arg_ty);
    if let Some(foreign_class) = conv_map.find_foreigner_class_with_such_self_type(arg_ty, true) {
        let jlong_ti: RustType = parse_type! { jlong }.into();
        let converter = JavaForeignTypeInfo {
            base: ForeignTypeInfo {
                name: foreign_class.name.to_string().into(),
                correspoding_rust_type: jlong_ti,
            },
            java_transition_type: Some("long".into()),
            java_converter: format!(
                "        long {to_var} = {from_var}.mNativeObj;",
                to_var = TO_VAR_TEMPLATE,
                from_var = FROM_VAR_TEMPLATE
            ),
            annotation: Some(NullAnnotation::NonNull),
        };
        return Ok(Some(converter));
    }

    if let Some(ty) = if_option_return_some_type(arg_ty) {
        return handle_option_type_in_input(conv_map, &ty);
    }

    trace!("Oridinary type {:?}", arg_ty);
    Ok(None)
}

fn calc_converter_for_foreign_class_arg(
    foreigner_class: &ForeignerClassInfo,
    arg_ty: &Type,
) -> JavaForeignTypeInfo {
    let this_ty = calc_this_type_for_method(foreigner_class).unwrap();
    let this_ty: RustType = this_ty.clone().into();

    let java_converter = if this_ty.normalized_name == normalize_ty_lifetimes(arg_ty) {
        format!(
            r#"
        long {to_var} = {from_var}.mNativeObj;
        {from_var}.mNativeObj = 0;
"#,
            to_var = TO_VAR_TEMPLATE,
            from_var = FROM_VAR_TEMPLATE
        )
    } else if let syn::Type::Reference(syn::TypeReference { ref elem, .. }) = arg_ty {
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
    let jlong_ti: RustType = parse_type! { jlong }.into();
    JavaForeignTypeInfo {
        base: ForeignTypeInfo {
            name: foreigner_class.name.to_string().into(),
            correspoding_rust_type: jlong_ti,
        },
        java_transition_type: Some("long".into()),
        java_converter,
        annotation: Some(NullAnnotation::NonNull),
    }
}

fn calc_converter_for_enum(foreign_enum: &ForeignEnumInfo) -> JavaForeignTypeInfo {
    let jint_ti: RustType = parse_type! { jint }.into();
    let java_converter: String = format!(
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
        java_transition_type: Some("int".into()),
        java_converter,
        annotation: Some(NullAnnotation::NonNull),
    }
}

fn handle_option_type_in_input(
    conv_map: &mut TypeMap,
    opt_inside_ty: &Type,
) -> Result<Option<JavaForeignTypeInfo>> {
    if let Some(fclass) = conv_map.find_foreigner_class_with_such_self_type(opt_inside_ty, false) {
        let jlong_ti: RustType = parse_type! { jlong }.into();
        Ok(Some(JavaForeignTypeInfo {
            base: ForeignTypeInfo {
                name: fclass.name.to_string().into(),
                correspoding_rust_type: jlong_ti,
            },
            java_transition_type: Some("long".into()),
            java_converter: format!(
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
            annotation: Some(NullAnnotation::Nullable),
        }))
    } else {
        Ok(None)
    }
}
