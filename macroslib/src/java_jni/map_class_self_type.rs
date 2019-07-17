use log::debug;
use syn::spanned::Spanned;

use crate::{
    error::{invalid_src_id_span, Result},
    source_registry::SourceId,
    typemap::{
        ast::{parse_ty_with_given_span_checked, DisplayToTokens},
        ty::RustType,
        utils::{convert_to_heap_pointer, unpack_from_heap_pointer},
        ForeignTypeInfo, TypeConvCode, TypeMap, FROM_VAR_TEMPLATE, TO_VAR_TEMPLATE,
    },
    types::{ForeignerClassInfo, SelfTypeDesc},
};

pub(in crate::java_jni) fn register_typemap_for_self_type(
    conv_map: &mut TypeMap,
    class: &ForeignerClassInfo,
    this_type: RustType,
    self_desc: &SelfTypeDesc,
) -> Result<()> {
    debug!(
        "register_typemap_for_self_type: add implements SwigForeignClass for {}",
        this_type
    );
    let constructor_ret_type = &self_desc.constructor_ret_type;

    let my_jobj_ti = conv_map.find_or_alloc_rust_type_with_suffix(
        &parse_type! { jobject },
        &this_type.normalized_name,
        SourceId::none(),
    );

    conv_map.cache_class_for_rust_to_foreign_conv(
        &this_type,
        ForeignTypeInfo {
            correspoding_rust_type: my_jobj_ti,
            name: class.name.to_string().into(),
        },
        (class.src_id, class.name.span()),
    )?;

    conv_map.find_or_alloc_rust_type(constructor_ret_type, class.src_id);

    let (this_type_for_method, _code_box_this) =
        convert_to_heap_pointer(conv_map, &this_type, "this");

    let jlong_ti: RustType = conv_map.find_or_alloc_rust_type_no_src_id(&parse_type! { jlong });
    let this_type_for_method_ty = &this_type_for_method.ty;
    let code = format!("& {}", DisplayToTokens(this_type_for_method_ty));
    let gen_ty = parse_ty_with_given_span_checked(&code, this_type_for_method_ty.span());
    let this_type_ref = conv_map.find_or_alloc_rust_type(&gen_ty, this_type_for_method.src_id);
    //handle foreigner_class as input arg
    conv_map.add_conversation_rule(
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
                this_type = this_type_for_method.normalized_name,
            ),
            invalid_src_id_span(),
        )
        .into(),
    );
    let code = format!("&mut {}", DisplayToTokens(this_type_for_method_ty));
    let gen_ty = parse_ty_with_given_span_checked(&code, this_type_for_method_ty.span());
    let this_type_mut_ref = conv_map.find_or_alloc_rust_type(&gen_ty, this_type_for_method.src_id);
    //handle foreigner_class as input arg
    conv_map.add_conversation_rule(
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
                this_type = this_type_for_method.normalized_name,
            ),
            invalid_src_id_span(),
        )
        .into(),
    );

    let unpack_code = unpack_from_heap_pointer(&this_type_for_method, TO_VAR_TEMPLATE, true);
    conv_map.add_conversation_rule(
        jlong_ti.to_idx(),
        this_type.to_idx(),
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
                this_type = this_type_for_method.normalized_name,
                unpack_code = unpack_code,
            ),
            invalid_src_id_span(),
        )
        .into(),
    );
    Ok(())
}
