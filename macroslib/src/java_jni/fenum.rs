use log::trace;
use proc_macro2::{Span, TokenStream};
use quote::quote;
use std::{io::Write, rc::Rc};
use syn::{Ident, Type};

use super::{
    java_class_full_name, java_class_name_to_jni, java_code::doc_comments_to_java_comments,
    map_write_err, JavaContext,
};
use crate::{
    error::{invalid_src_id_span, DiagnosticError, Result},
    file_cache::FileWriteCache,
    typemap::{
        ast::{parse_ty_with_given_span, ForeignTypeName},
        ty::{ForeignConversionIntermediate, ForeignConversionRule, ForeignTypeS},
        RustTypeIdx, TypeConvCode, TypeConvEdge, FROM_VAR_TEMPLATE, TO_VAR_TEMPLATE,
    },
    types::ForeignEnumInfo,
    WRITE_TO_MEM_FAILED_MSG,
};

const C_LIKE_ENUM_TRAIT: &str = "SwigForeignCLikeEnum";

pub(in crate::java_jni) fn generate_enum(
    ctx: &mut JavaContext,
    fenum: &ForeignEnumInfo,
) -> Result<()> {
    let enum_name = &fenum.name;
    trace!("generate_enum: enum {}", enum_name);
    if (fenum.items.len() as u64) >= (i32::MAX as u64) {
        return Err(DiagnosticError::new(
            fenum.src_id,
            fenum.span(),
            "Too many items in enum",
        ));
    }
    let enum_ti: Type = parse_ty_with_given_span(&enum_name.to_string(), fenum.name.span())
        .map_err(|err| DiagnosticError::from_syn_err(fenum.src_id, err))?;
    let enum_rty = ctx.conv_map.find_or_alloc_rust_type_that_implements(
        &enum_ti,
        &[C_LIKE_ENUM_TRAIT],
        fenum.src_id,
    );

    generate_java_code_for_enum(ctx, fenum)
        .map_err(|err| DiagnosticError::new(fenum.src_id, fenum.span(), err))?;
    generate_rust_code_for_enum(ctx, fenum)?;

    let jint_rty = ctx.conv_map.ty_to_rust_type(&parse_type! { jint });

    let enum_ftype = ForeignTypeS {
        name: ForeignTypeName::new(fenum.name.to_string(), (fenum.src_id, fenum.name.span())),
        provided_by_module: vec![],
        into_from_rust: Some(ForeignConversionRule {
            rust_ty: enum_rty.to_idx(),
            intermediate: Some(ForeignConversionIntermediate {
                input_to_output: false,
                intermediate_ty: jint_rty.to_idx(),
                conv_code: Rc::new(TypeConvCode::new(
                    format!(
                        "        {enum_name} {out} = {enum_name}.fromInt({var});",
                        out = TO_VAR_TEMPLATE,
                        enum_name = fenum.name,
                        var = FROM_VAR_TEMPLATE
                    ),
                    invalid_src_id_span(),
                )),
            }),
        }),
        from_into_rust: Some(ForeignConversionRule {
            rust_ty: enum_rty.to_idx(),
            intermediate: Some(ForeignConversionIntermediate {
                input_to_output: false,
                intermediate_ty: jint_rty.to_idx(),
                conv_code: Rc::new(TypeConvCode::new(
                    format!("        int {TO_VAR_TEMPLATE} = {FROM_VAR_TEMPLATE}.getValue();"),
                    invalid_src_id_span(),
                )),
            }),
        }),
    };
    ctx.conv_map.alloc_foreign_type(enum_ftype)?;

    add_conversion_from_enum_to_jobject_for_callbacks(ctx, fenum, enum_rty.to_idx());
    let enum_name = fenum.name.to_string();
    ctx.java_type_to_jni_sig_map.insert(
        enum_name.clone().into(),
        format!(
            "L{};",
            java_class_full_name(&ctx.cfg.package_name, &enum_name)
        )
        .into(),
    );

    Ok(())
}

fn generate_java_code_for_enum(
    ctx: &mut JavaContext,
    fenum: &ForeignEnumInfo,
) -> std::result::Result<(), String> {
    let path = ctx.cfg.output_dir.join(format!("{}.java", fenum.name));
    let mut file = FileWriteCache::new(&path, ctx.generated_foreign_files);
    let enum_doc_comments = doc_comments_to_java_comments(&fenum.doc_comments, true);
    writeln!(
        file,
        r#"// Automatically generated by flapigen
package {package_name};

{doc_comments}
public enum {enum_name} {{"#,
        package_name = ctx.cfg.package_name,
        enum_name = fenum.name,
        doc_comments = enum_doc_comments,
    )
    .expect(WRITE_TO_MEM_FAILED_MSG);

    for (i, item) in fenum.items.iter().enumerate() {
        let mut doc_comments = doc_comments_to_java_comments(&item.doc_comments, false);
        if !doc_comments.is_empty() {
            if !doc_comments.ends_with('\n') {
                doc_comments.push('\n');
            }
            doc_comments.push_str("    ");
        }
        writeln!(
            file,
            "    {doc_comments}{item_name}({index}){separator}",
            item_name = item.name,
            index = i,
            doc_comments = doc_comments,
            separator = if i == fenum.items.len() - 1 { ';' } else { ',' },
        )
        .expect(WRITE_TO_MEM_FAILED_MSG);
    }

    write!(
        file,
        r#"
    private final int value;
    {enum_name}(int value) {{
        this.value = value;
    }}
    public final int getValue() {{ return value; }}
    /*package*/ static {enum_name} fromInt(int x) {{
        switch (x) {{"#,
        enum_name = fenum.name
    )
    .expect(WRITE_TO_MEM_FAILED_MSG);

    for (i, item) in fenum.items.iter().enumerate() {
        write!(
            file,
            r#"
            case {index}: return {item_name};"#,
            index = i,
            item_name = item.name
        )
        .expect(WRITE_TO_MEM_FAILED_MSG);
    }

    writeln!(
        file,
        r#"
            default: throw new Error("Invalid value for enum {enum_name}: " + x);
        }}
    }}
}}"#,
        enum_name = fenum.name
    )
    .expect(WRITE_TO_MEM_FAILED_MSG);

    file.update_file_if_necessary().map_err(map_write_err)?;
    Ok(())
}

fn generate_rust_code_for_enum(ctx: &mut JavaContext, fenum: &ForeignEnumInfo) -> Result<()> {
    let mut arms_to_jint = Vec::with_capacity(fenum.items.len());
    let mut arms_from_jint = Vec::with_capacity(fenum.items.len());
    assert!((fenum.items.len() as u64) <= u64::from(i32::MAX as u32));
    for (i, item) in fenum.items.iter().enumerate() {
        let item_name = &item.rust_name;
        let idx = i as i32;
        arms_to_jint.push(quote! { #item_name => #idx });
        arms_from_jint.push(quote! { #idx => #item_name });
    }

    let rust_enum_name = &fenum.name;
    let trait_name = syn::Ident::new(C_LIKE_ENUM_TRAIT, Span::call_site());

    ctx.rust_code.push(quote! {
        impl #trait_name for #rust_enum_name {
            fn as_jint(&self) -> jint {
                match *self {
                    #(#arms_to_jint),*
                }
            }
            fn from_jint(x: jint) -> Self {
                match x {
                    #(#arms_from_jint),*
                    ,
                    _ => panic!(concat!("{} not expected for ", stringify!(#rust_enum_name)), x),
                }
            }
        }
    });

    Ok(())
}

fn add_conversion_from_enum_to_jobject_for_callbacks(
    ctx: &mut JavaContext,
    fenum: &ForeignEnumInfo,
    fenum_rty: RustTypeIdx,
) {
    let java_enum_full_name = java_class_full_name(&ctx.cfg.package_name, &fenum.name.to_string());
    let enum_class_name = java_class_name_to_jni(&java_enum_full_name);
    let enum_type = &fenum.name;
    let enum_id_upper = Ident::new(
        &format!("FOREIGN_ENUM_{}", fenum.name.to_string().to_uppercase()),
        Span::call_site(),
    );

    let mut arms_match_fields_names = Vec::with_capacity(fenum.items.len());
    for item in &fenum.items {
        let rust_name = &item.rust_name;
        let java_item = item.name.to_string();
        let enum_sig = format!("L{enum_class_name};");
        let enum_filed_global_var = Ident::new(
            &format!("{}_{}", enum_id_upper, java_item.to_uppercase()),
            Span::call_site(),
        );

        arms_match_fields_names.push(quote! {
            #rust_name => {
                let field = swig_jni_get_static_field_id!(#enum_filed_global_var, #enum_id_upper,
                                                         #java_item, #enum_sig);
                assert!(!field.is_null());
                field
            }
        });
    }

    let conv_code: TokenStream = quote! {
        #[allow(dead_code)]
        impl SwigFrom<#enum_type> for jobject {
            fn swig_from(x: #enum_type, env: *mut JNIEnv) -> jobject {
                let cls: jclass = swig_jni_find_class!(#enum_id_upper, #enum_class_name);
                assert!(!cls.is_null());
                let static_field_id: jfieldID = match x {
                    #(#arms_match_fields_names),*
                };
                assert!(!static_field_id.is_null());
                let ret: jobject = unsafe {
                    (**env).GetStaticObjectField.unwrap()(env, cls, static_field_id)
                };
                assert!(!ret.is_null(), concat!("Can get value of item in ", #enum_class_name));
                ret
            }
        }
    };
    ctx.rust_code.push(conv_code);

    let jobject_ty = ctx
        .conv_map
        .find_or_alloc_rust_type_no_src_id(&parse_type! { jobject });
    ctx.conv_map.add_conversion_rule(
        fenum_rty,
        jobject_ty.to_idx(),
        TypeConvEdge::new(
            TypeConvCode::new2(
                format!(
                    "let mut {to_var}: jobject = <jobject>::swig_from({from_var}, env);",
                    to_var = TO_VAR_TEMPLATE,
                    from_var = FROM_VAR_TEMPLATE,
                ),
                invalid_src_id_span(),
            ),
            None,
        ),
    );
}
