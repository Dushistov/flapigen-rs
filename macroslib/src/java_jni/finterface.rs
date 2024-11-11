use petgraph::Direction;
use proc_macro2::{Span, TokenStream};
use quote::quote;
use rustc_hash::FxHashMap;
use std::{io::Write, sync::LazyLock};
use syn::{spanned::Spanned, Ident};

use super::{
    java_code, map_type::map_type, map_write_err, rust_code, JavaContext, JavaForeignTypeInfo,
    JniForeignMethodSignature,
};
use crate::{
    error::{panic_on_syn_error, DiagnosticError, Result},
    file_cache::FileWriteCache,
    source_registry::SourceId,
    typemap::{
        ast::{DisplayToTokens, ForeignTypeName},
        ty::RustType,
        utils::rust_to_foreign_convert_method_inputs,
        ForeignTypeInfo,
    },
    types::ForeignInterface,
    WRITE_TO_MEM_FAILED_MSG,
};

pub(in crate::java_jni) fn generate_interface(
    ctx: &mut JavaContext,
    interface: &ForeignInterface,
) -> Result<()> {
    let f_methods = find_suitable_ftypes_for_interace_methods(ctx, interface)?;
    generate_java_code_for_interface(
        ctx,
        interface,
        &f_methods,
        ctx.cfg.null_annotation_package.as_deref(),
    )
    .map_err(|err| DiagnosticError::new(interface.src_id, interface.span(), err))?;
    generate_rust_code_for_interface(ctx, interface, &f_methods)?;

    let my_jobj_ti = ctx.conv_map.find_or_alloc_rust_type_with_suffix(
        &parse_type! { jobject },
        &interface.name.to_string(),
        SourceId::none(),
    );
    ctx.conv_map.add_foreign(
        my_jobj_ti,
        ForeignTypeName::from_ident(&interface.name, interface.src_id),
    )?;
    Ok(())
}

fn find_suitable_ftypes_for_interace_methods(
    ctx: &mut JavaContext,
    interace: &ForeignInterface,
) -> Result<Vec<JniForeignMethodSignature>> {
    let void_sym = "void";
    let dummy_ty = parse_type! { () };
    let dummy_rust_ty = ctx.conv_map.find_or_alloc_rust_type_no_src_id(&dummy_ty);
    let mut f_methods = Vec::with_capacity(interace.items.len());
    let jobject_ty = ctx
        .conv_map
        .find_or_alloc_rust_type_no_src_id(&parse_type! { jobject });

    for method in &interace.items {
        let mut input = Vec::<JavaForeignTypeInfo>::with_capacity(method.fn_decl.inputs.len() - 1);
        for arg in method.fn_decl.inputs.iter().skip(1) {
            let named_arg = arg
                .as_named_arg()
                .map_err(|err| DiagnosticError::from_syn_err(interace.src_id, err))?;
            let arg_rust_ty = ctx
                .conv_map
                .find_or_alloc_rust_type(&named_arg.ty, interace.src_id);
            let arg_span = (interace.src_id, named_arg.ty.span());
            let mut f_arg_type = map_type(ctx, &arg_rust_ty, Direction::Outgoing, arg_span)?;
            if let Some(java_conv) = f_arg_type.java_converter.as_ref() {
                // it is hard to use Java code during callback, so may be
                // there is way to convert it to jobject ?
                ctx.conv_map
                    .convert_rust_types(
                        arg_rust_ty.to_idx(),
                        jobject_ty.to_idx(),
                        "x",
                        "y",
                        "ret",
                        arg_span,
                    )
                    .map_err(|err| {
                        err.add_span_note(
                            arg_span,
                            format!(
                                "Java code required to convert type to jobject
It is impossible to use this Java code:{}\nfor callback types conversion",
                                java_conv.converter
                            ),
                        )
                    })?;
                f_arg_type.java_converter = None;
                f_arg_type.base.corresponding_rust_type = jobject_ty.clone();
            }

            input.push(f_arg_type);
        }
        let output = match method.fn_decl.output {
            syn::ReturnType::Default => ForeignTypeInfo {
                name: void_sym.into(),
                corresponding_rust_type: dummy_rust_ty.clone(),
            }
            .into(),
            syn::ReturnType::Type(_, ref ret_ty) => {
                let rust_ret_ty = ctx
                    .conv_map
                    .find_or_alloc_rust_type(ret_ty, interace.src_id);
                let f_ret_type = map_type(
                    ctx,
                    &rust_ret_ty,
                    Direction::Incoming,
                    rust_ret_ty.src_id_span(),
                )?;
                if let Some(conv) = f_ret_type.java_converter {
                    return Err(DiagnosticError::new2(
                        rust_ret_ty.src_id_span(),
                        format!("Java code:\n```{}\n```\n required to convert Rust output type to Java type.
It is impossible to use for callback function.", conv.converter),
                    ));
                }

                f_ret_type
            }
        };
        f_methods.push(JniForeignMethodSignature { output, input });
    }
    Ok(f_methods)
}

fn generate_java_code_for_interface(
    ctx: &mut JavaContext,
    interface: &ForeignInterface,
    methods_sign: &[JniForeignMethodSignature],
    use_null_annotation: Option<&str>,
) -> std::result::Result<(), String> {
    let path = ctx.cfg.output_dir.join(format!("{}.java", interface.name));
    let mut file = FileWriteCache::new(&path, ctx.generated_foreign_files);
    let imports = java_code::get_null_annotation_imports(use_null_annotation, methods_sign);
    let interface_comments =
        java_code::doc_comments_to_java_comments(&interface.doc_comments, true);
    writeln!(
        file,
        r#"// Automatically generated by flapigen
package {package_name};
{imports}
{doc_comments}
public interface {interface_name} {{"#,
        package_name = ctx.cfg.package_name,
        interface_name = interface.name,
        doc_comments = interface_comments,
        imports = imports,
    )
    .expect(WRITE_TO_MEM_FAILED_MSG);

    for (method, f_method) in interface.items.iter().zip(methods_sign) {
        writeln!(
            file,
            r#"
{doc_comments}
    {output_type} {method_name}({single_args_with_types});"#,
            method_name = method.name,
            doc_comments = java_code::doc_comments_to_java_comments(&method.doc_comments, false),
            single_args_with_types = java_code::args_with_java_types(
                f_method,
                method.arg_names_without_self(),
                java_code::ArgsFormatFlags::EXTERNAL,
                use_null_annotation.is_some()
            ),
            output_type = f_method.output.base.name,
        )
        .expect(WRITE_TO_MEM_FAILED_MSG);
    }

    file.write_all(b"\n}\n").expect(WRITE_TO_MEM_FAILED_MSG);
    file.update_file_if_necessary().map_err(map_write_err)?;
    Ok(())
}

fn generate_rust_code_for_interface(
    ctx: &mut JavaContext,
    interface: &ForeignInterface,
    methods_sign: &[JniForeignMethodSignature],
) -> Result<()> {
    use std::fmt::Write;

    let mut new_conv_code = format!(
        r#"
#[swig_from_foreigner_hint = "{interface_name}"]
impl SwigFrom<jobject> for Box<dyn {trait_name}> {{
    fn swig_from(this: jobject, env: *mut JNIEnv) -> Self {{
        let mut cb = JavaCallback::new(this, env);
        cb.methods.reserve({methods_len});
        let class = unsafe {{ (**env).GetObjectClass.unwrap()(env, cb.this) }};
        assert!(!class.is_null(), "GetObjectClass return null class for {interface_name}");
"#,
        interface_name = interface.name,
        trait_name = DisplayToTokens(&interface.self_type),
        methods_len = interface.items.len(),
    );
    for (method, f_method) in interface.items.iter().zip(methods_sign) {
        writeln!(
            &mut new_conv_code,
            r#"
        let method_id: jmethodID = unsafe {{
            (**env).GetMethodID.unwrap()(env, class, swig_c_str!("{method_name}"),
                                         swig_c_str!("{method_sig}"))
        }};
        assert!(!method_id.is_null(), "Can not find {method_name} id");
        cb.methods.push(method_id);"#,
            method_name = method.name,
            method_sig = rust_code::jni_method_signature(ctx, f_method),
        )
        .unwrap();
    }
    new_conv_code.push_str(
        r#"
        Box::new(cb)
    }
}
"#,
    );
    ctx.conv_map
        .merge(SourceId::none(), &new_conv_code, ctx.pointer_target_width)?;

    let mut trait_impl_funcs = Vec::<TokenStream>::with_capacity(interface.items.len());

    for (method_idx, (method, f_method)) in interface.items.iter().zip(methods_sign).enumerate() {
        let func_name = &method
            .rust_name
            .segments
            .last()
            .ok_or_else(|| {
                DiagnosticError::new(
                    interface.src_id,
                    method.rust_name.span(),
                    "Empty trait function name",
                )
            })?
            .ident;

        let self_arg: TokenStream = method.fn_decl.inputs[0]
            .as_self_arg(interface.src_id)?
            .into();
        let mut args_with_types = Vec::with_capacity(method.fn_decl.inputs.len());
        args_with_types.push(self_arg);
        args_with_types.extend(
            method
                .fn_decl
                .inputs
                .iter()
                .skip(1)
                .enumerate()
                .map(|(i, v)| {
                    let arg_ty = &v.as_named_arg().unwrap().ty;
                    let arg_name = Ident::new(&format!("a{}", i), Span::call_site());
                    quote!(#arg_name: #arg_ty)
                }),
        );
        assert!(!method.fn_decl.inputs.is_empty());
        let n_args = method.fn_decl.inputs.len() - 1;
        let (args, type_size_asserts) = convert_args_for_variadic_function_call(f_method);

        let (mut conv_deps, convert_args_code) = rust_to_foreign_convert_method_inputs(
            ctx.conv_map,
            interface.src_id,
            method,
            f_method,
            (0..n_args).map(|v| format!("a{}", v)),
            "()",
        )?;
        ctx.rust_code.append(&mut conv_deps);
        let convert_args: TokenStream = syn::parse_str(&convert_args_code).unwrap_or_else(|err| {
            panic_on_syn_error(
                "java/jni internal parse failed for convert arguments code",
                convert_args_code,
                err,
            )
        });
        match method.fn_decl.output {
            syn::ReturnType::Default => trait_impl_funcs.push(quote! {
                #[allow(unused_mut)]
                fn #func_name(#(#args_with_types),*) {
                    #type_size_asserts
                    let env = self.get_jni_env();
                    if let Some(env) = env.env {
                        #convert_args
                        unsafe {
                            (**env).CallVoidMethod.unwrap()(env, self.this,
                                                            self.methods[#method_idx],
                                                            #(#args),*);
                            if (**env).ExceptionCheck.unwrap()(env) != 0 {
                                log::error!(concat!(stringify!(#func_name), ": java throw exception"));
                                (**env).ExceptionDescribe.unwrap()(env);
                                (**env).ExceptionClear.unwrap()(env);
                            }
                        };
                    }
                }
            }),
            syn::ReturnType::Type(_, ref ret_ty) => {
                let real_output_type: RustType = ctx
                    .conv_map
                    .find_or_alloc_rust_type(ret_ty, interface.src_id);
                let jni_ret_type = &f_method.output.base.corresponding_rust_type;
                let (mut conv_deps, out_conv_code) = ctx.conv_map.convert_rust_types(
                    jni_ret_type.to_idx(),
                    real_output_type.to_idx(),
                    "ret",
                    "ret",
                    real_output_type.normalized_name.as_str(),
                    (interface.src_id, ret_ty.span()),
                )?;
                ctx.rust_code.append(&mut conv_deps);

                let jni_caller = match jni_ret_type.normalized_name.as_str() {
                    "jboolean" => quote! { CallBooleanMethod },
                    "jbyte" => quote! {CallByteMethod },
                    "jshort" => quote! { CallShortMethod },
                    "jint" => quote!{ CallIntMethod },
                    "jlong" => quote!{ CallLongMethod },
                    "jfloat" => quote!{ CallFloatMethod },
                    "jdouble" => quote!{ CallDoubleMethod },
                    "jobject" => quote!{ CallObjectMethod },
                    _ => return Err(DiagnosticError::new2(jni_ret_type.src_id_span(),
                                                          format!("Have not idea how to handle this type `{}` as return of callback function", jni_ret_type))),
                };
                let jni_ret_type = &jni_ret_type.ty;
                let out_conv_code: TokenStream = syn::parse_str(&out_conv_code).unwrap_or_else(|err| {
                    panic_on_syn_error("Internal: java_jni/finterface: out_conv_code", out_conv_code, err)
                });
                trait_impl_funcs.push(quote! {
                    #[allow(unused_mut)]
                    fn #func_name(#(#args_with_types),*) -> #ret_ty {
                        #type_size_asserts
                        let env = self.get_jni_env();
                        let env = env.env.expect(concat!("Can not get env for ", stringify!(#func_name)));

                        #convert_args
                        let mut ret: #jni_ret_type;
                        unsafe {
                            ret = (**env).#jni_caller.unwrap()(env, self.this,
                                                            self.methods[#method_idx],
                                                            #(#args),*);
                            if (**env).ExceptionCheck.unwrap()(env) != 0 {
                                log::error!(concat!(stringify!(#func_name), ": java throw exception"));
                                (**env).ExceptionDescribe.unwrap()(env);
                                (**env).ExceptionClear.unwrap()(env);
                            }
                        };
                        #out_conv_code
                        ret
                    }
                });
            }
        }
    }

    let self_type_name = &interface.self_type.bounds[0];
    let tt: TokenStream = quote! {
        impl #self_type_name for JavaCallback {
            #(#trait_impl_funcs)*
        }
    };
    ctx.rust_code.push(tt);
    Ok(())
}

static JNI_FOR_VARIADIC_C_FUNC_CALL: LazyLock<FxHashMap<&'static str, &'static str>> =
    LazyLock::new(|| {
        let mut m = FxHashMap::default();
        m.insert("jboolean", "::std::os::raw::c_uint");
        m.insert("jbyte", "::std::os::raw::c_int");
        m.insert("jshort", "::std::os::raw::c_int");
        m.insert("jfloat", "f64");
        m
    });

// To use `C` function with variable number of arguments,
// we need automatic type conversion, see
// http://en.cppreference.com/w/c/language/conversion#Default_argument_promotions
// for more details.
// return arg with conversion plus asserts
fn convert_args_for_variadic_function_call(
    f_method: &JniForeignMethodSignature,
) -> (Vec<TokenStream>, TokenStream) {
    let mut ret = Vec::with_capacity(f_method.input.len());
    for (i, arg) in f_method.input.iter().enumerate() {
        let arg_name = Ident::new(&format!("a{}", i), Span::call_site());
        if let Some(conv_type_str) = JNI_FOR_VARIADIC_C_FUNC_CALL.get(
            arg.as_ref()
                .corresponding_rust_type
                .normalized_name
                .as_str(),
        ) {
            let conv_type: TokenStream = syn::parse_str(conv_type_str).unwrap_or_else(|err| {
                panic_on_syn_error(
                    "java/jni internal error: can not parse type for variable conversion",
                    conv_type_str.to_string(),
                    err,
                )
            });
            ret.push(quote!(#arg_name as #conv_type));
        } else {
            ret.push(quote!(#arg_name));
        }
    }
    let check_sizes = quote! {
        swig_assert_eq_size!(::std::os::raw::c_uint, u32);
        swig_assert_eq_size!(::std::os::raw::c_int, i32);
    };
    (ret, check_sizes)
}
