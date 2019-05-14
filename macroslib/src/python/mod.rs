use crate::{
    ast, error::Result, DiagnosticError, ForeignEnumInfo, ForeignInterface, ForeignerClassInfo,
    ForeignerMethod, LanguageGenerator, MethodVariant, PythonConfig, RustType, SelfTypeVariant,
    TypeMap,
};
use heck::SnakeCase;
use proc_macro2::{Span, TokenStream};
use quote::quote;
use quote::ToTokens;
use std::ops::Deref;
use syn::parse_quote;
use syn::spanned::Spanned;

use syn::{Ident, Type};

impl LanguageGenerator for PythonConfig {
    fn register_class(&self, _conv_map: &mut TypeMap, _class: &ForeignerClassInfo) -> Result<()> {
        Ok(())
    }

    fn generate(
        &self,
        conv_map: &mut TypeMap,
        _: usize,
        class: &ForeignerClassInfo,
    ) -> Result<Vec<TokenStream>> {
        let class_name = &class.name;
        let wrapper_mod_name =
            syn::parse_str::<Ident>(&py_wrapper_mod_name(&class_name.to_string()))?;
        self.module_initialization_code.borrow_mut().push(quote! {
            {
                m.add_class::<#wrapper_mod_name::#class_name>(py)?;
            }
        });
        let (rust_instance_field, rust_instance_getter) =
            generate_rust_instance_field_and_methods(class)?;
        let methods_code = class
            .methods
            .iter()
            .map(|m| generate_method_code(class, m, conv_map))
            .collect::<Result<Vec<_>>>()?;
        let class_code = quote! {
            mod #wrapper_mod_name {
                #[allow(unused)]
                py_class!(pub class #class_name |py| {
                    #rust_instance_field

                    #( #methods_code )*
                });

                #rust_instance_getter
            }
        };
        Ok(vec![class_code])
    }

    fn generate_enum(
        &self,
        conv_map: &mut TypeMap,
        _pointer_target_width: usize,
        enum_info: &ForeignEnumInfo,
    ) -> Result<Vec<TokenStream>> {
        let enum_name = &enum_info.name;
        let wrapper_mod_name =
            syn::parse_str::<Ident>(&py_wrapper_mod_name(&enum_name.to_string()))?;
        self.module_initialization_code.borrow_mut().push(quote! {
            {
                m.add_class::<#wrapper_mod_name::#enum_name>(py)?;
            }
        });
        let foreign_variants = enum_info.items.iter().map(|item| &item.name);
        let rust_variants = enum_info.items.iter().map(|item| &item.rust_name).collect::<Vec<_>>();
        let rust_variants_ref_1 = &rust_variants;
        let rust_variants_ref_2 = &rust_variants;
        let enum_name_str = enum_name.to_string();
        let class_code = quote! {
            mod #wrapper_mod_name {
                py_class!(pub class #enum_name |py| {
                    #( static #foreign_variants = super::#rust_variants_ref_1 as u32; )*
                });

                pub fn from_u32(py: cpython::Python, value: u32) -> cpython::PyResult<super::#enum_name> {
                    #( if value == super::#rust_variants_ref_1 as u32 { return Ok(super::#rust_variants_ref_2); } )*
                    Err(cpython::PyErr::new::<cpython::exc::ValueError, _>(
                        py, format!("{} is not valid value for enum {}", value, #enum_name_str)
                    ))
                }
            }
        };
        conv_map.register_exported_enum(enum_info);
        Ok(vec![class_code])
    }

    fn generate_interface(
        &self,
        _conv_map: &mut TypeMap,
        _pointer_target_width: usize,
        _interface: &ForeignInterface,
    ) -> Result<Vec<TokenStream>> {
        Ok(vec![])
    }

    fn finish_glue_rs(&self, _conv_map: &mut TypeMap) -> Result<Vec<TokenStream>> {
        let module_initialization_code_cell = self.module_initialization_code.borrow();
        let module_initialization_code = &*module_initialization_code_cell;
        let module_name = syn::parse_str::<syn::Ident>(&self.module_name)?;
        let registration_code = vec![quote! {
            py_exception!(#module_name, Error);
            py_module_initializer!(librust_swig_test_python, initlibrust_swig_test_python, PyInit_rust_swig_test_python, |py, m| {
                m.add(py, "__doc__", "This is test module for rust_swig.")?;
                //m.add_class::<Error>(py)?;
                #(#module_initialization_code)*
                Ok(())
            });
        }];
        Ok(registration_code)
    }
}

fn generate_rust_instance_field_and_methods(
    class: &ForeignerClassInfo,
) -> Result<(TokenStream, TokenStream)> {
    if let Some(ref rust_self_type) = class.self_type {
        let self_type = &rust_self_type.ty;
        let class_name = &class.name;
        Ok((
            quote! {
                data rust_instance: std::sync::RwLock<super::#self_type>;
            },
            // For some reason, rust-cpython generates private `rust_instance` getter method.
            // As a workaround, we add public function in the same module, that gets `rust_instance`.
            // The same goes for `create_instance
            quote! {
                pub fn rust_instance<'a>(class: &'a #class_name, py: cpython::Python<'a>) -> &'a std::sync::RwLock<super::#self_type> {
                    class.rust_instance(py)
                }

                pub fn create_instance(py: cpython::Python, instance: std::sync::RwLock<super::#self_type>) -> cpython::PyResult<#class_name> {
                    #class_name::create_instance(py, instance)
                }
            },
        ))
    } else if !has_any_methods(class) {
        Ok((TokenStream::new(), TokenStream::new()))
    } else {
        Err(DiagnosticError::new(
            class.span(),
            format!(
                "Class {} has non-static methods, but no self_type",
                class.name
            ),
        ))
    }
}

fn generate_method_code(
    class: &ForeignerClassInfo,
    method: &ForeignerMethod,
    conv_map: &TypeMap,
) -> Result<TokenStream> {
    if method.is_dummy_constructor() {
        return Ok(TokenStream::new());
    }
    let method_name = method_name(method)?;
    let method_rust_path = &method.rust_id;
    let skip_args_count = if let MethodVariant::Method(_) = method.variant {
        1
    } else {
        0
    };
    let (args_types, mut args_convertions): (Vec<_>, Vec<_>) = method
        .fn_decl
        .inputs
        .iter()
        .skip(skip_args_count)
        .enumerate()
        .map(|(i, a)| {
            let arg_name = format!("a_{}", i);
            generate_conversion_for_argument(
                &ast::fn_arg_type(a).clone().into(),
                method.span(),
                conv_map,
                &arg_name,
            )
        })
        .collect::<Result<Vec<_>>>()?
        .into_iter()
        .unzip();
    if let Some(self_convertion) = self_type_conversion(class, method, conv_map)? {
        args_convertions.insert(0, self_convertion);
    }
    let mut args_list = args_types
        .into_iter()
        .enumerate()
        .map(|(i, t)| syn::parse_str(&format!("a_{}: {}", i, t.into_token_stream().to_string())))
        .collect::<std::result::Result<Vec<TokenStream>, _>>()?;
    if let MethodVariant::Method(_) = method.variant {
        args_list.insert(0, syn::parse_str("&self")?);
    } else if method.variant == MethodVariant::Constructor {
        args_list.insert(0, syn::parse_str("_cls")?);
    }
    let attribute = if method.variant == MethodVariant::StaticMethod {
        syn::parse_str("@staticmethod")?
    } else {
        TokenStream::new()
    };
    let (return_type, return_conversion) = generate_conversion_for_return(
        &extract_return_type(&method.fn_decl.output),
        method.span(),
        conv_map,
        "_ret",
    )?;
    Ok(quote! {
        #attribute def #method_name(
            #( #args_list ),*
        ) -> cpython::PyResult<#return_type> {
            let _ret = super::#method_rust_path(
                #( #args_convertions ),*
            );
            Ok(#return_conversion)
        }
    })
}

fn standard_method_name(method: &ForeignerMethod) -> Result<syn::Ident> {
    Ok(method
        .name_alias
        .as_ref()
        .or_else(|| method.rust_id.segments.last().map(|p| &p.value().ident))
        .ok_or_else(|| DiagnosticError::new(method.span(), "Method has no name"))?
        .clone())
}

fn method_name(method: &ForeignerMethod) -> Result<syn::Ident> {
    if method.variant == MethodVariant::Constructor {
        Ok(syn::parse_str("__new__")?)
    } else {
        standard_method_name(method)
    }
}

fn self_type_conversion(
    class: &ForeignerClassInfo,
    method: &ForeignerMethod,
    conv_map: &TypeMap,
) -> Result<Option<TokenStream>> {
    if let MethodVariant::Method(self_variant) = method.variant {
        let self_type = &class
            .self_type
            .as_ref()
            .ok_or_else(|| {
                DiagnosticError::new(
                    class.span(),
                    "Class have non-static methods, but no self_type",
                )
            })?
            .ty;
        let self_type_ref = match self_variant {
            SelfTypeVariant::Rptr => parse_type! {&#self_type},
            SelfTypeVariant::RptrMut => parse_type! {&mut #self_type},
            _ => unimplemented!("Passing self by value not implemented"),
        };
        Ok(Some(
            generate_conversion_for_argument(&self_type_ref.into(), method.span(), conv_map, "self")?.1,
        ))
    } else {
        Ok(None)
    }
}

fn has_any_methods(class: &ForeignerClassInfo) -> bool {
    class.methods.iter().any(|m| {
        if let MethodVariant::Method(_) = m.variant {
            true
        } else {
            false
        }
    })
}

fn generate_conversion_for_argument(
    rust_type: &RustType,
    method_span: Span,
    conv_map: &TypeMap,
    arg_name: &str,
) -> Result<(Type, TokenStream)> {
    let arg_name_ident: TokenStream = syn::parse_str(arg_name)?;
    if is_cpython_supported_type(rust_type) {
        Ok((rust_type.ty.clone(), arg_name_ident))
    } else if let Some(foreign_class) =
        conv_map.find_foreigner_class_with_such_self_type(&rust_type.ty, true)
    {
        let class_name = foreign_class.name.to_string();
        let py_mod_str = py_wrapper_mod_name(&class_name);
        let py_mod: Ident = syn::parse_str(&py_mod_str)?;
        let py_type: Type = syn::parse_str(&format!("&super::{}::{}", &py_mod_str, &class_name))?;
        let self_conversion_code = if let Type::Reference(ref reference) = rust_type.ty {
            if reference.mutability.is_some() {
                quote! {
                    &mut *super::#py_mod::rust_instance(#arg_name_ident, py).write().unwrap()
                }
            } else {
                quote! {
                    &*super::#py_mod::rust_instance(#arg_name_ident, py).read().unwrap()
                }
            }
        } else {
            unimplemented!("Passing object by value not implemented")
        };
        Ok((py_type, self_conversion_code))
    } else if let Some(enum_info) = conv_map.is_this_exported_enum(&rust_type.ty) {
        let enum_py_mod: Ident = syn::parse_str(&py_wrapper_mod_name(&enum_info.name.to_string()))?;
        Ok((
            parse_type!(u32),
            quote!{
                super::#enum_py_mod::from_u32(py, #arg_name_ident)?
            }
        ))
    } else if let Some(inner) = ast::if_option_return_some_type(&rust_type.ty) {
        let (inner_py_type, inner_conversion) = generate_conversion_for_argument(
            &inner.into(),
            method_span,
            conv_map,
            "inner",
        )?;
        Ok((
            parse_type!(Option<#inner_py_type>),
            quote!{
                #arg_name_ident.map(|inner| #inner_conversion)
            }
        ))
    } else if let Some(inner) = ast::if_type_slice_return_elem_type(&rust_type.ty, false) {
        let (inner_py_type, inner_conversion) = generate_conversion_for_argument(
            &inner.clone().into(),
            method_span,
            conv_map,
            "inner",
        )?;
        Ok((
            parse_type!(Vec<#inner_py_type>),
            quote!{
                &#arg_name_ident.into_iter().map(|inner| #inner_conversion).collect::<Vec<_>>()
            }
        ))
    } else if let Some(inner) = ast::if_vec_return_elem_type(&rust_type.ty) {
        let (inner_py_type, inner_conversion) = generate_conversion_for_argument(
            &inner.clone().into(),
            method_span,
            conv_map,
            "inner",
        )?;
        Ok((
            parse_type!(Vec<#inner_py_type>),
            quote!{
                #arg_name_ident.into_iter().map(|inner| #inner_conversion).collect::<Vec<_>>()
            }
        ))
    } else if let Type::Reference(ref inner) = rust_type.ty {
        if inner.mutability.is_some() {
            return Err(DiagnosticError::new(method_span, "mutable reference is only supported for exported class types"));
        }
        let (inner_py_type, inner_conversion) = generate_conversion_for_argument(
            &inner.elem.deref().clone().into(),
            method_span,
            conv_map,
            arg_name,
        )?;
        Ok((
            parse_type!(#inner_py_type),
            quote!{
                &#inner_conversion
            }
        ))
    } else {
        unimplemented!("other arg");
    }
}

fn generate_conversion_for_return(
    rust_type: &RustType,
    method_span: Span,
    conv_map: &TypeMap,
    ret_name: &str,
) -> Result<(Type, TokenStream)> {
    let ret_name_ident: TokenStream = syn::parse_str(ret_name)?;
    if rust_type.normalized_name == "( )" {
        Ok((parse_type!(cpython::PyObject), quote!(py.None())))
    } else if is_cpython_supported_type(rust_type) {
        Ok((rust_type.ty.clone(), ret_name_ident))
    } else if let Some(class) = conv_map.find_foreigner_class_with_such_this_type(&rust_type.ty) {
        if let Type::Reference(_) = rust_type.ty {
            return Err(DiagnosticError::new(
                method_span,
                "Returning rust object into python by reference is not safe and not supported.",
            ));
        };
        let class_name = &class.name;
        let py_mod: Ident = syn::parse_str(&py_wrapper_mod_name(&class_name.to_string()))?;
        Ok((
            parse_type!(super::#py_mod::#class_name),
            quote! {
                super::#py_mod::create_instance(py, std::sync::RwLock::new(#ret_name_ident))?
            },
        ))
    } else if conv_map.is_this_exported_enum(&rust_type.ty).is_some() {
        Ok((
            parse_type!(u32),
            quote!{
                #ret_name_ident as u32
            }
        ))
    } else if let Some(inner) = ast::if_option_return_some_type(&rust_type.ty) {
        let (inner_py_type, inner_conversion) = generate_conversion_for_return(
            &inner.into(),
            method_span,
            conv_map,
            "ret_inner",
        )?;
        Ok((
            parse_type!(Option<#inner_py_type>),
            quote!{
                #ret_name_ident.map(|ret_inner| #inner_conversion)
            }
        ))
    } else if let Some(inner) = ast::if_vec_return_elem_type(&rust_type.ty) {
        let (inner_py_type, inner_conversion) = generate_conversion_for_return(
            &inner.clone().into(),
            method_span,
            conv_map,
            "inner",
        )?;
        Ok((
            parse_type!(Vec<#inner_py_type>),
            quote!{
                #ret_name_ident.into_iter().map(|inner| #inner_conversion).collect::<Vec<_>>()
            }
        ))
    } else if let Some((inner_ok, inner_err)) = ast::if_result_return_ok_err_types(&rust_type.ty) {
        let (inner_py_type, inner_conversion) = generate_conversion_for_return(
            &inner_ok.into(),
            method_span,
            conv_map,
            "ok_inner",
        )?;
        let (inner_py_err_type, inner_err_conversion) = generate_conversion_for_return(
            &inner_err.into(),
            method_span,
            conv_map,
            "err_inner",
        )?;
        Ok((
            parse_type!(#inner_py_type),
            quote!{
                match #ret_name_ident {
                    Ok(ok_inner) => #inner_conversion,
                    Err(err_inner) => return Err(cpython::PyErr::new::<super::Error, #inner_py_err_type>(py, #inner_err_conversion)),
                }
            }
        ))
    } else {
        unimplemented!();
    }
}

// fn option_inner_type(rust_type: &RustType) -> Some(RustType) {
//     if let Type::Path(type_path) = foo {
//         foo
//     }
// }



// fn container_and_inner_type(
//     rust_type: &RustType,
//     conv_map: &TypeMap,
//     arg_name: &str,) -> Option<(RustType, RustType, )>

fn is_cpython_supported_type(rust_type: &RustType) -> bool {
    let primitive_types = [
        "bool", "i8", "i16", "i32", "i64", "isize", "u8", "u16", "u32", "u64", "usize", "f32",
        "f64", "String", "& str",
    ];
    primitive_types.contains(&rust_type.normalized_name.as_str())
}

fn extract_return_type(syn_return_type: &syn::ReturnType) -> RustType {
    match syn_return_type {
        syn::ReturnType::Default => {
            let mut ty: Type = parse_type! { () };
            ast::change_span(&mut ty, syn_return_type.span());
            ty.into()
        }
        syn::ReturnType::Type(_, ref ty) => {
            let ty: Type = *ty.clone();
            ty.into()
        }
    }
}

fn py_wrapper_mod_name(type_name: &str) -> String {
    format!("py_{}", type_name.to_snake_case())
}
