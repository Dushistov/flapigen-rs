use crate::{
    ast, DiagnosticError, error::Result, ForeignEnumInfo, ForeignInterface, ForeignerClassInfo, ForeignerMethod, LanguageGenerator,
    MethodVariant, PythonConfig, RustType, SelfTypeVariant, TypeMap,
};
use heck::SnakeCase;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{Ident, Type};
use syn::spanned::Spanned;
use syn::parse_quote;

fn method_name(method: &ForeignerMethod) -> Result<&syn::Ident> {
    Ok(&method.rust_id.segments.last().ok_or_else(|| {
        DiagnosticError::new(method.span(), "Method has no name")
    })?.value().ident)
}

impl LanguageGenerator for PythonConfig {
    fn register_class(&self, conv_map: &mut TypeMap, class: &ForeignerClassInfo) -> Result<()> {
        Ok(())
    }

    fn generate(
        &self,
        conv_map: &mut TypeMap,
        _: usize,
        class: &ForeignerClassInfo,
    ) -> Result<Vec<TokenStream>> {
        let class_name = &class.name;
        let wrapper_mod_name = syn::parse_str::<Ident>(&py_wrapper_mod_name(&class_name.to_string()))?;
        self.module_initialization_code.borrow_mut().push(quote! {
            {
                m.add_class::<#wrapper_mod_name::#class_name>(py)?;
            }
        });
        let self_type = class.self_type_as_ty();
        let (rust_instance_field, rust_instance_getter) = generate_rust_instance_field_and_getter(class, conv_map)?;
        let methods_code = class.methods
            .iter()
            .map(|m| generate_method_code(class, m, conv_map))
            .collect::<Result<Vec<_>>>()?;
        let class_code = quote!{
            mod #wrapper_mod_name {
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
        pointer_target_width: usize,
        enum_info: &ForeignEnumInfo,
    ) -> Result<Vec<TokenStream>> {
        let enum_name = &enum_info.name;
        let wrapper_mod_name = syn::parse_str::<Ident>(&py_wrapper_mod_name(&enum_name.to_string()))?;
        self.module_initialization_code.borrow_mut().push(quote! {
            {
                m.add_class::<#wrapper_mod_name::#enum_name>(py)?;
            }

        });
        let foreign_variants = enum_info.items.iter().map(|item| &item.name);
        let rust_variants = enum_info.items.iter().map(|item| &item.rust_name);
        let class_code = quote! {
            mod #wrapper_mod_name {
                py_class!(pub class #enum_name |py| {
                    #( static #foreign_variants = super::#rust_variants as u32; )*
                });
            }
        };
        conv_map.register_exported_enum(enum_info);
        Ok(vec![class_code])
    }

    fn generate_interface(
        &self,
        conv_map: &mut TypeMap,
        pointer_target_width: usize,
        interface: &ForeignInterface,
    ) -> Result<Vec<TokenStream>> {
        Ok(vec![])
    }

    fn finish_glue_rs(&self, _conv_map: &mut TypeMap) -> Result<Vec<TokenStream>> {
        let module_initialization_code_cell = self.module_initialization_code.borrow();
        let module_initialization_code = &*module_initialization_code_cell;
        let registration_code = vec![quote! {
            py_module_initializer!(librust_swig_test_python, initlibrust_swig_test_python, PyInit_rust_swig_test_python, |py, m| {
                m.add(py, "__doc__", "This is test module for rust_swig.")?;
                #(#module_initialization_code)*
                Ok(())
            });
        }];
        Ok(registration_code)
    }
}

fn generate_rust_instance_field_and_getter(class: &ForeignerClassInfo, conv_map: &TypeMap) -> Result<(TokenStream, TokenStream)> {
    if let Some(ref rust_self_type) = class.self_type {
        let self_type = &rust_self_type.ty;
        let class_name = &class.name;
        Ok((quote!{
            data rust_instance: std::sync::RwLock<super::#self_type>;
        }, 
        // For some reason, rust-cpython generates private `rust_instance` getter method.
        // As a workaround, we add public function in the same module, that gets `rust_instance`.
        quote!{
            pub fn rust_instance<'a>(class: &'a #class_name, py: cpython::Python<'a>) -> &'a std::sync::RwLock<super::#self_type> {
                class.rust_instance(py)
            }
        }))
    } else if !has_any_methods(class) {
        Ok((TokenStream::new(), TokenStream::new()))
    } else {
        Err(DiagnosticError::new(class.span(), format!("Class {} has non-static methods, but no self_type", class.name)))
    }
}

fn generate_method_code(class: &ForeignerClassInfo, method: &ForeignerMethod, conv_map: &TypeMap) -> Result<TokenStream> {
    match method.variant {
        MethodVariant::Constructor => generate_constructor_code(class, conv_map),
        MethodVariant::Method(self_variant) => generate_standard_method_code(method, self_variant, conv_map),
        MethodVariant::StaticMethod => generate_static_method_code(method, conv_map),
    }
}

fn generate_constructor_code(class: &ForeignerClassInfo, conv_map: &TypeMap) -> Result<TokenStream> {
    if let Some(constructor) = class.methods
        .iter()
        .find(|m| m.variant == MethodVariant::Constructor) {
        let class_name = &class.name;
        let constructor_rust_path = &constructor.rust_id;
        Ok(quote!{
             def __new__(_cls) -> cpython::PyResult<#class_name> {
                let rust_instance = super::#constructor_rust_path();
                #class_name::create_instance(py, std::sync::RwLock::new(rust_instance))
            }
        })
    } else if !has_any_methods(class) {
        Ok(TokenStream::new())
    } else {
        Err(DiagnosticError::new(class.span(), format!("Class {} has non-static methods, but no constructor", class.name)))
    }
}

fn generate_standard_method_code(method: &ForeignerMethod, self_variant: SelfTypeVariant, conv_map: &TypeMap) -> Result<TokenStream> {
    let method_name = method_name(method)?;
    let method_rust_path = &method.rust_id;
    let self_conversion_code = match self_variant {
        SelfTypeVariant::Rptr => quote!{
            &*self.rust_instance(py).read().unwrap()
        },
        SelfTypeVariant::RptrMut => quote!{
            &mut *self.rust_instance(py).write().unwrap()
        },
        _ => unimplemented!("Passing self by value not implemented yet"),
    };
    Ok(quote!{
        def #method_name(&self) -> cpython::PyResult<cpython::PyObject> {
            super::#method_rust_path(#self_conversion_code);
            Ok(py.None())
        }
    })
}

fn generate_static_method_code(method: &ForeignerMethod, conv_map: &TypeMap) -> Result<TokenStream> {
    let static_method_name = method_name(method)?;
    let static_method_rust_path = &method.rust_id;
    let (args_types, args_convertions): (Vec<_>, Vec<_>) = method.fn_decl.inputs.iter().enumerate().map(|(i, a)| {
        let arg_name = format!("a_{}", i);
        generate_conversion_for_argument(&ast::fn_arg_type(a).clone().into(), conv_map, &arg_name)
    }).collect::<Result<Vec<_>>>()?.into_iter().unzip();
    let args_names = (0..method.fn_decl.inputs.len())
        .map(|i| syn::parse_str(&format!("a_{}", i)))
        .collect::<std::result::Result<Vec<TokenStream>, _>>()?;
    let (return_type, return_conversion) = generate_conversion_for_return(
        &extract_return_type(&method.fn_decl.output), conv_map, "_ret"
    )?;
    Ok(quote!{
        @staticmethod def #static_method_name(
            #( #args_names: #args_types ),*
        ) -> cpython::PyResult<#return_type> {
            let _ret = super::#static_method_rust_path(
                #( #args_convertions ),*
            );
            Ok(#return_conversion)
        }
    })
}

fn has_any_methods(class: &ForeignerClassInfo) -> bool {
    class.methods
        .iter()
        .any(|m| if let MethodVariant::Method(_) = m.variant { true } else { false })
}

fn generate_conversion_for_argument(rust_type: &RustType, conv_map: &TypeMap, arg_name: &str) -> Result<(Type, TokenStream)> {
    let arg_name_ident: TokenStream = syn::parse_str(arg_name)?;
    if is_cpython_supported_type(rust_type) {
        Ok((rust_type.ty.clone(), arg_name_ident))
    } else if let Some(foreign_class) = conv_map.find_foreigner_class_with_such_self_type(&rust_type.ty, true) {
        let class_name = foreign_class.name.to_string();
        let py_mod: Ident = syn::parse_str(&py_wrapper_mod_name(&class_name))?;
        let py_type: Type = syn::parse_str(&format!("&super::{}::{}", py_wrapper_mod_name(&class_name), &class_name))?;
        let self_conversion_code = if let Type::Reference(ref reference) = rust_type.ty {
            if reference.mutability.is_some() {
                quote!{
                    &mut *super::#py_mod::rust_instance(#arg_name_ident, py).write().unwrap()
                }
            } else {
                quote!{
                    &*super::#py_mod::rust_instance(#arg_name_ident, py).read().unwrap()
                }
            }
        } else {
            unimplemented!("Passing object by value not implemented yet")
        };
        Ok((py_type, self_conversion_code))
    } else {
        unimplemented!("other arg");
    }
}

fn generate_conversion_for_return(rust_type: &RustType, conv_map: &TypeMap, ret_name: &str) -> Result<(Type, TokenStream)> {
    if rust_type.normalized_name == "( )" {
        Ok((parse_type!(cpython::PyObject), quote!(py.None())))
    } else if is_cpython_supported_type(rust_type) {
        Ok((rust_type.ty.clone(), syn::parse_str(ret_name)?))
    } else if let Some(foreing_class) = conv_map.find_foreigner_class_with_such_this_type(&rust_type.ty) {
        unimplemented!();
    } else {
        unimplemented!();
    }
}

fn is_cpython_supported_type(rust_type: &RustType) -> bool {
    let primitive_types = [
        "bool", "i8", "i16", "i32", "i64", "isize", "u8", "u16", "u32", "u64", "usize",
        "f32", "f64", "String", "& str"
    ];
    primitive_types.contains(&rust_type.normalized_name.as_str())
}

fn extract_return_type(syn_return_type: &syn::ReturnType) -> RustType {
    match syn_return_type {
        syn::ReturnType::Default => {
                let mut ty: Type = parse_type! { () };
                ast::change_span(&mut ty, syn_return_type.span());
                ty.into()
        },
        syn::ReturnType::Type(_, ref ty) => {
            let ty: Type = *ty.clone();
            ty.into()
        },
    }
}

fn py_wrapper_mod_name(type_name: &str) -> String {
    format!(
        "py_{}",
        type_name.to_snake_case()
    )
}
