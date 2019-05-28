use crate::typemap::ast;
use crate::typemap::ty::RustType;
use crate::{
    error::Result, DiagnosticError, ForeignEnumInfo, ForeignInterface, ForeignerClassInfo,
    ForeignerMethod, LanguageGenerator, MethodVariant, PythonConfig, SelfTypeVariant, TypeMap,
};
use heck::SnakeCase;
use proc_macro2::{Span, TokenStream};
use quote::quote;
use quote::ToTokens;
use std::ops::Deref;
use syn::parse_quote;
use syn::{Ident, Type};

impl LanguageGenerator for PythonConfig {
    fn register_class(&self, conv_map: &mut TypeMap, class: &ForeignerClassInfo) -> Result<()> {
        if let Some(ref ty) = class.self_type {
            conv_map.find_or_alloc_rust_type(ty);
        }
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
        let (rust_instance_field, rust_instance_getter) =
            generate_rust_instance_field_and_methods(class, conv_map)?;
        let methods_code = class
            .methods
            .iter()
            .map(|m| generate_method_code(class, m, conv_map))
            .collect::<Result<Vec<_>>>()?;
        let docstring = class.doc_comments.as_slice().join("\n");
        let class_code = quote! {
            mod #wrapper_mod_name {
                use super::*;
                #[allow(unused)]
                py_class!(pub class #class_name |py| {
                    static __doc__  = #docstring;

                    #rust_instance_field

                    #( #methods_code )*
                });

                #rust_instance_getter
            }
        };

        self.module_initialization_code.borrow_mut().push(quote! {
            {
                m.add_class::<#wrapper_mod_name::#class_name>(py)?;
            }
        });
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
        let rust_variants = enum_info
            .items
            .iter()
            .map(|item| &item.rust_name)
            .collect::<Vec<_>>();
        let rust_variants_ref_1 = &rust_variants;
        let rust_variants_ref_2 = &rust_variants;
        let enum_name_str = enum_name.to_string();
        let docstring = enum_info.doc_comments.as_slice().join("\n");
        let class_code = quote! {
            mod #wrapper_mod_name {
                py_class!(pub class #enum_name |py| {
                    static __doc__  = #docstring;
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
        let module_init = syn::parse_str::<syn::Ident>(&format!("init{}", &self.module_name))?;
        let module_py_init =
            syn::parse_str::<syn::Ident>(&format!("PyInit_{}", &self.module_name))?;
        let registration_code = vec![quote! {
            mod py_error {
                py_exception!(#module_name, Error);
            }

            py_module_initializer!(#module_name, #module_init, #module_py_init, |py, m| {
                m.add(py, "Error", py_error::Error::type_object(py))?;
                #(#module_initialization_code)*
                Ok(())
            });
        }];
        Ok(registration_code)
    }
}

fn generate_rust_instance_field_and_methods(
    class: &ForeignerClassInfo,
    conv_map: &mut TypeMap,
) -> Result<(TokenStream, TokenStream)> {
    if let Some(ref rust_self_type) = class.self_type {
        let storage_smart_pointer = storage_smart_pointer_for_class(class, conv_map)?;
        if storage_smart_pointer.inner_ty.normalized_name
            != conv_map.find_or_alloc_rust_type(rust_self_type).normalized_name
        {
            return Err(DiagnosticError::new(class.span(), "Self type and (inner) type returned from constructor doesn't match"));
        }
        let storage_type = wrap_type_for_class(&rust_self_type, storage_smart_pointer.pointer_type);
        let storage_type_ref = &storage_type;
        let class_name = &class.name;
        Ok((
            quote! {
                data rust_instance: #storage_type_ref;
            },
            // For some reason, rust-cpython generates private `rust_instance` getter method.
            // As a workaround, we add public function in the same module, that gets `rust_instance`.
            // The same goes for `create_instance
            quote! {
                pub fn rust_instance<'a>(class: &'a #class_name, py: cpython::Python<'a>) -> &'a #storage_type_ref {
                    class.rust_instance(py)
                }

                pub fn create_instance(py: cpython::Python, instance: #storage_type_ref) -> cpython::PyResult<#class_name> {
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
    conv_map: &mut TypeMap,
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
                &conv_map.find_or_alloc_rust_type(ast::fn_arg_type(a)),
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
    let (return_type, rust_call_with_return_conversion) = generate_conversion_for_return(
        &conv_map.find_or_alloc_rust_type(&extract_return_type(&method.fn_decl.output)),
        method.span(),
        conv_map,
        quote! {
            #method_rust_path(#( #args_convertions ),*)
        },
    )?;
    Ok(quote! {
        #attribute def #method_name(
            #( #args_list ),*
        ) -> cpython::PyResult<#return_type> {
            #[allow(unused)]
            use super::*;
            Ok(#rust_call_with_return_conversion)
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
    conv_map: &mut TypeMap,
) -> Result<Option<TokenStream>> {
    if let MethodVariant::Method(self_variant) = method.variant {
        let self_type = &class.self_type.as_ref().ok_or_else(|| {
            DiagnosticError::new(
                class.span(),
                "Class have non-static methods, but no self_type",
            )
        })?;
        let self_type_ty = match self_variant {
            SelfTypeVariant::Rptr => parse_type! {&#self_type},
            SelfTypeVariant::RptrMut => parse_type! {&mut #self_type},
            _ => parse_type! {#self_type},
        };
        Ok(Some(
            generate_conversion_for_argument(
                &conv_map.find_or_alloc_rust_type(&self_type_ty),
                method.span(),
                conv_map,
                "self",
            )?
            .1,
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
    conv_map: &mut TypeMap,
    arg_name: &str,
) -> Result<(Type, TokenStream)> {
    let arg_name_ident: TokenStream = syn::parse_str(arg_name)?;
    if is_cpython_supported_type(rust_type) {
        Ok((rust_type.ty.clone(), arg_name_ident))
    } else if let Some((ty, conversion)) = if_exported_class_generate_argument_conversion(
        rust_type,
        conv_map,
        &arg_name_ident,
        method_span,
    )? {
        Ok((ty, conversion))
    } else if let Some(enum_info) = conv_map.is_this_exported_enum(&rust_type) {
        let enum_py_mod: Ident = syn::parse_str(&py_wrapper_mod_name(&enum_info.name.to_string()))?;
        Ok((
            parse_type!(u32),
            quote! {
                super::#enum_py_mod::from_u32(py, #arg_name_ident)?
            },
        ))
    } else if let Some(inner) = ast::if_option_return_some_type(&rust_type) {
        let (inner_py_type, inner_conversion) = generate_conversion_for_argument(
            &conv_map.find_or_alloc_rust_type(&inner),
            method_span,
            conv_map,
            "inner",
        )?;
        Ok((
            parse_type!(Option<#inner_py_type>),
            quote! {
                match #arg_name_ident {
                    Some(inner) => Some(#inner_conversion),
                    None => None,
                }
            },
        ))
    } else if let Some(inner) = ast::if_type_slice_return_elem_type(&rust_type.ty, false) {
        let (inner_py_type, inner_conversion) = generate_conversion_for_argument(
            &conv_map.find_or_alloc_rust_type(&inner),
            method_span,
            conv_map,
            "inner",
        )?;
        Ok((
            parse_type!(Vec<#inner_py_type>),
            quote! {
                &#arg_name_ident.into_iter().map(|inner| Ok(#inner_conversion)).collect::<cpython::PyResult<Vec<_>>>()?
            },
        ))
    } else if let Some(inner) = ast::if_vec_return_elem_type(&rust_type) {
        let (inner_py_type, inner_conversion) = generate_conversion_for_argument(
            &conv_map.find_or_alloc_rust_type(&inner),
            method_span,
            conv_map,
            "inner",
        )?;
        Ok((
            parse_type!(Vec<#inner_py_type>),
            quote! {
                #arg_name_ident.into_iter().map(|inner| Ok(#inner_conversion)).collect::<cpython::PyResult<Vec<_>>>()?
            },
        ))
    } else if let Type::Reference(ref inner) = rust_type.ty {
        if inner.mutability.is_some() {
            return Err(DiagnosticError::new(
                method_span,
                "mutable reference is only supported for exported class types",
            ));
        }
        let (inner_py_type, inner_conversion) = generate_conversion_for_argument(
            &conv_map.find_or_alloc_rust_type(&inner.elem.deref()),
            method_span,
            conv_map,
            arg_name,
        )?;
        Ok((
            parse_type!(#inner_py_type),
            quote! {
                &#inner_conversion
            },
        ))
    } else {
        Err(DiagnosticError::new(
            method_span,
            format!("Unsupported argument type: {:?}", rust_type.normalized_name),
        ))
    }
}

fn generate_conversion_for_return(
    rust_type: &RustType,
    method_span: Span,
    conv_map: &mut TypeMap,
    rust_call: TokenStream,
) -> Result<(Type, TokenStream)> {
    if rust_type.normalized_name == "( )" {
        Ok((
            parse_type!(cpython::PyObject),
            quote! {
                {#rust_call; py.None()}
            },
        ))
    } else if is_cpython_supported_type(rust_type) {
        Ok((rust_type.ty.clone(), rust_call))
    } else if let Some((ty, conversion)) =
        if_exported_class_generate_return_conversion(&rust_type, conv_map, &rust_call, method_span)?
    {
        Ok((ty, conversion))
    } else if conv_map.is_this_exported_enum(&rust_type).is_some() {
        Ok((
            parse_type!(u32),
            quote! {
                #rust_call as u32
            },
        ))
    } else if let Some(inner) = ast::if_option_return_some_type(&rust_type) {
        let (inner_py_type, inner_conversion) = generate_conversion_for_return(
            &conv_map.find_or_alloc_rust_type(&inner),
            method_span,
            conv_map,
            quote! {inner},
        )?;
        Ok((
            parse_type!(Option<#inner_py_type>),
            quote! {
                match #rust_call {
                    Some(inner) => Some(#inner_conversion),
                    None => None
                }
            },
        ))
    } else if let Some(inner) = ast::if_type_slice_return_elem_type(&rust_type.ty, false) {
        let (inner_py_type, inner_conversion) = generate_conversion_for_return(
            &conv_map.find_or_alloc_rust_type(&inner),
            method_span,
            conv_map,
            quote! {inner},
        )?;
        Ok((
            parse_type!(Vec<#inner_py_type>),
            quote! {
                #rust_call.iter().cloned().map(|inner| Ok(#inner_conversion)).collect::<cpython::PyResult<Vec<_>>>()?
            },
        ))
    } else if let Some(inner) = ast::if_vec_return_elem_type(&rust_type) {
        let (inner_py_type, inner_conversion) = generate_conversion_for_return(
            &conv_map.find_or_alloc_rust_type(&inner),
            method_span,
            conv_map,
            quote! {inner},
        )?;
        Ok((
            parse_type!(Vec<#inner_py_type>),
            quote! {
                #rust_call.into_iter().map(|inner| Ok(#inner_conversion)).collect::<cpython::PyResult<Vec<_>>>()?
            },
        ))
    } else if let Some((inner_ok, _inner_err)) = ast::if_result_return_ok_err_types(&rust_type) {
        let (inner_py_type, inner_conversion) = generate_conversion_for_return(
            &conv_map.find_or_alloc_rust_type(&inner_ok),
            method_span,
            conv_map,
            quote! {ok_inner},
        )?;
        Ok((
            parse_type!(#inner_py_type),
            quote! {
                match #rust_call {
                    Ok(ok_inner) => #inner_conversion,
                    Err(err_inner) => return Err(cpython::PyErr::new::<super::py_error::Error, _>(py, err_inner.to_string())),
                }
            },
        ))
    } else if let Type::Reference(ref inner) = rust_type.ty {
        generate_conversion_for_return(
            &conv_map.find_or_alloc_rust_type(&inner.elem.deref()),
            method_span,
            conv_map,
            quote! {(#rust_call).clone()},
        )
    } else if let Type::Tuple(ref tuple) = rust_type.ty {
        let (types, conversions): (Vec<_>, Vec<_>) = tuple.elems.iter().enumerate().map(|(i, ty)| {
            generate_conversion_for_return(
                &conv_map.find_or_alloc_rust_type(ty),
                method_span,
                conv_map,
                quote!{tuple.#i}
            )
        }).collect::<Result<Vec<_>>>()?.into_iter().unzip();
        Ok((
            parse_type!{( #( #types, )* )},
            quote!{
                {
                    let tuple = #rust_call;
                    (
                        #( #conversions, )*
                    )
                }
            }
        ))
    } else {
        Err(DiagnosticError::new(
            method_span,
            format!("Unsupported return type: {:?}", rust_type.normalized_name),
        ))
    }
}

fn is_cpython_supported_type(rust_type: &RustType) -> bool {
    let primitive_types = [
        "bool", "i8", "i16", "i32", "i64", "isize", "u8", "u16", "u32", "u64", "usize", "f32",
        "f64", "String", "& str",
    ];
    primitive_types.contains(&rust_type.normalized_name.as_str())
}

fn extract_return_type(syn_return_type: &syn::ReturnType) -> Type {
    match syn_return_type {
        syn::ReturnType::Default => {
            parse_type! { () }
        }
        syn::ReturnType::Type(_, ref ty) => ty.deref().clone(),
    }
}

fn py_wrapper_mod_name(type_name: &str) -> String {
    format!("py_{}", type_name.to_snake_case())
}

// `rust_cpython` provides access only to non-mutable reference of the wrapped Rust object.
// What's more `rust_cpytho`n requires the object to be `Send + 'static`, because Python VM
// can move it between threads without any control from Rust.
// As a result, we need to wrap the object in `RwLock` or `Mutex`, to provide mutability.
// By default, `RwLock` is used.
// This could be overriden by the smart pointer returned from the constructor.
// Following smart pointers are supported:
// - `Arc<RwLock<T>>`: wrapped Rust object is mutable and can be shared between Rust and Python,
// - `RwLock<T>`: wrapped Rust object is mutable, but only Python owns it,
// - `Arc<T>`: wrapped Rust object is immutable and can be shared between Rust and Python,
// - `Box<T>`: wrapped Rust object is immutable and only Python owns it,
// Note, that `Rc` is NOT supported. This is because it is not `Send`.
// `RefCell` theoretically could be supported, but generated Python API would be thread unsafe
// (it is `Send`, but no `Sync`), so it is intentionally omitted.
#[derive(Debug, Clone, Copy, PartialEq)]
enum PointerType {
    ArcRwLock,
    RwLock,
    Arc,
    Box,
    None,
}

impl PointerType {
    fn is_shared(self) -> bool {
        self == PointerType::Arc || self == PointerType::ArcRwLock
    }
}

struct SmartPointerInfo {
    pointer_type: PointerType,
    inner_ty: RustType,
}

impl SmartPointerInfo {
    fn new(
        pointer_type: PointerType,
        inner_ty: RustType,
    ) -> SmartPointerInfo {
        SmartPointerInfo {
            pointer_type,
            inner_ty,
        }
    }
}

fn smart_pointer(rust_type: &RustType, conv_map: &mut TypeMap) -> SmartPointerInfo {
    if let Some(inner_ty) = ast::check_if_smart_pointer_return_inner_type(rust_type, "Arc") {
        let rust_inner_ty = conv_map.find_or_alloc_rust_type(&inner_ty);
        if let Some(inner_inner_ty) =
            ast::check_if_smart_pointer_return_inner_type(&rust_inner_ty, "RwLock")
        {
            SmartPointerInfo::new(
                PointerType::ArcRwLock,
                conv_map.find_or_alloc_rust_type(&inner_inner_ty),
            )
        } else {
            SmartPointerInfo::new(PointerType::Arc, rust_inner_ty)
        }
    } else if let Some(inner_ty) =
        ast::check_if_smart_pointer_return_inner_type(&rust_type, "RwLock")
    {
        SmartPointerInfo::new(
            PointerType::RwLock,
            conv_map.find_or_alloc_rust_type(&inner_ty),
        )
    } else if let Some(inner_ty) = ast::check_if_smart_pointer_return_inner_type(&rust_type, "Box")
    {
        SmartPointerInfo::new(
            PointerType::Box,
            conv_map.find_or_alloc_rust_type(&inner_ty),
        )
    } else {
        SmartPointerInfo::new(PointerType::None, rust_type.clone())
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Reference {
    None,
    Ref,
    MutRef,
}

fn get_reference_info_and_inner_type(
    rust_type: &RustType,
    conv_map: &mut TypeMap,
) -> (Reference, RustType) {
    if let Type::Reference(ref reference) = rust_type.ty {
        if reference.mutability.is_some() {
            (
                Reference::MutRef,
                conv_map.find_or_alloc_rust_type(&*reference.elem),
            )
        } else {
            (
                Reference::Ref,
                conv_map.find_or_alloc_rust_type(&*reference.elem),
            )
        }
    } else {
        (Reference::None, rust_type.clone())
    }
}

fn if_exported_class_generate_return_conversion(
    rust_type: &RustType,
    conv_map: &mut TypeMap,
    rust_call: &TokenStream,
    method_span: Span,
) -> Result<Option<(Type, TokenStream)>> {
    let (reference_type, rust_type_unref) = get_reference_info_and_inner_type(rust_type, conv_map);
    let smart_pointer_info = smart_pointer(&rust_type_unref, conv_map);
    let class = match conv_map
        .find_foreigner_class_with_such_self_type(&smart_pointer_info.inner_ty, false)
    {
        Some(fc) => fc.clone(),
        None => return Ok(None),
    };
    let class_smart_pointer = storage_smart_pointer_for_class(&class, conv_map)?;
    let rust_call_with_deref = if reference_type != Reference::None {
        if smart_pointer_info.pointer_type == PointerType::RwLock {
            return Err(DiagnosticError::new(
                method_span,
               "Returning a rust object into python by reference is not safe, so the copy of the object needs to be make.\
However, `RwLock` doesn't implement `Clone`, so it can't be returned by reference."
            ));
        } else if class.copy_derived || smart_pointer_info.pointer_type.is_shared() {
            quote! {
                ((#rust_call).clone())
            }
        } else {
            return Err(DiagnosticError::new(
                method_span,
                "Returning a rust object into python by reference is not safe, so the copy of the object needs to be make. \
Thus, the returned type must marked with `#[derive(Copy)]` inside its `foreigner_class` macro. \
(Note, that the corresponding rust type doesn't actually need to be `Copy`. `Clone` is sufficient)"
            ));
        }
    } else {
        rust_call.clone()
    };
    let class_name = &class.name;
    let py_mod: Ident = syn::parse_str(&py_wrapper_mod_name(&class_name.to_string()))?;
    let rust_call_with_wrapper = match class_smart_pointer.pointer_type {
        PointerType::RwLock => generate_wrapper_constructor_for_rwlock(
            &class,
            &smart_pointer_info,
            rust_call_with_deref,
            method_span,
        )?,
        PointerType::ArcRwLock => generate_wrapper_constructor_for_arc_rwlock(
            &class,
            &smart_pointer_info,
            rust_call_with_deref,
            method_span,
        )?,
        PointerType::Arc => generate_wrapper_constructor_for_arc(
            &class,
            &smart_pointer_info,
            rust_call_with_deref,
            method_span,
        )?,
        PointerType::Box => generate_wrapper_constructor_for_box(
            &class,
            &smart_pointer_info,
            rust_call_with_deref,
            method_span,
        )?,
        _ => unreachable!("`PointerType::None` as class storage pointer"),
    };
    let conversion = quote! {
        super::#py_mod::create_instance(py, #rust_call_with_wrapper)?
    };
    Ok(Some((parse_type!(super::#py_mod::#class_name), conversion)))
}

fn generate_wrapper_constructor_for_rwlock(
    class: &ForeignerClassInfo,
    returned_smart_pointer: &SmartPointerInfo,
    rust_call: TokenStream,
    method_span: Span,
) -> Result<TokenStream> {
    match returned_smart_pointer.pointer_type {
        PointerType::RwLock => Ok(rust_call),
        PointerType::None => Ok(quote! {std::sync::RwLock::new(#rust_call)}),
        _ => Err(DiagnosticError::new(
            method_span,
            format!(
                "Unsupported conversion for smart pointer. \
Foreigner class {} is stored as `RwLock` and can be returned eiter as `RwLock` or bare type",
                class.name
            ),
        )),
    }
}

fn generate_wrapper_constructor_for_arc_rwlock(
    class: &ForeignerClassInfo,
    returned_smart_pointer: &SmartPointerInfo,
    rust_call: TokenStream,
    method_span: Span,
) -> Result<TokenStream> {
    match returned_smart_pointer.pointer_type {
        PointerType::ArcRwLock => Ok(rust_call),
        _ => Err(DiagnosticError::new(
            method_span,
            format!(
                "Unsupported conversion for smart pointer. \
Foreigner class {} is stored as `Arc<RwLock<T>>`, so it is intended for sharing between Rust and Python.\
Thus, it must always be returned from Rust literally by `Arc<RwLock<T>>` \
(or reference to `Arc<RwLock<T>>`) for any sharing to occur.",
                class.name
            ),
        ))
    }
}

fn generate_wrapper_constructor_for_arc(
    class: &ForeignerClassInfo,
    returned_smart_pointer: &SmartPointerInfo,
    rust_call: TokenStream,
    method_span: Span,
) -> Result<TokenStream> {
    match returned_smart_pointer.pointer_type {
        PointerType::Arc => Ok(rust_call),
        _ => Err(DiagnosticError::new(
            method_span,
            format!(
                "Unsupported conversion for smart pointer. \
Foreigner class {} is stored as `Arc<T>`, so it is intended for sharing between Rust and Python.\
Thus, it must always be returned Rust literally by `Arc<T>` \
(or reference to `Arc<T>`) for any sharing to occur.",
                class.name
            ),
        )),
    }
}

fn generate_wrapper_constructor_for_box(
    class: &ForeignerClassInfo,
    returned_smart_pointer: &SmartPointerInfo,
    rust_call: TokenStream,
    method_span: Span,
) -> Result<TokenStream> {
    match returned_smart_pointer.pointer_type {
        PointerType::None => Ok(rust_call),
        PointerType::Box => Ok(quote! {(*#rust_call)}),
        _ => Err(DiagnosticError::new(
            method_span,
            format!(
                "Unsupported conversion for smart pointer. \
Foreigner class {} is stored as `Box`, and can be returned eiter as `Box` or bare type",
                class.name
            ),
        )),
    }
}

fn if_exported_class_generate_argument_conversion(
    rust_type: &RustType,
    conv_map: &mut TypeMap,
    arg_name_ident: &TokenStream,
    method_span: Span,
) -> Result<Option<(Type, TokenStream)>> {
    let (reference_type, rust_type_unref) = get_reference_info_and_inner_type(rust_type, conv_map);
    let smart_pointer_info = smart_pointer(&rust_type_unref, conv_map);
    let class = match conv_map
        .find_foreigner_class_with_such_self_type(&smart_pointer_info.inner_ty, false)
    {
        Some(fc) => fc.clone(),
        None => return Ok(None),
    };
    let class_smart_pointer = storage_smart_pointer_for_class(&class, conv_map)?;
    let class_name = class.name.to_string();
    let py_mod_str = py_wrapper_mod_name(&class_name);
    let py_mod: Ident = syn::parse_str(&py_mod_str)?;
    let py_type: Type = syn::parse_str(&format!("&super::{}::{}", &py_mod_str, &class_name))?;

    let rust_instance_code = quote! {
        super::#py_mod::rust_instance(#arg_name_ident, py)
    };
    let deref_code = match class_smart_pointer.pointer_type {
        PointerType::ArcRwLock => generate_deref_for_arc_rwlock(
            &class,
            smart_pointer_info.pointer_type,
            reference_type,
            rust_instance_code,
            method_span,
        )?,
        PointerType::Arc => generate_deref_for_arc(
            &class,
            smart_pointer_info.pointer_type,
            reference_type,
            rust_instance_code,
            method_span,
        )?,
        PointerType::RwLock => generate_deref_for_rwlock(
            &class,
            smart_pointer_info.pointer_type,
            reference_type,
            rust_instance_code,
            method_span,
        )?,
        PointerType::Box => generate_deref_for_box(
            &class,
            smart_pointer_info.pointer_type,
            reference_type,
            rust_instance_code,
            method_span,
        )?,
        _ => unreachable!("Class stored as None"),
    };

    Ok(Some((py_type, deref_code)))
}

fn generate_deref_for_rwlock(
    class: &ForeignerClassInfo,
    arg_smart_pointer: PointerType,
    arg_reference: Reference,
    rust_instance_code: TokenStream,
    method_span: Span,
) -> Result<TokenStream> {
    match arg_smart_pointer {
        PointerType::None => match arg_reference {
            Reference::MutRef => Ok(quote!{(&mut *#rust_instance_code.write().unwrap())}),
            Reference::Ref => Ok(quote!{(&*#rust_instance_code.read().unwrap())}),
            Reference::None => append_clone_if_supported(class, quote!{*#rust_instance_code.read().unwrap()}, method_span),
        },
        PointerType::RwLock => match arg_reference {
            Reference::Ref => Ok(rust_instance_code),
            _ => Err(DiagnosticError::new(
                method_span,
                "RwLock can be passed to function only by const reference `RwLock`",
            ))
        }
        _ => Err(DiagnosticError::new(
            method_span,
            format!(
                "Unsupported conversion for smart pointer. \
Foreigner class {} is stored as `RwLock` and can be passed to function either as `RwLock` reference or bare type",
                class.name
            ),
        ))
    }
}

fn generate_deref_for_arc_rwlock(
    class: &ForeignerClassInfo,
    arg_smart_pointer: PointerType,
    arg_reference: Reference,
    rust_instance_code: TokenStream,
    method_span: Span,
) -> Result<TokenStream> {
    match arg_smart_pointer {
        PointerType::None => match arg_reference {
            Reference::MutRef => Ok(quote!{(&mut *#rust_instance_code.write().unwrap())}),
            Reference::Ref => Ok(quote!{(&*#rust_instance_code.read().unwrap())}),
            Reference::None => append_clone_if_supported(class, quote!{*#rust_instance_code.read().unwrap()}, method_span),
        },
        PointerType::ArcRwLock => match arg_reference {
            Reference::Ref => Ok(rust_instance_code),
            Reference::None => Ok(quote!{#rust_instance_code.clone()}),
            _ => Err(DiagnosticError::new(
                method_span,
                "Arc<RwLock<T>> can't be passed to function by mut reference. It doesn't make sense anyway.",
            ))
        }
        _ => Err(DiagnosticError::new(
            method_span,
            format!(
                "Unsupported conversion for smart pointer. \
Foreigner class {} is stored as `Arc<RwLock<T>>` and can be passed to function either as `Arc<RwLock<T>>` or bare type",
                class.name
            ),
        ))
    }
}

fn generate_deref_for_arc(
    class: &ForeignerClassInfo,
    arg_smart_pointer: PointerType,
    arg_reference: Reference,
    rust_instance_code: TokenStream,
    method_span: Span,
) -> Result<TokenStream> {
    match arg_smart_pointer {
        PointerType::None => match arg_reference {
            Reference::Ref => Ok(quote!{(&*#rust_instance_code)}),
            Reference::None => append_clone_if_supported(class, quote!{*#rust_instance_code}, method_span),
            Reference::MutRef => Err(DiagnosticError::new(
                method_span,
                "Object is stored in `Arc`, so it is immutable. If you need mutability, use Arc<RwLock<T>> for constructor type",
            ))
        },
        PointerType::Arc => match arg_reference {
            Reference::Ref => Ok(rust_instance_code),
            Reference::None => Ok(quote!{#rust_instance_code.clone()}),
            _ => Err(DiagnosticError::new(
                method_span,
                "`Arc` can't be passed to function by mut reference. It's immutable",
            ))
        }
        _ => Err(DiagnosticError::new(
            method_span,
            format!(
                "Unsupported conversion for smart pointer. \
Foreigner class {} is stored as `Arc` and can be passed to function either as `Arc` or a bare type",
                class.name
            ),
        ))
    }
}

fn generate_deref_for_box(
    class: &ForeignerClassInfo,
    arg_smart_pointer: PointerType,
    arg_reference: Reference,
    rust_instance_code: TokenStream,
    method_span: Span,
) -> Result<TokenStream> {
    match arg_smart_pointer {
        PointerType::None => match arg_reference {
            Reference::Ref => Ok(rust_instance_code),
            Reference::None => append_clone_if_supported(class, rust_instance_code, method_span),
            Reference::MutRef => Err(DiagnosticError::new(
                method_span,
                "Object is stored in `Box`, so it is immutable. If you need mutability, use `RwLock` for constructor type",
            ))
        },
        _ => Err(DiagnosticError::new(
            method_span,
            format!(
                "Unsupported conversion for smart pointer. \
Foreigner class {} is stored as `Box` and can be passed to function anly as a bare type",
                class.name
            ),
        ))
    }
}

fn append_clone_if_supported(
    class: &ForeignerClassInfo,
    rust_instance_code: TokenStream,
    method_span: Span,
) -> Result<TokenStream> {
    if class.copy_derived {
        Ok(quote!((#rust_instance_code).clone()))
    } else {
        Err(DiagnosticError::new(
            method_span,
            "Passing object by value requires that it is marked with `#[derive(Copy)]` inside its `foreigner_class` macro. \
    (Note, that the corresponding rust type doesn't actually need to be `Copy`. `Clone` is sufficient)"
        ))
    }
}

fn storage_smart_pointer_for_class(
    class: &ForeignerClassInfo,
    conv_map: &mut TypeMap,
) -> Result<SmartPointerInfo> {
    if let Some(ref constructor_ret_type_ty) = class.constructor_ret_type {
        let constructor_ret_rust_type = conv_map.find_or_alloc_rust_type(constructor_ret_type_ty);
        let pointer = smart_pointer(&constructor_ret_rust_type, conv_map);
        match pointer.pointer_type {
            // Default wrapper type for storage is `RwLock`.
            PointerType::None => Ok(SmartPointerInfo::new(PointerType::RwLock, pointer.inner_ty)),
            _ => Ok(pointer),
        }
    } else {
        Err(DiagnosticError::new(
            class.span(),
            "Class doesn't define a type returned from constructor, nor self_type, but is not static"
        ))
    }
}

fn wrap_type_for_class(self_type: &Type, storage_pointer: PointerType) -> TokenStream {
    match storage_pointer {
        PointerType::ArcRwLock => quote!{std::sync::Arc<std::sync::RwLock<super::#self_type>>},
        PointerType::Arc => quote!{std::sync::Arc<super::#self_type>},
        PointerType::RwLock => quote!{std::sync::RwLock<super::#self_type>},
        PointerType::Box => quote!{super::#self_type},
        PointerType::None => unreachable!("None pointer for object storage"),
    }
}
