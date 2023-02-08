use crate::typemap::ty::RustType;
use crate::{
    error::Result,
    extension::{ClassExtHandlers, MethodExtHandlers},
    source_registry::SourceId,
    typemap::{
        ast::{ForeignTypeName, GenericTypeConv},
        ty::ForeignTypeS,
        TypeConvCode,
    },
    types::{
        ForeignClassInfo, ForeignEnumInfo, ForeignInterface, ForeignMethod, ItemToExpand,
        MethodVariant, SelfTypeVariant,
    },
    DiagnosticError, LanguageGenerator, PythonConfig, SourceCode, TypeMap,
};
use crate::{extension::ExtHandlers, typemap::ast};
use heck::ToSnakeCase;
use proc_macro2::{Span, TokenStream};
use quote::quote;
use quote::ToTokens;
use std::ops::Deref;
use syn::parse_quote;
use syn::{Ident, Type};

const ENUM_TRAIT_NAME: &str = "SwigForeignEnum";

impl LanguageGenerator for PythonConfig {
    fn expand_items(
        &self,
        conv_map: &mut TypeMap,
        _pointer_target_width: usize,
        _code: &[SourceCode],
        items: Vec<ItemToExpand>,
        _remove_not_generated_files: bool,
        ext_handlers: ExtHandlers,
    ) -> Result<Vec<TokenStream>> {
        for item in &items {
            if let ItemToExpand::Class(ref fclass) = item {
                self.register_class(conv_map, fclass)?;
            }
        }
        let mut code = Vec::with_capacity(items.len());
        let mut module_initialization = Vec::with_capacity(items.len());
        for item in items {
            let (class_code, initialization) = match item {
                ItemToExpand::Class(fclass) => self.generate_class(
                    conv_map,
                    &fclass,
                    ext_handlers.class_ext_handlers,
                    ext_handlers.method_ext_handlers,
                )?,
                ItemToExpand::Enum(fenum) => self.generate_enum(conv_map, &fenum)?,
                ItemToExpand::Interface(finterface) => {
                    self.generate_interface(conv_map, &finterface)?
                }
            };
            code.push(class_code);
            module_initialization.push(initialization);
        }
        code.push(self.generate_module_initialization(&module_initialization)?);
        Ok(code)
    }
}

impl PythonConfig {
    fn register_class(&self, conv_map: &mut TypeMap, class: &ForeignClassInfo) -> Result<()> {
        if let Some(ref self_desc) = class.self_desc {
            conv_map.find_or_alloc_rust_type(&self_desc.self_type, class.src_id);
        }
        Ok(())
    }

    /// Generate class code and module initialization code for this class.
    fn generate_class(
        &self,
        conv_map: &mut TypeMap,
        class: &ForeignClassInfo,
        class_ext_handlers: &ClassExtHandlers,
        method_ext_handlers: &MethodExtHandlers,
    ) -> Result<(TokenStream, TokenStream)> {
        if !class_ext_handlers.is_empty() || !method_ext_handlers.is_empty() {
            return Err(DiagnosticError::new(
                class.src_id,
                class.span(),
                format!(
                    "class {}: has attributes, this is not supported for python",
                    class.name
                ),
            ));
        }

        let class_name = &class.name;
        let wrapper_mod_name =
            parse::<Ident>(&py_wrapper_mod_name(&class_name.to_string()), class.src_id)?;
        let (rust_instance_field, rust_instance_getter) =
            generate_rust_instance_field_and_methods(class, conv_map)?;
        let methods_code = class
            .methods
            .iter()
            .map(|m| generate_method_code(class, m, conv_map))
            .collect::<Result<Vec<_>>>()?;
        let mut doc_comments = class.doc_comments.clone();
        if let Some(constructor) = class
            .methods
            .iter()
            .find(|m| m.variant == MethodVariant::Constructor)
        {
            // Python API doesn't allow to add docstring to the special methods (slots),
            // including __new__ and __init__.
            // The convention is, to document the constructor in class's docstring.
            doc_comments.push("".to_owned());
            doc_comments.extend_from_slice(&constructor.doc_comments);
        }
        let docstring = doc_comments.as_slice().join("\n");
        let class_code = quote! {
            mod #wrapper_mod_name {
                use super::*;
                #[allow(unused)]
                cpython::py_class!(pub class #class_name |py| {
                    static __doc__  = #docstring;

                    #rust_instance_field

                    #( #methods_code )*
                });

                #rust_instance_getter
            }
        };

        let module_initialization_code = quote! {
            {
                m.add_class::<#wrapper_mod_name::#class_name>(py)?;
            }
        };
        Ok((class_code, module_initialization_code))
    }

    fn generate_enum(
        &self,
        conv_map: &mut TypeMap,
        enum_info: &ForeignEnumInfo,
    ) -> Result<(TokenStream, TokenStream)> {
        let enum_name = &enum_info.name;
        let wrapper_mod_name = parse::<Ident>(
            &py_wrapper_mod_name(&enum_name.to_string()),
            enum_info.src_id,
        )?;
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
                cpython::py_class!(pub class #enum_name |py| {
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
        let enum_ti: Type =
            ast::parse_ty_with_given_span(&enum_name.to_string(), enum_info.name.span())
                .map_err(|err| DiagnosticError::from_syn_err(enum_info.src_id, err))?;
        conv_map.find_or_alloc_rust_type_that_implements(
            &enum_ti,
            &[ENUM_TRAIT_NAME],
            enum_info.src_id,
        );
        let enum_ftype = ForeignTypeS {
            name: ForeignTypeName::new(
                enum_info.name.to_string(),
                (enum_info.src_id, enum_info.name.span()),
            ),
            provided_by_module: vec![],
            into_from_rust: None,
            from_into_rust: None,
        };
        conv_map.alloc_foreign_type(enum_ftype)?;

        let module_initialization_code = quote! {
            {
                m.add_class::<#wrapper_mod_name::#enum_name>(py)?;
            }
        };
        Ok((class_code, module_initialization_code))
    }

    fn generate_interface(
        &self,
        _conv_map: &mut TypeMap,
        _interface: &ForeignInterface,
    ) -> Result<(TokenStream, TokenStream)> {
        unimplemented!("Interfaces are currently unsupported for Python.")
    }

    fn generate_module_initialization(
        &self,
        module_initialization_code: &[TokenStream],
    ) -> Result<TokenStream> {
        let module_name = parse::<syn::Ident>(&self.module_name, SourceId::none())?;
        let module_init =
            parse::<syn::Ident>(&format!("init{}", &self.module_name), SourceId::none())?;
        let module_py_init =
            parse::<syn::Ident>(&format!("PyInit_{}", &self.module_name), SourceId::none())?;
        let registration_code = quote! {
            mod py_error {
                cpython::py_exception!(#module_name, Error);
            }

            cpython::py_module_initializer!(#module_name, #module_init, #module_py_init, |py, m| {
                m.add(py, "Error", py_error::Error::type_object(py))?;
                #(#module_initialization_code)*
                Ok(())
            });
        };
        Ok(registration_code)
    }
}

fn generate_rust_instance_field_and_methods(
    class: &ForeignClassInfo,
    conv_map: &mut TypeMap,
) -> Result<(TokenStream, TokenStream)> {
    if let Some(ref self_desc) = class.self_desc {
        let rust_self_type = &self_desc.self_type;
        let storage_smart_pointer = storage_smart_pointer_for_class(class, conv_map)?;
        if storage_smart_pointer.inner_ty.normalized_name
            != conv_map
                .find_or_alloc_rust_type(rust_self_type, class.src_id)
                .normalized_name
        {
            return Err(DiagnosticError::new(
                class.src_id,
                class.span(),
                "Self type and (inner) type returned from constructor doesn't match",
            ));
        }
        let storage_type = wrap_type_for_class(rust_self_type, storage_smart_pointer.pointer_type);
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
            class.src_id,
            class.span(),
            format!(
                "Class {} has non-static methods, but no self_type",
                class.name
            ),
        ))
    }
}

fn generate_method_code(
    class: &ForeignClassInfo,
    method: &ForeignMethod,
    conv_map: &mut TypeMap,
) -> Result<TokenStream> {
    if method.is_dummy_constructor() {
        return Ok(TokenStream::new());
    }
    let method_name = method_name(method, class.src_id)?;
    let method_rust_path = &method.rust_id;
    let skip_args_count = if let MethodVariant::Method(_) = method.variant {
        1
    } else {
        0
    };
    let (args_list, mut args_conversions): (Vec<_>, Vec<_>) = method
        .fn_decl
        .inputs
        .iter()
        .skip(skip_args_count)
        .map(|a| {
            let named_arg = a
                .as_named_arg()
                .map_err(|err| DiagnosticError::from_syn_err(class.src_id, err))?;
            let (arg_type, arg_conversion) = generate_conversion_for_argument(
                &conv_map.find_or_alloc_rust_type(&named_arg.ty, class.src_id),
                method.span(),
                class.src_id,
                conv_map,
                &named_arg.name,
                true,
            )?;
            Ok(((&named_arg.name, arg_type), arg_conversion))
        })
        .collect::<Result<Vec<_>>>()?
        .into_iter()
        .unzip();
    if let Some(self_conversion) = self_type_conversion(class, method, conv_map)? {
        args_conversions.insert(0, self_conversion);
    }
    let mut args_list_tokens = args_list
        .into_iter()
        .map(|(name, t)| {
            parse(
                &format!("{}: {}", name, t.into_token_stream()),
                class.src_id,
            )
        })
        .collect::<std::result::Result<Vec<TokenStream>, _>>()?;
    if let MethodVariant::Method(_) = method.variant {
        args_list_tokens.insert(0, parse("&self", class.src_id)?);
    } else if method.variant == MethodVariant::Constructor {
        args_list_tokens.insert(0, parse("_cls", class.src_id)?);
    }
    let attribute = if method.variant == MethodVariant::StaticMethod {
        parse("@staticmethod", class.src_id)?
    } else {
        TokenStream::new()
    };
    let (return_type, rust_call_with_return_conversion) = generate_conversion_for_return(
        &conv_map
            .find_or_alloc_rust_type(&extract_return_type(&method.fn_decl.output), class.src_id),
        method.span(),
        class.src_id,
        conv_map,
        quote! {
            #method_rust_path(#( #args_conversions ),*)
        },
    )?;
    let docstring = if !method_name.to_string().starts_with("__") {
        parse::<TokenStream>(
            &("/// ".to_owned() + &method.doc_comments.as_slice().join("\n/// ")),
            class.src_id,
        )?
    } else {
        // Python API doesn't support defining docstrings on the special methods (slots)
        quote! {}
    };
    Ok(quote! {
        #docstring #attribute def #method_name(
            #( #args_list_tokens ),*
        ) -> cpython::PyResult<#return_type> {
            #[allow(unused)]
            use super::*;
            Ok(#rust_call_with_return_conversion)
        }
    })
}

fn standard_method_name(method: &ForeignMethod, src_id: SourceId) -> Result<syn::Ident> {
    Ok(method
        .name_alias
        .as_ref()
        .or_else(|| method.rust_id.segments.last().map(|p| &p.ident))
        .ok_or_else(|| DiagnosticError::new(src_id, method.span(), "Method has no name"))?
        .clone())
}

fn method_name(method: &ForeignMethod, src_id: SourceId) -> Result<syn::Ident> {
    if method.variant == MethodVariant::Constructor {
        parse("__new__", src_id)
    } else {
        let name = standard_method_name(method, src_id)?;
        let name_str = name.to_string();
        match name_str.as_ref() {
            "to_string" => parse("__repr__", src_id),
            _ => Ok(name),
        }
    }
}

fn self_type_conversion(
    class: &ForeignClassInfo,
    method: &ForeignMethod,
    conv_map: &mut TypeMap,
) -> Result<Option<TokenStream>> {
    if let MethodVariant::Method(self_variant) = method.variant {
        let self_type = &class
            .self_desc
            .as_ref()
            .ok_or_else(|| {
                DiagnosticError::new(
                    class.src_id,
                    class.span(),
                    "Class have non-static methods, but no self_type",
                )
            })?
            .self_type;
        let self_type_ty = match self_variant {
            SelfTypeVariant::Rptr => parse_type! {&#self_type},
            SelfTypeVariant::RptrMut => parse_type! {&mut #self_type},
            _ => parse_type! {#self_type},
        };
        Ok(Some(
            generate_conversion_for_argument(
                &conv_map.find_or_alloc_rust_type(&self_type_ty, class.src_id),
                method.span(),
                class.src_id,
                conv_map,
                "self",
                true,
            )?
            .1,
        ))
    } else {
        Ok(None)
    }
}

fn has_any_methods(class: &ForeignClassInfo) -> bool {
    class
        .methods
        .iter()
        .any(|m| matches!(m.variant, MethodVariant::Method(_)))
}

fn generate_conversion_for_argument(
    rust_type: &RustType,
    method_span: Span,
    src_id: SourceId,
    conv_map: &mut TypeMap,
    arg_name: &str,
    reference_allowed: bool,
) -> Result<(Type, TokenStream)> {
    let arg_name_ident: TokenStream = parse(arg_name, src_id)?;
    if is_cpython_supported_type(rust_type) {
        Ok((rust_type.ty.clone(), arg_name_ident))
    } else if let Some((ty, conversion)) = if_exported_class_generate_argument_conversion(
        rust_type,
        conv_map,
        &arg_name_ident,
        method_span,
        src_id,
        reference_allowed,
    )? {
        Ok((ty, conversion))
    } else if rust_type
        .implements
        .contains_path(&parse(ENUM_TRAIT_NAME, src_id)?)
    {
        let enum_py_mod: Ident = parse(&py_wrapper_mod_name(&rust_type.normalized_name), src_id)?;
        Ok((
            parse_type!(u32),
            quote! {
                super::#enum_py_mod::from_u32(py, #arg_name_ident)?
            },
        ))
    } else if let Some(inner) = ast::if_option_return_some_type(rust_type) {
        let (inner_py_type, inner_conversion) = generate_conversion_for_argument(
            &conv_map.find_or_alloc_rust_type(&inner, src_id),
            method_span,
            src_id,
            conv_map,
            "inner",
            false,
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
    } else if let Some(inner) = if_type_slice_return_elem_type(&rust_type.ty, false) {
        let (inner_py_type, inner_conversion) = generate_conversion_for_argument(
            &conv_map.find_or_alloc_rust_type(inner, src_id),
            method_span,
            src_id,
            conv_map,
            "inner",
            false,
        )?;
        Ok((
            parse_type!(Vec<#inner_py_type>),
            quote! {
                &#arg_name_ident.into_iter().map(|inner| Ok(#inner_conversion)).collect::<cpython::PyResult<Vec<_>>>()?
            },
        ))
    } else if let Some(inner) = if_vec_return_elem_type(rust_type) {
        let (inner_py_type, inner_conversion) = generate_conversion_for_argument(
            &conv_map.find_or_alloc_rust_type(&inner, src_id),
            method_span,
            src_id,
            conv_map,
            "inner",
            false,
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
                src_id,
                method_span,
                "mutable reference is only supported for exported class types",
            ));
        }
        let (inner_py_type, inner_conversion) = generate_conversion_for_argument(
            &conv_map.find_or_alloc_rust_type(inner.elem.deref(), src_id),
            method_span,
            src_id,
            conv_map,
            arg_name,
            false,
        )?;
        Ok((
            parse_type!(#inner_py_type),
            quote! {
                &#inner_conversion
            },
        ))
    } else {
        Err(DiagnosticError::new(
            src_id,
            method_span,
            format!("Unsupported argument type: {}", rust_type),
        ))
    }
}

fn generate_conversion_for_return(
    rust_type: &RustType,
    method_span: Span,
    src_id: SourceId,
    conv_map: &mut TypeMap,
    rust_call: TokenStream,
) -> Result<(Type, TokenStream)> {
    if rust_type.ty == parse_type! { () } {
        Ok((
            parse_type!(cpython::PyObject),
            quote! {
                {#rust_call; py.None()}
            },
        ))
    } else if is_cpython_supported_type(rust_type) {
        Ok((rust_type.ty.clone(), rust_call))
    } else if let Some((ty, conversion)) = if_exported_class_generate_return_conversion(
        rust_type,
        conv_map,
        &rust_call,
        method_span,
        src_id,
    )? {
        Ok((ty, conversion))
    } else if rust_type
        .implements
        .contains_path(&parse(ENUM_TRAIT_NAME, src_id)?)
    {
        Ok((
            parse_type!(u32),
            quote! {
                #rust_call as u32
            },
        ))
    } else if let Some(inner) = ast::if_option_return_some_type(rust_type) {
        let (inner_py_type, inner_conversion) = generate_conversion_for_return(
            &conv_map.find_or_alloc_rust_type(&inner, src_id),
            method_span,
            src_id,
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
    } else if let Some(inner) = if_type_slice_return_elem_type(&rust_type.ty, false) {
        let (inner_py_type, inner_conversion) = generate_conversion_for_return(
            &conv_map.find_or_alloc_rust_type(inner, src_id),
            method_span,
            src_id,
            conv_map,
            quote! {inner},
        )?;
        Ok((
            parse_type!(Vec<#inner_py_type>),
            quote! {
                #rust_call.iter().cloned().map(|inner| Ok(#inner_conversion)).collect::<cpython::PyResult<Vec<_>>>()?
            },
        ))
    } else if let Some(inner) = if_vec_return_elem_type(rust_type) {
        let (inner_py_type, inner_conversion) = generate_conversion_for_return(
            &conv_map.find_or_alloc_rust_type(&inner, src_id),
            method_span,
            src_id,
            conv_map,
            quote! {inner},
        )?;
        Ok((
            parse_type!(Vec<#inner_py_type>),
            quote! {
                #rust_call.into_iter().map(|inner| Ok(#inner_conversion)).collect::<cpython::PyResult<Vec<_>>>()?
            },
        ))
    } else if let Some((inner_ok, _inner_err)) = ast::if_result_return_ok_err_types(rust_type) {
        let (inner_py_type, inner_conversion) = generate_conversion_for_return(
            &conv_map.find_or_alloc_rust_type(&inner_ok, src_id),
            method_span,
            src_id,
            conv_map,
            quote! {ok_inner},
        )?;
        Ok((
            parse_type!(#inner_py_type),
            quote! {
                match #rust_call {
                    Ok(ok_inner) => #inner_conversion,
                    Err(err_inner) => return Err(cpython::PyErr::new::<super::py_error::Error, _>(
                        py,
                        swig_collect_error_message(&err_inner)
                    )),
                }
            },
        ))
    } else if let Type::Reference(ref inner) = rust_type.ty {
        generate_conversion_for_return(
            &conv_map.find_or_alloc_rust_type(inner.elem.deref(), src_id),
            method_span,
            src_id,
            conv_map,
            quote! {(#rust_call).clone()},
        )
    } else if let Type::Tuple(ref tuple) = rust_type.ty {
        let (types, conversions): (Vec<_>, Vec<_>) = tuple
            .elems
            .iter()
            .enumerate()
            .map(|(i, ty)| {
                let i_ident = syn::Index {
                    index: i as u32,
                    span: Span::call_site(),
                };
                generate_conversion_for_return(
                    &conv_map.find_or_alloc_rust_type(ty, src_id),
                    method_span,
                    src_id,
                    conv_map,
                    quote! {tuple.#i_ident},
                )
            })
            .collect::<Result<Vec<_>>>()?
            .into_iter()
            .unzip();
        Ok((
            parse_type! {( #( #types, )* )},
            quote! {
                {
                    let tuple = #rust_call;
                    (
                        #( #conversions, )*
                    )
                }
            },
        ))
    } else {
        Err(DiagnosticError::new(
            src_id,
            method_span,
            format!("Unsupported return type: {}", rust_type),
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
// What's more `rust_cpython` requires the object to be `Send + 'static`, because Python VM
// can move it between threads without any control from Rust.
// As a result, we need to wrap the object in `Mutex`, to provide mutability.
// By default, `Mutex` is used.
// This could be overriden by the smart pointer returned from the constructor.
// Following smart pointers are supported:
// - `Arc<Mutex<T>>`: wrapped Rust object is mutable and can be shared between Rust and Python,
// - `Mutex<T>`: wrapped Rust object is mutable, but only Python owns it,
// - `Arc<T>`: wrapped Rust object is immutable and can be shared between Rust and Python,
// - `Box<T>`: wrapped Rust object is immutable and only Python owns it,
// Note, that `Rc` is NOT supported. This is because it is not `Send`.
// `RefCell` theoretically could be supported, but generated Python API would be thread unsafe
// (it is `Send`, but no `Sync`), so it is intentionally omitted.
#[derive(Debug, Clone, Copy, PartialEq)]
enum PointerType {
    ArcMutex,
    Mutex,
    Arc,
    Box,
    None,
}

impl PointerType {
    fn is_shared(self) -> bool {
        self == PointerType::Arc || self == PointerType::ArcMutex
    }
}

struct SmartPointerInfo {
    pointer_type: PointerType,
    inner_ty: RustType,
}

impl SmartPointerInfo {
    fn new(pointer_type: PointerType, inner_ty: RustType) -> SmartPointerInfo {
        SmartPointerInfo {
            pointer_type,
            inner_ty,
        }
    }
}

fn smart_pointer(
    rust_type: &RustType,
    conv_map: &mut TypeMap,
    src_id: SourceId,
) -> SmartPointerInfo {
    if let Some(inner_ty) = ast::check_if_smart_pointer_return_inner_type(rust_type, "Arc") {
        let rust_inner_ty = conv_map.find_or_alloc_rust_type(&inner_ty, src_id);
        if let Some(inner_inner_ty) =
            ast::check_if_smart_pointer_return_inner_type(&rust_inner_ty, "Mutex")
        {
            SmartPointerInfo::new(
                PointerType::ArcMutex,
                conv_map.find_or_alloc_rust_type(&inner_inner_ty, src_id),
            )
        } else {
            SmartPointerInfo::new(PointerType::Arc, rust_inner_ty)
        }
    } else if let Some(inner_ty) = ast::check_if_smart_pointer_return_inner_type(rust_type, "Mutex")
    {
        SmartPointerInfo::new(
            PointerType::Mutex,
            conv_map.find_or_alloc_rust_type(&inner_ty, src_id),
        )
    } else if let Some(inner_ty) = ast::check_if_smart_pointer_return_inner_type(rust_type, "Box") {
        SmartPointerInfo::new(
            PointerType::Box,
            conv_map.find_or_alloc_rust_type(&inner_ty, src_id),
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
    src_id: SourceId,
) -> (Reference, RustType) {
    if let Type::Reference(ref reference) = rust_type.ty {
        if reference.mutability.is_some() {
            (
                Reference::MutRef,
                conv_map.find_or_alloc_rust_type(&reference.elem, src_id),
            )
        } else {
            (
                Reference::Ref,
                conv_map.find_or_alloc_rust_type(&reference.elem, src_id),
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
    src_id: SourceId,
) -> Result<Option<(Type, TokenStream)>> {
    let (reference_type, rust_type_unref) =
        get_reference_info_and_inner_type(rust_type, conv_map, src_id);
    let smart_pointer_info = smart_pointer(&rust_type_unref, conv_map, src_id);
    let class = match conv_map
        .find_foreigner_class_with_such_this_type(&smart_pointer_info.inner_ty.ty, |_, ft| {
            ft.self_desc.as_ref().map(|x| x.self_type.clone())
        }) {
        Some(fc) => fc.clone(),
        None => return Ok(None),
    };
    let class_smart_pointer = storage_smart_pointer_for_class(&class, conv_map)?;
    let rust_call_with_deref = if reference_type != Reference::None {
        if smart_pointer_info.pointer_type == PointerType::Mutex {
            return Err(DiagnosticError::new(
                src_id,
                method_span,
               "Returning a rust object into python by reference is not safe, so the clone of the object needs to be make.\
However, `Mutex` doesn't implement `Clone`, so it can't be returned by reference."
            ));
        } else if class.clone_derived()
            || class.copy_derived()
            || smart_pointer_info.pointer_type.is_shared()
        {
            quote! {
                ((#rust_call).clone())
            }
        } else {
            return Err(DiagnosticError::new(
                src_id,
                method_span,
                "Returning a rust object into python by reference is not safe, so the clone of the object needs to be make. \
Thus, the returned type must marked with `#[derive(Clone)]` or `#[derive(Copy)]` inside its `foreigner_class` macro."
            ));
        }
    } else {
        rust_call.clone()
    };
    let class_name = &class.name;
    let py_mod: Ident = parse(&py_wrapper_mod_name(&class_name.to_string()), src_id)?;
    let rust_call_with_wrapper = match class_smart_pointer.pointer_type {
        PointerType::Mutex => generate_wrapper_constructor_for_mutex(
            &class,
            &smart_pointer_info,
            rust_call_with_deref,
            method_span,
            src_id,
        )?,
        PointerType::ArcMutex => generate_wrapper_constructor_for_arc_mutex(
            &class,
            &smart_pointer_info,
            rust_call_with_deref,
            method_span,
            src_id,
        )?,
        PointerType::Arc => generate_wrapper_constructor_for_arc(
            &class,
            &smart_pointer_info,
            rust_call_with_deref,
            method_span,
            src_id,
        )?,
        PointerType::Box => generate_wrapper_constructor_for_box(
            &class,
            &smart_pointer_info,
            rust_call_with_deref,
            method_span,
            src_id,
        )?,
        _ => unreachable!("`PointerType::None` as class storage pointer"),
    };
    let conversion = quote! {
        super::#py_mod::create_instance(py, #rust_call_with_wrapper)?
    };
    Ok(Some((parse_type!(super::#py_mod::#class_name), conversion)))
}

fn generate_wrapper_constructor_for_mutex(
    class: &ForeignClassInfo,
    returned_smart_pointer: &SmartPointerInfo,
    rust_call: TokenStream,
    method_span: Span,
    src_id: SourceId,
) -> Result<TokenStream> {
    match returned_smart_pointer.pointer_type {
        PointerType::Mutex => Ok(rust_call),
        PointerType::None => Ok(quote! {std::sync::Mutex::new(#rust_call)}),
        _ => Err(DiagnosticError::new(
            src_id,
            method_span,
            format!(
                "Unsupported conversion for smart pointer. \
Foreigner class {} is stored as `Mutex` and can be returned eiter as `Mutex` or bare type",
                class.name
            ),
        )),
    }
}

fn generate_wrapper_constructor_for_arc_mutex(
    class: &ForeignClassInfo,
    returned_smart_pointer: &SmartPointerInfo,
    rust_call: TokenStream,
    method_span: Span,
    src_id: SourceId,
) -> Result<TokenStream> {
    match returned_smart_pointer.pointer_type {
        PointerType::ArcMutex => Ok(rust_call),
        _ => Err(DiagnosticError::new(
            src_id,
            method_span,
            format!(
                "Unsupported conversion for smart pointer. \
Foreigner class {} is stored as `Arc<Mutex<T>>`, so it is intended for sharing between Rust and Python.\
Thus, it must always be returned from Rust literally by `Arc<Mutex<T>>` \
(or reference to `Arc<Mutex<T>>`) for any sharing to occur.",
                class.name
            ),
        ))
    }
}

fn generate_wrapper_constructor_for_arc(
    class: &ForeignClassInfo,
    returned_smart_pointer: &SmartPointerInfo,
    rust_call: TokenStream,
    method_span: Span,
    src_id: SourceId,
) -> Result<TokenStream> {
    match returned_smart_pointer.pointer_type {
        PointerType::Arc => Ok(rust_call),
        _ => Err(DiagnosticError::new(
            src_id,
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
    class: &ForeignClassInfo,
    returned_smart_pointer: &SmartPointerInfo,
    rust_call: TokenStream,
    method_span: Span,
    src_id: SourceId,
) -> Result<TokenStream> {
    match returned_smart_pointer.pointer_type {
        PointerType::None => Ok(rust_call),
        PointerType::Box => Ok(quote! {(*#rust_call)}),
        _ => Err(DiagnosticError::new(
            src_id,
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
    src_id: SourceId,
    reference_allowed: bool,
) -> Result<Option<(Type, TokenStream)>> {
    let (reference_type, rust_type_unref) =
        get_reference_info_and_inner_type(rust_type, conv_map, src_id);
    let smart_pointer_info = smart_pointer(&rust_type_unref, conv_map, src_id);
    let class = match conv_map
        .find_foreigner_class_with_such_this_type(&smart_pointer_info.inner_ty.ty, |_, ft| {
            ft.self_desc.as_ref().map(|x| x.self_type.clone())
        }) {
        Some(fc) => fc.clone(),
        None => return Ok(None),
    };
    let class_smart_pointer = storage_smart_pointer_for_class(&class, conv_map)?;
    let class_name = class.name.to_string();
    let py_mod_str = py_wrapper_mod_name(&class_name);
    let py_mod: Ident = parse(&py_mod_str, src_id)?;
    let py_type: Type = if reference_allowed {
        parse(&format!("&super::{}::{}", &py_mod_str, &class_name), src_id)?
    } else {
        parse(&format!("super::{}::{}", &py_mod_str, &class_name), src_id)?
    };

    let rust_instance_code = if reference_allowed {
        quote! {
            super::#py_mod::rust_instance(#arg_name_ident, py)
        }
    } else {
        quote! {
            super::#py_mod::rust_instance(&#arg_name_ident, py)
        }
    };
    let deref_code = match class_smart_pointer.pointer_type {
        PointerType::ArcMutex => generate_deref_for_arc_mutex(
            &class,
            smart_pointer_info.pointer_type,
            reference_type,
            rust_instance_code,
            method_span,
            src_id,
        )?,
        PointerType::Arc => generate_deref_for_arc(
            &class,
            smart_pointer_info.pointer_type,
            reference_type,
            rust_instance_code,
            method_span,
            src_id,
        )?,
        PointerType::Mutex => generate_deref_for_mutex(
            &class,
            smart_pointer_info.pointer_type,
            reference_type,
            rust_instance_code,
            method_span,
            src_id,
        )?,
        PointerType::Box => generate_deref_for_box(
            &class,
            smart_pointer_info.pointer_type,
            reference_type,
            rust_instance_code,
            method_span,
            src_id,
        )?,
        _ => unreachable!("Class stored as None"),
    };

    Ok(Some((py_type, deref_code)))
}

fn generate_deref_for_mutex(
    class: &ForeignClassInfo,
    arg_smart_pointer: PointerType,
    arg_reference: Reference,
    rust_instance_code: TokenStream,
    method_span: Span,
    src_id: SourceId,
) -> Result<TokenStream> {
    match arg_smart_pointer {
        PointerType::None => match arg_reference {
            Reference::MutRef => Ok(quote!{(&mut *#rust_instance_code.lock().unwrap())}),
            Reference::Ref => Ok(quote!{(&*#rust_instance_code.lock().unwrap())}),
            Reference::None => append_clone_if_supported(class, quote!{*#rust_instance_code.lock().unwrap()}, method_span),
        },
        PointerType::Mutex => match arg_reference {
            Reference::Ref => Ok(rust_instance_code),
            _ => Err(DiagnosticError::new(
                src_id,
                method_span,
                "Mutex can be passed to function only by const reference `Mutex`",
            ))
        }
        _ => Err(DiagnosticError::new(
            src_id,
            method_span,
            format!(
                "Unsupported conversion for smart pointer. \
Foreigner class {} is stored as `Mutex` and can be passed to function either as `Mutex` reference or bare type",
                class.name
            ),
        ))
    }
}

fn generate_deref_for_arc_mutex(
    class: &ForeignClassInfo,
    arg_smart_pointer: PointerType,
    arg_reference: Reference,
    rust_instance_code: TokenStream,
    method_span: Span,
    src_id: SourceId,
) -> Result<TokenStream> {
    match arg_smart_pointer {
        PointerType::None => match arg_reference {
            Reference::MutRef => Ok(quote!{(&mut *#rust_instance_code.lock().unwrap())}),
            Reference::Ref => Ok(quote!{(&*#rust_instance_code.lock().unwrap())}),
            Reference::None => append_clone_if_supported(class, quote!{*#rust_instance_code.lock().unwrap()}, method_span),
        },
        PointerType::ArcMutex => match arg_reference {
            Reference::Ref => Ok(rust_instance_code),
            Reference::None => Ok(quote!{#rust_instance_code.clone()}),
            _ => Err(DiagnosticError::new(
                src_id,
                method_span,
                "Arc<Mutex<T>> can't be passed to function by mut reference. It doesn't make sense anyway.",
            ))
        }
        _ => Err(DiagnosticError::new(
            src_id,
            method_span,
            format!(
                "Unsupported conversion for smart pointer. \
Foreigner class {} is stored as `Arc<Mutex<T>>` and can be passed to function either as `Arc<Mutex<T>>` or bare type",
                class.name
            ),
        ))
    }
}

fn generate_deref_for_arc(
    class: &ForeignClassInfo,
    arg_smart_pointer: PointerType,
    arg_reference: Reference,
    rust_instance_code: TokenStream,
    method_span: Span,
    src_id: SourceId,
) -> Result<TokenStream> {
    match arg_smart_pointer {
        PointerType::None => match arg_reference {
            Reference::Ref => Ok(quote!{(&*#rust_instance_code)}),
            Reference::None => append_clone_if_supported(class, quote!{*#rust_instance_code}, method_span),
            Reference::MutRef => Err(DiagnosticError::new(
                src_id,
                method_span,
                "Object is stored in `Arc`, so it is immutable. If you need mutability, use Arc<Mutex<T>> for constructor type",
            ))
        },
        PointerType::Arc => match arg_reference {
            Reference::Ref => Ok(rust_instance_code),
            Reference::None => Ok(quote!{#rust_instance_code.clone()}),
            _ => Err(DiagnosticError::new(
                src_id,
                method_span,
                "`Arc` can't be passed to function by mut reference. It's immutable",
            ))
        }
        _ => Err(DiagnosticError::new(
            src_id,
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
    class: &ForeignClassInfo,
    arg_smart_pointer: PointerType,
    arg_reference: Reference,
    rust_instance_code: TokenStream,
    method_span: Span,
    src_id: SourceId,
) -> Result<TokenStream> {
    match arg_smart_pointer {
        PointerType::None => match arg_reference {
            Reference::Ref => Ok(rust_instance_code),
            Reference::None => append_clone_if_supported(class, rust_instance_code, method_span),
            Reference::MutRef => Err(DiagnosticError::new(
                src_id,
                method_span,
                "Object is stored in `Box`, so it is immutable. If you need mutability, use `Mutex` for constructor type",
            ))
        },
        _ => Err(DiagnosticError::new(
            src_id,
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
    class: &ForeignClassInfo,
    rust_instance_code: TokenStream,
    method_span: Span,
) -> Result<TokenStream> {
    if class.clone_derived() || class.copy_derived() {
        Ok(quote!((#rust_instance_code).clone()))
    } else {
        Err(DiagnosticError::new(
            class.src_id,
            method_span,
            "Passing object by value requires that it is marked with `#[derive(Clone)]` or `#[derive(Copy)]`\
inside its `foreigner_class` macro."
        ))
    }
}

fn storage_smart_pointer_for_class(
    class: &ForeignClassInfo,
    conv_map: &mut TypeMap,
) -> Result<SmartPointerInfo> {
    if let Some(ref self_desc) = class.self_desc {
        let constructor_ret_rust_type =
            conv_map.find_or_alloc_rust_type(&self_desc.constructor_ret_type, class.src_id);
        let pointer = smart_pointer(&constructor_ret_rust_type, conv_map, class.src_id);
        match pointer.pointer_type {
            // Default wrapper type for storage is `Mutex`.
            PointerType::None => Ok(SmartPointerInfo::new(PointerType::Mutex, pointer.inner_ty)),
            _ => Ok(pointer),
        }
    } else {
        Err(DiagnosticError::new(
            class.src_id,
            class.span(),
            "Class doesn't define a type returned from constructor, nor self_type, but is not static"
        ))
    }
}

fn wrap_type_for_class(self_type: &Type, storage_pointer: PointerType) -> TokenStream {
    match storage_pointer {
        PointerType::ArcMutex => quote! {std::sync::Arc<std::sync::Mutex<super::#self_type>>},
        PointerType::Arc => quote! {std::sync::Arc<super::#self_type>},
        PointerType::Mutex => quote! {std::sync::Mutex<super::#self_type>},
        PointerType::Box => quote! {super::#self_type},
        PointerType::None => unreachable!("None pointer for object storage"),
    }
}

fn parse<T: syn::parse::Parse>(ident_str: &str, src_id: SourceId) -> Result<T> {
    syn::parse_str::<T>(ident_str).map_err(|err| DiagnosticError::from_syn_err(src_id, err))
}

fn if_type_slice_return_elem_type(ty: &Type, accept_mutbl_slice: bool) -> Option<&Type> {
    if let syn::Type::Reference(syn::TypeReference {
        ref elem,
        mutability,
        ..
    }) = ty
    {
        if mutability.is_some() && !accept_mutbl_slice {
            return None;
        }
        if let syn::Type::Slice(syn::TypeSlice { ref elem, .. }) = **elem {
            Some(elem)
        } else {
            None
        }
    } else {
        None
    }
}

fn if_vec_return_elem_type(ty: &RustType) -> Option<Type> {
    let from_ty: Type = parse_quote! { Vec<T> };
    let to_ty: Type = parse_quote! { T };
    let generic_params: syn::Generics = parse_quote! { <T> };

    GenericTypeConv::new(from_ty, to_ty, generic_params, TypeConvCode::invalid())
        .is_conv_possible(ty, None, |_| None)
        .map(|x| x.to_ty)
}
