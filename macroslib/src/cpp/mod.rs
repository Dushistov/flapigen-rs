mod code_for_class;
mod cpp_code;
mod map_type;

use std::{fmt, io::Write};

use log::{debug, trace};
use petgraph::Direction;
use proc_macro2::TokenStream;
use quote::ToTokens;
use smol_str::SmolStr;
use syn::{parse_quote, spanned::Spanned, Type};

use crate::{
    cpp::map_type::map_type,
    error::{panic_on_syn_error, DiagnosticError, Result},
    file_cache::FileWriteCache,
    source_registry::SourceId,
    typemap::{
        ast::{
            fn_arg_type, parse_ty_with_given_span, parse_ty_with_given_span_checked,
            DisplayToTokens, TypeName,
        },
        ty::{ForeignType, RustType},
        unpack_unique_typename,
        utils::rust_to_foreign_convert_method_inputs,
        ForeignMethodSignature, ForeignTypeInfo, RustTypeIdx, FROM_VAR_TEMPLATE, TO_VAR_TEMPLATE,
    },
    types::{
        ForeignEnumInfo, ForeignInterface, ForeignerClassInfo, ForeignerMethod, MethodAccess,
        MethodVariant,
    },
    CppConfig, LanguageGenerator, SourceCode, TypeMap,
};

#[derive(Debug)]
struct CppConverter {
    typename: SmolStr,
    converter: String,
}

#[derive(Debug)]
struct CppForeignTypeInfo {
    base: ForeignTypeInfo,
    pub(in crate::cpp) cpp_converter: Option<CppConverter>,
}

impl CppForeignTypeInfo {
    pub(in crate::cpp) fn try_new(
        tmap: &mut TypeMap,
        direction: petgraph::Direction,
        ftype_idx: ForeignType,
    ) -> Result<Self> {
        let ftype = &tmap[ftype_idx];
        let mut cpp_converter = None;

        let rule = match direction {
            petgraph::Direction::Outgoing => ftype.into_from_rust.as_ref(),
            petgraph::Direction::Incoming => ftype.from_into_rust.as_ref(),
        }
        .ok_or_else(|| {
            DiagnosticError::new2(
                ftype.src_id_span(),
                format!(
                    "No rule to convert foreign type {} as input/output type",
                    ftype.name
                ),
            )
        })?;

        let base_rt;
        let base_ft_name;
        if let Some(intermediate) = rule.intermediate.as_ref() {
            base_rt = intermediate.intermediate_ty;
            let typename = ftype.name.typename.clone();
            let converter = intermediate.conv_code.to_string();
            let inter_ft = convert_rt_to_ft(tmap, intermediate.intermediate_ty)?;
            base_ft_name = tmap[inter_ft].name.typename.clone();
            cpp_converter = Some(CppConverter {
                typename,
                converter,
            });
        } else {
            base_rt = rule.rust_ty;
            base_ft_name = ftype.name.typename.clone();
        }
        Ok(CppForeignTypeInfo {
            base: ForeignTypeInfo {
                name: base_ft_name,
                correspoding_rust_type: tmap[base_rt].clone(),
            },
            cpp_converter,
        })
    }
}

impl AsRef<ForeignTypeInfo> for CppForeignTypeInfo {
    fn as_ref(&self) -> &ForeignTypeInfo {
        &self.base
    }
}

struct CppForeignMethodSignature {
    output: CppForeignTypeInfo,
    input: Vec<CppForeignTypeInfo>,
}

impl From<ForeignTypeInfo> for CppForeignTypeInfo {
    fn from(x: ForeignTypeInfo) -> Self {
        CppForeignTypeInfo {
            base: ForeignTypeInfo {
                name: x.name,
                correspoding_rust_type: x.correspoding_rust_type,
            },
            cpp_converter: None,
        }
    }
}

impl ForeignMethodSignature for CppForeignMethodSignature {
    type FI = CppForeignTypeInfo;
    fn output(&self) -> &ForeignTypeInfo {
        &self.output.base
    }
    fn input(&self) -> &[CppForeignTypeInfo] {
        &self.input[..]
    }
}

struct MethodContext<'a> {
    class: &'a ForeignerClassInfo,
    method: &'a ForeignerMethod,
    f_method: &'a CppForeignMethodSignature,
    c_func_name: &'a str,
    decl_func_args: &'a str,
    args_names: &'a str,
    real_output_typename: &'a str,
}

impl LanguageGenerator for CppConfig {
    fn register_class(&self, conv_map: &mut TypeMap, class: &ForeignerClassInfo) -> Result<()> {
        class
            .validate_class()
            .map_err(|err| DiagnosticError::new(class.src_id, class.span(), err))?;
        if let Some(constructor_ret_type) = class.constructor_ret_type.as_ref() {
            let this_type_for_method = constructor_ret_type;
            let this_type = conv_map.find_or_alloc_rust_type_that_implements(
                this_type_for_method,
                "SwigForeignClass",
                class.src_id,
            );

            let void_ptr_ty = parse_type! { *mut ::std::os::raw::c_void };
            let void_ptr_rust_ty = conv_map.find_or_alloc_rust_type_with_suffix(
                &void_ptr_ty,
                &this_type.normalized_name,
                SourceId::none(),
            );
            let foreign_typename = format!("{} *", cpp_code::c_class_type(class));
            conv_map.cache_rust_to_foreign_conv(
                &this_type,
                ForeignTypeInfo {
                    correspoding_rust_type: void_ptr_rust_ty.clone(),
                    name: foreign_typename.into(),
                },
            )?;

            let const_void_ptr_ty = parse_type! { *const ::std::os::raw::c_void };
            let const_void_ptr_rust_ty = conv_map.find_or_alloc_rust_type_with_suffix(
                &const_void_ptr_ty,
                &this_type.normalized_name,
                SourceId::none(),
            );
            let const_foreign_typename = format!("const {} *", cpp_code::c_class_type(class));
            conv_map.cache_rust_to_foreign_conv(
                &this_type,
                ForeignTypeInfo {
                    correspoding_rust_type: const_void_ptr_rust_ty.clone(),
                    name: const_foreign_typename.into(),
                },
            )?;

            let this_type_ty = &this_type.ty;
            //handle foreigner_class as input arg

            let code = format!("& {}", DisplayToTokens(this_type_ty));
            let gen_ty = parse_ty_with_given_span_checked(&code, this_type_ty.span());
            let this_type_ref = conv_map.find_or_alloc_rust_type(&gen_ty, class.src_id);
            conv_map.add_conversation_rule(
                const_void_ptr_rust_ty.clone(),
                this_type_ref,
                format!(
                    r#"
    assert!(!{from_var}.is_null());
    let {to_var}: &{this_type} = unsafe {{ &*({from_var} as *const {this_type}) }};
"#,
                    to_var = TO_VAR_TEMPLATE,
                    from_var = FROM_VAR_TEMPLATE,
                    this_type = this_type.normalized_name.clone(),
                )
                .into(),
            );

            let code = format!("&mut {}", DisplayToTokens(this_type_ty));
            let gen_ty = parse_ty_with_given_span_checked(&code, this_type_ty.span());
            let this_type_mut_ref = conv_map.find_or_alloc_rust_type(&gen_ty, class.src_id);
            //handle foreigner_class as input arg
            conv_map.add_conversation_rule(
                void_ptr_rust_ty.clone(),
                this_type_mut_ref,
                format!(
                    r#"
    assert!(!{from_var}.is_null());
    let {to_var}: &mut {this_type} = unsafe {{ &mut *({from_var} as *mut {this_type}) }};
"#,
                    to_var = TO_VAR_TEMPLATE,
                    from_var = FROM_VAR_TEMPLATE,
                    this_type = this_type.normalized_name,
                )
                .into(),
            );

            debug!(
                "register class: add implements SwigForeignClass for {}",
                this_type.normalized_name
            );

            conv_map.find_or_alloc_rust_type(constructor_ret_type, class.src_id);

            let (this_type_for_method, _code_box_this) =
                conv_map.convert_to_heap_pointer(&this_type, "this");
            let unpack_code = TypeMap::unpack_from_heap_pointer(&this_type, TO_VAR_TEMPLATE, true);
            let this_type_name = this_type_for_method.normalized_name.clone();
            conv_map.add_conversation_rule(
                void_ptr_rust_ty,
                this_type,
                format!(
                    r#"
            assert!(!{from_var}.is_null());
            let {to_var}: *mut {this_type} = {from_var} as *mut {this_type};
        {unpack_code}
        "#,
                    to_var = TO_VAR_TEMPLATE,
                    from_var = FROM_VAR_TEMPLATE,
                    this_type = this_type_name,
                    unpack_code = unpack_code,
                )
                .into(),
            );
        }
        conv_map.find_or_alloc_rust_type(&class.self_type_as_ty(), class.src_id);
        Ok(())
    }

    fn generate(
        &self,
        conv_map: &mut TypeMap,
        _: usize,
        class: &ForeignerClassInfo,
    ) -> Result<Vec<TokenStream>> {
        debug!(
            "generate: begin for {}, this_type_for_method {:?}",
            class.name, class.constructor_ret_type
        );
        let has_methods = class.methods.iter().any(|m| match m.variant {
            MethodVariant::Method(_) => true,
            _ => false,
        });
        let has_constructor = class
            .methods
            .iter()
            .any(|m| m.variant == MethodVariant::Constructor);

        if has_methods && !has_constructor {
            return Err(DiagnosticError::new(
                class.src_id,
                class.span(),
                format!(
                    "namespace {}, class {}: has methods, but no constructor\n
May be you need to use `private constructor = empty;` syntax?",
                    self.namespace_name, class.name
                ),
            ));
        }

        let m_sigs = find_suitable_foreign_types_for_methods(conv_map, class, self)?;
        let mut code_items = code_for_class::generate(
            conv_map,
            &self.output_dir,
            &self.namespace_name,
            self.separate_impl_headers,
            class,
            &m_sigs,
        )?;
        code_items.append(&mut self.to_generate.borrow_mut());
        Ok(code_items)
    }

    fn generate_enum(
        &self,
        conv_map: &mut TypeMap,
        pointer_target_width: usize,
        enum_info: &ForeignEnumInfo,
    ) -> Result<Vec<TokenStream>> {
        if (enum_info.items.len() as u64) >= u64::from(u32::max_value()) {
            return Err(DiagnosticError::new(
                enum_info.src_id,
                enum_info.span(),
                "Too many items in enum",
            ));
        }

        trace!("enum_ti: {}", enum_info.name);
        let enum_name = &enum_info.name;
        let enum_ti: Type = parse_ty_with_given_span(&enum_name.to_string(), enum_info.name.span())
            .map_err(|err| DiagnosticError::from_syn_err(enum_info.src_id, err))?;
        conv_map.find_or_alloc_rust_type_that_implements(
            &enum_ti,
            "SwigForeignEnum",
            enum_info.src_id,
        );

        cpp_code::generate_code_for_enum(&self.output_dir, enum_info)
            .map_err(|err| DiagnosticError::new(enum_info.src_id, enum_info.span(), err))?;
        let code = generate_rust_code_for_enum(conv_map, pointer_target_width, enum_info)?;
        Ok(code)
    }

    fn generate_interface(
        &self,
        conv_map: &mut TypeMap,
        pointer_target_width: usize,
        interface: &ForeignInterface,
    ) -> Result<Vec<TokenStream>> {
        let f_methods = find_suitable_ftypes_for_interace_methods(conv_map, interface, self)?;
        cpp_code::generate_for_interface(
            &self.output_dir,
            &self.namespace_name,
            interface,
            &f_methods,
        )
        .map_err(|err| DiagnosticError::new(interface.src_id, interface.span(), err))?;

        let items =
            rust_code_generate_interface(conv_map, pointer_target_width, interface, &f_methods)?;

        let c_struct_name = format!("C_{}", interface.name);
        let rust_struct_pointer = format!("*const {}", c_struct_name);
        let rust_ty: Type = parse_ty_with_given_span(&rust_struct_pointer, interface.name.span())
            .map_err(|err| DiagnosticError::from_syn_err(interface.src_id, err))?;
        let c_struct_pointer = format!("const struct {} * const", c_struct_name);

        let rust_ty = conv_map.find_or_alloc_rust_type_no_src_id(&rust_ty);

        conv_map.add_foreign(
            rust_ty,
            TypeName::new(c_struct_pointer, interface.src_id_span()),
        )?;

        Ok(items)
    }

    fn init(&self, conv_map: &mut TypeMap, code: &[SourceCode]) -> std::result::Result<(), String> {
        //for enum
        conv_map.find_or_alloc_rust_type_no_src_id(&parse_type! { u32 });

        for cu in code {
            let src_path = self.output_dir.join(&cu.id_of_code);
            let mut src_file = FileWriteCache::new(&src_path);
            src_file
                .write_all(
                    cu.code
                        .replace("RUST_SWIG_USER_NAMESPACE", &self.namespace_name)
                        .as_bytes(),
                )
                .map_err(|err| format!("write to {} failed: {}", src_path.display(), err))?;
            src_file
                .update_file_if_necessary()
                .map_err(|err| format!("update of {} failed: {}", src_path.display(), err))?;
        }

        Ok(())
    }
}

fn find_suitable_foreign_types_for_methods(
    conv_map: &mut TypeMap,
    class: &ForeignerClassInfo,
    cpp_cfg: &CppConfig,
) -> Result<Vec<CppForeignMethodSignature>> {
    let mut ret = Vec::<CppForeignMethodSignature>::with_capacity(class.methods.len());
    let dummy_ty = parse_type! { () };
    let dummy_rust_ty = conv_map.find_or_alloc_rust_type_no_src_id(&dummy_ty);

    for method in &class.methods {
        //skip self argument
        let skip_n = match method.variant {
            MethodVariant::Method(_) => 1,
            _ => 0,
        };
        assert!(method.fn_decl.inputs.len() >= skip_n);
        let mut input =
            Vec::<CppForeignTypeInfo>::with_capacity(method.fn_decl.inputs.len() - skip_n);
        for arg in method.fn_decl.inputs.iter().skip(skip_n) {
            let arg_rust_ty = conv_map.find_or_alloc_rust_type(fn_arg_type(arg), class.src_id);
            input.push(map_type(
                conv_map,
                cpp_cfg,
                &arg_rust_ty,
                Direction::Incoming,
                (class.src_id, fn_arg_type(arg).span()),
            )?);
        }
        let output: CppForeignTypeInfo = match method.variant {
            MethodVariant::Constructor => ForeignTypeInfo {
                name: "".into(),
                correspoding_rust_type: dummy_rust_ty.clone(),
            }
            .into(),
            _ => match method.fn_decl.output {
                syn::ReturnType::Default => ForeignTypeInfo {
                    name: "void".into(),
                    correspoding_rust_type: dummy_rust_ty.clone(),
                }
                .into(),
                syn::ReturnType::Type(_, ref rt) => {
                    let ret_rust_ty = conv_map.find_or_alloc_rust_type(rt, class.src_id);
                    map_type(
                        conv_map,
                        cpp_cfg,
                        &ret_rust_ty,
                        Direction::Outgoing,
                        (class.src_id, rt.span()),
                    )?
                }
            },
        };
        ret.push(CppForeignMethodSignature { output, input });
    }
    Ok(ret)
}

fn c_func_name(class: &ForeignerClassInfo, method: &ForeignerMethod) -> String {
    format!(
        "{access}{class_name}_{func}",
        access = match method.access {
            MethodAccess::Private => "private_",
            MethodAccess::Protected => "protected_",
            MethodAccess::Public => "",
        },
        class_name = class.name,
        func = method.short_name(),
    )
}

fn rust_generate_args_with_types(
    f_method: &CppForeignMethodSignature,
) -> std::result::Result<String, String> {
    use std::fmt::Write;

    let mut buf = String::new();
    for (i, f_type_info) in f_method.input.iter().enumerate() {
        write!(
            &mut buf,
            "a_{}: {}, ",
            i,
            unpack_unique_typename(&f_type_info.as_ref().correspoding_rust_type.normalized_name),
        )
        .map_err(fmt_write_err_map)?;
    }
    Ok(buf)
}

fn fmt_write_err_map(err: fmt::Error) -> String {
    format!("fmt write error: {}", err)
}

fn generate_rust_code_for_enum(
    conv_map: &mut TypeMap,
    pointer_target_width: usize,
    enum_info: &ForeignEnumInfo,
) -> Result<Vec<TokenStream>> {
    use std::fmt::Write;

    let rust_enum_name = enum_info.rust_enum_name();

    let mut code = format!(
        r#"
impl SwigFrom<u32> for {rust_enum_name} {{
    fn swig_from(x: u32) -> {rust_enum_name} {{
        match x {{

"#,
        rust_enum_name = rust_enum_name,
    );
    for (i, item) in enum_info.items.iter().enumerate() {
        writeln!(
            &mut code,
            "{index} => {item_name},",
            index = i,
            item_name = DisplayToTokens(&item.rust_name)
        )
        .unwrap();
    }
    write!(
        &mut code,
        r#"
        _ => panic!("{{}} not expected for {rust_enum_name}", x),
        }}
    }}
}}
"#,
        rust_enum_name = rust_enum_name,
    )
    .unwrap();

    write!(
        &mut code,
        r#"
impl SwigFrom<Option<u32>> for Option<{rust_enum_name}> {{
    fn swig_from(x: Option<u32>) -> Option<{rust_enum_name}> {{
        x.map(|v| match v {{

"#,
        rust_enum_name = rust_enum_name,
    )
    .unwrap();
    for (i, item) in enum_info.items.iter().enumerate() {
        writeln!(
            &mut code,
            "{index} => {item_name},",
            index = i,
            item_name = DisplayToTokens(&item.rust_name)
        )
        .unwrap();
    }
    write!(
        &mut code,
        r#"
        _ => panic!("{{}} not expected for {rust_enum_name}", v),
        }})
    }}
}}
"#,
        rust_enum_name = rust_enum_name,
    )
    .unwrap();

    let mut trait_impl = format!(
        r#"
impl SwigForeignEnum for {rust_enum_name} {{
    fn as_u32(&self) -> u32 {{
        match *self {{
"#,
        rust_enum_name = rust_enum_name
    );
    for (i, item) in enum_info.items.iter().enumerate() {
        write!(
            &mut trait_impl,
            r#"
            {item_name} => {index},
"#,
            index = i,
            item_name = DisplayToTokens(&item.rust_name)
        )
        .unwrap();
    }
    write!(
        &mut trait_impl,
        r#"
        }}
    }}
}}
"#
    )
    .unwrap();

    let trait_impl: syn::Item = syn::parse_str(&trait_impl)
        .unwrap_or_else(|err| panic_on_syn_error("trait_impl", trait_impl, err));

    write!(
        &mut code,
        r#"
mod swig_foreign_types_map {{
    #![swig_foreigner_type = "{enum_name}"]
    #![swig_rust_type = "{rust_enum_name}"]
}}

impl SwigFrom<{rust_enum_name}> for u32 {{
   fn swig_from(x: {rust_enum_name}) -> u32 {{
       x.as_u32()
   }}
}}
"#,
        enum_name = enum_info.name,
        rust_enum_name = rust_enum_name,
    )
    .unwrap();

    write!(
        &mut code,
        r#"
impl SwigFrom<Option<{rust_enum_name}>> for Option<u32> {{
   fn swig_from(x: Option<{rust_enum_name}>) -> Option<u32> {{
        x.map(|v| match v {{
"#,
        rust_enum_name = rust_enum_name,
    )
    .unwrap();

    for (i, item) in enum_info.items.iter().enumerate() {
        write!(
            &mut code,
            r#"
           {item_name} => {index},
"#,
            index = i,
            item_name = DisplayToTokens(&item.rust_name)
        )
        .unwrap();
    }
    write!(
        &mut code,
        r#"
       }})
    }}
}}
"#
    )
    .unwrap();

    conv_map.register_exported_enum(enum_info);
    conv_map.merge(SourceId::none(), &code, pointer_target_width)?;
    Ok(vec![trait_impl.into_token_stream()])
}

fn find_suitable_ftypes_for_interace_methods(
    conv_map: &mut TypeMap,
    interace: &ForeignInterface,
    cpp_cfg: &CppConfig,
) -> Result<Vec<CppForeignMethodSignature>> {
    let void_sym = "void";
    let dummy_ty = parse_type! { () };
    let dummy_rust_ty = conv_map.find_or_alloc_rust_type_no_src_id(&dummy_ty);
    let mut f_methods = Vec::with_capacity(interace.items.len());

    for method in &interace.items {
        let mut input = Vec::<CppForeignTypeInfo>::with_capacity(method.fn_decl.inputs.len() - 1);
        for arg in method.fn_decl.inputs.iter().skip(1) {
            let arg_rust_ty = conv_map.find_or_alloc_rust_type(fn_arg_type(arg), interace.src_id);
            input.push(map_type(
                conv_map,
                cpp_cfg,
                &arg_rust_ty,
                Direction::Outgoing,
                (interace.src_id, fn_arg_type(arg).span()),
            )?);
        }
        let output = match method.fn_decl.output {
            syn::ReturnType::Default => ForeignTypeInfo {
                name: void_sym.into(),
                correspoding_rust_type: dummy_rust_ty.clone(),
            }
            .into(),
            syn::ReturnType::Type(_, ref ret_ty) => {
                let ret_rust_ty = conv_map.find_or_alloc_rust_type(ret_ty, interace.src_id);
                map_type(
                    conv_map,
                    cpp_cfg,
                    &ret_rust_ty,
                    Direction::Incoming,
                    (interace.src_id, ret_ty.span()),
                )?
            }
        };
        f_methods.push(CppForeignMethodSignature { output, input });
    }
    Ok(f_methods)
}

fn n_arguments_list(n: usize) -> String {
    (0..n)
        .map(|v| format!("a_{}", v))
        .fold(String::new(), |mut acc, x| {
            if !acc.is_empty() {
                acc.push_str(", ");
            }
            acc.push_str(&x);
            acc
        })
}

fn rust_code_generate_interface(
    conv_map: &mut TypeMap,
    pointer_target_width: usize,
    interface: &ForeignInterface,
    methods_sign: &[CppForeignMethodSignature],
) -> Result<Vec<TokenStream>> {
    use std::fmt::Write;

    let struct_with_funcs = format!("C_{}", interface.name);

    let mut code = format!(
        r#"
#[repr(C)]
#[derive(Clone)]
#[allow(non_snake_case)]
pub struct {struct_with_funcs} {{
    opaque: *const ::std::os::raw::c_void,
    {struct_with_funcs}_deref:
        extern "C" fn(_: *const ::std::os::raw::c_void),
"#,
        struct_with_funcs = struct_with_funcs,
    );
    for (method, f_method) in interface.items.iter().zip(methods_sign) {
        let args = rust_generate_args_with_types(f_method).map_err(|err| {
            DiagnosticError::new(
                interface.src_id,
                interface.span(),
                format!("gen args with types error: {}", err),
            )
        })?;
        write!(
            &mut code,
            r#"
{method_name}: extern "C" fn({args}_: *const ::std::os::raw::c_void) -> {ret_type},
"#,
            method_name = method.name,
            args = args,
            ret_type = DisplayToTokens(&f_method.output.base.correspoding_rust_type.ty),
        )
        .unwrap();
    }

    write!(
        &mut code,
        r#"
}}
"#
    )
    .unwrap();

    let mut gen_items = vec![];

    gen_items.push(
        syn::parse_str(&code)
            .unwrap_or_else(|err| panic_on_syn_error("cpp internal code", code.clone(), err)),
    );

    code.clear();
    write!(
        &mut code,
        r#"
impl SwigFrom<*const {struct_with_funcs}> for Box<{trait_name}> {{
    fn swig_from(this: *const {struct_with_funcs}) -> Self {{
       let this: &{struct_with_funcs} = unsafe {{ this.as_ref().unwrap() }};
       Box::new(this.clone())
    }}
}}
"#,
        struct_with_funcs = struct_with_funcs,
        trait_name = DisplayToTokens(&interface.self_type),
    )
    .unwrap();

    conv_map.merge(SourceId::none(), &code, pointer_target_width)?;

    code.clear();

    write!(
        &mut code,
        r#"
impl {trait_name} for {struct_with_funcs} {{
"#,
        trait_name = DisplayToTokens(&interface.self_type),
        struct_with_funcs = struct_with_funcs,
    )
    .unwrap();

    for (method, f_method) in interface.items.iter().zip(methods_sign) {
        let func_name = method
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
            .value()
            .ident
            .to_string();
        let rest_args_with_types: String = method
            .fn_decl
            .inputs
            .iter()
            .skip(1)
            .enumerate()
            .map(|(i, v)| format!("a_{}: {}", i, DisplayToTokens(fn_arg_type(v))))
            .fold(String::new(), |mut acc, x| {
                acc.push_str(", ");
                acc.push_str(&x);
                acc
            });
        let self_arg = format!("{}", DisplayToTokens(&method.fn_decl.inputs[0]));

        let args_with_types: String = [self_arg.to_string(), rest_args_with_types].concat();
        assert!(!method.fn_decl.inputs.is_empty());
        let n_args = method.fn_decl.inputs.len() - 1;
        let (mut conv_deps, convert_args) = rust_to_foreign_convert_method_inputs(
            conv_map,
            interface.src_id,
            method,
            f_method,
            (0..n_args).map(|v| format!("a_{}", v)),
            "()",
        )?;
        gen_items.append(&mut conv_deps);
        let (real_output_typename, output_conv) = match method.fn_decl.output {
            syn::ReturnType::Default => ("()".to_string(), String::new()),
            syn::ReturnType::Type(_, ref ret_ty) => {
                let real_output_type: RustType =
                    conv_map.find_or_alloc_rust_type(ret_ty, interface.src_id);
                let (mut conv_deps, conv_code) = conv_map.convert_rust_types(
                    &f_method.output.base.correspoding_rust_type,
                    &real_output_type,
                    "ret",
                    &real_output_type.normalized_name.as_str(),
                    (interface.src_id, ret_ty.span()),
                )?;
                gen_items.append(&mut conv_deps);
                (real_output_type.normalized_name.to_string(), conv_code)
            }
        };
        let ret_type = format!(
            "{}",
            DisplayToTokens(&f_method.output.base.correspoding_rust_type.ty)
        );
        write!(
            &mut code,
            r#"
    #[allow(unused_mut)]
    fn {func_name}({args_with_types}) -> {real_ret_type} {{
{convert_args}
        let ret: {ret_type} = (self.{method_name})({args}self.opaque);
{output_conv}
        ret
    }}
"#,
            func_name = func_name,
            convert_args = convert_args,
            method_name = method.name,
            args_with_types = args_with_types,
            args = if n_args == 0 {
                "".to_string()
            } else {
                n_arguments_list(n_args) + ","
            },
            real_ret_type = real_output_typename,
            ret_type = ret_type,
            output_conv = output_conv,
        )
        .unwrap();
    }
    write!(
        &mut code,
        r#"
}}
"#
    )
    .unwrap();

    write!(
        &mut code,
        r#"
impl Drop for {struct_with_funcs} {{
    fn drop(&mut self) {{
       (self.{struct_with_funcs}_deref)(self.opaque);
    }}
}}
"#,
        struct_with_funcs = struct_with_funcs
    )
    .unwrap();

    gen_items.push(
        syn::parse_str(&code)
            .unwrap_or_else(|err| panic_on_syn_error("cpp internal code", code, err)),
    );

    Ok(gen_items)
}

fn convert_rt_to_ft(tmap: &mut TypeMap, rt: RustTypeIdx) -> Result<ForeignType> {
    let rtype = tmap[rt].clone();
    tmap.map_through_conversation_to_foreign_ext(
        &rtype,
        Direction::Outgoing,
        rtype.src_id_span(),
        self::map_type::calc_this_type_for_method,
    )
    .ok_or_else(|| {
        DiagnosticError::new(
            rtype.src_id,
            rtype.ty.span(),
            format!(
                "Do not know conversation from \
                 such rust type '{}' to foreign",
                rtype
            ),
        )
    })
}
