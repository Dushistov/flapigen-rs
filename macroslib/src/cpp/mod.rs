mod cpp_code;
mod fclass;
mod fenum;
mod finterface;
mod map_type;

use std::{fmt, io::Write};

use log::{debug, trace};
use petgraph::Direction;
use proc_macro2::TokenStream;
use smol_str::SmolStr;
use syn::{parse_quote, spanned::Spanned, Type};

use crate::{
    cpp::map_type::map_type,
    error::{DiagnosticError, Result},
    file_cache::FileWriteCache,
    source_registry::SourceId,
    typemap::{
        ast::{
            parse_ty_with_given_span, parse_ty_with_given_span_checked, DisplayToTokens, TypeName,
        },
        ty::{ForeignType, RustType},
        unpack_unique_typename,
        utils::{ForeignMethodSignature, ForeignTypeInfoT},
        ForeignTypeInfo, RustTypeIdx, FROM_VAR_TEMPLATE, TO_VAR_TEMPLATE,
    },
    types::{
        ForeignEnumInfo, ForeignInterface, ForeignerClassInfo, ForeignerMethod, ItemToExpand,
        MethodAccess, MethodVariant,
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
    provides_by_module: Option<String>,
    pub(in crate::cpp) cpp_converter: Option<CppConverter>,
}

impl ForeignTypeInfoT for CppForeignTypeInfo {
    fn name(&self) -> &str {
        self.base.name.as_str()
    }
    fn correspoding_rust_type(&self) -> &RustType {
        &self.base.correspoding_rust_type
    }
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
        let provides_by_module = ftype.provides_by_module.clone();
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
            provides_by_module,
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
            provides_by_module: None,
            cpp_converter: None,
        }
    }
}

impl ForeignMethodSignature for CppForeignMethodSignature {
    type FI = CppForeignTypeInfo;
    fn output(&self) -> &ForeignTypeInfoT {
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

impl CppConfig {
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

        let m_sigs = fclass::find_suitable_foreign_types_for_methods(conv_map, class, self)?;
        let mut code_items = fclass::generate(
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

        fenum::generate_code_for_enum(&self.output_dir, enum_info)
            .map_err(|err| DiagnosticError::new(enum_info.src_id, enum_info.span(), err))?;
        let code = fenum::generate_rust_code_for_enum(conv_map, pointer_target_width, enum_info)?;
        Ok(code)
    }

    fn generate_interface(
        &self,
        conv_map: &mut TypeMap,
        pointer_target_width: usize,
        interface: &ForeignInterface,
    ) -> Result<Vec<TokenStream>> {
        let f_methods =
            finterface::find_suitable_ftypes_for_interace_methods(conv_map, interface, self)?;
        finterface::generate_for_interface(
            &self.output_dir,
            &self.namespace_name,
            interface,
            &f_methods,
        )
        .map_err(|err| DiagnosticError::new(interface.src_id, interface.span(), err))?;

        let items = finterface::rust_code_generate_interface(
            conv_map,
            pointer_target_width,
            interface,
            &f_methods,
        )?;

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

impl LanguageGenerator for CppConfig {
    fn expand_items(
        &self,
        conv_map: &mut TypeMap,
        pointer_target_width: usize,
        code: &[SourceCode],
        items: Vec<ItemToExpand>,
    ) -> Result<Vec<TokenStream>> {
        self.init(conv_map, code).map_err(|err| {
            DiagnosticError::new_without_src_info(format!("C++ initialization failure: {}", err))
        })?;
        for item in &items {
            if let ItemToExpand::Class(ref fclass) = item {
                self.register_class(conv_map, fclass)?;
            }
        }
        let mut ret = Vec::with_capacity(items.len());
        for item in items {
            match item {
                ItemToExpand::Class(fclass) => ret.append(&mut self.generate(conv_map, &fclass)?),
                ItemToExpand::Enum(fenum) => {
                    ret.append(&mut self.generate_enum(conv_map, pointer_target_width, &fenum)?)
                }
                ItemToExpand::Interface(finterface) => ret.append(&mut self.generate_interface(
                    conv_map,
                    pointer_target_width,
                    &finterface,
                )?),
            }
        }
        Ok(ret)
    }
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

fn map_write_err<Err: fmt::Display>(err: Err) -> String {
    format!("write failed: {}", err)
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

fn convert_rt_to_ft(tmap: &mut TypeMap, rt: RustTypeIdx) -> Result<ForeignType> {
    let rtype = tmap[rt].clone();
    tmap.map_through_conversation_to_foreign(
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
