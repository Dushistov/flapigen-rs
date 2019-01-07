mod cpp_code;
mod map_type;

use std::{
    io::Write,
    path::Path,
    {fmt, mem},
};

use log::{debug, trace};
use petgraph::Direction;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse_quote, spanned::Spanned, Type};

use crate::{
    ast::{
        change_span, fn_arg_type, list_lifetimes, normalize_ty_lifetimes, DisplayToTokens, RustType,
    },
    cpp::map_type::map_type,
    error::{DiagnosticError, Result},
    file_cache::FileWriteCache,
    typemap::{
        make_unique_rust_typename, unpack_unique_typename,
        utils::{
            create_suitable_types_for_constructor_and_self,
            foreign_from_rust_convert_method_output, foreign_to_rust_convert_method_inputs,
            rust_to_foreign_convert_method_inputs,
        },
        ForeignMethodSignature, ForeignTypeInfo, FROM_VAR_TEMPLATE, TO_VAR_TEMPLATE,
    },
    CppConfig, ForeignEnumInfo, ForeignInterface, ForeignerClassInfo, ForeignerMethod,
    LanguageGenerator, MethodAccess, MethodVariant, SelfTypeVariant, SourceCode, TypeMap,
};

#[derive(Debug)]
struct CppConverter {
    typename: String,
    output_converter: String,
    input_converter: String,
}

#[derive(Debug)]
struct CppForeignTypeInfo {
    base: ForeignTypeInfo,
    c_converter: String,
    pub(in crate::cpp) cpp_converter: Option<CppConverter>,
}

impl AsRef<ForeignTypeInfo> for CppForeignTypeInfo {
    fn as_ref(&self) -> &ForeignTypeInfo {
        &self.base
    }
}

impl CppForeignTypeInfo {
    fn c_need_conversation(&self) -> bool {
        !self.c_converter.is_empty()
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
            c_converter: String::new(),
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
    method: &'a ForeignerMethod,
    f_method: &'a CppForeignMethodSignature,
    c_func_name: &'a str,
    decl_func_args: &'a str,
    args_names: &'a str,
    real_output_typename: &'a str,
}

impl LanguageGenerator for CppConfig {
    fn generate(
        &self,
        conv_map: &mut TypeMap,
        _: usize,
        class: &ForeignerClassInfo,
    ) -> Result<Vec<TokenStream>> {
        debug!(
            "generate: begin for {}, this_type_for_method {:?}",
            class.name, class.this_type_for_method
        );
        class
            .validate_class()
            .map_err(|err| DiagnosticError::new(class.span(), err))?;
        if let Some(this_type_for_method) = class.this_type_for_method.as_ref() {
            let this_type: RustType = this_type_for_method.clone().into();
            let this_type = this_type.implements("SwigForeignClass");
            let void_ptr_ty = parse_type! { *mut ::std::os::raw::c_void };
            let void_ptr_ty_name = format!("{}", DisplayToTokens(&void_ptr_ty));
            let my_void_ptr_ti = RustType::new(
                void_ptr_ty.clone(),
                make_unique_rust_typename(
                    void_ptr_ty_name.clone(),
                    this_type.normalized_name.clone(),
                ),
            );
            let foreign_typename = format!("{} *", cpp_code::c_class_type(class));
            conv_map.cache_rust_to_foreign_conv(
                &this_type,
                ForeignTypeInfo {
                    correspoding_rust_type: my_void_ptr_ti,
                    name: foreign_typename,
                },
            );

            let const_void_ptr_ty = parse_type! { *const ::std::os::raw::c_void };
            let const_void_ptr_typename = format!("{}", DisplayToTokens(&const_void_ptr_ty));

            let my_const_void_ptr_ti = RustType::new(
                const_void_ptr_ty,
                make_unique_rust_typename(
                    const_void_ptr_typename,
                    this_type.normalized_name.clone(),
                ),
            );
            let my_const_void_ptr_ti2 = my_const_void_ptr_ti.clone();
            let const_foreign_typename = format!("const {} *", cpp_code::c_class_type(class));
            conv_map.cache_rust_to_foreign_conv(
                &this_type,
                ForeignTypeInfo {
                    correspoding_rust_type: my_const_void_ptr_ti,
                    name: const_foreign_typename,
                },
            );

            let this_type_ty = &this_type.ty;
            //handle foreigner_class as input arg
            conv_map.add_conversation_rule(
                my_const_void_ptr_ti2,
                parse_type! { & #this_type_ty }.into(),
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

            let my_mut_void_ptr_ti = RustType::new(
                void_ptr_ty,
                make_unique_rust_typename(void_ptr_ty_name, this_type.normalized_name.clone()),
            );
            //handle foreigner_class as input arg
            conv_map.add_conversation_rule(
                my_mut_void_ptr_ti,
                parse_type! { &mut #this_type_ty }.into(),
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
        }
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
                class.span(),
                format!(
                    "namespace {}, class {}: has methods, but no constructor\n
May be you need to use `private constructor = empty;` syntax?",
                    self.namespace_name, class.name
                ),
            ));
        }

        let m_sigs = find_suitable_foreign_types_for_methods(conv_map, class, self)?;
        let mut code_items = generate_code_for_class(
            conv_map,
            &self.output_dir,
            &self.namespace_name,
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
                enum_info.span(),
                "Too many items in enum",
            ));
        }

        trace!("enum_ti: {}", enum_info.name);
        let enum_ti: Type = syn::parse_str(&enum_info.rust_enum_name())?;
        let enum_ti: RustType = enum_ti.into();
        let enum_ti = enum_ti.implements("SwigForeignEnum");
        conv_map.add_type(enum_ti);

        cpp_code::generate_code_for_enum(&self.output_dir, enum_info)
            .map_err(|err| DiagnosticError::new(enum_info.span(), err))?;
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
        .map_err(|err| DiagnosticError::new(interface.span(), err))?;

        let items =
            rust_code_generate_interface(conv_map, pointer_target_width, interface, &f_methods)?;

        let c_struct_name = format!("C_{}", interface.name);
        let rust_struct_pointer = format!("*const {}", c_struct_name);
        let rust_ty: Type = syn::parse_str(&rust_struct_pointer)?;
        let c_struct_pointer = format!("const struct {} * const", c_struct_name);

        conv_map.add_foreign(rust_ty.into(), c_struct_pointer);

        Ok(items)
    }

    fn place_foreign_lang_helpers(&self, code: &[SourceCode]) -> std::result::Result<(), String> {
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
            input.push(map_type(
                conv_map,
                cpp_cfg,
                fn_arg_type(arg),
                Direction::Incoming,
            )?);
        }
        let output: CppForeignTypeInfo = match method.variant {
            MethodVariant::Constructor => ForeignTypeInfo {
                name: String::new(),
                correspoding_rust_type: dummy_ty.clone().into(),
            }
            .into(),
            _ => match method.fn_decl.output {
                syn::ReturnType::Default => ForeignTypeInfo {
                    name: "void".into(),
                    correspoding_rust_type: {
                        let mut ty: Type = dummy_ty.clone();
                        change_span(&mut ty, method.fn_decl.output.span());
                        ty.into()
                    },
                }
                .into(),
                syn::ReturnType::Type(_, ref rt) => {
                    map_type(conv_map, cpp_cfg, &*rt, Direction::Outgoing)?
                }
            },
        };
        ret.push(CppForeignMethodSignature { output, input });
    }
    Ok(ret)
}

fn generate_code_for_class(
    conv_map: &mut TypeMap,
    output_dir: &Path,
    namespace_name: &str,
    class: &ForeignerClassInfo,
    methods_sign: &[CppForeignMethodSignature],
) -> Result<Vec<TokenStream>> {
    use std::fmt::Write;

    let c_path = output_dir.join(format!("c_{}.h", class.name));
    let mut c_include_f = FileWriteCache::new(&c_path);
    let cpp_path = output_dir.join(format!("{}.hpp", class.name));
    let mut cpp_include_f = FileWriteCache::new(&cpp_path);
    let cpp_fwd_path = output_dir.join(format!("{}_fwd.hpp", class.name));
    let mut cpp_fwd_f = FileWriteCache::new(&cpp_fwd_path);

    let map_write_err = |err| {
        DiagnosticError::new(
            class.span(),
            format!("write to {:?} failed: {}", c_path, err),
        )
    };
    let c_class_type = cpp_code::c_class_type(class);
    let class_doc_comments = cpp_code::doc_comments_to_c_comments(&class.doc_comments, true);

    write!(
        c_include_f,
        r##"// Automaticaly generated by rust_swig
{doc_comments}
#pragma once

//for (u)intX_t types
#include <stdint.h>

#ifdef __cplusplus
static_assert(sizeof(uintptr_t) == sizeof(uint8_t) * {sizeof_usize},
   "our conversation usize <-> uintptr_t is wrong");
extern "C" {{
#endif

    typedef struct {c_class_type} {c_class_type};

"##,
        doc_comments = class_doc_comments,
        c_class_type = c_class_type,
        sizeof_usize = mem::size_of::<usize>(),
    )
    .map_err(&map_write_err)?;
    let class_name = format!("{}Wrapper", class.name);

    write!(
        cpp_include_f,
        r#"// Automaticaly generated by rust_swig
#pragma once

//for std::abort
#include <cstdlib>
//for std::move
#include <utility>
//for std::conditional
#include <type_traits>

#include "c_{class_dot_name}.h"

namespace {namespace} {{

template<bool>
class {class_name};
using {class_dot_name} = {class_name}<true>;
using {class_dot_name}Ref = {class_name}<false>;

{doc_comments}
template<bool OWN_DATA>
class {class_name} {{
public:
    using SelfType = typename std::conditional<OWN_DATA, {c_class_type} *, const {c_class_type} *>::type;
    using CForeignType = {c_class_type};
    using value_type = {class_name}<true>;
    friend class {class_name}<true>;
    friend class {class_name}<false>;

    {class_name}(const {class_name}&) = delete;
    {class_name} &operator=(const {class_name}&) = delete;
    {class_name}({class_name} &&o) noexcept: self_(o.self_)
    {{
        o.self_ = nullptr;
    }}
    {class_name} &operator=({class_name} &&o) noexcept
    {{
        assert(this != &o);
        free_mem(this->self_);
        self_ = o.self_;
        o.self_ = nullptr;
        return *this;
    }}
    explicit {class_name}(SelfType o) noexcept: self_(o) {{}}
    {c_class_type} *release() noexcept
    {{
        {c_class_type} *ret = self_;
        self_ = nullptr;
        return ret;
    }}
    explicit operator SelfType() const noexcept {{ return self_; }}
    {class_name}<false> as_rref() const noexcept {{ return {class_name}<false>{{ self_ }}; }}
    const {class_name}<true> &as_cref() const noexcept {{ return reinterpret_cast<const {class_name}<true> &>(*this); }}
"#,
        c_class_type = c_class_type,
        class_name = class_name,
        class_dot_name = class.name,
        doc_comments = class_doc_comments,
        namespace = namespace_name,
    ).map_err(&map_write_err)?;

    let dummy_ty = parse_type! { () };
    let mut gen_code = Vec::new();

    let (this_type_for_method, code_box_this) =
        if let (Some(this_type), Some(constructor_ret_type)) = (
            class.this_type_for_method.as_ref(),
            class.constructor_ret_type.as_ref(),
        ) {
            let this_type: RustType = this_type.clone().into();
            let this_type = this_type.implements("SwigForeignClass");
            debug!(
                "generate_code: add implements SwigForeignClass for {}",
                this_type.normalized_name
            );
            conv_map.add_type(this_type.clone());

            let constructor_ret_type: RustType = constructor_ret_type.clone().into();
            conv_map.add_type(constructor_ret_type);

            let (this_type_for_method, code_box_this) =
                TypeMap::convert_to_heap_pointer(&this_type, "this");
            let lifetimes = {
                let mut ret = String::new();
                let lifetimes = list_lifetimes(&this_type.ty);
                for (i, l) in lifetimes.iter().enumerate() {
                    ret.push_str(&*l.as_str());
                    if i != lifetimes.len() - 1 {
                        ret.push(',');
                    }
                }
                ret
            };
            let unpack_code = TypeMap::unpack_from_heap_pointer(&this_type, TO_VAR_TEMPLATE, true);
            gen_code.push(syn::parse_str(&format!(
                r#"impl<{lifetimes}> SwigForeignClass for {class_name} {{
    fn c_class_name() -> *const ::std::os::raw::c_char {{
        swig_c_str!("{class_name}")
    }}
    fn box_object(this: Self) -> *mut ::std::os::raw::c_void {{
{code_box_this}
        this as *mut ::std::os::raw::c_void
    }}
    fn unbox_object(p: *mut ::std::os::raw::c_void) -> Self {{
        let p = p as *mut {this_type_for_method};
{unpack_code}
       p
    }}
}}"#,
                lifetimes = lifetimes,
                class_name = DisplayToTokens(&this_type.ty),
                code_box_this = code_box_this,
                unpack_code = unpack_code.replace(TO_VAR_TEMPLATE, "p"),
                this_type_for_method = this_type_for_method.normalized_name.clone()
            ))?);
            let unpack_code = TypeMap::unpack_from_heap_pointer(&this_type, TO_VAR_TEMPLATE, true);
            let void_ptr_ty = parse_type! { *mut ::std::os::raw::c_void };
            let void_ptr_typename = format!("{}", DisplayToTokens(&void_ptr_ty));
            let my_void_ptr_ti = RustType::new(
                void_ptr_ty,
                make_unique_rust_typename(void_ptr_typename, this_type.normalized_name.clone()),
            );
            let this_type_name = this_type_for_method.normalized_name.clone();
            conv_map.add_conversation_rule(
                my_void_ptr_ti,
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

            (this_type_for_method, code_box_this)
        } else {
            (dummy_ty.clone().into(), String::new())
        };
    let no_this_info = || {
        DiagnosticError::new(
            class.span(),
            format!(
                "Class {} (namespace {}) has methods, but there is no constructor\n
May be you need to use `private constructor = empty;` syntax?",
                class.name, namespace_name,
            ),
        )
    };

    let mut need_destructor = false;
    //because of VC++ has problem with cross-references of types
    let mut inline_impl = String::new();

    for (method, f_method) in class.methods.iter().zip(methods_sign) {
        write!(
            c_include_f,
            "{}",
            cpp_code::doc_comments_to_c_comments(&method.doc_comments, false)
        )
        .map_err(&map_write_err)?;

        let method_access = match method.access {
            MethodAccess::Private => "private",
            MethodAccess::Public => "public",
            MethodAccess::Protected => "protected",
        };
        let cpp_comments = cpp_code::doc_comments_to_c_comments(&method.doc_comments, false);
        write!(cpp_include_f, "{}:\n{}", method_access, cpp_comments,).map_err(&map_write_err)?;
        let c_func_name = c_func_name(class, method, f_method);
        let c_args_with_types = cpp_code::c_generate_args_with_types(f_method, false)
            .map_err(|err| DiagnosticError::new(class.span(), err))?;
        let comma_c_args_with_types = if c_args_with_types.is_empty() {
            "".to_string()
        } else {
            format!(", {}", c_args_with_types)
        };
        let args_names = n_arguments_list(f_method.input.len());

        let cpp_args_with_types = cpp_code::cpp_generate_args_with_types(f_method)
            .map_err(|err| DiagnosticError::new(class.span(), err))?;
        let cpp_args_for_c = cpp_code::cpp_generate_args_to_call_c(f_method, false)
            .map_err(|err| DiagnosticError::new(class.span(), err))?;
        let real_output_typename = match method.fn_decl.output {
            syn::ReturnType::Default => "()".to_string(),
            syn::ReturnType::Type(_, ref t) => normalize_ty_lifetimes(&*t),
        };

        let rust_args_with_types = rust_generate_args_with_types(f_method)
            .map_err(|err| DiagnosticError::new(class.span(), err))?;
        let method_ctx = MethodContext {
            method,
            f_method,
            c_func_name: &c_func_name,
            decl_func_args: &rust_args_with_types,
            args_names: &args_names,
            real_output_typename: &real_output_typename,
        };

        let method_name = method.short_name().as_str().to_string();
        let (cpp_ret_type, convert_ret_for_cpp) =
            if let Some(cpp_converter) = f_method.output.cpp_converter.as_ref() {
                (
                    cpp_converter.typename.clone(),
                    cpp_converter
                        .output_converter
                        .replace(FROM_VAR_TEMPLATE, "ret"),
                )
            } else {
                (f_method.output.as_ref().name.clone(), "ret".to_string())
            };
        //rename types like "struct Foo" to "Foo" to make VC++ compiler happy
        let cpp_ret_type = cpp_ret_type.as_str().replace("struct", "");

        match method.variant {
            MethodVariant::StaticMethod => {
                write!(
                    c_include_f,
                    r#"
    {ret_type} {c_func_name}({args_with_types});
"#,
                    ret_type = f_method.output.as_ref().name,
                    c_func_name = c_func_name,
                    args_with_types = c_args_with_types,
                )
                .map_err(&map_write_err)?;

                if f_method.output.as_ref().name != "void" {
                    write!(
                        cpp_include_f,
                        r#"
    static {cpp_ret_type} {method_name}({cpp_args_with_types}) noexcept;
"#,
                        method_name = method_name,
                        cpp_ret_type = cpp_ret_type,
                        cpp_args_with_types = cpp_args_with_types,
                    )
                    .map_err(&map_write_err)?;
                    write!(
                        &mut inline_impl,
                        r#"
    template<bool OWN_DATA>
    inline {cpp_ret_type} {class_name}<OWN_DATA>::{method_name}({cpp_args_with_types}) noexcept
    {{
        {c_ret_type} ret = {c_func_name}({cpp_args_for_c});
        return {convert_ret_for_cpp};
    }}
"#,
                        c_ret_type = f_method.output.as_ref().name,
                        convert_ret_for_cpp = convert_ret_for_cpp,
                        cpp_args_for_c = cpp_args_for_c,
                        c_func_name = c_func_name,
                        cpp_ret_type = cpp_ret_type,
                        class_name = class_name,
                        method_name = method_name,
                        cpp_args_with_types = cpp_args_with_types,
                    )
                    .unwrap();
                } else {
                    write!(
                        cpp_include_f,
                        r#"
    static void {method_name}({cpp_args_with_types}) noexcept;
"#,
                        method_name = method_name,
                        cpp_args_with_types = cpp_args_with_types,
                    )
                    .map_err(&map_write_err)?;
                    write!(
                        &mut inline_impl,
                        r#"
    template<bool OWN_DATA>
    inline void {class_name}<OWN_DATA>::{method_name}({cpp_args_with_types}) noexcept
    {{
        {c_func_name}({cpp_args_for_c});
    }}
"#,
                        cpp_args_with_types = cpp_args_with_types,
                        class_name = class_name,
                        method_name = method_name,
                        c_func_name = c_func_name,
                        cpp_args_for_c = cpp_args_for_c,
                    )
                    .unwrap();
                }
                gen_code.append(&mut generate_static_method(conv_map, &method_ctx)?);
            }
            MethodVariant::Method(ref self_variant) => {
                let const_if_readonly = if self_variant.is_read_only() {
                    "const "
                } else {
                    ""
                };
                write!(
                    c_include_f,
                    r#"
    {ret_type} {func_name}({const_if_readonly}{c_class_type} * const self{args_with_types});
"#,
                    ret_type = f_method.output.as_ref().name,
                    c_class_type = c_class_type,
                    func_name = c_func_name,
                    args_with_types = comma_c_args_with_types,
                    const_if_readonly = const_if_readonly,
                )
                .map_err(&map_write_err)?;

                if f_method.output.as_ref().name != "void" {
                    write!(
                        cpp_include_f,
                        r#"
    {cpp_ret_type} {method_name}({cpp_args_with_types}) {const_if_readonly} noexcept;
"#,
                        method_name = method_name,
                        cpp_ret_type = cpp_ret_type,
                        cpp_args_with_types = cpp_args_with_types,
                        const_if_readonly = const_if_readonly,
                    )
                    .map_err(&map_write_err)?;
                    write!(&mut inline_impl, r#"
    template<bool OWN_DATA>
    inline {cpp_ret_type} {class_name}<OWN_DATA>::{method_name}({cpp_args_with_types}) {const_if_readonly} noexcept
    {{
        {c_ret_type} ret = {c_func_name}(this->self_{cpp_args_for_c});
        return {convert_ret_for_cpp};
    }}
"#,
                           method_name = method_name,
                           convert_ret_for_cpp = convert_ret_for_cpp,
                           c_ret_type = f_method.output.as_ref().name,
                           class_name = class_name,
                           cpp_ret_type = cpp_ret_type,
                           c_func_name = c_func_name,
                           cpp_args_with_types = cpp_args_with_types,
                                                   cpp_args_for_c = if args_names.is_empty() {
                            "".to_string()
                        } else {
                            format!(", {}", cpp_args_for_c)
                                                   },
                           const_if_readonly = const_if_readonly,
                    ).unwrap();
                } else {
                    write!(
                        cpp_include_f,
                        r#"
    void {method_name}({cpp_args_with_types}) {const_if_readonly} noexcept;
"#,
                        method_name = method_name,
                        cpp_args_with_types = cpp_args_with_types,
                        const_if_readonly = const_if_readonly,
                    )
                    .map_err(&map_write_err)?;
                    write!(&mut inline_impl, r#"
    template<bool OWN_DATA>
    inline void {class_name}<OWN_DATA>::{method_name}({cpp_args_with_types}) {const_if_readonly} noexcept
    {{
        {c_func_name}(this->self_{cpp_args_for_c});
    }}
"#,
                           method_name = method_name,
                           c_func_name = c_func_name,
                           class_name = class_name,
                           cpp_args_with_types = cpp_args_with_types,
                           cpp_args_for_c = if args_names.is_empty() {
                            "".to_string()
                        } else {
                            format!(", {}", cpp_args_for_c)
                           },
                           const_if_readonly = const_if_readonly,
                    ).unwrap();
                }

                gen_code.append(&mut generate_method(
                    conv_map,
                    &method_ctx,
                    class,
                    *self_variant,
                    &this_type_for_method,
                )?);
            }
            MethodVariant::Constructor => {
                need_destructor = true;
                if method.is_dummy_constructor() {
                    write!(
                        cpp_include_f,
                        r#"
    {class_name}() noexcept {{}}
"#,
                        class_name = class_name,
                    )
                    .map_err(&map_write_err)?;
                } else {
                    write!(
                        c_include_f,
                        r#"
    {c_class_type} *{func_name}({args_with_types});
"#,
                        c_class_type = c_class_type,
                        func_name = c_func_name,
                        args_with_types = c_args_with_types,
                    )
                    .map_err(&map_write_err)?;

                    write!(
                        cpp_include_f,
                        r#"
    {class_name}({cpp_args_with_types}) noexcept
    {{
        this->self_ = {c_func_name}({cpp_args_for_c});
        if (this->self_ == nullptr) {{
            std::abort();
        }}
    }}
"#,
                        c_func_name = c_func_name,
                        cpp_args_with_types = cpp_args_with_types,
                        class_name = class_name,
                        cpp_args_for_c = cpp_args_for_c,
                    )
                    .map_err(&map_write_err)?;

                    let constructor_ret_type = class
                        .constructor_ret_type
                        .as_ref()
                        .ok_or_else(&no_this_info)?
                        .clone();
                    let this_type = class
                        .this_type_for_method
                        .as_ref()
                        .ok_or_else(&no_this_info)?
                        .clone();
                    gen_code.append(&mut generate_constructor(
                        conv_map,
                        &method_ctx,
                        constructor_ret_type,
                        this_type,
                        &code_box_this,
                    )?);
                }
            }
        }
    }

    if need_destructor {
        let this_type: RustType = class
            .this_type_for_method
            .as_ref()
            .ok_or_else(&no_this_info)?
            .clone()
            .into();
        let unpack_code = TypeMap::unpack_from_heap_pointer(&this_type, "this", false);
        let c_destructor_name = format!("{}_delete", class.name);
        let code = format!(
            r#"
#[allow(unused_variables, unused_mut, non_snake_case)]
#[no_mangle]
pub extern "C" fn {c_destructor_name}(this: *mut {this_type}) {{
{unpack_code}
    drop(this);
}}
"#,
            c_destructor_name = c_destructor_name,
            unpack_code = unpack_code,
            this_type = this_type_for_method.normalized_name,
        );
        debug!("we generate and parse code: {}", code);
        gen_code.push(syn::parse_str(&code)?);
        write!(
            c_include_f,
            r#"
    void {c_destructor_name}(const {c_class_type} *self);
"#,
            c_class_type = c_class_type,
            c_destructor_name = c_destructor_name,
        )
        .map_err(&map_write_err)?;

        write!(
            cpp_include_f,
            r#"
private:
   static void free_mem(SelfType &p) noexcept
   {{
        if (OWN_DATA && p != nullptr) {{
            {c_destructor_name}(p);
        }}
        p = nullptr;
   }}
public:
    ~{class_name}() noexcept
    {{
        free_mem(this->self_);
    }}
"#,
            c_destructor_name = c_destructor_name,
            class_name = class_name,
        )
        .map_err(&map_write_err)?;
    } else {
        // not need_destructor
        write!(
            cpp_include_f,
            r#"
private:
   static void free_mem(SelfType &) noexcept
   {{
   }}
"#,
        )
        .map_err(&map_write_err)?;
    }

    write!(
        c_include_f,
        r#"
#ifdef __cplusplus
}}
#endif

"#
    )
    .map_err(&map_write_err)?;

    write!(
        cpp_include_f,
        r#"
{foreigner_code}
private:
    SelfType self_;
}};

{inline_impl}

}} // namespace {namespace}
"#,
        namespace = namespace_name,
        foreigner_code = class.foreigner_code,
        inline_impl = inline_impl,
    )
    .map_err(&map_write_err)?;

    write!(
        cpp_fwd_f,
        r#"// Automaticaly generated by rust_swig
#pragma once

namespace {namespace} {{
template<bool>
class {base_class_name};
using {class_name} = {base_class_name}<true>;
using {class_name}Ref = {base_class_name}<false>;
}} // namespace {namespace}
"#,
        namespace = namespace_name,
        class_name = class.name,
        base_class_name = class_name
    )
    .map_err(&map_write_err)?;

    cpp_fwd_f
        .update_file_if_necessary()
        .map_err(&map_write_err)?;
    c_include_f
        .update_file_if_necessary()
        .map_err(&map_write_err)?;
    cpp_include_f
        .update_file_if_necessary()
        .map_err(&map_write_err)?;
    Ok(gen_code)
}

fn need_cpp_helper_for_input_or_output(f_method: &CppForeignMethodSignature) -> bool {
    for ti in &f_method.input {
        if ti.c_need_conversation() {
            return true;
        }
    }
    f_method.output.c_need_conversation()
}

fn c_func_name(
    class: &ForeignerClassInfo,
    method: &ForeignerMethod,
    f_method: &CppForeignMethodSignature,
) -> String {
    format!(
        "{access}{internal}{class_name}_{func}",
        access = match method.access {
            MethodAccess::Private => "private_",
            MethodAccess::Protected => "protected_",
            MethodAccess::Public => "",
        },
        internal = if need_cpp_helper_for_input_or_output(f_method) {
            "internal_"
        } else {
            ""
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

fn generate_static_method(conv_map: &mut TypeMap, mc: &MethodContext) -> Result<Vec<TokenStream>> {
    let c_ret_type = unpack_unique_typename(
        &mc.f_method
            .output
            .as_ref()
            .correspoding_rust_type
            .normalized_name,
    );
    let (mut deps_code_out, convert_output_code) = foreign_from_rust_convert_method_output(
        conv_map,
        &mc.method.fn_decl.output,
        mc.f_method.output.as_ref(),
        "ret",
        &c_ret_type,
    )?;
    let n_args = mc.f_method.input.len();
    let (deps_code_in, convert_input_code) = foreign_to_rust_convert_method_inputs(
        conv_map,
        mc.method,
        mc.f_method,
        (0..n_args).map(|v| format!("a_{}", v)),
        &c_ret_type,
    )?;
    let code = format!(
        r#"
#[allow(non_snake_case, unused_variables, unused_mut)]
#[no_mangle]
pub extern "C" fn {func_name}({decl_func_args}) -> {c_ret_type} {{
{convert_input_code}
    let mut ret: {real_output_typename} = {rust_func_name}({args_names});
{convert_output_code}
    ret
}}
"#,
        func_name = mc.c_func_name,
        decl_func_args = mc.decl_func_args,
        c_ret_type = c_ret_type,
        convert_input_code = convert_input_code,
        rust_func_name = DisplayToTokens(&mc.method.rust_id),
        args_names = mc.args_names,
        convert_output_code = convert_output_code,
        real_output_typename = mc.real_output_typename,
    );
    let mut gen_code = deps_code_in;
    gen_code.append(&mut deps_code_out);
    gen_code.push(syn::parse_str(&code)?);
    Ok(gen_code)
}

fn generate_method(
    conv_map: &mut TypeMap,
    mc: &MethodContext,
    class: &ForeignerClassInfo,
    self_variant: SelfTypeVariant,
    this_type_for_method: &RustType,
) -> Result<Vec<TokenStream>> {
    let c_ret_type = unpack_unique_typename(
        &mc.f_method
            .output
            .as_ref()
            .correspoding_rust_type
            .normalized_name,
    );
    let n_args = mc.f_method.input.len();
    let (deps_code_in, convert_input_code) = foreign_to_rust_convert_method_inputs(
        conv_map,
        mc.method,
        mc.f_method,
        (0..n_args).map(|v| format!("a_{}", v)),
        &c_ret_type,
    )?;
    let (mut deps_code_out, convert_output_code) = foreign_from_rust_convert_method_output(
        conv_map,
        &mc.method.fn_decl.output,
        mc.f_method.output.as_ref(),
        "ret",
        &c_ret_type,
    )?;
    //&mut constructor_real_type -> &mut class.self_type
    let (from_ty, to_ty): (Type, Type) = create_suitable_types_for_constructor_and_self(
        self_variant,
        class,
        &this_type_for_method.ty,
    );
    let this_type_ref = normalize_ty_lifetimes(&from_ty);
    let (mut deps_this, convert_this) = conv_map.convert_rust_types(
        &from_ty.into(),
        &to_ty.into(),
        "this",
        &c_ret_type,
        mc.method.span(),
    )?;
    let code = format!(
        r#"
#[allow(non_snake_case, unused_variables, unused_mut)]
#[no_mangle]
pub extern "C" fn {func_name}(this: *mut {this_type}, {decl_func_args}) -> {c_ret_type} {{
{convert_input_code}
    let this: {this_type_ref} = unsafe {{
        this.as_mut().unwrap()
    }};
{convert_this}
    let mut ret: {real_output_typename} = {rust_func_name}(this, {args_names});
{convert_output_code}
    ret
}}
"#,
        func_name = mc.c_func_name,
        decl_func_args = mc.decl_func_args,
        convert_input_code = convert_input_code,
        c_ret_type = c_ret_type,
        this_type_ref = this_type_ref,
        this_type = this_type_for_method.normalized_name,
        convert_this = convert_this,
        rust_func_name = DisplayToTokens(&mc.method.rust_id),
        args_names = mc.args_names,
        convert_output_code = convert_output_code,
        real_output_typename = mc.real_output_typename,
    );

    let mut gen_code = deps_code_in;
    gen_code.append(&mut deps_code_out);
    gen_code.append(&mut deps_this);
    gen_code.push(syn::parse_str(&code)?);
    Ok(gen_code)
}

fn generate_constructor(
    conv_map: &mut TypeMap,
    mc: &MethodContext,
    construct_ret_type: Type,
    this_type: Type,
    code_box_this: &str,
) -> Result<Vec<TokenStream>> {
    let n_args = mc.f_method.input.len();
    let this_type: RustType = this_type.into();
    let ret_type_name = this_type.normalized_name.as_str();
    let (deps_code_in, convert_input_code) = foreign_to_rust_convert_method_inputs(
        conv_map,
        mc.method,
        mc.f_method,
        (0..n_args).map(|v| format!("a_{}", v)),
        &ret_type_name,
    )?;
    let construct_ret_type: RustType = construct_ret_type.into();
    let (mut deps_this, convert_this) = conv_map.convert_rust_types(
        &construct_ret_type,
        &this_type,
        "this",
        &ret_type_name,
        mc.method.span(),
    )?;

    let code = format!(
        r#"
#[no_mangle]
#[allow(unused_variables, unused_mut, non_snake_case)]
pub extern "C" fn {func_name}({decl_func_args}) -> *const ::std::os::raw::c_void {{
{convert_input_code}
    let this: {real_output_typename} = {rust_func_name}({args_names});
{convert_this}
{box_this}
    this as *const ::std::os::raw::c_void
}}
"#,
        func_name = mc.c_func_name,
        convert_this = convert_this,
        decl_func_args = mc.decl_func_args,
        convert_input_code = convert_input_code,
        rust_func_name = DisplayToTokens(&mc.method.rust_id),
        args_names = mc.args_names,
        box_this = code_box_this,
        real_output_typename = &construct_ret_type.normalized_name.as_str(),
    );
    let mut gen_code = deps_code_in;
    gen_code.append(&mut deps_this);
    gen_code.push(syn::parse_str(&code)?);
    Ok(gen_code)
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
        write!(
            &mut code,
            "{index} => {item_name},\n",
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
        write!(
            &mut code,
            "{index} => {item_name},\n",
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

    let trait_impl: syn::Item = syn::parse_str(&trait_impl)?;

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
    conv_map.merge(
        &*enum_info.rust_enum_name().as_str(),
        &code,
        pointer_target_width,
    )?;
    Ok(vec![trait_impl.into_token_stream()])
}

fn find_suitable_ftypes_for_interace_methods(
    conv_map: &mut TypeMap,
    interace: &ForeignInterface,
    cpp_cfg: &CppConfig,
) -> Result<Vec<CppForeignMethodSignature>> {
    let void_sym = "void";
    let dummy_ty = parse_type! { () };
    let mut f_methods = vec![];

    for method in &interace.items {
        let mut input = Vec::<CppForeignTypeInfo>::with_capacity(method.fn_decl.inputs.len() - 1);
        for arg in method.fn_decl.inputs.iter().skip(1) {
            input.push(map_type(
                conv_map,
                cpp_cfg,
                fn_arg_type(arg),
                Direction::Outgoing,
            )?);
        }
        let output = match method.fn_decl.output {
            syn::ReturnType::Default => ForeignTypeInfo {
                name: void_sym.into(),
                correspoding_rust_type: {
                    let mut ty: Type = dummy_ty.clone();
                    change_span(&mut ty, method.fn_decl.output.span());
                    ty.into()
                },
            }
            .into(),
            syn::ReturnType::Type(_, ref ret_ty) => {
                map_type(conv_map, cpp_cfg, ret_ty, Direction::Incoming)?
            }
        };
        f_methods.push(CppForeignMethodSignature {
            output: output,
            input,
        });
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

    gen_items.push(syn::parse_str(&code)?);

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

    conv_map.merge(
        &format!("{}", DisplayToTokens(&interface.self_type)),
        &code,
        pointer_target_width,
    )?;

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
                DiagnosticError::new(method.rust_name.span(), "Empty trait function name")
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
            method,
            f_method,
            (0..n_args).map(|v| format!("a_{}", v)),
            "()",
        )?;
        gen_items.append(&mut conv_deps);
        let (real_output_typename, output_conv) = match method.fn_decl.output {
            syn::ReturnType::Default => ("()".to_string(), String::new()),
            syn::ReturnType::Type(_, ref ret_ty) => {
                let real_output_type: RustType = (**ret_ty).clone().into();
                let (mut conv_deps, conv_code) = conv_map.convert_rust_types(
                    &f_method.output.base.correspoding_rust_type,
                    &real_output_type,
                    "ret",
                    &real_output_type.normalized_name.as_str(),
                    ret_ty.span(),
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

    gen_items.push(syn::parse_str(&code)?);

    Ok(gen_items)
}
