mod cpp_code;
mod map_type;

use std::io::Write;
use std::path::Path;
use std::{fmt, mem};

use petgraph::Direction;
use syntex_pos::DUMMY_SP;
use syntex_syntax::ast;
use syntex_syntax::ast::DUMMY_NODE_ID;
use syntex_syntax::parse::{PResult, ParseSess};
use syntex_syntax::print::pprust;
use syntex_syntax::ptr::P;
use syntex_syntax::symbol::Symbol;

use self::map_type::map_type;
use errors::fatal_error;
use file_cache::FileWriteCache;
use my_ast::{code_to_item, get_ref_type, list_lifetimes, normalized_ty_string, parse_ty,
             self_variant, RustType};
use types_conv_map::utils::{create_suitable_types_for_constructor_and_self,
                            foreign_from_rust_convert_method_output,
                            foreign_to_rust_convert_method_inputs,
                            rust_to_foreign_convert_method_inputs};
use types_conv_map::{make_unique_rust_typename, unpack_unique_typename, ForeignMethodSignature,
                     ForeignTypeInfo, FROM_VAR_TEMPLATE, TO_VAR_TEMPLATE};
use {CppConfig, ForeignEnumInfo, ForeignInterface, ForeignerClassInfo, ForeignerMethod,
     LanguageGenerator, MethodVariant, SelfTypeVariant, SourceCode, TypesConvMap};

struct CppConverter {
    typename: Symbol,
    output_converter: String,
    input_converter: String,
}

struct CppForeignTypeInfo {
    base: ForeignTypeInfo,
    c_converter: String,
    pub(in cpp) cpp_converter: Option<CppConverter>,
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
    fn generate<'a>(
        &self,
        sess: &'a ParseSess,
        conv_map: &mut TypesConvMap,
        _: usize,
        class: &ForeignerClassInfo,
    ) -> PResult<'a, Vec<P<ast::Item>>> {
        debug!(
            "generate: begin for {}, this_type_for_method {:?}",
            class.name, class.this_type_for_method
        );
        if let Some(this_type_for_method) = class.this_type_for_method.as_ref() {
            let this_type: RustType = this_type_for_method.clone().into();
            let this_type = this_type.implements("SwigForeignClass");
            let void_ptr_typename = Symbol::intern("*mut ::std::os::raw::c_void");
            let my_void_ptr_ti = RustType::new(
                parse_ty(sess, DUMMY_SP, void_ptr_typename)?,
                make_unique_rust_typename(void_ptr_typename, this_type.normalized_name),
            );
            let foreign_typename = Symbol::intern(&format!("{} *", cpp_code::c_class_type(class)));
            conv_map.cache_rust_to_foreign_conv(
                &this_type,
                ForeignTypeInfo {
                    correspoding_rust_type: my_void_ptr_ti,
                    name: foreign_typename,
                },
            );

            let const_void_ptr_typename = Symbol::intern("*const ::std::os::raw::c_void");
            let my_const_void_ptr_ti = RustType::new(
                parse_ty(sess, DUMMY_SP, const_void_ptr_typename)?,
                make_unique_rust_typename(const_void_ptr_typename, this_type.normalized_name),
            );
            let my_const_void_ptr_ti2 = my_const_void_ptr_ti.clone();
            let const_foreign_typename =
                Symbol::intern(&format!("const {} *", cpp_code::c_class_type(class)));
            conv_map.cache_rust_to_foreign_conv(
                &this_type,
                ForeignTypeInfo {
                    correspoding_rust_type: my_const_void_ptr_ti,
                    name: const_foreign_typename,
                },
            );

            conv_map.add_conversation_rule(
                my_const_void_ptr_ti2,
                get_ref_type(&this_type.ty, ast::Mutability::Immutable).into(),
                Symbol::intern(&format!(
                    r#"
    assert!(!{from_var}.is_null());
    let {to_var}: &{this_type} = unsafe {{ &*({from_var} as *const {this_type}) }};
"#,
                    to_var = TO_VAR_TEMPLATE,
                    from_var = FROM_VAR_TEMPLATE,
                    this_type = this_type.normalized_name,
                )).into(),
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
            return Err(fatal_error(
                sess,
                class.span,
                &format!(
                    "namespace {}, class {}: has methods, but no constructor",
                    self.namespace_name, class.name
                ),
            ));
        }

        let m_sigs = find_suitable_foreign_types_for_methods(sess, conv_map, class, self)?;
        let mut code_items = generate_code_for_class(
            sess,
            conv_map,
            &self.output_dir,
            &self.namespace_name,
            class,
            &m_sigs,
        )?;
        code_items.append(&mut self.to_generate.borrow_mut());
        Ok(code_items)
    }

    fn generate_enum<'a>(
        &self,
        sess: &'a ParseSess,
        conv_map: &mut TypesConvMap,
        pointer_target_width: usize,
        enum_info: &ForeignEnumInfo,
    ) -> PResult<'a, Vec<P<ast::Item>>> {
        if (enum_info.items.len() as u64) >= u64::from(u32::max_value()) {
            return Err(fatal_error(sess, enum_info.span, "Too many items in enum"));
        }
        cpp_code::generate_code_for_enum(&self.output_dir, enum_info)
            .map_err(|err| fatal_error(sess, enum_info.span, &err))?;
        generate_rust_code_for_enum(sess, conv_map, pointer_target_width, enum_info)
    }

    fn generate_interface<'a>(
        &self,
        sess: &'a ParseSess,
        conv_map: &mut TypesConvMap,
        pointer_target_width: usize,
        interface: &ForeignInterface,
    ) -> PResult<'a, Vec<P<ast::Item>>> {
        let f_methods = find_suitable_ftypes_for_interace_methods(sess, conv_map, interface)?;
        cpp_code::generate_for_interface(
            &self.output_dir,
            &self.namespace_name,
            interface,
            &f_methods,
        ).map_err(|err| fatal_error(sess, interface.span, &err))?;

        let items = rust_code_generate_interface(
            sess,
            conv_map,
            pointer_target_width,
            interface,
            &f_methods,
        )?;

        let c_struct_name = format!("C_{}", interface.name);
        let rust_struct_pointer = Symbol::intern(&format!("*const {}", c_struct_name));
        let rust_ty = parse_ty(sess, DUMMY_SP, rust_struct_pointer)?;
        let c_struct_pointer = Symbol::intern(&format!("const struct {} * const", c_struct_name));

        conv_map.add_foreign(rust_ty.into(), c_struct_pointer);

        Ok(items)
    }

    fn place_foreign_lang_helpers(&self, code: &[SourceCode]) -> Result<(), String> {
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

fn find_suitable_foreign_types_for_methods<'a>(
    sess: &'a ParseSess,
    conv_map: &mut TypesConvMap,
    class: &ForeignerClassInfo,
    cpp_cfg: &CppConfig,
) -> PResult<'a, Vec<CppForeignMethodSignature>> {
    let mut ret = Vec::<CppForeignMethodSignature>::with_capacity(class.methods.len());
    let empty_symbol = Symbol::intern("");
    let dummy_ty = ast::Ty {
        id: DUMMY_NODE_ID,
        span: DUMMY_SP,
        node: ast::TyKind::Tup(vec![]),
    };

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
                sess,
                conv_map,
                cpp_cfg,
                &arg.ty,
                Direction::Incoming,
            )?);
        }
        let output: CppForeignTypeInfo = match method.variant {
            MethodVariant::Constructor => ForeignTypeInfo {
                name: empty_symbol,
                correspoding_rust_type: dummy_ty.clone().into(),
            }.into(),
            _ => match method.fn_decl.output {
                ast::FunctionRetTy::Default(sp) => ForeignTypeInfo {
                    name: Symbol::intern("void"),
                    correspoding_rust_type: {
                        let mut ty: ast::Ty = dummy_ty.clone();
                        ty.span = sp;
                        ty.into()
                    },
                }.into(),
                ast::FunctionRetTy::Ty(ref rt) => {
                    map_type(sess, conv_map, cpp_cfg, &*rt, Direction::Outgoing)?
                }
            },
        };
        ret.push(CppForeignMethodSignature { output, input });
    }
    Ok(ret)
}

fn generate_code_for_class<'a>(
    sess: &'a ParseSess,
    conv_map: &mut TypesConvMap,
    output_dir: &Path,
    namespace_name: &str,
    class: &ForeignerClassInfo,
    methods_sign: &[CppForeignMethodSignature],
) -> PResult<'a, Vec<P<ast::Item>>> {
    use std::fmt::Write;

    let c_path = output_dir.join(format!("c_{}.h", class.name));
    let mut c_include_f = FileWriteCache::new(&c_path);
    let cpp_path = output_dir.join(format!("{}.hpp", class.name));
    let mut cpp_include_f = FileWriteCache::new(&cpp_path);

    let map_write_err = |err| {
        fatal_error(
            sess,
            class.span,
            &format!("write to {:?} failed: {}", c_path, err),
        )
    };
    let c_class_type = cpp_code::c_class_type(class);
    let class_doc_comments = cpp_code::doc_comments_to_c_comments(&class.doc_comments, true);
    let mut cpp_class_ref_code = String::new();
    write!(
        c_include_f,
        r##"// Automaticaly generated by rust_swig
{doc_comments}
#pragma once

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
    ).map_err(&map_write_err)?;

    write!(
        cpp_include_f,
        r#"// Automaticaly generated by rust_swig
#pragma once

#include <cstdlib>
#include <type_traits>

#include "c_{class_name}.h"
namespace {namespace} {{
{doc_comments}
class {class_name} {{
public:
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
    explicit {class_name}({c_class_type} *o) noexcept: self_(o) {{}}
    {c_class_type} *release() noexcept
    {{
        {c_class_type} *ret = self_;
        self_ = nullptr;
        return ret;
    }}
    explicit operator {c_class_type}*() const noexcept {{ return self_; }}
"#,
        c_class_type = c_class_type,
        class_name = class.name,
        doc_comments = class_doc_comments,
        namespace = namespace_name,
    ).map_err(&map_write_err)?;
    let class_ref_name = format!("{}Ref", class.name);
    write!(
        &mut cpp_class_ref_code,
        r##"
{doc_comments}
class {class_name} {{
public:
    using CForeignType = {c_class_type};
    using value_type = {value_class_type};

    {class_name}(const {class_name}&) = delete;
    {class_name} &operator=(const {class_name}&) = delete;
    {class_name}({class_name} &&o) noexcept: self_(o.self_)
    {{
        o.self_ = nullptr;
    }}
    {class_name} &operator=({class_name} &&o) noexcept
    {{
        assert(this != &o);
        self_ = o.self_;
        o.self_ = nullptr;
        return *this;
    }}
    explicit {class_name}(const {c_class_type} *o) noexcept : self_(o) {{}}
    explicit operator const {c_class_type}*() const noexcept {{ return self_; }}
"##,
        class_name = class_ref_name,
        c_class_type = c_class_type,
        doc_comments = class_doc_comments,
        value_class_type = class.name,
    ).unwrap();

    let dummy_ty = ast::Ty {
        id: DUMMY_NODE_ID,
        node: ast::TyKind::Tup(vec![]),
        span: DUMMY_SP,
    };
    let mut gen_code = Vec::<P<ast::Item>>::new();

    let (this_type_for_method, code_box_this) = if let (
        Some(this_type),
        Some(constructor_ret_type),
    ) = (
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
            TypesConvMap::convert_to_heap_pointer(&this_type, "this");
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
        let unpack_code = TypesConvMap::unpack_from_heap_pointer(&this_type, TO_VAR_TEMPLATE, true);
        gen_code.append(&mut code_to_item(
            sess,
            &class.name.as_str(),
            &format!(
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
                class_name = pprust::ty_to_string(&this_type.ty),
                code_box_this = code_box_this,
                unpack_code = unpack_code.replace(TO_VAR_TEMPLATE, "p"),
                this_type_for_method = this_type_for_method.normalized_name
            ),
        )?);

        let unpack_code =
            TypesConvMap::unpack_from_heap_pointer(&this_type_for_method, TO_VAR_TEMPLATE, true);
        let void_ptr_typename = Symbol::intern("*mut ::std::os::raw::c_void");
        let my_void_ptr_ti = RustType::new(
            parse_ty(sess, DUMMY_SP, void_ptr_typename)?,
            make_unique_rust_typename(void_ptr_typename, this_type.normalized_name),
        );

        conv_map.add_conversation_rule(
            my_void_ptr_ti,
            this_type,
            Symbol::intern(&format!(
                r#"
    let {to_var}: *mut {this_type} = {from_var} as *mut {this_type};
{unpack_code}
"#,
                to_var = TO_VAR_TEMPLATE,
                from_var = FROM_VAR_TEMPLATE,
                this_type = this_type_for_method.normalized_name,
                unpack_code = unpack_code,
            )).into(),
        );

        (this_type_for_method, code_box_this)
    } else {
        (dummy_ty.clone().into(), String::new())
    };
    let no_this_info = || {
        fatal_error(
            sess,
            class.span,
            &format!(
                "Class {} (namespace {}) have methods, but there is no constructor",
                class.name, namespace_name,
            ),
        )
    };

    let mut need_destructor = false;

    for (method, f_method) in class.methods.iter().zip(methods_sign) {
        write!(
            c_include_f,
            "{}",
            cpp_code::doc_comments_to_c_comments(&method.doc_comments, false)
        ).map_err(&map_write_err)?;

        let method_access = if method.foreigner_private {
            "private"
        } else {
            "public"
        };
        let cpp_comments = cpp_code::doc_comments_to_c_comments(&method.doc_comments, false);
        write!(cpp_include_f, "{}:\n{}", method_access, cpp_comments,).map_err(&map_write_err)?;
        let c_func_name = c_func_name(class, method, f_method);
        let c_args_with_types = cpp_code::c_generate_args_with_types(f_method, false)
            .map_err(|err| fatal_error(sess, class.span, &err))?;
        let comma_c_args_with_types = if c_args_with_types.is_empty() {
            "".to_string()
        } else {
            format!(", {}", c_args_with_types)
        };
        let args_names = n_arguments_list(f_method.input.len());

        let cpp_args_with_types = cpp_code::cpp_generate_args_with_types(f_method)
            .map_err(|err| fatal_error(sess, class.span, &err))?;
        let cpp_args_for_c = cpp_code::cpp_generate_args_to_call_c(f_method)
            .map_err(|err| fatal_error(sess, class.span, &err))?;
        let real_output_typename = match method.fn_decl.output {
            ast::FunctionRetTy::Default(_) => "()".to_string(),
            ast::FunctionRetTy::Ty(ref t) => normalized_ty_string(&*t),
        };

        let rust_args_with_types = rust_generate_args_with_types(f_method)
            .map_err(|err| fatal_error(sess, class.span, &err))?;
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
                    cpp_converter.typename,
                    cpp_converter
                        .output_converter
                        .replace(FROM_VAR_TEMPLATE, "ret"),
                )
            } else {
                (f_method.output.as_ref().name, "ret".to_string())
            };

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
                ).map_err(&map_write_err)?;

                if f_method.output.as_ref().name != "void" {
                    write!(
                        cpp_include_f,
                        r#"
    static {cpp_ret_type} {method_name}({cpp_args_with_types})
    {{
        {c_ret_type} ret = {c_func_name}({cpp_args_for_c});
        return {convert_ret_for_cpp};
    }}
"#,
                        method_name = method_name,
                        c_ret_type = f_method.output.as_ref().name,
                        cpp_ret_type = cpp_ret_type,
                        convert_ret_for_cpp = convert_ret_for_cpp,
                        c_func_name = c_func_name,
                        cpp_args_with_types = cpp_args_with_types,
                        cpp_args_for_c = cpp_args_for_c,
                    ).map_err(&map_write_err)?;
                } else {
                    write!(
                        cpp_include_f,
                        r#"
    static void {method_name}({cpp_args_with_types})
    {{
        {c_func_name}({cpp_args_for_c});
    }}
"#,
                        method_name = method_name,
                        c_func_name = c_func_name,
                        cpp_args_with_types = cpp_args_with_types,
                        cpp_args_for_c = cpp_args_for_c,
                    ).map_err(&map_write_err)?;
                }
                gen_code.append(&mut generate_static_method(sess, conv_map, &method_ctx)?);
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
                ).map_err(&map_write_err)?;

                let cpp_code = if f_method.output.as_ref().name != "void" {
                    format!(
                        r#"
    {cpp_ret_type} {method_name}({cpp_args_with_types}) {const_if_readonly}
    {{
        {c_ret_type} ret = {c_func_name}(this->self_{cpp_args_for_c});
        return {convert_ret_for_cpp};
    }}
"#,
                        method_name = method_name,
                        c_ret_type = f_method.output.as_ref().name,
                        convert_ret_for_cpp = convert_ret_for_cpp,
                        cpp_ret_type = cpp_ret_type,
                        c_func_name = c_func_name,
                        cpp_args_with_types = cpp_args_with_types,
                        cpp_args_for_c = if args_names.is_empty() {
                            "".to_string()
                        } else {
                            format!(", {}", cpp_args_for_c)
                        },
                        const_if_readonly = const_if_readonly,
                    )
                } else {
                    format!(
                        r#"
    void {method_name}({cpp_args_with_types}) {const_if_readonly}
    {{
        {c_func_name}(this->self_{cpp_args_for_c});
    }}
"#,
                        method_name = method_name,
                        c_func_name = c_func_name,
                        cpp_args_with_types = cpp_args_with_types,
                        cpp_args_for_c = if args_names.is_empty() {
                            "".to_string()
                        } else {
                            format!(", {}", cpp_args_for_c)
                        },
                        const_if_readonly = const_if_readonly,
                    )
                };
                cpp_include_f
                    .write_all(cpp_code.as_bytes())
                    .map_err(&map_write_err)?;
                if self_variant.is_read_only() {
                    write!(
                        &mut cpp_class_ref_code,
                        "{}:\n{}",
                        method_access, cpp_comments,
                    ).unwrap();
                    cpp_class_ref_code.push_str(&cpp_code);
                }
                gen_code.append(&mut generate_method(
                    sess,
                    conv_map,
                    &method_ctx,
                    class,
                    *self_variant,
                    &this_type_for_method,
                )?);
            }
            MethodVariant::Constructor => {
                need_destructor = true;
                write!(
                    c_include_f,
                    r#"
    {c_class_type} *{func_name}({args_with_types});
"#,
                    c_class_type = c_class_type,
                    func_name = c_func_name,
                    args_with_types = c_args_with_types,
                ).map_err(&map_write_err)?;

                write!(
                    cpp_include_f,
                    r#"
    {class_name}({cpp_args_with_types})
    {{
        this->self_ = {c_func_name}({cpp_args_for_c});
        if (this->self_ == nullptr) {{
            std::abort();
        }}
    }}
"#,
                    c_func_name = c_func_name,
                    cpp_args_with_types = cpp_args_with_types,
                    class_name = class.name,
                    cpp_args_for_c = cpp_args_for_c,
                ).map_err(&map_write_err)?;
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
                    sess,
                    conv_map,
                    &method_ctx,
                    constructor_ret_type,
                    this_type,
                    &code_box_this,
                )?);
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
        let unpack_code = TypesConvMap::unpack_from_heap_pointer(&this_type, "this", false);
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
        gen_code.append(&mut code_to_item(sess, &c_destructor_name, &code)?);
        write!(
            c_include_f,
            r#"
    void {c_destructor_name}(const {c_class_type} *self);
"#,
            c_class_type = c_class_type,
            c_destructor_name = c_destructor_name,
        ).map_err(&map_write_err)?;

        write!(
            cpp_include_f,
            r#"
private:
   static void free_mem({c_class_type} *&p) noexcept
   {{
        if (p != nullptr) {{
            {c_destructor_name}(p);
            p = nullptr;
        }}
   }}
public:
    ~{class_name}() noexcept
    {{
        free_mem(this->self_);
    }}
"#,
            c_class_type = c_class_type,
            c_destructor_name = c_destructor_name,
            class_name = class.name,
        ).map_err(&map_write_err)?;
    } else {
        //need_destructor
        write!(
            cpp_include_f,
            r#"
private:
   static void free_mem({c_class_type} *&) noexcept
   {{
   }}
"#,
            c_class_type = c_class_type
        ).map_err(&map_write_err)?;
    }

    write!(
        c_include_f,
        r#"
#ifdef __cplusplus
}}
#endif

"#
    ).map_err(&map_write_err)?;

    write!(
        &mut cpp_class_ref_code,
        r#"
private:
    const {c_class_type} *self_;
}};
"#,
        c_class_type = c_class_type,
    ).unwrap();

    write!(
        cpp_include_f,
        r#"
{foreigner_code}
private:
    {c_class_type} *self_;
}};

{cpp_class_ref_code}
}}//{namespace} {{
"#,
        c_class_type = c_class_type,
        namespace = namespace_name,
        foreigner_code = class.foreigner_code,
        cpp_class_ref_code = cpp_class_ref_code,
    ).map_err(&map_write_err)?;

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
        access = if method.foreigner_private {
            "private_"
        } else {
            ""
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

fn rust_generate_args_with_types(f_method: &CppForeignMethodSignature) -> Result<String, String> {
    use std::fmt::Write;

    let mut buf = String::new();
    for (i, f_type_info) in f_method.input.iter().enumerate() {
        write!(
            &mut buf,
            "a_{}: {}, ",
            i,
            unpack_unique_typename(f_type_info.as_ref().correspoding_rust_type.normalized_name),
        ).map_err(fmt_write_err_map)?;
    }
    Ok(buf)
}

fn fmt_write_err_map(err: fmt::Error) -> String {
    format!("fmt write error: {}", err)
}

fn generate_static_method<'a>(
    sess: &'a ParseSess,
    conv_map: &mut TypesConvMap,
    mc: &MethodContext,
) -> PResult<'a, Vec<P<ast::Item>>> {
    let c_ret_type = unpack_unique_typename(
        mc.f_method
            .output
            .as_ref()
            .correspoding_rust_type
            .normalized_name,
    );
    let (mut deps_code_out, convert_output_code) = foreign_from_rust_convert_method_output(
        sess,
        conv_map,
        &mc.method.fn_decl.output,
        mc.f_method.output.as_ref(),
        "ret",
        &c_ret_type.as_str(),
    )?;
    let n_args = mc.f_method.input.len();
    let (deps_code_in, convert_input_code) = foreign_to_rust_convert_method_inputs(
        sess,
        conv_map,
        mc.method,
        mc.f_method,
        (0..n_args).map(|v| format!("a_{}", v)),
        &c_ret_type.as_str(),
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
        rust_func_name = mc.method.rust_id,
        args_names = mc.args_names,
        convert_output_code = convert_output_code,
        real_output_typename = mc.real_output_typename,
    );
    let mut gen_code = deps_code_in;
    gen_code.append(&mut deps_code_out);
    gen_code.append(&mut code_to_item(sess, mc.c_func_name, &code)?);
    Ok(gen_code)
}

fn generate_method<'a>(
    sess: &'a ParseSess,
    conv_map: &mut TypesConvMap,
    mc: &MethodContext,
    class: &ForeignerClassInfo,
    self_variant: SelfTypeVariant,
    this_type_for_method: &RustType,
) -> PResult<'a, Vec<P<ast::Item>>> {
    let c_ret_type = unpack_unique_typename(
        mc.f_method
            .output
            .as_ref()
            .correspoding_rust_type
            .normalized_name,
    );
    let n_args = mc.f_method.input.len();
    let (deps_code_in, convert_input_code) = foreign_to_rust_convert_method_inputs(
        sess,
        conv_map,
        mc.method,
        mc.f_method,
        (0..n_args).map(|v| format!("a_{}", v)),
        &c_ret_type.as_str(),
    )?;
    let (mut deps_code_out, convert_output_code) = foreign_from_rust_convert_method_output(
        sess,
        conv_map,
        &mc.method.fn_decl.output,
        mc.f_method.output.as_ref(),
        "ret",
        &c_ret_type.as_str(),
    )?;
    //&mut constructor_real_type -> &mut class.self_type
    let (from_ty, to_ty): (ast::Ty, ast::Ty) = create_suitable_types_for_constructor_and_self(
        self_variant,
        class,
        &this_type_for_method.ty,
    );
    let this_type_ref = normalized_ty_string(&from_ty);
    let (mut deps_this, convert_this) = conv_map.convert_rust_types(
        sess,
        &from_ty.into(),
        &to_ty.into(),
        "this",
        &c_ret_type.as_str(),
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
        rust_func_name = mc.method.rust_id,
        args_names = mc.args_names,
        convert_output_code = convert_output_code,
        real_output_typename = mc.real_output_typename,
    );

    let mut gen_code = deps_code_in;
    gen_code.append(&mut deps_code_out);
    gen_code.append(&mut deps_this);
    gen_code.append(&mut code_to_item(sess, mc.c_func_name, &code)?);
    Ok(gen_code)
}

fn generate_constructor<'a>(
    sess: &'a ParseSess,
    conv_map: &mut TypesConvMap,
    mc: &MethodContext,
    construct_ret_type: ast::Ty,
    this_type: ast::Ty,
    code_box_this: &str,
) -> PResult<'a, Vec<P<ast::Item>>> {
    let n_args = mc.f_method.input.len();
    let this_type: RustType = this_type.into();
    let ret_type_name = this_type.normalized_name.as_str();
    let (deps_code_in, convert_input_code) = foreign_to_rust_convert_method_inputs(
        sess,
        conv_map,
        mc.method,
        mc.f_method,
        (0..n_args).map(|v| format!("a_{}", v)),
        &ret_type_name,
    )?;
    let construct_ret_type: RustType = construct_ret_type.into();
    let (mut deps_this, convert_this) = conv_map.convert_rust_types(
        sess,
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
        rust_func_name = mc.method.rust_id,
        args_names = mc.args_names,
        box_this = code_box_this,
        real_output_typename = &construct_ret_type.normalized_name.as_str(),
    );
    let mut gen_code = deps_code_in;
    gen_code.append(&mut deps_this);
    gen_code.append(&mut code_to_item(sess, mc.c_func_name, &code)?);
    Ok(gen_code)
}

fn generate_rust_code_for_enum<'a>(
    sess: &'a ParseSess,
    conv_map: &mut TypesConvMap,
    pointer_target_width: usize,
    enum_info: &ForeignEnumInfo,
) -> PResult<'a, Vec<P<ast::Item>>> {
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
            item_name = item.rust_name
        ).unwrap();
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
    ).unwrap();

    write!(
        &mut code,
        r#"
impl SwigFrom<Option<u32>> for Option<{rust_enum_name}> {{
    fn swig_from(x: Option<u32>) -> Option<{rust_enum_name}> {{
        x.map(|v| match v {{

"#,
        rust_enum_name = rust_enum_name,
    ).unwrap();
    for (i, item) in enum_info.items.iter().enumerate() {
        write!(
            &mut code,
            "{index} => {item_name},\n",
            index = i,
            item_name = item.rust_name
        ).unwrap();
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
    ).unwrap();

    write!(
        &mut code,
        r#"
mod swig_foreign_types_map {{
    #![swig_foreigner_type = "{enum_name}"]
    #![swig_rust_type = "{rust_enum_name}"]
}}

impl SwigFrom<{rust_enum_name}> for u32 {{
   fn swig_from(x: {rust_enum_name}) -> u32 {{
        match x {{
"#,
        enum_name = enum_info.name,
        rust_enum_name = rust_enum_name,
    ).unwrap();

    for (i, item) in enum_info.items.iter().enumerate() {
        write!(
            &mut code,
            r#"
           {item_name} => {index},
"#,
            index = i,
            item_name = item.rust_name
        ).unwrap();
    }
    write!(
        &mut code,
        r#"
       }}
    }}
}}
"#
    ).unwrap();

    write!(
        &mut code,
        r#"
impl SwigFrom<Option<{rust_enum_name}>> for Option<u32> {{
   fn swig_from(x: Option<{rust_enum_name}>) -> Option<u32> {{
        x.map(|v| match v {{
"#,
        rust_enum_name = rust_enum_name,
    ).unwrap();

    for (i, item) in enum_info.items.iter().enumerate() {
        write!(
            &mut code,
            r#"
           {item_name} => {index},
"#,
            index = i,
            item_name = item.rust_name
        ).unwrap();
    }
    write!(
        &mut code,
        r#"
       }})
    }}
}}
"#
    ).unwrap();

    conv_map.register_exported_enum(enum_info);
    conv_map.merge(
        sess,
        &*enum_info.rust_enum_name().as_str(),
        &code,
        pointer_target_width,
    )?;
    Ok(vec![])
}

fn find_suitable_ftypes_for_interace_methods<'a>(
    sess: &'a ParseSess,
    conv_map: &mut TypesConvMap,
    interace: &ForeignInterface,
) -> PResult<'a, Vec<CppForeignMethodSignature>> {
    let void_sym = Symbol::intern("void");
    let dummy_ty = ast::Ty {
        id: DUMMY_NODE_ID,
        span: DUMMY_SP,
        node: ast::TyKind::Tup(vec![]),
    };
    let mut f_methods = vec![];

    for method in &interace.items {
        let mut input = Vec::<CppForeignTypeInfo>::with_capacity(method.fn_decl.inputs.len() - 1);
        for arg in method.fn_decl.inputs.iter().skip(1) {
            let f_arg_type = conv_map
                .map_through_conversation_to_foreign(&arg.ty, Direction::Outgoing, arg.ty.span)
                .ok_or_else(|| {
                    fatal_error(
                        sess,
                        arg.ty.span,
                        &format!(
                            "Do not know conversation to foreign \
                             from such rust type '{}'",
                            normalized_ty_string(&arg.ty)
                        ),
                    )
                })?;
            input.push(f_arg_type.into());
        }
        let output = match method.fn_decl.output {
            ast::FunctionRetTy::Default(sp) => ForeignTypeInfo {
                name: void_sym,
                correspoding_rust_type: {
                    let mut ty: ast::Ty = dummy_ty.clone();
                    ty.span = sp;
                    ty.into()
                },
            },
            _ => unimplemented!(),
        };
        f_methods.push(CppForeignMethodSignature {
            output: output.into(),
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

fn rust_code_generate_interface<'a>(
    sess: &'a ParseSess,
    conv_map: &mut TypesConvMap,
    pointer_target_width: usize,
    interface: &ForeignInterface,
    methods_sign: &[CppForeignMethodSignature],
) -> PResult<'a, Vec<P<ast::Item>>> {
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
            fatal_error(
                sess,
                interface.span,
                &format!("gen args with types error: {}", err),
            )
        })?;
        write!(
            &mut code,
            r#"
{method_name}: extern "C" fn({args}_: *const ::std::os::raw::c_void),
"#,
            method_name = method.name,
            args = args,
        ).unwrap();
    }

    write!(
        &mut code,
        r#"
}}
"#
    ).unwrap();

    let mut gen_items = vec![];

    gen_items.append(&mut code_to_item(sess, &struct_with_funcs, &code)?);

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
        trait_name = interface.self_type,
    ).unwrap();

    conv_map.merge(
        sess,
        &format!("{}", interface.self_type),
        &code,
        pointer_target_width,
    )?;

    code.clear();

    write!(
        &mut code,
        r#"
impl {trait_name} for {struct_with_funcs} {{
"#,
        trait_name = interface.self_type,
        struct_with_funcs = struct_with_funcs,
    ).unwrap();

    for (method, f_method) in interface.items.iter().zip(methods_sign) {
        let func_name = method
            .rust_name
            .segments
            .last()
            .ok_or_else(|| fatal_error(sess, method.rust_name.span, "Empty trait function name"))?
            .identifier
            .name;
        let rest_args_with_types: String = method
            .fn_decl
            .inputs
            .iter()
            .skip(1)
            .enumerate()
            .map(|(i, v)| format!("a_{}: {}", i, pprust::ty_to_string(&*v.ty)))
            .fold(String::new(), |mut acc, x| {
                acc.push_str(", ");
                acc.push_str(&x);
                acc
            });
        let self_arg = match self_variant(&method.fn_decl.inputs[0].ty)
            .expect("Expect Self type for first argument")
        {
            SelfTypeVariant::Default => "self",
            SelfTypeVariant::Mut => "mut self",
            SelfTypeVariant::Rptr => "&self",
            SelfTypeVariant::RptrMut => "&mut self",
        };
        let args_with_types: String = [self_arg.to_string(), rest_args_with_types].concat();
        assert!(!method.fn_decl.inputs.is_empty());
        let n_args = method.fn_decl.inputs.len() - 1;
        let (mut conv_deps, convert_args) = rust_to_foreign_convert_method_inputs(
            sess,
            conv_map,
            method,
            f_method,
            (0..n_args).map(|v| format!("a_{}", v)),
            "()",
        )?;
        gen_items.append(&mut conv_deps);
        write!(
            &mut code,
            r#"
    #[allow(unused_mut)]
    fn {func_name}({args_with_types}) {{
{convert_args}
        (self.{method_name})({args}self.opaque);
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
        ).unwrap();
    }
    write!(
        &mut code,
        r#"
}}
"#
    ).unwrap();

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
    ).unwrap();

    gen_items.append(&mut code_to_item(
        sess,
        &format!("impl {} for {}", interface.self_type, struct_with_funcs),
        &code,
    )?);

    Ok(gen_items)
}
