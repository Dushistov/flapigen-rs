mod cpp_code;

use std::path::Path;
use std::fs::File;
use std::io::Write;
use std::fmt;

use petgraph::Direction;
use syntex_syntax::ptr::P;
use syntex_syntax::parse::{PResult, ParseSess};
use syntex_syntax::ast;
use syntex_syntax::ast::DUMMY_NODE_ID;
use syntex_pos::DUMMY_SP;
use syntex_syntax::symbol::Symbol;
use syntex_syntax::print::pprust;

use my_ast::{code_to_item, list_lifetimes, normalized_ty_string, parse_ty, self_variant, RustType};
use errors::fatal_error;
use types_conv_map::{make_unique_rust_typename, unpack_unique_typename, ForeignMethodSignature,
                     ForeignTypeInfo, FROM_VAR_TEMPLATE, TO_VAR_TEMPLATE};
use types_conv_map::utils::{create_suitable_types_for_constructor_and_self,
                            foreign_from_rust_convert_method_output,
                            foreign_to_rust_convert_method_inputs,
                            rust_to_foreign_convert_method_inputs};
use {CppConfig, ForeignEnumInfo, ForeignInterface, ForeignerClassInfo, ForeignerMethod,
     LanguageGenerator, MethodVariant, SelfTypeVariant, TypesConvMap};

struct CppForeignTypeInfo {
    base: ForeignTypeInfo,
    pub cpp_transition_type: Option<Symbol>,
    cpp_converter: String,
}

impl AsRef<ForeignTypeInfo> for CppForeignTypeInfo {
    fn as_ref(&self) -> &ForeignTypeInfo {
        &self.base
    }
}

impl CppForeignTypeInfo {
    fn cpp_need_conversation(&self) -> bool {
        !self.cpp_converter.is_empty()
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
            cpp_transition_type: None,
            cpp_converter: String::new(),
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

struct InfoForCpp {
    class_name: Symbol,
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
            class.name,
            class.this_type_for_method
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
            conv_map.cache_rust_to_foreign_conv(&this_type, (my_void_ptr_ti, foreign_typename));
            conv_map.set_generator_data_for_foreign_type(
                foreign_typename,
                Box::new(InfoForCpp {
                    class_name: class.name,
                }),
            );
        }

        let m_sigs = find_suitable_foreign_types_for_methods(sess, conv_map, class)?;
        generate_code(
            sess,
            conv_map,
            &self.output_dir,
            &self.namespace_name,
            class,
            &m_sigs,
        )
    }

    fn generate_enum<'a>(
        &self,
        sess: &'a ParseSess,
        conv_map: &mut TypesConvMap,
        pointer_target_width: usize,
        enum_info: &ForeignEnumInfo,
    ) -> PResult<'a, Vec<P<ast::Item>>> {
        if (enum_info.items.len() as u64) >= (u32::max_value() as u64) {
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
        cpp_code::generate_for_interface(&self.output_dir, interface, &f_methods)
            .map_err(|err| fatal_error(sess, interface.span, &err))?;

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
}

fn find_suitable_foreign_types_for_methods<'a>(
    sess: &'a ParseSess,
    conv_map: &mut TypesConvMap,
    class: &ForeignerClassInfo,
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
            if let Some(converter) = special_type(conv_map, &arg.ty)? {
                input.push(converter);
                continue;
            }
            let f_arg_type = conv_map
                .map_through_conversation_to_foreign(&arg.ty, Direction::Incoming, arg.ty.span)
                .ok_or_else(|| {
                    fatal_error(
                        sess,
                        arg.ty.span,
                        &format!(
                            "Do not know conversation from foreign \
                             to such rust type '{}'",
                            normalized_ty_string(&arg.ty)
                        ),
                    )
                })?;
            input.push(f_arg_type.into());
        }
        let output = match method.variant {
            MethodVariant::Constructor => ForeignTypeInfo {
                name: empty_symbol,
                correspoding_rust_type: dummy_ty.clone().into(),
            },
            _ => match method.fn_decl.output {
                ast::FunctionRetTy::Default(sp) => ForeignTypeInfo {
                    name: Symbol::intern("void"),
                    correspoding_rust_type: {
                        let mut ty: ast::Ty = dummy_ty.clone().into();
                        ty.span = sp;
                        ty.into()
                    },
                },
                ast::FunctionRetTy::Ty(ref rt) => conv_map
                    .map_through_conversation_to_foreign(&*rt, Direction::Outgoing, rt.span)
                    .ok_or_else(|| {
                        fatal_error(
                            sess,
                            rt.span,
                            &format!(
                                "Do not know conversation from \
                                 such rust type '{}' to foreign",
                                normalized_ty_string(&*rt)
                            ),
                        )
                    })?,
            },
        };
        ret.push(CppForeignMethodSignature {
            output: output.into(),
            input,
        });
    }
    Ok(ret)
}

fn generate_code<'a>(
    sess: &'a ParseSess,
    conv_map: &mut TypesConvMap,
    output_dir: &Path,
    namespace_name: &str,
    class: &ForeignerClassInfo,
    methods_sign: &[CppForeignMethodSignature],
) -> PResult<'a, Vec<P<ast::Item>>> {
    let c_path = output_dir.join(format!("c_{}.h", class.name));
    let mut c_include_f = File::create(&c_path).map_err(|err| {
        fatal_error(
            sess,
            class.span,
            &format!("Couldn't create {:?}: {}", c_path, err),
        )
    })?;
    let cpp_path = output_dir.join(format!("{}.hpp", class.name));
    let mut cpp_include_f = File::create(&cpp_path).map_err(|err| {
        fatal_error(
            sess,
            class.span,
            &format!("Couldn't create {:?}: {}", cpp_path, err),
        )
    })?;

    let map_write_err = |err| {
        fatal_error(
            sess,
            class.span,
            &format!("write to {:?} failed: {}", c_path, err),
        )
    };
    let c_class_type = cpp_code::c_class_type(class);
    let class_doc_comments = cpp_code::doc_comments_to_c_comments(&class.doc_comments, true);
    write!(
        c_include_f,
        r##"// Automaticaly generated by rust_swig
{doc_comments}
#pragma once

#include <stdint.h>

#ifdef __cplusplus
extern "C" {{
#endif

    typedef struct {c_class_type} {c_class_type};

"##,
        doc_comments = class_doc_comments,
        c_class_type = c_class_type,
    ).map_err(&map_write_err)?;

    write!(
        cpp_include_f,
        r#"// Automaticaly generated by rust_swig
#pragma once

#include <cstdlib>

#include "c_{class_name}.h"

{doc_comments}
class {class_name} {{
public:
    {class_name}(const {class_name}&) = delete;
    {class_name} &operator=(const {class_name}&) = delete;
    {class_name}({class_name} &&o): self_(o.self_)
    {{
        o.self_ = nullptr;
    }}
    {class_name} &operator=({class_name} &&o)
    {{
        assert(this != &o);
        self_ = o.self_;
        o.self_ = nullptr;
        return *this;
    }}
    explicit {class_name}({c_class_type} *o): self_(o) {{}}
    {c_class_type} *release()
    {{
        {c_class_type} *ret = self_;
        self_ = nullptr;
        return ret;
    }}
"#,
        c_class_type = c_class_type,
        class_name = class.name,
        doc_comments = class_doc_comments,
    ).map_err(&map_write_err)?;

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
}}"#,
                lifetimes = lifetimes,
                class_name = pprust::ty_to_string(&this_type.ty),
                code_box_this = code_box_this,
            ),
        )?);

        let void_ptr_typename = Symbol::intern("*mut ::std::os::raw::c_void");
        let my_void_ptr_ti = RustType::new(
            parse_ty(sess, DUMMY_SP, void_ptr_typename)?,
            make_unique_rust_typename(void_ptr_typename, this_type.normalized_name),
        );

        let unpack_code =
            TypesConvMap::unpack_from_heap_pointer(&this_type_for_method, TO_VAR_TEMPLATE, true);
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
                class.name,
                namespace_name,
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
        write!(
            cpp_include_f,
            "{}",
            cpp_code::doc_comments_to_c_comments(&method.doc_comments, false)
        ).map_err(&map_write_err)?;
        let c_func_name = c_func_name(class, method, f_method);
        let c_args_with_types = cpp_code::generate_args_with_types(f_method)
            .map_err(|err| fatal_error(sess, class.span, &err))?;
        let comma_c_args_with_types = if c_args_with_types.is_empty() {
            "".to_string()
        } else {
            format!(", {}", c_args_with_types)
        };
        let args_names = n_arguments_list(f_method.input.len());

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
        let method_access = if method.foreigner_private {
            "private"
        } else {
            "public"
        };

        let wrap_ret_for_cpp = if let Some(info_for_cpp) =
            conv_map.generator_data_for_foreign_type(f_method.output.base.name)
        {
            let info_for_cpp: &InfoForCpp = info_for_cpp.downcast_ref::<InfoForCpp>().unwrap();
            info_for_cpp.class_name
        } else {
            Symbol::intern("")
        };
        let is_wrap_ret_for_cpp = wrap_ret_for_cpp.as_str().is_empty();

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

                write!(
                    cpp_include_f,
                    r#"
{access}:
    static {ret_type} {method_name}({args_with_types})
    {{
        return {wrap_ret_for_cpp}({c_func_name}({args}));
    }}
"#,
                    access = method_access,
                    method_name = method_name,
                    ret_type = if is_wrap_ret_for_cpp {
                        f_method.output.as_ref().name
                    } else {
                        wrap_ret_for_cpp
                    },
                    wrap_ret_for_cpp = wrap_ret_for_cpp,
                    c_func_name = c_func_name,
                    args_with_types = c_args_with_types,
                    args = args_names,
                ).map_err(&map_write_err)?;

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
                write!(
                    cpp_include_f,
                    r#"
{access}:
    {ret_type} {method_name}({args_with_types}) {const_if_readonly}
    {{
        return {wrap_ret_for_cpp}({c_func_name}(this->self_{args}));
    }}
"#,
                    access = method_access,
                    method_name = method_name,
                    ret_type = if is_wrap_ret_for_cpp {
                        f_method.output.as_ref().name
                    } else {
                        wrap_ret_for_cpp
                    },
                    wrap_ret_for_cpp = wrap_ret_for_cpp,
                    c_func_name = c_func_name,
                    args_with_types = c_args_with_types,
                    args = if args_names.is_empty() {
                        "".to_string()
                    } else {
                        format!(", {}", args_names)
                    },
                    const_if_readonly = const_if_readonly,
                ).map_err(&map_write_err)?;
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
{access}:
    {class_name}({args_with_types})
    {{
        this->self_ = {c_func_name}({args});
        if (this->self_ == nullptr) {{
            std::abort();
        }}
    }}
"#,
                    class_name = class.name,
                    c_func_name = c_func_name,
                    args_with_types = c_args_with_types,
                    access = method_access,
                    args = args_names,
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
public:
    ~{class_name}()
    {{
        if (this->self_ != nullptr) {{
            {c_destructor_name}(this->self_);
            this->self_ = nullptr;
        }}
    }}
"#,
            class_name = class.name,
            c_destructor_name = c_destructor_name,
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
        cpp_include_f,
        r#"
private:
    {c_class_type} *self_;
}};
"#,
        c_class_type = c_class_type
    ).map_err(&map_write_err)?;
    Ok(gen_code)
}

fn need_cpp_helper_for_input_or_output(f_method: &CppForeignMethodSignature) -> bool {
    for ti in &f_method.input {
        if ti.cpp_need_conversation() {
            return true;
        }
    }
    f_method.output.cpp_need_conversation()
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

fn special_type<'a>(
    conv_map: &TypesConvMap,
    arg_ty: &ast::Ty,
) -> PResult<'a, Option<CppForeignTypeInfo>> {
    trace!("Check is arg.ty({:?}) implements exported enum", arg_ty);
    if let Some(foreign_enum) = conv_map.is_this_exported_enum(&arg_ty) {
        let converter = calc_converter_for_enum(foreign_enum);
        return Ok(Some(converter));
    }

    trace!("Oridinary type {:?}", arg_ty);
    Ok(None)
}

fn calc_converter_for_enum(foreign_enum: &ForeignEnumInfo) -> CppForeignTypeInfo {
    let sess = ParseSess::new();
    let u32_ti: RustType = parse_ty(&sess, DUMMY_SP, Symbol::intern("u32"))
        .unwrap()
        .into();
    let cpp_converter: String = r#"
        uint32_t {to_var} = {from_var};
"#.into();
    CppForeignTypeInfo {
        base: ForeignTypeInfo {
            name: foreign_enum.name,
            correspoding_rust_type: u32_ti,
        },
        cpp_transition_type: Some(Symbol::intern("uint32_t")),
        cpp_converter,
    }
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
mod swig_foreign_types_map {{
    #![swig_foreigner_type = "{enum_name}"]
    #![swig_rust_type = "{rust_enum_name}"]
}}
#[swig_to_foreigner_hint = "{enum_name}"]
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
                    let mut ty: ast::Ty = dummy_ty.clone().into();
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
        (self.{method_name})({args}, self.opaque);
    }}
"#,
            func_name = func_name,
            convert_args = convert_args,
            method_name = method.name,
            args_with_types = args_with_types,
            args = n_arguments_list(n_args),
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
