use std::{io::Write, path::Path};

use petgraph::Direction;
use proc_macro2::TokenStream;
use syn::{parse_quote, spanned::Spanned, Type};

use crate::{
    cpp::{
        cpp_code, fmt_write_err_map, map_type, map_write_err, n_arguments_list,
        rust_generate_args_with_types, CppForeignMethodSignature, CppForeignTypeInfo,
    },
    error::{panic_on_syn_error, DiagnosticError, Result},
    file_cache::FileWriteCache,
    source_registry::SourceId,
    typemap::{
        ast::{fn_arg_type, DisplayToTokens},
        ty::RustType,
        utils::rust_to_foreign_convert_method_inputs,
        ForeignTypeInfo, TypeMap, FROM_VAR_TEMPLATE,
    },
    types::ForeignInterface,
    CppConfig,
};

pub(in crate::cpp) fn rust_code_generate_interface(
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

pub(in crate::cpp) fn find_suitable_ftypes_for_interace_methods(
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

pub(in crate::cpp) fn generate_for_interface(
    output_dir: &Path,
    namespace_name: &str,
    interface: &ForeignInterface,
    req_includes: &[String],
    f_methods: &[CppForeignMethodSignature],
) -> std::result::Result<(), String> {
    use std::fmt::Write;

    let c_interface_struct_header = format!("c_{}.h", interface.name);
    let c_path = output_dir.join(&c_interface_struct_header);
    let mut file_c = FileWriteCache::new(&c_path);
    let cpp_path = output_dir.join(format!("{}.hpp", interface.name));
    let mut file_cpp = FileWriteCache::new(&cpp_path);
    let interface_comments = cpp_code::doc_comments_to_c_comments(&interface.doc_comments, true);

    write!(
        file_c,
        r#"// Automaticaly generated by rust_swig
#pragma once
{doc_comments}
struct C_{interface_name} {{
    void *opaque;
    //! call by Rust side when callback not need anymore
    void (*C_{interface_name}_deref)(void *opaque);
    "#,
        interface_name = interface.name,
        doc_comments = interface_comments
    )
    .map_err(&map_write_err)?;

    let mut cpp_virtual_methods = String::new();
    let mut cpp_static_reroute_methods = format!(
        r#"
    static void c_{interface_name}_deref(void *opaque)
    {{
        auto p = static_cast<{interface_name} *>(opaque);
        delete p;
    }}
"#,
        interface_name = interface.name
    );
    let mut cpp_fill_c_interface_struct = format!(
        r#"
        ret.C_{interface_name}_deref = c_{interface_name}_deref;
"#,
        interface_name = interface.name
    );

    for (method, f_method) in interface.items.iter().zip(f_methods) {
        let c_ret_type = f_method.output.base.name.clone();
        let (cpp_ret_type, cpp_out_conv) =
            if let Some(out_conv) = f_method.output.cpp_converter.as_ref() {
                let conv_code = out_conv
                    .converter
                    .as_str()
                    .replace(FROM_VAR_TEMPLATE, "ret");
                (out_conv.typename.clone(), conv_code)
            } else {
                (c_ret_type.clone(), String::new())
            };
        write!(
            file_c,
            r#"
{doc_comments}
    {c_ret_type} (*{method_name})({single_args_with_types}void *opaque);
"#,
            method_name = method.name,
            doc_comments = cpp_code::doc_comments_to_c_comments(&method.doc_comments, false),
            single_args_with_types = cpp_code::c_generate_args_with_types(f_method, true)?,
            c_ret_type = c_ret_type,
        )
        .map_err(&map_write_err)?;

        write!(
            &mut cpp_virtual_methods,
            r#"
{doc_comments}
    virtual {cpp_ret_type} {method_name}({single_args_with_types}) = 0;
"#,
            method_name = method.name,
            doc_comments = cpp_code::doc_comments_to_c_comments(&method.doc_comments, false),
            single_args_with_types = cpp_code::cpp_generate_args_with_types(f_method)?,
            cpp_ret_type = cpp_ret_type,
        )
        .map_err(&map_write_err)?;
        if c_ret_type == "void" {
            write!(
                &mut cpp_static_reroute_methods,
                r#"
   static void c_{method_name}({single_args_with_types}void *opaque)
   {{
        auto p = static_cast<{interface_name} *>(opaque);
        assert(p != nullptr);
        p->{method_name}({input_args});
   }}
"#,
                method_name = method.name,
                single_args_with_types = cpp_code::c_generate_args_with_types(f_method, true)?,
                input_args = cpp_code::cpp_generate_args_to_call_c(f_method)?,
                interface_name = interface.name,
            )
            .map_err(&map_write_err)?;
        } else {
            write!(
                &mut cpp_static_reroute_methods,
                r#"
   static {c_ret_type} c_{method_name}({single_args_with_types}void *opaque)
   {{
        auto p = static_cast<{interface_name} *>(opaque);
        assert(p != nullptr);
        auto ret = p->{method_name}({input_args});
        return {cpp_out_conv};
   }}
"#,
                method_name = method.name,
                single_args_with_types = cpp_code::c_generate_args_with_types(f_method, true)?,
                input_args = cpp_code::cpp_generate_args_to_call_c(f_method)?,
                interface_name = interface.name,
                c_ret_type = c_ret_type,
                cpp_out_conv = cpp_out_conv,
            )
            .map_err(&map_write_err)?;
        }
        writeln!(
            &mut cpp_fill_c_interface_struct,
            "        ret.{method_name} = c_{method_name};",
            method_name = method.name,
        )
        .map_err(&map_write_err)?;
    }
    write!(
        file_c,
        r#"
}};
"#
    )
    .map_err(map_write_err)?;

    let mut includes = String::new();
    for inc in req_includes {
        writeln!(&mut includes, r#"#include {}"#, inc).map_err(fmt_write_err_map)?;
    }

    write!(
        file_cpp,
        r##"// Automaticaly generated by rust_swig
#pragma once

#include <cassert>

{includes}
#include "{c_interface_struct_header}"

namespace {namespace_name} {{
{doc_comments}
class {interface_name} {{
public:
    virtual ~{interface_name}() {{}}
{virtual_methods}
    //! @p should be allocated by new
    static C_{interface_name} to_c_interface({interface_name} *p)
    {{
        assert(p != nullptr);
        C_{interface_name} ret;
        ret.opaque = p;
{cpp_fill_c_interface_struct}
        return ret;
    }}
private:
{static_reroute_methods}
}};
}} // namespace {namespace_name}
"##,
        interface_name = interface.name,
        includes = includes,
        doc_comments = interface_comments,
        c_interface_struct_header = c_interface_struct_header,
        virtual_methods = cpp_virtual_methods,
        static_reroute_methods = cpp_static_reroute_methods,
        cpp_fill_c_interface_struct = cpp_fill_c_interface_struct,
        namespace_name = namespace_name,
    )
    .map_err(&map_write_err)?;

    file_c.update_file_if_necessary().map_err(&map_write_err)?;
    file_cpp
        .update_file_if_necessary()
        .map_err(&map_write_err)?;

    Ok(())
}
