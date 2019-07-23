use std::io::Write;

use petgraph::Direction;
use rustc_hash::FxHashSet;
use smol_str::SmolStr;
use syn::{spanned::Spanned, Type};

use crate::{
    cpp::{
        cpp_code, fmt_write_err_map, map_type, map_write_err, rust_generate_args_with_types,
        CppContext, CppForeignMethodSignature, CppForeignTypeInfo,
    },
    error::{panic_on_syn_error, DiagnosticError, Result},
    file_cache::FileWriteCache,
    namegen::new_unique_name,
    source_registry::SourceId,
    typemap::{
        ast::{parse_ty_with_given_span, DisplayToTokens, TypeName},
        ty::RustType,
        utils::rust_to_foreign_convert_method_inputs,
        ForeignTypeInfo, FROM_VAR_TEMPLATE,
    },
    types::ForeignInterface,
    WRITE_TO_MEM_FAILED_MSG,
};

pub(in crate::cpp) fn generate_interface(
    ctx: &mut CppContext,
    interface: &ForeignInterface,
) -> Result<()> {
    let mut f_methods = find_suitable_ftypes_for_interace_methods(ctx, interface)?;
    let req_includes = cpp_code::cpp_list_required_includes(&mut f_methods);
    generate_for_interface(ctx, interface, &req_includes, &f_methods)
        .map_err(|err| DiagnosticError::new(interface.src_id, interface.span(), err))?;
    rust_code_generate_interface(ctx, interface, &f_methods)?;

    let c_struct_name = format!("C_{}", interface.name);
    let rust_struct_pointer = format!("*const {}", c_struct_name);
    let rust_ty: Type = parse_ty_with_given_span(&rust_struct_pointer, interface.name.span())
        .map_err(|err| DiagnosticError::from_syn_err(interface.src_id, err))?;
    let c_struct_pointer = format!("const struct {} * const", c_struct_name);

    let rust_ty = ctx.conv_map.find_or_alloc_rust_type_no_src_id(&rust_ty);

    ctx.conv_map.add_foreign(
        rust_ty,
        TypeName::new(c_struct_pointer, interface.src_id_span()),
    )?;

    Ok(())
}

fn rust_code_generate_interface(
    ctx: &mut CppContext,
    interface: &ForeignInterface,
    methods_sign: &[CppForeignMethodSignature],
) -> Result<()> {
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
        let args = rust_generate_args_with_types(f_method);
        writeln!(
            &mut code,
            r#"
{method_name}: extern "C" fn({args}_: *const ::std::os::raw::c_void) -> {ret_type},"#,
            method_name = method.name,
            args = args,
            ret_type = DisplayToTokens(&f_method.output.base.correspoding_rust_type.ty),
        )
        .unwrap();
    }

    code.push_str(
        r#"
}
"#,
    );

    ctx.rust_code.push(
        syn::parse_str(&code)
            .unwrap_or_else(|err| panic_on_syn_error("cpp internal code", code.clone(), err)),
    );

    code.clear();
    writeln!(
        &mut code,
        r#"
impl SwigFrom<*const {struct_with_funcs}> for Box<{trait_name}> {{
    fn swig_from(this: *const {struct_with_funcs}) -> Self {{
       let this: &{struct_with_funcs} = unsafe {{ this.as_ref().unwrap() }};
       Box::new(this.clone())
    }}
}}"#,
        struct_with_funcs = struct_with_funcs,
        trait_name = DisplayToTokens(&interface.self_type),
    )
    .unwrap();

    ctx.conv_map
        .merge(SourceId::none(), &code, ctx.target_pointer_width)?;

    code.clear();

    writeln!(
        &mut code,
        r#"
impl {trait_name} for {struct_with_funcs} {{"#,
        trait_name = DisplayToTokens(&interface.self_type),
        struct_with_funcs = struct_with_funcs,
    )
    .expect(WRITE_TO_MEM_FAILED_MSG);

    fn n_arguments_list(n: usize) -> String {
        (0..n)
            .map(|v| format!("a{}", v))
            .fold(String::new(), |mut acc, x| {
                if !acc.is_empty() {
                    acc.push_str(", ");
                }
                acc.push_str(&x);
                acc
            })
    }

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
            .map(|(i, v)| format!("a{}: {}", i, DisplayToTokens(&v.as_named_arg().unwrap().ty)))
            .fold(String::new(), |mut acc, x| {
                acc.push_str(", ");
                acc.push_str(&x);
                acc
            });
        let self_arg = format!(
            "{}",
            method.fn_decl.inputs[0].as_self_arg(interface.src_id)?
        );

        let args_with_types: String = [self_arg.to_string(), rest_args_with_types].concat();
        assert!(!method.fn_decl.inputs.is_empty());
        let n_args = method.fn_decl.inputs.len() - 1;
        let (mut conv_deps, convert_args) = rust_to_foreign_convert_method_inputs(
            ctx.conv_map,
            interface.src_id,
            method,
            f_method,
            (0..n_args).map(|v| format!("a{}", v)),
            "()",
        )?;
        ctx.rust_code.append(&mut conv_deps);
        let (real_output_typename, output_conv) = match method.fn_decl.output {
            syn::ReturnType::Default => ("()".to_string(), String::new()),
            syn::ReturnType::Type(_, ref ret_ty) => {
                let real_output_type: RustType = ctx
                    .conv_map
                    .find_or_alloc_rust_type(ret_ty, interface.src_id);
                let (mut conv_deps, conv_code) = ctx.conv_map.convert_rust_types(
                    f_method.output.base.correspoding_rust_type.to_idx(),
                    real_output_type.to_idx(),
                    "ret",
                    "ret",
                    real_output_type.normalized_name.as_str(),
                    (interface.src_id, ret_ty.span()),
                )?;
                ctx.rust_code.append(&mut conv_deps);
                (real_output_type.normalized_name.to_string(), conv_code)
            }
        };
        let ret_type = format!(
            "{}",
            DisplayToTokens(&f_method.output.base.correspoding_rust_type.ty)
        );
        writeln!(
            &mut code,
            r#"
    #[allow(unused_mut)]
    fn {func_name}({args_with_types}) -> {real_ret_type} {{
{convert_args}
        let ret: {ret_type} = (self.{method_name})({args}self.opaque);
{output_conv}
        ret
    }}"#,
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
        .expect(WRITE_TO_MEM_FAILED_MSG);
    }
    code.push_str(
        r#"
}
"#,
    );

    writeln!(
        &mut code,
        r#"
impl Drop for {struct_with_funcs} {{
    fn drop(&mut self) {{
       (self.{struct_with_funcs}_deref)(self.opaque);
    }}
}}"#,
        struct_with_funcs = struct_with_funcs
    )
    .unwrap();

    ctx.rust_code.push(
        syn::parse_str(&code)
            .unwrap_or_else(|err| panic_on_syn_error("cpp internal code", code, err)),
    );

    Ok(())
}

fn find_suitable_ftypes_for_interace_methods(
    ctx: &mut CppContext,
    interace: &ForeignInterface,
) -> Result<Vec<CppForeignMethodSignature>> {
    let void_sym = "void";
    let dummy_ty = parse_type! { () };
    let dummy_rust_ty = ctx.conv_map.find_or_alloc_rust_type_no_src_id(&dummy_ty);
    let mut f_methods = Vec::with_capacity(interace.items.len());

    for method in &interace.items {
        let mut input = Vec::<CppForeignTypeInfo>::with_capacity(method.fn_decl.inputs.len() - 1);
        for arg in method.fn_decl.inputs.iter().skip(1) {
            let named_arg = arg
                .as_named_arg()
                .map_err(|err| DiagnosticError::from_syn_err(interace.src_id, err))?;
            let arg_rust_ty = ctx
                .conv_map
                .find_or_alloc_rust_type(&named_arg.ty, interace.src_id);
            input.push(map_type(
                ctx,
                &arg_rust_ty,
                Direction::Outgoing,
                (interace.src_id, named_arg.ty.span()),
            )?);
        }
        let output = match method.fn_decl.output {
            syn::ReturnType::Default => ForeignTypeInfo {
                name: void_sym.into(),
                correspoding_rust_type: dummy_rust_ty.clone(),
            }
            .into(),
            syn::ReturnType::Type(_, ref ret_ty) => {
                let ret_rust_ty = ctx
                    .conv_map
                    .find_or_alloc_rust_type(ret_ty, interace.src_id);
                map_type(
                    ctx,
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
    ctx: &mut CppContext,
    interface: &ForeignInterface,
    req_includes: &[SmolStr],
    f_methods: &[CppForeignMethodSignature],
) -> std::result::Result<(), String> {
    use std::fmt::Write;

    let c_interface_struct_header = format!("c_{}.h", interface.name);
    let c_path = ctx.cfg.output_dir.join(&c_interface_struct_header);
    let mut file_c = FileWriteCache::new(&c_path, ctx.generated_foreign_files);
    let cpp_path = ctx.cfg.output_dir.join(format!("{}.hpp", interface.name));
    let mut file_cpp = FileWriteCache::new(&cpp_path, ctx.generated_foreign_files);
    let interface_comments = cpp_code::doc_comments_to_c_comments(&interface.doc_comments, true);

    writeln!(
        file_c,
        r#"// Automatically generated by rust_swig
#pragma once
{doc_comments}
struct C_{interface_name} {{
    void *opaque;
    //! call by Rust side when callback not need anymore
    void (*C_{interface_name}_deref)(void *opaque);"#,
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
        let mut known_names: FxHashSet<SmolStr> =
            method.arg_names_without_self().map(|x| x.into()).collect();
        let opaque_name = new_unique_name(&known_names, "opaque");
        known_names.insert(opaque_name.clone());
        let interface_ptr = new_unique_name(&known_names, "pi");
        known_names.insert(interface_ptr.clone());
        let ret_name = new_unique_name(&known_names, "ret");
        known_names.insert(ret_name.clone());

        let (cpp_ret_type, cpp_out_conv) =
            if let Some(out_conv) = f_method.output.cpp_converter.as_ref() {
                let conv_code = out_conv
                    .converter
                    .as_str()
                    .replace(FROM_VAR_TEMPLATE, &ret_name);
                (out_conv.typename.clone(), conv_code)
            } else {
                (c_ret_type.clone(), String::new())
            };
        writeln!(
            file_c,
            r#"{doc_comments}
    {c_ret_type} (*{method_name})({single_args_with_types}void *opaque);"#,
            method_name = method.name,
            doc_comments = cpp_code::doc_comments_to_c_comments(&method.doc_comments, false),
            single_args_with_types = cpp_code::c_generate_args_with_types(
                f_method,
                method.arg_names_without_self(),
                true
            ),
            c_ret_type = c_ret_type,
        )
        .map_err(&map_write_err)?;

        writeln!(
            &mut cpp_virtual_methods,
            r#"{doc_comments}
    virtual {cpp_ret_type} {method_name}({single_args_with_types}) noexcept = 0;"#,
            method_name = method.name,
            doc_comments = cpp_code::doc_comments_to_c_comments(&method.doc_comments, false),
            single_args_with_types =
                cpp_code::cpp_generate_args_with_types(f_method, method.arg_names_without_self()),
            cpp_ret_type = cpp_ret_type,
        )
        .expect(WRITE_TO_MEM_FAILED_MSG);

        let (conv_args_code, call_input_args) =
            cpp_code::convert_args(f_method, known_names, method.arg_names_without_self());

        write!(
            &mut cpp_static_reroute_methods,
            r#"
    static {c_ret_type} c_{method_name}({single_args_with_types}void *{opaque})
    {{
        assert({opaque} != nullptr);
        auto {p} = static_cast<{interface_name} *>({opaque});
{conv_args_code}"#,
            c_ret_type = c_ret_type,
            method_name = method.name,
            single_args_with_types = cpp_code::c_generate_args_with_types(
                f_method,
                method.arg_names_without_self(),
                true
            ),
            opaque = opaque_name,
            p = interface_ptr,
            interface_name = interface.name,
            conv_args_code = conv_args_code,
        )
        .expect(WRITE_TO_MEM_FAILED_MSG);

        if c_ret_type == "void" {
            writeln!(
                &mut cpp_static_reroute_methods,
                r#"
        {p}->{method_name}({input_args});
    }}"#,
                p = interface_ptr,
                method_name = method.name,
                input_args = call_input_args,
            )
            .map_err(&map_write_err)?;
        } else {
            writeln!(
                &mut cpp_static_reroute_methods,
                r#"
        auto {ret} = {p}->{method_name}({input_args});
        return {cpp_out_conv};
    }}"#,
                ret = ret_name,
                method_name = method.name,
                input_args = call_input_args,
                cpp_out_conv = cpp_out_conv,
                p = interface_ptr,
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
    file_c
        .write_all(
            br#"
};
"#,
        )
        .expect(WRITE_TO_MEM_FAILED_MSG);

    let mut includes = String::new();
    for inc in req_includes {
        writeln!(&mut includes, r#"#include {}"#, inc).map_err(fmt_write_err_map)?;
    }

    writeln!(
        file_cpp,
        r##"// Automatically generated by rust_swig
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
}} // namespace {namespace_name}"##,
        interface_name = interface.name,
        includes = includes,
        doc_comments = interface_comments,
        c_interface_struct_header = c_interface_struct_header,
        virtual_methods = cpp_virtual_methods,
        static_reroute_methods = cpp_static_reroute_methods,
        cpp_fill_c_interface_struct = cpp_fill_c_interface_struct,
        namespace_name = ctx.cfg.namespace_name,
    )
    .map_err(&map_write_err)?;

    file_c.update_file_if_necessary().map_err(&map_write_err)?;
    file_cpp
        .update_file_if_necessary()
        .map_err(&map_write_err)?;

    Ok(())
}
