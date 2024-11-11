use std::{borrow::Cow, fmt::Write, mem};

use proc_macro2::TokenStream;
use quote::ToTokens;
use rustc_hash::FxHashSet;
use smol_str::SmolStr;
use syn::spanned::Spanned;

use crate::{
    code_parse::parse_fn_args,
    cpp::{map_type::map_repr_c_type, CppContext, CppForeignMethodSignature, MergeCItemsFlags},
    error::{panic_on_syn_error, DiagnosticError},
    file_cache::FileWriteCache,
    namegen::new_unique_name,
    source_registry::SourceId,
    typemap::{
        ast::DisplayToTokens, CItem, CItems, TypeConvCodeSubstParam, FROM_VAR_TEMPLATE,
        TO_VAR_TEMPLATE, TO_VAR_TYPE_TEMPLATE,
    },
    types::{FnArg, ForeignClassInfo, ForeignEnumInfo},
    WRITE_TO_MEM_FAILED_MSG,
};

pub(in crate::cpp) fn doc_comments_to_c_comments(
    doc_comments: &[String],
    class_comments: bool,
) -> String {
    let mut comments = String::new();
    for (i, comment) in doc_comments.iter().enumerate() {
        if i != 0 {
            comments.push('\n');
        }
        if !class_comments {
            comments.push_str("    ");
        }
        write!(&mut comments, "//{}", comment.trim()).unwrap();
    }
    comments
}

pub(in crate::cpp) fn c_generate_args_with_types<'a, NI>(
    f_method: &CppForeignMethodSignature,
    name_iter: NI,
    append_comma_if_not_empty: bool,
) -> String
where
    NI: Iterator<Item = &'a str>,
{
    let mut buf = String::new();
    for (f_type_info, arg_name) in f_method.input.iter().zip(name_iter) {
        if !buf.is_empty() {
            buf.push_str(", ");
        }
        write!(&mut buf, "{} {}", f_type_info.as_ref().name, arg_name)
            .expect(WRITE_TO_MEM_FAILED_MSG);
    }
    if !buf.is_empty() && append_comma_if_not_empty {
        buf.push_str(", ");
    }
    buf
}

pub(in crate::cpp) fn c_class_type(class: &ForeignClassInfo) -> String {
    format!("{}Opaque", class.name)
}

pub(in crate::cpp) fn cpp_generate_args_with_types<'a, NI: Iterator<Item = &'a str>>(
    f_method: &CppForeignMethodSignature,
    arg_name_iter: NI,
) -> String {
    let mut ret = String::new();
    for (f_type_info, arg_name) in f_method.input.iter().zip(arg_name_iter) {
        if f_type_info.input_to_output {
            continue;
        }
        if !ret.is_empty() {
            ret.push_str(", ");
        }

        write!(
            &mut ret,
            "{} {}",
            if let Some(conv) = f_type_info.cpp_converter.as_ref() {
                conv.typename.clone()
            } else {
                f_type_info.as_ref().name.clone()
            },
            arg_name,
        )
        .expect(WRITE_TO_MEM_FAILED_MSG);
    }
    ret
}

pub(in crate::cpp) fn convert_args<'a, NI: Iterator<Item = &'a str>>(
    f_method: &CppForeignMethodSignature,
    known_names: &mut FxHashSet<SmolStr>,
    arg_name_iter: NI,
) -> Result<(String, String), DiagnosticError> {
    let mut conv_deps = String::new();
    let mut converted_args = String::new();
    for (i, (f_type_info, arg_name)) in f_method.input.iter().zip(arg_name_iter).enumerate() {
        if i > 0 {
            converted_args.push_str(", ");
        }
        if let Some(conv) = f_type_info.cpp_converter.as_ref() {
            let var_name = if conv.converter.has_param(TO_VAR_TYPE_TEMPLATE)
                || conv.converter.has_param(TO_VAR_TEMPLATE)
            {
                let templ = format!("a{}", i);
                let var_name = new_unique_name(known_names, &templ);
                known_names.insert(var_name.clone());
                Some(var_name)
            } else {
                None
            };
            let conv_code =
                conv.converter
                    .generate_code_with_subst_func(|param_name| match param_name {
                        TypeConvCodeSubstParam::Name(name) => {
                            if name == FROM_VAR_TEMPLATE {
                                Some(Cow::Borrowed(arg_name))
                            } else if name == TO_VAR_TYPE_TEMPLATE {
                                Some(
                                    format!(
                                        "{} {}",
                                        f_type_info.as_ref().name,
                                        var_name.as_ref().unwrap()
                                    )
                                    .into(),
                                )
                            } else if name == TO_VAR_TEMPLATE {
                                Some(Cow::Borrowed(var_name.as_ref().unwrap()))
                            } else {
                                None
                            }
                        }
                        TypeConvCodeSubstParam::Tmp(name_template) => {
                            let tmp_name = new_unique_name(known_names, name_template);
                            let tmp_name_ret = tmp_name.to_string().into();
                            known_names.insert(tmp_name);
                            Some(tmp_name_ret)
                        }
                    })?;
            if !conv.converter.has_param(TO_VAR_TYPE_TEMPLATE) {
                converted_args.push_str(&conv_code);
            } else {
                converted_args.push_str(&format!("std::move({})", var_name.as_ref().unwrap()));
                conv_deps.push_str(&conv_code);
            }
        } else {
            converted_args.push_str(arg_name);
        }
    }
    Ok((conv_deps, converted_args))
}

pub(in crate::cpp) fn cpp_header_name(class: &ForeignClassInfo) -> String {
    format!("{}.hpp", class.name)
}

pub(in crate::cpp) fn c_header_name(class: &ForeignClassInfo) -> String {
    format!("c_{}.h", class.name)
}

pub(in crate::cpp) fn cpp_header_name_for_enum(enum_info: &ForeignEnumInfo) -> String {
    format!("{}.hpp", enum_info.name)
}

pub(in crate::cpp) fn c_header_name_for_enum(enum_info: &ForeignEnumInfo) -> String {
    format!("c_{}.h", enum_info.name)
}

pub(in crate::cpp) fn cpp_list_required_includes(
    methods: &mut [CppForeignMethodSignature],
) -> Vec<SmolStr> {
    let mut includes = Vec::<SmolStr>::with_capacity(methods.len());
    for m in methods {
        for p in &mut m.input {
            includes.extend(mem::take(&mut p.provided_by_module).into_iter());
        }
        includes.extend(mem::take(&mut m.output.provided_by_module).into_iter());
    }

    // preserve order of includes
    let mut uniques = FxHashSet::default();
    includes.retain(|e| uniques.insert(e.clone()));

    includes
}

pub(in crate::cpp) fn generate_c_type(
    ctx: &mut CppContext,
    c_types: &CItems,
    flags: MergeCItemsFlags,
    src_id: SourceId,
) -> Result<(), DiagnosticError> {
    use std::io::Write;

    fn is_item_defined(ctx: &mut CppContext, module_name: &SmolStr, item: &str) -> bool {
        let common_files = &mut ctx.common_files;
        let out: &mut FileWriteCache = file_for_module!(ctx, common_files, module_name);
        out.is_item_defined(item)
    }
    fn define_item(ctx: &mut CppContext, module_name: &SmolStr, item: String) {
        let common_files = &mut ctx.common_files;
        let out: &mut FileWriteCache = file_for_module!(ctx, common_files, module_name);
        out.define_item(item);
    }

    let module_name = &c_types.header_name;
    for c_type in &c_types.items {
        let ctype: &dyn CItemDescriptor = match c_type {
            CItem::Struct(ref s) => s,
            CItem::Union(ref u) => u,
            CItem::Fn(ref f) => {
                let fn_id = format!("fn {}", f.sig.ident);
                if is_item_defined(ctx, module_name, &fn_id) {
                    continue;
                }
                add_func_forward_decl(ctx, f, src_id, module_name)?;
                ctx.rust_code.push(f.into_token_stream());
                define_item(ctx, module_name, fn_id);
                continue;
            }
            CItem::Static(ref s) => {
                let s_id = format!("static {}", s.ident);
                if is_item_defined(ctx, module_name, &s_id) {
                    continue;
                }
                add_const_forward_decl(ctx, s, src_id, module_name)?;
                ctx.rust_code.push(s.into_token_stream());
                define_item(ctx, module_name, s_id);
                continue;
            }
        };
        do_generate_c_type(ctx, flags, src_id, &c_types.header_name, ctype)?;
    }

    Ok(())
}

trait CItemDescriptor: ToTokens {
    fn c_type_prefix(&self) -> &'static str;
    fn name(&self) -> &syn::Ident;
    fn fields(&self) -> Result<&syn::FieldsNamed, syn::Error>;
}

impl CItemDescriptor for syn::ItemStruct {
    fn c_type_prefix(&self) -> &'static str {
        "struct"
    }
    fn name(&self) -> &syn::Ident {
        &self.ident
    }
    fn fields(&self) -> Result<&syn::FieldsNamed, syn::Error> {
        match self.fields {
            syn::Fields::Named(ref x) => Ok(x),
            _ => Err(syn::Error::new(
                self.fields.span(),
                "only fields with names accepted in this context",
            )),
        }
    }
}

impl CItemDescriptor for syn::ItemUnion {
    fn c_type_prefix(&self) -> &'static str {
        "union"
    }
    fn name(&self) -> &syn::Ident {
        &self.ident
    }
    fn fields(&self) -> Result<&syn::FieldsNamed, syn::Error> {
        Ok(&self.fields)
    }
}

fn do_generate_c_type(
    ctx: &mut CppContext,
    flags: MergeCItemsFlags,
    src_id: SourceId,
    c_type_header_name: &SmolStr,
    ctype: &dyn CItemDescriptor,
) -> Result<(), DiagnosticError> {
    use std::io::Write;

    let s_id = format!("{} {}", ctype.c_type_prefix(), ctype.name());
    {
        let common_files = &mut ctx.common_files;
        let file_out: &mut FileWriteCache = file_for_module!(ctx, common_files, c_type_header_name);

        if file_out.is_item_defined(&s_id) {
            return Ok(());
        }
    }
    let mut rust_layout_test = format!(
        r#"
#[allow(non_snake_case)]
#[test]
fn test_{name}_layout() {{
#[repr(C)]
{c_type_prefix} My{name} {{
"#,
        c_type_prefix = ctype.c_type_prefix(),
        name = ctype.name(),
    );
    let mut mem_out = Vec::<u8>::new();
    writeln!(mem_out, "{s_id} {{").expect(WRITE_TO_MEM_FAILED_MSG);

    let mut includes = FxHashSet::<SmolStr>::default();

    let mut fields_asserts_code = String::new();
    let fields = &ctype
        .fields()
        .map_err(|err| DiagnosticError::from_syn_err(src_id, err))?
        .named;
    for f in fields {
        let id = f.ident.as_ref().ok_or_else(|| {
            DiagnosticError::new(
                src_id,
                f.span(),
                "only fields with names accepted in this context",
            )
        })?;
        let rty = ctx.conv_map.find_or_alloc_rust_type(&f.ty, src_id);
        let field_fty = map_repr_c_type(ctx, &rty, rty.src_id_span())?;

        for inc in &field_fty.provided_by_module {
            includes.insert(inc.clone());
        }

        writeln!(mem_out, "    {} {};", field_fty.base.name, id).expect(WRITE_TO_MEM_FAILED_MSG);
        writeln!(rust_layout_test, "{}: {},", id, DisplayToTokens(&f.ty))
            .expect(WRITE_TO_MEM_FAILED_MSG);

        writeln!(
            fields_asserts_code,
            r#"
#[allow(dead_code)]
fn check_{struct_name}_{field_name}_type_fn(s: &{struct_name}) -> &{field_type} {{
    &s.{field_name}
}}
    let offset_our = ((&our_s.{field_name} as *const {field_type}) as usize) - ((&our_s as *const My{struct_name}) as usize);
    let offset_user = ((&user_s.{field_name} as *const {field_type}) as usize) - ((&user_s as *const {struct_name}) as usize);
    assert_eq!(offset_our, offset_user);
"#,
                        struct_name = ctype.name(),
                        field_name = id,
                        field_type = DisplayToTokens(&f.ty),
                    )
            .expect(WRITE_TO_MEM_FAILED_MSG);
    }
    mem_out.write_all(b"};\n").expect(WRITE_TO_MEM_FAILED_MSG);

    writeln!(
        rust_layout_test,
        r#"}}
    assert_eq!(::std::mem::size_of::<My{name}>(), ::std::mem::size_of::<{name}>());
    assert_eq!(::std::mem::align_of::<My{name}>(), ::std::mem::align_of::<{name}>());
    let our_s: My{name} = unsafe {{ ::std::mem::zeroed() }};
    let user_s: {name} = unsafe {{ ::std::mem::zeroed() }};

{fields_asserts}
}}
"#,
        name = ctype.name(),
        fields_asserts = fields_asserts_code,
    )
    .expect(WRITE_TO_MEM_FAILED_MSG);
    match flags {
        MergeCItemsFlags::DefineOnlyCItem => {
            let tt: TokenStream = syn::parse_str(&rust_layout_test).unwrap_or_else(|err| {
                panic_on_syn_error("Internal: layout unit test", rust_layout_test, err)
            });
            ctx.rust_code.push(tt);
        }
        MergeCItemsFlags::DefineAlsoRustType => {
            ctx.rust_code.push(ctype.into_token_stream());
        }
    }
    let self_inc = format!("\"{}\"", c_type_header_name);
    let common_files = &mut ctx.common_files;
    let file_out: &mut FileWriteCache = file_for_module!(ctx, common_files, c_type_header_name);

    for inc in &includes {
        if self_inc != *inc {
            writeln!(file_out, "#include {inc}").expect(WRITE_TO_MEM_FAILED_MSG);
        }
    }
    file_out
        .write_all(
            br##"
#ifdef __cplusplus
extern "C" {
#endif
"##,
        )
        .expect(WRITE_TO_MEM_FAILED_MSG);
    file_out.write_all(&mem_out).expect(WRITE_TO_MEM_FAILED_MSG);
    file_out
        .write_all(
            br##"
#ifdef __cplusplus
} // extern "C" {
#endif
"##,
        )
        .expect(WRITE_TO_MEM_FAILED_MSG);
    file_out.define_item(s_id);
    Ok(())
}

fn add_const_forward_decl(
    ctx: &mut CppContext,
    static_: &syn::ItemStatic,
    src_id: SourceId,
    c_type_header_name: &SmolStr,
) -> Result<(), DiagnosticError> {
    use std::io::Write;

    {
        let common_files = &mut ctx.common_files;
        let out: &mut FileWriteCache = file_for_module!(ctx, common_files, c_type_header_name);
        out.write_all(
            br##"
#ifdef __cplusplus
extern "C" {
#endif
"##,
        )
        .expect(WRITE_TO_MEM_FAILED_MSG);
    }

    let rty = ctx.conv_map.find_or_alloc_rust_type(&static_.ty, src_id);
    let fti = map_repr_c_type(ctx, &rty, (src_id, rty.ty.span()))?;

    let common_files = &mut ctx.common_files;
    let out: &mut FileWriteCache = file_for_module!(ctx, common_files, c_type_header_name);

    write!(
        out,
        "extern const {f_ty} {name};\n",
        f_ty = fti.base.name.display(),
        name = static_.ident
    )
    .expect(WRITE_TO_MEM_FAILED_MSG);

    out.write_all(
        br##"
#ifdef __cplusplus
} // extern "C" {
#endif
"##,
    )
    .expect(WRITE_TO_MEM_FAILED_MSG);
    Ok(())
}

fn add_func_forward_decl(
    ctx: &mut CppContext,
    f: &syn::ItemFn,
    src_id: SourceId,
    c_type_header_name: &SmolStr,
) -> Result<(), DiagnosticError> {
    use std::io::Write;
    {
        let common_files = &mut ctx.common_files;
        let out: &mut FileWriteCache = file_for_module!(ctx, common_files, c_type_header_name);

        out.write_all(
            br##"
#ifdef __cplusplus
extern "C" {
#endif
"##,
        )
        .expect(WRITE_TO_MEM_FAILED_MSG);
    }
    let mut fn_decl_out = Vec::with_capacity(100);
    let mut includes = FxHashSet::<SmolStr>::default();

    match f.sig.output {
        syn::ReturnType::Default => {
            fn_decl_out
                .write_all(b"void")
                .expect(WRITE_TO_MEM_FAILED_MSG);
        }
        syn::ReturnType::Type(_, ref ty) => {
            let rty = ctx.conv_map.find_or_alloc_rust_type(ty, src_id);
            let fti = map_repr_c_type(ctx, &rty, (src_id, rty.ty.span()))?;
            for inc in &fti.provided_by_module {
                includes.insert(inc.clone());
            }
            fn_decl_out
                .write_all(fti.base.name.display().as_bytes())
                .expect(WRITE_TO_MEM_FAILED_MSG);
        }
    }
    write!(&mut fn_decl_out, " {}(", f.sig.ident).expect(WRITE_TO_MEM_FAILED_MSG);

    let fn_args = parse_fn_args(f.sig.inputs.clone())
        .map_err(|err| DiagnosticError::from_syn_err(src_id, err))?
        .0;
    for (i, arg) in fn_args.iter().enumerate() {
        match arg {
            FnArg::SelfArg(sp, _) => {
                return Err(DiagnosticError::new(
                    src_id,
                    *sp,
                    "it is impossible to self argument in this context",
                ))
            }
            FnArg::Default(named_arg) => {
                let rty = ctx.conv_map.find_or_alloc_rust_type(&named_arg.ty, src_id);
                let fti = map_repr_c_type(ctx, &rty, (src_id, rty.ty.span()))?;
                for inc in &fti.provided_by_module {
                    includes.insert(inc.clone());
                }
                if i != 0 {
                    fn_decl_out.write_all(b", ").expect(WRITE_TO_MEM_FAILED_MSG);
                }
                write!(
                    &mut fn_decl_out,
                    "{} {}",
                    fti.base.name.display(),
                    named_arg.name
                )
                .expect(WRITE_TO_MEM_FAILED_MSG);
            }
        }
    }

    fn_decl_out.write_all(b");").expect(WRITE_TO_MEM_FAILED_MSG);

    let common_files = &mut ctx.common_files;
    let out: &mut FileWriteCache = file_for_module!(ctx, common_files, c_type_header_name);
    let self_inc = format!("\"{}\"", c_type_header_name);
    for inc in &includes {
        if self_inc != *inc {
            writeln!(out, "#include {}", inc).expect(WRITE_TO_MEM_FAILED_MSG);
        }
    }
    out.write_all(&fn_decl_out).expect(WRITE_TO_MEM_FAILED_MSG);

    out.write_all(
        br##"
#ifdef __cplusplus
} // extern "C" {
#endif
"##,
    )
    .expect(WRITE_TO_MEM_FAILED_MSG);
    Ok(())
}
