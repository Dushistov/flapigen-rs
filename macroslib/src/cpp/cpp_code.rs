use std::{fmt::Write, mem};

use proc_macro2::TokenStream;
use quote::ToTokens;
use rustc_hash::FxHashSet;
use smol_str::SmolStr;
use syn::spanned::Spanned;

use crate::{
    cpp::{map_any_err_to_our_err, CppForeignMethodSignature, MergeCTypesFlags},
    error::{panic_on_syn_error, DiagnosticError},
    file_cache::FileWriteCache,
    namegen::new_unique_name,
    source_registry::SourceId,
    typemap::{
        ast::DisplayToTokens, CType, CTypes, TypeMap, FROM_VAR_TEMPLATE, TO_VAR_TEMPLATE,
        TO_VAR_TYPE_TEMPLATE,
    },
    types::{ForeignEnumInfo, ForeignerClassInfo},
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
    for (i, (f_type_info, arg_name)) in f_method.input.iter().zip(name_iter).enumerate() {
        if i > 0 {
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

pub(in crate::cpp) fn c_class_type(class: &ForeignerClassInfo) -> String {
    format!("{}Opaque", class.name)
}

pub(in crate::cpp) fn cpp_generate_args_with_types<'a, NI: Iterator<Item = &'a str>>(
    f_method: &CppForeignMethodSignature,
    arg_name_iter: NI,
) -> String {
    let mut ret = String::new();
    for (i, (f_type_info, arg_name)) in f_method.input.iter().zip(arg_name_iter).enumerate() {
        if i > 0 {
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
    mut known_names: FxHashSet<SmolStr>,
    arg_name_iter: NI,
) -> (String, String) {
    let mut conv_deps = String::new();
    let mut converted_args = String::new();
    for (i, (f_type_info, arg_name)) in f_method.input.iter().zip(arg_name_iter).enumerate() {
        if i > 0 {
            converted_args.push_str(", ");
        }
        if let Some(conv) = f_type_info.cpp_converter.as_ref() {
            let conv_code = conv.converter.as_str().replace(FROM_VAR_TEMPLATE, arg_name);
            if !conv_code.contains(TO_VAR_TYPE_TEMPLATE) {
                converted_args.push_str(&conv_code);
            } else {
                let templ = format!("a{}", i);
                let var_name = new_unique_name(&known_names, &templ);
                converted_args.push_str(&format!("std::move({})", var_name));
                let conv_code = conv_code
                    .replace(
                        TO_VAR_TYPE_TEMPLATE,
                        &format!("{} {}", f_type_info.as_ref().name, var_name),
                    )
                    .replace(TO_VAR_TEMPLATE, &var_name);
                conv_deps.push_str(&conv_code);
                known_names.insert(var_name);
            }
        } else {
            converted_args.push_str(arg_name);
        }
    }
    (conv_deps, converted_args)
}

pub(in crate::cpp) fn cpp_header_name(class: &ForeignerClassInfo) -> String {
    format!("{}.hpp", class.name)
}

pub(in crate::cpp) fn c_header_name(class: &ForeignerClassInfo) -> String {
    format!("c_{}.h", class.name)
}

pub(in crate::cpp) fn cpp_header_name_for_enum(enum_info: &ForeignEnumInfo) -> String {
    format!("c_{}.h", enum_info.name)
}

pub(in crate::cpp) fn cpp_list_required_includes(
    methods: &mut [CppForeignMethodSignature],
) -> Vec<SmolStr> {
    let mut includes = FxHashSet::<SmolStr>::default();
    for m in methods {
        for p in &mut m.input {
            includes.extend(mem::replace(&mut p.provides_by_module, Vec::new()).into_iter());
        }
        includes.extend(mem::replace(&mut m.output.provides_by_module, Vec::new()).into_iter());
    }

    let mut ret: Vec<_> = includes.into_iter().collect();
    ret.sort();
    ret
}

pub(in crate::cpp) fn generate_c_type(
    tmap: &TypeMap,
    c_types: &CTypes,
    flags: MergeCTypesFlags,
    src_id: SourceId,
    out: &mut FileWriteCache,
) -> Result<Vec<TokenStream>, DiagnosticError> {
    let mut rust_code = vec![];

    for c_type in &c_types.types {
        let ctype: &CTypeDescriptor = match c_type {
            CType::Struct(ref s) => s,
            CType::Union(ref u) => u,
        };
        do_generate_c_type(
            tmap,
            flags,
            src_id,
            c_types.header_name.as_str(),
            ctype,
            out,
            &mut rust_code,
        )?;
    }

    Ok(rust_code)
}

trait CTypeDescriptor: ToTokens {
    fn c_type_prefix(&self) -> &'static str;
    fn name(&self) -> &syn::Ident;
    fn fields(&self) -> Result<&syn::FieldsNamed, syn::Error>;
}

impl CTypeDescriptor for syn::ItemStruct {
    fn c_type_prefix(&self) -> &'static str {
        "struct"
    }
    fn name(&self) -> &syn::Ident {
        &self.ident
    }
    fn fields(&self) -> Result<&syn::FieldsNamed, syn::Error> {
        match self.fields {
            syn::Fields::Named(ref x) => Ok(&x),
            _ => Err(syn::Error::new(
                self.fields.span(),
                "only fields with names accepted in this context",
            )),
        }
    }
}

impl CTypeDescriptor for syn::ItemUnion {
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
    tmap: &TypeMap,
    flags: MergeCTypesFlags,
    src_id: SourceId,
    c_type_header_name: &str,
    ctype: &CTypeDescriptor,
    file_out: &mut FileWriteCache,
    rust_code: &mut Vec<TokenStream>,
) -> Result<(), DiagnosticError> {
    use std::io::Write;

    let s_id = format!("{} {}", ctype.c_type_prefix(), ctype.name());
    if file_out.is_item_defined(&s_id) {
        return Ok(());
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
    writeln!(&mut mem_out, "{} {{", s_id).map_err(map_any_err_to_our_err)?;

    let mut includes = FxHashSet::<&str>::default();

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
        let field_rty = tmap.ty_to_rust_type_checked(&f.ty).ok_or_else(|| {
            DiagnosticError::new(
                src_id,
                f.ty.span(),
                format!("unknown Rust type: '{}'", DisplayToTokens(&f.ty)),
            )
        })?;
        let field_fty = tmap
            .find_foreign_type_related_to_rust_ty(field_rty.to_idx())
            .ok_or_else(|| {
                DiagnosticError::new(src_id, f.ty.span(), "no foreign type related to this type")
            })?;
        for inc in &tmap[field_fty].provides_by_module {
            includes.insert(inc.as_str());
        }

        writeln!(
            &mut mem_out,
            "    {} {};",
            tmap[field_fty].name.as_str(),
            id
        )
        .map_err(map_any_err_to_our_err)?;
        writeln!(&mut rust_layout_test, "{}: {},", id, DisplayToTokens(&f.ty))
            .map_err(map_any_err_to_our_err)?;

        writeln!(
                        &mut fields_asserts_code,
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
                    .map_err(map_any_err_to_our_err)?;
    }
    mem_out.write_all(b"};\n").map_err(map_any_err_to_our_err)?;

    writeln!(
        &mut rust_layout_test,
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
    .map_err(map_any_err_to_our_err)?;
    match flags {
        MergeCTypesFlags::DefineOnlyCType => {
            let tt: TokenStream = syn::parse_str(&rust_layout_test).unwrap_or_else(|err| {
                panic_on_syn_error("Internal: layout unit test", rust_layout_test, err)
            });
            rust_code.push(tt);
        }
        MergeCTypesFlags::DefineAlsoRustType => {
            rust_code.push(ctype.into_token_stream());
        }
    }
    let self_inc = format!("\"{}\"", c_type_header_name);
    for inc in &includes {
        if self_inc != *inc {
            writeln!(file_out, "#include {}", inc).map_err(map_any_err_to_our_err)?;
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
        .map_err(map_any_err_to_our_err)?;
    file_out
        .write_all(&mem_out)
        .map_err(map_any_err_to_our_err)?;
    file_out
        .write_all(
            br##"
#ifdef __cplusplus
} // extern "C" {
#endif
"##,
        )
        .map_err(map_any_err_to_our_err)?;
    file_out.define_item(s_id);
    Ok(())
}
