use std::{fmt::Write, mem, rc::Rc};

use proc_macro2::TokenStream;
use quote::ToTokens;
use rustc_hash::FxHashSet;
use smol_str::SmolStr;
use syn::spanned::Spanned;

use crate::{
    code_parse::parse_fn_args,
    cpp::{map_any_err_to_our_err, CppForeignMethodSignature, MergeCItemsFlags},
    error::{panic_on_syn_error, DiagnosticError, SourceIdSpan},
    file_cache::FileWriteCache,
    namegen::new_unique_name,
    source_registry::SourceId,
    typemap::{
        ast::{DisplayToTokens, TyParamsSubstList},
        ty::{ForeignType, TraitNamesSet},
        CItem, CItems, ExpandedFType, TypeMap, TypeMapConvRuleInfoExpanderHelper,
        FROM_VAR_TEMPLATE, TO_VAR_TEMPLATE, TO_VAR_TYPE_TEMPLATE,
    },
    types::{FnArg, ForeignEnumInfo, ForeignerClassInfo},
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
    tmap: &mut TypeMap,
    c_types: &CItems,
    flags: MergeCItemsFlags,
    src_id: SourceId,
    out: &mut FileWriteCache,
) -> Result<Vec<TokenStream>, DiagnosticError> {
    let mut rust_code = vec![];

    for c_type in &c_types.items {
        let ctype: &CItemDescriptor = match c_type {
            CItem::Struct(ref s) => s,
            CItem::Union(ref u) => u,
            CItem::Fn(ref f) => {
                let fn_id = format!("fn {}", f.ident);
                if out.is_item_defined(&fn_id) {
                    continue;
                }
                add_func_forward_decl(f, out, tmap, src_id, c_types.header_name.as_str())?;
                rust_code.push(f.into_token_stream());
                out.define_item(fn_id);
                continue;
            }
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
            syn::Fields::Named(ref x) => Ok(&x),
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
    tmap: &mut TypeMap,
    flags: MergeCItemsFlags,
    src_id: SourceId,
    c_type_header_name: &str,
    ctype: &CItemDescriptor,
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

        let field_fty = simple_map_rust_to_c(tmap, &f.ty, src_id)?;

        for inc in &tmap[field_fty].provides_by_module {
            includes.insert(inc.clone());
        }

        writeln!(
            &mut mem_out,
            "    {} {};",
            tmap[field_fty].name.as_str(),
            id
        )
        .expect(WRITE_TO_MEM_FAILED_MSG);
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
        MergeCItemsFlags::DefineOnlyCItem => {
            let tt: TokenStream = syn::parse_str(&rust_layout_test).unwrap_or_else(|err| {
                panic_on_syn_error("Internal: layout unit test", rust_layout_test, err)
            });
            rust_code.push(tt);
        }
        MergeCItemsFlags::DefineAlsoRustType => {
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

fn is_ty_repr_c(tmap: &TypeMap, ty: &syn::Type, traits: &TraitNamesSet) -> bool {
    match traits.len() {
        0 => true,
        1 => {
            for tname in traits.iter() {
                if tname.is_ident("SwigTypeIsReprC") {
                    if let Some(r_ty) = tmap.ty_to_rust_type_checked(ty) {
                        if tmap
                            .find_foreign_type_related_to_rust_ty(r_ty.to_idx())
                            .is_none()
                        {
                            return false;
                        }
                    } else {
                        return false;
                    }
                } else {
                    return false;
                }
            }
            true
        }
        _ => false,
    }
}

struct FTypeExpander<'a> {
    tmap: &'a TypeMap,
    ty_span: SourceIdSpan,
}

impl<'a> TypeMapConvRuleInfoExpanderHelper for FTypeExpander<'_> {
    fn swig_i_type(&mut self, _ty: &syn::Type) -> Result<syn::Type, DiagnosticError> {
        unimplemented!()
    }
    fn swig_from_rust_to_i_type(
        &mut self,
        _ty: &syn::Type,
        _in_var_name: &str,
        _out_var_name: &str,
    ) -> Result<String, DiagnosticError> {
        unimplemented!()
    }
    fn swig_from_i_type_to_rust(
        &mut self,
        _ty: &syn::Type,
        _in_var_name: &str,
        _out_var_name: &str,
    ) -> Result<String, DiagnosticError> {
        unimplemented!()
    }
    fn swig_f_type(
        &mut self,
        ty: &syn::Type,
        _: Option<petgraph::Direction>,
    ) -> Result<ExpandedFType, DiagnosticError> {
        let rty = self.tmap.ty_to_rust_type_checked(ty).ok_or_else(|| {
            DiagnosticError::new2(
                self.ty_span,
                format!("Unknown rust type {}", DisplayToTokens(ty)),
            )
        })?;

        let fty = self
            .tmap
            .find_foreign_type_related_to_rust_ty(rty.to_idx())
            .ok_or_else(|| {
                DiagnosticError::new2(
                    self.ty_span,
                    format!("No related foreign type to rust type {}", rty),
                )
            })?;
        let ft = &self.tmap[fty];
        Ok(ExpandedFType {
            name: ft.name.typename.clone(),
            provides_by_module: ft.provides_by_module.clone(),
        })
    }
    fn swig_foreign_to_i_type(
        &mut self,
        _ty: &syn::Type,
        _var_name: &str,
    ) -> Result<String, DiagnosticError> {
        unimplemented!()
    }
    fn swig_foreign_from_i_type(
        &mut self,
        _ty: &syn::Type,
        _var_name: &str,
    ) -> Result<String, DiagnosticError> {
        unimplemented!()
    }
}

fn simple_map_rust_to_c(
    tmap: &mut TypeMap,
    ty: &syn::Type,
    src_id: SourceId,
) -> Result<ForeignType, DiagnosticError> {
    let rty = tmap.find_or_alloc_rust_type(ty, src_id);
    match tmap.find_foreign_type_related_to_rust_ty(rty.to_idx()) {
        Some(x) => Ok(x),
        None => {
            // direction not important
            let direction = petgraph::Direction::Outgoing;
            let idx_subst_map: Option<(Rc<_>, TyParamsSubstList)> =
                tmap.generic_rules().iter().find_map(|grule| {
                    if grule.if_simple_rtype_ftype_map().is_some() {
                        grule
                            .is_ty_subst_of_my_generic_rtype(
                                &rty.ty,
                                direction,
                                |ty, traits| -> bool { is_ty_repr_c(tmap, ty, traits) },
                            )
                            .map(|sm| (grule.clone(), sm.into()))
                    } else {
                        None
                    }
                });
            if let Some((grule, subst_list)) = idx_subst_map {
                let subst_map = subst_list.as_slice().into();
                let new_rule = grule.subst_generic_params(
                    subst_map,
                    direction,
                    &mut FTypeExpander {
                        tmap,
                        ty_span: (src_id, ty.span()),
                    },
                )?;
                if new_rule.ftype_left_to_right.len() != 1
                    || new_rule
                        .ftype_left_to_right
                        .iter()
                        .any(|x| x.cfg_option.is_some())
                {
                    return Err(DiagnosticError::new(
                        src_id,
                        ty.span(),
                        "found generic rule for this type, but it contains options",
                    ));
                }
                tmap.merge_conv_rule(grule.src_id, new_rule)?;
                tmap.find_foreign_type_related_to_rust_ty(rty.to_idx())
                    .ok_or_else(|| {
                        DiagnosticError::new(
                            src_id,
                            ty.span(),
                            format!("no foreign type related to type '{}'", rty),
                        )
                    })
            } else {
                Err(DiagnosticError::new(
                    src_id,
                    ty.span(),
                    format!("no foreign type related to type '{}'", rty),
                ))
            }
        }
    }
}

fn add_func_forward_decl(
    f: &syn::ItemFn,
    out: &mut FileWriteCache,
    tmap: &mut TypeMap,
    src_id: SourceId,
    c_type_header_name: &str,
) -> Result<(), DiagnosticError> {
    use std::io::Write;

    out.write_all(
        br##"
#ifdef __cplusplus
extern "C" {
#endif
"##,
    )
    .expect(WRITE_TO_MEM_FAILED_MSG);

    let mut fn_decl_out = Vec::with_capacity(100);
    let mut includes = FxHashSet::<SmolStr>::default();

    match f.decl.output {
        syn::ReturnType::Default => {
            fn_decl_out
                .write_all(b"void")
                .expect(WRITE_TO_MEM_FAILED_MSG);
        }
        syn::ReturnType::Type(_, ref ty) => {
            let fti = simple_map_rust_to_c(tmap, &*ty, src_id)?;
            for inc in &tmap[fti].provides_by_module {
                includes.insert(inc.clone());
            }
            fn_decl_out
                .write_all(tmap[fti].name.as_str().as_bytes())
                .expect(WRITE_TO_MEM_FAILED_MSG);
        }
    }
    write!(&mut fn_decl_out, " {}(", f.ident).expect(WRITE_TO_MEM_FAILED_MSG);

    let fn_args = parse_fn_args(f.decl.inputs.clone())
        .map_err(|err| DiagnosticError::from_syn_err(src_id, err))?;
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
                let fti = simple_map_rust_to_c(tmap, &named_arg.ty, src_id)?;
                for inc in &tmap[fti].provides_by_module {
                    includes.insert(inc.clone());
                }
                if i != 0 {
                    fn_decl_out.write_all(b", ").expect(WRITE_TO_MEM_FAILED_MSG);
                }
                write!(
                    &mut fn_decl_out,
                    "{} {}",
                    tmap[fti].name.as_str(),
                    named_arg.name
                )
                .expect(WRITE_TO_MEM_FAILED_MSG);
            }
        }
    }

    fn_decl_out.write_all(b");").expect(WRITE_TO_MEM_FAILED_MSG);

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
