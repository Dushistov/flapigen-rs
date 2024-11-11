use std::{cell::RefCell, rc::Rc, str::FromStr};

use log::{debug, trace};
use proc_macro2::{Ident, Span, TokenStream};
use quote::ToTokens;
use rustc_hash::FxHashMap;
use syn::{
    parse_quote,
    punctuated::Punctuated,
    spanned::Spanned,
    visit_mut::{visit_attribute_mut, VisitMut},
    Item, ItemMod, Token, Type,
};

use crate::{
    error::{DiagnosticError, Result, SourceIdSpan},
    source_registry::SourceId,
    typemap::{
        ast::{
            normalize_type, parse_ty_with_given_span, DisplayToTokens, GenericTypeConv, TypeName,
        },
        ty::{ForeignTypesStorage, RustTypeS},
        typemap_macro::TypeMapConvRuleInfo,
        validate_code_template, TypeConvCode, TypeConvEdge, TypeMap, TypesConvGraph,
    },
    FOREIGN_TYPEMAP,
};

use super::ast::ForeignTypeName;

static MOD_NAME_WITH_FOREIGN_TYPES: &str = "swig_foreign_types_map";
static SWIG_FOREIGNER_TYPE: &str = "swig_foreigner_type";
static SWIG_RUST_TYPE: &str = "swig_rust_type";
static SWIG_RUST_TYPE_NOT_UNIQUE: &str = "swig_rust_type_not_unique";

static SWIG_TO_FOREIGNER_HINT: &str = "swig_to_foreigner_hint";
static SWIG_FROM_FOREIGNER_HINT: &str = "swig_from_foreigner_hint";
static SWIG_CODE: &str = "swig_code";
static SWIG_GENERIC_ARG: &str = "swig_generic_arg";
static SWIG_FROM_ATTR_NAME: &str = "swig_from";
static SWIG_TO_ATTR_NAME: &str = "swig_to";

static SWIG_INTO_TRAIT: &str = "SwigInto";
static SWIG_FROM_TRAIT: &str = "SwigFrom";

type MyAttrs = FxHashMap<String, Vec<(String, Span)>>;

pub(in crate::typemap) fn parse(
    name: SourceId,
    code: &str,
    target_pointer_width: usize,
    traits_usage_code: FxHashMap<Ident, String>,
) -> Result<TypeMap> {
    let file = syn::parse_str::<syn::File>(code)
        .map_err(|err| DiagnosticError::from_syn_err(name, err))?;
    let sym_foreign_types_map = Ident::new(MOD_NAME_WITH_FOREIGN_TYPES, Span::call_site());

    let mut types_map_span: Option<Span> = None;

    let mut ret = TypeMap {
        conv_graph: TypesConvGraph::new(),
        rust_names_map: FxHashMap::default(),
        utils_code: Vec::with_capacity(file.items.len()),
        generic_edges: Vec::<GenericTypeConv>::new(),
        rust_from_foreign_cache: FxHashMap::default(),
        rust_to_foreign_cache: FxHashMap::default(),
        foreign_classes: Vec::new(),
        traits_usage_code,
        ftypes_storage: ForeignTypesStorage::default(),
        not_merged_data: vec![],
        generic_rules: vec![],
    };

    macro_rules! handle_attrs {
        ($item:expr) => {{
            if is_wrong_cfg_pointer_width(&$item.attrs, target_pointer_width) {
                continue;
            }
            my_syn_attrs_to_hashmap(name, &$item.attrs)?
        }};
    }

    fn item_impl_path_is(item_impl: &syn::ItemImpl, var1: &str, var2: &str) -> bool {
        if let syn::ItemImpl {
            trait_: Some((_, ref trait_path, _)),
            ..
        } = item_impl
        {
            is_ident_ignore_params(trait_path, var1) || is_ident_ignore_params(trait_path, var2)
        } else {
            false
        }
    }

    for mut item in file.items {
        match item {
            Item::Mod(ref item_mod) if item_mod.ident == sym_foreign_types_map => {
                if let Some(span) = types_map_span {
                    let mut err = DiagnosticError::new(
                        name,
                        item_mod.span(),
                        format!(
                            "Should only one {} per types map",
                            MOD_NAME_WITH_FOREIGN_TYPES
                        ),
                    );
                    err.span_note((name, span), "Previously defined here");
                    return Err(err);
                }
                types_map_span = Some(item_mod.span());
                debug!("Found foreign_types_map_mod");

                fill_foreign_types_map(name, item_mod, &mut ret)?;
            }
            Item::Impl(ref mut item_impl)
                if item_impl_path_is(item_impl, SWIG_INTO_TRAIT, SWIG_FROM_TRAIT) =>
            {
                let swig_attrs = handle_attrs!(item_impl);
                let mut filter = FilterSwigAttrs;
                filter.visit_item_impl_mut(item_impl);
                handle_into_from_impl(name, &swig_attrs, item_impl, &mut ret)?;
            }
            syn::Item::Trait(mut item_trait) => {
                let swig_attrs = handle_attrs!(item_trait);
                let mut filter = FilterSwigAttrs;
                filter.visit_item_trait_mut(&mut item_trait);
                if !swig_attrs.is_empty() {
                    let conv_code_template = get_swig_code_from_attrs(
                        (name, item_trait.span()),
                        SWIG_CODE,
                        &swig_attrs,
                    )?;

                    ret.traits_usage_code
                        .insert(item_trait.ident.clone(), conv_code_template.to_string());
                }
                ret.utils_code.push(syn::Item::Trait(item_trait));
            }
            Item::Macro(mut item_macro) => {
                if item_macro.mac.path.is_ident(FOREIGN_TYPEMAP) {
                    let tmap_conv_rule: TypeMapConvRuleInfo = syn::parse2(item_macro.mac.tokens)
                        .map_err(|err| DiagnosticError::from_syn_err(name, err))?;

                    ret.may_be_merge_conv_rule(name, tmap_conv_rule)?;
                } else {
                    let swig_attrs = handle_attrs!(item_macro);
                    if swig_attrs.is_empty() {
                        ret.utils_code.push(Item::Macro(item_macro));
                    } else {
                        let mut filter = FilterSwigAttrs;
                        filter.visit_item_macro_mut(&mut item_macro);
                        handle_macro(name, &swig_attrs, item_macro, &mut ret)?;
                    }
                }
            }
            _ => {
                ret.utils_code.push(item);
            }
        }
    }
    Ok(ret)
}

fn fill_foreign_types_map(
    src_id: SourceId,
    item_mod: &syn::ItemMod,
    ret: &mut TypeMap,
) -> Result<()> {
    let names_map = parse_foreign_types_map_mod(src_id, item_mod)?;
    trace!("names_map {:?}", names_map);
    for entry in names_map {
        let TypeNamesMapEntry {
            foreign_name,
            rust_name,
            rust_ty,
        } = entry;
        let rust_name = rust_name.typename;
        let rust_names_map = &mut ret.rust_names_map;
        let conv_graph = &mut ret.conv_graph;
        let graph_idx = *rust_names_map.entry(rust_name.clone()).or_insert_with(|| {
            let idx = conv_graph.add_node(Rc::new(RustTypeS::new_without_graph_idx(
                rust_ty, rust_name, src_id,
            )));
            Rc::get_mut(&mut conv_graph[idx])
                .expect("Internal error: can not modify Rc")
                .graph_idx = idx;
            idx
        });

        ret.add_foreign_rust_ty_idx(foreign_name, graph_idx)?;
    }
    Ok(())
}

#[derive(Debug)]
struct TypeNamesMapEntry {
    foreign_name: ForeignTypeName,
    rust_name: TypeName,
    rust_ty: Type,
}

fn parse_foreign_types_map_mod(src_id: SourceId, item: &ItemMod) -> Result<Vec<TypeNamesMapEntry>> {
    let mut ftype: Option<ForeignTypeName> = None;

    let mut names_map = FxHashMap::<ForeignTypeName, (TypeName, Type)>::default();

    for a in &item.attrs {
        if a.path().is_ident(SWIG_FOREIGNER_TYPE) {
            if let syn::Meta::NameValue(syn::MetaNameValue {
                value:
                    syn::Expr::Lit(syn::ExprLit {
                        lit: syn::Lit::Str(ref value),
                        ..
                    }),
                ..
            }) = a.meta
            {
                ftype = Some(ForeignTypeName::new(value.value(), (src_id, value.span())));
            } else {
                return Err(DiagnosticError::new(
                    src_id,
                    a.meta.span(),
                    "Expect name value attribute",
                ));
            }
        } else if a.path().is_ident(SWIG_RUST_TYPE) {
            if let Some(ftype) = ftype.take() {
                let attr_value = if let syn::Meta::NameValue(syn::MetaNameValue {
                    value:
                        syn::Expr::Lit(syn::ExprLit {
                            lit: syn::Lit::Str(ref value),
                            ..
                        }),
                    ..
                }) = a.meta
                {
                    value
                } else {
                    return Err(DiagnosticError::new(
                        src_id,
                        a.meta.span(),
                        "Expect name value attribute",
                    ));
                };
                let span = attr_value.span();
                let mut attr_value_tn = TypeName::new(attr_value.value());

                let rust_ty = parse_ty_with_given_span(&attr_value_tn.typename, span)
                    .map_err(|err| DiagnosticError::from_syn_err(src_id, err))?;
                attr_value_tn.typename = normalize_type(&rust_ty).into();
                names_map.insert(ftype, (attr_value_tn, rust_ty));
            } else {
                return Err(DiagnosticError::new(
                    src_id,
                    a.span(),
                    format!("No {SWIG_FOREIGNER_TYPE} for {SWIG_RUST_TYPE}"),
                ));
            }
        } else if a.path().is_ident(SWIG_RUST_TYPE_NOT_UNIQUE) {
            if let Some(ftype) = ftype.take() {
                let attr_value = if let syn::Meta::NameValue(syn::MetaNameValue {
                    value:
                        syn::Expr::Lit(syn::ExprLit {
                            lit: syn::Lit::Str(ref value),
                            ..
                        }),
                    ..
                }) = a.meta
                {
                    value
                } else {
                    return Err(DiagnosticError::new(
                        src_id,
                        a.meta.span(),
                        "Expect name value attribute",
                    ));
                };
                let span = attr_value.span();
                let mut attr_value_tn = TypeName::new(attr_value.value());
                let rust_ty = parse_ty_with_given_span(&attr_value_tn.typename, span)
                    .map_err(|err| DiagnosticError::from_syn_err(src_id, err))?;
                attr_value_tn.typename = normalize_type(&rust_ty).into();
                let unique_name = RustTypeS::make_unique_typename(
                    &attr_value_tn.typename,
                    ftype.typename.value(),
                );
                names_map.insert(ftype, (TypeName::new(unique_name), rust_ty));
            } else {
                return Err(DiagnosticError::new(
                    src_id,
                    a.span(),
                    format!("No {SWIG_FOREIGNER_TYPE} for {SWIG_RUST_TYPE_NOT_UNIQUE}"),
                ));
            }
        } else {
            return Err(DiagnosticError::new(
                src_id,
                a.span(),
                format!("Unexpected attribute: '{}'", DisplayToTokens(a)),
            ));
        }
    }

    Ok(names_map
        .into_iter()
        .map(|(k, v)| TypeNamesMapEntry {
            foreign_name: k,
            rust_name: v.0,
            rust_ty: v.1,
        })
        .collect())
}

fn is_wrong_cfg_pointer_width(attrs: &[syn::Attribute], target_pointer_width: usize) -> bool {
    let mut result = false;
    for a in attrs {
        if a.path().is_ident("cfg") {
            if let Err(err) = a.parse_nested_meta(|meta| {
                if meta.path.is_ident("target_pointer_width") {
                    let value = meta.value()?;
                    let value: syn::LitStr = value.parse()?;
                    match <usize>::from_str(&value.value()) {
                        Ok(width) => {
                            result = target_pointer_width != width;
                        }
                        Err(err) => {
                            eprintln!("Error during parse of value of cfg item: {err}");
                        }
                    }
                }
                Ok(())
            }) {
                eprintln!("Error during parse cfg item: {err}");
                break;
            }
        }
    }

    result
}

fn my_syn_attrs_to_hashmap(src_id: SourceId, attrs: &[syn::Attribute]) -> Result<MyAttrs> {
    static KNOWN_SWIG_ATTRS: [&str; 6] = [
        SWIG_TO_FOREIGNER_HINT,
        SWIG_FROM_FOREIGNER_HINT,
        SWIG_CODE,
        SWIG_GENERIC_ARG,
        SWIG_FROM_ATTR_NAME,
        SWIG_TO_ATTR_NAME,
    ];
    let mut ret = FxHashMap::default();
    for a in attrs {
        if KNOWN_SWIG_ATTRS.iter().any(|x| a.path().is_ident(x)) {
            if let syn::Meta::NameValue(syn::MetaNameValue {
                ref path,
                value:
                    syn::Expr::Lit(syn::ExprLit {
                        lit: syn::Lit::Str(ref value),
                        ..
                    }),
                ..
            }) = a.meta
            {
                ret.entry(path.into_token_stream().to_string())
                    .or_insert_with(Vec::new)
                    .push((value.value(), a.span()));
            } else {
                return Err(DiagnosticError::new(src_id, a.span(), "Invalid attribute"));
            }
        }
    }
    Ok(ret)
}

fn get_swig_code_from_attrs<'b>(
    item_span: SourceIdSpan,
    swig_code_attr_name: &str,
    attrs: &'b MyAttrs,
) -> Result<&'b str> {
    if let Some(swig_code) = attrs.get(swig_code_attr_name) {
        if swig_code.len() != 1 {
            Err(DiagnosticError::new2(
                item_span,
                format!(
                    "Expect to have {} attribute, and it should be only one",
                    swig_code_attr_name
                ),
            ))
        } else {
            let (ref conv_code_template, sp) = swig_code[0];
            validate_code_template((item_span.0, sp), conv_code_template.as_str())?;
            Ok(conv_code_template)
        }
    } else {
        Err(DiagnosticError::new2(
            item_span,
            format!("No {} attribute", swig_code_attr_name),
        ))
    }
}

fn handle_into_from_impl(
    src_id: SourceId,
    swig_attrs: &MyAttrs,
    item_impl: &syn::ItemImpl,
    ret: &mut TypeMap,
) -> Result<()> {
    let to_suffix = if !swig_attrs.is_empty() && swig_attrs.contains_key(SWIG_TO_FOREIGNER_HINT) {
        if swig_attrs.len() != 1 || swig_attrs[SWIG_TO_FOREIGNER_HINT].len() != 1 {
            return Err(DiagnosticError::new(
                src_id,
                item_impl.span(),
                format!("Expect only {} attribute", SWIG_TO_FOREIGNER_HINT),
            ));
        }
        Some(swig_attrs[SWIG_TO_FOREIGNER_HINT][0].0.clone())
    } else {
        None
    };

    let from_suffix = if !swig_attrs.is_empty() && swig_attrs.contains_key(SWIG_FROM_FOREIGNER_HINT)
    {
        if swig_attrs.len() != 1 || swig_attrs[SWIG_FROM_FOREIGNER_HINT].len() != 1 {
            return Err(DiagnosticError::new(
                src_id,
                item_impl.span(),
                format!("Expect only {} attribute", SWIG_FROM_FOREIGNER_HINT),
            ));
        }
        Some(swig_attrs[SWIG_FROM_FOREIGNER_HINT][0].0.clone())
    } else {
        None
    };
    let trait_path = if let Some((_, ref trait_path, _)) = item_impl.trait_ {
        trait_path
    } else {
        unreachable!();
    };
    let type_param = extract_trait_param_type(src_id, trait_path)?;

    let (from_ty, to_ty, trait_name) = if is_ident_ignore_params(trait_path, SWIG_INTO_TRAIT) {
        (
            (*item_impl.self_ty).clone(),
            type_param.clone(),
            SWIG_INTO_TRAIT,
        )
    } else {
        (
            type_param.clone(),
            (*item_impl.self_ty).clone(),
            SWIG_FROM_TRAIT,
        )
    };

    let conv_code: &String = ret
        .traits_usage_code
        .get(&Ident::new(trait_name, Span::call_site()))
        .ok_or_else(|| {
            DiagnosticError::new(
                src_id,
                item_impl.span(),
                "Can not find conversion code for SwigInto/SwigFrom",
            )
        })?;

    if item_impl.generics.type_params().next().is_some() {
        trace!("handle_into_from_impl: generics {:?}", item_impl.generics);
        let item_code = item_impl.into_token_stream();
        ret.generic_edges.push(GenericTypeConv {
            src_id,
            from_ty,
            to_ty,
            code: TypeConvCode::new(conv_code.clone(), (src_id, item_impl.span())),
            dependency: Rc::new(RefCell::new(Some(item_code))),
            generic_params: item_impl.generics.clone(),
            to_foreigner_hint: get_foreigner_hint_for_generic(
                src_id,
                &item_impl.generics,
                swig_attrs,
                ForeignHintVariant::To,
            )?,
            from_foreigner_hint: get_foreigner_hint_for_generic(
                src_id,
                &item_impl.generics,
                swig_attrs,
                ForeignHintVariant::From,
            )?,
        });
    } else {
        let item_code = item_impl.into_token_stream();
        add_conv_code(
            src_id,
            (from_ty, from_suffix),
            (to_ty, to_suffix),
            item_code,
            TypeConvCode::new(conv_code.clone(), (src_id, item_impl.span())),
            ret,
        );
    }
    Ok(())
}

fn handle_macro(
    src_id: SourceId,
    swig_attrs: &MyAttrs,
    item_macro: syn::ItemMacro,
    ret: &mut TypeMap,
) -> Result<()> {
    assert!(!swig_attrs.is_empty());

    debug!("conversion macro {:?}", item_macro.ident);

    let from_typename = swig_attrs.get(SWIG_FROM_ATTR_NAME).ok_or_else(|| {
        DiagnosticError::new(
            src_id,
            item_macro.span(),
            format!(
                "No {} but there are other attr {:?}",
                SWIG_FROM_ATTR_NAME, swig_attrs
            ),
        )
    })?;

    assert!(!from_typename.is_empty());
    let to_typename = swig_attrs.get(SWIG_TO_ATTR_NAME).ok_or_else(|| {
        DiagnosticError::new(
            src_id,
            item_macro.span(),
            format!(
                "No {} but there are other attr {:?}",
                SWIG_TO_ATTR_NAME, swig_attrs
            ),
        )
    })?;
    assert!(!to_typename.is_empty());

    let code_template =
        get_swig_code_from_attrs((src_id, item_macro.span()), SWIG_CODE, swig_attrs)?;

    if let Some(generic_types) = swig_attrs.get(SWIG_GENERIC_ARG) {
        assert!(!generic_types.is_empty());
        let mut types_list = Punctuated::<Type, Token![,]>::new();

        fn spanned_str_to_type(src_id: SourceId, (name, span): &(String, Span)) -> Result<Type> {
            let ty: Type = parse_ty_with_given_span(name, *span)
                .map_err(|err| DiagnosticError::from_syn_err(src_id, err))?;
            Ok(ty)
        }

        for g_ty in generic_types {
            types_list.push(spanned_str_to_type(src_id, g_ty)?);
        }
        let generic_params: syn::Generics = parse_quote! { <#types_list> };

        let from_ty: Type = spanned_str_to_type(src_id, &from_typename[0])?;
        let to_ty: Type = spanned_str_to_type(src_id, &to_typename[0])?;

        let to_foreigner_hint = get_foreigner_hint_for_generic(
            src_id,
            &generic_params,
            swig_attrs,
            ForeignHintVariant::To,
        )?;
        let from_foreigner_hint = get_foreigner_hint_for_generic(
            src_id,
            &generic_params,
            swig_attrs,
            ForeignHintVariant::From,
        )?;

        let item_macro_span = (src_id, item_macro.span());
        let item_code = item_macro.into_token_stream();

        ret.generic_edges.push(GenericTypeConv {
            src_id,
            from_ty,
            to_ty,
            code: TypeConvCode::new(code_template, item_macro_span),
            dependency: Rc::new(RefCell::new(Some(item_code))),
            generic_params,
            to_foreigner_hint,
            from_foreigner_hint,
        });
    } else {
        unimplemented!();
    }

    Ok(())
}

fn extract_trait_param_type(src_id: SourceId, trait_path: &syn::Path) -> Result<&Type> {
    if trait_path.segments.len() != 1 {
        return Err(DiagnosticError::new(
            src_id,
            trait_path.span(),
            "Invalid trait path",
        ));
    }
    if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
        ref args,
        ..
    }) = trait_path.segments[0].arguments
    {
        if args.len() != 1 {
            return Err(DiagnosticError::new(
                src_id,
                args.span(),
                "Should be only one generic argument",
            ));
        }
        if let syn::GenericArgument::Type(ref ty) = args[0] {
            Ok(ty)
        } else {
            Err(DiagnosticError::new(
                src_id,
                args[0].span(),
                "Expect type here",
            ))
        }
    } else {
        Err(DiagnosticError::new(
            src_id,
            trait_path.segments[0].arguments.span(),
            "Expect generic arguments here",
        ))
    }
}

#[derive(PartialEq, Clone, Copy)]
enum ForeignHintVariant {
    From,
    To,
}

fn get_foreigner_hint_for_generic(
    src_id: SourceId,
    generic: &syn::Generics,
    attrs: &MyAttrs,
    variant: ForeignHintVariant,
) -> Result<Option<String>> {
    let attr_name = if variant == ForeignHintVariant::To {
        SWIG_TO_FOREIGNER_HINT
    } else {
        SWIG_FROM_FOREIGNER_HINT
    };

    if let Some(attrs) = attrs.get(attr_name) {
        assert!(!attrs.is_empty());
        if attrs.len() != 1 {
            let mut err = DiagnosticError::new(
                src_id,
                attrs[1].1,
                format!("Several {} attributes", attr_name),
            );
            err.span_note((src_id, attrs[0].1), format!("First {attr_name}"));
            return Err(err);
        }
        let mut ty_params = generic.type_params();
        let first_ty_param = ty_params.next();
        if first_ty_param.is_none() || ty_params.next().is_some() {
            return Err(DiagnosticError::new(
                src_id,
                generic.span(),
                format!("Expect exactly one generic parameter for {}", attr_name),
            ));
        }
        let first_ty_param = first_ty_param.expect("should have value");

        if !attrs[0]
            .0
            .as_str()
            .contains(first_ty_param.ident.to_string().as_str())
        {
            let mut err = DiagnosticError::new(
                src_id,
                attrs[0].1,
                format!("{} not contains {}", attr_name, first_ty_param.ident),
            );
            err.span_note(
                (src_id, generic.span()),
                format!("{} defined here", first_ty_param.ident),
            );
            return Err(err);
        }
        Ok(Some(attrs[0].0.clone()))
    } else {
        Ok(None)
    }
}

fn add_conv_code(
    src_id: SourceId,
    (from_ty, from_suffix): (Type, Option<String>),
    (to_ty, to_suffix): (Type, Option<String>),
    item_code: TokenStream,
    conv_code: TypeConvCode,
    ret: &mut TypeMap,
) {
    let from = ret.find_or_alloc_rust_type_with_may_be_suffix(&from_ty, from_suffix, src_id);
    let to = ret.find_or_alloc_rust_type_with_may_be_suffix(&to_ty, to_suffix, src_id);
    debug!("add_conv_code: from {} to {}", from, to);
    ret.conv_graph.update_edge(
        from.graph_idx,
        to.graph_idx,
        TypeConvEdge::new(conv_code, Some(item_code)),
    );
}

fn is_ident_ignore_params<I>(path: &syn::Path, ident: I) -> bool
where
    syn::Ident: PartialEq<I>,
{
    // without check path.segments[0].arguments.is_none() like in Path::is_ident
    path.leading_colon.is_none() && path.segments.len() == 1 && path.segments[0].ident == ident
}

struct FilterSwigAttrs;

impl VisitMut for FilterSwigAttrs {
    fn visit_attribute_mut(&mut self, i: &mut syn::Attribute) {
        if i.path()
            .clone()
            .into_token_stream()
            .to_string()
            .starts_with("swig_")
        {
            *i = syn::parse_quote! { #[doc = ""] };
        } else {
            visit_attribute_mut(self, i);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::error::invalid_src_id_span;

    use super::*;
    use rustc_hash::FxHashSet;
    use syn::parse_quote;

    #[test]
    fn test_parsing_only_types_map_mod() {
        let _ = env_logger::try_init();
        let types_map = parse(
            SourceId::none(),
            r#"
mod swig_foreign_types_map {
    #![swig_foreigner_type="boolean"]
    #![swig_rust_type="jboolean"]
    #![swig_foreigner_type="int"]
    #![swig_rust_type="jint"]
}
"#,
            64,
            FxHashMap::default(),
        )
        .unwrap();
        let ftype_set = {
            let mut set = FxHashSet::default();
            for ftype in types_map.ftypes_storage.iter() {
                let r1 = ftype.into_from_rust.as_ref().unwrap();
                let r2 = ftype.from_into_rust.as_ref().unwrap();
                assert_eq!(r1.rust_ty, r2.rust_ty);
                set.insert((
                    ftype.name.typename.clone(),
                    types_map.conv_graph[r1.rust_ty].normalized_name.as_str(),
                ));
            }
            set
        };

        assert_eq!(
            {
                let mut set = FxHashSet::default();
                set.insert(("boolean".into(), "jboolean"));
                set.insert(("int".into(), "jint"));
                set
            },
            ftype_set
        );
    }

    #[test]
    fn test_parse_foreign_types_map_mod() {
        let mod_item = syn::parse_str::<ItemMod>(
            r#"
mod swig_foreign_types_map {
    #![swig_foreigner_type="boolean"]
    #![swig_rust_type="jboolean"]
    #![swig_foreigner_type="short"]
    #![swig_rust_type="jshort"]
    #![swig_foreigner_type="int"]
    #![swig_rust_type="jint"]
}
"#,
        )
        .unwrap();
        let map = parse_foreign_types_map_mod(SourceId::none(), &mod_item).unwrap();
        assert_eq!(
            vec![
                ("boolean".into(), "jboolean".into()),
                ("int".into(), "jint".into()),
                ("short".into(), "jshort".into()),
            ],
            {
                let mut ret = map
                    .into_iter()
                    .map(|v| (v.foreign_name.typename, v.rust_name.typename))
                    .collect::<Vec<_>>();
                ret.sort_by(|a, b| a.0.cmp(&b.0));
                ret
            }
        );
    }

    #[test]
    fn test_double_map_err() {
        parse(
            SourceId::none(),
            r#"
mod swig_foreign_types_map {}
mod swig_foreign_types_map {}
"#,
            64,
            FxHashMap::default(),
        )
        .unwrap_err();
    }

    #[test]
    fn test_parse_cfg_target_width() {
        let _ = env_logger::try_init();
        let item_impl: syn::ItemImpl = parse_quote! {
            #[swig_to_foreigner_hint = "T"]
            #[cfg(target_pointer_width = "64")]
            impl SwigFrom<isize> for jlong {
                fn swig_from(x: isize, _: *mut JNIEnv) -> Self {
                    x as jlong
                }
            }
        };
        assert!(is_wrong_cfg_pointer_width(&item_impl.attrs, 32));
        assert!(!is_wrong_cfg_pointer_width(&item_impl.attrs, 64));
    }

    #[test]
    fn test_my_syn_attrs_to_hashmap() {
        let item_impl: syn::ItemImpl = parse_quote! {
            #[swig_to_foreigner_hint = "T"]
            #[cfg(target_pointer_width = "64")]
            impl SwigFrom<isize> for jlong {
                fn swig_from(x: isize, _: *mut JNIEnv) -> Self {
                    x as jlong
                }
            }
        };
        assert_eq!(
            vec![("swig_to_foreigner_hint".into(), vec!["T".into()])],
            my_syn_attrs_to_hashmap(SourceId::none(), &item_impl.attrs)
                .unwrap()
                .into_iter()
                .map(|(k, v)| (k, v.into_iter().map(|v| v.0).collect::<Vec<_>>()))
                .collect::<Vec<_>>()
        );

        let item_impl: syn::ItemImpl = parse_quote! {
            #[swig_to_foreigner_hint = "T"]
            #[swig_code = "let mut {to_var}: {to_var_type} = <{to_var_type}>::swig_from({from_var});"]
            #[cfg(target_pointer_width = "64")]
            impl SwigFrom<isize> for jlong {
                fn swig_from(x: isize, _: *mut JNIEnv) -> Self {
                    x as jlong
                }
            }
        };
        assert_eq!(
            {
                let mut v = vec![
                    ("swig_to_foreigner_hint".into(), vec!["T".into()]),
                    (
                        "swig_code".into(),
                        vec![
                        "let mut {to_var}: {to_var_type} = <{to_var_type}>::swig_from({from_var});"
                            .into()
                    ],
                    ),
                ];
                v.sort();
                v
            },
            {
                let mut v: Vec<_> = my_syn_attrs_to_hashmap(SourceId::none(), &item_impl.attrs)
                    .unwrap()
                    .into_iter()
                    .map(|(k, v)| (k, v.into_iter().map(|v| v.0).collect::<Vec<_>>()))
                    .collect();
                v.sort();
                v
            }
        );
    }

    #[test]
    fn test_extract_trait_param_type() {
        let trait_impl: syn::ItemImpl = parse_quote! {
            impl<'bugaga> SwigFrom<jobject> for Option<&'bugaga str> {
                fn swig_from(x: jobject) -> Self {
                    unimplemented!();
                }
            }
        };
        let trait_impl_path = trait_impl.trait_.unwrap().1;

        assert_eq!(
            {
                let ty: Type = parse_quote!(jobject);
                ty
            },
            *extract_trait_param_type(SourceId::none(), &trait_impl_path).unwrap()
        );
    }

    #[test]
    fn test_get_foreigner_hint_for_generic() {
        let trait_impl: syn::ItemImpl = parse_quote! {
            #[swig_to_foreigner_hint = "T"]
            impl<T: SwigForeignClass> SwigFrom<T> for *mut ::std::os::raw::c_void {
                fn swig_from(x: T) -> Self {
                    unimplemented!();
                }
            }
        };
        let my_attrs = my_syn_attrs_to_hashmap(SourceId::none(), &trait_impl.attrs).unwrap();
        assert_eq!(
            "T",
            get_foreigner_hint_for_generic(
                SourceId::none(),
                &trait_impl.generics,
                &my_attrs,
                ForeignHintVariant::To
            )
            .unwrap()
            .unwrap()
        );
    }

    #[test]
    fn test_parse_trait_with_code() {
        let _ = env_logger::try_init();
        let mut conv_map = parse(
            SourceId::none(),
            r#"
#[allow(dead_code)]
#[swig_code = "let {to_var}: {to_var_type} = {from_var}.swig_into(env);"]
trait SwigInto<T> {
    fn swig_into(self, env: *mut JNIEnv) -> T;
}

impl SwigInto<bool> for jboolean {
    fn swig_into(self, _: *mut JNIEnv) -> bool {
        self != 0
    }
}

#[allow(dead_code)]
#[swig_code = "let {to_var}: {to_var_type} = <{to_var_type}>::swig_from({from_var}, env);"]
trait SwigFrom<T> {
    fn swig_from(_: T, env: *mut JNIEnv) -> Self;
}
impl SwigFrom<bool> for jboolean {
    fn swig_from(x: bool, _: *mut JNIEnv) -> Self {
        if x {
            1 as jboolean
        } else {
            0 as jboolean
        }
    }
}
"#,
            64,
            FxHashMap::default(),
        )
        .unwrap();

        let jboolean_ty =
            conv_map.find_or_alloc_rust_type(&parse_type! { jboolean }, SourceId::none());
        let bool_ty = conv_map.find_or_alloc_rust_type(&parse_type! { bool }, SourceId::none());

        let (_, code) = conv_map
            .convert_rust_types(
                jboolean_ty.to_idx(),
                bool_ty.to_idx(),
                "a0",
                "a1",
                "jlong",
                invalid_src_id_span(),
            )
            .unwrap();
        assert_eq!("    let a1: bool = a0.swig_into(env);\n".to_string(), code);

        let (_, code) = conv_map
            .convert_rust_types(
                bool_ty.to_idx(),
                jboolean_ty.to_idx(),
                "a0",
                "a1",
                "jlong",
                invalid_src_id_span(),
            )
            .unwrap();

        assert_eq!(
            "    let a1: jboolean = <jboolean>::swig_from(a0, env);\n".to_string(),
            code
        );
    }

    #[test]
    fn test_parse_conv_impl_with_type_params() {
        let _ = env_logger::try_init();

        let mut conv_map = parse(
            SourceId::none(),
            r#"
#[allow(dead_code)]
#[swig_code = "let {to_var}: {to_var_type} = <{to_var_type}>::swig_from({from_var}, env);"]
trait SwigFrom<T> {
    fn swig_from(_: T, env: *mut JNIEnv) -> Self;
}

impl<T: SwigForeignClass> SwigFrom<T> for jobject {
    fn swig_from(x: T, env: *mut JNIEnv) -> Self {
        object_to_jobject(x, <T>::jni_class_name(), env)
    }
}

foreign_typemap!(
    ($p:r_type) <T> Arc<Mutex<T>> => &Mutex<T> {
        $out = & $p;
    };
);

foreign_typemap!(
    ($p:r_type) <T> &Mutex<T> => MutexGuard<T> {
        $out = $p.lock().unwrap();
    };
);

foreign_typemap!(
    ($p:r_type) <T> MutexGuard<T> => &T {
        $out = & $p;
    };
);
"#,
            64,
            FxHashMap::default(),
        )
        .unwrap();

        conv_map.find_or_alloc_rust_type_that_implements(
            &parse_type! { Foo },
            &["SwigForeignClass"],
            SourceId::none(),
        );
        let arc_mutex_foo =
            conv_map.find_or_alloc_rust_type(&parse_type! { Arc<Mutex<Foo>> }, SourceId::none());
        let foo_ref = conv_map.find_or_alloc_rust_type(&parse_type! { &Foo }, SourceId::none());

        let (_, code) = conv_map
            .convert_rust_types(
                arc_mutex_foo.to_idx(),
                foo_ref.to_idx(),
                "a0",
                "a1",
                "jlong",
                invalid_src_id_span(),
            )
            .unwrap();
        assert_eq!(
            r#"    let mut a1: & Mutex < Foo > = & a0 ;
    let mut a1: MutexGuard < Foo > = a1 . lock () . unwrap () ;
    let mut a1: & Foo = & a1 ;
"#
            .to_string(),
            code
        );
    }

    #[test]
    fn test_parse_macros_conv() {
        let mut conv_map = parse(
            SourceId::none(),
            r#"
mod swig_foreign_types_map {
    #![swig_foreigner_type="byte"]
    #![swig_rust_type="jbyte"]
    #![swig_foreigner_type="short"]
    #![swig_rust_type="jshort"]
}

#[swig_code = "let {to_var}: {to_var_type} = <{to_var_type}>::swig_from({from_var}, env);"]
trait SwigFrom<T> {
    fn swig_from(_: T, env: *mut JNIEnv) -> Self;
}

impl SwigFrom<u8> for jshort {
    fn swig_from(x: u8, _: *mut JNIEnv) -> Self {
        x as jshort
    }
}


#[allow(unused_macros)]
#[swig_generic_arg = "T"]
#[swig_generic_arg = "E"]
#[swig_from = "Result<T, E>"]
#[swig_to = "T"]
#[swig_code = "let {to_var}: {to_var_type} = jni_unpack_return!({from_var}, env);"]
macro_rules! jni_unpack_return {
    ($result_value:expr, $default_value:expr, $env:ident) => {
        {
            let ret = match $result_value {
                Ok(x) => x,
                Err(msg) => {
                    jni_throw_exception($env, &msg);
                    return $default_value;
                }
            };
            ret
        }
    }
}
"#,
            64,
            FxHashMap::default(),
        )
        .unwrap();

        let foo_ty = conv_map.find_or_alloc_rust_type(&parse_type! { Foo }, SourceId::none());
        let result_foo_str_ty = conv_map
            .find_or_alloc_rust_type(&parse_type! { Result<Foo, String> }, SourceId::none());

        let (_, code) = conv_map
            .convert_rust_types(
                result_foo_str_ty.to_idx(),
                foo_ty.to_idx(),
                "a0",
                "a1",
                "jlong",
                invalid_src_id_span(),
            )
            .unwrap();
        assert_eq!(
            r#"    let a1: Foo = jni_unpack_return!(a0, env);
"#,
            code
        );

        let result_u8_str_ty = conv_map
            .find_or_alloc_rust_type(&parse_type! { Result<u8, &'static str> }, SourceId::none());
        let jshort_ty = conv_map.find_or_alloc_rust_type(&parse_type! { jshort }, SourceId::none());

        let (_, code) = conv_map
            .convert_rust_types(
                result_u8_str_ty.to_idx(),
                jshort_ty.to_idx(),
                "a0",
                "a1",
                "jlong",
                invalid_src_id_span(),
            )
            .unwrap();
        assert_eq!(
            r#"    let a1: u8 = jni_unpack_return!(a0, env);
    let a1: jshort = <jshort>::swig_from(a1, env);
"#,
            code
        );
    }

    #[test]
    fn test_parse_main_lang_typemaps() {
        parse(
            SourceId::none(),
            include_str!("../java_jni/jni-include.rs"),
            64,
            FxHashMap::default(),
        )
        .unwrap();
        parse(
            SourceId::none(),
            include_str!("../cpp/cpp-include.rs"),
            64,
            FxHashMap::default(),
        )
        .unwrap();
    }
}
