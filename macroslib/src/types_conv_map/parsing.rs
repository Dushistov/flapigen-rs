use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use syntex_syntax::parse::{PResult, ParseSess};
use syntex_syntax::symbol::Symbol;
use syntex_syntax::{ast, codemap, parse};
use syntex_pos::Span;
use syntex_syntax::codemap::Spanned;
use syntex_syntax::ptr::P;

use my_ast::{normalized_ty_string, parse_ty, GenericTypeConv, RustType};
use super::{validate_code_template, NodeIndex, TypeConvEdge, TypeGraphIdx, TypesConvGraph,
            TypesConvMap};
use types_conv_map::{make_unique_rust_typename, make_unique_rust_typename_if_need};
use errors::fatal_error;

static MOD_NAME_WITH_FOREIGN_TYPES: &'static str = "swig_foreign_types_map";
static SWIG_FOREIGNER_TYPE: &'static str = "swig_foreigner_type";
static SWIG_RUST_TYPE: &'static str = "swig_rust_type";

pub(in types_conv_map) fn parse_types_conv_map<'a>(
    sess: &'a ParseSess,
    name: &str,
    code: &str,
    mut traits_usage_code: HashMap<Symbol, Symbol>,
) -> PResult<'a, TypesConvMap> {

    let swig_code = Symbol::intern("swig_code");
    let swig_to_foreigner_hint = Symbol::intern("swig_to_foreigner_hint");
    let swig_from_foreigner_hint = Symbol::intern("swig_from_foreigner_hint");
    let swig_into_trait = Symbol::intern("SwigInto");
    let swig_from_trait = Symbol::intern("SwigFrom");
    let swig_deref_trait = Symbol::intern("SwigDeref");
    let swig_deref_mut_trait = Symbol::intern("SwigDerefMut");
    let target_assoc_type = Symbol::intern("Target");
    let swig_generic_arg = Symbol::intern("swig_generic_arg");
    let swig_from_attr_name = Symbol::intern("swig_from");
    let swig_to_attr_name = Symbol::intern("swig_to");

    let mut parser = parse::new_parser_from_source_str(sess, name.into(), code.into());

    let krate = parser.parse_crate_mod()?;

    let mut names_map_span: Option<Span> = None;
    let mut conv_graph = TypesConvGraph::new();
    let mut foreign_names_map = HashMap::new();
    let mut rust_names_map = HashMap::new();
    let mut utils_code = Vec::new();
    let mut generic_edges = Vec::<GenericTypeConv>::new();

    for item in krate.module.items {
        let mut item: ast::Item = item.unwrap();

        if is_foreign_types_map_mod(&item) {
            if let Some(span) = names_map_span {
                let mut err = fatal_error(
                    sess,
                    parser.span,
                    &format!(
                        "Should only one {} per types map",
                        MOD_NAME_WITH_FOREIGN_TYPES
                    ),
                );
                err.span_note(span, "Previously defined here");
                return Err(err);
            }
            names_map_span = Some(item.span);
            let names_map = parse_foreign_types_map_mod(sess, &item)?;
            debug!("names_map {:?}", names_map);
            for entry in names_map {
                let TypeNamesMapEntry {
                    foreign_name,
                    rust_name,
                    rust_ty,
                } = entry;
                let graph_id = *rust_names_map
                    .entry(rust_name)
                    .or_insert_with(|| conv_graph.add_node(RustType::new(rust_ty, rust_name)));
                assert!(!foreign_names_map.contains_key(&foreign_name));
                foreign_names_map.insert(foreign_name, graph_id);
            }
            continue;
        }

        let (swig_attrs, other_attrs): (Vec<ast::Attribute>, Vec<ast::Attribute>) = item.attrs
            .into_iter()
            .partition(|attr| attr.value.name.as_str().starts_with("swig_"));
        item.attrs = other_attrs;
        let swig_attrs = ast_attrs_to_hashmap(sess, swig_attrs)?;

        match item {
            ast::Item {
                node: ast::ItemKind::Impl(
                    ast::Unsafety::Normal,
                    ast::ImplPolarity::Positive,
                    ref generic,
                    Some(ref trait_type),
                    ref for_type,
                    _,
                ),
                ..
            } if trait_path_match(&trait_type.path, "SwigInto") ||
                trait_path_match(&trait_type.path, "SwigFrom") =>
            {
                let to_suffix = if !swig_attrs.is_empty() &&
                    swig_attrs.contains_key(&swig_to_foreigner_hint)
                {
                    if swig_attrs.len() != 1 || swig_attrs[&swig_to_foreigner_hint].len() != 1 {
                        return Err(fatal_error(
                            sess,
                            item.span,
                            &format!("Expect only {} attribute", swig_to_foreigner_hint.as_str()),
                        ));
                    }
                    Some(swig_attrs[&swig_to_foreigner_hint][0].0)
                } else {
                    None
                };

                let from_suffix = if !swig_attrs.is_empty() &&
                    swig_attrs.contains_key(&swig_from_foreigner_hint)
                {
                    if swig_attrs.len() != 1 || swig_attrs[&swig_from_foreigner_hint].len() != 1 {
                        return Err(fatal_error(
                            sess,
                            item.span,
                            &format!(
                                "Expect only {} attribute",
                                swig_from_foreigner_hint.as_str()
                            ),
                        ));
                    }
                    Some(swig_attrs[&swig_from_foreigner_hint][0].0)
                } else {
                    None
                };

                let type_param = extract_trait_param_type(sess, trait_type)?;
                let (from_ty, to_ty, trait_name) =
                    if trait_path_match(&trait_type.path, "SwigInto") {
                        ((**for_type).clone(), type_param, swig_into_trait)
                    } else {
                        (type_param, (**for_type).clone(), swig_from_trait)
                    };

                let conv_code = *traits_usage_code.get(&trait_name).ok_or_else(|| {
                    fatal_error(
                        sess,
                        item.span,
                        "Can not find conversation code for SwigInto/SwigFrom",
                    )
                })?;

                if generic.is_type_parameterized() {
                    debug!("generic {:?}", generic);
                    generic_edges.push(GenericTypeConv {
                        from_ty,
                        to_ty,
                        code_template: conv_code,
                        dependency: Rc::new(RefCell::new(Some(item.clone()))),
                        generic_params: generic.clone(),
                        to_foreigner_hint: get_to_foreigner_hint_for_generic(
                            sess,
                            generic,
                            &swig_attrs,
                        )?,
                    });
                } else {
                    add_conv_code(
                        (from_ty, from_suffix),
                        (to_ty, to_suffix),
                        P(item.clone()),
                        conv_code,
                        &mut conv_graph,
                        &mut rust_names_map,
                    );
                }
            }
            ast::Item {
                node: ast::ItemKind::Trait(..),
                ..
            } => {
                let trait_name = item.ident;
                if !swig_attrs.is_empty() {
                    let conv_code_template =
                        get_swig_code_from_attrs(sess, item.span, swig_code, &swig_attrs)?;

                    traits_usage_code.insert(trait_name.name, conv_code_template);
                }
                utils_code.push(P(item));
            }
            ast::Item {
                node: ast::ItemKind::Impl(
                    ast::Unsafety::Normal,
                    ast::ImplPolarity::Positive,
                    ref generic,
                    Some(ref trait_type),
                    ref for_type,
                    ref impl_items,
                ),
                ..
            } if trait_path_match(&trait_type.path, "SwigDeref") ||
                trait_path_match(&trait_type.path, "SwigDerefMut") =>
            {
                let target_ty = unpack_first_associated_type(impl_items, target_assoc_type)
                    .ok_or_else(|| fatal_error(sess, item.span, "No Target associated type"))?;
                debug!(
                    "parsing swigderef target {:?}, for_type {:?}",
                    target_ty,
                    for_type
                );
                let deref_target_name = normalized_ty_string(&target_ty);
                let (deref_trait, to_typename, mutbl) =
                    if trait_path_match(&trait_type.path, "SwigDeref") {
                        (
                            swig_deref_trait,
                            Symbol::intern(&format!("&{}", deref_target_name.as_str())),
                            ast::Mutability::Immutable,
                        )
                    } else {
                        (
                            swig_deref_mut_trait,
                            Symbol::intern(&format!("&mut {}", deref_target_name.as_str())),
                            ast::Mutability::Mutable,
                        )
                    };
                let conv_code = *traits_usage_code.get(&deref_trait).ok_or_else(|| {
                    fatal_error(
                        sess,
                        item.span,
                        "Can not find conversation code for SwigDeref/SwigDerefMut",
                    )
                })?;
                let from_ty = (**for_type).clone();
                //for_type -> &Target
                if generic.is_type_parameterized() {
                    let to_ty = ast::Ty {
                        node: ast::TyKind::Rptr(
                            None,
                            ast::MutTy {
                                ty: P(target_ty.clone()),
                                mutbl,
                            },
                        ),
                        ..target_ty.clone()
                    };
                    generic_edges.push(GenericTypeConv {
                        from_ty,
                        to_ty,
                        code_template: conv_code,
                        dependency: Rc::new(RefCell::new(Some(item.clone()))),
                        generic_params: generic.clone(),
                        to_foreigner_hint: get_to_foreigner_hint_for_generic(
                            sess,
                            generic,
                            &swig_attrs,
                        )?,
                    });

                } else {
                    let to_ty = if let Some(ty_type_idx) = rust_names_map.get(&to_typename) {
                        conv_graph[*ty_type_idx].ty.clone()
                    } else {
                        ast::Ty {
                            node: ast::TyKind::Rptr(
                                None,
                                ast::MutTy {
                                    ty: P(target_ty.clone()),
                                    mutbl: ast::Mutability::Immutable,
                                },
                            ),
                            ..target_ty.clone()
                        }
                    };
                    add_conv_code(
                        (from_ty, None),
                        (to_ty, None),
                        P(item.clone()),
                        conv_code,
                        &mut conv_graph,
                        &mut rust_names_map,
                    );
                }
            }
            ast::Item {
                node: ast::ItemKind::Mac(_),
                ..
            } => if !swig_attrs.is_empty() {
                debug!("conversation macro {:?}", item.ident);
                let from_typename = swig_attrs.get(&swig_from_attr_name).ok_or_else(|| {
                    fatal_error(
                        sess,
                        item.span,
                        &format!(
                            "No {} but there are other attr {:?}",
                            swig_from_attr_name,
                            swig_attrs
                        ),
                    )
                })?;
                assert!(!from_typename.is_empty());
                let to_typename = swig_attrs.get(&swig_to_attr_name).ok_or_else(|| {
                    fatal_error(
                        sess,
                        item.span,
                        &format!(
                            "No {} but there are other attr {:?}",
                            swig_to_attr_name,
                            swig_attrs
                        ),
                    )
                })?;
                assert!(!to_typename.is_empty());

                let code_template =
                    get_swig_code_from_attrs(sess, item.span, swig_code, &swig_attrs)?;

                if let Some(generic_types) = swig_attrs.get(&swig_generic_arg) {
                    assert!(!generic_types.is_empty());
                    let ty_params = swig_attrs_to_ty_params(generic_types);
                    assert!(!ty_params.is_empty());
                    let generic_span = ty_params.last().unwrap().span;
                    let generic_params = ast::Generics {
                        lifetimes: vec![],
                        ty_params,
                        where_clause: ast::WhereClause {
                            id: ast::DUMMY_NODE_ID,
                            predicates: vec![],
                        },
                        span: generic_span,
                    };
                    let from_ty = parse_ty(sess, from_typename[0].1, from_typename[0].0)?;
                    let to_ty = parse_ty(sess, to_typename[0].1, to_typename[0].0)?;
                    let to_foreigner_hint =
                        get_to_foreigner_hint_for_generic(sess, &generic_params, &swig_attrs)?;
                    generic_edges.push(GenericTypeConv {
                        from_ty,
                        to_ty,
                        code_template,
                        dependency: Rc::new(RefCell::new(Some(item.clone()))),
                        generic_params,
                        to_foreigner_hint,
                    });
                } else {
                    unimplemented!();
                }
            } else {
                utils_code.push(P(item.clone()));
            },
            _ => utils_code.push(P(item)),            
        }
    }

    Ok(TypesConvMap {
        conv_graph,
        foreign_names_map,
        rust_names_map,
        utils_code,
        generic_edges,
        rust_to_foreign_cache: HashMap::new(),
        foreign_classes: Vec::new(),
        exported_enums: HashMap::new(),
        traits_usage_code,
    })
}

fn is_foreign_types_map_mod(item: &ast::Item) -> bool {
    debug!("check is_foreign_types_map_mod");
    let sym_foreign_types_map = Symbol::intern(MOD_NAME_WITH_FOREIGN_TYPES);
    if let ast::ItemKind::Mod(_) = item.node {
        item.ident.name == sym_foreign_types_map
    } else {
        false
    }
}

#[derive(Debug)]
struct TypeNamesMapEntry {
    foreign_name: Symbol,
    rust_name: Symbol,
    rust_ty: ast::Ty,
}

fn parse_foreign_types_map_mod<'a>(
    sess: &'a ParseSess,
    item: &ast::Item,
) -> PResult<'a, Vec<TypeNamesMapEntry>> {
    let mut ftype: Option<Symbol> = None;
    let sym_foreigner_type = Symbol::intern(SWIG_FOREIGNER_TYPE);
    let sym_rust_type = Symbol::intern(SWIG_RUST_TYPE);
    let sym_rust_type_not_unique = Symbol::intern("swig_rust_type_not_unique");

    let mut names_map = HashMap::<Symbol, (Symbol, ast::Ty)>::new();

    for a in &item.attrs {
        let attr_value: Symbol = match a.value.node {
            ast::MetaItemKind::NameValue(codemap::Spanned {
                node: ast::LitKind::Str(lit, _),
                ..
            }) => lit,
            _ => return Err(fatal_error(sess, a.span, "Can not get attribute value")),
        };

        match a.value.name {
            x if x == sym_foreigner_type => {
                if ftype.is_some() {
                    return Err(fatal_error(
                        sess,
                        a.span,
                        "Two foreigner_type without rust_type",
                    ));
                }
                if let Some(val) = names_map.get(&attr_value) {
                    return Err(fatal_error(
                        sess,
                        a.span,
                        &format!(
                            "Such type({}) already defined here {:?}",
                            attr_value.as_str(),
                            val.1
                        ),
                    ));
                }
                ftype = Some(attr_value);
            }
            x if x == sym_rust_type => if let Some(ftype) = ftype.take() {
                let rust_ty = parse_ty(sess, a.span, attr_value)?;
                names_map.insert(ftype, (attr_value, rust_ty));
            } else {
                return Err(fatal_error(
                    sess,
                    a.span,
                    &format!("No {} for {}", SWIG_FOREIGNER_TYPE, SWIG_RUST_TYPE),
                ));
            },
            x if x == sym_rust_type_not_unique => if let Some(ftype) = ftype.take() {
                let rust_ty = parse_ty(sess, a.span, attr_value)?;
                let unique_name = make_unique_rust_typename(attr_value, ftype);
                names_map.insert(ftype, (unique_name, rust_ty));

            } else {
                return Err(fatal_error(
                    sess,
                    a.span,
                    &format!("No {} for {}", SWIG_FOREIGNER_TYPE, SWIG_RUST_TYPE),
                ));
            },
            _ => {
                return Err(fatal_error(
                    sess,
                    a.span,
                    &format!(
                        "{}:{} Unknown name of attribute: '{}'",
                        file!(),
                        line!(),
                        a.value.name
                    ),
                ));
            }
        }

    }


    Ok(
        names_map
            .into_iter()
            .map(|(k, v)| {
                TypeNamesMapEntry {
                    foreign_name: k,
                    rust_name: v.0,
                    rust_ty: v.1,
                }
            })
            .collect(),
    )
}


fn trait_path_match(path: &ast::Path, type_name: &str) -> bool {
    path.segments.len() == 1 &&
        path.segments
            .last()
            .map(|v| &*v.identifier.name.as_str() == type_name)
            .unwrap_or(false)
}

fn get_to_foreigner_hint_for_generic<'a>(
    sess: &'a ParseSess,
    generic: &ast::Generics,
    attrs: &HashMap<Symbol, Vec<(Symbol, Span)>>,
) -> PResult<'a, Option<Symbol>> {
    if let Some(attrs) = attrs.get(&Symbol::intern("swig_to_foreigner_hint")) {
        assert!(!attrs.is_empty());
        if attrs.len() != 1 {
            let mut err = fatal_error(
                sess,
                attrs[1].1,
                "Several swig_to_foreigner_hint attributes",
            );
            err.span_note(attrs[0].1, "First swig_to_foreigner_hint");
            return Err(err);
        }
        if generic.ty_params.len() != 1 {
            return Err(fatal_error(
                sess,
                generic.span,
                "Expect only one generic parameter for swig_to_foreigner_hint",
            ));
        }
        if !(*attrs[0].0.as_str()).contains(&*generic.ty_params[0].ident.name.as_str()) {
            let mut err = fatal_error(
                sess,
                attrs[0].1,
                &format!(
                    "swig_to_foreigner_hint not contains {}",
                    generic.ty_params[0].ident.name
                ),
            );
            err.span_note(
                generic.span,
                &format!("{} defined here", generic.ty_params[0].ident.name),
            );

            return Err(err);
        }
        Ok(Some(attrs[0].0))
    } else {
        Ok(None)
    }
}

fn ast_attrs_to_hashmap(
    sess: &ParseSess,
    attrs: Vec<ast::Attribute>,
) -> PResult<HashMap<Symbol, Vec<(Symbol, Span)>>> {
    let mut ret = HashMap::new();
    for attr in attrs {
        let attr_name: Symbol = attr.value.name;
        match attr.value.node {
            ast::MetaItemKind::NameValue(Spanned {
                node: ast::LitKind::Str(lit, _),
                ..
            }) => {
                ret.entry(attr_name)
                    .or_insert_with(Vec::new)
                    .push((lit, attr.span));
            }
            _ => return Err(fatal_error(sess, attr.span, "Invalid attribute")),
        }
    }
    Ok(ret)
}

fn extract_trait_param_type<'a>(
    sess: &'a ParseSess,
    trait_ref: &ast::TraitRef,
) -> PResult<'a, ast::Ty> {
    assert_eq!(1, trait_ref.path.segments.len());
    let seg = &trait_ref.path.segments[0];
    let param = seg.parameters
        .as_ref()
        .ok_or_else(|| fatal_error(sess, trait_ref.path.span, "No type param"))?;
    match **param {
        ast::PathParameters::AngleBracketed(ref p) => {
            if p.types.len() != 1 {
                return Err(fatal_error(
                    sess,
                    trait_ref.path.span,
                    "Expect one type parameter",
                ));
            }
            let pty: &P<ast::Ty> = &p.types[0];
            Ok((**pty).clone())
        }
        _ => Err(fatal_error(
            sess,
            trait_ref.path.span,
            "Expect type params in <>",
        )),
    }
}

fn add_conv_code(
    from: (ast::Ty, Option<Symbol>),
    to: (ast::Ty, Option<Symbol>),
    item: P<ast::Item>,
    conv_code: Symbol,
    conv_graph: &mut TypesConvGraph,
    rust_names_map: &mut HashMap<Symbol, NodeIndex<TypeGraphIdx>>,
) {
    let (from, from_suffix) = from;
    let from_typename = make_unique_rust_typename_if_need(
        Symbol::intern(&normalized_ty_string(&from)),
        from_suffix,
    );
    let from: RustType = RustType::new(from, from_typename);
    let (to, to_suffix) = to;
    let to_typename =
        make_unique_rust_typename_if_need(Symbol::intern(&normalized_ty_string(&to)), to_suffix);
    let to = RustType::new(to, to_typename);
    debug!(
        "add conv from {} to {}",
        from.normalized_name,
        to.normalized_name
    );
    let from = *rust_names_map
        .entry(from.normalized_name)
        .or_insert_with(|| conv_graph.add_node(from));

    let to = *rust_names_map
        .entry(to.normalized_name)
        .or_insert_with(|| conv_graph.add_node(to));
    conv_graph.add_edge(from, to, TypeConvEdge::new(conv_code, Some(item)));
}

fn unpack_first_associated_type(
    items: &[ast::ImplItem],
    assoc_type_name: Symbol,
) -> Option<ast::Ty> {
    for item in items {
        if item.ident.name == assoc_type_name {
            if let ast::ImplItemKind::Type(ref ty) = item.node {
                return Some((**ty).clone());
            }
        }
    }
    None
}

fn swig_attrs_to_ty_params(attrs: &[(Symbol, Span)]) -> Vec<ast::TyParam> {
    let mut ret = Vec::new();
    for attr in attrs {
        ret.push(ast::TyParam {
            attrs: ast::ThinVec::new(),
            ident: ast::Ident::with_empty_ctxt(attr.0),
            id: ast::DUMMY_NODE_ID,
            bounds: vec![],
            default: None,
            span: attr.1,
        });
    }
    ret
}

fn get_swig_code_from_attrs<'a>(
    sess: &'a ParseSess,
    item_span: Span,
    swig_code_attr_name: Symbol,
    attrs: &HashMap<Symbol, Vec<(Symbol, Span)>>,
) -> PResult<'a, Symbol> {
    if let Some(swig_code) = attrs.get(&swig_code_attr_name) {
        if swig_code.len() != 1 {
            Err(fatal_error(
                sess,
                item_span,
                &format!(
                    "Expect to have {} attribute, and it should be only one",
                    swig_code_attr_name
                ),
            ))
        } else {
            let (conv_code_template, sp) = swig_code[0];
            validate_code_template(sess, sp, &conv_code_template.as_str())?;
            Ok(conv_code_template)
        }
    } else {
        Err(fatal_error(
            sess,
            item_span,
            &format!("No {} attribute", swig_code_attr_name),
        ))
    }
}

#[cfg(test)]
#[macro_use]
#[path = "../test_helper.rs"]
mod test_helper;

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use super::*;
    use syntex_pos::DUMMY_SP;
    use self::test_helper::*;

    #[test]
    fn test_double_map_err() {
        let sess = ParseSess::new();
        let mut err = parse_types_conv_map(
            &sess,
            "double_map_err",
            r#"
mod swig_foreign_types_map {}
mod swig_foreign_types_map {}
"#,
            HashMap::new(),
        ).unwrap_err();
        err.emit();
    }

    #[test]
    fn test_parse_foreign_types_map_mod() {
        let sess = ParseSess::new();
        let mut parser = parse::new_parser_from_source_str(
            &sess,
            "test".into(),
            r#"
mod swig_foreign_types_map {
    #![swig_foreigner_type="boolean"]
    #![swig_rust_type="jboolean"]
    #![swig_foreigner_type="short"]
    #![swig_rust_type="jshort"]
    #![swig_foreigner_type="int"]
    #![swig_rust_type="jint"]
}
"#.into(),
        );
        let item = parser.parse_item().unwrap().unwrap();
        let map = parse_foreign_types_map_mod(&sess, &item).unwrap();
        assert_eq!(
            vec![
                ("boolean".to_string(), "jboolean".to_string()),
                ("int".to_string(), "jint".to_string()),
                ("short".to_string(), "jshort".to_string()),
            ],
            {
                let mut ret = map.into_iter()
                    .map(|v| {
                        (
                            v.foreign_name.as_str().to_string(),
                            v.rust_name.as_str().to_string(),
                        )
                    })
                    .collect::<Vec<_>>();
                ret.sort_by(|a, b| a.0.cmp(&b.0));
                ret
            }
        );
    }

    #[test]
    fn test_parsing_only_types_map_mod() {
        logger_init();
        let sess = ParseSess::new();
        let conv_map = unwrap_presult!(parse_types_conv_map(
            &sess,
            "foreign_mod",
            r#"
mod swig_foreign_types_map {
    #![swig_foreigner_type="boolean"]
    #![swig_rust_type="jboolean"]
    #![swig_foreigner_type="int"]
    #![swig_rust_type="jint"]
}
"#,
            HashMap::new(),
        ));
        assert_eq!(
            {
                let mut set = HashSet::new();
                for (k, v) in conv_map.foreign_names_map {
                    set.insert((k, conv_map.conv_graph[v].normalized_name));
                }
                set
            },
            {
                let mut set = HashSet::new();
                set.insert((Symbol::intern("boolean"), Symbol::intern("jboolean")));
                set.insert((Symbol::intern("int"), Symbol::intern("jint")));
                set
            }
        );
    }

    #[test]
    fn test_parse_trait_with_code() {
        let sess = ParseSess::new();
        let mut conv_map = parse_types_conv_map(
            &sess,
            "trait_with_code",
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
    fn swig_from(T, env: *mut JNIEnv) -> Self;
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
            HashMap::new(),
        ).unwrap();

        let (_, code) = unwrap_presult!(conv_map.convert_rust_types(
            &sess,
            &rust_type_from_str("jboolean"),
            &rust_type_from_str("bool"),
            "a0",
            "jlong",
            DUMMY_SP
        ));
        assert_eq!("    let a0: bool = a0.swig_into(env);\n".to_string(), code);

        let (_, code) = unwrap_presult!(conv_map.convert_rust_types(
            &sess,
            &rust_type_from_str("bool"),
            &rust_type_from_str("jboolean"),
            "a0",
            "jlong",
            DUMMY_SP
        ));

        assert_eq!(
            "    let a0: jboolean = <jboolean>::swig_from(a0, env);\n".to_string(),
            code
        );
    }

    #[test]
    fn test_parse_deref() {
        logger_init();
        let sess = ParseSess::new();
        let mut conv_map = unwrap_presult!(parse_types_conv_map(
            &sess,
            "deref_code",
            r#"
#[allow(dead_code)]
#[swig_code = "let {to_var}: {to_var_type} = {from_var}.swig_deref();"]
trait SwigDeref {
    type Target: ?Sized;
    fn swig_deref(&self) -> &Self::Target;
}

impl SwigDeref for String {
    type Target = str;
    fn swig_deref(&self) -> &str {
        &self
    }
}
"#,
            HashMap::new(),
        ));
        let (_, code) = unwrap_presult!(conv_map.convert_rust_types(
            &sess,
            &rust_type_from_str("String"),
            &rust_type_from_str("&str"),
            "a0",
            "jlong",
            DUMMY_SP
        ));
        assert_eq!("    let a0: &str = a0.swig_deref();\n".to_string(), code);
    }

    #[test]
    fn test_parse_conv_impl_with_type_params() {
        logger_init();
        let sess = ParseSess::new();
        let mut conv_map = unwrap_presult!(parse_types_conv_map(
            &sess,
            "trait_with_type_params_code",
            r#"
#[allow(dead_code)]
#[swig_code = "let {to_var}: {to_var_type} = <{to_var_type}>::swig_from({from_var}, env);"]
trait SwigFrom<T> {
    fn swig_from(T, env: *mut JNIEnv) -> Self;
}

#[allow(dead_code)]
#[swig_code = "let {to_var}: {to_var_type} = {from_var}.swig_deref();"]
trait SwigDeref {
    type Target: ?Sized;
    fn swig_deref(&self) -> &Self::Target;
}

impl<T: SwigForeignClass> SwigFrom<T> for jobject {
    fn swig_from(x: T, env: *mut JNIEnv) -> Self {
        object_to_jobject(x, <T>::jni_class_name(), env)
    }
}

impl<T> SwigDeref for Arc<Mutex<T>> {
    type Target = Mutex<T>;
    fn swig_deref(&self) -> &Mutex<T> {
        &self
    }
}

impl<'a, T> SwigFrom<&'a Mutex<T>> for MutexGuard<'a, T> {
    fn swig_from(m: &'a Mutex<T>, _: *mut JNIEnv) -> MutexGuard<'a, T> {
        m.lock().unwrap()
    }
}

impl<'a, T> SwigDeref for MutexGuard<'a, T> {
    type Target = T;
    fn swig_deref(&self) -> &T {
        &self
    }
}
"#,
            HashMap::new(),
        ));

        conv_map.add_type(rust_type_from_str("Foo").implements("SwigForeignClass"));

        let (_, code) = unwrap_presult!(conv_map.convert_rust_types(
            &sess,
            &rust_type_from_str("Arc<Mutex<Foo>>"),
            &rust_type_from_str("&Foo"),
            "a0",
            "jlong",
            DUMMY_SP
        ));
        assert_eq!(
            r#"    let a0: &Mutex<Foo> = a0.swig_deref();
    let a0: MutexGuard<Foo> = <MutexGuard<Foo>>::swig_from(a0, env);
    let a0: &Foo = a0.swig_deref();
"#.to_string(),
            code
        );
    }

    #[test]
    fn test_parse_macros_conv() {
        logger_init();
        let sess = ParseSess::new();
        let mut conv_map = unwrap_presult!(parse_types_conv_map(
            &sess,
            "macros",
            r#"
mod swig_foreign_types_map {
    #![swig_foreigner_type="byte"]
    #![swig_rust_type="jbyte"]
    #![swig_foreigner_type="short"]
    #![swig_rust_type="jshort"]
}

#[swig_code = "let {to_var}: {to_var_type} = <{to_var_type}>::swig_from({from_var}, env);"]
trait SwigFrom<T> {
    fn swig_from(T, env: *mut JNIEnv) -> Self;
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
            HashMap::new(),
        ));
        conv_map.add_type(rust_type_from_str("Foo"));

        let (_, code) = unwrap_presult!(conv_map.convert_rust_types(
            &sess,
            &rust_type_from_str("Result<Foo, String>"),
            &rust_type_from_str("Foo"),
            "a0",
            "jlong",
            DUMMY_SP
        ));
        assert_eq!(
            r#"    let a0: Foo = jni_unpack_return!(a0, env);
"#,
            code
        );


        let (_, code) = unwrap_presult!(conv_map.convert_rust_types(
            &sess,
            &rust_type_from_str("Result<u8, &'static str>"),
            &rust_type_from_str("jshort"),
            "a0",
            "jlong",
            DUMMY_SP
        ));
        assert_eq!(
            r#"    let a0: u8 = jni_unpack_return!(a0, env);
    let a0: jshort = <jshort>::swig_from(a0, env);
"#,
            code
        );
    }

    fn rust_type_from_str(code: &str) -> RustType {
        let sess = ParseSess::new();
        let ty = parse_ty(&sess, DUMMY_SP, Symbol::intern(code)).unwrap();
        ty.into()
    }
}
