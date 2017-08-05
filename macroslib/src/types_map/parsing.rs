use std::collections::HashMap;

use syntex_syntax::symbol::Symbol;
use syntex_syntax::parse::ParseSess;
use syntex_syntax::{ast, codemap, parse};
use syntex_pos::Span;
use syntex_syntax::ptr::P;
use syntex_syntax::ast::TraitRef;
use syntex_syntax::codemap::Spanned;

use petgraph::graph::NodeIndex;

use ForeignTypesMap;
use utils::fatal_error;
use types_map::{ForeignTypeConv, TypeGraphIdx, TypeConvNode, TypeConvEdge, FrontierTypeNode,
                make_unique_rust_typename};
use types_map::norm_ty::normalized_ty_string;
use my_ast::{path_match, unpack_first_associated_type, path_unpack_generic_first_parameter};

struct NamesPair {
    foreign_name: Symbol,
    rust_name: Symbol,
}

/// may panics
pub(in types_map) fn parse_types_map(session: &ParseSess,
                                     name: &str,
                                     code: &str)
                                     -> ForeignTypesMap {
    fn add_conv_code(from_type: Symbol,
                     to_type: Symbol,
                     code: Symbol,
                     rust_names_map: &mut HashMap<Symbol, NodeIndex<TypeGraphIdx>>,
                     conv_graph: &mut ForeignTypeConv,
                     item: ast::Item) {
        let from = *rust_names_map
            .entry(from_type)
            .or_insert_with(|| conv_graph.add_node(TypeConvNode::Internal(from_type)));
        let to = *rust_names_map
            .entry(to_type)
            .or_insert_with(|| conv_graph.add_node(TypeConvNode::Internal(to_type)));
        conv_graph.add_edge(from, to, TypeConvEdge::new(code, Some(item)));
    }

    let mut parser = parse::new_parser_from_source_str(session, name.into(), code.into());

    let mut krate = match parser.parse_crate_mod() {
        Ok(x) => x,
        Err(mut diag) => {
            diag.emit();
            panic!("{}: {}: Can not parse {}", file!(), line!(), name);
        }
    };

    let mut names_map_span: Option<Span> = None;
    let mut conv_graph = ForeignTypeConv::new();
    let mut foreign_names_map = HashMap::new();
    let mut rust_names_map = HashMap::new();
    let mut utils_code = Vec::new();

    let sym_to_foreigner_hint = Symbol::intern("to_foreigner_hint");

    for item in krate.module.items.drain(..) {
        debug!("parsing item: {}\n", item.ident);

        let to_foreigner_hint = item.attrs
            .iter()
            .enumerate()
            .filter_map(|(idx, attr)| {
                let attr_name: Symbol = attr.value.name;
                match attr.value.node {
                    ast::MetaItemKind::NameValue(Spanned {
                                                     node: ast::LitKind::Str(lit, _), ..
                                                 }) if attr_name == sym_to_foreigner_hint => {
                        Some((idx, lit))
                    }
                    _ => None,
                }
            })
            .nth(0);

        match *item {
            _ if is_foreign_types_map_module(&*item) => {
                if let Some(span) = names_map_span {
                    fatal_error(session,
                                &item.span,
                                &format!("Another foreign type map mod, previous defined here {:?}",
                                         span));
                }
                names_map_span = Some(item.span);
                let mut names_map = parse_foreign_types_map_module(session, &*item);
                for it in names_map.drain(..) {
                    assert!(!foreign_names_map.contains_key(&it.foreign_name));
                    let node = conv_graph.add_node(TypeConvNode::Frontier(FrontierTypeNode {
                                                                              foreign:
                                                                                  it.foreign_name,
                                                                              rust: it.rust_name,
                                                                          }));
                    foreign_names_map.insert(it.foreign_name, node);
                    rust_names_map.entry(it.rust_name).or_insert_with(|| {
                        conv_graph.add_node(TypeConvNode::Internal(it.rust_name))
                    });
                }
            }
            ast::Item {
                node: ast::ItemKind::Impl(ast::Unsafety::Normal,
                                    ast::ImplPolarity::Positive,
                                    _,
                                    Some(ref trait_type),
                                    ref for_type,
                                    _),
                ..
            } if path_match(&trait_type.path, "SwigInto") ||
                 path_match(&trait_type.path, "SwigFrom") => {
                let conv = match parse_type_converter(trait_type, for_type) {
                    Ok(x) => x,
                    Err(msg) => fatal_error(session, &item.span, msg),
                };
                let new_item = if let Some(foreigner_type_idx) =
                    to_foreigner_hint.map(|(idx, _)| idx) {
                    let mut x = (*item).clone();
                    x.attrs.remove(foreigner_type_idx);
                    x
                } else {
                    (*item).clone()
                };
                add_conv_code(conv.from_type,
                              make_unique_rust_typename_if_need(conv.to_type,
                                                                to_foreigner_hint.map(|(_,
                                                                                        v)| v)),
                              conv.code,
                              &mut rust_names_map,
                              &mut conv_graph,
                              new_item);
            }
            ast::Item {
                node: ast::ItemKind::Impl(ast::Unsafety::Normal,
                                    ast::ImplPolarity::Positive,
                                    _,
                                    Some(ref trait_type),
                                    ref for_type,
                                    ref impl_items),
                ..
            } if path_match(&trait_type.path, "SwigDeref") => {
                let target =
                    unpack_first_associated_type(impl_items, "Target").unwrap_or_else(|| {
                        fatal_error(session, &item.span, "no associated Target for Deref")
                    });
                debug!("target {:?}, for_type {:?}", target, for_type);
                let for_typename = normalized_ty_string(for_type);
                let deref_target_name = normalized_ty_string(&target);
                let to_typename = format!("&{}", deref_target_name.as_str());
                //for_type -> &Target
                add_conv_code(Symbol::intern(&for_typename),
                              Symbol::intern(&to_typename),
                              Symbol::intern(
                                  &format!("let {{to_var}}: {} = {{from_var}}.swig_deref();",
                                           to_typename)),
                              &mut rust_names_map,
                              &mut conv_graph,
                              (*item).clone());
            }
            _ => utils_code.push(P((*item).clone())),
        }
    }

    let empty_code = Symbol::intern("");
    for fnode in foreign_names_map.values() {
        let rust_typename = conv_graph[*fnode].frontier_ref_unwrap().rust;
        let rnode = match rust_names_map.get(&rust_typename) {
            Some(x) => x,
            None => continue,
        };
        if !conv_graph.contains_edge(*rnode, *fnode) {
            conv_graph.add_edge(*rnode, *fnode, TypeConvEdge::new(empty_code, None));
        }

        if !conv_graph.contains_edge(*fnode, *rnode) {
            conv_graph.add_edge(*fnode, *rnode, TypeConvEdge::new(empty_code, None));
        }
    }

    ForeignTypesMap {
        conv_graph,
        foreign_names_map,
        rust_names_map,
        utils_code,
    }
}

fn make_unique_rust_typename_if_need(rust_typename: Symbol, suffix: Option<Symbol>) -> Symbol {
    match suffix {
        Some(s) => make_unique_rust_typename(rust_typename, s),
        None => rust_typename,
    }
}

struct TypeConverter {
    from_type: Symbol,
    to_type: Symbol,
    code: Symbol,
}

fn parse_type_converter(trait_type: &TraitRef,
                        for_type: &ast::Ty)
                        -> Result<TypeConverter, &'static str> {
    let is_into = path_match(&trait_type.path, "SwigInto");
    let is_from = path_match(&trait_type.path, "SwigFrom");

    let rust_type =
        path_unpack_generic_first_parameter(&trait_type.path,
                                            if is_into { "SwigInto" } else { "SwigFrom" })
            .ok_or("Expect only SwigFrom|SwigInto trait")?;
    let rust_typename = Symbol::intern(&normalized_ty_string(&rust_type));
    let for_typename = Symbol::intern(&normalized_ty_string(for_type));

    debug!("parse_type_converter is_into {}, rust_type {}, for_typename {}",
           is_into,
           rust_typename,
           for_typename);
    match (is_into, is_from) {
        (true, false) => {
            Ok(TypeConverter {
                from_type: for_typename,
                to_type: rust_typename,
                code: Symbol::intern(&format!(
                    r#"
let {{to_var}}: {} = {{from_var}}.swig_into(env);
"#,
                    rust_typename
                )),
            })
        }
        (false, true) => {
            Ok(TypeConverter {
                from_type: rust_typename,
                to_type: for_typename,
                code: Symbol::intern(&format!(
                    r#"
    let {{to_var}}: {} = {}::swig_from({{from_var}}, env);
"#,
                    for_typename,
                    for_typename
                )),
            })
        }
        _ => Err("Unknown trait in types map"),
    }
}


fn parse_foreign_types_map_module(sess: &ParseSess, item: &ast::Item) -> Vec<NamesPair> {
    let mut ftype: Option<Symbol> = None;

    let sym_foreigner_type = Symbol::intern("foreigner_type");
    let sym_rust_type = Symbol::intern("rust_type");
    let sym_rust_type_not_unique = Symbol::intern("rust_type_not_unique");

    let mut names_map = HashMap::<Symbol, (Symbol, Span)>::new();

    for a in &item.attrs {
        let attr_value: Symbol = match a.value.node {
            ast::MetaItemKind::NameValue(codemap::Spanned {
                                             node: ast::LitKind::Str(lit, _), ..
                                         }) => lit,
            _ => fatal_error(sess, &a.span, "Can not get attribute value"),
        };

        match a.value.name {
            x if x == sym_foreigner_type => {
                if ftype.is_some() {
                    fatal_error(sess, &a.span, "Two foreigner_type without rust_type");
                }
                if let Some(val) = names_map.get(&attr_value) {
                    fatal_error(sess,
                                &a.span,
                                &format!("Such type({}) already defined here {:?}",
                                         attr_value.as_str(),
                                         val.1));
                }
                ftype = Some(attr_value);
            }
            x if x == sym_rust_type => {
                if let Some(ftype) = ftype.take() {
                    names_map.insert(ftype, (attr_value, a.span));
                } else {
                    fatal_error(sess, &a.span, "No foreigner_type for rust_type");
                }
            }
            x if x == sym_rust_type_not_unique => {
                if let Some(ftype) = ftype.take() {
                    let unique_name = make_unique_rust_typename(attr_value, ftype);
                    names_map.insert(ftype, (unique_name, a.span));
                } else {
                    fatal_error(sess, &a.span, "No foreigner_type for rust_type");
                }
            }
            _ => {
                fatal_error(sess,
                            &a.span,
                            &format!("{}:{} Unknown name of attribute: '{}'",
                                     file!(),
                                     line!(),
                                     a.value.name));
            }
        }

    }


    names_map
        .iter()
        .map(|(k, v)| {
                 NamesPair {
                     foreign_name: *k,
                     rust_name: v.0,
                 }
             })
        .collect()
}

fn is_foreign_types_map_module(item: &ast::Item) -> bool {
    let sym_foreign_types_map = Symbol::intern("foreign_types_map");
    if let ast::ItemKind::Mod(_) = item.node {
        item.ident.name == sym_foreign_types_map
    } else {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_foreign_types_names_map_parsing() {
        let parse_sess = ParseSess::new();
        let types_map = parse_types_map(
            &parse_sess,
            "test",
            r#"
mod foreign_types_map {
    #![foreigner_type="boolean"]
    #![rust_type="jboolean"]
    #![foreigner_type="short"]
    #![rust_type="jshort"]
    #![foreigner_type="int"]
    #![rust_type="jint"]
}
"#,
        );
        assert_eq!(vec![(Symbol::intern("boolean"), Symbol::intern("jboolean")),
                        (Symbol::intern("int"), Symbol::intern("jint")),
                        (Symbol::intern("short"), Symbol::intern("jshort"))],
                   {
                       let mut ret = vec![];
                       for (fsym, idx) in &types_map.foreign_names_map {
                           let rust_sym = types_map.conv_graph[*idx].frontier_ref_unwrap().rust;
                           ret.push((*fsym, rust_sym));
                       }
                       ret.sort_by(|a, b| a.0.as_str().cmp(&b.0.as_str()));
                       ret
                   });
    }

    #[test]
    fn test_foreign_type_map_parsing() {
        let parse_sess = ParseSess::new();
        let types_map = parse_types_map(
            &parse_sess,
            "test",
            r#"
mod foreign_types_map {
    #![foreigner_type="boolean"]
    #![rust_type="jboolean"]
    #![foreigner_type="int"]
    #![rust_type="jint"]
}

impl SwigInto<bool> for jboolean {
    fn swig_into(self, _: *mut JNIEnv) -> bool {
        self != 0
    }
}

impl SwigFrom<bool> for jboolean {
    fn swig_from(x: bool, _: *mut JNIEnv) -> Self {
        if x { 1 as jboolean } else { 0 as jboolean }
    }
}

impl SwigFrom<i32> for jint {
    fn swig_from(x: i32, _: *mut JNIEnv) -> Self {
        x
    }
}
"#,
        );
        assert_eq!(types_map
                       .to_foreign_type_name(Symbol::intern("bool"))
                       .unwrap()
                       .0,
                   Symbol::intern("boolean"));

        assert_eq!(types_map
                       .to_foreign_type_name(Symbol::intern("i32"))
                       .unwrap()
                       .0,
                   Symbol::intern("int"));
    }
}
