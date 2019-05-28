use crate::typemap::ty::ForeignConversationRule;
use std::{mem, rc::Rc};

use log::{debug, info};
use petgraph::graph::NodeIndex;
use rustc_hash::FxHashMap;
use syn::spanned::Spanned;

use crate::{
    error::{DiagnosticError, Result},
    source_registry::SourceId,
    typemap::{
        ast::TypeName,
        parse_typemap_macro::{FTypeLeftRightPair, TypeMapConvRuleInfo},
        ty::{ForeignConversationIntermediate, ForeignTypeS, ForeignTypesStorage},
        TypeConvEdge, TypeMap,
    },
};

impl TypeMap {
    pub(crate) fn merge(
        &mut self,
        id_of_code: SourceId,
        code: &str,
        target_pointer_width: usize,
    ) -> Result<()> {
        debug!("TypeMap::merge {:?} with our rules", id_of_code);
        self.rust_to_foreign_cache.clear();
        let mut was_traits_usage_code = FxHashMap::default();
        mem::swap(&mut was_traits_usage_code, &mut self.traits_usage_code);
        let mut new_data = crate::typemap::parse::parse(
            id_of_code,
            code,
            target_pointer_width,
            was_traits_usage_code,
        )?;
        mem::swap(&mut new_data.traits_usage_code, &mut self.traits_usage_code);

        let mut new_node_to_our_map = FxHashMap::<NodeIndex, NodeIndex>::default();
        add_new_nodes(&new_data, self, &mut new_node_to_our_map);
        add_new_edges(&new_data, self, &new_node_to_our_map);
        let TypeMap {
            ftypes_storage: new_ftypes_storage,
            generic_edges: mut new_generic_edges,
            utils_code: mut new_utils_code,
            ..
        } = new_data;
        add_new_ftypes(new_ftypes_storage, self, &new_node_to_our_map);

        self.utils_code.append(&mut new_utils_code);
        //TODO: more intellect to process new generics
        self.generic_edges.append(&mut new_generic_edges);
        Ok(())
    }

    pub(in crate::typemap) fn merge_conv_rule(
        &mut self,
        src_id: SourceId,
        ri: TypeMapConvRuleInfo,
    ) -> Result<()> {
        if let Some((r_ty, f_ty)) = ri.if_simple_rtype_ftype_map() {
            let r_ty = self.find_or_alloc_rust_type(r_ty, src_id).graph_idx;

            self.add_foreign_rust_ty_idx(
                TypeName::new(f_ty.name.clone(), (src_id, f_ty.sp)),
                r_ty,
            )?;
            return Ok(());
        }

        let mut rtype_left_to_right = None;
        if let Some(rule) = ri.rtype_left_to_right {
            let (right_ty, code) = if let (Some(right_ty), Some(code)) = (rule.right_ty, rule.code)
            {
                (right_ty, code)
            } else {
                return Err(DiagnosticError::new2(
                    (src_id, rule.left_ty.span()),
                    "rule (r_type 'from type' => 'to type') is not simple, but no code or 'to type'",
                ));
            };

            let from_ty = self
                .find_or_alloc_rust_type(&rule.left_ty, src_id)
                .graph_idx;
            let to_ty = self.find_or_alloc_rust_type(&right_ty, src_id).graph_idx;
            self.conv_graph
                .update_edge(from_ty, to_ty, TypeConvEdge::new(code.into(), None));
            rtype_left_to_right = Some((from_ty, to_ty));
        }

        let mut rtype_right_to_left = None;
        if let Some(rule) = ri.rtype_right_to_left {
            let (right_ty, code) = if let (Some(right_ty), Some(code)) = (rule.right_ty, rule.code)
            {
                (right_ty, code)
            } else {
                return Err(DiagnosticError::new(
                    src_id, rule.left_ty.span(),
                     "rule (r_type 'to type' <= 'from type') is not simple, but no code or 'from type'",
                ));
            };

            let to_ty = self
                .find_or_alloc_rust_type(&rule.left_ty, src_id)
                .graph_idx;
            let from_ty = self.find_or_alloc_rust_type(&right_ty, src_id).graph_idx;
            self.conv_graph
                .update_edge(from_ty, to_ty, TypeConvEdge::new(code.into(), None));
            rtype_right_to_left = Some((from_ty, to_ty));
        }

        let mut ft_into_from_rust = None;
        if let Some(rule) = ri.ftype_left_to_right {
            let right_fty = match rule.left_right_ty {
                FTypeLeftRightPair::OnlyLeft(left_ty) => {
                    return Err(DiagnosticError::new(
                        src_id,
                        left_ty.sp,
                        "rule (f_type 'from type' => 'to type') is not simple, but no 'to type'",
                    ));
                }
                FTypeLeftRightPair::Both(left_ty, _right_ty) => {
                    return Err(DiagnosticError::new(
                        src_id,
                        left_ty.sp,
                        "not supported rule type: f_type 'from_type' => 'to type'",
                    ));
                }
                FTypeLeftRightPair::OnlyRight(right_ty) => right_ty,
            };
            let (rty_left, rty_right) = rtype_left_to_right.ok_or_else(|| {
                DiagnosticError::new(
                    src_id,
                    right_fty.sp,
                    "no r_type corresponding to this f_type rule",
                )
            })?;
            let conv_code = rule.code.ok_or_else(|| {
                DiagnosticError::new(src_id, right_fty.sp, "expect conversation code here")
            })?;

            ft_into_from_rust = Some((
                right_fty,
                ForeignConversationRule {
                    rust_ty: rty_left,
                    intermediate: Some(ForeignConversationIntermediate {
                        intermediate_ty: rty_right,
                        conv_code,
                    }),
                },
            ));
        }

        let mut ft_from_into_rust = None;
        if let Some(rule) = ri.ftype_right_to_left {
            let right_fty = match rule.left_right_ty {
                FTypeLeftRightPair::OnlyLeft(left_ty) => {
                    return Err(DiagnosticError::new(
                        src_id,
                        left_ty.sp,
                        "rule (f_type 'to type' <= 'from type') is not simple, but no 'from type'",
                    ));
                }
                FTypeLeftRightPair::Both(left_ty, _right_ty) => {
                    return Err(DiagnosticError::new(
                        src_id,
                        left_ty.sp,
                        "not supported rule type: f_type 'to type' <= 'from type'",
                    ));
                }
                FTypeLeftRightPair::OnlyRight(right_ty) => right_ty,
            };
            let (rty_right, rty_left) = rtype_right_to_left.ok_or_else(|| {
                DiagnosticError::new(
                    src_id,
                    right_fty.sp,
                    "no r_type corresponding to this f_type rule",
                )
            })?;
            let conv_code = rule.code.ok_or_else(|| {
                DiagnosticError::new(src_id, right_fty.sp, "expect conversation code here")
            })?;

            ft_from_into_rust = Some((
                right_fty,
                ForeignConversationRule {
                    rust_ty: rty_left,
                    intermediate: Some(ForeignConversationIntermediate {
                        intermediate_ty: rty_right,
                        conv_code,
                    }),
                },
            ));
        }

        match (ft_into_from_rust, ft_from_into_rust) {
            (Some((ft1, into_from_rust)), Some((ft2, from_into_rust))) => {
                if ft1 != ft2 {
                    return Err(DiagnosticError::new(
                        src_id,
                        ft1.sp,
                        format!("types name mismatch, one type is {}", ft1.name),
                    )
                    .add_span_note((src_id, ft2.sp), format!("another type is {}", ft2.name)));
                }
                let name = TypeName::new(ft1.name, (src_id, ft1.sp));
                let ftype_idx = self.ftypes_storage.find_or_alloc(name);
                self.ftypes_storage[ftype_idx].into_from_rust = Some(into_from_rust);
                self.ftypes_storage[ftype_idx].from_into_rust = Some(from_into_rust);
            }
            (Some((ft, into_from_rust)), None) => {
                let name = TypeName::new(ft.name, (src_id, ft.sp));
                let ftype_idx = self.ftypes_storage.find_or_alloc(name);
                self.ftypes_storage[ftype_idx].into_from_rust = Some(into_from_rust);
            }
            (None, Some((ft, from_into_rust))) => {
                let name = TypeName::new(ft.name, (src_id, ft.sp));
                let ftype_idx = self.ftypes_storage.find_or_alloc(name);
                self.ftypes_storage[ftype_idx].from_into_rust = Some(from_into_rust);
            }
            (None, None) => {}
        }

        Ok(())
    }
}

fn add_new_nodes(
    new_data: &TypeMap,
    data: &mut TypeMap,
    new_node_to_our_map: &mut FxHashMap<NodeIndex, NodeIndex>,
) {
    for new_node_idx in new_data.conv_graph.node_indices() {
        let new_node = &new_data.conv_graph[new_node_idx];
        let data_rust_names_map = &mut data.rust_names_map;
        let data_conv_graph = &mut data.conv_graph;
        let data_idx = *data_rust_names_map
            .entry(new_node.normalized_name.clone())
            .or_insert_with(|| {
                let idx = data_conv_graph.add_node((*new_node).clone());
                Rc::make_mut(&mut data_conv_graph[idx]).graph_idx = idx;
                idx
            });
        Rc::make_mut(&mut data_conv_graph[data_idx]).merge(&new_data.conv_graph[new_node_idx]);
        new_node_to_our_map.insert(new_node_idx, data_idx);
    }
}

fn add_new_edges(
    new_data: &TypeMap,
    data: &mut TypeMap,
    new_node_to_our_map: &FxHashMap<NodeIndex, NodeIndex>,
) {
    for (new_node_idx, our_idx) in new_node_to_our_map {
        let mut new_edges = new_data
            .conv_graph
            .neighbors_directed(*new_node_idx, petgraph::Outgoing)
            .detach();
        while let Some((new_edge, new_target)) = new_edges.next(&new_data.conv_graph) {
            let our_target = *new_node_to_our_map
                .get(&new_target)
                .expect("At this step we should have full map new -> our");
            if let Some(existing_edge) = data.conv_graph.find_edge(*our_idx, our_target) {
                info!(
                    "typemap merge: replace {:?} with new conversation rule {:?}, for {} -> {}",
                    data.conv_graph[existing_edge],
                    new_data.conv_graph[new_edge],
                    data.conv_graph[*our_idx],
                    data.conv_graph[our_target],
                );
            }
            data.conv_graph.update_edge(
                *our_idx,
                our_target,
                new_data.conv_graph[new_edge].clone(),
            );
        }
    }
}

fn add_new_ftypes(
    new_ftypes_storage: ForeignTypesStorage,
    data: &mut TypeMap,
    new_node_to_our_map: &FxHashMap<NodeIndex, NodeIndex>,
) {
    for mut new_ftype in new_ftypes_storage.into_iter() {
        ftype_map_rust_types(&mut new_ftype, new_node_to_our_map);
        match data
            .ftypes_storage
            .find_ftype_by_name(new_ftype.name.as_str())
        {
            Some(ftype_idx) => {
                ftype_merge(&mut data.ftypes_storage[ftype_idx], new_ftype);
            }
            None => {
                data.ftypes_storage.add_new_ftype(new_ftype);
            }
        }
    }
}

fn ftype_map_rust_types(
    extrn_ft: &mut ForeignTypeS,
    new_node_to_our_map: &FxHashMap<NodeIndex, NodeIndex>,
) {
    if let Some(rule) = extrn_ft.into_from_rust.as_mut() {
        ftype_rule_map_rust_type(rule, new_node_to_our_map);
    }
    if let Some(rule) = extrn_ft.from_into_rust.as_mut() {
        ftype_rule_map_rust_type(rule, new_node_to_our_map);
    }
}

fn ftype_rule_map_rust_type(
    rule: &mut ForeignConversationRule,
    new_node_to_our_map: &FxHashMap<NodeIndex, NodeIndex>,
) {
    rule.rust_ty = *new_node_to_our_map
        .get(&rule.rust_ty)
        .expect("Internal Error: no full types map");
    if let Some(intr_ty) = rule.intermediate.as_mut() {
        intr_ty.intermediate_ty = *new_node_to_our_map
            .get(&intr_ty.intermediate_ty)
            .expect("Internal Error: no full types map");
    }
}

fn ftype_merge(our: &mut ForeignTypeS, extrn_ft: ForeignTypeS) {
    if let Some(rule) = extrn_ft.into_from_rust {
        our.into_from_rust = Some(rule);
    }
    if let Some(rule) = extrn_ft.from_into_rust {
        our.from_into_rust = Some(rule);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{error::invalid_src_id_span, typemap::find_conversation_path};
    use rustc_hash::FxHashSet;
    use syn::{parse_quote, Type};

    #[test]
    fn test_merge() {
        let mut types_map = TypeMap::default();
        types_map
            .merge(
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
            )
            .unwrap();
        types_map
            .merge(
                SourceId::none(),
                r#"
mod swig_foreign_types_map {
    #![swig_foreigner_type="boolean"]
    #![swig_rust_type="jboolean"]
    #![swig_foreigner_type="int"]
    #![swig_rust_type="jint"]
}

fn helper1() {
}

#[swig_code = "let mut {to_var}: {to_var_type} = {from_var}.swig_into(env);"]
trait SwigInto<T> {
    fn swig_into(self, env: *mut JNIEnv) -> T;
}

#[swig_code = "let mut {to_var}: {to_var_type} = <{to_var_type}>::swig_from({from_var}, env);"]
trait SwigFrom<T> {
    fn swig_from(T, env: *mut JNIEnv) -> Self;
}

impl SwigInto<bool> for jboolean {
    fn swig_into(self, _: *mut JNIEnv) -> bool {
        self != 0
    }
}

fn helper2() {
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

fn helper3() {
}
"#,
                64,
            )
            .unwrap();
        assert_eq!(
            {
                let mut set = FxHashSet::default();
                for ft in types_map.ftypes_storage.iter() {
                    set.insert(ft.name.typename.clone());
                }
                set
            },
            {
                let mut set = FxHashSet::default();
                set.insert("boolean".into());
                set.insert("int".into());
                set
            }
        );
        let ty_i32 = types_map.find_or_alloc_rust_type(&parse_type! { i32 }, SourceId::none());
        let fti = types_map
            .map_through_conversation_to_foreign(
                &ty_i32,
                petgraph::Direction::Outgoing,
                invalid_src_id_span(),
                |_, fc| fc.constructor_ret_type.clone(),
            )
            .unwrap();
        assert_eq!("int", types_map[fti].name.as_str(),);
        assert_eq!(
            "let mut {to_var}: {to_var_type} = {from_var}.swig_into(env);",
            {
                let from = types_map.rust_names_map["jboolean"];
                let to = types_map.rust_names_map["bool"];
                let conv = &types_map.conv_graph[types_map.conv_graph.find_edge(from, to).unwrap()];
                conv.code_template.clone()
            },
        );

        let from = types_map.rust_names_map["jboolean"];
        let to = types_map.rust_names_map["bool"];
        assert_eq!(
            find_conversation_path(&types_map.conv_graph, from, to, invalid_src_id_span()).unwrap(),
            vec![types_map.conv_graph.find_edge(from, to).unwrap()]
        );

        let from = types_map.rust_names_map["bool"];
        let to = types_map.rust_names_map["jboolean"];
        assert_eq!(
            find_conversation_path(&types_map.conv_graph, from, to, invalid_src_id_span()).unwrap(),
            vec![types_map.conv_graph.find_edge(from, to).unwrap()]
        );
        assert_eq!(
            types_map
                .utils_code
                .iter()
                .filter_map(|v| {
                    let item: syn::Item = v.clone();
                    match item {
                        syn::Item::Fn(ref fun) => Some(fun.ident.to_string()),
                        syn::Item::Trait(ref trait_) => Some(trait_.ident.to_string()),
                        _ => None,
                    }
                })
                .collect::<Vec<_>>(),
            vec!["helper1", "SwigInto", "SwigFrom", "helper2", "helper3"]
        );
    }
}
