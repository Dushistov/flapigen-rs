use crate::typemap::ty::ForeignConversionRule;
use std::{convert::TryInto, mem, rc::Rc};

use log::{debug, error, info};
use petgraph::graph::NodeIndex;
use rustc_hash::FxHashMap;
use smol_str::SmolStr;
use syn::spanned::Spanned;

use crate::{
    error::{DiagnosticError, Result},
    source_registry::SourceId,
    typemap::{
        ast::{ForeignTypeName, SpannedSmolStr},
        ty::{ForeignConversionIntermediate, ForeignTypeS, ForeignTypesStorage},
        typemap_macro::{FTypeLeftRightPair, ModuleName, TypeMapConvRuleInfo},
        TypeConvEdge, TypeMap,
    },
};

use super::TypesConvGraph;

impl TypeMap {
    pub(crate) fn merge(
        &mut self,
        id_of_code: SourceId,
        code: &str,
        target_pointer_width: usize,
    ) -> Result<()> {
        debug!("TypeMap::merge {:?} with our rules", id_of_code);
        self.invalidate_conv_cache();
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
            not_merged_data: mut new_not_merged_data,
            generic_rules: mut new_generic_rules,
            ..
        } = new_data;
        add_new_ftypes(new_ftypes_storage, self, &new_node_to_our_map)?;

        self.utils_code.append(&mut new_utils_code);
        //TODO: more intellect to process new generics
        self.generic_edges.append(&mut new_generic_edges);
        //TODO: add more checks
        self.not_merged_data.append(&mut new_not_merged_data);
        self.generic_rules.append(&mut new_generic_rules);
        Ok(())
    }

    pub(crate) fn may_be_merge_conv_rule(
        &mut self,
        src_id: SourceId,
        mut ri: TypeMapConvRuleInfo,
    ) -> Result<()> {
        debug!("may_be_merge_conv_rule: {:?}", ri);
        ri.set_src_id(src_id);
        if ri.is_generic() {
            match ri.try_into() {
                Ok(edge) => self.generic_edges.push(edge),
                Err(err) => self.generic_rules.push(Rc::new(err)),
            }
            return Ok(());
        }
        if ri.contains_data_for_language_backend() {
            self.not_merged_data.push(ri);
            return Ok(());
        }
        self.merge_conv_rule(src_id, ri)
    }

    pub(crate) fn merge_conv_rule(
        &mut self,
        src_id: SourceId,
        mut ri: TypeMapConvRuleInfo,
    ) -> Result<()> {
        debug!("merge_conv_rule: ri {:?}", ri);
        assert!(!ri.contains_data_for_language_backend());
        assert!(!ri.is_generic());
        if let Some((r_ty, f_ty, req_modules, unique_prefix)) =
            ri.if_simple_rtype_ftype_map_no_lang_backend()
        {
            let r_ty = self.find_or_alloc_rust_type(r_ty, src_id).graph_idx;
            self.invalidate_conv_for_rust_type(r_ty);
            let ftype_idx = self.add_foreign_rust_ty_idx(
                ForeignTypeName::new(f_ty.name.clone(), (src_id, f_ty.sp)),
                r_ty,
            )?;
            let ftype = &mut self.ftypes_storage[ftype_idx];
            ftype.provided_by_module =
                convert_req_module_to_provided_by_module(req_modules.to_vec());
            set_unique_prefix(ftype, unique_prefix.cloned(), src_id)?;
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
                .update_edge(from_ty, to_ty, TypeConvEdge::new(code, None));
            rtype_left_to_right = Some((from_ty, to_ty));
            self.invalidate_conv_for_rust_type(from_ty);
            self.invalidate_conv_for_rust_type(to_ty);
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
                .update_edge(from_ty, to_ty, TypeConvEdge::new(code, None));
            rtype_right_to_left = Some((from_ty, to_ty));
            self.invalidate_conv_for_rust_type(from_ty);
            self.invalidate_conv_for_rust_type(to_ty);
        }

        let mut req_modules = vec![];

        let mut ft_into_from_rust = None;
        let mut ft_unique_prefix = None;
        assert!(ri.ftype_left_to_right.len() <= 1);
        if !ri.ftype_left_to_right.is_empty() {
            let mut rule = ri.ftype_left_to_right.remove(0);
            req_modules.append(&mut rule.req_modules);
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

            self.invalidate_conv_for_rust_type(rty_right);
            self.invalidate_conv_for_rust_type(rty_left);
            ft_unique_prefix = rule.unique_prefix;
            match rule.code {
                Some(conv_code) => {
                    ft_into_from_rust = Some((
                        right_fty,
                        ForeignConversionRule {
                            rust_ty: rty_left,
                            intermediate: Some(ForeignConversionIntermediate {
                                input_to_output: rule.input_to_output,
                                intermediate_ty: rty_right,
                                conv_code: Rc::new(conv_code),
                            }),
                        },
                    ));
                }
                None => {
                    ft_into_from_rust = Some((
                        right_fty,
                        ForeignConversionRule {
                            rust_ty: rty_right,
                            intermediate: None,
                        },
                    ));
                }
            }
        }

        let mut ft_from_into_rust = None;
        assert!(ri.ftype_right_to_left.len() <= 1);
        if !ri.ftype_right_to_left.is_empty() {
            let mut rule = ri.ftype_right_to_left.remove(0);
            req_modules.append(&mut rule.req_modules);
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
            if let Some(ref ft_unique_prefix) = ft_unique_prefix {
                if let Some(rule_prefix) = rule.unique_prefix {
                    if rule_prefix.as_str() != ft_unique_prefix.as_str() {
                        return Err(DiagnosticError::new(
                            src_id,
                            rule_prefix.sp,
                            format!(
                                "unique_prefix mismatch '{}' vs '{}'",
                                rule_prefix.as_str(),
                                ft_unique_prefix.as_str()
                            ),
                        )
                        .add_span_note((src_id, ft_unique_prefix.sp), "previous unique_prefix"));
                    }
                }
            } else {
                ft_unique_prefix = rule.unique_prefix;
            }

            self.invalidate_conv_for_rust_type(rty_right);
            self.invalidate_conv_for_rust_type(rty_left);
            match rule.code {
                Some(conv_code) => {
                    ft_from_into_rust = Some((
                        right_fty,
                        ForeignConversionRule {
                            rust_ty: rty_left,
                            intermediate: Some(ForeignConversionIntermediate {
                                input_to_output: rule.input_to_output,
                                intermediate_ty: rty_right,
                                conv_code: Rc::new(conv_code),
                            }),
                        },
                    ));
                }
                None => {
                    ft_from_into_rust = Some((
                        right_fty,
                        ForeignConversionRule {
                            rust_ty: rty_right,
                            intermediate: None,
                        },
                    ));
                }
            }
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
                let name = ForeignTypeName::new(ft1.name, (src_id, ft1.sp));
                let ftype_idx = self.ftypes_storage.find_or_alloc(name);
                let res_ftype = &mut self.ftypes_storage[ftype_idx];
                validate_rule_rewrite(
                    res_ftype.into_from_rust.as_ref(),
                    &into_from_rust,
                    &self.conv_graph,
                )?;
                res_ftype.into_from_rust = Some(into_from_rust);
                validate_rule_rewrite(
                    res_ftype.from_into_rust.as_ref(),
                    &from_into_rust,
                    &self.conv_graph,
                )?;
                res_ftype.from_into_rust = Some(from_into_rust);
                res_ftype.provided_by_module =
                    convert_req_module_to_provided_by_module(req_modules);
                set_unique_prefix(res_ftype, ft_unique_prefix, src_id)?;
            }
            (Some((ft, into_from_rust)), None) => {
                let name = ForeignTypeName::new(ft.name, (src_id, ft.sp));
                let ftype_idx = self.ftypes_storage.find_or_alloc(name);
                let res_ftype = &mut self.ftypes_storage[ftype_idx];
                validate_rule_rewrite(
                    res_ftype.into_from_rust.as_ref(),
                    &into_from_rust,
                    &self.conv_graph,
                )?;
                res_ftype.into_from_rust = Some(into_from_rust);
                res_ftype.provided_by_module =
                    convert_req_module_to_provided_by_module(req_modules);
                set_unique_prefix(res_ftype, ft_unique_prefix, src_id)?;
            }
            (None, Some((ft, from_into_rust))) => {
                let name = ForeignTypeName::new(ft.name, (src_id, ft.sp));
                let ftype_idx = self.ftypes_storage.find_or_alloc(name);
                let res_ftype = &mut self.ftypes_storage[ftype_idx];
                validate_rule_rewrite(
                    res_ftype.from_into_rust.as_ref(),
                    &from_into_rust,
                    &self.conv_graph,
                )?;
                res_ftype.from_into_rust = Some(from_into_rust);
                res_ftype.provided_by_module =
                    convert_req_module_to_provided_by_module(req_modules);
                set_unique_prefix(res_ftype, ft_unique_prefix, src_id)?;
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
                    "typemap merge: replace {:?} with new conversion rule {:?}, for {} -> {}",
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
) -> Result<()> {
    for mut new_ftype in new_ftypes_storage.into_iter() {
        ftype_map_rust_types(&mut new_ftype, new_node_to_our_map);
        match data
            .ftypes_storage
            .find_ftype_by_name(new_ftype.name.value())
        {
            Some(ftype_idx) => {
                ftype_merge(&mut data.ftypes_storage[ftype_idx], new_ftype);
            }
            None => {
                data.ftypes_storage.add_new_ftype(new_ftype)?;
            }
        }
    }
    Ok(())
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
    rule: &mut ForeignConversionRule,
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

fn convert_req_module_to_provided_by_module(v: Vec<ModuleName>) -> Vec<SmolStr> {
    let mut ret = Vec::with_capacity(v.len());
    for x in v {
        ret.push(x.name);
    }
    ret
}

fn validate_rule_rewrite(
    prev: Option<&ForeignConversionRule>,
    new: &ForeignConversionRule,
    diagnostic_map: &TypesConvGraph,
) -> Result<()> {
    fn types_from_rule_to_string(
        diagnostic_map: &TypesConvGraph,
        rule: &ForeignConversionRule,
    ) -> String {
        format!(
            "main rust type {}, intermediate {}",
            diagnostic_map[rule.rust_ty],
            rule.intermediate
                .as_ref()
                .map(|x| diagnostic_map[x.intermediate_ty].normalized_name.as_str())
                .unwrap_or("")
        )
    }

    if let Some(prev) = prev {
        if let (Some(prev_rule), Some(new_rule)) =
            (prev.intermediate.as_ref(), new.intermediate.as_ref())
        {
            if !prev_rule.conv_code.src_id().is_none()
                && prev_rule.conv_code.src_id() == new_rule.conv_code.src_id()
            {
                error!("prev_rule {:?}, new_rule {:?}", prev_rule, new_rule);
                return Err(DiagnosticError::new(
                    new_rule.conv_code.src_id(),
                    new_rule.conv_code.span(),
                    format!(
                        "new rule f_type ({}) here",
                        types_from_rule_to_string(diagnostic_map, new)
                    ),
                )
                .add_span_note(
                    (prev_rule.conv_code.src_id(), prev_rule.conv_code.span()),
                    format!(
                        "overwrite f_type ({}) defined in the same file",
                        types_from_rule_to_string(diagnostic_map, prev)
                    ),
                ));
            }
        }
    }

    Ok(())
}

fn set_unique_prefix(
    ft: &mut ForeignTypeS,
    unique_prefix: Option<SpannedSmolStr>,
    src_id: SourceId,
) -> Result<()> {
    let different = match (&unique_prefix, ft.name.unique_prefix()) {
        (Some(x), Some(y)) => x.as_str() != y,
        (None, None) => false,
        (Some(_), None) | (None, Some(_)) => true,
    };
    if let Some(name_prefix) = ft.name.unique_prefix() {
        if different && !ft.name.span.0.is_none() && ft.name.span.0 == src_id {
            return Err(DiagnosticError::new2(
                ft.name.span,
                format!(
                    "you change unique_prefix in the same file, was '{}', new '{:?}'",
                    name_prefix, unique_prefix
                ),
            ));
        }
    }
    if let Some(unique_prefix) = unique_prefix.as_ref() {
        if !ft.name.value().starts_with(unique_prefix.as_str()) {
            //just ignore prefix
            return Ok(());
        }
    }
    ft.name
        .set_name_prefix(unique_prefix.as_ref().map(|x| x.value.as_str()));
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        error::invalid_src_id_span,
        typemap::{find_conversion_path, MapToForeignFlag},
    };
    use rustc_hash::FxHashSet;

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
    fn swig_from(_: T, env: *mut JNIEnv) -> Self;
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
            .map_through_conversion_to_foreign(
                ty_i32.to_idx(),
                petgraph::Direction::Outgoing,
                MapToForeignFlag::FullSearch,
                invalid_src_id_span(),
                |_, fc| {
                    fc.self_desc
                        .as_ref()
                        .map(|x| x.constructor_ret_type.clone())
                },
            )
            .unwrap();
        assert_eq!("int", types_map[fti].name.display());
        assert_eq!(
            "let mut {to_var}: {to_var_type} = {from_var}.swig_into(env);",
            {
                let from = types_map.rust_names_map["jboolean"];
                let to = types_map.rust_names_map["bool"];
                let conv = &types_map.conv_graph[types_map.conv_graph.find_edge(from, to).unwrap()];
                conv.code.to_string()
            },
        );

        let from = types_map.rust_names_map["jboolean"];
        let to = types_map.rust_names_map["bool"];
        assert_eq!(
            find_conversion_path(&types_map.conv_graph, from, to, invalid_src_id_span()).unwrap(),
            vec![types_map.conv_graph.find_edge(from, to).unwrap()]
        );

        let from = types_map.rust_names_map["bool"];
        let to = types_map.rust_names_map["jboolean"];
        assert_eq!(
            find_conversion_path(&types_map.conv_graph, from, to, invalid_src_id_span()).unwrap(),
            vec![types_map.conv_graph.find_edge(from, to).unwrap()]
        );
        assert_eq!(
            types_map
                .utils_code
                .iter()
                .filter_map(|v| {
                    let item: syn::Item = v.clone();
                    match item {
                        syn::Item::Fn(ref fun) => Some(fun.sig.ident.to_string()),
                        syn::Item::Trait(ref trait_) => Some(trait_.ident.to_string()),
                        _ => None,
                    }
                })
                .collect::<Vec<_>>(),
            vec!["helper1", "SwigInto", "SwigFrom", "helper2", "helper3"]
        );
    }
}
