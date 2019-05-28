pub mod ast;
mod merge;
mod parse;
mod parse_typemap_macro;
pub mod ty;
pub mod utils;

use std::{cell::RefCell, fmt, mem, ops, rc::Rc};

use log::{debug, log_enabled, trace, warn};
use petgraph::{
    graph::{EdgeIndex, NodeIndex},
    Graph,
};
use proc_macro2::TokenStream;
use rustc_hash::{FxHashMap, FxHashSet};
use smallvec::SmallVec;
use smol_str::SmolStr;
use syn::{parse_quote, Ident, Type};

use crate::{
    error::{invalid_src_id_span, DiagnosticError, Result, SourceIdSpan},
    source_registry::SourceId,
    typemap::{
        ast::{
            check_if_smart_pointer_return_inner_type, get_trait_bounds, normalize_ty_lifetimes,
            DisplayToTokens, GenericTypeConv, TypeName,
        },
        ty::{
            ForeignConversationRule, ForeignType, ForeignTypeS, ForeignTypesStorage, RustType,
            RustTypeS,
        },
    },
    types::{ForeignEnumInfo, ForeignerClassInfo},
};

pub(crate) static TO_VAR_TEMPLATE: &str = "{to_var}";
pub(crate) static FROM_VAR_TEMPLATE: &str = "{from_var}";
pub(in crate::typemap) static TO_VAR_TYPE_TEMPLATE: &str = "{to_var_type}";
pub(in crate::typemap) static FUNCTION_RETURN_TYPE_TEMPLATE: &str = "{function_ret_type}";
const MAX_TRY_BUILD_PATH_STEPS: usize = 7;

#[derive(Debug, Clone)]
pub(crate) struct TypeConvEdge {
    code_template: String,
    dependency: Rc<RefCell<Option<TokenStream>>>,
}

impl From<String> for TypeConvEdge {
    fn from(x: String) -> Self {
        TypeConvEdge {
            code_template: x,
            dependency: Rc::new(RefCell::new(None)),
        }
    }
}

impl TypeConvEdge {
    fn new(code_template: String, dependency: Option<TokenStream>) -> TypeConvEdge {
        TypeConvEdge {
            code_template,
            dependency: Rc::new(RefCell::new(dependency)),
        }
    }
}

pub(crate) type TypeGraphIdx = u32;
pub(crate) type TypesConvGraph = Graph<RustType, TypeConvEdge, petgraph::Directed, TypeGraphIdx>;

pub(crate) trait ForeignMethodSignature {
    type FI: AsRef<ForeignTypeInfo>;
    fn output(&self) -> &ForeignTypeInfo;
    fn input(&self) -> &[Self::FI];
}

pub(crate) type RustTypeIdx = NodeIndex<TypeGraphIdx>;

type RustTypeNameToGraphIdx = FxHashMap<SmolStr, RustTypeIdx>;

#[derive(Debug)]
pub(crate) struct TypeMap {
    conv_graph: TypesConvGraph,
    ftypes_storage: ForeignTypesStorage,
    rust_to_foreign_cache: FxHashMap<SmolStr, ForeignType>,
    rust_names_map: RustTypeNameToGraphIdx,
    utils_code: Vec<syn::Item>,
    generic_edges: Vec<GenericTypeConv>,
    foreign_classes: Vec<ForeignerClassInfo>,
    exported_enums: FxHashMap<SmolStr, ForeignEnumInfo>,
    /// How to use trait to convert types, Trait Name -> Code
    traits_usage_code: FxHashMap<Ident, String>,
}

impl Default for TypeMap {
    fn default() -> Self {
        let generic_params: syn::Generics = parse_quote! { <T> };
        let default_rules = vec![
            GenericTypeConv {
                code_template: "let mut {to_var}: {to_var_type} = &{from_var};".into(),
                ..GenericTypeConv::simple_new(
                    parse_type! { T },
                    parse_type! { &T },
                    generic_params.clone(),
                )
            },
            GenericTypeConv {
                code_template: "let mut {to_var}: {to_var_type} = &mut {from_var};".into(),
                ..GenericTypeConv::simple_new(
                    parse_type! { T },
                    parse_type! { &mut T },
                    generic_params.clone(),
                )
            },
            GenericTypeConv {
                code_template: "let mut {to_var}: {to_var_type} = {from_var};".into(),
                ..GenericTypeConv::simple_new(
                    parse_type! { &mut T },
                    parse_type! { &T },
                    generic_params.clone(),
                )
            },
            GenericTypeConv {
                code_template: "let mut {to_var}: {to_var_type} = {from_var}.as_ref();".into(),
                ..GenericTypeConv::simple_new(
                    parse_type! { & Box<T> },
                    parse_type! { &T },
                    generic_params.clone(),
                )
            },
            GenericTypeConv {
                code_template: "let mut {to_var}: {to_var_type} = {from_var}.as_mut();".into(),
                ..GenericTypeConv::simple_new(
                    parse_type! { & mut Box<T> },
                    parse_type! { &mut T },
                    generic_params,
                )
            },
        ];
        TypeMap {
            conv_graph: TypesConvGraph::new(),
            rust_names_map: FxHashMap::default(),
            utils_code: Vec::new(),
            generic_edges: default_rules,
            rust_to_foreign_cache: FxHashMap::default(),
            foreign_classes: Vec::new(),
            exported_enums: FxHashMap::default(),
            traits_usage_code: FxHashMap::default(),
            ftypes_storage: ForeignTypesStorage::default(),
        }
    }
}

struct DisplayTypesConvGraph<'a>(&'a TypesConvGraph);

impl<'a> fmt::Display for DisplayTypesConvGraph<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> std::result::Result<(), fmt::Error> {
        let conv_graph = self.0;
        writeln!(f, "conversation graph begin")?;
        for node in conv_graph.node_indices() {
            write!(f, "node {}: ", conv_graph[node].normalized_name)?;
            let mut edges = conv_graph
                .neighbors_directed(node, petgraph::Outgoing)
                .detach();
            while let Some((_, target)) = edges.next(conv_graph) {
                write!(f, "->{}, ", conv_graph[target].normalized_name)?;
            }
            writeln!(f)?;
        }
        writeln!(f, "conversation graph end")
    }
}

#[derive(Debug)]
struct PossiblePath {
    path_len: usize,
    new_edges: Vec<(RustType, RustType, TypeConvEdge)>,
}

impl PossiblePath {
    fn len(&self) -> usize {
        self.path_len
    }
    fn new(graph_snapshot: TypeGraphSnapshot, path: Vec<EdgeIndex<TypeGraphIdx>>) -> Self {
        let mut new_edges = Vec::with_capacity(path.len());
        for edge in &path {
            if graph_snapshot.new_edges.iter().any(|x| *x == *edge) {
                let (from, to) = graph_snapshot
                    .conv_graph
                    .edge_endpoints(*edge)
                    .expect("Internal error: PossiblePath::new no edge");
                let conv_rule = graph_snapshot
                    .conv_graph
                    .edge_weight(*edge)
                    .expect("Internal error: PossiblePath::new no edge")
                    .clone();
                new_edges.push((
                    graph_snapshot.conv_graph[from].clone(),
                    graph_snapshot.conv_graph[to].clone(),
                    conv_rule,
                ));
            }
        }
        PossiblePath {
            path_len: path.len(),
            new_edges,
        }
    }
}

#[derive(Debug)]
pub(crate) struct ForeignTypeInfo {
    pub name: SmolStr,
    pub correspoding_rust_type: RustType,
}

impl AsRef<ForeignTypeInfo> for ForeignTypeInfo {
    fn as_ref(&self) -> &ForeignTypeInfo {
        self
    }
}

struct TypeGraphSnapshot<'a> {
    conv_graph: &'a mut TypesConvGraph,
    rust_names_map: &'a RustTypeNameToGraphIdx,
    new_nodes_names_map: RustTypeNameToGraphIdx,
    new_nodes: SmallVec<[RustTypeIdx; 32]>,
    new_edges: SmallVec<[EdgeIndex<TypeGraphIdx>; 32]>,
}

impl<'a> TypeGraphSnapshot<'a> {
    fn new(conv_graph: &'a mut TypesConvGraph, rust_names_map: &'a RustTypeNameToGraphIdx) -> Self {
        TypeGraphSnapshot {
            conv_graph,
            rust_names_map,
            new_nodes: SmallVec::new(),
            new_nodes_names_map: RustTypeNameToGraphIdx::default(),
            new_edges: SmallVec::new(),
        }
    }

    fn node_for_ty(
        &mut self,
        src_id: SourceId,
        (ty, ty_name): (syn::Type, SmolStr),
    ) -> RustTypeIdx {
        let graph = &mut self.conv_graph;
        let mut new_node = false;
        let idx = if let Some(idx) = self.rust_names_map.get(&ty_name) {
            *idx
        } else {
            let names_to_graph_map = &mut self.new_nodes_names_map;
            *names_to_graph_map
                .entry(ty_name.clone())
                .or_insert_with(|| {
                    new_node = true;
                    let idx = graph.add_node(Rc::new(RustTypeS::new_without_graph_idx(
                        ty, ty_name, src_id,
                    )));
                    Rc::get_mut(&mut graph[idx])
                        .expect("Internal error: can not modify Rc")
                        .graph_idx = idx;
                    idx
                })
        };
        if new_node {
            self.new_nodes.push(idx);
        }
        idx
    }

    fn find_type_by_name(&self, type_name: &str) -> Option<&RustType> {
        self.rust_names_map
            .get(type_name)
            .map(|i| &self.conv_graph[*i])
            .or_else(|| {
                self.new_nodes_names_map
                    .get(type_name)
                    .map(|i| &self.conv_graph[*i])
            })
    }

    fn add_edge(&mut self, from: RustTypeIdx, to: RustTypeIdx, edge: TypeConvEdge) {
        if self.conv_graph.find_edge(from, to).is_none() {
            let edge_idx = self.conv_graph.add_edge(from, to, edge);
            self.new_edges.push(edge_idx);
        }
    }
}

impl<'a> Drop for TypeGraphSnapshot<'a> {
    fn drop(&mut self) {
        for edge in self.new_edges.iter().rev() {
            self.conv_graph.remove_edge(*edge);
        }
        for idx in self.new_nodes.iter().rev() {
            self.conv_graph.remove_node(*idx);
        }
    }
}

impl TypeMap {
    pub(crate) fn is_empty(&self) -> bool {
        self.conv_graph.node_count() == 0
    }

    pub(crate) fn take_utils_code(&mut self) -> Vec<syn::Item> {
        let mut ret = Vec::new();
        ret.append(&mut self.utils_code);
        ret
    }

    pub(crate) fn add_foreign(
        &mut self,
        correspoding_rty: RustType,
        foreign_name: TypeName,
    ) -> Result<()> {
        self.ftypes_storage
            .alloc_new(foreign_name, correspoding_rty.graph_idx)?;
        Ok(())
    }

    pub(crate) fn add_foreign_rust_ty_idx(
        &mut self,
        foreign_name: TypeName,
        correspoding_rty: NodeIndex,
    ) -> Result<()> {
        self.ftypes_storage
            .alloc_new(foreign_name, correspoding_rty)?;
        Ok(())
    }
    //TODO: should be removed in the future
    pub(crate) fn find_foreign_type_info_by_name(
        &self,
        foreign_name: &str,
    ) -> Option<ForeignTypeInfo> {
        if let Some(ft) = self.ftypes_storage.find_ftype_by_name(foreign_name) {
            let ftype = &self.ftypes_storage[ft];
            let ty_idx = match (ftype.into_from_rust.as_ref(), ftype.from_into_rust.as_ref()) {
                (Some(rule), _) => rule.rust_ty,
                (None, Some(rule)) => rule.rust_ty,
                (None, None) => return None,
            };
            Some(ForeignTypeInfo {
                name: ftype.name.typename.clone(),
                correspoding_rust_type: self.conv_graph[ty_idx].clone(),
            })
        } else {
            None
        }
    }

    pub(crate) fn cache_rust_to_foreign_conv(
        &mut self,
        from: &RustType,
        to: ForeignTypeInfo,
    ) -> Result<()> {
        let to_id = to.correspoding_rust_type.graph_idx;
        let ftype = self.ftypes_storage.alloc_new(
            TypeName::new(
                to.name,
                //TODO: need more right span
                invalid_src_id_span(),
            ),
            to_id,
        )?;
        self.rust_to_foreign_cache
            .insert(from.normalized_name.clone(), ftype);
        Ok(())
    }

    pub(crate) fn convert_to_heap_pointer(
        &mut self,
        from: &RustType,
        var_name: &str,
    ) -> (RustType, String) {
        for smart_pointer in &["Box", "Rc", "Arc"] {
            if let Some(inner_ty) = check_if_smart_pointer_return_inner_type(from, *smart_pointer) {
                let inner_ty: RustType = self.find_or_alloc_rust_type(&inner_ty, from.src_id);
                let code = format!(
                    r#"
    let {var_name}: *const {inner_ty} = {smart_pointer}::into_raw({var_name});
"#,
                    var_name = var_name,
                    inner_ty = inner_ty.normalized_name,
                    smart_pointer = *smart_pointer,
                );
                return (inner_ty, code);
            }
        }

        let inner_ty = from.clone();
        let inner_ty_str = normalize_ty_lifetimes(&inner_ty.ty);
        (
            inner_ty,
            format!(
                r#"
    let {var_name}: Box<{inner_ty}> = Box::new({var_name});
    let {var_name}: *mut {inner_ty} = Box::into_raw({var_name});
"#,
                var_name = var_name,
                inner_ty = inner_ty_str
            ),
        )
    }

    pub(crate) fn unpack_from_heap_pointer(
        from: &RustType,
        var_name: &str,
        unbox_if_boxed: bool,
    ) -> String {
        for smart_pointer in &["Box", "Rc", "Arc"] {
            if check_if_smart_pointer_return_inner_type(from, *smart_pointer).is_some() {
                return format!(
                    r#"
    let {var_name}: {rc_type}  = unsafe {{ {smart_pointer}::from_raw({var_name}) }};
"#,
                    var_name = var_name,
                    rc_type = from.normalized_name,
                    smart_pointer = *smart_pointer,
                );
            }
        }
        let unbox_code = if unbox_if_boxed {
            format!(
                r#"
    let {var_name}: {inside_box_type} = *{var_name};
"#,
                var_name = var_name,
                inside_box_type = from.normalized_name
            )
        } else {
            String::new()
        };
        format!(
            r#"
    let {var_name}: Box<{inside_box_type}> = unsafe {{ Box::from_raw({var_name}) }};
{unbox_code}
"#,
            var_name = var_name,
            inside_box_type = from.normalized_name,
            unbox_code = unbox_code
        )
    }

    pub(crate) fn is_ty_implements(&self, ty: &RustType, trait_name: &str) -> Option<RustType> {
        if ty.implements.contains(trait_name) {
            Some(ty.clone())
        } else {
            if let syn::Type::Reference(syn::TypeReference { ref elem, .. }) = ty.ty {
                let ty_name = normalize_ty_lifetimes(&*elem);
                self.rust_names_map.get(ty_name).and_then(|idx| {
                    if self.conv_graph[*idx].implements.contains(trait_name) {
                        Some(self.conv_graph[*idx].clone())
                    } else {
                        None
                    }
                })
            } else {
                None
            }
        }
    }

    pub(crate) fn find_foreigner_class_with_such_self_type(
        &self,
        may_be_self_ty: &RustType,
        if_ref_search_reftype: bool,
    ) -> Option<&ForeignerClassInfo> {
        let type_name = match may_be_self_ty.ty {
            syn::Type::Reference(syn::TypeReference { ref elem, .. }) if if_ref_search_reftype => {
                normalize_ty_lifetimes(&*elem)
            }
            _ => may_be_self_ty.normalized_name.as_str(),
        };

        trace!("find self type: possible name {}", type_name);
        for fc in &self.foreign_classes {
            let self_rust_ty = self
                .ty_to_rust_type_checked(&fc.self_type_as_ty())
                .unwrap_or_else(|| {
                    panic!(
                        "Internal error: self_type ({}) not registered",
                        DisplayToTokens(&fc.self_type_as_ty())
                    )
                });
            trace!("self_type {}", self_rust_ty);
            if self_rust_ty.normalized_name == type_name {
                return Some(fc);
            }
        }
        None
    }

    pub(crate) fn add_conversation_rule(
        &mut self,
        from: RustType,
        to: RustType,
        rule: TypeConvEdge,
    ) {
        debug!(
            "TypesConvMap::add_conversation_rule {} -> {}: {:?}",
            from, to, rule
        );
        let from = from.graph_idx;
        let to = to.graph_idx;
        self.conv_graph.update_edge(from, to, rule);
    }

    pub(crate) fn register_exported_enum(&mut self, enum_info: &ForeignEnumInfo) {
        self.exported_enums
            .insert(enum_info.name.to_string().into(), enum_info.clone());
    }

    pub(crate) fn is_this_exported_enum(&self, ty: &RustType) -> Option<&ForeignEnumInfo> {
        self.exported_enums.get(&ty.normalized_name)
    }

    pub(crate) fn is_generated_foreign_type(&self, foreign_name: &str) -> bool {
        if self.exported_enums.contains_key(foreign_name) {
            return true;
        }
        self.foreign_classes
            .iter()
            .any(|fc| fc.name == foreign_name)
    }

    pub(crate) fn convert_rust_types(
        &mut self,
        from: &RustType,
        to: &RustType,
        var_name: &str,
        function_ret_type: &str,
        build_for_sp: SourceIdSpan,
    ) -> Result<(Vec<TokenStream>, String)> {
        let path = match self.find_path(from, to, build_for_sp) {
            Ok(x) => x,
            Err(_err) => {
                debug!("convert_rust_types: no path, trying to build it");
                self.build_path_if_possible(from, to, build_for_sp);
                self.find_path(from, to, build_for_sp)?
            }
        };
        let mut ret_code = String::new();
        let mut code_deps = Vec::<TokenStream>::new();

        for edge in path {
            let (_, target) = self.conv_graph.edge_endpoints(edge).unwrap();
            let target_type = self.conv_graph[target].normalized_name.clone();
            let edge = &mut self.conv_graph[edge];
            if let Some(dep) = edge.dependency.borrow_mut().take() {
                code_deps.push(dep);
            }
            let code = apply_code_template(
                &edge.code_template,
                var_name,
                var_name,
                &unpack_unique_typename(&target_type),
                function_ret_type,
            );
            ret_code.push_str(&code);
        }
        Ok((code_deps, ret_code))
    }

    fn find_path(
        &self,
        from: &RustType,
        to: &RustType,
        build_for_sp: SourceIdSpan,
    ) -> Result<Vec<EdgeIndex<TypeGraphIdx>>> {
        debug!("find_path: begin {} -> {}", from, to);
        if from.normalized_name == to.normalized_name {
            return Ok(vec![]);
        }
        find_conversation_path(&self.conv_graph, from.graph_idx, to.graph_idx, build_for_sp)
    }

    fn build_path_if_possible(
        &mut self,
        start_from: &RustType,
        goal_to: &RustType,
        build_for_sp: SourceIdSpan,
    ) {
        debug!(
            "build_path_if_possible begin {}\n {} -> {}",
            DisplayTypesConvGraph(&self.conv_graph),
            start_from,
            goal_to
        );
        if let Some(path) = try_build_path(
            start_from,
            goal_to,
            build_for_sp,
            &mut self.conv_graph,
            &self.rust_names_map,
            &self.generic_edges,
            MAX_TRY_BUILD_PATH_STEPS,
        ) {
            merge_path_to_conv_map(path, self);
        }
    }

    /// find correspoint to rust foreign type (extended)
    pub(crate) fn map_through_conversation_to_foreign<
        F: Fn(&TypeMap, &ForeignerClassInfo) -> Option<Type>,
    >(
        &mut self,
        rust_ty: &RustType,
        direction: petgraph::Direction,
        build_for_sp: SourceIdSpan,
        calc_this_type_for_method: F,
    ) -> Option<ForeignType> {
        debug!("map foreign: {} {:?}", rust_ty, direction);

        if direction == petgraph::Direction::Outgoing {
            if let Some(ftype) = self.rust_to_foreign_cache.get(&rust_ty.normalized_name) {
                let fts = &self.ftypes_storage[*ftype];
                if fts.into_from_rust.is_some() {
                    return Some(*ftype);
                }
            }
        }

        {
            debug!(
                "map foreign: graph node {:?}",
                self.conv_graph[rust_ty.graph_idx]
            );
            let find_path = |from, to| match find_conversation_path(
                &self.conv_graph,
                from,
                to,
                invalid_src_id_span(),
            ) {
                Ok(x) => Some(x),
                Err(_) => None,
            };
            let mut min_path: Option<(usize, RustTypeIdx, ForeignType)> = None;
            for (ftype_idx, ftype) in self.ftypes_storage.iter_enumerate() {
                let (related_rty_idx, path) = match direction {
                    petgraph::Direction::Outgoing => {
                        if let Some(rule) = ftype.into_from_rust.as_ref() {
                            (rule.rust_ty, find_path(rust_ty.graph_idx, rule.rust_ty))
                        } else {
                            continue;
                        }
                    }
                    petgraph::Direction::Incoming => {
                        if let Some(rule) = ftype.from_into_rust.as_ref() {
                            (rule.rust_ty, find_path(rule.rust_ty, rust_ty.graph_idx))
                        } else {
                            continue;
                        }
                    }
                };
                if let Some(path) = path {
                    trace!(
                        "map foreign: we find path {} <-> {}",
                        ftype.name,
                        self.conv_graph[related_rty_idx]
                    );
                    let cur: (usize, RustTypeIdx, ForeignType) =
                        (path.len(), related_rty_idx, ftype_idx);
                    min_path = Some(if let Some(x) = min_path {
                        if cur.0 < x.0 {
                            cur
                        } else {
                            x
                        }
                    } else {
                        cur
                    });
                }
            }
            if let Some((_path_len, rust_type_idx, ftype)) = min_path {
                debug!(
                    "map foreign: we found min path {} <-> {} ({})",
                    rust_ty, self.conv_graph[rust_type_idx], self[ftype].name
                );

                return Some(ftype);
            }
        }

        debug!(
            "map foreign: No paths exists, may be we can create one for '{}' {:?}?",
            rust_ty, direction
        );

        let mut new_foreign_types = FxHashSet::default();
        for edge in &self.generic_edges {
            if let Some(ref to_foreigner_hint) = edge.to_foreigner_hint {
                let trait_bounds = get_trait_bounds(&edge.generic_params);
                for graph_idx in self.rust_names_map.values() {
                    for trait_bound in &trait_bounds {
                        let rust_ty = &self.conv_graph[*graph_idx];
                        if rust_ty.implements.contains_subset(&trait_bound.trait_names) {
                            if let Some(class) = self.find_foreigner_class_with_such_this_type(
                                &rust_ty.ty,
                                &calc_this_type_for_method,
                            ) {
                                let ty_param_name = trait_bound.ty_param.as_ref().to_string();
                                let suffix = to_foreigner_hint
                                    .as_str()
                                    .replace(&ty_param_name, &*rust_ty.normalized_name.as_str());
                                let foreign_name = to_foreigner_hint
                                    .as_str()
                                    .replace(&ty_param_name, &*class.name.to_string());
                                new_foreign_types.insert((
                                    edge.to_ty.clone(),
                                    suffix,
                                    TypeName::new(foreign_name, (class.src_id, class.name.span())),
                                ));
                            } else {
                                warn!("No foreign_class for type '{}'", rust_ty.normalized_name);
                            }
                        }
                    }
                }
            }
        }
        for (ty, suffix, foreign_name) in new_foreign_types {
            debug!(
                "map foreign: add possible type {} {} <-> {}",
                DisplayToTokens(&ty),
                suffix,
                foreign_name
            );
            let rust_ty = self.find_or_alloc_rust_type_with_suffix(&ty, &suffix, SourceId::none());
            let ftype_idx = self.ftypes_storage.find_or_alloc(foreign_name);
            match direction {
                petgraph::Direction::Outgoing => {
                    self.ftypes_storage[ftype_idx].into_from_rust = Some(ForeignConversationRule {
                        rust_ty: rust_ty.graph_idx,
                        intermediate: None,
                    });
                }
                petgraph::Direction::Incoming => {
                    self.ftypes_storage[ftype_idx].from_into_rust = Some(ForeignConversationRule {
                        rust_ty: rust_ty.graph_idx,
                        intermediate: None,
                    });
                }
            }
        }

        let from: RustType = rust_ty.clone().into();
        let mut possible_paths = Vec::<(PossiblePath, ForeignType, RustTypeIdx)>::new();
        for max_steps in 1..=MAX_TRY_BUILD_PATH_STEPS {
            for (ftype_idx, ftype) in self.ftypes_storage.iter_enumerate() {
                let (path, related_rust_ty) = match direction {
                    petgraph::Direction::Outgoing => {
                        if let Some(rule) = ftype.into_from_rust.as_ref() {
                            let related_rust_ty = rule.rust_ty;
                            let other = self.conv_graph[related_rust_ty].clone();
                            (
                                try_build_path(
                                    &from,
                                    &other,
                                    build_for_sp,
                                    &mut self.conv_graph,
                                    &self.rust_names_map,
                                    &self.generic_edges,
                                    max_steps,
                                ),
                                related_rust_ty,
                            )
                        } else {
                            continue;
                        }
                    }
                    petgraph::Direction::Incoming => {
                        if let Some(rule) = ftype.from_into_rust.as_ref() {
                            let related_rust_ty = rule.rust_ty;
                            let other = self.conv_graph[related_rust_ty].clone();
                            (
                                try_build_path(
                                    &other,
                                    &from,
                                    build_for_sp,
                                    &mut self.conv_graph,
                                    &self.rust_names_map,
                                    &self.generic_edges,
                                    max_steps,
                                ),
                                related_rust_ty,
                            )
                        } else {
                            continue;
                        }
                    }
                };
                if let Some(path) = path {
                    possible_paths.push((path, ftype_idx, related_rust_ty));
                }
            }
            if !possible_paths.is_empty() {
                break;
            }
        }
        let ret = possible_paths
            .into_iter()
            .min_by_key(|(path, _, _)| path.len())
            .map(|(pp, ftype, rtype_idx)| {
                merge_path_to_conv_map(pp, self);
                debug!(
                    "map foreign: we found min path {} <-> {} ({})",
                    rust_ty, self.conv_graph[rtype_idx], self[ftype].name
                );
                ftype
            });
        if ret.is_none() {
            debug!(
                "map to foreign failed, foreign_map {}\n conv_graph: {}",
                self.ftypes_storage,
                DisplayTypesConvGraph(&self.conv_graph),
            );
        }
        ret
    }

    pub(crate) fn find_foreigner_class_with_such_this_type<
        F: Fn(&TypeMap, &ForeignerClassInfo) -> Option<Type>,
    >(
        &self,
        this_ty: &Type,
        get_this_type: F,
    ) -> Option<&ForeignerClassInfo> {
        let this_name = normalize_ty_lifetimes(this_ty);
        for fc in &self.foreign_classes {
            if let Some(this_type_for_method) = get_this_type(self, fc) {
                let cur_this = normalize_ty_lifetimes(&this_type_for_method);
                if cur_this == this_name {
                    return Some(fc);
                }
            }
        }
        None
    }

    pub(crate) fn register_foreigner_class(&mut self, class: &ForeignerClassInfo) {
        self.foreign_classes.push(class.clone());
    }

    fn add_node<F: FnOnce() -> RustTypeS>(
        &mut self,
        key: SmolStr,
        init_without_graph_idx: F,
    ) -> NodeIndex {
        let rust_names_map = &mut self.rust_names_map;
        let conv_graph = &mut self.conv_graph;
        *rust_names_map.entry(key).or_insert_with(|| {
            let idx = conv_graph.add_node(Rc::new(init_without_graph_idx()));
            Rc::get_mut(&mut conv_graph[idx])
                .expect("Internal error: can not modify Rc")
                .graph_idx = idx;
            idx
        })
    }

    pub(crate) fn find_or_alloc_rust_type(&mut self, ty: &Type, src_id: SourceId) -> RustType {
        let name = normalize_ty_lifetimes(ty);
        let idx = self.add_node(name.into(), || {
            RustTypeS::new_without_graph_idx(ty.clone(), name, src_id)
        });
        self.conv_graph[idx].clone()
    }

    pub(crate) fn find_or_alloc_rust_type_no_src_id(&mut self, ty: &Type) -> RustType {
        let name = normalize_ty_lifetimes(ty);
        let idx = self.add_node(name.into(), || {
            RustTypeS::new_without_graph_idx(ty.clone(), name, SourceId::none())
        });
        self.conv_graph[idx].clone()
    }

    pub(crate) fn find_or_alloc_rust_type_that_implements(
        &mut self,
        ty: &Type,
        trait_name: &str,
        src_id: SourceId,
    ) -> RustType {
        let name = normalize_ty_lifetimes(ty);
        let idx = self.add_node(name.into(), || {
            RustTypeS::new_without_graph_idx(ty.clone(), name, src_id).implements(trait_name)
        });
        self.conv_graph[idx].clone()
    }

    pub(crate) fn find_or_alloc_rust_type_with_suffix(
        &mut self,
        ty: &Type,
        suffix: &str,
        src_id: SourceId,
    ) -> RustType {
        let name: SmolStr = make_unique_rust_typename(normalize_ty_lifetimes(ty), suffix).into();
        let idx = self.add_node(name.clone(), || {
            RustTypeS::new_without_graph_idx(ty.clone(), name, src_id)
        });
        self.conv_graph[idx].clone()
    }

    pub(crate) fn find_or_alloc_rust_type_with_may_be_suffix(
        &mut self,
        ty: &Type,
        suffix: Option<String>,
        src_id: SourceId,
    ) -> RustType {
        if let Some(suffix) = suffix {
            self.find_or_alloc_rust_type_with_suffix(ty, &suffix, src_id)
        } else {
            self.find_or_alloc_rust_type(ty, src_id)
        }
    }

    /// # Panics
    pub(crate) fn ty_to_rust_type(&self, ty: &Type) -> RustType {
        self.ty_to_rust_type_checked(ty).unwrap_or_else(|| {
            panic!(
                "Internal Error: type '{}' unknown (ty_to_rust_type)",
                DisplayToTokens(ty)
            )
        })
    }

    pub(crate) fn ty_to_rust_type_checked(&self, ty: &Type) -> Option<RustType> {
        let name = normalize_ty_lifetimes(ty);
        self.rust_names_map
            .get(name)
            .map(|idx| self.conv_graph[*idx].clone())
    }

    pub(crate) fn find_rust_type_with_suffix(&self, ty: &Type, suffix: &str) -> Option<RustType> {
        let name: SmolStr = make_unique_rust_typename(normalize_ty_lifetimes(ty), suffix).into();
        self.rust_names_map
            .get(&name)
            .map(|idx| self.conv_graph[*idx].clone())
    }
}

impl ops::Index<ForeignType> for TypeMap {
    type Output = ForeignTypeS;
    fn index(&self, idx: ForeignType) -> &Self::Output {
        &self.ftypes_storage[idx]
    }
}

impl ops::Index<RustTypeIdx> for TypeMap {
    type Output = Rc<RustTypeS>;
    fn index(&self, idx: RustTypeIdx) -> &Self::Output {
        &self.conv_graph[idx]
    }
}

pub(in crate::typemap) fn validate_code_template(sp: SourceIdSpan, code: &str) -> Result<()> {
    if code.contains(TO_VAR_TEMPLATE)
        && code.contains(FROM_VAR_TEMPLATE)
        && code.contains(TO_VAR_TYPE_TEMPLATE)
    {
        Ok(())
    } else {
        Err(DiagnosticError::new(
            sp.0,
            sp.1,
            format!(
                "{} not contains one of {}, {}, {}",
                code, TO_VAR_TEMPLATE, FROM_VAR_TEMPLATE, TO_VAR_TYPE_TEMPLATE
            ),
        ))
    }
}

pub(crate) fn make_unique_rust_typename(
    not_unique_name: &str,
    suffix_to_make_unique: &str,
) -> String {
    format!("{}{}{}", not_unique_name, 0 as char, suffix_to_make_unique)
}

pub(crate) fn make_unique_rust_typename_if_need(
    rust_typename: String,
    suffix: Option<String>,
) -> String {
    match suffix {
        Some(s) => make_unique_rust_typename(&rust_typename, &s),
        None => rust_typename,
    }
}

pub(crate) fn unpack_unique_typename(name: &str) -> &str {
    match name.find('\0') {
        Some(pos) => &name[0..pos],
        None => name,
    }
}

fn apply_code_template(
    code_temlate: &str,
    to_name: &str,
    from_name: &str,
    to_typename: &str,
    func_ret_type: &str,
) -> String {
    let mut ret = String::new();
    ret.push_str("    ");
    ret.push_str(code_temlate);
    ret.push('\n');
    ret.replace(TO_VAR_TEMPLATE, to_name)
        .replace(FROM_VAR_TEMPLATE, from_name)
        .replace(TO_VAR_TYPE_TEMPLATE, to_typename)
        .replace(FUNCTION_RETURN_TYPE_TEMPLATE, func_ret_type)
}

fn find_conversation_path(
    conv_graph: &TypesConvGraph,
    from: RustTypeIdx,
    to: RustTypeIdx,
    build_for_sp: SourceIdSpan,
) -> Result<Vec<EdgeIndex<TypeGraphIdx>>> {
    trace!(
        "find_conversation_path: begin {} -> {}",
        conv_graph[from],
        conv_graph[to]
    );

    if let Some((_, nodes_path)) = petgraph::algo::astar(
        conv_graph,
        from,
        |idx| idx == to,
        |_| 1,
        |idx| if idx != from { 1 } else { 0 },
    ) {
        let mut edges = Vec::with_capacity(nodes_path.len());
        for (cur_node, next_node) in nodes_path.iter().zip(nodes_path.iter().skip(1)) {
            edges.push(
                conv_graph
                    .find_edge(*cur_node, *next_node)
                    .expect("Internal error: find_conversation_path no edge"),
            );
        }
        Ok(edges)
    } else {
        let mut err = DiagnosticError::new2(
            conv_graph[from].src_id_span(),
            format!("Can not find conversation from type '{}'", conv_graph[from]),
        );
        err.span_note(
            conv_graph[to].src_id_span(),
            format!("to type '{}'", conv_graph[to]),
        );
        err.span_note(build_for_sp, "In this context");
        Err(err)
    }
}

fn merge_path_to_conv_map(path: PossiblePath, conv_map: &mut TypeMap) {
    let PossiblePath {
        new_edges,
        path_len: _,
    } = path;

    for (from, to, conv_rule) in new_edges {
        let from_idx = conv_map.add_node(from.normalized_name.clone(), || (*from).clone());
        let to_idx = conv_map.add_node(to.normalized_name.clone(), || (*to).clone());
        assert!(conv_map.conv_graph.find_edge(from_idx, to_idx).is_none());
        conv_map.conv_graph.add_edge(from_idx, to_idx, conv_rule);
    }
}

fn try_build_path(
    start_from: &RustType,
    goal_to: &RustType,
    build_for_sp: SourceIdSpan,
    conv_graph: &mut TypesConvGraph,
    rust_names_map: &RustTypeNameToGraphIdx,
    generic_edges: &[GenericTypeConv],
    max_steps: usize,
) -> Option<PossiblePath> {
    debug!(
        "try_build_path: from {} to {}, ty names len {}, graph nodes {}, edges {}",
        start_from,
        goal_to,
        rust_names_map.len(),
        conv_graph.node_count(),
        conv_graph.edge_count()
    );
    let mut ty_graph = TypeGraphSnapshot::new(conv_graph, &rust_names_map);

    let start_from_idx = start_from.graph_idx;

    let goal_to_idx = goal_to.graph_idx;

    let mut cur_step = FxHashSet::default();
    cur_step.insert(start_from_idx);
    let mut next_step = FxHashSet::default();

    for step in 0..max_steps {
        debug!("try_build_path: do step {}", step);
        if cur_step.is_empty() {
            break;
        }
        if log_enabled!(log::Level::Debug) {
            use std::fmt::Write;
            let mut step_types = String::new();
            for from_ty in &cur_step {
                write!(step_types, "{} ", ty_graph.conv_graph[*from_ty]).unwrap();
            }
            debug!("try_build_path: cur_step {}", step_types);
        }
        for from_ty in &cur_step {
            let from: RustType = ty_graph.conv_graph[*from_ty].clone();
            for neighbor in ty_graph
                .conv_graph
                .neighbors_directed(*from_ty, petgraph::Outgoing)
            {
                next_step.insert(neighbor);
            }
            for edge in generic_edges {
                trace!(
                    "try_build_path: we check edge({:?} -> {:?}) for {}",
                    edge.from_ty,
                    edge.to_ty,
                    from
                );
                if let Some((to_ty, to_ty_name)) =
                    edge.is_conv_possible(&from, Some(goal_to), |name| {
                        ty_graph.find_type_by_name(name)
                    })
                {
                    if from.normalized_name == to_ty_name {
                        continue;
                    }
                    let to = ty_graph.node_for_ty(edge.src_id, (to_ty, to_ty_name));
                    ty_graph.add_edge(
                        *from_ty,
                        to,
                        TypeConvEdge {
                            code_template: edge.code_template.clone(),
                            dependency: edge.dependency.clone(),
                        },
                    );

                    if petgraph::algo::has_path_connecting(
                        &*ty_graph.conv_graph,
                        to,
                        goal_to_idx,
                        None,
                    ) {
                        debug!("try_build_path: NEW ALGO: we found PATH!!!!");
                        let path = find_conversation_path(
                            &ty_graph.conv_graph,
                            start_from_idx,
                            goal_to_idx,
                            build_for_sp,
                        )
                        .expect("path must exists");
                        if log_enabled!(log::Level::Debug) {
                            for edge in &path {
                                if let Some((from, to)) = ty_graph.conv_graph.edge_endpoints(*edge)
                                {
                                    debug!(
                                        "try_build_path: path: {} -> {}",
                                        ty_graph.conv_graph[from], ty_graph.conv_graph[to]
                                    );
                                }
                            }
                        }

                        return Some(PossiblePath::new(ty_graph, path));
                    }
                    next_step.insert(to);
                }
            }
        }
        mem::swap(&mut cur_step, &mut next_step);
        next_step.clear();
    }
    debug!("try_build_path: No results");
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{source_registry::SourceRegistry, SourceCode};
    use proc_macro2::Span;

    #[test]
    fn test_try_build_path() {
        let _ = env_logger::try_init();
        let mut types_map = TypeMap::default();
        let mut src_reg = SourceRegistry::default();
        let src_id = src_reg.register(SourceCode {
            id_of_code: "test_try_build_path".into(),
            code: include_str!("java_jni/jni-include.rs").into(),
        });
        types_map.merge(src_id, src_reg.src(src_id), 64).unwrap();

        let foo_rt: RustType = types_map.find_or_alloc_rust_type_that_implements(
            &parse_type! { Foo },
            "SwigForeignClass",
            SourceId::none(),
        );
        types_map.register_foreigner_class(&ForeignerClassInfo {
            src_id: SourceId::none(),
            name: Ident::new("Foo", Span::call_site()),
            methods: vec![],
            self_type: None,
            foreigner_code: String::new(),
            constructor_ret_type: Some(foo_rt.ty.clone()),
            doc_comments: vec![],
            copy_derived: false,
        });

        let rc_refcell_foo_ty = types_map
            .find_or_alloc_rust_type(&parse_type! { &mut Rc<RefCell<Foo>> }, SourceId::none());
        let foo_ref_ty =
            types_map.find_or_alloc_rust_type(&parse_type! { &mut Foo }, SourceId::none());

        assert_eq!(
            r#"    let mut a0: & Rc < RefCell < Foo > > = a0;
    let mut a0: & RefCell < Foo > = a0.swig_deref();
    let mut a0: RefMut < Foo > = <RefMut < Foo >>::swig_from(a0, env);
    let mut a0: & mut Foo = a0.swig_deref_mut();
"#,
            types_map
                .convert_rust_types(
                    &rc_refcell_foo_ty,
                    &foo_ref_ty,
                    "a0",
                    "jlong",
                    invalid_src_id_span(),
                )
                .expect("path from &mut Rc<RefCell<Foo>> to &mut Foo NOT exists")
                .1,
        );

        let rc_refcell_foo_ty =
            types_map.find_or_alloc_rust_type(&parse_type! { &RefCell<Foo> }, SourceId::none());
        let foo_ref_ty = types_map.find_or_alloc_rust_type(&parse_type! { &Foo }, SourceId::none());

        assert_eq!(
            r#"    let mut a0: Ref < Foo > = <Ref < Foo >>::swig_from(a0, env);
    let mut a0: & Foo = a0.swig_deref();
"#,
            types_map
                .convert_rust_types(
                    &rc_refcell_foo_ty,
                    &foo_ref_ty,
                    "a0",
                    "jlong",
                    invalid_src_id_span(),
                )
                .expect("path from &RefCell<Foo> to &Foo NOT exists")
                .1
        );

        let vec_foo_ty =
            types_map.find_or_alloc_rust_type(&parse_type! { Vec<Foo> }, SourceId::none());

        let fti = types_map
            .map_through_conversation_to_foreign(
                &vec_foo_ty,
                petgraph::Direction::Outgoing,
                invalid_src_id_span(),
                |_, fc| fc.constructor_ret_type.clone(),
            )
            .unwrap();
        assert_eq!("Foo []", types_map[fti].name.as_str());

        assert!(try_build_path(
            &types_map.find_or_alloc_rust_type(&parse_type! { Vec<i32> }, SourceId::none()),
            &types_map.find_or_alloc_rust_type(&parse_type! { jlong }, SourceId::none()),
            invalid_src_id_span(),
            &mut types_map.conv_graph,
            &mut types_map.rust_names_map,
            &types_map.generic_edges,
            MAX_TRY_BUILD_PATH_STEPS,
        )
        .is_none());
    }
}
