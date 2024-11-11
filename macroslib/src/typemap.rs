pub mod ast;
mod merge;
mod parse;
pub mod ty;
mod typemap_macro;
pub mod utils;

use std::{borrow::Cow, cell::RefCell, fmt, mem, ops, rc::Rc};

use log::{debug, log_enabled, trace, warn};
use petgraph::{
    graph::{EdgeIndex, NodeIndex},
    Graph,
};
use proc_macro2::{Span, TokenStream};
use rustc_hash::{FxHashMap, FxHashSet};
use smallvec::SmallVec;
use smol_str::SmolStr;
use syn::{parse_quote, Ident, Type};

use crate::{
    error::{invalid_src_id_span, DiagnosticError, Result, SourceIdSpan},
    source_registry::SourceId,
    typemap::{
        ast::{
            get_trait_bounds, normalize_type, DisplayToTokens, ForeignTypeName, GenericTypeConv,
        },
        ty::{
            ForeignConversionRule, ForeignType, ForeignTypeS, ForeignTypesStorage, RustType,
            RustTypeS,
        },
    },
    types::ForeignClassInfo,
};
use ast::ConversionResult;

pub(crate) use typemap_macro::{
    CItem, CItems, ExpandedFType, MacroArgs, TypeMapConvRuleInfo, TypeMapConvRuleInfoExpanderHelper,
};

use self::ast::UniqueName;
pub(crate) static TO_VAR_TEMPLATE: &str = "{to_var}";
pub(crate) static FROM_VAR_TEMPLATE: &str = "{from_var}";
pub(crate) static TO_VAR_TYPE_TEMPLATE: &str = "{to_var_type}";
pub(in crate::typemap) static FUNCTION_RETURN_TYPE_TEMPLATE: &str = "{function_ret_type}";
const MAX_TRY_BUILD_PATH_STEPS: usize = 7;

/// contains code to convert from one type to another
#[derive(Debug, Clone)]
pub(crate) struct TypeConvCode {
    pub(in crate::typemap) span: SourceIdSpan,
    code: String,
    params: Vec<SmolStr>,
}

impl PartialEq for TypeConvCode {
    fn eq(&self, o: &Self) -> bool {
        self.code == o.code
    }
}

#[derive(Debug)]
pub(crate) enum TypeConvCodeSubstParam<'a> {
    Name(&'a str),
    Tmp(&'a str),
}

impl TypeConvCode {
    pub(crate) fn invalid() -> Self {
        TypeConvCode {
            span: invalid_src_id_span(),
            code: "invalid code".into(),
            params: vec![],
        }
    }

    pub(crate) fn with_params<S: Into<String>>(
        code: S,
        span: SourceIdSpan,
        params: Vec<SmolStr>,
    ) -> TypeConvCode {
        let code: String = code.into();
        TypeConvCode { code, span, params }
    }

    /// # Panics
    pub(crate) fn new<S: Into<String>>(code: S, span: SourceIdSpan) -> TypeConvCode {
        let code: String = code.into();

        let mut params = Vec::with_capacity(1);

        if code.contains(TO_VAR_TEMPLATE) {
            params.push(TO_VAR_TEMPLATE.into());
        }
        if code.contains(FROM_VAR_TEMPLATE) {
            params.push(FROM_VAR_TEMPLATE.into());
        }

        if params.is_empty() {
            panic!(
                "Code: '{}' should contains {} or {}",
                code, TO_VAR_TEMPLATE, FROM_VAR_TEMPLATE
            );
        }

        TypeConvCode { code, span, params }
    }
    /// # Panics
    pub(crate) fn new2<S: Into<String>>(code: S, span: SourceIdSpan) -> TypeConvCode {
        let code: String = code.into();
        let mut params = Vec::with_capacity(2);
        if code.contains(TO_VAR_TEMPLATE) {
            params.push(TO_VAR_TEMPLATE.into());
        }
        if code.contains(FROM_VAR_TEMPLATE) {
            params.push(FROM_VAR_TEMPLATE.into());
        }

        assert!(
            params.len() >= 2,
            "Code: '{}' should contains {} and {}",
            code,
            TO_VAR_TEMPLATE,
            FROM_VAR_TEMPLATE
        );

        TypeConvCode { code, span, params }
    }

    pub(crate) fn as_str(&self) -> &str {
        self.code.as_str()
    }
    pub(crate) fn src_id(&self) -> SourceId {
        self.span.0
    }
    pub(crate) fn span(&self) -> Span {
        self.span.1
    }
    pub(crate) fn full_span(&self) -> SourceIdSpan {
        self.span
    }
    pub(crate) fn params(&self) -> &[SmolStr] {
        &self.params
    }

    pub(crate) fn generate_code(
        &self,
        to_name: &str,
        from_name: &str,
        to_typename: &str,
        func_ret_type: &str,
    ) -> String {
        let mut ret = String::with_capacity(self.code.len() + 5);
        ret.push_str("    ");
        ret.push_str(&self.code);
        ret.push('\n');
        ret.replace(TO_VAR_TEMPLATE, to_name)
            .replace(FROM_VAR_TEMPLATE, from_name)
            .replace(TO_VAR_TYPE_TEMPLATE, to_typename)
            .replace(FUNCTION_RETURN_TYPE_TEMPLATE, func_ret_type)
    }

    pub(crate) fn generate_code_with_subst_func<'b, F>(&self, mut subst_func: F) -> Result<String>
    where
        for<'a> F: FnMut(TypeConvCodeSubstParam<'a>) -> Option<Cow<'b, str>>,
    {
        let mut subst = Vec::with_capacity(self.params.len());
        for p in &self.params {
            let param_id = if let Some(stripped) = p.strip_prefix('$') {
                TypeConvCodeSubstParam::Tmp(stripped)
            } else {
                TypeConvCodeSubstParam::Name(p)
            };
            let param_val = subst_func(param_id).ok_or_else(|| {
                DiagnosticError::new2(self.span, format!("Can not substitude parameter {}", p))
            })?;
            subst.push(param_val);
        }
        let mut ret = self.code.clone();
        for (id, val) in self.params.iter().zip(subst.iter()) {
            ret = ret.replace(id.as_str(), val.as_ref());
        }
        Ok(ret)
    }
    pub(crate) fn has_param(&self, param_name: &str) -> bool {
        self.params.iter().any(|x| *x == param_name)
    }
}

impl ToString for TypeConvCode {
    fn to_string(&self) -> String {
        self.code.clone()
    }
}

impl From<TypeConvCode> for String {
    fn from(x: TypeConvCode) -> Self {
        x.code
    }
}

#[derive(Debug, Clone)]
pub(crate) struct TypeConvEdge {
    code: TypeConvCode,
    dependency: Rc<RefCell<Option<TokenStream>>>,
}

impl From<TypeConvCode> for TypeConvEdge {
    fn from(code: TypeConvCode) -> Self {
        TypeConvEdge {
            code,
            dependency: Rc::new(RefCell::new(None)),
        }
    }
}

impl TypeConvEdge {
    pub(crate) fn new(code: TypeConvCode, dependency: Option<TokenStream>) -> TypeConvEdge {
        TypeConvEdge {
            code,
            dependency: Rc::new(RefCell::new(dependency)),
        }
    }
}

pub(crate) type TypeGraphIdx = u32;
pub(crate) type TypesConvGraph = Graph<RustType, TypeConvEdge, petgraph::Directed, TypeGraphIdx>;

pub(crate) type RustTypeIdx = NodeIndex<TypeGraphIdx>;

type RustTypeNameToGraphIdx = FxHashMap<SmolStr, RustTypeIdx>;

#[derive(Debug)]
pub(crate) struct TypeMap {
    conv_graph: TypesConvGraph,
    ftypes_storage: ForeignTypesStorage,
    rust_to_foreign_cache: FxHashMap<RustTypeIdx, ForeignType>,
    rust_from_foreign_cache: FxHashMap<RustTypeIdx, ForeignType>,
    rust_names_map: RustTypeNameToGraphIdx,
    utils_code: Vec<syn::Item>,
    generic_edges: Vec<GenericTypeConv>,
    foreign_classes: Vec<ForeignClassInfo>,
    /// How to use trait to convert types, Trait Name -> Code
    traits_usage_code: FxHashMap<Ident, String>,
    /// code that parsed, but not yet integrated to TypeMap,
    /// because of it is possible only in langauge backend
    not_merged_data: Vec<TypeMapConvRuleInfo>,
    generic_rules: Vec<Rc<TypeMapConvRuleInfo>>,
}

impl Default for TypeMap {
    fn default() -> Self {
        let generic_params: syn::Generics = parse_quote! { <T> };
        let default_rules = vec![
            GenericTypeConv::new(
                parse_type! { T },
                parse_type! { &T },
                generic_params.clone(),
                TypeConvCode::new2(
                    "let mut {to_var}: {to_var_type} = &{from_var};",
                    invalid_src_id_span(),
                ),
            ),
            GenericTypeConv::new(
                parse_type! { T },
                parse_type! { &mut T },
                generic_params.clone(),
                TypeConvCode::new2(
                    "let mut {to_var}: {to_var_type} = &mut {from_var};",
                    invalid_src_id_span(),
                ),
            ),
            GenericTypeConv::new(
                parse_type! { &mut T },
                parse_type! { &T },
                generic_params.clone(),
                TypeConvCode::new2(
                    "let mut {to_var}: {to_var_type} = {from_var};",
                    invalid_src_id_span(),
                ),
            ),
            GenericTypeConv::new(
                parse_type! { & Box<T> },
                parse_type! { &T },
                generic_params.clone(),
                TypeConvCode::new2(
                    "let mut {to_var}: {to_var_type} = {from_var}.as_ref();",
                    invalid_src_id_span(),
                ),
            ),
            GenericTypeConv::new(
                parse_type! { & mut Box<T> },
                parse_type! { &mut T },
                generic_params,
                TypeConvCode::new2(
                    "let mut {to_var}: {to_var_type} = {from_var}.as_mut();",
                    invalid_src_id_span(),
                ),
            ),
        ];
        TypeMap {
            conv_graph: TypesConvGraph::new(),
            rust_names_map: FxHashMap::default(),
            utils_code: Vec::new(),
            generic_edges: default_rules,
            rust_from_foreign_cache: FxHashMap::default(),
            rust_to_foreign_cache: FxHashMap::default(),
            foreign_classes: Vec::new(),
            traits_usage_code: FxHashMap::default(),
            ftypes_storage: ForeignTypesStorage::default(),
            not_merged_data: vec![],
            generic_rules: vec![],
        }
    }
}

struct DisplayTypesConvGraph<'a>(&'a TypesConvGraph);

impl fmt::Display for DisplayTypesConvGraph<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> std::result::Result<(), fmt::Error> {
        let conv_graph = self.0;
        writeln!(f, "conversion graph begin")?;
        for node in conv_graph.node_indices() {
            write!(
                f,
                "node({}) {}: ",
                node.index(),
                conv_graph[node].normalized_name
            )?;
            let mut edges = conv_graph
                .neighbors_directed(node, petgraph::Outgoing)
                .detach();
            while let Some((_, target)) = edges.next(conv_graph) {
                write!(f, "->{}, ", conv_graph[target].normalized_name)?;
            }
            writeln!(f)?;
        }
        writeln!(f, "conversion graph end")
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
    pub name: UniqueName,
    pub corresponding_rust_type: RustType,
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

impl Drop for TypeGraphSnapshot<'_> {
    fn drop(&mut self) {
        for edge in self.new_edges.iter().rev() {
            self.conv_graph.remove_edge(*edge);
        }
        for idx in self.new_nodes.iter().rev() {
            self.conv_graph.remove_node(*idx);
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) enum MapToForeignFlag {
    FastSearch,
    FullSearch,
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
        corresponding_rty: RustType,
        foreign_name: ForeignTypeName,
    ) -> Result<ForeignType> {
        trace!("add_foreign: {} / {}", foreign_name, corresponding_rty);
        self.ftypes_storage
            .alloc_new(foreign_name, corresponding_rty.graph_idx)
    }

    pub(crate) fn add_foreign_rust_ty_idx(
        &mut self,
        foreign_name: ForeignTypeName,
        corresponding_rty: NodeIndex,
    ) -> Result<ForeignType> {
        trace!(
            "add_foreign_rust_ty_idx: {} / {}",
            foreign_name,
            self[corresponding_rty]
        );
        self.ftypes_storage
            .alloc_new(foreign_name, corresponding_rty)
    }

    pub(crate) fn alloc_foreign_type(&mut self, ft: ForeignTypeS) -> Result<ForeignType> {
        self.ftypes_storage.add_new_ftype(ft)
    }

    pub(crate) fn find_foreign_type_related_to_rust_ty(
        &self,
        rust_ty: RustTypeIdx,
    ) -> Option<ForeignType> {
        for (idx, ft) in self.ftypes_storage.iter_enumerate() {
            if let (
                Some(ForeignConversionRule {
                    rust_ty: into,
                    intermediate: None,
                }),
                Some(ForeignConversionRule {
                    rust_ty: from,
                    intermediate: None,
                }),
            ) = (ft.into_from_rust.as_ref(), ft.from_into_rust.as_ref())
            {
                if *into == *from && *into == rust_ty {
                    return Some(idx);
                }
            }
        }
        None
    }

    pub(crate) fn add_conversion_rule(
        &mut self,
        from: RustTypeIdx,
        to: RustTypeIdx,
        rule: TypeConvEdge,
    ) {
        debug!(
            "TypesConvMap::add_conversion_rule '{}' -> '{}': {:?}",
            self[from], self[to], rule
        );
        self.conv_graph.update_edge(from, to, rule);
    }

    fn find_or_build_path(
        &mut self,
        from: RustTypeIdx,
        to: RustTypeIdx,
        build_for_sp: SourceIdSpan,
    ) -> Result<Vec<EdgeIndex<TypeGraphIdx>>> {
        let path = match self.find_path(from, to, build_for_sp) {
            Ok(x) => x,
            Err(_err) => {
                debug!("convert_rust_types: no path, trying to build it");
                self.build_path_if_possible(from, to, build_for_sp);
                self.find_path(from, to, build_for_sp)?
            }
        };
        Ok(path)
    }

    pub(crate) fn convert_rust_types(
        &mut self,
        from: RustTypeIdx,
        to: RustTypeIdx,
        in_var_name: &str,
        out_var_name: &str,
        function_ret_type: &str,
        build_for_sp: SourceIdSpan,
    ) -> Result<(Vec<TokenStream>, String)> {
        let path = self.find_or_build_path(from, to, build_for_sp)?;
        let mut ret_code = String::new();
        let mut code_deps = Vec::<TokenStream>::new();

        if path.is_empty() && in_var_name != out_var_name {
            return Ok((
                code_deps,
                format!("let mut {} = {};", out_var_name, in_var_name),
            ));
        }

        for (idx, edge) in path.into_iter().enumerate() {
            let (_, target) = self.conv_graph.edge_endpoints(edge).unwrap();
            let target_typename: SmolStr = self.conv_graph[target].typename().into();
            let edge = &mut self.conv_graph[edge];
            if let Some(dep) = edge.dependency.borrow_mut().take() {
                code_deps.push(dep);
            }
            let code = edge.code.generate_code(
                out_var_name,
                if idx != 0 { out_var_name } else { in_var_name },
                &target_typename,
                function_ret_type,
            );
            ret_code.push_str(&code);
        }
        Ok((code_deps, ret_code))
    }

    fn find_path(
        &self,
        from: RustTypeIdx,
        to: RustTypeIdx,
        build_for_sp: SourceIdSpan,
    ) -> Result<Vec<EdgeIndex<TypeGraphIdx>>> {
        debug!(
            "find_path: begin {}/{:?} -> {}/{:?}",
            self[from], from, self[to], to
        );
        if from == to {
            return Ok(vec![]);
        }
        find_conversion_path(&self.conv_graph, from, to, build_for_sp)
    }

    fn build_path_if_possible(
        &mut self,
        start_from: RustTypeIdx,
        goal_to: RustTypeIdx,
        build_for_sp: SourceIdSpan,
    ) {
        debug!(
            "build_path_if_possible begin {}\n {} -> {}",
            DisplayTypesConvGraph(&self.conv_graph),
            self[start_from],
            self[goal_to]
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
    pub(crate) fn map_through_conversion_to_foreign<
        F: Fn(&TypeMap, &ForeignClassInfo) -> Option<Type>,
    >(
        &mut self,
        rust_ty: RustTypeIdx,
        direction: petgraph::Direction,
        flag: MapToForeignFlag,
        build_for_sp: SourceIdSpan,
        calc_this_type_for_method: F,
    ) -> Option<ForeignType> {
        debug!(
            "map_through_conversion_to_foreign: {} {:?}",
            self[rust_ty], direction
        );

        let cached = match direction {
            petgraph::Direction::Outgoing => self.rust_to_foreign_cache.get(&rust_ty),
            petgraph::Direction::Incoming => self.rust_from_foreign_cache.get(&rust_ty),
        };
        if let Some(cached) = cached {
            return Some(*cached);
        }

        {
            debug!(
                "map_through_conversion_to_foreign: graph node {:?}",
                self.conv_graph[rust_ty]
            );
            let find_path = |from, to| {
                find_conversion_path(&self.conv_graph, from, to, invalid_src_id_span()).ok()
            };
            let mut min_path: Option<(usize, RustTypeIdx, ForeignType)> = None;
            for (ftype_idx, ftype) in self.ftypes_storage.iter_enumerate() {
                let (related_rty_idx, path) = match direction {
                    petgraph::Direction::Outgoing => {
                        if let Some(rule) = ftype.into_from_rust.as_ref() {
                            (rule.rust_ty, find_path(rust_ty, rule.rust_ty))
                        } else {
                            continue;
                        }
                    }
                    petgraph::Direction::Incoming => {
                        if let Some(rule) = ftype.from_into_rust.as_ref() {
                            (rule.rust_ty, find_path(rule.rust_ty, rust_ty))
                        } else {
                            continue;
                        }
                    }
                };
                if let Some(path) = path {
                    trace!(
                        "map_through_conversion_to_foreign: path found: {} / {}",
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
            if let Some((path_len, rust_type_idx, ftype)) = min_path {
                debug!(
                    "map foreign: we found min path ({}) {} <-> {} ({})",
                    path_len, self[rust_ty], self.conv_graph[rust_type_idx], self[ftype].name
                );

                match direction {
                    petgraph::Direction::Outgoing => {
                        self.rust_to_foreign_cache.insert(rust_ty, ftype);
                    }
                    petgraph::Direction::Incoming => {
                        self.rust_from_foreign_cache.insert(rust_ty, ftype);
                    }
                }

                return Some(ftype);
            }
        }

        if flag == MapToForeignFlag::FastSearch {
            debug!("No direct path, because of FastSearch just exiting");
            return None;
        }

        debug!(
            "map foreign: No paths exists, may be we can create one for '{}' {:?}?",
            self[rust_ty], direction
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
                                    .replace(&ty_param_name, rust_ty.normalized_name.as_str());
                                let foreign_name = to_foreigner_hint
                                    .as_str()
                                    .replace(&ty_param_name, &class.name.to_string());
                                new_foreign_types.insert((
                                    edge.to_ty.clone(),
                                    suffix,
                                    ForeignTypeName::new(
                                        foreign_name,
                                        (class.src_id, class.name.span()),
                                    ),
                                ));
                            } else {
                                println!(
                                    "cargo:warning=No foreign_class for type '{}'",
                                    rust_ty.normalized_name
                                );
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
            let fname = foreign_name.typename.clone();
            let ftype_idx = self.ftypes_storage.find_or_alloc(foreign_name);
            let conv_rule = match direction {
                petgraph::Direction::Outgoing => &mut self.ftypes_storage[ftype_idx].into_from_rust,
                petgraph::Direction::Incoming => &mut self.ftypes_storage[ftype_idx].from_into_rust,
            };
            if let Some(r) = conv_rule {
                warn!(
                    "attempt to rewrite rule {}/{} with {}/{}",
                    fname, self.conv_graph[r.rust_ty], fname, rust_ty
                );
                continue;
            }
            *conv_rule = Some(ForeignConversionRule {
                rust_ty: rust_ty.graph_idx,
                intermediate: None,
            });
        }

        let from: RustType = self[rust_ty].clone();
        let mut possible_paths =
            Vec::<(PossiblePath, ForeignType, RustTypeIdx, Option<RustTypeIdx>)>::new();
        for max_steps in 1..=MAX_TRY_BUILD_PATH_STEPS {
            for (ftype_idx, ftype) in self.ftypes_storage.iter_enumerate() {
                let rule = match direction {
                    petgraph::Direction::Outgoing => ftype.into_from_rust.as_ref(),
                    petgraph::Direction::Incoming => ftype.from_into_rust.as_ref(),
                };

                let rule = match rule {
                    Some(x) => x,
                    None => continue,
                };
                let other = rule.rust_ty;
                let (from, to) = match direction {
                    petgraph::Direction::Outgoing => (from.to_idx(), other),
                    petgraph::Direction::Incoming => (other, from.to_idx()),
                };
                let path = try_build_path(
                    from,
                    to,
                    build_for_sp,
                    &mut self.conv_graph,
                    &self.rust_names_map,
                    &self.generic_edges,
                    max_steps,
                );

                if let Some(path) = path {
                    possible_paths.push((
                        path,
                        ftype_idx,
                        rule.rust_ty,
                        rule.intermediate.as_ref().map(|x| x.intermediate_ty),
                    ));
                }
            }
            if !possible_paths.is_empty() {
                break;
            }
        }
        // remove the same paths: ty -> intermediate_ty)
        // ty -> ftype(intermediate_ty), they have the same length,
        // so min_by_key can not find appropriate
        {
            let mut inter_ty_set = FxHashSet::default();
            for (_, _, _, inter_ty) in &possible_paths {
                if let Some(inter_ty) = inter_ty {
                    inter_ty_set.insert(*inter_ty);
                }
            }
            possible_paths.retain(|(_, _, idx, _)| !inter_ty_set.contains(idx));
        }
        let ret = possible_paths
            .into_iter()
            .min_by_key(|(path, ftype_idx, other, inter_ty)| {
                let mut addon_path_len = 0;
                if let Some(inter_ty) = inter_ty {
                    let addon_path = match direction {
                        petgraph::Direction::Outgoing => {
                            self.find_or_build_path(*other, *inter_ty, invalid_src_id_span())
                        }
                        petgraph::Direction::Incoming => {
                            self.find_or_build_path(*inter_ty, *other, invalid_src_id_span())
                        }
                    };
                    addon_path_len = match addon_path {
                        Ok(addon_path) => addon_path.len(),
                        Err(_err) => {
                            println!(
                                "cargo:warning=can not build path between foreign type
 '{}' / '{}' and it's intermediate '{}'",
                                self[*ftype_idx].name, self[*other], self[*inter_ty]
                            );
                            0_usize
                        }
                    };
                }
                path.len() + addon_path_len
            })
            .map(|(pp, ftype, rtype_idx, _)| {
                let path_len = pp.len();
                merge_path_to_conv_map(pp, self);
                debug!(
                    "map foreign: we found min path ({}) '{}' <-> '{}' ({})",
                    path_len, self[rust_ty], self.conv_graph[rtype_idx], self[ftype].name
                );
                ftype
            });
        if log_enabled!(log::Level::Debug) {
            match ret {
                Some(f_idx) => debug!(
                    "map_through_conversion_to_foreign: we found path after deep search: r {} <-> f {}",
                    self[rust_ty], self[f_idx].name,
                ),
                None => debug!(
                    "map to foreign failed, foreign_map {}\n conv_graph: {}",
                    self.ftypes_storage,
                    DisplayTypesConvGraph(&self.conv_graph),
                ),
            }
        }
        ret
    }

    pub(crate) fn find_foreigner_class_with_such_this_type<
        F: Fn(&TypeMap, &ForeignClassInfo) -> Option<Type>,
    >(
        &self,
        this_ty: &Type,
        get_this_type: F,
    ) -> Option<&ForeignClassInfo> {
        let this_name = normalize_type(this_ty);
        for fc in &self.foreign_classes {
            if let Some(this_type_for_method) = get_this_type(self, fc) {
                let cur_this = normalize_type(&this_type_for_method);
                if cur_this == this_name {
                    return Some(fc);
                }
            }
        }
        None
    }

    pub(crate) fn register_foreigner_class(&mut self, class: &ForeignClassInfo) {
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
        let name = normalize_type(ty);
        let idx = self.add_node(name.into(), || {
            RustTypeS::new_without_graph_idx(ty.clone(), name, src_id)
        });
        self.conv_graph[idx].clone()
    }

    pub(crate) fn find_or_alloc_rust_type_no_src_id(&mut self, ty: &Type) -> RustType {
        let name = normalize_type(ty);
        let idx = self.add_node(name.into(), || {
            RustTypeS::new_without_graph_idx(ty.clone(), name, SourceId::none())
        });
        self.conv_graph[idx].clone()
    }

    pub(crate) fn find_or_alloc_rust_type_that_implements(
        &mut self,
        ty: &Type,
        traits_name: &[&str],
        src_id: SourceId,
    ) -> RustType {
        let name = normalize_type(ty);
        let idx = self.add_node(name.into(), || {
            let mut ty = RustTypeS::new_without_graph_idx(ty.clone(), name, src_id);
            for tn in traits_name {
                ty.implements.insert((*tn).into());
            }
            ty
        });
        self.conv_graph[idx].clone()
    }

    pub(crate) fn find_or_alloc_rust_type_with_suffix(
        &mut self,
        ty: &Type,
        suffix: &str,
        src_id: SourceId,
    ) -> RustType {
        let name: SmolStr = RustTypeS::make_unique_typename(normalize_type(ty), suffix).into();
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
        let name = normalize_type(ty);
        self.rust_names_map
            .get(name)
            .map(|idx| self.conv_graph[*idx].clone())
    }

    pub(crate) fn take_not_merged_not_generic_rules(&mut self) -> Vec<TypeMapConvRuleInfo> {
        mem::take(&mut self.not_merged_data)
    }

    pub(crate) fn generic_rules(&self) -> &[Rc<TypeMapConvRuleInfo>] {
        &self.generic_rules
    }

    pub(crate) fn parse_foreign_typemap_macro(
        &mut self,
        src_id: SourceId,
        tts: TokenStream,
    ) -> Result<()> {
        let tmap_conv_rule: TypeMapConvRuleInfo =
            syn::parse2(tts).map_err(|err| DiagnosticError::from_syn_err(src_id, err))?;

        self.may_be_merge_conv_rule(src_id, tmap_conv_rule)
    }

    pub(in crate::typemap) fn invalidate_conv_cache(&mut self) {
        self.rust_to_foreign_cache.clear();
        self.rust_from_foreign_cache.clear();
    }
    pub(in crate::typemap) fn invalidate_conv_for_rust_type(&mut self, rti: RustTypeIdx) {
        self.rust_from_foreign_cache.remove(&rti);
        self.rust_to_foreign_cache.remove(&rti);
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

fn find_conversion_path(
    conv_graph: &TypesConvGraph,
    from: RustTypeIdx,
    to: RustTypeIdx,
    build_for_sp: SourceIdSpan,
) -> Result<Vec<EdgeIndex<TypeGraphIdx>>> {
    trace!(
        "find_conversion_path: search path {} -> {}",
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
                    .expect("Internal error: find_conversion_path no edge"),
            );
        }
        Ok(edges)
    } else {
        let mut err = DiagnosticError::new2(
            conv_graph[from].src_id_span(),
            format!("Can not find conversion from type '{}'", conv_graph[from]),
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
    let PossiblePath { new_edges, .. } = path;

    for (from, to, conv_rule) in new_edges {
        let from_idx = conv_map.add_node(from.normalized_name.clone(), || (*from).clone());
        let to_idx = conv_map.add_node(to.normalized_name.clone(), || (*to).clone());
        assert!(conv_map.conv_graph.find_edge(from_idx, to_idx).is_none());
        conv_map.conv_graph.add_edge(from_idx, to_idx, conv_rule);
    }
}

fn try_build_path(
    start_from_idx: RustTypeIdx,
    goal_to_idx: RustTypeIdx,
    build_for_sp: SourceIdSpan,
    conv_graph: &mut TypesConvGraph,
    rust_names_map: &RustTypeNameToGraphIdx,
    generic_edges: &[GenericTypeConv],
    max_steps: usize,
) -> Option<PossiblePath> {
    let goal_to = conv_graph[goal_to_idx].clone();
    debug!(
        "try_build_path: from '{}' to '{}', ty names len {}, graph nodes {}, edges {}",
        conv_graph[start_from_idx],
        goal_to,
        rust_names_map.len(),
        conv_graph.node_count(),
        conv_graph.edge_count()
    );
    let mut ty_graph = TypeGraphSnapshot::new(conv_graph, rust_names_map);

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
                write!(step_types, "{} | ", ty_graph.conv_graph[*from_ty]).unwrap();
            }
            debug!("try_build_path: step({}): {}", step, step_types);
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
                if let Some(ConversionResult {
                    to_ty,
                    to_ty_name,
                    subst_map,
                }) = edge.is_conv_possible(&from, Some(&goal_to), |name| {
                    ty_graph.find_type_by_name(name)
                }) {
                    if from.normalized_name == to_ty_name {
                        continue;
                    }
                    let to = ty_graph.node_for_ty(edge.src_id, (to_ty, to_ty_name));
                    ty_graph.add_edge(
                        *from_ty,
                        to,
                        TypeConvEdge {
                            code: edge.code_for_conversion(subst_map),
                            dependency: edge.dependency.clone(),
                        },
                    );

                    if petgraph::algo::has_path_connecting(
                        &*ty_graph.conv_graph,
                        to,
                        goal_to_idx,
                        None,
                    ) {
                        let path = find_conversion_path(
                            ty_graph.conv_graph,
                            start_from_idx,
                            goal_to_idx,
                            build_for_sp,
                        )
                        .expect("path must exists");
                        debug!("try_build_path: we found PATH({})!!!!", path.len());
                        if log_enabled!(log::Level::Debug) {
                            for edge in &path {
                                if let Some((from, to)) = ty_graph.conv_graph.edge_endpoints(*edge)
                                {
                                    debug!(
                                        "try_build_path: path: '{}' -> '{}'",
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
    use crate::{source_registry::SourceRegistry, types::SelfTypeDesc, SourceCode};

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
            &["SwigForeignClass"],
            SourceId::none(),
        );
        types_map.register_foreigner_class(&ForeignClassInfo {
            src_id: SourceId::none(),
            name: Ident::new("Foo", Span::call_site()),
            methods: vec![],
            self_desc: Some(SelfTypeDesc {
                self_type: foo_rt.ty.clone(),
                constructor_ret_type: foo_rt.ty.clone(),
            }),
            foreign_code: String::new(),
            doc_comments: vec![],
            derive_list: vec![],
        });

        let rc_refcell_foo_ty = types_map
            .find_or_alloc_rust_type(&parse_type! { &mut Rc<RefCell<Foo>> }, SourceId::none());
        let foo_ref_ty =
            types_map.find_or_alloc_rust_type(&parse_type! { &mut Foo }, SourceId::none());

        assert_eq!(
            r#"    let mut a1: & Rc < RefCell < Foo > > = a0;
    let mut a1: & RefCell < Foo > = & a1 ;
    let mut a1: RefMut < Foo > = a1 . borrow_mut () ;
    let mut a1: & mut Foo = & mut a1 ;
"#,
            types_map
                .convert_rust_types(
                    rc_refcell_foo_ty.to_idx(),
                    foo_ref_ty.to_idx(),
                    "a0",
                    "a1",
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
            r#"    let mut a1: Ref < Foo > = a0 . borrow () ;
    let mut a1: & Foo = & a1 ;
"#,
            types_map
                .convert_rust_types(
                    rc_refcell_foo_ty.to_idx(),
                    foo_ref_ty.to_idx(),
                    "a0",
                    "a1",
                    "jlong",
                    invalid_src_id_span(),
                )
                .expect("path from &RefCell<Foo> to &Foo NOT exists")
                .1
        );
    }
}
