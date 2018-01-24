mod parsing;
pub mod utils;

use std::cell::RefCell;
use std::rc::Rc;
use std::collections::{HashMap, HashSet};
use std::{fmt, mem};

use syntex_syntax::ptr::P;
use syntex_syntax::symbol::Symbol;
use syntex_syntax::ast;
use syntex_syntax::parse::{PResult, ParseSess};
use syntex_pos::{Span, DUMMY_SP};
use syntex_errors::DiagnosticBuilder;

use petgraph::Graph;
use petgraph::graph::{EdgeIndex, NodeIndex};
use petgraph;
use petgraph::algo::dijkstra;
use petgraph::visit::EdgeRef;

use errors::fatal_error;
use my_ast::{check_if_smart_pointer_return_inner_type, get_trait_bounds, normalized_ty_string,
             parse_ty, GenericTypeConv, RustType};
use self::parsing::parse_types_conv_map;
use {ForeignEnumInfo, ForeignerClassInfo};

pub(crate) static TO_VAR_TEMPLATE: &'static str = "{to_var}";
pub(crate) static FROM_VAR_TEMPLATE: &'static str = "{from_var}";
pub(in types_conv_map) static TO_VAR_TYPE_TEMPLATE: &'static str = "{to_var_type}";
pub(in types_conv_map) static FUNCTION_RETURN_TYPE_TEMPLATE: &'static str = "{function_ret_type}";

#[derive(Debug, Clone)]
pub(crate) struct TypeConvEdge {
    code_template: Symbol,
    dependency: Rc<RefCell<Option<ast::Item>>>,
}

impl From<Symbol> for TypeConvEdge {
    fn from(x: Symbol) -> Self {
        TypeConvEdge {
            code_template: x,
            dependency: Rc::new(RefCell::new(None)),
        }
    }
}

impl TypeConvEdge {
    fn new(code_template: Symbol, dependency: Option<P<ast::Item>>) -> TypeConvEdge {
        TypeConvEdge {
            code_template,
            dependency: Rc::new(RefCell::new(dependency.map(|v| v.unwrap()))),
        }
    }
}

pub(crate) type TypeGraphIdx = u32;

pub(crate) type TypesConvGraph = Graph<RustType, TypeConvEdge, petgraph::Directed, TypeGraphIdx>;

#[derive(Debug)]
pub(crate) struct TypesConvMap {
    conv_graph: TypesConvGraph,
    foreign_names_map: HashMap<Symbol, NodeIndex<TypeGraphIdx>>,
    rust_names_map: HashMap<Symbol, NodeIndex<TypeGraphIdx>>,
    utils_code: Vec<P<ast::Item>>,
    generic_edges: Vec<GenericTypeConv>,
    rust_to_foreign_cache: HashMap<Symbol, Symbol>,
    foreign_classes: Vec<ForeignerClassInfo>,
    exported_enums: HashMap<Symbol, ForeignEnumInfo>,
    traits_usage_code: HashMap<Symbol, Symbol>,
}

struct DisplayTypesConvGraph<'a>(&'a TypesConvGraph);

impl<'a> fmt::Display for RustType {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.normalized_name)
    }
}

impl<'a> fmt::Display for DisplayTypesConvGraph<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let conv_graph = self.0;
        writeln!(f, "conversation graph begin").unwrap();
        for node in conv_graph.node_indices() {
            write!(f, "node {}: ", conv_graph[node].normalized_name)?;
            let mut edges = conv_graph
                .neighbors_directed(node, petgraph::Outgoing)
                .detach();
            while let Some((_, target)) = edges.next(conv_graph) {
                write!(f, "->{}, ", conv_graph[target].normalized_name)?;
            }
            write!(f, "\n")?;
        }
        writeln!(f, "conversation graph end").unwrap();
        Ok(())
    }
}

impl fmt::Display for TypesConvMap {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", DisplayTypesConvGraph(&self.conv_graph))
    }
}

pub(crate) struct ForeignTypeInfo {
    pub name: Symbol,
    pub correspoding_rust_type: RustType,
}

impl AsRef<ForeignTypeInfo> for ForeignTypeInfo {
    fn as_ref(&self) -> &ForeignTypeInfo {
        self
    }
}

pub(crate) trait ForeignMethodSignature {
    type FI: AsRef<ForeignTypeInfo>;
    fn output(&self) -> &ForeignTypeInfo;
    fn input(&self) -> &[Self::FI];
}

#[derive(Debug)]
struct PossibePath {
    tmp_graph: TypesConvGraph,
    path: Vec<EdgeIndex<TypeGraphIdx>>,
}

impl TypesConvMap {
    pub(crate) fn is_empty(&self) -> bool {
        self.conv_graph.node_count() == 0
    }

    pub(crate) fn take_utils_code(&mut self) -> Vec<P<ast::Item>> {
        let mut ret = Vec::new();
        ret.append(&mut self.utils_code);
        ret
    }

    pub(crate) fn merge<'a>(
        &mut self,
        sess: &'a ParseSess,
        id_of_code: &str,
        code: &str,
        target_pointer_width: usize,
    ) -> PResult<'a, ()> {
        debug!("merging {} with our rules", id_of_code);
        let mut was_traits_usage_code = HashMap::new();
        mem::swap(&mut was_traits_usage_code, &mut self.traits_usage_code);
        let mut new_data = parse_types_conv_map(
            sess,
            id_of_code,
            code,
            was_traits_usage_code,
            target_pointer_width,
        )?;
        mem::swap(&mut new_data.traits_usage_code, &mut self.traits_usage_code);

        fn get_graph_node_idx(
            node_new_data_idx: NodeIndex<TypeGraphIdx>,
            new_data: &TypesConvMap,
            data: &mut TypesConvMap,
        ) -> NodeIndex<TypeGraphIdx> {
            let node = &new_data.conv_graph[node_new_data_idx];
            debug!("handling new node {:?}", node);
            let node2 = node.clone();
            let data_rust_names_map = &mut data.rust_names_map;
            let data_conv_graph = &mut data.conv_graph;
            let idx = *data_rust_names_map
                .entry(node.normalized_name)
                .or_insert_with(|| data_conv_graph.add_node(node2));
            data_conv_graph[idx] = node.clone();

            if let Some((foreign_name, _)) = new_data
                .foreign_names_map
                .iter()
                .find(|x| *x.1 == node_new_data_idx)
            {
                data.foreign_names_map.insert(*foreign_name, idx);
            }
            idx
        }

        fn process_new_node(
            new_data_idx: NodeIndex<TypeGraphIdx>,
            new_data: &TypesConvMap,
            data: &mut TypesConvMap,
        ) {
            let self_src = get_graph_node_idx(new_data_idx, new_data, data);
            let mut edges = new_data
                .conv_graph
                .neighbors_directed(new_data_idx, petgraph::Outgoing)
                .detach();
            while let Some((edge, target)) = edges.next(&new_data.conv_graph) {
                let self_target = get_graph_node_idx(target, new_data, data);

                if let Some(existing_edge) = data.conv_graph.find_edge(self_src, self_target) {
                    warn!(
                        "Converstation {:?} from {:?} to {:?} ignored, we use {:?} instead",
                        new_data.conv_graph[edge],
                        self_src,
                        self_target,
                        data.conv_graph[existing_edge]
                    );
                } else {
                    data.conv_graph.add_edge(
                        self_src,
                        self_target,
                        new_data.conv_graph[edge].clone(),
                    );
                }
            }
        }

        for node in new_data.conv_graph.node_indices() {
            process_new_node(node, &new_data, self);
        }
        self.utils_code.append(&mut new_data.utils_code);
        //TODO: more intellect to process new generics
        self.generic_edges.append(&mut new_data.generic_edges);
        Ok(())
    }

    /// find correspoint to rust foreign type
    pub(crate) fn map_through_conversation_to_foreign(
        &mut self,
        rust_ty: &ast::Ty,
        direction: petgraph::Direction,
        build_for_sp: Span,
    ) -> Option<ForeignTypeInfo> {
        let norm_rust_typename = Symbol::intern(&normalized_ty_string(rust_ty));
        trace!("map foreign: {:?} {:?}", rust_ty, direction);
        if direction == petgraph::Direction::Outgoing {
            if let Some(foreign_name) = self.rust_to_foreign_cache.get(&norm_rust_typename) {
                if let Some(to) = self.foreign_names_map.get(foreign_name) {
                    let to = &self.conv_graph[*to];
                    return Some(ForeignTypeInfo {
                        name: *foreign_name,
                        correspoding_rust_type: to.clone(),
                    });
                }
            }
        }

        if let Some(from) = self.rust_names_map.get(&norm_rust_typename).cloned() {
            let sess = ParseSess::new();
            let find_path = |from, to| match find_conversation_path(
                &sess,
                &self.conv_graph,
                from,
                to,
                DUMMY_SP,
            ) {
                Ok(x) => Some(x),
                Err(mut err) => {
                    err.cancel();
                    None
                }
            };
            let mut min_path: Option<(usize, NodeIndex, Symbol)> = None;
            for (foreign_name, graph_idx) in &self.foreign_names_map {
                let path = match direction {
                    petgraph::Direction::Outgoing => find_path(from, *graph_idx),
                    petgraph::Direction::Incoming => find_path(*graph_idx, from),
                };
                if let Some(path) = path {
                    trace!(
                        "map foreign: we find path {} <-> {}",
                        foreign_name,
                        self.conv_graph[*graph_idx]
                    );
                    let cur = (path.len(), *graph_idx, *foreign_name);
                    min_path = Some(min_path.map_or(cur, |x| if cur.0 < x.0 { cur } else { x }));
                }
            }
            if let Some(min_path) = min_path {
                let node = &self.conv_graph[min_path.1];
                debug!("map foreign {:?} <-> {}", rust_ty, min_path.2);
                return Some(ForeignTypeInfo {
                    name: min_path.2,
                    correspoding_rust_type: node.clone(),
                });
            }
        }
        debug!(
            "No paths exists, may be we can create one for '{:?}'?",
            rust_ty
        );

        let mut new_foreign_types = HashSet::new();
        for edge in &self.generic_edges {
            if let Some(to_foreigner_hint) = edge.to_foreigner_hint {
                let trait_bounds: HashMap<Symbol, HashSet<Symbol>> = get_trait_bounds(
                    &edge.generic_params,
                ).into_iter()
                    .map(|v| (v.ty_param, v.trait_names))
                    .collect();
                for graph_idx in self.rust_names_map.values() {
                    for (ty_param, traits) in &trait_bounds {
                        let rust_ty = &self.conv_graph[*graph_idx];
                        if traits.is_subset(&rust_ty.implements) {
                            if let Some(class) =
                                self.find_foreigner_class_with_such_this_type(&rust_ty.ty)
                            {
                                let suffix = Symbol::intern(&to_foreigner_hint.as_str().replace(
                                    &*ty_param.as_str(),
                                    &*rust_ty.normalized_name.as_str(),
                                ));
                                let foreign_name = to_foreigner_hint
                                    .as_str()
                                    .replace(&*ty_param.as_str(), &*class.name.as_str());
                                new_foreign_types.insert((
                                    edge.to_ty.clone(),
                                    suffix,
                                    Symbol::intern(&foreign_name),
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
                "map foreign: add possible type {:?} {} <-> {}",
                ty, suffix, foreign_name
            );
            let not_uniq_name = Symbol::intern(&normalized_ty_string(&ty));
            let node = self.add_type(RustType::new(
                ty,
                make_unique_rust_typename(not_uniq_name, suffix),
            ));
            self.foreign_names_map.insert(foreign_name, node);
        }

        let from: RustType = rust_ty.clone().into();
        let mut possible_paths = Vec::<(PossibePath, Symbol, NodeIndex)>::new();
        for (foreign_name, graph_idx) in &self.foreign_names_map {
            let path = match direction {
                petgraph::Direction::Outgoing => {
                    self.try_build_path(&from, &self.conv_graph[*graph_idx], build_for_sp)
                }
                petgraph::Direction::Incoming => {
                    self.try_build_path(&self.conv_graph[*graph_idx], &from, build_for_sp)
                }
            };
            if let Some(path) = path {
                possible_paths.push((path, *foreign_name, *graph_idx));
            }
        }
        let ret = possible_paths
            .into_iter()
            .min_by_key(|pp| pp.0.path.len())
            .map(|(pp, foreign_name, graph_idx)| {
                merge_path_to_conv_map(pp, self);
                let node = &self.conv_graph[graph_idx];
                ForeignTypeInfo {
                    name: foreign_name,
                    correspoding_rust_type: node.clone(),
                }
            });
        if ret.is_none() {
            debug!(
                "map to foreign failed, foreign_map {:?}",
                self.foreign_names_map
            );
        }
        ret
    }

    fn find_rust_type<'a>(
        &self,
        sess: &'a ParseSess,
        ty: &RustType,
    ) -> PResult<'a, NodeIndex<TypeGraphIdx>> {
        self.rust_names_map
            .get(&ty.normalized_name)
            .cloned()
            .ok_or_else(|| {
                fatal_error(
                    sess,
                    ty.ty.span,
                    &format!("Can not find such rust type {}", ty.normalized_name),
                )
            })
    }

    pub(crate) fn convert_rust_types<'a>(
        &mut self,
        sess: &'a ParseSess,
        from: &RustType,
        to: &RustType,
        var_name: &str,
        function_ret_type: &str,
        build_for_sp: Span,
    ) -> PResult<'a, (Vec<P<ast::Item>>, String)> {
        let path = match self.find_path(sess, from, to, build_for_sp) {
            Ok(x) => x,
            Err(mut err) => {
                err.cancel();
                debug!("convert_rust_types: no path, trying to build it");
                self.build_path_if_possible(from, to, build_for_sp);
                self.find_path(sess, from, to, build_for_sp)?
            }
        };
        let mut ret_code = String::new();
        let mut code_deps = Vec::<P<ast::Item>>::new();

        for edge in path {
            let (_, target) = self.conv_graph.edge_endpoints(edge).unwrap();
            let target_type = self.conv_graph[target].normalized_name;
            let edge = &mut self.conv_graph[edge];
            if let Some(dep) = edge.dependency.borrow_mut().take() {
                code_deps.push(P(dep));
            }
            let code = apply_code_template(
                edge.code_template,
                var_name,
                var_name,
                &unpack_unique_typename(target_type).as_str(),
                function_ret_type,
            );
            ret_code.push_str(&code);
        }
        Ok((code_deps, ret_code))
    }

    fn find_path<'a>(
        &self,
        sess: &'a ParseSess,
        from: &RustType,
        to: &RustType,
        build_for_sp: Span,
    ) -> PResult<'a, Vec<EdgeIndex<TypeGraphIdx>>> {
        debug!(
            "find_path: begin {} -> {}",
            from.normalized_name, to.normalized_name
        );
        if from.normalized_name == to.normalized_name {
            return Ok(vec![]);
        }
        let err_add_note = |mut err: DiagnosticBuilder<'a>| -> DiagnosticBuilder<'a> {
            err.span_note(
                DUMMY_SP,
                &format!(
                    "Can not find conversation from {} to {}",
                    from.normalized_name, to.normalized_name
                ),
            );
            err
        };
        let from = self.find_rust_type(sess, from).map_err(&err_add_note)?;
        debug!("find_path: from {:?}", from);
        let to = self.find_rust_type(sess, to).map_err(&err_add_note)?;
        debug!("find_path: to {:?}", to);
        find_conversation_path(sess, &self.conv_graph, from, to, build_for_sp)
    }

    fn try_build_path(
        &self,
        start_from: &RustType,
        goal_to: &RustType,
        build_for_sp: Span,
    ) -> Option<PossibePath> {
        debug!(
            "try_build_path from {} to {}",
            start_from.normalized_name, goal_to.normalized_name
        );
        let mut possible_ways_graph = self.conv_graph.clone();
        let mut names_to_graph_map = self.rust_names_map.clone();

        let start_from_idx = get_graph_node(
            &mut possible_ways_graph,
            &mut names_to_graph_map,
            (*start_from).clone(),
        );

        let goal_to_idx = get_graph_node(
            &mut possible_ways_graph,
            &mut names_to_graph_map,
            (*goal_to).clone(),
        );

        let mut cur_step = HashSet::new();
        cur_step.insert(start_from_idx);
        let mut next_step = HashSet::new();

        const MAX_STEPS: usize = 7;
        for step in 0..MAX_STEPS {
            debug!("try_build_path do step {}", step);
            if cur_step.is_empty() {
                break;
            }
            {
                use std::fmt::Write;
                let mut step_types = String::new();
                for from_ty in &cur_step {
                    write!(
                        step_types,
                        "{:?} ",
                        possible_ways_graph[*from_ty].normalized_name
                    ).unwrap();
                }
                debug!("cur_step {}", step_types);
            }
            for from_ty in &cur_step {
                let from: RustType = possible_ways_graph[*from_ty].clone();
                for neighbor in possible_ways_graph.neighbors_directed(*from_ty, petgraph::Outgoing)
                {
                    next_step.insert(neighbor);
                }
                for edge in &self.generic_edges {
                    trace!("we check {:?} for {:?}", edge.from_ty, from);
                    if let Some(to_ty) = edge.is_conv_possible(&from, Some(goal_to), |name| {
                        names_to_graph_map
                            .get(&name)
                            .map(|i| &possible_ways_graph[*i])
                    }) {
                        if from.normalized_name == to_ty.normalized_name {
                            continue;
                        }
                        let to = get_graph_node(
                            &mut possible_ways_graph,
                            &mut names_to_graph_map,
                            to_ty,
                        );
                        possible_ways_graph.add_edge(
                            *from_ty,
                            to,
                            TypeConvEdge {
                                code_template: edge.code_template,
                                dependency: edge.dependency.clone(),
                            },
                        );

                        if petgraph::algo::has_path_connecting(
                            &possible_ways_graph,
                            to,
                            goal_to_idx,
                            None,
                        ) {
                            debug!("NEW ALGO: we found PATH!!!!");
                            let tmp_sess = ParseSess::new();
                            let path = find_conversation_path(
                                &tmp_sess,
                                &possible_ways_graph,
                                start_from_idx,
                                goal_to_idx,
                                build_for_sp,
                            ).unwrap();
                            for edge in &path {
                                if let Some((from, to)) = possible_ways_graph.edge_endpoints(*edge)
                                {
                                    debug!(
                                        "path: {} -> {}",
                                        possible_ways_graph[from].normalized_name,
                                        possible_ways_graph[to].normalized_name
                                    );
                                }
                            }
                            return Some(PossibePath {
                                tmp_graph: possible_ways_graph,
                                path,
                            });
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

    fn build_path_if_possible(
        &mut self,
        start_from: &RustType,
        goal_to: &RustType,
        build_for_sp: Span,
    ) {
        debug!(
            "build_path_if_possible begin {}\n {} -> {}",
            DisplayTypesConvGraph(&self.conv_graph),
            start_from.normalized_name,
            goal_to.normalized_name
        );
        if let Some(path) = self.try_build_path(start_from, goal_to, build_for_sp) {
            merge_path_to_conv_map(path, self);
        }
    }

    pub(crate) fn add_type(&mut self, ty: RustType) -> NodeIndex {
        let rust_names_map = &mut self.rust_names_map;
        let conv_graph = &mut self.conv_graph;
        *rust_names_map
            .entry(ty.normalized_name)
            .or_insert_with(|| conv_graph.add_node(ty))
    }

    pub(crate) fn add_foreign(&mut self, correspoding_rty: RustType, foreign_name: Symbol) {
        let idx = self.add_type(correspoding_rty);
        self.foreign_names_map.insert(foreign_name, idx);
    }

    pub(crate) fn find_foreign_type_info_by_name(
        &self,
        foreign_name: Symbol,
    ) -> Option<ForeignTypeInfo> {
        self.foreign_names_map
            .get(&foreign_name)
            .map(|x| ForeignTypeInfo {
                name: foreign_name,
                correspoding_rust_type: self.conv_graph[*x].clone(),
            })
    }

    pub(crate) fn cache_rust_to_foreign_conv(&mut self, from: &RustType, to: ForeignTypeInfo) {
        self.add_type(from.clone());
        let to_id = self.add_type(to.correspoding_rust_type);
        self.rust_to_foreign_cache
            .insert(from.normalized_name, to.name);
        self.foreign_names_map.insert(to.name, to_id);
    }

    pub(crate) fn convert_to_heap_pointer(from: &RustType, var_name: &str) -> (RustType, String) {
        for smart_pointer in &["Box", "Rc", "Arc"] {
            if let Some(inner_ty) =
                check_if_smart_pointer_return_inner_type(&from.ty, *smart_pointer)
            {
                let inner_ty: RustType = inner_ty.into();
                let inner_ty_str = normalized_ty_string(&inner_ty.ty);
                return (
                    inner_ty,
                    format!(
                        r#"
    let {var_name}: *const {inner_ty} = {smart_pointer}::into_raw({var_name});
"#,
                        var_name = var_name,
                        inner_ty = inner_ty_str,
                        smart_pointer = *smart_pointer,
                    ),
                );
            }
        }

        let inner_ty = from.clone();
        let inner_ty_str = normalized_ty_string(&inner_ty.ty);
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
            if check_if_smart_pointer_return_inner_type(&from.ty, *smart_pointer).is_some() {
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

    pub(crate) fn is_ty_implements(&self, ty: &ast::Ty, trait_name: Symbol) -> Option<RustType> {
        let ty_name = Symbol::intern(&normalized_ty_string(ty));
        if let Some(idx) = self.rust_names_map.get(&ty_name) {
            if self.conv_graph[*idx].implements.contains(&trait_name) {
                return Some(self.conv_graph[*idx].clone());
            }
        }
        if let ast::TyKind::Rptr(_, ref mut_ty) = ty.node {
            let ty_name = Symbol::intern(&normalized_ty_string(&mut_ty.ty));
            self.rust_names_map.get(&ty_name).map_or(None, |idx| {
                if self.conv_graph[*idx].implements.contains(&trait_name) {
                    Some(self.conv_graph[*idx].clone())
                } else {
                    None
                }
            })
        } else {
            None
        }
    }

    pub(crate) fn register_foreigner_class(&mut self, class: &ForeignerClassInfo) {
        self.foreign_classes.push(class.clone());
    }

    pub(crate) fn find_foreigner_class_with_such_this_type(
        &self,
        this_ty: &ast::Ty,
    ) -> Option<&ForeignerClassInfo> {
        let this_name = normalized_ty_string(this_ty);
        for fc in &self.foreign_classes {
            if let Some(this_type_for_method) = fc.this_type_for_method.as_ref() {
                let cur_this = normalized_ty_string(this_type_for_method);
                if cur_this == this_name {
                    return Some(fc);
                }
            }
        }
        None
    }

    pub(crate) fn find_foreigner_class_with_such_self_type(
        &self,
        may_be_self_ty: &ast::Ty,
    ) -> Option<&ForeignerClassInfo> {
        let type_name = if let ast::TyKind::Rptr(_, ref mut_ty) = may_be_self_ty.node {
            normalized_ty_string(&*mut_ty.ty)
        } else {
            normalized_ty_string(&may_be_self_ty)
        };
        trace!("find self type: possible name {:?}", type_name);
        for fc in &self.foreign_classes {
            let self_ty = format!("{}", fc.self_type);
            trace!("self_type {:?}", fc.self_type);
            if self_ty == type_name {
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
        debug!("TypesConvMap::add_conversation_rule {} -> {}", from, to);
        let from = get_graph_node(&mut self.conv_graph, &mut self.rust_names_map, from);
        let to = get_graph_node(&mut self.conv_graph, &mut self.rust_names_map, to);
        self.conv_graph.add_edge(from, to, rule);
    }

    pub(crate) fn register_exported_enum(&mut self, enum_info: &ForeignEnumInfo) {
        self.exported_enums
            .insert(enum_info.name, enum_info.clone());
    }

    pub(crate) fn is_this_exported_enum(&self, ty: &ast::Ty) -> Option<&ForeignEnumInfo> {
        let type_name = Symbol::intern(&normalized_ty_string(ty));
        self.exported_enums.get(&type_name)
    }

    pub(crate) fn is_generated_foreign_type(&self, foreign_name: Symbol) -> bool {
        if self.exported_enums.contains_key(&foreign_name) {
            return true;
        }
        self.foreign_classes
            .iter()
            .any(|fc| fc.name == foreign_name)
    }
}

fn find_conversation_path<'a>(
    sess: &'a ParseSess,
    conv_graph: &TypesConvGraph,
    from: NodeIndex<TypeGraphIdx>,
    to: NodeIndex<TypeGraphIdx>,
    build_for_sp: Span,
) -> PResult<'a, Vec<EdgeIndex<TypeGraphIdx>>> {
    debug!("search {:?} -> {:?}", conv_graph[from], conv_graph[to]);

    let paths_cost = dijkstra(conv_graph, from, Some(to), |_| 1);

    let mut path = Vec::new();
    let mut cur_node = to;

    while cur_node != from {
        let edge = match conv_graph
            .edges_directed(cur_node, petgraph::Direction::Incoming)
            .filter_map(|edge| paths_cost.get(&edge.source()).map(|v| (edge, v)))
            .min_by_key(|x| x.1)
        {
            Some((edge, _)) => edge,
            _ => {
                let mut err = fatal_error(
                    sess,
                    conv_graph[from].ty.span,
                    &format!("Can not find conversation from type '{}'", conv_graph[from]),
                );
                err.span_note(
                    conv_graph[to].ty.span,
                    &format!("to type '{}'", conv_graph[to]),
                );
                err.span_note(build_for_sp, "In this context");
                return Err(err);
            }
        };
        path.push(edge.id());
        cur_node = edge.source();
    }

    path.reverse();
    Ok(path)
}

pub(in types_conv_map) fn validate_code_template<'a>(
    sess: &'a ParseSess,
    sp: Span,
    code: &str,
) -> PResult<'a, ()> {
    if code.contains(TO_VAR_TEMPLATE) && code.contains(FROM_VAR_TEMPLATE)
        && code.contains(TO_VAR_TYPE_TEMPLATE)
    {
        Ok(())
    } else {
        Err(fatal_error(
            sess,
            sp,
            &format!(
                "{} not contains one of {}, {}, {}",
                code, TO_VAR_TEMPLATE, FROM_VAR_TEMPLATE, TO_VAR_TYPE_TEMPLATE
            ),
        ))
    }
}

pub(crate) fn make_unique_rust_typename_if_need(
    rust_typename: Symbol,
    suffix: Option<Symbol>,
) -> Symbol {
    match suffix {
        Some(s) => make_unique_rust_typename(rust_typename, s),
        None => rust_typename,
    }
}

pub(crate) fn make_unique_rust_typename(
    not_unique_name: Symbol,
    suffix_to_make_unique: Symbol,
) -> Symbol {
    Symbol::intern(&format!(
        "{}{}{}",
        not_unique_name, 0 as char, suffix_to_make_unique
    ))
}

pub(crate) fn unpack_unique_typename(name: Symbol) -> Symbol {
    let s = name.as_str();
    match s.find('\0') {
        Some(pos) => Symbol::intern(&(&s)[0..pos]),
        None => name,
    }
}

fn get_graph_node(
    graph: &mut TypesConvGraph,
    names_to_graph_map: &mut HashMap<Symbol, NodeIndex<TypeGraphIdx>>,
    rty: RustType,
) -> NodeIndex<TypeGraphIdx> {
    *names_to_graph_map
        .entry(rty.normalized_name)
        .or_insert_with(|| graph.add_node(rty))
}

fn merge_path_to_conv_map(path: PossibePath, conv_map: &mut TypesConvMap) {
    let PossibePath { tmp_graph, path } = path;
    for edge in path {
        if let Some((from, to)) = tmp_graph.edge_endpoints(edge) {
            let new_from = get_graph_node(
                &mut conv_map.conv_graph,
                &mut conv_map.rust_names_map,
                tmp_graph[from].clone(),
            );

            let new_to = get_graph_node(
                &mut conv_map.conv_graph,
                &mut conv_map.rust_names_map,
                tmp_graph[to].clone(),
            );
            conv_map
                .conv_graph
                .add_edge(new_from, new_to, tmp_graph[edge].clone());
        }
    }
}

impl Default for TypesConvMap {
    fn default() -> TypesConvMap {
        let generic_params = ast::Generics {
            lifetimes: vec![],
            ty_params: vec![
                ast::TyParam {
                    attrs: ast::ThinVec::new(),
                    ident: ast::Ident::from_str("T"),
                    id: ast::DUMMY_NODE_ID,
                    bounds: vec![],
                    default: None,
                    span: DUMMY_SP,
                },
            ],
            where_clause: ast::WhereClause {
                id: ast::DUMMY_NODE_ID,
                predicates: vec![],
            },
            span: DUMMY_SP,
        };
        let sess = ParseSess::new();
        let default_rules = vec![
            GenericTypeConv {
                from_ty: unwrap_presult!(parse_ty(&sess, DUMMY_SP, Symbol::intern("T"))),
                to_ty: unwrap_presult!(parse_ty(&sess, DUMMY_SP, Symbol::intern("&T"))),
                dependency: Rc::new(RefCell::new(None)),
                code_template: Symbol::intern("let mut {to_var}: {to_var_type} = &{from_var};"),
                generic_params: generic_params.clone(),
                to_foreigner_hint: None,
            },
            GenericTypeConv {
                from_ty: unwrap_presult!(parse_ty(&sess, DUMMY_SP, Symbol::intern("T"))),
                to_ty: unwrap_presult!(parse_ty(&sess, DUMMY_SP, Symbol::intern("&mut T"))),
                dependency: Rc::new(RefCell::new(None)),
                code_template: Symbol::intern("let mut {to_var}: {to_var_type} = &mut {from_var};"),
                generic_params: generic_params.clone(),
                to_foreigner_hint: None,
            },
            GenericTypeConv {
                from_ty: unwrap_presult!(parse_ty(&sess, DUMMY_SP, Symbol::intern("&mut T"))),
                to_ty: unwrap_presult!(parse_ty(&sess, DUMMY_SP, Symbol::intern("&T"))),
                dependency: Rc::new(RefCell::new(None)),
                code_template: Symbol::intern("let mut {to_var}: {to_var_type} = {from_var};"),
                generic_params: generic_params,
                to_foreigner_hint: None,
            },
        ];
        TypesConvMap {
            conv_graph: TypesConvGraph::new(),
            foreign_names_map: HashMap::new(),
            rust_names_map: HashMap::new(),
            utils_code: Vec::new(),
            generic_edges: default_rules,
            rust_to_foreign_cache: HashMap::new(),
            foreign_classes: Vec::new(),
            exported_enums: HashMap::new(),
            traits_usage_code: HashMap::new(),
        }
    }
}

fn apply_code_template(
    code_temlate: Symbol,
    to_name: &str,
    from_name: &str,
    to_typename: &str,
    func_ret_type: &str,
) -> String {
    let mut ret = String::new();
    ret.push_str("    ");
    ret.push_str(&code_temlate.as_str());
    ret.push('\n');
    ret.replace(TO_VAR_TEMPLATE, to_name)
        .replace(FROM_VAR_TEMPLATE, from_name)
        .replace(TO_VAR_TYPE_TEMPLATE, to_typename)
        .replace(FUNCTION_RETURN_TYPE_TEMPLATE, func_ret_type)
}

#[cfg(test)]
#[macro_use]
#[path = "../test_helper.rs"]
mod test_helper;

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use super::*;
    use self::test_helper::*;
    use syntex_pos::DUMMY_SP;
    use my_ast::parse_ty;

    #[test]
    fn test_merge() {
        logger_init();
        let mut types_map = TypesConvMap::default();
        let sess = ParseSess::new();
        unwrap_presult!(types_map.merge(
            &sess,
            "test_merge",
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
            64
        ));
        assert_eq!(
            {
                let mut set = HashSet::new();
                for k in types_map.foreign_names_map.keys() {
                    set.insert(*k);
                }
                set
            },
            {
                let mut set = HashSet::new();
                set.insert(Symbol::intern("boolean"));
                set.insert(Symbol::intern("int"));
                set
            }
        );
        assert_eq!(
            types_map
                .map_through_conversation_to_foreign(
                    &parse_ty(&sess, DUMMY_SP, Symbol::intern("i32")).unwrap(),
                    petgraph::Direction::Outgoing,
                    DUMMY_SP
                )
                .unwrap()
                .name,
            Symbol::intern("int")
        );
        assert_eq!(
            {
                let from = types_map.rust_names_map[&Symbol::intern("jboolean")];
                let to = types_map.rust_names_map[&Symbol::intern("bool")];
                let conv = &types_map.conv_graph[types_map.conv_graph.find_edge(from, to).unwrap()];
                conv.code_template
            },
            Symbol::intern("let mut {to_var}: {to_var_type} = {from_var}.swig_into(env);")
        );

        let from = types_map.rust_names_map[&Symbol::intern("jboolean")];
        let to = types_map.rust_names_map[&Symbol::intern("bool")];
        assert_eq!(
            find_conversation_path(&sess, &types_map.conv_graph, from, to, DUMMY_SP).unwrap(),
            vec![types_map.conv_graph.find_edge(from, to).unwrap()]
        );

        let from = types_map.rust_names_map[&Symbol::intern("bool")];
        let to = types_map.rust_names_map[&Symbol::intern("jboolean")];
        assert_eq!(
            find_conversation_path(&sess, &types_map.conv_graph, from, to, DUMMY_SP).unwrap(),
            vec![types_map.conv_graph.find_edge(from, to).unwrap()]
        );
        assert_eq!(
            types_map
                .utils_code
                .iter()
                .map(|v| v.ident.name.as_str().to_string())
                .collect::<Vec<_>>(),
            vec!["helper1", "SwigInto", "SwigFrom", "helper2", "helper3"]
        );
    }

    #[test]
    fn test_try_build_path() {
        logger_init();
        let mut types_map = TypesConvMap::default();
        let sess = ParseSess::new();
        unwrap_presult!(types_map.merge(
            &sess,
            "test_try_build_path",
            include_str!("../java_jni/jni-include.rs"),
            64
        ));

        let foo_rt: RustType = parse_ty(&sess, DUMMY_SP, Symbol::intern("Foo"))
            .unwrap()
            .into();
        let foo_rt = foo_rt.implements("SwigForeignClass");
        types_map.add_type(foo_rt.clone());
        types_map.register_foreigner_class(&ForeignerClassInfo {
            name: Symbol::intern("Foo"),
            methods: vec![],
            self_type: ast::Path {
                span: DUMMY_SP,
                segments: vec![],
            },
            this_type_for_method: Some(foo_rt.ty.clone()),
            foreigner_code: String::new(),
            constructor_ret_type: Some(foo_rt.ty.clone()),
            span: DUMMY_SP,
            doc_comments: vec![],
        });

        assert_eq!(
            types_map
                .convert_rust_types(
                    &sess,
                    &parse_ty(&sess, DUMMY_SP, Symbol::intern("&mut Rc<RefCell<Foo>>"))
                        .unwrap()
                        .into(),
                    &parse_ty(&sess, DUMMY_SP, Symbol::intern("&mut Foo"))
                        .unwrap()
                        .into(),
                    "a0",
                    "jlong",
                    DUMMY_SP
                )
                .expect("path from &mut Rc<RefCell<Foo>> to &mut Foo NOT exists")
                .1,
            r#"    let mut a0: &Rc<RefCell<Foo>> = a0;
    let mut a0: &RefCell<Foo> = a0.swig_deref();
    let mut a0: RefMut<Foo> = <RefMut<Foo>>::swig_from(a0, env);
    let mut a0: &mut Foo = a0.swig_deref_mut();
"#.to_string()
        );

        assert_eq!(
            types_map
                .convert_rust_types(
                    &sess,
                    &parse_ty(&sess, DUMMY_SP, Symbol::intern("&RefCell<Foo>"))
                        .unwrap()
                        .into(),
                    &parse_ty(&sess, DUMMY_SP, Symbol::intern("&Foo"))
                        .unwrap()
                        .into(),
                    "a0",
                    "jlong",
                    DUMMY_SP
                )
                .expect("path from &RefCell<Foo> to &Foo NOT exists")
                .1,
            r#"    let mut a0: Ref<Foo> = <Ref<Foo>>::swig_from(a0, env);
    let mut a0: &Foo = a0.swig_deref();
"#.to_string()
        );

        assert_eq!(
            types_map
                .map_through_conversation_to_foreign(
                    &parse_ty(&sess, DUMMY_SP, Symbol::intern("Vec<Foo>")).unwrap(),
                    petgraph::Direction::Outgoing,
                    DUMMY_SP
                )
                .unwrap()
                .name,
            Symbol::intern("Foo []")
        );

        assert!(
            types_map
                .try_build_path(
                    &parse_ty(&sess, DUMMY_SP, Symbol::intern("Vec<i32>"))
                        .unwrap()
                        .into(),
                    &parse_ty(&sess, DUMMY_SP, Symbol::intern("jlong"))
                        .unwrap()
                        .into(),
                    DUMMY_SP
                )
                .is_none()
        );
    }
}
