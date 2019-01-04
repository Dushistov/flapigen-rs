use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt, mem,
    rc::Rc,
};

use log::{debug, trace};
use petgraph::{
    algo::dijkstra,
    graph::{EdgeIndex, NodeIndex},
    visit::EdgeRef,
    Graph,
};
use proc_macro2::{Span, TokenStream};
use syn::{spanned::Spanned, Ident};

use crate::{
    ast::{GenericTypeConv, RustType},
    error::{DiagnosticError, Result},
};

mod parse;

pub(crate) static TO_VAR_TEMPLATE: &str = "{to_var}";
pub(crate) static FROM_VAR_TEMPLATE: &str = "{from_var}";
pub(in crate::typemap) static TO_VAR_TYPE_TEMPLATE: &str = "{to_var_type}";
pub(in crate::typemap) static FUNCTION_RETURN_TYPE_TEMPLATE: &str = "{function_ret_type}";

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

#[derive(Debug)]
struct TypeMap {
    conv_graph: TypesConvGraph,
    foreign_names_map: HashMap<String, NodeIndex<TypeGraphIdx>>,
    rust_names_map: HashMap<String, NodeIndex<TypeGraphIdx>>,
    utils_code: Vec<TokenStream>,
    generic_edges: Vec<GenericTypeConv>,
    rust_to_foreign_cache: HashMap<String, String>,
    //    foreign_classes: Vec<ForeignerClassInfo>,
    //    exported_enums: HashMap<String, ForeignEnumInfo>,
    traits_usage_code: HashMap<Ident, String>,
}

struct DisplayTypesConvGraph<'a>(&'a TypesConvGraph);

impl<'a> fmt::Display for DisplayTypesConvGraph<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> std::result::Result<(), fmt::Error> {
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

#[derive(Debug)]
struct PossibePath {
    tmp_graph: TypesConvGraph,
    path: Vec<EdgeIndex<TypeGraphIdx>>,
}

impl TypeMap {
    pub(crate) fn add_type(&mut self, ty: RustType) -> NodeIndex {
        let rust_names_map = &mut self.rust_names_map;
        let conv_graph = &mut self.conv_graph;
        *rust_names_map
            .entry(ty.normalized_name.clone())
            .or_insert_with(|| conv_graph.add_node(ty))
    }

    pub(crate) fn convert_rust_types(
        &mut self,
        from: &RustType,
        to: &RustType,
        var_name: &str,
        function_ret_type: &str,
        build_for_sp: Span,
    ) -> Result<(Vec<TokenStream>, String)> {
        let path = match self.find_path(from, to, build_for_sp) {
            Ok(x) => x,
            Err(mut err) => {
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
        build_for_sp: Span,
    ) -> Result<Vec<EdgeIndex<TypeGraphIdx>>> {
        debug!(
            "find_path: begin {} -> {}",
            from.normalized_name, to.normalized_name
        );
        if from.normalized_name == to.normalized_name {
            return Ok(vec![]);
        }
        let err_add_note = |mut err: DiagnosticError| -> DiagnosticError {
            err.span_note(
                Span::call_site(),
                &format!(
                    "Can not find conversation from {} to {}",
                    from.normalized_name, to.normalized_name
                ),
            );
            err
        };
        let from = self.find_rust_type(from).map_err(&err_add_note)?;
        let to = self.find_rust_type(to).map_err(&err_add_note)?;
        find_conversation_path(&self.conv_graph, from, to, build_for_sp)
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
                    )
                    .unwrap();
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
                    trace!(
                        "we check edge({:?} -> {:?}) for {:?}",
                        edge.from_ty,
                        edge.to_ty,
                        from
                    );
                    if let Some(to_ty) = edge.is_conv_possible(&from, Some(goal_to), |name| {
                        names_to_graph_map
                            .get(name)
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
                                code_template: edge.code_template.clone(),
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
                            let path = find_conversation_path(
                                &possible_ways_graph,
                                start_from_idx,
                                goal_to_idx,
                                build_for_sp,
                            )
                            .unwrap();
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

    fn find_rust_type(&self, ty: &RustType) -> Result<NodeIndex<TypeGraphIdx>> {
        self.rust_names_map
            .get(&ty.normalized_name)
            .cloned()
            .ok_or_else(|| {
                DiagnosticError::new(
                    ty.ty.span(),
                    format!("Can not find such rust type {}", ty.normalized_name),
                )
            })
    }
}

pub(in crate::typemap) fn validate_code_template(sp: Span, code: &str) -> Result<()> {
    if code.contains(TO_VAR_TEMPLATE)
        && code.contains(FROM_VAR_TEMPLATE)
        && code.contains(TO_VAR_TYPE_TEMPLATE)
    {
        Ok(())
    } else {
        Err(DiagnosticError::new(
            sp,
            format!(
                "{} not contains one of {}, {}, {}",
                code, TO_VAR_TEMPLATE, FROM_VAR_TEMPLATE, TO_VAR_TYPE_TEMPLATE
            ),
        ))
    }
}

pub(crate) fn make_unique_rust_typename(
    not_unique_name: String,
    suffix_to_make_unique: String,
) -> String {
    format!("{}{}{}", not_unique_name, 0 as char, suffix_to_make_unique)
}

pub(crate) fn make_unique_rust_typename_if_need(
    rust_typename: String,
    suffix: Option<String>,
) -> String {
    match suffix {
        Some(s) => make_unique_rust_typename(rust_typename, s),
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
    from: NodeIndex<TypeGraphIdx>,
    to: NodeIndex<TypeGraphIdx>,
    build_for_sp: Span,
) -> Result<Vec<EdgeIndex<TypeGraphIdx>>> {
    trace!(
        "find_conversation_path: begin {:?} -> {:?}",
        conv_graph[from],
        conv_graph[to]
    );

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
                let mut err = DiagnosticError::new(
                    conv_graph[from].ty.span(),
                    format!("Can not find conversation from type '{}'", conv_graph[from]),
                );
                err.span_note(
                    conv_graph[to].ty.span(),
                    format!("to type '{}'", conv_graph[to]),
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

fn merge_path_to_conv_map(path: PossibePath, conv_map: &mut TypeMap) {
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

fn get_graph_node(
    graph: &mut TypesConvGraph,
    names_to_graph_map: &mut HashMap<String, NodeIndex<TypeGraphIdx>>,
    rty: RustType,
) -> NodeIndex<TypeGraphIdx> {
    *names_to_graph_map
        .entry(rty.normalized_name.clone())
        .or_insert_with(|| graph.add_node(rty))
}
