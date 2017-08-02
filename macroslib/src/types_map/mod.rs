pub(crate) mod norm_ty;
mod parsing;

use std::collections::HashMap;
use std::cell::RefCell;
use std::fmt;

use syntex_syntax::{parse, ast};
use syntex_syntax::symbol::Symbol;
use syntex_syntax::ast::{Ty, FunctionRetTy};
use syntex_syntax::ptr::P;
use syntex_syntax::parse::ParseSess;

use petgraph::Graph;
use petgraph::graph::{NodeIndex, EdgeIndex};
use petgraph;
use petgraph::algo::dijkstra;
use petgraph::visit::EdgeRef;

use ForeignerMethod;
use MethodVariant;
use self::norm_ty::normalized_ty_string;
use utils::{fatal_error, function_ret_ty_to_string};
use self::parsing::parse_types_map;

#[derive(Debug, Clone)]
pub(crate) struct FrontierTypeNode {
    foreign: Symbol,
    rust: Symbol,
}

#[derive(Debug, Clone)]
pub(crate) enum TypeConvNode {
    Frontier(FrontierTypeNode),
    Internal(Symbol),
}

impl fmt::Display for TypeConvNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            TypeConvNode::Frontier(ref x) => {
                write!(f, "F {} ({})", x.rust.as_str(), x.foreign.as_str())
            }
            TypeConvNode::Internal(ref x) => write!(f, "I {}", x.as_str()),
        }
    }
}

impl TypeConvNode {
    pub(crate) fn frontier_ref_unwrap(&self) -> &FrontierTypeNode {
        match *self {
            TypeConvNode::Frontier(ref x) => x,
            _ => panic!("Expect Frontier, got {:?}", self),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct TypeConvEdge {
    code: Symbol,
    dependency: RefCell<Option<ast::Item>>,
}

impl TypeConvEdge {
    fn new(code: Symbol, dependency: Option<ast::Item>) -> TypeConvEdge {
        TypeConvEdge {
            code: code,
            dependency: RefCell::new(dependency),
        }
    }
}

pub(crate) type TypeGraphIdx = u32;

pub(crate) type ForeignTypeConv = Graph<TypeConvNode,
                                        TypeConvEdge,
                                        petgraph::Directed,
                                        TypeGraphIdx>;


#[derive(Default)]
pub(crate) struct ForeignTypesMap {
    conv_graph: ForeignTypeConv,
    foreign_names_map: HashMap<Symbol, NodeIndex<TypeGraphIdx>>,
    rust_names_map: HashMap<Symbol, NodeIndex<TypeGraphIdx>>,
    utils_code: Vec<P<ast::Item>>,
}

struct DisplayConvGraph<'a>(&'a ForeignTypeConv);

impl<'a> fmt::Display for DisplayConvGraph<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let conv_graph = self.0;
        for node in conv_graph.node_indices() {
            write!(f, "node {}: ", conv_graph[node])?;
            let mut edges = conv_graph
                .neighbors_directed(node, petgraph::Outgoing)
                .detach();
            while let Some((_, target)) = edges.next(conv_graph) {
                write!(f, "->{}, ", conv_graph[target])?;
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

impl fmt::Display for ForeignTypesMap {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", DisplayConvGraph(&self.conv_graph))
    }
}

pub(crate) struct MethodSignatureWithForeignTypes {
    /// foreign type names
    pub foreign_input: Vec<Symbol>,
    pub rust_input: Vec<Symbol>,
    pub foreign_output: Symbol,
    pub rust_output: Symbol,
}

impl ForeignTypesMap {
    pub(crate) fn take_utils_code(&mut self) -> Vec<P<ast::Item>> {
        let mut ret = Vec::new();
        ret.extend(self.utils_code.drain(..));
        ret
    }

    fn find_and_add_node_if_not_exists(&mut self,
                                       cur_node: &TypeConvNode)
                                       -> NodeIndex<TypeGraphIdx> {
        //        println!("merge add {:?}", *cur_node);
        match *cur_node {
            TypeConvNode::Frontier(ref x) => {
                let mut foreign_names_map = &mut self.foreign_names_map;
                let conv_graph = &mut self.conv_graph;
                let idx = *foreign_names_map
                    .entry(x.foreign)
                    .or_insert_with(|| conv_graph.add_node(cur_node.clone()));
                if conv_graph[idx].frontier_ref_unwrap().rust != x.rust {
                    debug!("Mapping of types {} => {} ignored, used {} => {}",
                           x.foreign,
                           x.rust,
                           x.foreign,
                           conv_graph[idx].frontier_ref_unwrap().rust);
                }
                idx
            }
            TypeConvNode::Internal(x) => {
                let mut rust_names_map = &mut self.rust_names_map;
                let conv_graph = &mut self.conv_graph;

                *rust_names_map
                    .entry(x)
                    .or_insert_with(|| conv_graph.add_node(cur_node.clone()))
            }
        }
    }

    pub(crate) fn merge(&mut self,
                        parse_session: &parse::ParseSess,
                        name: &str,
                        code: &str)
                        -> Result<(), String> {
        let mut new_data = parse_types_map(parse_session, name, code);
        //        println!("merge: new_data {:?}", new_data.foreign_names_map);
        {
            let graph = &new_data.conv_graph;

            let first_node_idx = match graph.node_indices().nth(0) {
                Some(x) => x,
                None => return Ok(()),
            };


            let mut process_node = |src| {
                let self_src = self.find_and_add_node_if_not_exists(&graph[src]);
                //            println!("process node {:?}", graph[src]);
                let mut edges = new_data
                    .conv_graph
                    .neighbors_directed(src, petgraph::Outgoing)
                    .detach();
                while let Some((edge, target)) = edges.next(graph) {
                    let self_target = self.find_and_add_node_if_not_exists(&graph[target]);

                    if let Some(existing_edge) = self.conv_graph.find_edge(self_src, self_target) {
                        debug!("Converstation {:?} from {:?} to {:?} ignored, we use {:?} instead",
                               graph[edge],
                               self_src,
                               self_target,
                               self.conv_graph[existing_edge]);
                    } else {
                        self.conv_graph
                            .add_edge(self_src, self_target, graph[edge].clone());
                    }
                }
            };
            process_node(first_node_idx);
            for node in graph.node_indices() {
                process_node(node);
            }
        }
        self.utils_code.extend(new_data.utils_code.drain(..));

        Ok(())
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.conv_graph.node_count() == 0
    }

    fn from_foreign_type_name(&self, rust_ty: &Ty) -> Option<(Symbol, Symbol)> {
        let rust_typename = normalized_ty_string(rust_ty);
        let rust_typename = Symbol::intern(&rust_typename);

        let to = match self.rust_names_map.get(&rust_typename) {
            Some(x) => x,
            None => return None,
        };

        for node in self.foreign_names_map.values() {
            if petgraph::algo::has_path_connecting(&self.conv_graph, *node, *to, None) {
                let node = self.conv_graph[*node].frontier_ref_unwrap();
                return Some((node.foreign, node.rust));
            }
        }

        None
    }

    /// return foreign, rust
    pub(crate) fn to_foreign_type_name(&self, rust_typename: Symbol) -> Option<(Symbol, Symbol)> {
        //        println!("ftn: rust_typename {:?}", rust_typename);
        let from = match self.rust_names_map.get(&rust_typename) {
            Some(x) => x,
            None => return None,
        };
        //        println!("ftn from: {:?}", self.conv_graph[*from]);
        for node in self.foreign_names_map.values() {
            //            println!("ftn node: {:?}", self.conv_graph[*node]);
            if petgraph::algo::has_path_connecting(&self.conv_graph, *from, *node, None) {
                let node = self.conv_graph[*node].frontier_ref_unwrap();
                return Some((node.foreign, node.rust));
            }
        }

        None
    }

    pub(crate) fn foreign_return_type(&self, ret_ty: &FunctionRetTy) -> Option<(Symbol, Symbol)> {
        let rust_typename = match *ret_ty {
            FunctionRetTy::Default(_) => "()".to_string(),
            FunctionRetTy::Ty(ref t) => normalized_ty_string(&*t),
        };
        let rust_typename = Symbol::intern(&rust_typename);
        self.to_foreign_type_name(rust_typename)
    }

    pub(crate) fn resolve_types(&self,
                                parse_sess: &ParseSess,
                                method: &ForeignerMethod)
                                -> Result<MethodSignatureWithForeignTypes, String> {
        //skip self argument
        let skip_n = match method.variant {
            MethodVariant::Method => 1,
            _ => 0,
        };

        let mut foreign_input = Vec::with_capacity(method.fn_decl.inputs.len());
        let mut rust_input = Vec::with_capacity(method.fn_decl.inputs.len());
        for arg in method.fn_decl.inputs.iter().skip(skip_n) {
            let (f_name, r_name) = self.from_foreign_type_name(&*arg.ty).unwrap_or_else(|| {
                eprintln!("{}: {}: Conversation graph: {}", file!(), line!(), self);
                fatal_error(parse_sess,
                            &arg.ty.span,
                            &format!("Do not know conversation from \
                                          rust type to foreign for {:?}",
                                     arg.ty));
            });
            foreign_input.push(f_name);
            rust_input.push(r_name);
        }
        let (foreign_output, rust_output) = match method.variant {
            MethodVariant::Constructor => (Symbol::intern(""), Symbol::intern("")),
            _ => {
                self.foreign_return_type(&method.fn_decl.output)
                    .unwrap_or_else(|| {
                        eprintln!("Conversation graph: {}", self);
                        fatal_error(parse_sess,
                                    &method.fn_decl.output.span(),
                                    &format!("No known conversation from {} to any foreign type",
                                             function_ret_ty_to_string(&method.fn_decl.output)));
                    })
            }
        };

        Ok(MethodSignatureWithForeignTypes {
               foreign_input,
               rust_input,
               foreign_output,
               rust_output,
           })
    }

    pub(crate) fn convert_foreign_input<GI>(&mut self,
                                            parse_sess: &ParseSess,
                                            method: &ForeignerMethod,
                                            method_sign: &MethodSignatureWithForeignTypes,
                                            arg_names: GI)
                                            -> Result<(Vec<ast::Item>, String), String>
        where GI: Iterator<Item = String>
    {
        let mut code_deps = Vec::<ast::Item>::new();
        let mut ret_code = String::new();

        //skip self
        let skip_n = match method.variant {
            MethodVariant::Method => 1,
            _ => 0,
        };
        for ((to_type, from_typename), arg_name) in
            method
                .fn_decl
                .inputs
                .iter()
                .skip(skip_n)
                .zip(method_sign.rust_input.iter())
                .zip(arg_names) {

            let to_typename = normalized_ty_string(&*to_type.ty);
            let to_typename = Symbol::intern(&to_typename);
            let to = self.rust_names_map
                .get(&to_typename)
                .ok_or_else(|| format!("Unknown rust type: {}", to_typename))?;
            let from = self.rust_names_map
                .get(&*from_typename)
                .ok_or_else(|| format!("Unknown rust type: {}", *from_typename))?;
            let conv_path = find_conversation_path(parse_sess, &self.conv_graph, *from, *to);

            for conv in &conv_path {
                let edge = &mut self.conv_graph[*conv];
                if let Some(dep) = edge.dependency.borrow_mut().take() {
                    code_deps.push(dep);
                }
                let code = format!("{}\n", edge.code.as_str())
                    .replace("{to_var}", &*arg_name)
                    .replace("{from_var}", &*arg_name);
                ret_code.push_str(&code);
            }
        }

        Ok((code_deps, ret_code))
    }

    pub(crate) fn convert_output_to_foreign(&mut self,
                                            parse_sess: &ParseSess,
                                            method: &ForeignerMethod,
                                            method_sign: &MethodSignatureWithForeignTypes,
                                            ret_var_name: &str)
                                            -> Result<(Vec<ast::Item>, String), String> {
        let from_typename =
            Symbol::intern(&match method.fn_decl.output {
                               FunctionRetTy::Default(_) => "()".to_string(),
                               FunctionRetTy::Ty(ref t) => normalized_ty_string(&*t),
                           });
        let from = self.rust_names_map
            .get(&from_typename)
            .ok_or_else(|| {
                            format!("{}:{} Unknown rust type: {}",
                                    file!(),
                                    line!(),
                                    from_typename)
                        })?;
        let to_typename = method_sign.rust_output;
        let to = self.rust_names_map
            .get(&to_typename)
            .ok_or_else(|| {
                            format!("Conversation graph: {}\n{}:{} Unknown rust type: {}",
                                    self,
                                    file!(),
                                    line!(),
                                    to_typename)
                        })?;

        let conv_path = find_conversation_path(parse_sess, &self.conv_graph, *from, *to);

        let mut code_deps = Vec::<ast::Item>::new();
        let mut ret_code = String::new();

        for conv in &conv_path {
            let edge = &mut self.conv_graph[*conv];
            if let Some(dep) = edge.dependency.borrow_mut().take() {
                code_deps.push(dep);
            }
            let code = format!("{}\n", edge.code.as_str())
                .replace("{to_var}", ret_var_name)
                .replace("{from_var}", ret_var_name);
            ret_code.push_str(&code);
        }
        Ok((code_deps, ret_code))
    }

    pub(crate) fn add_conversation(&mut self,
                                   from_rust_type: Symbol,
                                   to_rust_type: Symbol,
                                   to_foreign_type: Option<Symbol>,
                                   code: String)
                                   -> Result<(), String> {
        let from = self.find_and_add_node_if_not_exists(&TypeConvNode::Internal(from_rust_type));
        let internal_to =
            self.find_and_add_node_if_not_exists(&TypeConvNode::Internal(to_rust_type));
        self.conv_graph.add_edge(from,
                                 internal_to,
                                 TypeConvEdge::new(Symbol::intern(&code), None));
        if let Some(to_foreign_type) = to_foreign_type {
            let to =
                self.find_and_add_node_if_not_exists(&TypeConvNode::Frontier(FrontierTypeNode {
                    foreign:
                    to_foreign_type,
                    rust: to_rust_type,
                }));
            self.conv_graph
                .add_edge(internal_to, to, TypeConvEdge::new(Symbol::intern(""), None));
        }
        Ok(())
    }
}

pub(crate) fn make_unique_rust_typename(not_unique_name: Symbol,
                                        suffix_to_make_unique: Symbol)
                                        -> Symbol {
    Symbol::intern(&format!("{}{}{}", not_unique_name, 0 as char, suffix_to_make_unique))
}

pub(crate) fn unpack_unique_typename(name: Symbol) -> Symbol {
    let s = name.as_str();
    match s.find('\0') {
        Some(pos) => Symbol::intern(&(&s)[0..pos]),
        None => name,
    }
}

fn find_conversation_path(_: &ParseSess,
                          conv_graph: &ForeignTypeConv,
                          from: NodeIndex<TypeGraphIdx>,
                          to: NodeIndex<TypeGraphIdx>)
                          -> Vec<EdgeIndex<TypeGraphIdx>> {
    debug!("search {:?} -> {:?}", conv_graph[from], conv_graph[to]);

    let paths_cost = dijkstra(conv_graph, from, Some(to), |_| 1);

    let mut path = Vec::new();
    let mut cur_node = to;

    while cur_node != from {
        let (edge, _) = conv_graph
            .edges_directed(cur_node, petgraph::Direction::Incoming)
            .filter_map(|edge| paths_cost.get(&edge.source()).map(|v| (edge, v)))
            .min_by_key(|x| x.1)
            .unwrap_or_else(|| {
                                eprintln!("Conversation graph: {}", DisplayConvGraph(conv_graph));
                                panic!("Can not find conversation from {} to {}",
                                       conv_graph[from],
                                       conv_graph[to])
                            });

        path.push(edge.id());
        cur_node = edge.source();
    }

    path.reverse();
    path
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use super::*;


    #[test]
    fn test_foreign_types_map_merge() {
        let mut types_map = ForeignTypesMap::default();
        let parse_sess = ParseSess::new();
        types_map
            .merge(
                &parse_sess,
                "test",
                r#"
mod foreign_types_map {
    #![foreigner_type="boolean"]
    #![rust_type="jboolean"]
    #![foreigner_type="int"]
    #![rust_type="jint"]
}

fn helper1() {
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
            )
            .unwrap();
        assert_eq!({
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
                   });
        assert_eq!(types_map
                       .to_foreign_type_name(Symbol::intern("i32"))
                       .unwrap()
                       .0,
                   Symbol::intern("int"));
        assert_eq!({
                       let from = types_map.rust_names_map[&Symbol::intern("jboolean")];
                       let to = types_map.rust_names_map[&Symbol::intern("bool")];
                       let conv =
                           &types_map.conv_graph[types_map.conv_graph.find_edge(from, to).unwrap()];
                       conv.code
                   },
                   Symbol::intern("\nlet {to_var}: bool = {from_var}.swig_into(env);\n"));
        let parse_sess = ParseSess::new();

        let from = types_map.rust_names_map[&Symbol::intern("jboolean")];
        let to = types_map.rust_names_map[&Symbol::intern("bool")];
        assert_eq!(find_conversation_path(&parse_sess, &types_map.conv_graph, from, to),
                   vec![types_map.conv_graph.find_edge(from, to).unwrap()]);

        let from = types_map.rust_names_map[&Symbol::intern("bool")];
        let to = types_map.rust_names_map[&Symbol::intern("jboolean")];
        assert_eq!(find_conversation_path(&parse_sess, &types_map.conv_graph, from, to),
                   vec![types_map.conv_graph.find_edge(from, to).unwrap()]);
        assert_eq!(types_map
                       .utils_code
                       .iter()
                       .map(|v| v.ident.name.as_str().to_string())
                       .collect::<Vec<_>>(),
                   vec!["helper1", "helper2", "helper3"]);
    }
}
