foreign_class!(class ArcNode {
    self_type ArcNode;
    constructor ArcNode::new() -> Arc<ArcNode>;
    fn ArcNode::children(&self) -> Vec<Arc<ArcNode>>;
});

foreign_class!(class RcNode {
    self_type RcNode;
    constructor RcNode::new() -> Rc<RefCell<RcNode>>;
});

foreign_class!(class VecHost {
    self_type VecHost;
    constructor VecHost::new() -> VecHost;
    fn VecHost::arcs(&self) -> Vec<Arc<ArcNode>>;
    fn VecHost::rcs(&self) -> Vec<Rc<RefCell<RcNode>>>;
    fn VecHost::echo_arcs(values: Vec<Arc<ArcNode>>) -> Vec<Arc<ArcNode>>;
    fn VecHost::echo_rcs(values: Vec<Rc<RefCell<RcNode>>>) -> Vec<Rc<RefCell<RcNode>>>;
});
