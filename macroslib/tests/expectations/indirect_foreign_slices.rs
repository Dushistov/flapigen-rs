foreign_class!(class ArcNode {
    self_type ArcNode;
    constructor ArcNode::new() -> Arc<ArcNode>;
    fn ArcNode::children(&self) -> &[Arc<ArcNode>];
    fn ArcNode::value(&self) -> i32;
});

foreign_class!(class RcNode {
    self_type RcNode;
    constructor RcNode::new() -> Rc<RefCell<RcNode>>;
    fn RcNode::value(&self) -> i32;
});

foreign_class!(class OtherArcNode {
    self_type OtherArcNode;
    constructor OtherArcNode::new() -> Arc<OtherArcNode>;
});

foreign_class!(class IndirectSliceHost {
    self_type IndirectSliceHost;
    constructor IndirectSliceHost::new() -> IndirectSliceHost;
    fn IndirectSliceHost::arcs(&self) -> &[Arc<ArcNode>];
    fn IndirectSliceHost::rcs(&self) -> &[Rc<RefCell<RcNode>>];
    fn IndirectSliceHost::other_arcs(&self) -> &[Arc<OtherArcNode>];
    fn IndirectSliceHost::take_arcs(values: &[Arc<ArcNode>]) -> usize;
    fn IndirectSliceHost::take_rcs(values: &[Rc<RefCell<RcNode>>]) -> usize;
});
