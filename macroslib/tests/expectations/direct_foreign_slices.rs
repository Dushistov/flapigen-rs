foreign_class!(class BoxNode {
    self_type BoxNode;
    constructor BoxNode::new() -> BoxNode;
    fn BoxNode::value(&self) -> i32;
});

foreign_class!(class SliceHost {
    self_type SliceHost;
    constructor SliceHost::new() -> SliceHost;
    fn SliceHost::nodes(&self) -> &[BoxNode];
    fn SliceHost::nodes_mut(&mut self) -> &mut [BoxNode];
    fn SliceHost::sum_nodes(values: &[BoxNode]) -> i32;
    fn SliceHost::update_nodes(values: &mut [BoxNode]);
});
