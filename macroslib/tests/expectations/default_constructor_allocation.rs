use std::rc::Rc;
use std::sync::Arc;

foreign_class!(class DefaultOwned {
    self_type DefaultOwned;
    constructor DefaultOwned::default() -> DefaultOwned;
});

foreign_class!(class CustomOwned {
    self_type CustomOwned;
    constructor CustomOwned::new() -> CustomOwned;
});

foreign_class!(class FactoryOwned {
    self_type FactoryOwned;
    constructor Factory::default() -> FactoryOwned;
});

foreign_class!(class ArcOwned {
    self_type ArcOwned;
    constructor Arc::default() -> Arc<ArcOwned>;
});

foreign_class!(class RcOwned {
    self_type RcOwned;
    constructor Rc::default() -> Rc<RcOwned>;
});

foreign_class!(class ArcShorthand {
    self_type ArcShorthand;
    constructor ArcShorthand::default() -> Arc<ArcShorthand>;
});

foreign_class!(class RcShorthand {
    self_type RcShorthand;
    constructor RcShorthand::default() -> Rc<RcShorthand>;
});
