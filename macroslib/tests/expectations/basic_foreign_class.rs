use std::cell::{Ref, RefCell};
use std::rc::Rc;
use std::sync::{Arc, Mutex, MutexGuard};

pub struct BasicBox {
    value: i32,
}

impl BasicBox {
    fn new(value: i32) -> Self {
        Self { value }
    }

    fn value(&self) -> i32 {
        self.value
    }
}

foreign_class!(class BasicBox {
    self_type BasicBox;
    constructor BasicBox::new(value: i32) -> BasicBox;
    fn BasicBox::value(&self) -> i32;
});

pub struct BasicRc {
    value: i32,
}

impl BasicRc {
    fn new(value: i32) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self { value }))
    }

    fn value(&self) -> i32 {
        self.value
    }
}

foreign_class!(class BasicRc {
    self_type BasicRc;
    constructor BasicRc::new(value: i32) -> Rc<RefCell<BasicRc>>;
    fn BasicRc::value(&self) -> i32;
});

pub struct BasicArc {
    value: i32,
}

impl BasicArc {
    fn new(value: i32) -> Arc<Mutex<Self>> {
        Arc::new(Mutex::new(Self { value }))
    }

    fn value(&self) -> i32 {
        self.value
    }
}

foreign_class!(class BasicArc {
    self_type BasicArc;
    constructor BasicArc::new(value: i32) -> Arc<Mutex<BasicArc>>;
    fn BasicArc::value(&self) -> i32;
});
