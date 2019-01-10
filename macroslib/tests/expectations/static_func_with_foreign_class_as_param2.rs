foreigner_class!(class Boo {
    self_type Boo;
    constructor boo_init() -> Rc<RefCell<Boo>>;
    method Boo::f1(&self);
});
foreigner_class!(class Foo {
    static_method static_foo(_: &Boo);
});
