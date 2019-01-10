foreigner_class!(class Boo {
    self_type Boo;
    constructor boo_init() -> Rc<RefCell<Boo>>;
});
foreigner_class!(class Foo {
    static_method static_foo(_: &Boo);
});
