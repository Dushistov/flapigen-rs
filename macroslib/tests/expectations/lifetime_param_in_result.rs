foreigner_class!(class Foo {
    self_type Foo<'a>;
    constructor new<'a>() -> Rc<RefCell<Foo<'a>>>;
    method Foo::f(&self, _: i32);
});
