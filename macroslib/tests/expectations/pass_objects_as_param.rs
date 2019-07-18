foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32, _: &str) -> Rc<RefCell<Foo>>;
    fn Foo::f(&self, _: i32, _: i32) -> i32;
});

foreigner_class!(class TestPassObjectsAsParams {
    self_type TestPassObjectsAsParams;
    constructor TestPassObjectsAsParams::default() -> TestPassObjectsAsParams;
    fn TestPassObjectsAsParams::f1(&self, _: &RefCell<Foo>);
    fn TestPassObjectsAsParams::f2(&self, _: Rc<RefCell<Foo>>);
    fn TestPassObjectsAsParams::f3(&self, _: &mut RefCell<Foo>);
    fn TestPassObjectsAsParams::f4(&self, _: &Foo);
    fn TestPassObjectsAsParams::f5(&self, _: &mut Foo);
});
