foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32, _: &str) -> Foo;
    method Foo::f(&self, _: i32, _: i32) -> i32;  alias calcF;
});

foreigner_class!(class Boo {
    self_type Moo<'a>;
    constructor Moo::new(_: &str) -> Moo<'a>;
    method Moo::f(&self, _: &str) -> String;
});

foreigner_class!(class TestPassObjectsAsParams {
    self_type TestPassObjectsAsParams;
    constructor TestPassObjectsAsParams::default() -> TestPassObjectsAsParams;
    method TestPassObjectsAsParams::f1(&self, _: &Foo);
    method TestPassObjectsAsParams::f2(&self, _: Foo);
    method TestPassObjectsAsParams::f3(&self, _: &mut Foo);
    method TestPassObjectsAsParams::f3_a(&self, _: &mut Moo<'a>);
    static_method TestPassObjectsAsParams::f4(_: &Foo);
    static_method TestPassObjectsAsParams::f5(_: Foo);
});
