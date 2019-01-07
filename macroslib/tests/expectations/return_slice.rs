foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32) -> Foo;
    method Foo::f(&self, _: i32, _: i32) -> i32;
});

foreigner_class!(class Boo {
    self_type Boo;

    constructor Boo::new(_: i32, _: usize) -> Boo;
    method Boo::f1(&self) -> &[u32];
    method Boo::f2(&self) -> &[Foo];
    method Boo::f3(&self) -> &[usize];
});
