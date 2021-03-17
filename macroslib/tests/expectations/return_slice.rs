foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32) -> Foo;
    fn Foo::f(&self, _: i32, _: i32) -> i32;
});

foreigner_class!(class Boo {
    self_type Boo;

    constructor Boo::new(_: i32, _: usize) -> Boo;
    fn Boo::f1(&self) -> &[u32];
    fn Boo::f2(&self) -> &[Foo];
    fn Boo::f3(&self) -> &[usize];
    fn Boo::f4(&self) -> &mut [usize];
});
