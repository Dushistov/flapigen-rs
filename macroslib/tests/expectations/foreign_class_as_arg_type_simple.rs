foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32) -> Foo;
    method Foo::f(&self, _: i32, _: i32) -> i32;
});

foreigner_class!(class Boo {
    self_type Boo;

    constructor Boo::new(_: i32, _: usize) -> Boo;
    constructor Boo::with_foo(f: Foo) -> Boo;
    method Boo::f(&self, foo: Foo) -> usize;
    static_method Boo::f2(_: f64, foo: Foo) -> i32;
});
