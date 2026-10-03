// Register Boo in the type graph before its foreign class is registered.
foreign_typemap!(
    ($p:r_type) Boo => Option<Boo> {
        $out = Some($p);
    };
);

foreign_class!(class Foo {
    self_type Foo;
    constructor Foo::new() -> Foo;
    fn Foo::consume(values: &[Boo]);
});

foreign_class!(class Boo {
    self_type Boo;
    constructor Boo::new() -> Boo;
});
