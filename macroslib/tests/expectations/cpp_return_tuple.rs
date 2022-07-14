foreigner_class!(class One {
    self_type One;
    private constructor = empty;
});

foreigner_class!(class Two {
    self_type Two;
    private constructor = empty;
});

foreigner_class!(class Foo {
    self_type Foo;
    private constructor = empty;
    fn Foo::f(&self) -> (One, Two);
    fn Foo::g(&self) -> (i32, i32);
    fn Foo::h(&self) -> (&str, &str);
});
