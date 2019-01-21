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
    method Foo::f(&self) -> (One, Two);
});
