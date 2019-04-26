foreigner_class!(class A {
    self_type A;
    constructor A::default() -> A;
    static_method A::a(b: &B);
});

foreigner_class!(class B {
    self_type B;
    constructor B::default() -> B;
    static_method B::b(a: &A);
});
