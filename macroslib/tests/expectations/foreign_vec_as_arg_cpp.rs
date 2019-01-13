foreigner_class!(class Boo {
    self_type Boo;
    constructor Boo::default() -> Boo;
});
foreigner_class!(class FooImpl {
    self_type Foo<'a>;
    constructor Foo::create() -> Foo<'a>;
    method Foo::alternate_boarding(&self) -> &[Boo]; alias alternateBoarding;
    method Foo::set_alternate_boarding(&mut self, p: Vec<Boo>);
    alias setAlternateBoarding;
});
