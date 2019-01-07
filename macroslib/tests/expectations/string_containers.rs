foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::default() -> Foo;
    method Foo::list(&self) -> Vec<String>;
});
