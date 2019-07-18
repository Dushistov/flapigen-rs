foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32) -> Foo;
    method Foo::eq(&self, _: &Foo) -> bool;
    method Foo::f(&self, _: i32, _: i32, _: String) -> String;
});
