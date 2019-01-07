foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32, _: &str) -> Foo;
    method Foo::f(&self, a: i32, b: i32) -> i32;
});

foreigner_class!(class TestReferences {
    self_type TestReferences;
    constructor TestReferences::new(foo_data: i32, foo_name: &str) -> TestReferences;
    method TestReferences::get_foo_ref(&self) -> &Foo;
    method TestReferences::update_foo(&mut self, foo: &Foo);
    method TestReferences::update_mut_foo(&mut self, foo: &mut Foo);
});
