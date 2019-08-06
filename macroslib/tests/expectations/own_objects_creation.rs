foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32) -> Foo;
    fn Foo::f(&self, _: i32, _: i32) -> i32;
});

foreigner_class!(class Boo {
    self_type Boo;

    constructor Boo::new(_: i32, _: usize) -> Result<Boo, String>;
    fn Boo::factory_method() -> Result<Boo, String>;
    fn Boo::boo_as_arg(&self, _: Boo) -> i32;
    fn Boo::get_one_foo(&self) -> Foo;
});
