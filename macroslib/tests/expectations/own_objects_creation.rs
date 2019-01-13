foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32) -> Foo;
    method Foo::f(&self, _: i32, _: i32) -> i32;
});

foreigner_class!(class Boo {
    self_type Boo;

    constructor Boo::new(_: i32, _: usize) -> Result<Boo, String>;
    static_method Boo::factory_method() -> Result<Boo, String>;
    method Boo::boo_as_arg(&self, _: Boo) -> i32;
    method Boo::get_one_foo(&self) -> Foo;
});
