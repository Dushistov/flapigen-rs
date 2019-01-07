foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32) -> Foo;
    method Foo::f(&self, _: i32, _: i32) -> i32;
});

foreigner_class!(class Boo {
    self_type Boo;

    constructor Boo::new(_: i32, _: usize) -> Result<Boo, String>;
    method Boo::get_foo_arr(&self) -> Vec<Foo>;
    method Boo::get_one_foo(&self) -> Result<Foo, String>;
    static_method now() -> SystemTime;
    static_method r_test_u8(v: u8) -> Result<u8, &'static str>;
});
