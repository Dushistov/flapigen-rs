foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32) -> Foo;
    fn Foo::f(&self, _: i32, _: i32) -> i32;
});

foreigner_class!(class Boo {
    self_type Boo;

    constructor Boo::new(_: i32, _: usize) -> Result<Boo, String>;
    fn Boo::get_foo_arr(&self) -> Vec<Foo>;
    fn Boo::get_one_foo(&self) -> Result<Foo, String>;
    fn now() -> SystemTime;
    fn now2() -> Result<SystemTime, String>;
    fn r_test_u8(v: u8) -> Result<u8, &'static str>;
});
