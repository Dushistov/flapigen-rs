foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32) -> Foo;
    method Foo::f(&self, _: i32, _: i32) -> i32;
});

foreigner_class!(class Boo {
    self_type Boo;

    constructor Boo::new(_: i32, _: usize) -> Boo;
    method Boo::get_foo_arr(&self) -> Vec<Foo>;
    method Boo::get_foo_with_err(&self) -> Result<Foo, String>;
    method Boo::get_foo_arr_with_err(&self) -> Result<Vec<Foo>, String>;
});
