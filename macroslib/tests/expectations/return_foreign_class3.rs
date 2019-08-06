foreigner_class!(class Foo {
    self_type Foo;
    private constructor = empty -> Box<Box<Foo>>;
});
foreigner_class!(class Boo {
    self_type Boo;
    private constructor Boo::default() -> Boo;
    fn Boo::f(&mut self) -> Result<Box<Box<Foo>>, String>;
});
