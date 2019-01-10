foreigner_class!(class Foo {
    self_type Foo;
    private constructor = empty -> Box<Box<Foo>>;
});

foreigner_class!(class Boo {
    self_type Boo;
    private constructor Boo::default() -> Boo;
    method Boo::f(&mut self) -> Result<Box<Box<Foo>>, String>;
    method Boo::f2(&mut self) -> Option<Box<Box<Foo>>>;
});
