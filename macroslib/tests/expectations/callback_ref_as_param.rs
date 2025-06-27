foreign_callback!(callback Foo {
    self_type Foo;
    const_method = Foo::const_method(&self);
    mut_method = Foo::mut_method(&mut self);
});

foreign_class!(class TestFooRef {
    fn call_const_method(x: &dyn Foo);
    fn call_mut_method(x: &mut dyn Foo);
});
