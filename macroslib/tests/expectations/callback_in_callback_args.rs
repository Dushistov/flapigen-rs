foreign_callback!(callback Foo {
    self_type Foo;
    f = Foo::f(&self);
});

foreign_callback!(callback Boo {
    self_type Boo;
    g = Boo::g(&self, x: &C_Foo);
    h = Boo::h(&self, x: &mut C_Foo);
});

foreign_class!(
    #[derive(PlainClass)]
    class Class {
        fn static_member(x: *mut C_Boo);
    }
);
