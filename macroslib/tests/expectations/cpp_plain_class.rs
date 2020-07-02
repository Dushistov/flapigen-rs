foreign_class!(
    #[derive(PlainClass)]
    class Foo {
        self_type Foo;

        constructor Foo::new() -> Foo;
        fn Foo::method(&self);
        fn Foo::static_func();
    }
);
