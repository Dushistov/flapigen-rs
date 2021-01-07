foreign_class!(
    class Foo {
        self_type Foo;
        constructor Foo::new() -> Foo;
        private constructor Foo::from_int(_: i32) -> Foo;
        private fn Foo::private_f();
        fn Foo::public_f();
        protected fn Foo::protected_f();
    }
);
