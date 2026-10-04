foreign_class!(
    #[derive(PlainClass)]
    class Foo {
        self_type Foo;
        constructor Foo::new() -> Foo;
        fn Foo::get(&self) -> &Foo;
        fn Foo::get_mut(&mut self) -> &mut Foo;
        fn Foo::read_other(&self, other: &Foo);
        fn Foo::update_other(&mut self, other: &mut Foo);
    }
);
