foreign_enum!(
    enum Foo {
        cA = Foo::A,
        cB = Foo::B,
    }
);

foreigner_class!(class Moo {
   self_type Moo;
   private constructor = empty;
});

foreigner_class!(class Boo {
   self_type Boo;
   private constructor Boo::default() -> Boo;
   method Boo::f(&self) -> Result<Moo, Foo>;
   method Boo::f2(&self, _: Foo) -> Foo;
});
