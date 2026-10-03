foreigner_class!(class Foo {
   self_type SomeType;
   private constructor = empty;
   method SomeType::f(&self);
});

foreigner_class!(class Boo {
   self_type OtherType;
   private constructor = empty;
   method OtherType::f(&self);
});
