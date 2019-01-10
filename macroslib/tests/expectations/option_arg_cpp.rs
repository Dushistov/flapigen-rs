foreigner_class!(class Boo {
  self_type Boo;
  constructor Boo::new() -> Boo;
  method Boo::something(&self) -> i32;
});

foreign_enum!(
    enum ControlItem {
        GNSS = ControlItem::GnssWorking,
        GPS_PROVIDER = ControlItem::AndroidGPSOn,
    }
);

foreigner_class!(class Foo {
   self_type Foo;
   constructor Foo::default() -> Foo;
   constructor Foo::new(_: Option<Boo>, _: Option<f64>) -> Foo;
   method Foo::f1(&self, _: Option<f64>);
   method Foo::f2(&mut self, _: Option<Boo>);
   method Foo::f3(&mut self, _: Option<ControlItem>);
   method Foo::f4(&self, x: Option<usize>);
   static_method Foo::f5(x: Option<f64>, y: Option<usize>);
   static_method Foo::f6(x: Option<&str>);
});
