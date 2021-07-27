foreign_class!(class Boo {
  self_type Boo;
  constructor Boo::new() -> Boo;
  fn Boo::something(&self) -> i32;
});

foreign_enum!(
    enum ControlItem {
        GNSS = ControlItem::GnssWorking,
        GPS_PROVIDER = ControlItem::AndroidGPSOn,
    }
);

foreign_class!(class Foo {
   self_type Foo;
   constructor Foo::default() -> Foo;
   constructor Foo::new(_: Option<Boo>, _: Option<f64>) -> Foo;
   fn Foo::f1(&self, _: Option<f64>);
   fn Foo::f2(&mut self, _: Option<Boo>);
   fn Foo::f3(&mut self, _: Option<ControlItem>);
   fn Foo::f4(&self, x: Option<usize>);
   fn Foo::f5(x: Option<f64>, y: Option<usize>);
   fn Foo::f6(x: Option<&str>);
   fn Foo::f7(x: Option<&Boo>);
});
