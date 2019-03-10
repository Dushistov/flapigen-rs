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
   method Foo::f1(&self, _: Option<f64>) -> Option<f64>;
});
