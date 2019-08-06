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
    fn Foo::f1(&self, _: Option<f64>) -> Option<f64>;
    fn Foo::f2(&self, _: Option<i64>) -> Option<i64>;
    fn Foo::f3(&self) -> Option<Boo>;
    fn Foo::f4(&self, boo: Option<Boo>);
    fn Foo::f5(&self) -> Option<String>;
    fn Foo::f6(&self, boo: Option<&Boo>);
    fn Foo::f7(&self, _: Option<&str>);
    fn Foo::f8(&self, _: Option<i32>) -> Option<i32>;
});
