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
   fn Foo::f1(&self) -> Option<Boo>;
   fn Foo::f2(&self) -> Option<f64>;
   fn Foo::f3(&self) -> Option<u32>;
   fn Foo::f4(&self) -> Option<usize>;
   fn Foo::f5(&self) -> Option<&Boo>;
   fn Foo::f6(&self) -> Option<ControlItem>;
   fn Foo::f7(&self) -> Option<u64>;
   fn Foo::f8(&self) -> Option<&str>;
   fn Foo::f9(&self) -> Option<String>;
   fn Foo::f10(&self) -> Option<bool>;
});
