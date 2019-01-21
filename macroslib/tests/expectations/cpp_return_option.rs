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
   method Foo::f1(&self) -> Option<Boo>;
   method Foo::f2(&self) -> Option<f64>;
   method Foo::f3(&self) -> Option<u32>;
   method Foo::f4(&self) -> Option<usize>;
   method Foo::f5(&self) -> Option<&Boo>;
   method Foo::f6(&self) -> Option<ControlItem>;
   method Foo::f7(&self) -> Option<u64>;
   method Foo::f8(&self) -> Option<&str>;
   method Foo::f9(&self) -> Option<String>;
   method Foo::f10(&self) -> Option<bool>;
});
