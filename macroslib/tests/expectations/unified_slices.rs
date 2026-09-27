foreign_class!(class Foo {
    self_type Foo;
    constructor Foo::new() -> Foo;
    fn Foo::value(&self) -> i32;
});

foreign_class!(class Bar {
    self_type Bar;
    constructor Bar::new() -> Bar;
    fn Bar::value(&self) -> i32;
});

foreign_class!(class Mix {
    self_type Mix;
    constructor Mix::new() -> Mix;
    fn Mix::foo_ref(&self) -> &Foo;
    fn Mix::bar_ref(&self) -> &Bar;
    fn Mix::raw_ptr(&self) -> *const ::std::os::raw::c_void;
    fn Mix::foreign_first(&self) -> &[Foo];
    fn Mix::foreign_other(&self) -> &[Bar];
    fn Mix::native_second(&self) -> &[u32];
    fn Mix::native_other(&self) -> &[u64];
    fn Mix::mut_foreign(&self, values: &mut [Foo]);
    fn Mix::mut_foreign_other(&self, values: &mut [Bar]);
    fn Mix::mut_native(&self, values: &mut [u32]);
    fn Mix::mut_native_other(&self, values: &mut [u64]);
});
