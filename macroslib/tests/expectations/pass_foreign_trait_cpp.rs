// Foo and TestWorkWithVec here to reproduce bug
foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32, _: &str) -> Foo;
});

foreigner_class!(class TestWorkWithVec {
    self_type TestWorkWithVec;
    constructor TestWorkWithVec::new(_: &str) -> TestWorkWithVec;
    method TestWorkWithVec::get_bytes(&self, n: u32) -> Vec<u8>;

    static_method TestWorkWithVec::sort_foo_slice(v: &mut [Foo]);
});

foreigner_class!(class Interface {
    self_type dyn Interface;
    constructor create_interface() -> Box<Box<dyn Interface>>;
    method Interface::f(&self, _: i32) -> i32;
    method Interface::set(&mut self, x: i32);
});

foreigner_class!(class TestPassInterface {
    self_type TestPassInterface;
    constructor TestPassInterface::default() -> TestPassInterface;
    static_method use_interface(a: Box<Box<Interface>>, b: i32) -> i32;
});
