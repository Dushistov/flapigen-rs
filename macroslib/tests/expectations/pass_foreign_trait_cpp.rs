fn create_interface() -> Box<Box<Interface>> {
    Box::new(Box::new(InterfaceImpl { base: 17 }))
}

foreigner_class!(class Interface {
    self_type Interface;
    constructor create_interface() -> Box<Box<Interface>>;
    method Interface::f(&self, _: i32) -> i32;
    method Interface::set(&mut self, x: i32);
});

foreigner_class!(class TestPassInterface {
    self_type TestPassInterface;
    constructor TestPassInterface::default() -> TestPassInterface;
    static_method use_interface(a: Box<Box<Interface>>, b: i32) -> i32;
});
