foreigner_class!(
/// This is class Foo
class Foo {
    self_type Foo;
    /// Some documentation comment
    constructor Foo::new(_: i32, _: &str) -> Rc<RefCell<Foo>>;
    /// 1 Some documentation comment
    /// 2 Some documentation comment
    method Foo::f(&self, _: i32, _: i32) -> i32;
});
