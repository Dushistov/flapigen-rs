foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: bool) -> Foo;
    method f1(&mut self, _: bool) -> bool;
    static_method f2(_: bool) -> bool;
});

foreign_callback!(callback SomeObserver {
    self_type SomeTrait;
    onStateChanged1 = SomeTrait::on_state_changed1(&self, _: i32, _: bool);
});
