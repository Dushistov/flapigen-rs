trait SomeTrait {
    fn on_state_changed(&self, item: i32, is_ok: bool);
    fn on_state_changed_without_args(&self);
}

foreign_interface!(interface SomeObserver {
    self_type SomeTrait;
    onStateChanged = SomeTrait::on_state_changed(&self, _: i32, _: bool);
    onStateChangedWithoutArgs = SomeObserver::on_state_changed_without_args(&self);
});

foreigner_class!(class ClassWithCallbacks {
    self_type Foo;
    constructor Foo::default() -> Foo;
    method f1(&mut self, cb: Box<SomeTrait>);
});
