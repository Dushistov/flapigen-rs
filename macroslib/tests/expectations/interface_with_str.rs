foreign_callback!(callback SomeObserver {
    self_type SomeTrait;
    onStateChanged = SomeTrait::on_state_changed(&self, _: &str);
});

foreigner_class!(class ClassWithCallbacks {
    self_type Foo;
    constructor Foo::default() -> Foo;
    method f1(&mut self, cb: Box<dyn SomeTrait>);
});
