foreigner_class!(
    class Foo {
        self_type Foo;
        constructor Foo::default() -> Foo;
    }
);

trait SomeTrait {
    fn on_state_changed(&self, item: i32, is_ok: bool);
    fn on_state_changed_without_args(&self);
    fn on_state_changed_foo(&self, foo: Foo);
    fn get_text_size(&self) -> f32;
}

foreign_callback!(callback SomeObserver {
    self_type SomeTrait;
    onStateChanged = SomeTrait::on_state_changed(&self, _: i32, _: bool);
    onStateChangedWithoutArgs = SomeObserver::on_state_changed_without_args(&self);
    onStateChangedFoo = SomeObserver::on_state_changed_foo(&self, foo: Foo);
    getTextSize = SomeObserver::get_text_size(&self) -> f32;
});

foreigner_class!(class ClassWithCallbacks {
    self_type Foo;
    constructor Foo::default() -> Foo;
    fn f1(&mut self, cb: Box<dyn SomeTrait>);
});
