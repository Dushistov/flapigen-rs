foreign_callback!(callback SomeObserver {
    self_type SomeTrait;
    onStateChanged = SomeTrait::on_state_changed(&self, _: i32, _: bool) -> bool;
    onStateChangedWithoutArgs = SomeObserver::on_state_changed_without_args(&self);
});
