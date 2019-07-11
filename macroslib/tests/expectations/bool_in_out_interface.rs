foreign_callback!(callback SomeObserver {
    self_type SomeTrait;
    onStateChanged1 = SomeTrait::on_state_changed1(&self, _: i32, _: bool);
    onStateChanged2 = SomeTrait::on_state_changed2(&self, _: bool, _: f64) -> bool;
});
