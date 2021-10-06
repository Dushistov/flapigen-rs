foreign_callback!(callback MyObserver {
    self_type OnEvent + Send;
    onStateChanged = OnEvent::something_change(&self, x: i32, s: &str);
});

foreign_class!(class Test {
    fn f(_: Box<dyn OnEvent + Send>);
});
