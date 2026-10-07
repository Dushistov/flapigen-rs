foreign_class!(class RowFoo {
    self_type RowFoo;
    constructor RowFoo::new(value: i32) -> RowFoo;
    fn RowFoo::value(&self) -> i32;
});

foreign_class!(class RowBar {
    self_type RowBar;
    constructor RowBar::new(value: i32) -> RowBar;
    fn RowBar::value(&self) -> i32;
});

foreign_class!(class NestedVecHost {
    self_type NestedVecHost;
    constructor NestedVecHost::new() -> NestedVecHost;
    fn NestedVecHost::make_foos() -> Vec<Vec<RowFoo>>;
    fn NestedVecHost::echo_foos(rows: Vec<Vec<RowFoo>>) -> Vec<Vec<RowFoo>>;
    fn NestedVecHost::echo_bars(rows: Vec<Vec<RowBar>>) -> Vec<Vec<RowBar>>;
});
