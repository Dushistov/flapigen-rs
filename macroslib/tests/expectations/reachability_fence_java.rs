foreign_class!(class DropCounter {
    self_type DropCounter;
    constructor DropCounter::default() -> DropCounter;
});

foreign_class!(class LongOperation {
    self_type LongOperation;
    constructor new(drop_counter: &DropCounter) -> LongOperation;
    fn mf(&self, drop_counter: &DropCounter);
    fn f(drop_counter: &DropCounter);

    fn mf2(&self, drop_counter: &DropCounter) -> i32;
    fn f2(drop_counter: &DropCounter) -> i32;
});
