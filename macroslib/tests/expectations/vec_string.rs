foreign_class!(class StringVecs {
    self_type StringVecs;
    constructor StringVecs::new() -> StringVecs;
    fn StringVecs::strings(&self) -> Vec<String>;
    fn StringVecs::append(&self, values: Vec<String>) -> Vec<String>;
});
