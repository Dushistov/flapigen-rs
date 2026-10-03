foreign_class!(class StringSlices {
    self_type StringSlices;
    constructor StringSlices::new() -> StringSlices;
    fn StringSlices::refs(&self) -> &[&str];
    fn StringSlices::strings(&self) -> &[String];
    fn StringSlices::boxed(&self) -> &[Box<str>];
    fn StringSlices::same_refs(&self, values: &[&str]) -> bool;
    fn StringSlices::same_strings(&self, values: &[String]) -> bool;
    fn StringSlices::same_boxed(&self, values: &[Box<str>]) -> bool;
});
