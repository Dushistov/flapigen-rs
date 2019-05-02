foreigner_class!(
/// Class comment description for Foo.
class Foo {
    self_type Foo;
    /// some text about the new function
    ///
    /// ```
    /// some markdown example in the text
    /// ```
    ///
    /// @param a0 id - somenumber
    /// @param a1 desc - more information
    constructor Foo::new(_: i32, _: &str) -> Foo;
});
