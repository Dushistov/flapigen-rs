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
    /// @param id - some number
    /// @param desc - more information
    constructor Foo::new(id: i32, desc: &str) -> Foo;
});
