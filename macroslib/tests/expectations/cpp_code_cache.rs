// Before #435, a part of generated code is duplicated.
// This test case makes sure that this does not happen anymore.

foreign_class!(
    class Foo {
        fn Foo::return_is_array_u8() -> Vec<u8>;
        fn Foo::param_is_array_u8(_: Vec<u8>);
    }
);

