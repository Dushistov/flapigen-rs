foreign_class!(
    class Foo {
        fn Foo::f1(cb: Option<extern "C" fn(::std::os::raw::c_int, *mut ::std::os::raw::c_void)>);
        fn Foo::f2(cb: Option<extern "C" fn(*mut ::std::os::raw::c_void)>);
        fn Foo::f3(cb: Option<extern "C" fn(*mut ::std::os::raw::c_void) -> ::std::os::raw::c_char>);
    }
);
