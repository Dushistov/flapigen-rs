foreign_typemap!(
    foreign_code!(module = "foo_borrow.hpp"; r#"
#pragma once
#include "c_Foo.h"
namespace org_examples {
using FooBorrow = const FooOpaque *;
using FooMutBorrow = FooOpaque *;
}
"#);
    ($p:r_type) &Foo => *const ::std::os::raw::c_void {
        $out = ($p as *const Foo).cast::<::std::os::raw::c_void>();
    };
    ($p:f_type, req_modules = ["\"foo_borrow.hpp\""]) => "FooBorrow"
        r#"FooBorrow{ static_cast<const FooOpaque *>($p) }"#;
);

foreign_typemap!(
    ($p:r_type) &mut Foo => *mut ::std::os::raw::c_void {
        $out = ($p as *mut Foo).cast::<::std::os::raw::c_void>();
    };
    ($p:f_type, req_modules = ["\"foo_borrow.hpp\""]) => "FooMutBorrow"
        r#"FooMutBorrow{ static_cast<FooOpaque *>($p) }"#;
);

foreign_class!(
    #[derive(PlainClass)]
    class Foo {
        self_type Foo;
        constructor Foo::new() -> Foo;
        fn Foo::get(&self) -> &Foo;
        fn Foo::get_mut(&mut self) -> &mut Foo;
    }
);
