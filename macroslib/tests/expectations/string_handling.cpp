"RustString f(int32_t a0, int32_t a1, RustString a2) const noexcept";
r#"template<bool OWN_DATA>
    inline RustString FooWrapper<OWN_DATA>::f(int32_t a0, int32_t a1, RustString a2) const noexcept
    {

        struct CRustString ret = Foo_f(this->self_, a0, a1, a2.release());
        return RustString{ret};
    }"#;
r#"
#include "rust_str.h"

#include "c_Foo.h""#;
