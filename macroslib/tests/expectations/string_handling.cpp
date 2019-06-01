"RustString f(int32_t a_0, int32_t a_1, const char * a_2) const";
r#"template<bool OWN_DATA>
    inline RustString FooWrapper<OWN_DATA>::f(int32_t a_0, int32_t a_1, const char * a_2) const  noexcept
    {
        struct CRustString ret = Foo_f(this->self_, a_0, a_1, a_2);
        return RustString{ret};
    }"#;
r#"
#include "rust_str.h"

#include "c_Foo.h""#;
