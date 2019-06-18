"RustString f(int32_t a_0, int32_t a_1, std::string_view a_2) const  noexcept";
r#"template<bool OWN_DATA>
    inline RustString FooWrapper<OWN_DATA>::f(int32_t a_0, int32_t a_1, std::string_view a_2) const  noexcept
    {
        struct CRustString ret = Foo_f(this->self_, a_0, a_1, CRustStrView{ a_2.data(), a_2.size() });
        return RustString{ret};
    }"#;
r#"
#include "rust_str.h"
#include <string_view>

#include "c_Foo.h""#;
