@@expect {"after":"\n    uintptr_t len;\n    uintptr_t capacity;\n};\n\n#ifdef __cplusplus\n} // extern \"","before":"struct CRustStrView {\n    const char * data;\n    uintptr_t len;\n};\n\n#ifdef __cplusplus\n} // extern \"C\" {\n#endif\n\n#ifdef __cplusplus\nextern \"C\" {\n#endif\n","file":"rust_str.h","kind":"between"}
struct CRustString {
    char * data;
@@end

@@expect {"after":";\n\nprivate:\n","before":"            std::abort();\n        }\n    }\n\n    ","file":"Foo.hpp","kind":"between"}
RustString f(int32_t a0, int32_t a1, RustString a2) const noexcept
@@end

@@expect {"after":"\n\n} // names","before":"constexpr const uintptr_t &FooWrapper<OWN_DATA>::rust_elem_size;\n\n\n    ","file":"Foo.hpp","kind":"between"}
template<bool OWN_DATA>
    inline RustString FooWrapper<OWN_DATA>::f(int32_t a0, int32_t a1, RustString a2) const noexcept
    {

        struct CRustString ret = Foo_f(this->self_, a0, a1, a2.release());
        return RustString{ret};
    }
@@end

@@expect {"after":"\n\nnamespace ","before":"#include <stdint.h>","file":"Foo.hpp","kind":"between"}

#include "rust_str.h"

#include "c_Foo.h"
@@end
