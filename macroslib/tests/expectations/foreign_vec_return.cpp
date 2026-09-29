@@expect {"after":"\n    uintptr_t len;\n    uintptr_t capacity;\n};\n\n#ifdef __cplusplus\n} // extern \"","before":"\"our conversion usize <-> uintptr_t is wrong\");\n#endif\n            #include <stdint.h>\n\n#ifdef __cplusplus\nextern \"C\" {\n#endif\n","file":"rust_vec.h","kind":"between"}
struct CRustVecAccess {
    void * data;
@@end

@@expect {"after":"\n    uintptr_t len;\n    uintptr_t capacity;\n};\n\n#ifdef __cplusplus\n} // extern \"","before":"} // extern \"C\" {\n#endif\n#include <stdint.h>\n\n#ifdef __cplusplus\nextern \"C\" {\n#endif\n","file":"rust_vec.h","kind":"between"}
struct CRustForeignVec {
    void * data;
@@end

@@expect {"after":"\n\n    std::v","before":"            std::abort();\n        }\n    }\n\n    ","file":"Boo.hpp","kind":"between"}
RustForeignVecFoo get_foo_arr() const noexcept;
@@end

@@expect {"after":"\n\n    struct","before":"BooOpaque *Boo_new(int32_t a0, uintptr_t a1);\n\n    ","file":"c_Boo.h","kind":"between"}
struct CRustForeignVec Boo_get_foo_arr(const BooOpaque * const self);
@@end

@@expect {"after":"\n\n    templa","before":"constexpr const uintptr_t &BooWrapper<OWN_DATA>::rust_elem_size;\n\n\n    ","file":"Boo.hpp","kind":"between"}
template<bool OWN_DATA>
    inline RustForeignVecFoo BooWrapper<OWN_DATA>::get_foo_arr() const noexcept
    {

        struct CRustForeignVec ret = Boo_get_foo_arr(this->self_);
        return RustForeignVecFoo{ret};
    }
@@end

@@expect {"after":"\n\n    struct","before":"struct CRustForeignVec Boo_get_foo_arr(const BooOpaque * const self);\n\n    ","file":"c_Boo.h","kind":"between"}
struct CRustResult4232mut3232c_voidCRustString Boo_get_foo_with_err(const BooOpaque * const self);
@@end

@@expect {"after":"\n\n    std::v","before":"RustForeignVecFoo get_foo_arr() const noexcept;\n\n    ","file":"Boo.hpp","kind":"between"}
std::variant<Foo, RustString> get_foo_with_err() const noexcept;
@@end

@@expect {"after":"\n\n    template<bool OWN_","before":"return RustForeignVecFoo{ret};\n    }\n\n    ","file":"Boo.hpp","kind":"between"}
template<bool OWN_DATA>
    inline std::variant<Foo, RustString> BooWrapper<OWN_DATA>::get_foo_with_err() const noexcept
    {

        struct CRustResult4232mut3232c_voidCRustString ret = Boo_get_foo_with_err(this->self_);
        return ret.is_ok != 0 ?
              std::variant<Foo, RustString> { Foo(static_cast<FooOpaque *>(ret.data.ok)) } :
              std::variant<Foo, RustString> { RustString{ret.data.err} };
    }
@@end

@@expect {"after":"\n\n    void Boo_delete(const BooOpaque *s","before":"struct CRustResult4232mut3232c_voidCRustString Boo_get_foo_with_err(const BooOpaque * const self);\n\n    ","file":"c_Boo.h","kind":"between"}
struct CRustResultCRustForeignVecCRustString Boo_get_foo_arr_with_err(const BooOpaque * const self);
@@end

@@expect {"after":"\n\nprivate:\n   static void free_mem(SelfT","before":"std::variant<Foo, RustString> get_foo_with_err() const noexcept;\n\n    ","file":"Boo.hpp","kind":"between"}
std::variant<RustForeignVecFoo, RustString> get_foo_arr_with_err() const noexcept;
@@end

@@expect {"after":"\n\n} // namespace org_exa","before":"std::variant<Foo, RustString> { RustString{ret.data.err} };\n    }\n\n    ","file":"Boo.hpp","kind":"between"}
template<bool OWN_DATA>
    inline std::variant<RustForeignVecFoo, RustString> BooWrapper<OWN_DATA>::get_foo_arr_with_err() const noexcept
    {

        struct CRustResultCRustForeignVecCRustString ret = Boo_get_foo_arr_with_err(this->self_);
        return ret.is_ok != 0 ?
              std::variant<RustForeignVecFoo, RustString> { RustForeignVecFoo{ret.data.ok} } :
              std::variant<RustForeignVecFoo, RustString> { RustString{ret.data.err} };
    }
@@end
