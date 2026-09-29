@@expect {"after":"\n\n#ifdef __c","before":"extern \"C\" {\n#endif\n","file":"CRustSliceMutForeignFoo.h","kind":"between"}
struct CRustSliceMutForeignFoo {
    struct FooOpaque * data;
    uintptr_t len;
};
@@end

@@expect {"after":"\n\n#ifdef __c","before":"extern \"C\" {\n#endif\n","file":"CRustSliceForeignFoo.h","kind":"between"}
struct CRustSliceForeignFoo {
    const struct FooOpaque * data;
    uintptr_t len;
};
@@end

@@expect {"after":"\n\n    struct","before":"BooOpaque *Boo_new(int32_t a0, uintptr_t a1);\n\n    ","file":"c_Boo.h","kind":"between"}
struct CRustSliceu32 Boo_f1(const BooOpaque * const self, struct CRustSliceMutForeignFoo a0);
@@end

@@expect {"after":"\n\n    RustSl","before":"            std::abort();\n        }\n    }\n\n    ","file":"Boo.hpp","kind":"between"}
RustSlice<const uint32_t> f1(RustSlice<Foo> a0) const noexcept;
@@end

@@expect {"after":"\n\n    templa","before":"constexpr const uintptr_t &BooWrapper<OWN_DATA>::rust_elem_size;\n\n\n    ","file":"Boo.hpp","kind":"between"}
template<bool OWN_DATA>
    inline RustSlice<const uint32_t> BooWrapper<OWN_DATA>::f1(RustSlice<Foo> a0) const noexcept
    {

        struct CRustSliceu32 ret = Boo_f1(this->self_, a0.as_c<CRustSliceMutForeignFoo>());
        return RustSlice<const uint32_t>{ret};
    }
@@end

@@expect {"after":"\n\n    void B","before":"struct CRustSliceu32 Boo_f1(const BooOpaque * const self, struct CRustSliceMutForeignFoo a0);\n\n    ","file":"c_Boo.h","kind":"between"}
struct CRustSliceu32 Boo_f2(const BooOpaque * const self, struct CRustSliceForeignFoo a0);
@@end

@@expect {"after":"\n\nprivate:\n ","before":"RustSlice<const uint32_t> f1(RustSlice<Foo> a0) const noexcept;\n\n    ","file":"Boo.hpp","kind":"between"}
RustSlice<const uint32_t> f2(RustSlice<const Foo> a0) const noexcept;
@@end

@@expect {"after":"\n\n} // namespace org_exa","before":"return RustSlice<const uint32_t>{ret};\n    }\n\n    ","file":"Boo.hpp","kind":"between"}
template<bool OWN_DATA>
    inline RustSlice<const uint32_t> BooWrapper<OWN_DATA>::f2(RustSlice<const Foo> a0) const noexcept
    {

        struct CRustSliceu32 ret = Boo_f2(this->self_, a0.as_c<CRustSliceForeignFoo>());
        return RustSlice<const uint32_t>{ret};
    }
@@end
