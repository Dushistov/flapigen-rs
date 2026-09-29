@@expect {"after":"\n\n    RustSl","before":"            std::abort();\n        }\n    }\n\n    ","file":"Boo.hpp","kind":"between"}
RustSlice<const uint32_t> f1() const noexcept;
@@end

@@expect {"after":"\n\n    struct","before":"BooOpaque *Boo_new(int32_t a0, uintptr_t a1);\n\n    ","file":"c_Boo.h","kind":"between"}
struct CRustSliceu32 Boo_f1(const BooOpaque * const self);
@@end

@@expect {"after":"\n\n    templa","before":"constexpr const uintptr_t &BooWrapper<OWN_DATA>::rust_elem_size;\n\n\n    ","file":"Boo.hpp","kind":"between"}
template<bool OWN_DATA>
    inline RustSlice<const uint32_t> BooWrapper<OWN_DATA>::f1() const noexcept
    {

        struct CRustSliceu32 ret = Boo_f1(this->self_);
        return RustSlice<const uint32_t>{ret};
    }
@@end

@@expect {"after":"\n\n    struct CRustSliceu","before":"struct CRustSliceu32 Boo_f1(const BooOpaque * const self);\n\n    ","file":"c_Boo.h","kind":"between"}
struct CRustSliceForeignFoo Boo_f2(const BooOpaque * const self);
@@end

@@expect {"after":"\n\n    RustSlice<const ui","before":"RustSlice<const uint32_t> f1() const noexcept;\n\n    ","file":"Boo.hpp","kind":"between"}
RustSlice<const Foo> f2() const noexcept;
@@end

@@expect {"after":"\n\n    FooOpa","before":"typedef struct FooOpaque FooOpaque;\n    ","file":"c_Foo.h","kind":"between"}
extern const uintptr_t RustForeignClassFooElemSize;
@@end

@@expect {"after":"\n\n    FooWrapper(int32_t a0) noexcept\n  ","before":"FooWrapper &operator=(const FooWrapper&) = delete;\n    ","file":"Foo.hpp","kind":"between"}
static constexpr const uintptr_t &rust_elem_size = RustForeignClassFooElemSize;
@@end

@@expect {"after":"\n    friend ","before":"using value_type = FooWrapper<true>;\n    ","file":"Foo.hpp","kind":"between"}
using SliceRef = FooWrapper<false>;
@@end

@@expect {"after":"\n\n\n    template<bool OWN_DATA>\n    inlin","before":"SelfType self_;\n};\ntemplate<bool OWN_DATA>\n","file":"Foo.hpp","kind":"between"}
constexpr const uintptr_t &FooWrapper<OWN_DATA>::rust_elem_size;
@@end

@@expect {"after":"\n\n    template<bool OWN_","before":"return RustSlice<const uint32_t>{ret};\n    }\n\n    ","file":"Boo.hpp","kind":"between"}
template<bool OWN_DATA>
    inline RustSlice<const Foo> BooWrapper<OWN_DATA>::f2() const noexcept
    {

        struct CRustSliceForeignFoo ret = Boo_f2(this->self_);
        return RustSlice<const Foo>{ret};
    }
@@end

@@expect {"after":"\n\n    RustSlice<uintptr_t> f4() const no","before":"RustSlice<const Foo> f2() const noexcept;\n\n    ","file":"Boo.hpp","kind":"between"}
RustSlice<const uintptr_t> f3() const noexcept;
@@end

@@expect {"after":"\n\n    struct CRustSliceMutusize Boo_f4(c","before":"struct CRustSliceForeignFoo Boo_f2(const BooOpaque * const self);\n\n    ","file":"c_Boo.h","kind":"between"}
struct CRustSliceusize Boo_f3(const BooOpaque * const self);
@@end

@@expect {"after":"\n\n    template<bool OWN_","before":"return RustSlice<const Foo>{ret};\n    }\n\n    ","file":"Boo.hpp","kind":"between"}
template<bool OWN_DATA>
    inline RustSlice<const uintptr_t> BooWrapper<OWN_DATA>::f3() const noexcept
    {

        struct CRustSliceusize ret = Boo_f3(this->self_);
        return RustSlice<const uintptr_t>{ret};
    }
@@end

@@expect {"after":"\n\nprivate:\n   static void free_mem(SelfT","before":"RustSlice<const uintptr_t> f3() const noexcept;\n\n    ","file":"Boo.hpp","kind":"between"}
RustSlice<uintptr_t> f4() const noexcept;
@@end

@@expect {"after":"\n\n    void Boo_delete(const BooOpaque *s","before":"struct CRustSliceusize Boo_f3(const BooOpaque * const self);\n\n    ","file":"c_Boo.h","kind":"between"}
struct CRustSliceMutusize Boo_f4(const BooOpaque * const self);
@@end

@@expect {"after":"\n\n} // namespace org_exa","before":"return RustSlice<const uintptr_t>{ret};\n    }\n\n    ","file":"Boo.hpp","kind":"between"}
template<bool OWN_DATA>
    inline RustSlice<uintptr_t> BooWrapper<OWN_DATA>::f4() const noexcept
    {

        struct CRustSliceMutusize ret = Boo_f4(this->self_);
        return RustSlice<uintptr_t>{ret};
    }
@@end
