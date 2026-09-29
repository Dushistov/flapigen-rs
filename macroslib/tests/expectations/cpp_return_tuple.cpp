@@expect {"after":";\n\n    std::","before":"FooWrapper() noexcept {}\n","file":"Foo.hpp","kind":"between"}
public:

    std::pair<One, Two> f() const noexcept
@@end

@@expect {"after":"// Automatic","file":"Foo.hpp","kind":"between"}

@@end

@@expect {"after":"\n\n    templa","before":"constexpr const uintptr_t &FooWrapper<OWN_DATA>::rust_elem_size;\n\n\n    ","file":"Foo.hpp","kind":"between"}
template<bool OWN_DATA>
    inline std::pair<One, Two> FooWrapper<OWN_DATA>::f() const noexcept
    {

        struct CRustPair4232mut3232c_void4232mut3232c_void ret = Foo_f(this->self_);
        return std::make_pair(One(static_cast<OneOpaque *>(ret.first)), Two(static_cast<TwoOpaque *>(ret.second)));
    }
@@end

@@expect {"after":"\n\n    struct","before":"extern const uintptr_t RustForeignClassFooElemSize;\n\n    ","file":"c_Foo.h","kind":"between"}
struct CRustPair4232mut3232c_void4232mut3232c_void Foo_f(const FooOpaque * const self);
@@end

@@expect {"after":"\n\n    struct","before":"struct CRustPair4232mut3232c_void4232mut3232c_void Foo_f(const FooOpaque * const self);\n\n    ","file":"c_Foo.h","kind":"between"}
struct CRustPairi32i32 Foo_g(const FooOpaque * const self);
@@end

@@expect {"after":"\n\n    templa","before":"return std::make_pair(One(static_cast<OneOpaque *>(ret.first)), Two(static_cast<TwoOpaque *>(ret.second)));\n    }\n\n    ","file":"Foo.hpp","kind":"between"}
template<bool OWN_DATA>
    inline std::pair<int32_t, int32_t> FooWrapper<OWN_DATA>::g() const noexcept
    {

        struct CRustPairi32i32 ret = Foo_g(this->self_);
        return std::make_pair(ret.first, ret.second);
    }
@@end

@@expect {"after":"\n\n    void Foo_delete(const FooOpaque *s","before":"struct CRustPairi32i32 Foo_g(const FooOpaque * const self);\n\n    ","file":"c_Foo.h","kind":"between"}
struct CRustPairCRustStrViewCRustStrView Foo_h(const FooOpaque * const self);
@@end

@@expect {"after":"\n\n} // namespace org_exa","before":"return std::make_pair(ret.first, ret.second);\n    }\n\n    ","file":"Foo.hpp","kind":"between"}
template<bool OWN_DATA>
    inline std::pair<std::string_view, std::string_view> FooWrapper<OWN_DATA>::h() const noexcept
    {

        struct CRustPairCRustStrViewCRustStrView ret = Foo_h(this->self_);
        return std::make_pair(std::string_view{ ret.first.data, ret.first.len }, std::string_view{ ret.second.data, ret.second.len });
    }
@@end
